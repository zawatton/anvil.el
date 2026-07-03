;;; anvil-server-metrics-test.el --- ERT for anvil-server token telemetry -*- lexical-binding: t; -*-

;;; Commentary:
;; Doc 62 Phase 1 adds per-tool MCP payload telemetry inside
;; `anvil-server-metrics.el'.  These tests exercise the metrics seam and
;; the report handler directly, without JSON-RPC transport, matching the
;; raw-handler style used by `tests/anvil-fusion-mcp-test.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-server-metrics)

(defmacro anvil-server-metrics-test--with-clean-state (&rest body)
  "Run BODY with the metrics tables cleared before and after."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn
         (anvil-server-metrics-reset)
         ,@body)
     (anvil-server-metrics-reset)))

(ert-deftest anvil-server-metrics-test-accounting ()
  "Three successful seam calls accumulate per-tool and session totals."
  (anvil-server-metrics-test--with-clean-state
    (let* ((a-req-1 '((path . "/tmp/very-long-a") (mode . "read-write")))
           (a-req-2 '((path . "/tmp/another-long-file")
                      (mode . "append")
                      (flag . t)))
           (b-req '((alpha . 1) (beta . 2)))
           (a-resp-1 "1234567890")
           (a-resp-2 "xyz")
           (b-resp "12345678901234567890")
           (a-entry nil)
           (b-entry nil)
           (report nil)
           (totals nil))
      (anvil-server-metrics--track-tool-payload "tool-a" a-req-1 a-resp-1)
      (anvil-server-metrics--track-tool-payload "tool-b" b-req b-resp)
      (anvil-server-metrics--track-tool-payload "tool-a" a-req-2 a-resp-2)
      (setq a-entry (gethash "tool-a" anvil-server-metrics--tool-token-table))
      (setq b-entry (gethash "tool-b" anvil-server-metrics--tool-token-table))
      (should (= 2 (plist-get a-entry :calls)))
      (should (= (+ (anvil-server-metrics--measure-chars a-req-1)
                    (anvil-server-metrics--measure-chars a-req-2))
                 (plist-get a-entry :request-chars)))
      (should (= 13 (plist-get a-entry :response-chars)))
      (should (= 10 (plist-get a-entry :max-response-chars)))
      (should (= 1 (plist-get b-entry :calls)))
      (should (= (anvil-server-metrics--measure-chars b-req)
                 (plist-get b-entry :request-chars)))
      (should (= 20 (plist-get b-entry :response-chars)))
      (should (= 20 (plist-get b-entry :max-response-chars)))
      (setq report (anvil-server-metrics-token-report))
      (setq totals (plist-get report :session-totals))
      (should (= 3 (plist-get totals :calls)))
      (should (= (+ (plist-get a-entry :request-chars)
                    (plist-get b-entry :request-chars))
                 (plist-get totals :request-chars)))
      (should (= 33 (plist-get totals :response-chars)))
      (should (= (/ (+ (plist-get totals :request-chars) 33) 4)
                 (plist-get totals :est-tokens))))))

(ert-deftest anvil-server-metrics-test-report-sort-top-and-reset ()
  "Sort variants, top_n trimming, and report-then-reset all work."
  (anvil-server-metrics-test--with-clean-state
    (let ((a-req-1 '((path . "/tmp/very-long-a") (mode . "read-write")))
          (a-req-2 '((path . "/tmp/another-long-file")
                     (mode . "append")
                     (flag . t)))
          (b-req '((alpha . 1) (beta . 2))))
      (anvil-server-metrics--track-tool-payload "tool-a" a-req-1 "1234567890")
      (anvil-server-metrics--track-tool-payload "tool-b" b-req "12345678901234567890")
      (anvil-server-metrics--track-tool-payload "tool-a" a-req-2 "xyz")
      (let* ((top-response (plist-get (anvil-server-metrics-token-report 1 nil nil) :top))
             (top-request (plist-get (anvil-server-metrics-token-report 2 "request" nil) :top))
             (top-calls (plist-get (anvil-server-metrics-token-report 2 "calls" nil) :top))
             (pre-reset (anvil-server-metrics-token-report 1 "response" t))
             (post-reset (anvil-server-metrics-token-report))
             (post-totals (plist-get post-reset :session-totals)))
        (should (= 1 (length top-response)))
        (should (equal "tool-b" (plist-get (car top-response) :tool)))
        (should (equal "tool-a" (plist-get (car top-request) :tool)))
        (should (equal "tool-a" (plist-get (car top-calls) :tool)))
        (should (= 1 (length (plist-get pre-reset :top))))
        (should (= 0 (plist-get post-totals :calls)))
        (should (= 0 (plist-get post-totals :request-chars)))
        (should (= 0 (plist-get post-totals :response-chars)))
        (should (= 0 (plist-get post-totals :est-tokens)))
        (should (equal '() (plist-get post-reset :top)))))))

(ert-deftest anvil-server-metrics-test-arg-bool-false-does-not-reset ()
  "Explicit JSON false variants preserve existing counters."
  (anvil-server-metrics-test--with-clean-state
    (anvil-server-metrics--track-tool-payload "tool-a" '((x . 1)) "1234")
    (anvil-server-metrics-token-report nil nil :false)
    (anvil-server-metrics-token-report nil nil "false")
    (let ((totals (plist-get (anvil-server-metrics-token-report) :session-totals)))
      (should (= 1 (plist-get totals :calls)))
      (should (= 4 (plist-get totals :response-chars))))))

(ert-deftest anvil-server-metrics-test-overhead-guard-nil-and-object-response ()
  "The seam tolerates nil and non-string responses via `%S' approximation."
  (anvil-server-metrics-test--with-clean-state
    (anvil-server-metrics--track-tool-payload "tool-nil" '((x . 1)) nil)
    (anvil-server-metrics--track-tool-payload "tool-obj" '((y . 2)) '(:ok t))
    (let* ((nil-entry (gethash "tool-nil" anvil-server-metrics--tool-token-table))
           (obj-entry (gethash "tool-obj" anvil-server-metrics--tool-token-table)))
      (should (= (length "nil") (plist-get nil-entry :response-chars)))
      (should (= (length "(:ok t)") (plist-get obj-entry :response-chars)))
      (should (= (length "nil") (plist-get nil-entry :max-response-chars)))
      (should (= (length "(:ok t)") (plist-get obj-entry :max-response-chars))))))

(provide 'anvil-server-metrics-test)
;;; anvil-server-metrics-test.el ends here
