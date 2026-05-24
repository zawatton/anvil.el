;;; anvil-harness-telemetry-test.el --- Tests for anvil-harness-telemetry -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 46 Phase 1 — runtime harness failure classifier +
;; SQLite telemetry recorder + 3 MCP wrappers + dispatcher hook
;; integration.
;;
;; Each test runs against a fresh temp DB so the user's shared
;; anvil-worklog file is never touched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'anvil-server)
(require 'anvil-harness-telemetry nil t)

;; Forward-declare dynamic vars referenced via `let' below.  Without
;; these the byte-compiler treats them as lexical (the test file uses
;; lexical-binding: t) and `add-hook' cannot see the binding.
(defvar anvil-server-tool-error-hook)
(defvar anvil-server--current-tool-name)
(defvar anvil-harness-telemetry-db-path)
(defvar anvil-harness-telemetry--db)
(defvar anvil-harness-telemetry--resolved-db-path)


;;;; --- fixture helpers ---------------------------------------------------

(defun anvil-ht-test--sqlite-ok-p ()
  (and (fboundp 'sqlite-available-p) (sqlite-available-p)))

(defmacro anvil-ht-test--with-env (&rest body)
  "Run BODY with a fresh temp telemetry DB.
Binds `anvil-harness-telemetry-db-path' to a temp file and the
module's cached handle/path to nil so a clean open happens.  The
dispatcher hook is uninstalled before / after BODY."
  (declare (indent 0))
  `(let* ((anvil-harness-telemetry-db-path
           (make-temp-file "anvil-ht-" nil ".db"))
          (anvil-harness-telemetry--db nil)
          (anvil-harness-telemetry--resolved-db-path nil)
          (anvil-server-tool-error-hook nil))
     (unwind-protect
         (progn
           (anvil-harness-telemetry--open)
           ,@body)
       (anvil-harness-telemetry--close)
       (ignore-errors
         (delete-file anvil-harness-telemetry-db-path)))))


;;;; --- T1-T8: classifier rules ------------------------------------------

(ert-deftest anvil-ht-test-classify-no-exec-void-function ()
  "T1 — void-function symbol classifies as no-exec."
  (let ((c (anvil-harness-telemetry--classify
            '(void-function some-missing-fn)
            :source 'tool-body)))
    (should (eq (car c) 'no-exec))
    (should (= (cdr c) 1.0))))

(ert-deftest anvil-ht-test-classify-no-exec-connection-refused ()
  "T2 — `Connection refused' message classifies as no-exec."
  (let ((c (anvil-harness-telemetry--classify
            '(error "Connection refused: 127.0.0.1:8000")
            :source 'tool-body)))
    (should (eq (car c) 'no-exec))))

(ert-deftest anvil-ht-test-classify-no-exec-process-exited ()
  "T3 — `process exited N' (N≥1) message classifies as no-exec."
  (let ((c (anvil-harness-telemetry--classify
            '(error "subprocess process exited 137")
            :source 'tool-body)))
    (should (eq (car c) 'no-exec))))

(ert-deftest anvil-ht-test-classify-contract-violation-wrong-number ()
  "T4 — wrong-number-of-arguments symbol classifies as contract-violation."
  (let ((c (anvil-harness-telemetry--classify
            '(wrong-number-of-arguments (foo (a b)) 3)
            :source 'tool-body)))
    (should (eq (car c) 'contract-violation))))

(ert-deftest anvil-ht-test-classify-contract-violation-missing-param ()
  "T5 — `Missing required parameter' message classifies as contract-violation."
  (let ((c (anvil-harness-telemetry--classify
            '(anvil-server-invalid-params
              "Missing required parameter: path")
            :source 'dispatcher-validation)))
    (should (eq (car c) 'contract-violation))))

(ert-deftest anvil-ht-test-classify-stall-from-repetition-detector ()
  "T6 — :source 'repetition-detector forces stall regardless of error."
  (let ((c (anvil-harness-telemetry--classify
            '(error "anything at all")
            :source 'repetition-detector)))
    (should (eq (car c) 'stall))
    (should (= (cdr c) 1.0))))

(ert-deftest anvil-ht-test-classify-reasoning-fallback ()
  "T7 — Unknown error falls back to reasoning with low confidence."
  (let ((c (anvil-harness-telemetry--classify
            '(error "something completely novel and unmatched")
            :source 'tool-body)))
    (should (eq (car c) 'reasoning))
    (should (< (cdr c)
               anvil-harness-telemetry-confidence-low-threshold))))

(ert-deftest anvil-ht-test-classify-wrong-type-stack-depth ()
  "T8 — D1 stack-depth split for wrong-type-argument.
Same error symbol, different source yields different class."
  (let ((dispatcher (anvil-harness-telemetry--classify
                     '(wrong-type-argument stringp 42)
                     :source 'dispatcher-validation))
        (body (anvil-harness-telemetry--classify
               '(wrong-type-argument stringp 42)
               :source 'tool-body)))
    (should (eq (car dispatcher) 'contract-violation))
    (should (eq (car body) 'no-exec))))


;;;; --- T9-T12: schema / recorder ----------------------------------------

(ert-deftest anvil-ht-test-schema-tables-exist ()
  "T9 — Schema init creates harness_failures + harness_failures_fts."
  (skip-unless (anvil-ht-test--sqlite-ok-p))
  (anvil-ht-test--with-env
    (let* ((db (anvil-harness-telemetry--db))
           (names (mapcar #'car
                          (sqlite-select
                           db
                           "SELECT name FROM sqlite_master
                              WHERE type IN ('table','index')
                              ORDER BY name"))))
      (should (member "harness_failures" names))
      (should (member "harness_failures_fts" names))
      (should (member "harness_failures_class_idx" names))
      (should (member "harness_failures_ts_idx" names)))))

(ert-deftest anvil-ht-test-record-from-error-inserts-row ()
  "T10 — record-from-error inserts a row with classified class."
  (skip-unless (anvil-ht-test--sqlite-ok-p))
  (anvil-ht-test--with-env
    (let ((res (anvil-harness-telemetry-record-from-error
                '(void-function nope)
                :source 'tool-body
                :tool "test-tool"
                :provider "claude"
                :session "sess-1")))
      (should res)
      (should (eq (plist-get res :class) 'no-exec))
      (should (= (plist-get res :confidence) 1.0))
      (should-not (plist-get res :low-confidence)))
    (let* ((db (anvil-harness-telemetry--db))
           (rows (sqlite-select
                  db
                  "SELECT class, tool, provider, session_id
                     FROM harness_failures"))
           (row (car rows)))
      (should (= (length rows) 1))
      (should (equal (nth 0 row) "no-exec"))
      (should (equal (nth 1 row) "test-tool"))
      (should (equal (nth 2 row) "claude"))
      (should (equal (nth 3 row) "sess-1")))))

(ert-deftest anvil-ht-test-record-direct-returns-identity ()
  "T11 — anvil-harness-telemetry-record returns :id + :class + :confidence."
  (skip-unless (anvil-ht-test--sqlite-ok-p))
  (anvil-ht-test--with-env
    (let ((res (anvil-harness-telemetry-record
                'contract-violation
                :tool "foo"
                :error-message "Missing required parameter: bar")))
      (should (integerp (plist-get res :id)))
      (should (eq (plist-get res :class) 'contract-violation))
      (should (= (plist-get res :confidence) 1.0)))))

(ert-deftest anvil-ht-test-record-low-confidence-flag ()
  "T12 — confidence below threshold flags :low-confidence t."
  (skip-unless (anvil-ht-test--sqlite-ok-p))
  (anvil-ht-test--with-env
    (let ((high (anvil-harness-telemetry-record
                 'reasoning :confidence 0.9))
          (low (anvil-harness-telemetry-record
                'reasoning :confidence 0.3)))
      (should-not (plist-get high :low-confidence))
      (should (plist-get low :low-confidence)))))


;;;; --- T13-T16: stats aggregator ----------------------------------------

(ert-deftest anvil-ht-test-stats-total-count ()
  "T13 — :total counts every recorded row."
  (skip-unless (anvil-ht-test--sqlite-ok-p))
  (anvil-ht-test--with-env
    (dotimes (_ 5)
      (anvil-harness-telemetry-record 'no-exec))
    (let ((stats (anvil-harness-telemetry-stats)))
      (should (= (plist-get stats :total) 5)))))

(ert-deftest anvil-ht-test-stats-by-class ()
  "T14 — :by-class groups rows by class symbol."
  (skip-unless (anvil-ht-test--sqlite-ok-p))
  (anvil-ht-test--with-env
    (anvil-harness-telemetry-record 'no-exec)
    (anvil-harness-telemetry-record 'no-exec)
    (anvil-harness-telemetry-record 'contract-violation)
    (anvil-harness-telemetry-record 'reasoning :confidence 0.3)
    (let* ((stats (anvil-harness-telemetry-stats))
           (bc (plist-get stats :by-class)))
      (should (= (alist-get 'no-exec bc) 2))
      (should (= (alist-get 'contract-violation bc) 1))
      (should (= (alist-get 'reasoning bc) 1)))))

(ert-deftest anvil-ht-test-stats-by-tool-and-provider ()
  "T15 — :by-tool / :by-provider group correctly and skip NULL."
  (skip-unless (anvil-ht-test--sqlite-ok-p))
  (anvil-ht-test--with-env
    (anvil-harness-telemetry-record 'no-exec :tool "alpha" :provider "claude")
    (anvil-harness-telemetry-record 'no-exec :tool "alpha" :provider "claude")
    (anvil-harness-telemetry-record 'no-exec :tool "beta"  :provider "codex")
    (anvil-harness-telemetry-record 'no-exec) ; tool / provider both nil
    (let* ((stats (anvil-harness-telemetry-stats))
           (bt (plist-get stats :by-tool))
           (bp (plist-get stats :by-provider)))
      (should (= (cdr (assoc "alpha" bt)) 2))
      (should (= (cdr (assoc "beta" bt)) 1))
      (should-not (assoc nil bt))
      (should (= (cdr (assoc "claude" bp)) 2))
      (should (= (cdr (assoc "codex" bp)) 1))
      (should-not (assoc nil bp)))))

(ert-deftest anvil-ht-test-stats-low-confidence-bucket ()
  "T16 — :low-confidence-count counts only rows below threshold."
  (skip-unless (anvil-ht-test--sqlite-ok-p))
  (anvil-ht-test--with-env
    (anvil-harness-telemetry-record 'no-exec :confidence 1.0)
    (anvil-harness-telemetry-record 'no-exec :confidence 1.0)
    (anvil-harness-telemetry-record 'reasoning :confidence 0.3)
    (anvil-harness-telemetry-record 'reasoning :confidence 0.2)
    (let ((stats (anvil-harness-telemetry-stats)))
      (should (= (plist-get stats :total) 4))
      (should (= (plist-get stats :low-confidence-count) 2)))))


;;;; --- T17-T18: recent --------------------------------------------------

(ert-deftest anvil-ht-test-recent-order-and-limit ()
  "T17 — recent rows come back DESC by timestamp + id, limit honored."
  (skip-unless (anvil-ht-test--sqlite-ok-p))
  (anvil-ht-test--with-env
    (dotimes (i 5)
      (anvil-harness-telemetry-record 'no-exec :tool (format "t%d" i)))
    (let* ((rows (anvil-harness-telemetry-recent :limit 3))
           (tools (mapcar (lambda (r) (plist-get r :tool)) rows)))
      (should (= (length rows) 3))
      (should (equal tools '("t4" "t3" "t2"))))))

(ert-deftest anvil-ht-test-recent-class-filter ()
  "T18 — :class restricts results to that class only."
  (skip-unless (anvil-ht-test--sqlite-ok-p))
  (anvil-ht-test--with-env
    (anvil-harness-telemetry-record 'no-exec)
    (anvil-harness-telemetry-record 'contract-violation)
    (anvil-harness-telemetry-record 'no-exec)
    (let* ((rows (anvil-harness-telemetry-recent :class 'no-exec))
           (classes (mapcar (lambda (r) (plist-get r :class)) rows)))
      (should (= (length rows) 2))
      (should (cl-every (lambda (c) (eq c 'no-exec)) classes)))))


;;;; --- T19-T20: dispatcher hook integration -----------------------------

(ert-deftest anvil-ht-test-hook-fires-from-macro-path ()
  "T19 — `anvil-server-with-error-handling' fires tool-error hook on errors.
The hook routes through the harness telemetry recorder which persists
the failure event."
  (skip-unless (anvil-ht-test--sqlite-ok-p))
  (anvil-ht-test--with-env
    (add-hook 'anvil-server-tool-error-hook
              #'anvil-harness-telemetry--dispatcher-hook)
    (unwind-protect
        (let ((anvil-server--current-tool-name "synthetic-tool"))
          (ignore-errors
            (anvil-server-with-error-handling
              (signal 'void-function '(no-such-fn))))
          (let* ((rows (anvil-harness-telemetry-recent :limit 5))
                 (row (car rows)))
            (should rows)
            (should (eq (plist-get row :class) 'no-exec))
            (should (equal (plist-get row :tool) "synthetic-tool"))))
      (remove-hook 'anvil-server-tool-error-hook
                   #'anvil-harness-telemetry--dispatcher-hook))))

(ert-deftest anvil-ht-test-enable-disable-installs-and-removes-hook ()
  "T20 — enable adds the dispatcher hook + registers tools, disable cleans up."
  (skip-unless (anvil-ht-test--sqlite-ok-p))
  (let ((anvil-harness-telemetry-db-path
         (make-temp-file "anvil-ht-" nil ".db"))
        (anvil-harness-telemetry--db nil)
        (anvil-harness-telemetry--resolved-db-path nil)
        (anvil-server-tool-error-hook nil))
    (unwind-protect
        (progn
          (anvil-harness-telemetry-enable)
          (should (memq #'anvil-harness-telemetry--dispatcher-hook
                        anvil-server-tool-error-hook))
          (anvil-harness-telemetry-disable)
          (should-not (memq #'anvil-harness-telemetry--dispatcher-hook
                            anvil-server-tool-error-hook)))
      (anvil-harness-telemetry--close)
      (ignore-errors
        (delete-file anvil-harness-telemetry-db-path)))))


(provide 'anvil-harness-telemetry-test)
;;; anvil-harness-telemetry-test.el ends here
