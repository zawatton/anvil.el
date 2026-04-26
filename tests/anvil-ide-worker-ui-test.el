;;; anvil-worker-ui-test.el --- Phase 5 UI tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `anvil-worker-ui'.  We drive the row builders and the
;; `anvil-worker-metrics' entry-point directly; no real worker
;; daemons are spawned and no interactive display is required.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-worker)
(require 'anvil-ide-worker-ui)

(defmacro anvil-worker-ui-test--with-metrics (classify latency &rest body)
  "Run BODY with metric plists bound to CLASSIFY / LATENCY."
  (declare (indent 2))
  `(let ((anvil-worker--metrics-classify ,classify)
         (anvil-worker--metrics-latency ,latency))
     ,@body))

(defun anvil-worker-ui-test--bucket (n spawn-sum wait-sum total-sum &optional totals)
  "Build a latency bucket plist from scalars."
  (list :samples       n
        :spawn-ms-sum  spawn-sum
        :wait-ms-sum   wait-sum
        :total-ms-sum  total-sum
        :totals        totals))

(ert-deftest anvil-worker-ui-test-empty-row-uses-dashes ()
  "A lane with zero samples renders numeric columns as \"-\"."
  (anvil-worker-ui-test--with-metrics nil nil
    (let* ((row (anvil-worker-ui--lane-row :read))
           (vec (cadr row)))
      (should (eq :read (car row)))
      (should (equal "read" (aref vec 0)))
      ;; pool size from defcustom — numeric string
      (should (string-match-p "\\`[0-9]+\\'" (aref vec 1)))
      (should (equal "0" (aref vec 2)))   ; N
      (should (equal "0" (aref vec 3)))   ; Classify
      (should (equal "-" (aref vec 4)))   ; Mean ms
      (should (equal "-" (aref vec 5)))   ; Spawn ms
      (should (equal "-" (aref vec 6)))   ; Wait ms
      (should (equal "-" (aref vec 7)))   ; p50
      (should (equal "-" (aref vec 8))))))  ; p99

(ert-deftest anvil-worker-ui-test-populated-row-formats-numbers ()
  "A lane with samples computes means and percentiles correctly."
  (anvil-worker-ui-test--with-metrics
      (list :read 10 :write 0 :batch 0 :unknown-fallback 0)
      (list :read  (anvil-worker-ui-test--bucket
                    4 100.0 300.0 400.0 (list 50 100 200 1000))
            :write (anvil-worker-ui-test--bucket 0 0.0 0.0 0.0 nil)
            :batch (anvil-worker-ui-test--bucket 0 0.0 0.0 0.0 nil))
    (let* ((row (anvil-worker-ui--lane-row :read))
           (vec (cadr row)))
      (should (equal "read" (aref vec 0)))
      (should (equal "4" (aref vec 2)))       ; N
      (should (equal "10" (aref vec 3)))      ; Classify
      (should (equal "100.0" (aref vec 4)))   ; Mean ms = 400/4
      (should (equal "25.0" (aref vec 5)))    ; Spawn = 100/4
      (should (equal "75.0" (aref vec 6)))    ; Wait = 300/4
      ;; p50 and p99 are integer strings drawn from the ring
      (should (string-match-p "\\`[0-9]+\\'" (aref vec 7)))
      (should (string-match-p "\\`[0-9]+\\'" (aref vec 8))))))

(ert-deftest anvil-worker-ui-test-entries-one-per-lane ()
  "`anvil-worker-ui--entries' returns one row per configured lane."
  (anvil-worker-ui-test--with-metrics nil nil
    (let ((entries (anvil-worker-ui--entries)))
      (should (= (length anvil-worker--lanes) (length entries)))
      (should (equal anvil-worker--lanes
                     (mapcar #'car entries))))))

(ert-deftest anvil-worker-ui-test-footer-shows-fallback-count ()
  "Footer line contains the classifier unknown-fallback count."
  (anvil-worker-ui-test--with-metrics
      (list :unknown-fallback 42)
      nil
    (let ((footer (anvil-worker-ui--footer)))
      (should (string-match-p "unknown-fallback: 42" footer)))))

(ert-deftest anvil-worker-ui-test-metrics-opens-buffer ()
  "`anvil-worker-metrics' creates and populates the named buffer."
  (anvil-worker-ui-test--with-metrics nil nil
    (let ((buf-name anvil-worker-ui-buffer-name))
      (when (get-buffer buf-name) (kill-buffer buf-name))
      (save-window-excursion
        (anvil-worker-metrics))
      (unwind-protect
          (with-current-buffer buf-name
            (should (derived-mode-p 'anvil-worker-ui-mode))
            (should (eq 'anvil-worker-ui--entries tabulated-list-entries))
            (should (stringp header-line-format)))
        (kill-buffer buf-name)))))

(ert-deftest anvil-worker-ui-test-reset-zeros-metrics ()
  "`anvil-worker-ui-reset' zeros both classify and latency plists."
  (let ((anvil-worker--metrics-classify (list :read 5 :unknown-fallback 2))
        (anvil-worker--metrics-latency
         (list :read  (anvil-worker-ui-test--bucket 3 30.0 60.0 90.0 '(10 20 30))
               :write (anvil-worker-ui-test--bucket 0 0.0 0.0 0.0 nil)
               :batch (anvil-worker-ui-test--bucket 0 0.0 0.0 0.0 nil))))
    (anvil-worker-ui-reset)
    (should (equal '(:read 0 :write 0 :batch 0 :unknown-fallback 0)
                   anvil-worker--metrics-classify))
    (should (zerop (plist-get
                    (plist-get anvil-worker--metrics-latency :read)
                    :samples)))))

;;; anvil-worker-ui-test.el ends here
