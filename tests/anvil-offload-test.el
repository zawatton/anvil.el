;;; anvil-offload-test.el --- Tests for anvil-offload -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Tests are run with `-L .' in the project root by the Makefile / CI,
;; so a plain require resolves to sibling sources.
(require 'anvil-offload)

(defun anvil-offload-test--reset ()
  "Force a clean REPL + pending table between tests."
  (when (anvil-offload-repl-alive-p)
    (anvil-offload-stop-repl))
  (setq anvil-offload--next-id 0
        anvil-offload--pending (make-hash-table :test 'eql))
  ;; Give Emacs a tick to reap the killed subprocess.
  (sit-for 0.05))

(defmacro anvil-offload-test--with-clean-repl (&rest body)
  "Run BODY with a fresh REPL, always cleaning up afterwards."
  (declare (indent 0))
  `(unwind-protect
       (progn (anvil-offload-test--reset) ,@body)
     (anvil-offload-test--reset)))

(ert-deftest anvil-offload-test-basic-math ()
  "`(+ 1 2)' round-trips as 3."
  (anvil-offload-test--with-clean-repl
    (let ((future (anvil-offload '(+ 1 2))))
      (should (anvil-future-p future))
      (should (eq 'pending (anvil-future--status future)))
      (should (anvil-future-await future 30))
      (should (eq 'done (anvil-future--status future)))
      (should (= 3 (anvil-future-value future))))))

(ert-deftest anvil-offload-test-string-roundtrip ()
  "Strings survive utf-8 serialization."
  (anvil-offload-test--with-clean-repl
    (let ((future (anvil-offload '(concat "藤澤" "電気"))))
      (should (anvil-future-await future 30))
      (should (equal "藤澤電気" (anvil-future-value future))))))

(ert-deftest anvil-offload-test-error-propagation ()
  "Remote errors settle the future into the `error' state."
  (anvil-offload-test--with-clean-repl
    (let ((future (anvil-offload '(/ 1 0))))
      (should (anvil-future-await future 30))
      (should (eq 'error (anvil-future--status future)))
      (should (stringp (anvil-future-error future)))
      (should-error (anvil-future-value future) :type 'error))))

(ert-deftest anvil-offload-test-multiple-concurrent ()
  "Interleaved offloads match by request-id, not arrival order."
  (anvil-offload-test--with-clean-repl
    (let ((f1 (anvil-offload '(* 2 3)))
          (f2 (anvil-offload '(* 4 5)))
          (f3 (anvil-offload '(concat "a" "b"))))
      (should (anvil-future-await f1 30))
      (should (anvil-future-await f2 30))
      (should (anvil-future-await f3 30))
      (should (= 6 (anvil-future-value f1)))
      (should (= 20 (anvil-future-value f2)))
      (should (equal "ab" (anvil-future-value f3))))))

(ert-deftest anvil-offload-test-cancel-pending ()
  "Cancelling a pending future marks it cancelled without killing the REPL."
  (anvil-offload-test--with-clean-repl
    ;; Use a real pending future: send a slow form, then cancel immediately.
    (let ((future (anvil-offload '(sleep-for 30))))
      (should (eq 'pending (anvil-future--status future)))
      (anvil-future-cancel future)
      (should (eq 'cancelled (anvil-future--status future)))
      (should-error (anvil-future-value future) :type 'error)
      ;; REPL should still be alive — cancel is local-only.
      (should (anvil-offload-repl-alive-p)))))

(ert-deftest anvil-offload-test-await-timeout ()
  "`anvil-future-await' returns nil on timeout without settling the future."
  (anvil-offload-test--with-clean-repl
    (let ((future (anvil-offload '(sleep-for 30))))
      (should-not (anvil-future-await future 0.3))
      (should (eq 'pending (anvil-future--status future))))))

(ert-deftest anvil-offload-test-repl-death-fails-pending ()
  "If the REPL dies, pending futures become errored."
  (anvil-offload-test--with-clean-repl
    (let ((future (anvil-offload '(sleep-for 60))))
      (should (eq 'pending (anvil-future--status future)))
      (anvil-offload-stop-repl)
      ;; Let the sentinel run.
      (let ((deadline (+ (float-time) 5)))
        (while (and (eq 'pending (anvil-future--status future))
                    (< (float-time) deadline))
          (sit-for 0.05)))
      (should (eq 'error (anvil-future--status future)))
      (should (stringp (anvil-future-error future))))))

(ert-deftest anvil-offload-test-pool-spreads-across-workers ()
  "With pool-size=2, two offloads land on two distinct processes."
  (anvil-offload-test--with-clean-repl
    (let ((anvil-offload-pool-size 2))
      (let* ((f1 (anvil-offload '(emacs-pid)))
             (f2 (anvil-offload '(emacs-pid))))
        (should (anvil-future-await f1 30))
        (should (anvil-future-await f2 30))
        (let ((p1 (anvil-future-value f1))
              (p2 (anvil-future-value f2)))
          (should (integerp p1))
          (should (integerp p2))
          (should-not (= p1 p2))
          ;; Pool status reflects 2 live slots.
          (let ((status (anvil-offload-pool-status)))
            (should (= 2 (length status)))
            (should (cl-every (lambda (s) (plist-get s :alive)) status))))))))

(ert-deftest anvil-offload-test-pool-round-robin-across-three-calls ()
  "A 3rd offload with pool-size=2 cycles back to the first worker."
  (anvil-offload-test--with-clean-repl
    (let ((anvil-offload-pool-size 2))
      (let* ((f1 (anvil-offload '(emacs-pid)))
             (f2 (anvil-offload '(emacs-pid)))
             (f3 (anvil-offload '(emacs-pid))))
        (should (anvil-future-await f1 30))
        (should (anvil-future-await f2 30))
        (should (anvil-future-await f3 30))
        (let ((p1 (anvil-future-value f1))
              (p2 (anvil-future-value f2))
              (p3 (anvil-future-value f3)))
          (should-not (= p1 p2))
          ;; 3rd call returns to slot used by either f1 or f2.
          (should (or (= p1 p3) (= p2 p3))))))))

(ert-deftest anvil-offload-test-pool-dead-slot-respawns ()
  "Killing one slot does not poison the pool — next dispatch respawns it."
  (anvil-offload-test--with-clean-repl
    (let ((anvil-offload-pool-size 2))
      ;; Prime both slots.
      (anvil-future-await (anvil-offload '(+ 0 0)) 30)
      (anvil-future-await (anvil-offload '(+ 0 0)) 30)
      (let* ((status-before (anvil-offload-pool-status))
             (victim-pid (plist-get (car status-before) :pid)))
        (should (integerp victim-pid))
        (kill-process (aref anvil-offload--pool 0))
        ;; Sentinel runs async; give it a moment.
        (let ((deadline (+ (float-time) 3)))
          (while (and (aref anvil-offload--pool 0)
                      (process-live-p (aref anvil-offload--pool 0))
                      (< (float-time) deadline))
            (sit-for 0.05)))
        ;; Next dispatches should respawn slot 0 eventually.
        (anvil-future-await (anvil-offload '(+ 1 1)) 30)
        (anvil-future-await (anvil-offload '(+ 1 1)) 30)
        (should (cl-every (lambda (s) (plist-get s :alive))
                          (anvil-offload-pool-status)))))))

(ert-deftest anvil-offload-test-repl-respawns-after-stop ()
  "Stopping the REPL does not prevent a subsequent `anvil-offload'."
  (anvil-offload-test--with-clean-repl
    (should (= 1 (anvil-future-value
                  (let ((f (anvil-offload '(+ 0 1))))
                    (anvil-future-await f 30)
                    f))))
    (anvil-offload-stop-repl)
    (should-not (anvil-offload-repl-alive-p))
    (let ((f (anvil-offload '(+ 9 9))))
      (should (anvil-future-await f 30))
      (should (= 18 (anvil-future-value f))))))

(provide 'anvil-offload-test)
;;; anvil-offload-test.el ends here
