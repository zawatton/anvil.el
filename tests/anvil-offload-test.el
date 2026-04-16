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
