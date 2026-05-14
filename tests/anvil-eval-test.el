;;; anvil-eval-test.el --- Tests for anvil-eval helpers + NeLisp backend -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite covering two anvil-eval responsibilities:
;;
;;   1. Async eval result formatting (queue wait vs runtime split,
;;      job lifecycle plist fields).
;;
;;   2. The architecture α NeLisp interpreter backend
;;      (`anvil-eval--nelisp' + `anvil-eval--locate-nelisp-binary' +
;;      the `nelisp-eval' MCP tool registration).
;;
;; NeLisp tests skip cleanly when the `nelisp' binary cannot be
;; located so the suite is safe to ship in CI on hosts that have not
;; built the Rust runtime.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-eval)


;;;; --- async result formatting -------------------------------------------

(defun anvil-eval-test--time-ago (seconds)
  "Return a timestamp SECONDS before now."
  (time-subtract (current-time) (seconds-to-time seconds)))

(ert-deftest anvil-eval-test-result-reports-runtime-for-finished-job ()
  "Finished async jobs report runtime separately from job age."
  (let ((anvil-eval--async-jobs (make-hash-table :test 'equal)))
    (puthash
     "job-1"
     (let* ((start (anvil-eval-test--time-ago 12))
            (run-start (time-add start (seconds-to-time 10)))
            (finish (time-add run-start (seconds-to-time 0.4))))
       (list :status 'done
             :result "\"ok\""
             :expression "(progn \"ok\")"
             :start-time start
             :run-start-time run-start
             :finish-time finish
             :queue-wait-sec 10.0
             :runtime-sec 0.4))
     anvil-eval--async-jobs)
    (let ((out (anvil-eval--result "job-1")))
      (should (string-match-p "^status: done" out))
      (should (string-match-p "^elapsed: [0-9.]+s" out))
      (should (string-match-p "^age: [0-9.]+s" out))
      (should (string-match-p "^queue-wait: 10.0s" out))
      (should (string-match-p "^runtime: 0.4s" out))
      (should (string-match-p "^result: \"ok\"" out)))))

(ert-deftest anvil-eval-test-result-running-before-start-has-no-runtime ()
  "Queued async jobs have age and queue wait, but no runtime."
  (let ((anvil-eval--async-jobs (make-hash-table :test 'equal)))
    (puthash
     "job-1"
     (list :status 'running
           :result nil
           :expression "(sleep-for 1)"
           :start-time (anvil-eval-test--time-ago 3))
     anvil-eval--async-jobs)
    (let ((out (anvil-eval--result "job-1")))
      (should (string-match-p "^status: running" out))
      (should (string-match-p "^queue-wait: [0-9.]+s" out))
      (should (string-match-p "^runtime: N/A" out))
      (should (string-match-p "^result: N/A" out)))))

(ert-deftest anvil-eval-test-result-running-after-start-reports-runtime ()
  "Running async jobs report runtime since their run-start time."
  (let ((anvil-eval--async-jobs (make-hash-table :test 'equal)))
    (puthash
     "job-1"
     (let* ((start (anvil-eval-test--time-ago 4))
            (run-start (time-add start (seconds-to-time 2))))
       (list :status 'running
             :result nil
             :expression "(sleep-for 10)"
             :start-time start
             :run-start-time run-start))
     anvil-eval--async-jobs)
    (let ((out (anvil-eval--result "job-1")))
      (should (string-match-p "^status: running" out))
      (should (string-match-p "^queue-wait: 2.0s" out))
      (should (string-match-p "^runtime: [0-9.]+s" out))
      (should-not (string-match-p "^runtime: N/A" out)))))

(ert-deftest anvil-eval-test-async-records-runtime-fields ()
  "The async timer path records queue wait and runtime fields."
  (let ((anvil-eval--async-jobs (make-hash-table :test 'equal))
        (anvil-eval--async-counter 0))
    (let* ((started (anvil-eval--async "(+ 1 2)"))
           (job-id (replace-regexp-in-string "\\`Job started: " "" started))
           (deadline (+ (float-time) 1.0)))
      (while (and (< (float-time) deadline)
                  (eq 'running
                      (plist-get (gethash job-id anvil-eval--async-jobs)
                                 :status)))
        (accept-process-output nil 0.01))
      (let ((job (gethash job-id anvil-eval--async-jobs)))
        (should (eq 'done (plist-get job :status)))
        (should (numberp (plist-get job :queue-wait-sec)))
        (should (numberp (plist-get job :runtime-sec)))
        (should (equal "3" (plist-get job :result))))
      (let ((out (anvil-eval--result job-id)))
        (should (string-match-p "^queue-wait: [0-9.]+s" out))
        (should (string-match-p "^runtime: [0-9.]+s" out))))))


;;;; --- guards -------------------------------------------------------------

(defun anvil-eval-test--nelisp-bin-or-skip ()
  "Return the located `nelisp' binary path, or skip the calling test."
  (let ((anvil-eval--nelisp-binary-cache 'unset))
    (or (anvil-eval--locate-nelisp-binary)
        (ert-skip "nelisp binary not found on this host"))))


;;;; --- locate-nelisp-binary ----------------------------------------------

(ert-deftest anvil-eval-test-locate-honours-explicit-override ()
  "When `anvil-eval-nelisp-binary' points at an existing executable
the resolver returns it without consulting PATH or env vars."
  (let* ((tmp (make-temp-file "anvil-eval-test-bin-")))
    (unwind-protect
        (progn
          (set-file-modes tmp #o755)
          (let ((anvil-eval-nelisp-binary tmp)
                (anvil-eval--nelisp-binary-cache 'unset)
                (process-environment
                 (cons "NELISP_BIN=/nonexistent/should-not-be-used"
                       process-environment)))
            (should (equal tmp (anvil-eval--locate-nelisp-binary)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest anvil-eval-test-locate-honours-env-var ()
  "When `anvil-eval-nelisp-binary' is nil and $NELISP_BIN points at an
existing executable the resolver picks it up."
  (let* ((tmp (make-temp-file "anvil-eval-test-envbin-")))
    (unwind-protect
        (progn
          (set-file-modes tmp #o755)
          (let ((anvil-eval-nelisp-binary nil)
                (anvil-eval--nelisp-binary-cache 'unset)
                (process-environment
                 (cons (concat "NELISP_BIN=" tmp) process-environment)))
            (should (equal tmp (anvil-eval--locate-nelisp-binary)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest anvil-eval-test-locate-caches-result ()
  "Second call returns the same path without re-probing."
  (anvil-eval-test--nelisp-bin-or-skip)
  (let ((first (anvil-eval--locate-nelisp-binary))
        (second (anvil-eval--locate-nelisp-binary)))
    (should (equal first second))
    (should (stringp first))))


;;;; --- anvil-eval--nelisp -------------------------------------------------

(ert-deftest anvil-eval-test-nelisp-arithmetic ()
  "`(+ 1 2)' on the NeLisp interpreter returns \"3\"."
  (anvil-eval-test--nelisp-bin-or-skip)
  (should (equal "3" (anvil-eval--nelisp "(+ 1 2)"))))

(ert-deftest anvil-eval-test-nelisp-keyword-self-eval ()
  "Keywords self-evaluate on NeLisp (Phase 8.x fix shipped 2026-04-27)."
  (anvil-eval-test--nelisp-bin-or-skip)
  (should (equal ":foo" (anvil-eval--nelisp ":foo"))))

(ert-deftest anvil-eval-test-nelisp-string-len ()
  "Round-trip a string operation through the subprocess."
  (anvil-eval-test--nelisp-bin-or-skip)
  (should (equal "5" (anvil-eval--nelisp "(length \"hello\")"))))

(ert-deftest anvil-eval-test-nelisp-missing-binary-errors ()
  "When no binary can be located the eval helper signals an error.
Force the cache to a `nil' result so the resolver short-circuits to
\"not found\" without consulting PATH or env vars."
  (let ((anvil-eval--nelisp-binary-cache nil))
    (should-error (anvil-eval--nelisp "(+ 1 2)") :type 'error)))


;;;; --- tool registration --------------------------------------------------

(ert-deftest anvil-eval-test-nelisp-tool-registered ()
  "`anvil-eval-enable' registers a `nelisp-eval' tool that
`anvil-eval-disable' removes again.  Probes via the registry hash
table directly since anvil-server has no public `tool-registered-p'."
  (unwind-protect
      (progn
        (anvil-eval-enable)
        (should (gethash "nelisp-eval"
                         (anvil-server--get-server-tools
                          anvil-eval--server-id))))
    (anvil-eval-disable)
    (should-not (gethash "nelisp-eval"
                         (anvil-server--get-server-tools
                          anvil-eval--server-id)))))


(provide 'anvil-eval-test)
;;; anvil-eval-test.el ends here
