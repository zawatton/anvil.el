;;; anvil-orchestrator-test.el --- Tests for anvil-orchestrator -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for `anvil-orchestrator'.  A lightweight `test'
;; provider spawned via /bin/sh + printf emits fake stream-json so
;; no real claude CLI is needed.  One live smoke test at the bottom
;; exercises the real claude CLI under ANVIL_ALLOW_LIVE=1.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil-state)
(require 'anvil-orchestrator)

;;;; --- fixtures -----------------------------------------------------------

(defvar anvil-orchestrator-test--stream-json
  "{\"type\":\"assistant\",\"message\":{\"content\":\"working\"}}\n{\"type\":\"result\",\"result\":\"stub task done\",\"total_cost_usd\":0.00042,\"usage\":{\"input_tokens\":123,\"output_tokens\":45,\"cache_read_input_tokens\":0}}"
  "Pre-baked stream-json output for the `test' provider.")

(defun anvil-orchestrator-test--register-stub (&optional stream-json exit-code)
  "Register / overwrite the `test' provider with STREAM-JSON output.
EXIT-CODE overrides the sh exit status (default 0)."
  (let ((payload (or stream-json anvil-orchestrator-test--stream-json))
        (code    (or exit-code 0)))
    (anvil-orchestrator-register-provider
     'test
     :cli "sh"
     :version-check (lambda () t)
     :build-cmd (lambda (_task)
                  (list "sh" "-c"
                        (format "printf '%%s\\n' %s; exit %d"
                                (shell-quote-argument payload) code)))
     :parse-output #'anvil-orchestrator--claude-parse-output
     :supports-tool-use             t
     :supports-worktree             nil
     :supports-budget               nil
     :supports-system-prompt-append nil
     :default-model                 "stub"
     :cost-estimator (lambda (_task) 0.0))))

(defun anvil-orchestrator-test--register-slow (sleep-sec)
  "Register a slow `slow' provider that sleeps SLEEP-SEC before printing."
  (anvil-orchestrator-register-provider
   'slow
   :cli "sh"
   :version-check (lambda () t)
   :build-cmd (lambda (_task)
                (list "sh" "-c"
                      (format "sleep %s; printf '%%s\\n' %s"
                              (number-to-string sleep-sec)
                              (shell-quote-argument
                               anvil-orchestrator-test--stream-json))))
   :parse-output #'anvil-orchestrator--claude-parse-output
   :default-model "slow"))

(defmacro anvil-orchestrator-test--with-fresh (&rest body)
  "Run BODY inside a freshly initialised orchestrator + state DB."
  (declare (indent 0))
  `(let ((anvil-state-db-path (make-temp-file "anvil-orch-st-" nil ".db"))
         (anvil-state--db nil)
         (anvil-orchestrator-work-dir
          (make-temp-file "anvil-orch-work-" t))
         (anvil-orchestrator--tasks (make-hash-table :test 'equal))
         (anvil-orchestrator--running (make-hash-table :test 'equal))
         (anvil-orchestrator--batches (make-hash-table :test 'equal))
         (anvil-orchestrator--queue nil)
         (anvil-orchestrator--pump-timer nil)
         (anvil-orchestrator-poll-interval-sec 0.1))
     (unwind-protect
         (progn
           (anvil-state-enable)
           (anvil-orchestrator-test--register-stub)
           ,@body)
       (anvil-orchestrator--cancel-pump-timer)
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path))
       (ignore-errors
         (delete-directory anvil-orchestrator-work-dir t)))))

(defun anvil-orchestrator-test--wait-batch (batch-id &optional max-sec)
  "Block until BATCH-ID is terminal or MAX-SEC (default 10s) elapses."
  (let ((deadline (+ (float-time) (or max-sec 10))))
    (while (and (not (anvil-orchestrator--batch-terminal-p batch-id))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))))

;;;; --- provider registration ---------------------------------------------

(ert-deftest anvil-orchestrator-test-claude-provider-registered ()
  "Built-in `claude' provider is registered on load."
  (let ((prov (anvil-orchestrator--provider 'claude)))
    (should (anvil-orchestrator-provider-p prov))
    (should (equal "claude" (anvil-orchestrator-provider-cli prov)))
    (should (anvil-orchestrator-provider-supports-tool-use prov))))

(ert-deftest anvil-orchestrator-test-unknown-provider-errors ()
  (should-error (anvil-orchestrator--provider 'no-such)
                :type 'user-error))

;;;; --- validation --------------------------------------------------------

(ert-deftest anvil-orchestrator-test-submit-rejects-empty ()
  (anvil-orchestrator-test--with-fresh
    (should-error (anvil-orchestrator-submit nil) :type 'user-error)))

(ert-deftest anvil-orchestrator-test-submit-rejects-duplicate-name ()
  (anvil-orchestrator-test--with-fresh
    (should-error
     (anvil-orchestrator-submit
      (list (list :name "a" :provider 'test :prompt "hi")
            (list :name "a" :provider 'test :prompt "yo")))
     :type 'user-error)))

(ert-deftest anvil-orchestrator-test-submit-rejects-missing-prompt ()
  (anvil-orchestrator-test--with-fresh
    (should-error
     (anvil-orchestrator-submit
      (list (list :name "a" :provider 'test)))
     :type 'user-error)))

(ert-deftest anvil-orchestrator-test-submit-rejects-batch-over-cap ()
  (anvil-orchestrator-test--with-fresh
    (let ((anvil-orchestrator-batch-max-tasks 2))
      (should-error
       (anvil-orchestrator-submit
        (list (list :name "a" :provider 'test :prompt "x")
              (list :name "b" :provider 'test :prompt "y")
              (list :name "c" :provider 'test :prompt "z")))
       :type 'user-error))))

(ert-deftest anvil-orchestrator-test-submit-rejects-batch-budget ()
  (anvil-orchestrator-test--with-fresh
    (let ((anvil-orchestrator-batch-budget-usd-total 0.50))
      (should-error
       (anvil-orchestrator-submit
        (list (list :name "a" :provider 'test :prompt "x" :budget-usd 0.40)
              (list :name "b" :provider 'test :prompt "y" :budget-usd 0.40)))
       :type 'user-error))))

;;;; --- happy path --------------------------------------------------------

(ert-deftest anvil-orchestrator-test-submit-and-collect-one-task ()
  "Single task runs to done and surfaces summary + cost."
  (anvil-orchestrator-test--with-fresh
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "single" :provider 'test
                               :prompt "hello"))))
           (_     (anvil-orchestrator-test--wait-batch batch))
           (results (anvil-orchestrator-collect batch)))
      (should (= 1 (length results)))
      (let ((r (car results)))
        (should (eq 'done (plist-get r :status)))
        (should (equal "single" (plist-get r :name)))
        (should (equal "stub task done" (plist-get r :summary)))
        (should (= 0.00042 (plist-get r :cost-usd)))))))

(ert-deftest anvil-orchestrator-test-status-batch-counts ()
  "`orchestrator-status' on a batch id reports counts + slim task list."
  (anvil-orchestrator-test--with-fresh
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "a" :provider 'test :prompt "p")
                         (list :name "b" :provider 'test :prompt "q")))))
      (anvil-orchestrator-test--wait-batch batch)
      (let ((st (anvil-orchestrator-status batch)))
        (should (equal batch (plist-get st :batch-id)))
        (should (= 2 (plist-get st :total)))
        (should (= 2 (plist-get st :done)))
        (should (= 0 (plist-get st :failed)))
        (should (= 2 (length (plist-get st :tasks))))))))

(ert-deftest anvil-orchestrator-test-collect-wait-blocks-until-done ()
  (anvil-orchestrator-test--with-fresh
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "a" :provider 'test :prompt "p"))))
           (results (anvil-orchestrator-collect batch :wait t
                                                :max-wait-sec 10)))
      (should (eq 'done (plist-get (car results) :status))))))

;;;; --- failure / retry ---------------------------------------------------

(ert-deftest anvil-orchestrator-test-non-zero-exit-marks-failed ()
  "Provider exit != 0 surfaces as status=failed with error string."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-stub "" 3)
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "bad" :provider 'test :prompt "p")))))
      (anvil-orchestrator-test--wait-batch batch)
      (let ((r (car (anvil-orchestrator-collect batch))))
        (should (eq 'failed (plist-get r :status)))
        (should (string-match-p "exit" (plist-get r :error)))))))

(ert-deftest anvil-orchestrator-test-retry-creates-new-task ()
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-stub "" 1)
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "flaky" :provider 'test
                               :prompt "p" :auto-retry nil)))))
      (anvil-orchestrator-test--wait-batch batch)
      (let* ((orig (car (anvil-orchestrator-collect batch)))
             (new-id (anvil-orchestrator-retry (plist-get orig :id))))
        (should (stringp new-id))
        (should-not (equal new-id (plist-get orig :id)))
        (let ((new-task (anvil-orchestrator--task-get new-id)))
          (should (equal (plist-get orig :id) (plist-get new-task :retry-of))))))))

;;;; --- concurrency + queuing ---------------------------------------------

(ert-deftest anvil-orchestrator-test-concurrency-cap-respected ()
  "With cap=1, only one task runs at a time."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-slow 0.3)
    (let ((anvil-orchestrator-concurrency 1)
          (anvil-orchestrator-per-provider-concurrency
           '((slow . 1))))
      (let ((batch (anvil-orchestrator-submit
                    (list (list :name "a" :provider 'slow :prompt "x")
                          (list :name "b" :provider 'slow :prompt "y")))))
        ;; give the pump a tick
        (accept-process-output nil 0.2)
        (should (<= (anvil-orchestrator--running-count) 1))
        (anvil-orchestrator-test--wait-batch batch 15)
        (let ((st (anvil-orchestrator-status batch)))
          (should (= 2 (plist-get st :done))))))))

;;;; --- cancel ------------------------------------------------------------

(ert-deftest anvil-orchestrator-test-cancel-queued-task ()
  "Cancel before spawn sets status=cancelled without spawning."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-slow 5)
    (let ((anvil-orchestrator-concurrency 1))
      ;; Running one slow task to block the pool
      (let* ((batch (anvil-orchestrator-submit
                     (list (list :name "blocker" :provider 'slow :prompt "x")
                           (list :name "waits"   :provider 'slow :prompt "y"))))
             (ids (gethash batch anvil-orchestrator--batches))
             (queued-id (cadr ids)))
        (accept-process-output nil 0.1)
        (should (anvil-orchestrator-cancel queued-id))
        (should (eq 'cancelled
                    (plist-get (anvil-orchestrator--task-get queued-id)
                               :status)))
        ;; cleanup: cancel the running one too so fresh macro can clean up
        (anvil-orchestrator-cancel (car ids))
        (anvil-orchestrator-test--wait-batch batch 10)))))

;;;; --- state persistence -------------------------------------------------

(ert-deftest anvil-orchestrator-test-state-round-trip ()
  "Completed tasks read back through anvil-state."
  (anvil-orchestrator-test--with-fresh
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "persist" :provider 'test :prompt "p")))))
      (anvil-orchestrator-test--wait-batch batch)
      ;; blow away in-memory state, keep on-disk DB
      (clrhash anvil-orchestrator--tasks)
      (clrhash anvil-orchestrator--batches)
      (setq anvil-orchestrator--queue nil)
      (anvil-orchestrator--restore-from-state)
      (let ((id (car (gethash batch anvil-orchestrator--batches))))
        (should id)
        (should (eq 'done (plist-get (anvil-orchestrator--task-get id)
                                     :status)))))))

(ert-deftest anvil-orchestrator-test-state-running-becomes-failed-on-restore ()
  "Running tasks are reset to failed on restore (daemon restart interrupt)."
  (anvil-orchestrator-test--with-fresh
    (let* ((id (anvil-orchestrator--uuid))
           (task (list :id id :batch-id "b1" :name "orphan"
                       :provider 'test :prompt "x" :status 'running
                       :submitted-at (float-time))))
      (anvil-orchestrator--persist task)
      (clrhash anvil-orchestrator--tasks)
      (anvil-orchestrator--restore-from-state)
      (let ((restored (anvil-orchestrator--task-get id)))
        (should (eq 'failed (plist-get restored :status)))
        (should (string-match-p "daemon restart"
                                (plist-get restored :error)))))))

;;;; --- parser -------------------------------------------------------------

(ert-deftest anvil-orchestrator-test-parse-stream-json-extracts-usage ()
  (let* ((tmp-out (make-temp-file "anvil-orch-out-" nil ".jsonl"))
         (tmp-err (make-temp-file "anvil-orch-err-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file tmp-out
            (insert anvil-orchestrator-test--stream-json))
          (let ((parsed (anvil-orchestrator--claude-parse-output
                         tmp-out tmp-err 0)))
            (should (equal "stub task done" (plist-get parsed :summary)))
            (should (= 0.00042 (plist-get parsed :cost-usd)))
            (should (= 123 (plist-get (plist-get parsed :cost-tokens)
                                      :input)))))
      (delete-file tmp-out)
      (delete-file tmp-err))))

(ert-deftest anvil-orchestrator-test-parse-stream-json-truncates-long-summary ()
  (let* ((tmp-out (make-temp-file "anvil-orch-out-" nil ".jsonl"))
         (tmp-err (make-temp-file "anvil-orch-err-" nil ".txt"))
         (anvil-orchestrator-summary-max-chars 20)
         (long (make-string 500 ?a)))
    (unwind-protect
        (progn
          (with-temp-file tmp-out
            (insert (json-encode
                     `(:type "result" :result ,long
                             :total_cost_usd 0.01))))
          (let* ((parsed (anvil-orchestrator--claude-parse-output
                          tmp-out tmp-err 0))
                 (s (plist-get parsed :summary)))
            (should (= 21 (length s)))))
      (delete-file tmp-out)
      (delete-file tmp-err))))

;;;; --- live smoke test ---------------------------------------------------

(ert-deftest anvil-orchestrator-test-live-claude ()
  "Submit a tiny task via the real claude CLI (ANVIL_ALLOW_LIVE=1)."
  (skip-unless (and (getenv "ANVIL_ALLOW_LIVE")
                    (executable-find "claude")))
  (anvil-orchestrator-test--with-fresh
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "live-smoke"
                               :provider 'claude
                               :model "haiku"
                               :prompt "Respond with exactly: PONG")))))
      (anvil-orchestrator-test--wait-batch batch 120)
      (let ((r (car (anvil-orchestrator-collect batch))))
        (should (eq 'done (plist-get r :status)))
        (should (stringp (plist-get r :summary)))))))

(provide 'anvil-orchestrator-test)
;;; anvil-orchestrator-test.el ends here
