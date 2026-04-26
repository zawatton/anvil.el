;;; anvil-cron-test.el --- Tests for anvil-cron -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused contract tests for the cron scheduler's state machine.

;;; Code:

(require 'ert)
(require 'anvil-cron)

(ert-deftest anvil-cron-test-execute-clears-stale-result ()
  "Transition to `running' must drop the previous run's result so that a
concurrent status snapshot cannot pair stale output with the fresh
`running' state.  The deferred lambda queued by `run-with-timer' does
not fire while the test body holds control, which is exactly the
running window we are asserting against."
  (let ((id 'anvil-cron-test-stale))
    (unwind-protect
        (progn
          (anvil-cron-register :id id
                               :description "stale-result probe"
                               :interval 3600
                               :fn #'ignore)
          (let ((task (gethash id anvil-cron--tasks)))
            ;; Pretend a previous run left both fields populated.
            (plist-put task :last-status 'error)
            (plist-put task :last-result "prior result"))
          (let ((anvil-cron-use-worker nil))
            (anvil-cron--execute-task id))
          (let ((task (gethash id anvil-cron--tasks)))
            (should (eq (plist-get task :last-status) 'running))
            (should-not (plist-get task :last-result))))
      (anvil-cron-unregister id))))

(provide 'anvil-cron-test)
;;; anvil-cron-test.el ends here
