;;; anvil-orchestrator-openrouter-safety-test.el --- OpenRouter fan-out tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-orchestrator)
(require 'anvil-orchestrator-openrouter)

(defmacro anvil-orchestrator-openrouter-safety-test--isolated (&rest body)
  "Run BODY with isolated in-memory orchestrator state."
  (declare (indent 0))
  `(let ((anvil-orchestrator--tasks (make-hash-table :test 'equal))
         (anvil-orchestrator--running (make-hash-table :test 'equal))
         (anvil-orchestrator--batches (make-hash-table :test 'equal))
         (anvil-orchestrator--consensus-groups (make-hash-table :test 'equal))
         (anvil-orchestrator--provider-launch-deferred-until
          (make-hash-table :test 'eq))
         (anvil-orchestrator--queue nil)
         (anvil-orchestrator--pump-timer nil)
         (anvil-orchestrator-concurrency 10)
         (anvil-orchestrator-per-provider-concurrency
          '((openrouter . 3) (openrouter-generic-test . 3))))
     ,@body))

(ert-deftest anvil-orchestrator-openrouter-default-concurrency-is-three ()
  (should (= 3 (alist-get 'openrouter
                          (default-value
                           'anvil-orchestrator-per-provider-concurrency)))))

(ert-deftest anvil-orchestrator-openrouter-quota-admission-covers-limits ()
  (cl-labels
      ((admission (local &optional remote)
         (cl-letf (((symbol-function 'anvil-orchestrator-openrouter-quota)
                    (lambda () (list :remote remote :local local))))
           (anvil-orchestrator-openrouter--launch-capacity))))
    (should (equal '(:capacity 3)
                   (admission '(:remaining_daily 7 :remaining_minute 3
                                :blocked_seconds 0 :last_status "success"))))
    (should (equal '(:capacity 0 :retry-after-sec 60)
                   (admission '(:remaining_daily 7 :remaining_minute 0
                                :blocked_seconds 0 :last_status "success"))))
    (should (plist-get
             (admission '(:remaining_daily 7 :remaining_minute 3
                          :blocked_seconds 42 :last_status "rate_limited"))
             :terminal-reason))
    ;; Once the cooldown has elapsed the stale "rate_limited" status must not
    ;; keep the provider shut: nothing but a worker run can clear that status,
    ;; and this preflight is what decides whether a worker runs at all.
    (should (equal '(:capacity 3)
                   (admission '(:remaining_daily 7 :remaining_minute 3
                                :blocked_seconds 0 :last_status "rate_limited"))))
    ;; Without a usable cooldown reading, the status still has to be believed.
    (should (plist-get
             (admission '(:remaining_daily 7 :remaining_minute 3
                          :blocked_seconds nil :last_status "rate_limited"))
             :terminal-reason))
    (should (plist-get
             (admission '(:remaining_daily 0 :remaining_minute 3
                          :blocked_seconds 0 :last_status "success"))
             :terminal-reason))
    (should (plist-get
             (admission '(:remaining_daily 7 :remaining_minute 3
                          :blocked_seconds 0 :last_status "success")
                        '(:limit_remaining 0))
             :terminal-reason))))

(ert-deftest anvil-orchestrator-openrouter-pump-consumes-capacity-concurrently ()
  (anvil-orchestrator-openrouter-safety-test--isolated
    (let ((launched nil) (deferred nil))
      (anvil-orchestrator-register-provider
       'openrouter-generic-test :default-model "generic")
      (dolist (task '((:id "o1" :provider openrouter :status queued)
                      (:id "o2" :provider openrouter :status queued)
                      (:id "o3" :provider openrouter :status queued)
                      (:id "g1" :provider openrouter-generic-test :status queued)))
        (puthash (plist-get task :id) task anvil-orchestrator--tasks)
        (setq anvil-orchestrator--queue
              (append anvil-orchestrator--queue (list (plist-get task :id)))))
      (unwind-protect
          (cl-letf (((symbol-function 'anvil-orchestrator-openrouter-quota)
                     (lambda ()
                       '(:remote (:limit_remaining 99)
                         :local (:remaining_daily 9 :remaining_minute 2
                                 :blocked_seconds 0 :last_status "success"))))
                    ((symbol-function 'anvil-orchestrator--budget-block-reason)
                     (lambda (_task) nil))
                    ((symbol-function 'anvil-orchestrator--spawn)
                     (lambda (task) (push (plist-get task :id) launched)))
                    ((symbol-function 'anvil-orchestrator--defer-provider-launch)
                     (lambda (provider seconds)
                       (push (cons provider seconds) deferred))))
            (anvil-orchestrator--pump)
            (should (equal '("o1" "o2" "g1") (nreverse launched)))
            (should (equal '("o3") anvil-orchestrator--queue))
            (should (equal 'openrouter (caar deferred)))
            (should (= 5 (cdar deferred))))
        (remhash 'openrouter-generic-test anvil-orchestrator--providers)))))

(ert-deftest anvil-orchestrator-openrouter-minute-limit-wakes-and-429-fails ()
  (anvil-orchestrator-openrouter-safety-test--isolated
    (let ((task '(:id "waiting" :provider openrouter :status queued))
          deferred)
      (puthash "waiting" task anvil-orchestrator--tasks)
      (setq anvil-orchestrator--queue '("waiting"))
      (cl-letf (((symbol-function 'anvil-orchestrator-openrouter-quota)
                 (lambda ()
                   '(:remote (:limit_remaining 99)
                     :local (:remaining_daily 9 :remaining_minute 0
                             :blocked_seconds 0 :last_status "success"))))
                ((symbol-function 'anvil-orchestrator--budget-block-reason)
                 (lambda (_task) nil))
                ((symbol-function 'anvil-orchestrator--defer-provider-launch)
                 (lambda (provider seconds)
                   (setq deferred (cons provider seconds))))
                ((symbol-function 'anvil-orchestrator--spawn)
                 (lambda (_task) (ert-fail "minute-limited task launched"))))
        (anvil-orchestrator--pump))
      (should (equal '(openrouter . 60) deferred))
      (should (equal '("waiting") anvil-orchestrator--queue))
      (setq anvil-orchestrator--queue '("waiting"))
      (cl-letf (((symbol-function 'anvil-orchestrator-openrouter-quota)
                 (lambda ()
                   '(:remote (:limit_remaining 99)
                     :local (:remaining_daily 9 :remaining_minute 9
                             :blocked_seconds 58 :last_status "rate_limited"))))
                ((symbol-function 'anvil-orchestrator--budget-block-reason)
                 (lambda (_task) nil))
                ((symbol-function 'anvil-orchestrator--spawn)
                 (lambda (_task) (ert-fail "rate-limited task launched"))))
        (anvil-orchestrator--pump))
      (should (eq 'failed (plist-get (gethash "waiting" anvil-orchestrator--tasks)
                                     :status)))
      (should (string-match-p
               "429/cooldown"
               (plist-get (gethash "waiting" anvil-orchestrator--tasks) :error))))))

(ert-deftest anvil-orchestrator-openrouter-submit-deduplicates-worker-input ()
  (anvil-orchestrator-openrouter-safety-test--isolated
    (cl-letf (((symbol-function 'anvil-state-enable) #'ignore)
              ((symbol-function 'anvil-orchestrator--ensure-work-dir) #'ignore)
              ((symbol-function 'anvil-orchestrator--ensure-pump-timer) #'ignore)
              ((symbol-function 'anvil-orchestrator--pump) #'ignore)
              ((symbol-function 'anvil-orchestrator--persist)
               (lambda (task)
                 (puthash (plist-get task :id) task anvil-orchestrator--tasks)
                 task)))
      (let* ((batch (anvil-orchestrator-submit
                     '((:name "first" :provider openrouter :prompt "same")
                       (:name "second" :provider openrouter :prompt "same"))))
             (ids (gethash batch anvil-orchestrator--batches))
             (first (gethash (car ids) anvil-orchestrator--tasks))
             (second (gethash (cadr ids) anvil-orchestrator--tasks)))
        (should-not (plist-get first :alias-of))
        (should (equal (car ids) (plist-get second :alias-of)))))))

(ert-deftest anvil-orchestrator-openrouter-consensus-reuses-identical-result ()
  (anvil-orchestrator-openrouter-safety-test--isolated
    (let (result)
      (cl-letf (((symbol-function 'anvil-state-enable) #'ignore)
                ((symbol-function 'anvil-orchestrator--ensure-work-dir) #'ignore)
                ((symbol-function 'anvil-orchestrator--ensure-pump-timer) #'ignore)
                ((symbol-function 'anvil-orchestrator--pump) #'ignore)
                ((symbol-function 'anvil-orchestrator--consensus-persist) #'ignore)
                ((symbol-function 'anvil-orchestrator--persist)
                 (lambda (task)
                   (puthash (plist-get task :id) task anvil-orchestrator--tasks)
                   task)))
        (setq result (anvil-orchestrator-submit-consensus
                      :prompt "same" :providers '(openrouter openrouter))))
      (let* ((ids (plist-get result :task-ids))
             (first (gethash (car ids) anvil-orchestrator--tasks))
             (second (gethash (cadr ids) anvil-orchestrator--tasks)))
        (should (equal "consensus-openrouter" (plist-get first :name)))
        (should (equal "consensus-openrouter-2" (plist-get second :name)))
        (should (equal (car ids) (plist-get second :alias-of)))
        (setq first (plist-put first :status 'done))
        (setq first (plist-put first :summary "shared result"))
        (puthash (car ids) first anvil-orchestrator--tasks)
        (setq anvil-orchestrator--queue (list (cadr ids)))
        (cl-letf (((symbol-function 'anvil-orchestrator--spawn)
                   (lambda (_task) (ert-fail "alias launched a worker"))))
          (anvil-orchestrator--pump))
        (setq second (gethash (cadr ids) anvil-orchestrator--tasks))
        (should (eq 'done (plist-get second :status)))
        (should (equal "shared result" (plist-get second :summary)))
        (should (equal (car ids) (plist-get second :aliased-from)))))))

(ert-deftest anvil-orchestrator-openrouter-disables-all-retry-paths ()
  (anvil-orchestrator-openrouter-safety-test--isolated
    (let ((task '(:id "failed" :name "failed" :provider openrouter
                  :status failed :auto-retry-code 429 :retry-count 0)))
      (puthash "failed" task anvil-orchestrator--tasks)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _args) (ert-fail "automatic retry scheduled"))))
        (anvil-orchestrator--maybe-auto-retry "failed"))
      (should-error (anvil-orchestrator-retry "failed") :type 'user-error))))

(ert-deftest anvil-orchestrator-openrouter-worker-finishes-before-watchdog ()
  (let ((anvil-orchestrator-work-dir (make-temp-file "anvil-or-timeout-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'anvil-orchestrator-openrouter--check)
                   (lambda () t))
                  ((symbol-function 'anvil-orchestrator-openrouter--nelisp)
                   (lambda () "/bin/true"))
                  ((symbol-function 'anvil-orchestrator-openrouter--nelisp-root)
                   (lambda () "/tmp/nelisp")))
          (let* ((command (anvil-orchestrator-openrouter--build-cmd
                           '(:prompt "bounded" :timeout-sec 240)))
                 (tail (member "--timeout" command)))
            (should (equal "230" (cadr tail)))))
      (delete-directory anvil-orchestrator-work-dir t))))

(ert-deftest anvil-orchestrator-openrouter-context-only-is-per-task ()
  "Read-only tools stay the default, and a task can ask for write tools.
A worker without `terminal' / `file' cannot implement anything, and on
2026-09-23 one asked to read a file answered with an invented line
count rather than refusing, so which tools a task gets has to be
decided per task rather than fixed in the command."
  (let ((anvil-orchestrator-work-dir (make-temp-file "anvil-or-ctx-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'anvil-orchestrator-openrouter--check)
                   (lambda () t))
                  ((symbol-function 'anvil-orchestrator-openrouter--nelisp)
                   (lambda () "/bin/true"))
                  ((symbol-function 'anvil-orchestrator-openrouter--nelisp-root)
                   (lambda () "/tmp/nelisp")))
          (cl-labels ((flag-p (task)
                        (and (member "--context-only"
                                     (anvil-orchestrator-openrouter--build-cmd task))
                             t)))
            (let ((anvil-orchestrator-openrouter-context-only t))
              (should (flag-p '(:prompt "bounded")))
              (should-not (flag-p '(:prompt "bounded" :context-only nil)))
              (should-not (flag-p '(:prompt "bounded" :context_only nil)))
              (should (flag-p '(:prompt "bounded" :context-only t))))
            (let ((anvil-orchestrator-openrouter-context-only nil))
              (should-not (flag-p '(:prompt "bounded")))
              (should (flag-p '(:prompt "bounded" :context-only t))))))
      (delete-directory anvil-orchestrator-work-dir t))))

(provide 'anvil-orchestrator-openrouter-safety-test)
;;; anvil-orchestrator-openrouter-safety-test.el ends here
