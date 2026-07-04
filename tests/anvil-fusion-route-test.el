;;; anvil-fusion-route-test.el --- ERT for anvil-fusion-route -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for Doc 62 Phase 2's hard-prompt router.  The
;; orchestrator seam is faked with scripted single-task results by task
;; name; escalation is tested by stubbing `anvil-fusion-ask' directly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(or (require 'anvil-orchestrator nil t)
    (provide 'anvil-orchestrator))
(require 'anvil-fusion-route)

(defmacro anvil-fusion-route-test--with-fake-orchestrator
    (script &rest body)
  "Run BODY with a scripted fake single-task orchestrator.
SCRIPT is an alist NAME -> plist describing the task result.  Each
plist may contain :status, :summary, :error, or :signal.  Binds
`submitted' to the newest-first submitted task plists."
  (declare (indent 1) (debug t))
  `(let ((submitted '())
         (batches '())
         (batch-counter 0))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                (lambda (tasks)
                  (let* ((task (car tasks))
                         (batch (format "b%d" (setq batch-counter (1+ batch-counter))))
                         (name (plist-get task :name)))
                    (push task submitted)
                    (push (cons batch (list :task-id (format "%s-id" name)
                                            :script (cdr (assoc name ,script))))
                          batches)
                    batch)))
               ((symbol-function 'anvil-orchestrator-collect)
                (lambda (&rest _) t))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (id)
                  (let* ((entry (assoc id batches))
                         (task-id (plist-get (cdr entry) :task-id)))
                    (list :tasks (and task-id (list (list :id task-id)))))))
               ((symbol-function 'anvil-orchestrator-extract-result)
                (lambda (task-id full)
                  (should full)
                  (let* ((entry (cl-find-if
                                 (lambda (cell)
                                   (equal task-id (plist-get (cdr cell) :task-id)))
                                 batches))
                         (script-plist (plist-get (cdr entry) :script)))
                    (when (plist-get script-plist :signal)
                      (signal 'error (list (plist-get script-plist :signal))))
                    (append (list :status (or (plist-get script-plist :status) 'done))
                            (and (plist-member script-plist :summary)
                                 (list :summary (plist-get script-plist :summary)))
                            (and (plist-member script-plist :error)
                                 (list :error (plist-get script-plist :error))))))))
       ,@body)))

(ert-deftest anvil-fusion-route-test-parse-risk-exact ()
  (should (equal '(:risk low :reason "looks fine")
                 (anvil-fusion-route--parse-risk
                  "RISK: LOW\nREASON: looks fine"))))

(ert-deftest anvil-fusion-route-test-parse-risk-lowercase-and-prose ()
  (should (equal '(:risk high :reason "many constraints")
                 (anvil-fusion-route--parse-risk
                  "audit follows\nrisk: high\nreason: many constraints\nthanks"))))

(ert-deftest anvil-fusion-route-test-parse-risk-garbage-falls-to-medium ()
  (should (equal '(:risk medium :reason "")
                 (anvil-fusion-route--parse-risk "nonsense only"))))

(ert-deftest anvil-fusion-route-test-parse-risk-missing-reason-empty ()
  (should (equal '(:risk medium :reason "")
                 (anvil-fusion-route--parse-risk "RISK: medium"))))

(ert-deftest anvil-fusion-route-test-decide-defaults ()
  (should (equal '(:tier single)
                 (anvil-fusion-route--decide 'low)))
  (should (equal '(:tier panel :panel claude-pair :verify nil)
                 (anvil-fusion-route--decide 'medium)))
  (should (equal '(:tier panel :panel opus-solo :verify t)
                 (anvil-fusion-route--decide 'high)))
  (should (equal '(:tier panel :panel claude-pair :verify nil)
                 (anvil-fusion-route--decide 'weird))))

(ert-deftest anvil-fusion-route-test-single-path-low-risk-skips-ask ()
  (let ((anvil-fusion-route--log nil)
        ask-called)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest _) (setq ask-called t) (error "must not run"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SINGLE-ANSWER")
            ("fusion-route-probe" :status done :summary "RISK: LOW\nREASON: stable"))
        (let ((result (anvil-fusion-route-ask "Q")))
          (should (equal "SINGLE-ANSWER" (plist-get result :answer)))
          (should (eq 'single (plist-get (plist-get result :route) :tier)))
          (should (eq 'low (plist-get (plist-get result :route) :risk)))
          (should-not ask-called)
          (should (= 1 (length (anvil-fusion-route-log)))))))))

(ert-deftest anvil-fusion-route-test-medium-path-escalates-to-claude-pair ()
  (let ((anvil-fusion-route--log nil)
        ask-args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest args)
                 (setq ask-args args)
                 '(:answer "PANEL-MED" :panel claude-pair :verify nil :extra-tag t))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SINGLE-ANSWER")
            ("fusion-route-probe" :status done :summary "RISK: MEDIUM\nREASON: maybe wrong"))
        (let ((result (anvil-fusion-route-ask "Q" :extra "X")))
          (should (equal "PANEL-MED" (plist-get result :answer)))
          (should (eq 'claude-pair (plist-get (cdr ask-args) :panel)))
          (should-not (plist-get (cdr ask-args) :verify))
          (should (equal "SINGLE-ANSWER"
                         (plist-get (plist-get result :route) :single-answer)))
          (should (eq 'panel (plist-get (plist-get result :route) :tier))))))))

(ert-deftest anvil-fusion-route-test-high-path-escalates-to-opus-solo-verify ()
  (let (ask-args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest args)
                 (setq ask-args args)
                 '(:answer "PANEL-HIGH"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SINGLE-ANSWER")
            ("fusion-route-probe" :status done :summary "RISK: HIGH\nREASON: critical"))
        (let ((result (anvil-fusion-route-ask "Q")))
          (should (equal "PANEL-HIGH" (plist-get result :answer)))
          (should (eq 'opus-solo (plist-get (cdr ask-args) :panel)))
          (should (plist-get (cdr ask-args) :verify))
          (should (eq 'high (plist-get (plist-get result :route) :risk))))))))

(ert-deftest anvil-fusion-route-test-forced-path-skips-single-and-probe ()
  (let ((anvil-fusion-route-force-hard-regexps '("CRITICAL"))
        ask-args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest args)
                 (setq ask-args args)
                 '(:answer "FORCED"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SHOULD-NOT-RUN")
            ("fusion-route-probe" :status done :summary "SHOULD-NOT-RUN"))
        (let ((result (anvil-fusion-route-ask "CRITICAL: do the hard thing")))
          (should (equal "FORCED" (plist-get result :answer)))
          (should (eq 'forced (plist-get (plist-get result :route) :risk)))
          (should (eq 'opus-solo (plist-get (cdr ask-args) :panel)))
          (should-not (cl-find-if
                       (lambda (task)
                         (equal "fusion-route-single" (plist-get task :name)))
                       submitted)))))))

(ert-deftest anvil-fusion-route-test-single-ask-failure-escalates-high ()
  (let (ask-args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest args)
                 (setq ask-args args)
                 '(:answer "FALLBACK"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status failed :error "boom"))
        (let ((result (anvil-fusion-route-ask "Q")))
          (should (equal "FALLBACK" (plist-get result :answer)))
          (should (eq 'opus-solo (plist-get (cdr ask-args) :panel)))
          (should (plist-get (cdr ask-args) :verify))
          (should (eq 'high (plist-get (plist-get result :route) :risk)))
          (should (string-match-p "failed"
                                  (plist-get (plist-get result :route) :reason))))))))

(ert-deftest anvil-fusion-route-test-probe-failure-falls-to-medium ()
  (let (ask-args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest args)
                 (setq ask-args args)
                 '(:answer "PROBE-FAIL"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SINGLE-ANSWER")
            ("fusion-route-probe" :signal "probe broke"))
        (let ((result (anvil-fusion-route-ask "Q")))
          (should (equal "PROBE-FAIL" (plist-get result :answer)))
          (should (eq 'claude-pair (plist-get (cdr ask-args) :panel)))
          (should-not (plist-get (cdr ask-args) :verify))
          (should (eq 'medium (plist-get (plist-get result :route) :risk))))))))

(ert-deftest anvil-fusion-route-test-probe-garbage-falls-to-medium ()
  (let (ask-args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest args)
                 (setq ask-args args)
                 '(:answer "PROBE-GARBAGE"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SINGLE-ANSWER")
            ("fusion-route-probe" :status done :summary "unstructured prose"))
        (let ((result (anvil-fusion-route-ask "Q")))
          (should (equal "PROBE-GARBAGE" (plist-get result :answer)))
          (should (eq 'claude-pair (plist-get (cdr ask-args) :panel)))
          (should (eq 'medium (plist-get (plist-get result :route) :risk))))))))

(ert-deftest anvil-fusion-route-test-log-ring-clamps-to-size ()
  (let ((anvil-fusion-route--log nil)
        (anvil-fusion-route-log-size 2))
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest _) '(:answer "PANEL"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SINGLE-ANSWER")
            ("fusion-route-probe" :status done :summary "RISK: LOW\nREASON: ok"))
        (anvil-fusion-route-ask "Q1")
        (anvil-fusion-route-ask "Q2")
        (anvil-fusion-route-ask "Q3")
        (let ((log (anvil-fusion-route-log)))
          (should (= 2 (length log)))
          (should (equal "Q3" (plist-get (car log) :prompt-head)))
          (should (equal "Q2" (plist-get (cadr log) :prompt-head))))))))

(provide 'anvil-fusion-route-test)
;;; anvil-fusion-route-test.el ends here
