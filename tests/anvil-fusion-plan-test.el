;;; anvil-fusion-plan-test.el --- ERT for anvil-fusion-plan -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for Doc 61 Phase 8a plan fusion.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(or (require 'anvil-orchestrator nil t)
    (provide 'anvil-orchestrator))
(require 'anvil-fusion-plan)

(defmacro anvil-fusion-plan-test--with-fake-orchestrator
    (member-candidates &rest body)
  "Run BODY with a faked orchestrator returning MEMBER-CANDIDATES.
Binds `submitted' to the list of submitted task-lists (newest first)."
  (declare (indent 1) (debug t))
  `(let ((submitted '())
         (calls 0))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                (lambda (tasks)
                  (push tasks submitted)
                  (setq calls (1+ calls))
                  (if (= calls 1) "b-mem" "b-judge")))
               ((symbol-function 'anvil-orchestrator-collect)
                (lambda (&rest _) t))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (id)
                  (cond
                   ((equal id "b-mem")  (list :tasks ,member-candidates))
                   ((equal id "b-judge")
                    (list :tasks '((:id "j1" :provider ollama :status done))))
                   (t (list :tasks nil)))))
               ((symbol-function 'anvil-orchestrator-extract-result)
                (lambda (id full)
                  (should (equal id "j1"))
                  (should full)
                  (list :summary "PLAN-ANSWER"))))
       ,@body)))

(defconst anvil-fusion-plan-test--cands
  '((:id "m1" :provider ollama :status done :summary "候補A: 手順1")
    (:id "m2" :provider ollama :status done :summary "候補B: 手順2")
    (:id "m3" :provider ollama :status done :summary "候補C: 手順3"))
  "Three simulated plan candidates.")

(ert-deftest anvil-fusion-plan-test-member-template-has-one-slot ()
  "The member template has exactly one `%s' slot and embeds the task text."
  (should (= 1 (cl-count ?% anvil-fusion-plan-member-template)))
  (should (string-match-p "TASK-X"
                          (format anvil-fusion-plan-member-template "TASK-X"))))

(ert-deftest anvil-fusion-plan-test-judge-template-shape ()
  "The plan judge template keeps the claims marker and two `%s' slots."
  (should (string-match-p (regexp-quote "{{CLAIMS}}")
                          anvil-fusion-plan-judge-template))
  (should (= 2 (cl-count ?% anvil-fusion-plan-judge-template))))

(ert-deftest anvil-fusion-plan-test-verify-forwards-base-template ()
  "With VERIFY non-nil, the wrapper forwards VERIFY-BASE-TEMPLATE and no template."
  (let (args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest call-args)
                 (setq args call-args)
                 '(:answer "PLAN"))))
      (let ((result (anvil-fusion-plan "Implement TASK-X" :verify t :panel 'sovereign)))
        (should (equal "PLAN" (plist-get result :plan)))
        (should (string-match-p "Implement TASK-X" (car args)))
        (should (eq 'sovereign (plist-get (cdr args) :panel)))
        (should (null (plist-get (cdr args) :template)))
        (should (equal anvil-fusion-plan-judge-template
                       (plist-get (cdr args) :verify-base-template)))))))

(ert-deftest anvil-fusion-plan-test-no-verify-forwards-substituted-template ()
  "With VERIFY nil, the wrapper passes a pre-substituted plan template."
  (let (args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest call-args)
                 (setq args call-args)
                 '(:answer "PLAN"))))
      (anvil-fusion-plan "Implement TASK-X" :verify nil)
      (should (null (plist-get (cdr args) :verify)))
      (should (string-match-p "(検証済み主張なし)"
                              (plist-get (cdr args) :template)))
      (should-not (string-match-p (regexp-quote "{{CLAIMS}}")
                                  (plist-get (cdr args) :template))))))

(ert-deftest anvil-fusion-plan-test-let-binds-repo-root-and-checker ()
  "The wrapper let-binds repo-root and prepends the repo-reality checker."
  (cl-letf (((symbol-function 'anvil-fusion-ask)
             (lambda (&rest _)
               (should (equal "/tmp/repo-root" anvil-fusion-verify-repo-root))
               (should (eq #'anvil-fusion-verify-checker-repo-reality
                           (car anvil-fusion-verify-mechanical-checkers)))
               '(:answer "PLAN"))))
    (anvil-fusion-plan "Implement TASK-X" :repo-root "/tmp/repo-root")))

(ert-deftest anvil-fusion-plan-test-verify-nil-claims-fallback-keeps-plan-template ()
  "Nil extraction under VERIFY keeps the plan judge template via VERIFY-BASE-TEMPLATE."
  (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
             (lambda (&rest _) nil)))
    (anvil-fusion-plan-test--with-fake-orchestrator
        anvil-fusion-plan-test--cands
      (let ((result (anvil-fusion-plan "Implement TASK-X" :verify t :panel 'sovereign
                                       :max-rounds 0)))
        (should (equal "PLAN-ANSWER" (plist-get result :plan)))
        (let ((jprompt (plist-get (car (car submitted)) :prompt)))
          (should (string-match-p "統合計画" jprompt))
          (should (string-match-p "(検証済み主張なし)" jprompt)))))))

(provide 'anvil-fusion-plan-test)
;;; anvil-fusion-plan-test.el ends here
