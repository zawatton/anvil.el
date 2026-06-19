;;; anvil-fusion-ask-test.el --- ERT for anvil-fusion Phase 3 -*- lexical-binding: t; -*-

;;; Commentary:
;; Single-round wiring tests for `anvil-fusion-ask' (pinned with
;; :max-rounds 0 so the Phase 4 critique loop never fires here — looping
;; is covered in anvil-fusion-loop-test.el).  anvil-orchestrator is faked:
;; we `provide' the feature and cl-letf the four public functions the ask
;; path uses, simulating a fan-out + judge without any live model.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-fusion)
(require 'anvil-fusion-panels)
(or (require 'anvil-orchestrator nil t) ;; load the real module when present (anvil.el)
    (provide 'anvil-orchestrator))  ;; else fake it (fusion standalone repo)
(require 'anvil-fusion-ask)

(defmacro anvil-fusion-ask-test--with-fake-orchestrator
    (member-candidates &rest body)
  "Run BODY with a faked orchestrator returning MEMBER-CANDIDATES.
Binds `submitted' to the list of submitted task-lists (newest
first)."
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
                  (list :summary "FUSED-ANSWER"))))
       ,@body)))

(defconst anvil-fusion-ask-test--cands
  '((:id "m1" :provider ollama :status done :summary "候補A: DGR を使う。")
    (:id "m2" :provider ollama :status done :summary "候補B: 地絡方向継電器。")
    (:id "m3" :provider ollama :status done :summary "候補C: OCGR は不可。"))
  "Three simulated sovereign-panel candidate answers.")

(ert-deftest anvil-fusion-ask-test-happy-path-sovereign ()
  "Sovereign panel returns the fused answer + metadata.
Panel is passed explicitly: the default is no longer `sovereign'
(changed to `claude-pair' 2026-06-16), but the sovereign mechanism
is retained and must keep working when selected."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (let ((res (anvil-fusion-ask "地絡保護の継電器は？"
                                :panel 'sovereign
                                :fidelity 'summary :max-rounds 0)))
      (should (equal "FUSED-ANSWER" (plist-get res :answer)))
      (should (eq 'sovereign (plist-get res :panel)))
      (should (eq 'local-only (plist-get res :egress)))
      (should (= 0 (plist-get res :rounds)))
      (should (= 3 (length (plist-get res :candidates))))
      (should (> (plist-get res :prompt-chars) 0)))))

(ert-deftest anvil-fusion-ask-test-member-tasks-from-panel ()
  "The fan-out submits exactly the panel's member models."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 0)
    (let* ((member-list (cadr submitted)))
      (should (= 3 (length member-list)))
      (should (cl-every (lambda (tk) (eq 'ollama (plist-get tk :provider)))
                        member-list))
      (should (equal '("llama3.1:8b" "llama3.2:3b" "gemma4:e4b")
                     (mapcar (lambda (tk) (plist-get tk :model)) member-list)))
      (should (cl-every (lambda (tk) (equal "Q" (plist-get tk :prompt)))
                        member-list)))))

(ert-deftest anvil-fusion-ask-test-judge-prompt-has-candidates ()
  "The judge prompt embeds every candidate answer + the Fusion axes."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (anvil-fusion-ask "Q" :panel 'sovereign :fidelity 'summary :max-rounds 0)
    (let* ((judge-task (car (car submitted)))
           (jprompt (plist-get judge-task :prompt)))
      (should (string-match-p "候補A: DGR" jprompt))
      (should (string-match-p "候補B: 地絡方向継電器" jprompt))
      (should (string-match-p "候補C: OCGR は不可" jprompt))
      (should (string-match-p "最終回答" jprompt))
      (should (string-match-p "Q" jprompt)))))

(ert-deftest anvil-fusion-ask-test-judge-provider-from-panel ()
  "The judge task uses the panel's judge provider/model."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 0)))
      (should (eq 'ollama (plist-get res :judge-provider)))
      (let ((judge-task (car (car submitted))))
        (should (eq 'ollama (plist-get judge-task :provider)))
        (should (equal "llama3.1:8b" (plist-get judge-task :model)))))))

(ert-deftest anvil-fusion-ask-test-extra-appended-to-judge ()
  "EXTRA reaches the judge prompt."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (anvil-fusion-ask "Q" :panel 'sovereign :extra "120字以内。" :max-rounds 0)
    (let ((jprompt (plist-get (car (car submitted)) :prompt)))
      (should (string-suffix-p "120字以内。" jprompt)))))

(ert-deftest anvil-fusion-ask-test-sovereignty-guard-blocks-external-judge ()
  "A non-local :judge override on a local-only panel is refused, and
nothing is submitted."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (should-error (anvil-fusion-ask "Q" :panel 'sovereign :judge 'claude)
                  :type 'user-error)
    (should (null submitted))))

(ert-deftest anvil-fusion-ask-test-quality-panel-distinct-providers ()
  "Quality panel fans out to its three distinct providers."
  (anvil-fusion-ask-test--with-fake-orchestrator
      '((:id "m1" :provider claude :status done :summary "A")
        (:id "m2" :provider codex  :status done :summary "B")
        (:id "m3" :provider gemini :status done :summary "C"))
    (let ((res (anvil-fusion-ask "Q" :panel 'quality :max-rounds 0)))
      (should (eq 'external (plist-get res :egress)))
      (should (eq 'claude (plist-get res :judge-provider)))
      (let ((member-list (cadr submitted)))
        (should (equal '(claude codex gemini)
                       (mapcar (lambda (tk) (plist-get tk :provider))
                               member-list)))))))

(provide 'anvil-fusion-ask-test)
;;; anvil-fusion-ask-test.el ends here
