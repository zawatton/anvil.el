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

;;;; ============================================================
;;;; Phase 6d — :verify keyword
;;;; ============================================================

(defconst anvil-fusion-ask-test--claims-raw
  (list (list :claim "DGR を使う" :kind 'fact :candidates '("A" "B"))
        (list :claim "OCGR でも良い" :kind 'fact :candidates '("C")))
  "Two raw (Phase 6a) claims used as the extraction stub's return value.")

(defconst anvil-fusion-ask-test--claims-annotated
  (list (list :claim "DGR を使う" :kind 'fact :candidates '("A" "B")
              :verdict 'confirmed :evidence "a.org:1")
        (list :claim "OCGR でも良い" :kind 'fact :candidates '("C")
              :verdict 'refuted :evidence "反証: OCGR は不可"))
  "Annotated (Phase 6b) claims, one confirmed one refuted.")

(ert-deftest anvil-fusion-ask-test-verify-nil-skips-verification ()
  "With :verify nil (the default), extract-claims/verify-claims are
never invoked, and the judge prompt is the normal (un-verified)
template -- existing behavior is untouched."
  (let (extract-called verify-called)
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _) (setq extract-called t) nil))
              ((symbol-function 'anvil-fusion-verify-claims)
               (lambda (&rest _) (setq verify-called t) nil)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 0)))
          (should-not extract-called)
          (should-not verify-called)
          (should (null (plist-get res :claims)))
          (let ((jprompt (plist-get (car (car submitted)) :prompt)))
            (should (string-match-p "最終回答" jprompt))
            (should-not (string-match-p "検証済み主張表" jprompt))))))))

(ert-deftest anvil-fusion-ask-test-verify-happy-path ()
  "With :verify t, extraction + verification run, the verified judge
template is used (claims block + the refuted claim present in the
judge prompt), and the return plist carries :claims."
  (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
             (lambda (question _candidates &rest _)
               (should (equal "Q" question))
               anvil-fusion-ask-test--claims-raw))
            ((symbol-function 'anvil-fusion-verify-claims)
             (lambda (claims &rest kwargs)
               (should (equal anvil-fusion-ask-test--claims-raw claims))
               (should (equal "Q" (plist-get kwargs :question)))
               anvil-fusion-ask-test--claims-annotated)))
    (anvil-fusion-ask-test--with-fake-orchestrator
        anvil-fusion-ask-test--cands
      (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :verify t :max-rounds 0)))
        (let ((jprompt (plist-get (car (car submitted)) :prompt)))
          (should (string-match-p "検証済み主張表" jprompt))
          (should (string-match-p "OCGR でも良い" jprompt))
          (should (string-match-p "refuted" jprompt)))
        (should (equal anvil-fusion-ask-test--claims-annotated (plist-get res :claims)))))))

(ert-deftest anvil-fusion-ask-test-verify-extraction-nil-falls-back ()
  "When extraction returns nil, verify-claims is never called, the
normal template is used, the judge still runs, and :claims is nil."
  (let (verify-called)
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _) nil))
              ((symbol-function 'anvil-fusion-verify-claims)
               (lambda (&rest _) (setq verify-called t) nil)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :verify t :max-rounds 0)))
          (should-not verify-called)
          (should (equal "FUSED-ANSWER" (plist-get res :answer)))
          (should (null (plist-get res :claims)))
          (let ((jprompt (plist-get (car (car submitted)) :prompt)))
            (should-not (string-match-p "検証済み主張表" jprompt))))))))

(ert-deftest anvil-fusion-ask-test-verify-extraction-nil-uses-base-template-fallback ()
  "Nil extraction uses VERIFY-BASE-TEMPLATE when no explicit TEMPLATE was provided."
  (let ((base "DISTINCT-FALLBACK\n# 原問\n%s\n# 候補回答\n%s\n# 検証済み主張表\n{{CLAIMS}}\n"))
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _) nil)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (anvil-fusion-ask "Q" :panel 'sovereign :verify t
                          :verify-base-template base :max-rounds 0)
        (let ((jprompt (plist-get (car (car submitted)) :prompt)))
          (should (string-match-p "DISTINCT-FALLBACK" jprompt))
          (should (string-match-p "(検証済み主張なし)" jprompt)))))))

(ert-deftest anvil-fusion-ask-test-verify-extraction-nil-explicit-template-wins ()
  "An explicit TEMPLATE overrides the nil-extraction fallback template."
  (let ((base "DISTINCT-FALLBACK\n# 原問\n%s\n# 候補回答\n%s\n# 検証済み主張表\n{{CLAIMS}}\n")
        (explicit "EXPLICIT-TEMPLATE\nQ=%s\nC=%s\n"))
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _) nil)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (anvil-fusion-ask "Q" :panel 'sovereign :verify t
                          :template explicit
                          :verify-base-template base
                          :max-rounds 0)
        (let ((jprompt (plist-get (car (car submitted)) :prompt)))
          (should (string-match-p "EXPLICIT-TEMPLATE" jprompt))
          (should-not (string-match-p "DISTINCT-FALLBACK" jprompt)))))))

(ert-deftest anvil-fusion-ask-test-verify-local-only-threads-panel-judge ()
  "A local-only panel's :verify t threads the panel judge's
provider/model into both extraction and verification, and passes
:egress \\='local-only to verify-claims."
  (let (extract-kw verify-kw)
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (_question _candidates &rest kwargs)
                 (setq extract-kw kwargs)
                 anvil-fusion-ask-test--claims-raw))
              ((symbol-function 'anvil-fusion-verify-claims)
               (lambda (_claims &rest kwargs)
                 (setq verify-kw kwargs)
                 anvil-fusion-ask-test--claims-annotated)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (anvil-fusion-ask "Q" :panel 'sovereign :verify t :max-rounds 0)
        (should (eq 'ollama (plist-get extract-kw :provider)))
        (should (equal "llama3.1:8b" (plist-get extract-kw :model)))
        (should (eq 'ollama (plist-get verify-kw :provider)))
        (should (equal "llama3.1:8b" (plist-get verify-kw :model)))
        (should (eq 'local-only (plist-get verify-kw :egress)))))))

(ert-deftest anvil-fusion-ask-test-verify-base-template-forwarded ()
  "A custom VERIFY-BASE-TEMPLATE is used when building the verified judge prompt."
  (let ((base "DISTINCT-PLAN-BASE\n# 原問\n%s\n# 候補回答\n%s\n# 検証済み主張表\n{{CLAIMS}}\n"))
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _) anvil-fusion-ask-test--claims-raw))
              ((symbol-function 'anvil-fusion-verify-claims)
               (lambda (&rest _) anvil-fusion-ask-test--claims-annotated)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (anvil-fusion-ask "Q" :panel 'sovereign :verify t
                          :verify-base-template base :max-rounds 0)
        (let ((jprompt (plist-get (car (car submitted)) :prompt)))
          (should (string-match-p "DISTINCT-PLAN-BASE" jprompt)))))))

(provide 'anvil-fusion-ask-test)
;;; anvil-fusion-ask-test.el ends here
