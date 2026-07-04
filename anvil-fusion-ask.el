;;; anvil-fusion-ask.el --- one-call Fusion ask over a panel -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion

;;; Commentary:
;;
;; Phase 3 + 4 of docs/design/01-fusion-harness.org: the 1-call wrapper,
;; now with role lenses and a critique loop.
;;
;; `anvil-fusion-ask' fans a prompt out to a named panel (Phase 2),
;; waits, then has the panel's judge synthesize one answer with the
;; Fusion-structured template (Phase 1).  The caller gets ONE fused
;; answer.
;;
;; Phase 4 additions:
;;   * role lenses — each member can be given a fixed perspective
;;     (:lenses, applied by position; falls back to the panel body's
;;     `lenses' key) so the panel diversifies by role as well as model.
;;   * critique loop — when the round's candidates disagree
;;     (`anvil-fusion-should-loop-p'), the draft is fed back to the panel
;;     for an improvement round, up to `anvil-fusion-max-rounds' (cap).
;;
;; Wiring uses the *batch* path (`anvil-orchestrator-submit' +
;; `-collect' + `-status' + `-extract-result'), NOT the consensus-id
;; path, so distinct-provider (quality) and same-provider-multi-model
;; (sovereign) panels run through identical code.
;;
;; Sovereignty: for a `local-only' panel, validation guarantees every
;; member + judge is local, and a non-local :judge override is refused
;; before anything is submitted.
;;
;; Phase 6d (docs/design/61-fusion-verify.org §2) adds an optional
;; `:verify' keyword: between the member fan-out and the judge call,
;; contested claims are extracted (`anvil-fusion-verify-extract-claims')
;; and checked against evidence (`anvil-fusion-verify-claims'), and the
;; judge template switches to the verdict-annotated variant
;; (`anvil-fusion-verify-judge-template-for') built for exactly those
;; claims.  This module `require's `anvil-fusion-verify' eagerly at
;; load time -- the same choice already made for `anvil-fusion-panels'
;; -- because, like this file's own orchestrator calls,
;; `anvil-fusion-verify''s orchestrator dependency is itself lazy
;; (required only inside `anvil-fusion-verify-extract-claims' /
;; `anvil-fusion-verify-claims'), so an eager require here does not
;; drag the orchestrator stack in at load time either.  `:verify' is
;; nil by default: every existing caller keeps its exact prior
;; behavior (see `anvil-fusion-ask''s docstring for the full
;; fallback/sovereignty discipline).

;;; Code:

(require 'anvil-fusion)
(require 'anvil-fusion-panels)
(require 'anvil-fusion-verify)

(declare-function anvil-orchestrator-submit "anvil-orchestrator" (tasks))
(declare-function anvil-orchestrator-collect "anvil-orchestrator" (batch-id &rest _))
(declare-function anvil-orchestrator-status "anvil-orchestrator" (id))
(declare-function anvil-orchestrator-extract-result "anvil-orchestrator" (task-id &optional full))

(cl-defun anvil-fusion--run-members
    (member-prompt body lenses cwd &key (max-wait-sec 1800))
  "Fan MEMBER-PROMPT out to panel BODY's members and collect the results.
MEMBER-PROMPT is what each panel member answers (lens-prefixed).
LENSES / CWD are forwarded to `anvil-fusion-panel-tasks' unchanged
(member tasks carry no :timeout-sec -- matching prior
`anvil-fusion--run-round' behavior, where TIMEOUT-SEC only ever
capped the judge task).  Returns (:candidates :members-batch) -- the
member half of what `anvil-fusion--run-round' used to do in one
shot, split out so Phase 6d (`:verify' in `anvil-fusion-ask') can
inspect CANDIDATES before the judge is asked to synthesize them."
  (let* ((member-tasks (anvil-fusion-panel-tasks body member-prompt lenses cwd))
         (mbatch (anvil-orchestrator-submit member-tasks)))
    (anvil-orchestrator-collect mbatch :wait t :max-wait-sec max-wait-sec)
    (list :candidates (plist-get (anvil-orchestrator-status mbatch) :tasks)
          :members-batch mbatch)))

(cl-defun anvil-fusion--run-judge
    (judge-question candidates jprov jmodel
                    &key fidelity extra template cwd timeout-sec (max-wait-sec 1800))
  "Submit ONE judge task synthesizing CANDIDATES for JUDGE-QUESTION.
JUDGE-QUESTION is always the original prompt, even in critique
rounds.  :TEMPLATE overrides the judge prompt template (nil uses
`anvil-fusion-build-judge-prompt''s own default,
`anvil-fusion-judge-template' -- unchanged behavior).  Returns
(:answer :judge-task-id :judge-batch :prompt-chars)."
  (let* ((jprompt (anvil-fusion-build-judge-prompt
                   judge-question candidates
                   :template template :fidelity fidelity :extra extra))
         (jtask   (append
                   (list :provider jprov :prompt jprompt :name "fusion-judge")
                   (and jmodel (list :model jmodel))
                   (and cwd (list :cwd cwd))
                   (and timeout-sec (list :timeout-sec timeout-sec))))
         (jbatch  (anvil-orchestrator-submit (list jtask))))
    (anvil-orchestrator-collect jbatch :wait t :max-wait-sec max-wait-sec)
    (let* ((jid    (anvil-fusion--batch-first-task-id jbatch))
           (result (anvil-orchestrator-extract-result jid t)))
      (list :answer        (plist-get result :summary)
            :judge-task-id jid
            :judge-batch   jbatch
            :prompt-chars  (length jprompt)))))

(cl-defun anvil-fusion--run-round
    (member-prompt judge-question body jprov jmodel
                   &key fidelity extra template lenses cwd timeout-sec (max-wait-sec 1800))
  "Run one fan-out + judge round and return its result plist.
MEMBER-PROMPT is what each panel member answers (lens-prefixed);
JUDGE-QUESTION is the question the judge synthesizes against
(always the original prompt, even in critique rounds).  :TEMPLATE is
forwarded to `anvil-fusion--run-judge' (nil = the normal
`anvil-fusion-judge-template' default).  A thin composition of
`anvil-fusion--run-members' followed by `anvil-fusion--run-judge' --
kept as one call for callers (e.g. the critique loop in
`anvil-fusion-ask') that do not need to inspect the member
candidates before judging.  Returns (:answer :candidates
:judge-task-id :judge-batch :members-batch :prompt-chars)."
  (let* ((mresult    (anvil-fusion--run-members
                      member-prompt body lenses cwd :max-wait-sec max-wait-sec))
         (candidates (plist-get mresult :candidates))
         (jresult    (anvil-fusion--run-judge
                      judge-question candidates jprov jmodel
                      :fidelity fidelity :extra extra :template template
                      :cwd cwd :timeout-sec timeout-sec :max-wait-sec max-wait-sec)))
    (list :answer        (plist-get jresult :answer)
          :candidates    candidates
          :judge-task-id (plist-get jresult :judge-task-id)
          :judge-batch   (plist-get jresult :judge-batch)
          :members-batch (plist-get mresult :members-batch)
          :prompt-chars  (plist-get jresult :prompt-chars))))

(cl-defun anvil-fusion-ask
    (prompt &key panel fidelity judge judge-model extra lenses cwd
            max-rounds converge-threshold timeout-sec (max-wait-sec 1800)
            template verify verify-args verify-base-template exec-check)
  "Answer PROMPT by fusing a panel of models into one synthesized reply.

PANEL names a panel in `anvil-fusion-panels' (default
`anvil-fusion-default-panel').  FIDELITY (`summary' / `full')
controls how much of each candidate the judge sees.  JUDGE /
JUDGE-MODEL override the panel's judge.  EXTRA appends an
instruction to the judge prompt.  LENSES is a list of role lenses
applied to members by position (default: the panel body's `lenses'
key).  MAX-ROUNDS caps critique rounds (default
`anvil-fusion-max-rounds'); CONVERGE-THRESHOLD overrides
`anvil-fusion-converge-threshold'.  TIMEOUT-SEC caps each task;
MAX-WAIT-SEC caps the wait per batch.  TEMPLATE overrides the judge
prompt template for every round (forwarded to
`anvil-fusion-build-judge-prompt'); when VERIFY is also non-nil, an
explicit TEMPLATE always wins over the verified-judge template
Phase 6d would otherwise build for the judge call (see below) --
claim extraction/verification and the returned :CLAIMS still run,
only the template swap is skipped.  VERIFY-BASE-TEMPLATE optionally
overrides the base template used to build the verified judge prompt.

For a `local-only' panel a non-local JUDGE override is refused
before anything is submitted, preserving the zero-egress
guarantee.

VERIFY (default nil) runs the Doc 61 Phase 6d verifier-grounded judge
(docs/design/61-fusion-verify.org §2 6d) for the FIRST round only:
after the member fan-out, `anvil-fusion-verify-extract-claims' mines
contested claims from PROMPT + the collected candidates, then
`anvil-fusion-verify-claims' checks them against evidence, and --
unless an explicit TEMPLATE was given (see above) --
`anvil-fusion-verify-judge-template-for' builds the verdict-annotated
template used for the judge call.  ANY critique round thereafter
(see MAX-ROUNDS) re-judges its own fresh candidates against that SAME
template; claims are extracted and verified ONCE, never re-run per
round.  VERIFY-ARGS is a plist of `:provider' / `:model' /
`:skeptics' / `:timeout-sec' / `:max-wait-sec' overrides forwarded to
the extraction/verification calls below, taking priority over the
defaults `anvil-fusion-ask' computes for them.

Sovereignty threading: for a `local-only' panel, the panel's judge
:provider/:model (JPROV/JMODEL above -- guaranteed local by
`anvil-fusion-panel-validate') is passed as the default
:provider/:model to BOTH `anvil-fusion-verify-extract-claims' and
`anvil-fusion-verify-claims', and :egress `local-only' is passed to
`anvil-fusion-verify-claims' (so its own sovereignty gate refuses a
non-local skeptic provider); `anvil-fusion-ask' additionally refuses
(before extraction runs) a VERIFY-ARGS :provider override that is
itself non-local, so a caller cannot smuggle egress through the
extraction call, which has no sovereignty gate of its own.  For an
`external' panel no :provider/:model default is forced -- the
verify-layer's own defaults apply (claude/haiku extraction, claude
skeptics) unless VERIFY-ARGS overrides them.

Fallback discipline: when extraction finds nothing to verify (no
contested claims, or extraction fails/times out --
`anvil-fusion-verify-extract-claims' is itself best-effort and
returns nil rather than signaling), or verification returns no
annotations, the round falls back best-effort: an explicit TEMPLATE
still wins; otherwise a non-nil VERIFY-BASE-TEMPLATE is used via
`anvil-fusion-verify-judge-template-for' with an empty claims block,
and only if neither is present does the NORMAL (un-verified) judge
template remain in force.  One `message' notes the fallback.
Everything degrades to the pre-Phase-6d behavior when VERIFY is nil
-- zero behavior change for existing callers.

EXEC-CHECK (default nil) runs the Doc 61 Phase 6c local execution
verifier once, on the FIRST round only, after member collection and
before the judge call.  The value is a plist
`(:repo-root STR :check-cmd STR [:timeout-sec N])'.  When non-nil,
`anvil-fusion-exec-verify-candidates' is called lazily (this module
does not require `anvil-fusion-exec' at load time), its claims are
merged into the verified-claims list used for
`anvil-fusion-verify-judge-template-for', and candidates whose exec
status is `fail' or `error' are excluded from the FIRST judge call
unless that would exclude all candidates, in which case none are
excluded.  `no-patch' never excludes a candidate.  Critique rounds
reuse the same template and do not re-run execution.  Sovereignty note:
execution is local only (git + the caller's check command), so it is
allowed for local-only panels too.

Returns a plist: :answer :panel :egress :fidelity :rounds :looped
:members-batch :judge-batch :judge-task-id :judge-provider
:judge-model :candidates :prompt-chars :claims :exec-results.  :CLAIMS
is the merged annotated claim list when VERIFY and/or EXEC-CHECK
produced one, else nil.  :EXEC-RESULTS is the per-candidate execution
result list, else nil."
  (require 'anvil-orchestrator)
  (let ((pname (or panel anvil-fusion-default-panel)))
    (anvil-fusion-panel-validate pname)
    (let* ((body    (anvil-fusion-panel-get pname))
           (egress  (anvil-fusion-panel-egress body))
           (jspec   (anvil-fusion-panel-judge body))
           (jprov   (or judge (car jspec)))
           (jmodel  (or judge-model (cdr jspec)))
           (lenses  (or lenses (cdr (assq 'lenses body))))
           (cap     (or max-rounds anvil-fusion-max-rounds))
           (thr     converge-threshold))
      (when (and (eq egress 'local-only)
                 (not (anvil-fusion-provider-local-p jprov)))
        (user-error
         "anvil-fusion-ask: panel %s is local-only; refusing external judge %S"
         pname jprov))
      (let* ((mresult    (anvil-fusion--run-members prompt body lenses cwd
                                                     :max-wait-sec max-wait-sec))
             (candidates (plist-get mresult :candidates))
             (judge-candidates candidates)
             (claims     nil)
             (exec-results nil)
             (etemplate  template))
        (when verify
          (let* ((local-panel (eq egress 'local-only))
                 (vargs       (or verify-args nil))
                 (ex-provider (or (plist-get vargs :provider) (and local-panel jprov)))
                 (ex-model    (or (plist-get vargs :model) (and local-panel jmodel))))
            (when (and local-panel ex-provider
                       (not (anvil-fusion-provider-local-p ex-provider)))
              (user-error
               "anvil-fusion-ask: panel %s is local-only; refusing non-local :verify-args extraction provider %S"
               pname ex-provider))
            (let* ((extract-kwargs
                    (append (and ex-provider (list :provider ex-provider))
                            (and ex-model    (list :model ex-model))
                            (and (plist-get vargs :timeout-sec)
                                 (list :timeout-sec (plist-get vargs :timeout-sec)))
                            (and (plist-get vargs :max-wait-sec)
                                 (list :max-wait-sec (plist-get vargs :max-wait-sec)))))
                   (raw-claims (apply #'anvil-fusion-verify-extract-claims
                                       prompt candidates extract-kwargs)))
             (if (null raw-claims)
                  (progn
                    (message
                     "anvil-fusion-ask: :verify requested but claim extraction found nothing to verify; using the fallback judge template")
                    (when (and (null template) verify-base-template)
                      (setq etemplate
                            (anvil-fusion-verify-judge-template-for
                             nil verify-base-template))))
                (let* ((vk-provider (or (plist-get vargs :provider) ex-provider))
                       (vk-model    (or (plist-get vargs :model) ex-model))
                       (vk-egress   (if local-panel 'local-only 'external))
                       (verify-kwargs
                        (append (list :question prompt :egress vk-egress)
                                (and vk-provider (list :provider vk-provider))
                                (and vk-model    (list :model vk-model))
                                (and (plist-get vargs :skeptics)
                                     (list :skeptics (plist-get vargs :skeptics)))
                                (and (plist-get vargs :timeout-sec)
                                     (list :timeout-sec (plist-get vargs :timeout-sec)))
                                (and (plist-get vargs :max-wait-sec)
                                     (list :max-wait-sec (plist-get vargs :max-wait-sec)))))
                       (annotated (apply #'anvil-fusion-verify-claims raw-claims verify-kwargs)))
                  (when annotated
                    (setq claims annotated)
                    (unless template
                      (setq etemplate
                            (anvil-fusion-verify-judge-template-for
                             annotated verify-base-template))))
                  (when (and (null annotated)
                             (null template)
                             verify-base-template)
                    (message
                     "anvil-fusion-ask: :verify requested but verification produced no annotations; using the fallback judge template")
                    (setq etemplate
                          (anvil-fusion-verify-judge-template-for
                           nil verify-base-template))))))))
        (when exec-check
          (require 'anvil-fusion-exec)
          (let* ((exec-fn (symbol-function 'anvil-fusion-exec-verify-candidates))
                 (exec-plist (funcall exec-fn
                                      candidates
                                      :repo-root (plist-get exec-check :repo-root)
                                      :check-cmd (plist-get exec-check :check-cmd)
                                      :timeout-sec (plist-get exec-check :timeout-sec)))
                 (exec-claims (plist-get exec-plist :claims))
                 (excluded-names
                  (mapcar (lambda (row) (plist-get row :name))
                          (seq-filter
                           (lambda (row)
                             (memq (plist-get row :status) '(fail error)))
                           (plist-get exec-plist :results))))
                 (filtered
                  (seq-remove
                   (lambda (candidate)
                     (member (plist-get candidate :name) excluded-names))
                   candidates)))
            (setq exec-results (plist-get exec-plist :results))
            (setq claims (append claims exec-claims))
            (unless (or (null excluded-names) (null filtered))
              (setq judge-candidates filtered))
            (when (and (null template) claims)
              (setq etemplate
                    (anvil-fusion-verify-judge-template-for
                     claims verify-base-template)))))
        (let* ((jresult (anvil-fusion--run-judge
                         prompt judge-candidates jprov jmodel
                         :fidelity fidelity :extra extra :template etemplate
                         :cwd cwd :timeout-sec timeout-sec :max-wait-sec max-wait-sec))
               (round (list :answer        (plist-get jresult :answer)
                            :candidates    judge-candidates
                            :judge-task-id (plist-get jresult :judge-task-id)
                            :judge-batch   (plist-get jresult :judge-batch)
                            :members-batch (plist-get mresult :members-batch)
                            :prompt-chars  (plist-get jresult :prompt-chars)))
               (rounds 0))
          (while (and (< rounds cap)
                      (anvil-fusion-should-loop-p
                       (plist-get round :candidates) thr))
            (let ((critique (anvil-fusion-build-critique-prompt
                             prompt (plist-get round :answer))))
              (setq round (anvil-fusion--run-round
                           critique prompt body jprov jmodel
                           :fidelity fidelity :extra extra :template etemplate
                           :lenses lenses :cwd cwd
                           :timeout-sec timeout-sec :max-wait-sec max-wait-sec))
              (setq rounds (1+ rounds))))
          (list :answer        (plist-get round :answer)
                :panel         pname
                :egress        egress
                :fidelity      (or fidelity anvil-fusion-default-fidelity)
                :rounds        rounds
                :looped        (> rounds 0)
                :members-batch (plist-get round :members-batch)
                :judge-batch   (plist-get round :judge-batch)
                :judge-task-id (plist-get round :judge-task-id)
                :judge-provider jprov
                :judge-model   jmodel
                :candidates    (plist-get round :candidates)
                :prompt-chars  (plist-get round :prompt-chars)
                :claims        claims
                :exec-results  exec-results))))))

(provide 'anvil-fusion-ask)
;;; anvil-fusion-ask.el ends here
