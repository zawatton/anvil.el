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

;;; Code:

(require 'anvil-fusion)
(require 'anvil-fusion-panels)

(declare-function anvil-orchestrator-submit "anvil-orchestrator" (tasks))
(declare-function anvil-orchestrator-collect "anvil-orchestrator" (batch-id &rest _))
(declare-function anvil-orchestrator-status "anvil-orchestrator" (id))
(declare-function anvil-orchestrator-extract-result "anvil-orchestrator" (task-id &optional full))

(cl-defun anvil-fusion--run-round
    (member-prompt judge-question body jprov jmodel
                   &key fidelity extra lenses cwd timeout-sec (max-wait-sec 1800))
  "Run one fan-out + judge round and return its result plist.
MEMBER-PROMPT is what each panel member answers (lens-prefixed);
JUDGE-QUESTION is the question the judge synthesizes against
(always the original prompt, even in critique rounds).  Returns
(:answer :candidates :judge-task-id :judge-batch :members-batch
:prompt-chars)."
  (let* ((member-tasks (anvil-fusion-panel-tasks body member-prompt lenses cwd))
         (mbatch (anvil-orchestrator-submit member-tasks)))
    (anvil-orchestrator-collect mbatch :wait t :max-wait-sec max-wait-sec)
    (let* ((status     (anvil-orchestrator-status mbatch))
           (candidates (plist-get status :tasks))
           (jprompt    (anvil-fusion-build-judge-prompt
                        judge-question candidates :fidelity fidelity :extra extra))
           (jtask      (append
                        (list :provider jprov :prompt jprompt :name "fusion-judge")
                        (and jmodel (list :model jmodel))
                        (and cwd (list :cwd cwd))
                        (and timeout-sec (list :timeout-sec timeout-sec))))
           (jbatch     (anvil-orchestrator-submit (list jtask))))
      (anvil-orchestrator-collect jbatch :wait t :max-wait-sec max-wait-sec)
      (let* ((jid    (anvil-fusion--batch-first-task-id jbatch))
             (result (anvil-orchestrator-extract-result jid t)))
        (list :answer        (plist-get result :summary)
              :candidates    candidates
              :judge-task-id jid
              :judge-batch   jbatch
              :members-batch mbatch
              :prompt-chars  (length jprompt))))))

(cl-defun anvil-fusion-ask
    (prompt &key panel fidelity judge judge-model extra lenses cwd
            max-rounds converge-threshold timeout-sec (max-wait-sec 1800))
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
MAX-WAIT-SEC caps the wait per batch.

For a `local-only' panel a non-local JUDGE override is refused
before anything is submitted, preserving the zero-egress
guarantee.

Returns a plist: :answer :panel :egress :fidelity :rounds :looped
:members-batch :judge-batch :judge-task-id :judge-provider
:judge-model :candidates :prompt-chars."
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
      (let* ((round (anvil-fusion--run-round
                     prompt prompt body jprov jmodel
                     :fidelity fidelity :extra extra :lenses lenses :cwd cwd
                     :timeout-sec timeout-sec :max-wait-sec max-wait-sec))
             (rounds 0))
        (while (and (< rounds cap)
                    (anvil-fusion-should-loop-p
                     (plist-get round :candidates) thr))
          (let ((critique (anvil-fusion-build-critique-prompt
                           prompt (plist-get round :answer))))
            (setq round (anvil-fusion--run-round
                         critique prompt body jprov jmodel
                         :fidelity fidelity :extra extra :lenses lenses :cwd cwd
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
              :prompt-chars  (plist-get round :prompt-chars))))))

(provide 'anvil-fusion-ask)
;;; anvil-fusion-ask.el ends here
