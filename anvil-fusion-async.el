;;; anvil-fusion-async.el --- non-blocking fusion ask (submit + poll) -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion

;;; Commentary:
;;
;; Non-blocking variant of `anvil-fusion-ask' for the MCP transport.
;;
;; `anvil-fusion-ask' blocks for the whole fan-out + judge duration
;; (sovereign ~30-60s, codex ~230s in the 2026-06-15 smoke), which
;; exceeds the MCP tool dispatch timeout.  This module mirrors the
;; orchestrator's async model: `anvil-fusion-ask-async' submits the panel
;; fan-out and returns a job id IMMEDIATELY; `anvil-fusion-result' is a
;; cheap state-machine step the caller polls until :status is `done'.
;;
;; Doc 61 Phase 6d' (docs/design/61-fusion-verify.org §2 6d') brought this
;; module to full parity with `anvil-fusion-ask': the critique loop
;; (`anvil-fusion-should-loop-p' + :max-rounds, previously blocking-only)
;; and the Phase 6d `:verify' flow both now run inside the poll-driven
;; state machine.  State machine per job:
;;
;;   members -> [extracting -> verifying ->] judging -> (members -> ...
;;              -> judging ->) done
;;
;; The bracketed `extracting'/`verifying' pair only appears when :VERIFY
;; is non-nil, and only for the FIRST round (claims are extracted and
;; verified once, then reused by every critique round -- same
;; limitation `anvil-fusion-ask' has).  The parenthesized repeat only
;; appears when a round's candidates disagree
;; (`anvil-fusion-should-loop-p') and the :max-rounds cap has not been
;; reached.
;;
;; Every stage's poll is an O(1) `anvil-orchestrator-status' check
;; EXCEPT `extracting' and `verifying': `anvil-fusion-verify-extract-claims'
;; / `anvil-fusion-verify-claims' are themselves blocking (their own
;; `anvil-orchestrator-collect :wait t'), so those two stages call them
;; directly and can each block the polling caller for up to
;; `anvil-fusion-async-verify-max-wait-sec' (default 240s, overridable
;; per call via :verify-args :max-wait-sec).  This reuses the shipped,
;; tested Phase 6a/6b verification logic verbatim instead of
;; re-implementing the skeptic fan-out natively in this state machine --
;; a deliberate bounded-blocking tradeoff, not an oversight; see
;; `anvil-fusion-ask-async''s docstring for the full rationale.
;;
;; MAX-ROUNDS 0 (the caller must pass it explicitly -- the default cap
;; is `anvil-fusion-max-rounds', same as `anvil-fusion-ask') never
;; loops, exactly like this module's original v1.  Stubbable: depends
;; only on anvil-orchestrator public functions (plus, for :VERIFY, the
;; public anvil-fusion-verify API).

;;; Code:

(require 'anvil-fusion)
(require 'anvil-fusion-panels)
(require 'anvil-fusion-verify)

(declare-function anvil-orchestrator-submit "anvil-orchestrator" (tasks))
(declare-function anvil-orchestrator-status "anvil-orchestrator" (id))
(declare-function anvil-orchestrator-extract-result "anvil-orchestrator" (task-id &optional full))

;;;; --- customization ---------------------------------------------------

(defcustom anvil-fusion-verify-by-default nil
  "Default :verify flag for the `fusion-ask' MCP tool when the caller
omits it.

docs/design/61-fusion-verify.org §2 (6d') sketches \"default on for
quality/claude-pair\" panels, but the 6e effect-measurement battery
(benchmarks/doc61-phase6e/) is still running as of this Phase 6d'
bundle -- shipping a stale uplift claim would violate the \"validate
counts/claims with real numbers\" discipline.  So this ships nil
\(conservative): the mechanism (:verify plumbing end to end, both the
async state machine and this MCP tool) is complete, but the tool
answers exactly as it did before this flag existed unless a caller
opts in explicitly.  Flip to t (or panel-scope it) once
benchmarks/doc61-phase6e/ results justify a default-on posture for
external panels."
  :type 'boolean
  :group 'anvil-fusion)

(defcustom anvil-fusion-async-verify-max-wait-sec 240
  "Default :max-wait-sec bound for the async :verify pipeline's two
bounded-blocking poll steps (`extracting' / `verifying' -- see this
file's Commentary).  Each of `anvil-fusion-verify-extract-claims' /
`anvil-fusion-verify-claims' can block its poll call for up to this
many seconds before giving up and falling back (extraction) or
returning `unverified' verdicts (verification) -- both functions are
already best-effort / non-signaling on their own timeout.  A modest
default (240s, well under `anvil-fusion-verify-extract-claims' /
`anvil-fusion-verify-claims''s own 1800s default) keeps a single async
poll call from stalling the caller for the same duration the fully
blocking `anvil-fusion-ask' would tolerate.  Override per call via
:verify-args :max-wait-sec to `anvil-fusion-ask-async'."
  :type 'integer
  :group 'anvil-fusion)

(defvar anvil-fusion--jobs (make-hash-table :test 'equal)
  "Map job-id -> job plist for in-flight async fusion asks.")

(defun anvil-fusion--batch-terminal-p (batch-id)
  "Return non-nil when BATCH-ID has no running/queued tasks left.
Uses the public `anvil-orchestrator-status' so no orchestrator
internals are touched."
  (let ((s (anvil-orchestrator-status batch-id)))
    (and s
         (zerop (or (plist-get s :running) 0))
         (zerop (or (plist-get s :queued) 0)))))

;;;; --- member fan-out (shared by the first round and every critique round)

(defun anvil-fusion--submit-members (job)
  "Submit JOB's current :member-prompt to its panel members (non-blocking).
Uses JOB's :body / :lenses (unchanged across critique rounds; only
:member-prompt differs between the original round and a critique
round -- see `anvil-fusion--advance-after-judge').  Sets :member-batch
and moves JOB to stage `members'.  Returns the updated JOB; the
caller is responsible for `puthash'ing it under its job id."
  (let* ((member-tasks (anvil-fusion-panel-tasks
                        (plist-get job :body) (plist-get job :member-prompt)
                        (plist-get job :lenses)))
         (mbatch (anvil-orchestrator-submit member-tasks)))
    (setq job (plist-put job :member-batch mbatch))
    (setq job (plist-put job :stage 'members))
    job))

;;;; --- entry point --------------------------------------------------------

(cl-defun anvil-fusion-ask-async
    (prompt &key panel fidelity judge judge-model lenses extra timeout-sec
            max-rounds converge-threshold template verify verify-args)
  "Start a fusion ask for PROMPT and return a job plist immediately.

Submits the first round's panel fan-out (non-blocking) and registers
a job.  Keyword args mirror `anvil-fusion-ask' -- this async path now
matches its full round/verify surface (Doc 61 Phase 6d',
docs/design/61-fusion-verify.org §2 6d').  For a `local-only' panel a
non-local JUDGE override is refused before anything is submitted; a
non-local :VERIFY-ARGS :provider override is refused the same way,
ALSO before anything is submitted -- unlike `anvil-fusion-ask', which
only performs this check after its (blocking) member fan-out has
already run, because there the members were going to run regardless
of :verify.  Here an async job must never be created only to error
out on its first poll, so this refusal moves earlier than its
blocking counterpart; the condition and error message are otherwise
identical.

MAX-ROUNDS caps critique rounds (default `anvil-fusion-max-rounds');
CONVERGE-THRESHOLD overrides `anvil-fusion-converge-threshold'.
MAX-ROUNDS 0 never loops -- pass it explicitly for a guaranteed single
round; this is unchanged from the module's original v1 behavior.
TEMPLATE overrides the judge prompt template for every round; when
VERIFY is also non-nil, an explicit TEMPLATE always wins over the
verified-judge template Phase 6d would otherwise build (claim
extraction/verification and the returned :CLAIMS still run, only the
template swap is skipped) -- exactly `anvil-fusion-ask''s precedence.

VERIFY (default nil) runs the Doc 61 Phase 6d verifier-grounded judge
for the FIRST round only: after the member fan-out,
`anvil-fusion-verify-extract-claims' mines contested claims, then
`anvil-fusion-verify-claims' checks them against evidence, and --
unless an explicit TEMPLATE was given --
`anvil-fusion-verify-judge-template-for' builds the verdict-annotated
template used for the judge call in THIS and every subsequent
critique round (claims are extracted/verified ONCE, never re-run per
round).  Sovereignty threading and fallback
discipline (extraction finds nothing -> skip verification, normal
template, one `message') are identical to `anvil-fusion-ask' -- see
its docstring for the full rationale.  VERIFY-ARGS is a plist of
`:provider' / `:model' / `:skeptics' / `:timeout-sec' / `:max-wait-sec'
overrides forwarded to the extraction/verification calls; :MAX-WAIT-SEC
additionally bounds how long the `extracting'/`verifying' poll steps
may each block (default `anvil-fusion-async-verify-max-wait-sec') --
see this file's Commentary for why those two stages, alone among all
the others, can block their poll call.

Returns (:job-id STR :stage members :panel SYM :egress SYM).  Poll
`anvil-fusion-result' with the job id."
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
         "anvil-fusion-ask-async: panel %s is local-only; refusing external judge %S"
         pname jprov))
      (when (and (eq egress 'local-only) verify)
        (let ((ex-provider (plist-get verify-args :provider)))
          (when (and ex-provider (not (anvil-fusion-provider-local-p ex-provider)))
            (user-error
             "anvil-fusion-ask-async: panel %s is local-only; refusing non-local :verify-args extraction provider %S"
             pname ex-provider))))
      (let ((job (list :prompt prompt :panel pname :egress egress :body body
                       :jprov jprov :jmodel jmodel :fidelity fidelity
                       :extra extra :timeout-sec timeout-sec
                       :lenses lenses :max-rounds cap :converge-threshold thr
                       :etemplate template :verify verify :verify-args verify-args
                       :verify-done (not verify) :claims nil :raw-claims nil
                       :rounds 0 :candidates nil
                       :member-prompt prompt :member-batch nil
                       :judge-batch nil :answer nil)))
        (setq job (anvil-fusion--submit-members job))
        (let ((job-id (concat "fusion-" (plist-get job :member-batch))))
          (setq job (plist-put job :job-id job-id))
          (puthash job-id job anvil-fusion--jobs)
          (list :job-id job-id :stage 'members :panel pname :egress egress))))))

;;;; --- stage: members terminal (route to extraction or straight to judge)

(defun anvil-fusion--members-terminal (job-id job)
  "JOB's member batch is terminal: capture candidates, then route to
Phase 6a claim extraction (JOB's :verify pending) or straight to the
judge (no :verify, or :verify already ran for an earlier round)."
  (let* ((mbatch (plist-get job :member-batch))
         (cands  (plist-get (anvil-orchestrator-status mbatch) :tasks)))
    (setq job (plist-put job :candidates cands))
    (if (plist-get job :verify-done)
        (anvil-fusion--advance-to-judge job-id job)
      (progn
        (setq job (plist-put job :stage 'extracting))
        (puthash job-id job anvil-fusion--jobs)
        (list :status 'running :stage 'extracting :job-id job-id)))))

;;;; --- stage: extracting (bounded-blocking Phase 6a) ----------------------

(defun anvil-fusion--advance-extract (job-id job)
  "Run the bounded-blocking Phase 6a claim extraction for JOB.
This is one of the two stages in the async :verify pipeline whose
poll can itself block -- up to `anvil-fusion-async-verify-max-wait-sec'
(or JOB's :verify-args :max-wait-sec override).  Mirrors
`anvil-fusion-ask''s extraction-provider resolution: a `local-only'
panel's judge provider/model is the default when :verify-args omits
its own; an `external' panel forces no default (the verify layer's
own claude/haiku default applies).  On nil (nothing to verify) submits
the judge directly with JOB's un-verified :etemplate, exactly like the
no-verify path, and emits the same fallback `message' `anvil-fusion-ask'
does; on claims found, stores them and moves JOB to `verifying' -- the
actual `anvil-fusion-verify-claims' call happens on the NEXT poll, not
this one, so this stage's own bound is spent on extraction alone."
  (let* ((egress      (plist-get job :egress))
         (local-panel (eq egress 'local-only))
         (jprov       (plist-get job :jprov))
         (jmodel      (plist-get job :jmodel))
         (vargs       (plist-get job :verify-args))
         (ex-provider (or (plist-get vargs :provider) (and local-panel jprov)))
         (ex-model    (or (plist-get vargs :model) (and local-panel jmodel)))
         (mws         (or (plist-get vargs :max-wait-sec)
                          anvil-fusion-async-verify-max-wait-sec))
         (extract-kwargs
          (append (and ex-provider (list :provider ex-provider))
                  (and ex-model    (list :model ex-model))
                  (and (plist-get vargs :timeout-sec)
                       (list :timeout-sec (plist-get vargs :timeout-sec)))
                  (list :max-wait-sec mws)))
         (raw-claims (apply #'anvil-fusion-verify-extract-claims
                             (plist-get job :prompt) (plist-get job :candidates)
                             extract-kwargs)))
    (if (null raw-claims)
        (progn
          (message
           "anvil-fusion-async: :verify requested but claim extraction found nothing to verify; using the normal judge template")
          (setq job (plist-put job :verify-done t))
          (anvil-fusion--advance-to-judge job-id job))
      (progn
        (setq job (plist-put job :raw-claims raw-claims))
        (setq job (plist-put job :stage 'verifying))
        (puthash job-id job anvil-fusion--jobs)
        (list :status 'running :stage 'verifying :job-id job-id)))))

;;;; --- stage: verifying (bounded-blocking Phase 6b) -----------------------

(defun anvil-fusion--advance-verify (job-id job)
  "Run the bounded-blocking Phase 6b evidence check for JOB's raw
claims, then submit the judge task.  Same :max-wait-sec bound as
`anvil-fusion--advance-extract'.  When verification annotates the
claims, JOB's :claims is set and -- unless an explicit :TEMPLATE was
given to `anvil-fusion-ask-async' (JOB's :etemplate is already
non-nil in that case, so this `unless' is a no-op) -- the verified
judge template (`anvil-fusion-verify-judge-template-for') becomes
JOB's :etemplate for this round AND every critique round thereafter,
since :etemplate is never reset once set (claims are
extracted/verified once, never re-run per round -- same limitation
`anvil-fusion-ask' has)."
  (let* ((egress      (plist-get job :egress))
         (local-panel (eq egress 'local-only))
         (jprov       (plist-get job :jprov))
         (jmodel      (plist-get job :jmodel))
         (vargs       (plist-get job :verify-args))
         (ex-provider (or (plist-get vargs :provider) (and local-panel jprov)))
         (ex-model    (or (plist-get vargs :model) (and local-panel jmodel)))
         (vk-provider (or (plist-get vargs :provider) ex-provider))
         (vk-model    (or (plist-get vargs :model) ex-model))
         (vk-egress   (if local-panel 'local-only 'external))
         (mws         (or (plist-get vargs :max-wait-sec)
                          anvil-fusion-async-verify-max-wait-sec))
         (verify-kwargs
          (append (list :question (plist-get job :prompt) :egress vk-egress)
                  (and vk-provider (list :provider vk-provider))
                  (and vk-model    (list :model vk-model))
                  (and (plist-get vargs :skeptics)
                       (list :skeptics (plist-get vargs :skeptics)))
                  (and (plist-get vargs :timeout-sec)
                       (list :timeout-sec (plist-get vargs :timeout-sec)))
                  (list :max-wait-sec mws)))
         (annotated (apply #'anvil-fusion-verify-claims
                            (plist-get job :raw-claims) verify-kwargs)))
    (setq job (plist-put job :verify-done t))
    (when annotated
      (setq job (plist-put job :claims annotated))
      (unless (plist-get job :etemplate)
        (setq job (plist-put job :etemplate
                             (anvil-fusion-verify-judge-template-for annotated)))))
    (anvil-fusion--advance-to-judge job-id job)))

;;;; --- stage: submit judge -------------------------------------------------

(defun anvil-fusion--advance-to-judge (job-id job)
  "JOB's candidates (member-only, or member+verify) are ready: submit
the judge task and move JOB to `judging'.  Uses JOB's :etemplate
(nil unless an explicit :TEMPLATE was given or Phase 6d verification
built one -- see `anvil-fusion--advance-verify'), so
`anvil-fusion-build-judge-prompt' falls back to the normal
`anvil-fusion-judge-template' exactly as before this stage existed."
  (let* ((jprompt (anvil-fusion-build-judge-prompt
                   (plist-get job :prompt) (plist-get job :candidates)
                   :template (plist-get job :etemplate)
                   :fidelity (plist-get job :fidelity)
                   :extra (plist-get job :extra)))
         (jtask   (append
                   (list :provider (plist-get job :jprov)
                         :prompt jprompt :name "fusion-judge")
                   (and (plist-get job :jmodel)
                        (list :model (plist-get job :jmodel)))
                   (and (plist-get job :timeout-sec)
                        (list :timeout-sec (plist-get job :timeout-sec)))))
         (jbatch  (anvil-orchestrator-submit (list jtask))))
    (setq job (plist-put job :judge-batch jbatch))
    (setq job (plist-put job :stage 'judging))
    (puthash job-id job anvil-fusion--jobs)
    (list :status 'running :stage 'judging :job-id job-id)))

;;;; --- stage: judge terminal (loop or finish) ------------------------------

(defun anvil-fusion--advance-after-judge (job-id job)
  "JOB's judge batch is terminal: collect the answer, then either loop
into a critique round (same condition as `anvil-fusion-ask':
`anvil-fusion-should-loop-p' on this round's :candidates, under JOB's
:max-rounds cap) or finish."
  (let* ((jbatch (plist-get job :judge-batch))
         (jid    (anvil-fusion--batch-first-task-id jbatch))
         (res    (anvil-orchestrator-extract-result jid t))
         (answer (plist-get res :summary))
         (cands  (plist-get job :candidates))
         (rounds (plist-get job :rounds)))
    (setq job (plist-put job :answer answer))
    (if (and (< rounds (plist-get job :max-rounds))
             (anvil-fusion-should-loop-p cands (plist-get job :converge-threshold)))
        (let ((critique (anvil-fusion-build-critique-prompt
                         (plist-get job :prompt) answer)))
          (setq job (plist-put job :rounds (1+ rounds)))
          (setq job (plist-put job :member-prompt critique))
          (setq job (anvil-fusion--submit-members job))
          (puthash job-id job anvil-fusion--jobs)
          (list :status 'running :stage 'members :job-id job-id))
      (progn
        (setq job (plist-put job :stage 'done))
        (puthash job-id job anvil-fusion--jobs)
        (anvil-fusion--report-done job)))))

(defun anvil-fusion--report-done (job)
  "Return the terminal :status `done' plist for JOB."
  (list :status 'done :job-id (plist-get job :job-id)
        :answer (plist-get job :answer)
        :panel (plist-get job :panel)
        :egress (plist-get job :egress)
        :n-candidates (length (plist-get job :candidates))
        :rounds (plist-get job :rounds)
        :looped (> (plist-get job :rounds) 0)
        :claims (plist-get job :claims)))

;;;; --- poll entry point -----------------------------------------------------

(defun anvil-fusion-result (job-id)
  "Advance and report the async fusion JOB-ID.
Returns a status plist; poll until :status is `done' (or `error').
Stages: members -> [extracting -> verifying ->] judging -> (members ->
... -> judging ->) done -- see this file's Commentary for when the
bracketed / parenthesized stages appear.  Every stage's poll is an
O(1) `anvil-orchestrator-status' check EXCEPT `extracting' /
`verifying', which can each block up to
`anvil-fusion-async-verify-max-wait-sec' (see
`anvil-fusion--advance-extract' / `anvil-fusion--advance-verify')."
  (require 'anvil-orchestrator)
  (let ((job (gethash job-id anvil-fusion--jobs)))
    (unless job
      (user-error "anvil-fusion-result: unknown job %s" job-id))
    (pcase (plist-get job :stage)
      ('done   (anvil-fusion--report-done job))
      ('error  (list :status 'error :job-id job-id
                     :error (plist-get job :error)))
      ('members
       (if (anvil-fusion--batch-terminal-p (plist-get job :member-batch))
           (anvil-fusion--members-terminal job-id job)
         (list :status 'running :stage 'members :job-id job-id)))
      ('extracting (anvil-fusion--advance-extract job-id job))
      ('verifying  (anvil-fusion--advance-verify job-id job))
      ('judging
       (if (anvil-fusion--batch-terminal-p (plist-get job :judge-batch))
           (anvil-fusion--advance-after-judge job-id job)
         (list :status 'running :stage 'judging :job-id job-id))))))

(provide 'anvil-fusion-async)
;;; anvil-fusion-async.el ends here
