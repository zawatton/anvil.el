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
;; State machine per job: members -> judging -> done.  Each poll only
;; checks batch terminality (via the public `anvil-orchestrator-status')
;; and advances at most one stage, so no call blocks.
;;
;; This v1 runs a SINGLE round (no critique loop); use the blocking
;; `anvil-fusion-ask' for the loop.  Stubbable: depends only on
;; anvil-orchestrator public functions.

;;; Code:

(require 'anvil-fusion)
(require 'anvil-fusion-panels)

(declare-function anvil-orchestrator-submit "anvil-orchestrator" (tasks))
(declare-function anvil-orchestrator-status "anvil-orchestrator" (id))
(declare-function anvil-orchestrator-extract-result "anvil-orchestrator" (task-id &optional full))

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

(cl-defun anvil-fusion-ask-async
    (prompt &key panel fidelity judge judge-model lenses extra timeout-sec)
  "Start a fusion ask for PROMPT and return a job plist immediately.

Submits the panel fan-out (non-blocking) and registers a job.
Keyword args mirror `anvil-fusion-ask' (minus the loop knobs; the
async path is single-round).  For a `local-only' panel a non-local
JUDGE override is refused before anything is submitted.

Returns (:job-id STR :stage members :panel SYM :egress SYM).  Poll
`anvil-fusion-result' with the job id."
  (require 'anvil-orchestrator)
  (let ((pname (or panel anvil-fusion-default-panel)))
    (anvil-fusion-panel-validate pname)
    (let* ((body   (anvil-fusion-panel-get pname))
           (egress (anvil-fusion-panel-egress body))
           (jspec  (anvil-fusion-panel-judge body))
           (jprov  (or judge (car jspec)))
           (jmodel (or judge-model (cdr jspec))))
      (when (and (eq egress 'local-only)
                 (not (anvil-fusion-provider-local-p jprov)))
        (user-error
         "anvil-fusion-ask-async: panel %s is local-only; refusing external judge %S"
         pname jprov))
      (let* ((member-tasks (anvil-fusion-panel-tasks body prompt lenses))
             (mbatch (anvil-orchestrator-submit member-tasks))
             (job-id (concat "fusion-" mbatch))
             (job (list :job-id job-id :stage 'members :prompt prompt
                        :panel pname :egress egress :body body
                        :jprov jprov :jmodel jmodel :fidelity fidelity
                        :extra extra :timeout-sec timeout-sec
                        :member-batch mbatch :judge-batch nil :answer nil)))
        (puthash job-id job anvil-fusion--jobs)
        (list :job-id job-id :stage 'members :panel pname :egress egress)))))

(defun anvil-fusion--advance-to-judge (job-id job)
  "Members are terminal: submit the judge task and move JOB to judging."
  (let* ((mbatch  (plist-get job :member-batch))
         (status  (anvil-orchestrator-status mbatch))
         (cands   (plist-get status :tasks))
         (jprompt (anvil-fusion-build-judge-prompt
                   (plist-get job :prompt) cands
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
    (setq job (plist-put job :candidates cands))
    (setq job (plist-put job :stage 'judging))
    (puthash job-id job anvil-fusion--jobs)
    (list :status 'running :stage 'judging :job-id job-id)))

(defun anvil-fusion--finish (job-id job)
  "Judge is terminal: extract the fused answer and move JOB to done."
  (let* ((jbatch (plist-get job :judge-batch))
         (jid    (anvil-fusion--batch-first-task-id jbatch))
         (res    (anvil-orchestrator-extract-result jid t))
         (answer (plist-get res :summary)))
    (setq job (plist-put job :answer answer))
    (setq job (plist-put job :stage 'done))
    (puthash job-id job anvil-fusion--jobs)
    (list :status 'done :job-id job-id :answer answer
          :panel (plist-get job :panel) :egress (plist-get job :egress)
          :n-candidates (length (plist-get job :candidates)))))

(defun anvil-fusion-result (job-id)
  "Advance and report the async fusion JOB-ID (non-blocking).
Returns a status plist; poll until :status is `done' (or `error').
Stages: members -> judging -> done."
  (require 'anvil-orchestrator)
  (let ((job (gethash job-id anvil-fusion--jobs)))
    (unless job
      (user-error "anvil-fusion-result: unknown job %s" job-id))
    (pcase (plist-get job :stage)
      ('done   (list :status 'done :job-id job-id
                     :answer (plist-get job :answer)
                     :panel (plist-get job :panel)
                     :egress (plist-get job :egress)
                     :n-candidates (length (plist-get job :candidates))))
      ('error  (list :status 'error :job-id job-id
                     :error (plist-get job :error)))
      ('members
       (if (anvil-fusion--batch-terminal-p (plist-get job :member-batch))
           (anvil-fusion--advance-to-judge job-id job)
         (list :status 'running :stage 'members :job-id job-id)))
      ('judging
       (if (anvil-fusion--batch-terminal-p (plist-get job :judge-batch))
           (anvil-fusion--finish job-id job)
         (list :status 'running :stage 'judging :job-id job-id))))))

(provide 'anvil-fusion-async)
;;; anvil-fusion-async.el ends here
