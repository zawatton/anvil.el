;;; anvil-fusion-longrun-async.el --- non-blocking quest runner (submit + poll) -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion, long-horizon

;;; Commentary:
;;
;; Phase 3 of docs/design/02-longrun.org: a non-blocking quest runner so
;; a long-horizon quest can drive from the MCP transport without ever
;; blocking the dispatch.
;;
;; `anvil-fusion-longrun-run' blocks for the whole multi-step quest (each
;; step is an orchestrator submit + wait), which is far past the MCP
;; dispatch timeout.  This module mirrors `anvil-fusion-async': a job is a
;; small state machine the caller polls, advancing AT MOST one sub-stage
;; per poll while the orchestrator runs the submitted step / distill
;; subprocess in the background.
;;
;; State machine per step: step-ready -> step-running -> distill-running
;;   -> (checkpoint) -> next step-running/distill-running, until
;; STATUS: DONE or the step budget.
;; Every checkpoint persists to the SQLite store (Phase 2), so the parent
;; only ever sees the job id and, at the end, the distilled answer --
;; intermediate step outputs never enter the parent's context.
;;
;; Stubbable: depends only on anvil-orchestrator public functions plus
;; the Phase 1/2 pure helpers, so ERT drives the full lifecycle with
;; cl-letf stubs (no real model).
;;
;; Doc 61 Phase 8b adds an intentionally bounded blocking escape hatch:
;; when a step is routed to a fusion panel, the async runner does not
;; submit a normal step batch.  Instead, one `anvil-fusion-longrun-status'
;; poll synchronously calls `anvil-fusion-ask' (bounded by
;; `anvil-fusion-longrun-async-panel-max-wait-sec'), stores its answer
;; as the step output, and immediately submits the distill batch.  This
;; mirrors the deliberate bounded-blocking pattern already used in
;; `anvil-fusion-async''s extracting / verifying stages (Doc 61 6d').

;;; Code:

(require 'cl-lib)
(require 'anvil-fusion-longrun)
(require 'anvil-fusion-longrun-store)

(declare-function anvil-orchestrator-submit "anvil-orchestrator" (tasks))
(declare-function anvil-orchestrator-status "anvil-orchestrator" (id))
(declare-function anvil-orchestrator-extract-result "anvil-orchestrator" (task-id &optional full))
(declare-function anvil-fusion-ask "anvil-fusion-ask" (prompt &rest args))

;; Phase 4 convergence helpers / knobs live in anvil-fusion-longrun (required
;; above); forward-declare so a clean byte-compile env stays warning-free.
(defvar anvil-fusion-longrun-converge-threshold)
(defvar anvil-fusion-longrun-converge-patience)
(declare-function anvil-fusion-longrun--streak "anvil-fusion-longrun"
                  (streak prev digest threshold patience))
(declare-function anvil-fusion-longrun--converged-p "anvil-fusion-longrun"
                  (streak patience))
;; Phase 4b hermetic-step helpers / knobs (also in anvil-fusion-longrun).
(defvar anvil-fusion-longrun-hermetic-instruction)
(declare-function anvil-fusion-longrun-disclosure-tools-string "anvil-fusion-longrun" ())
(declare-function anvil-fusion-longrun--apply-suffix "anvil-fusion-longrun" (prompt suffix))
(declare-function anvil-fusion-longrun--allowed-tools-plist "anvil-fusion-longrun"
                  (allowed-tools))
(declare-function anvil-fusion-longrun--resolve-allowed-tools "anvil-fusion-longrun"
                  (step-allowed-tools hermetic profile))
(declare-function anvil-fusion-longrun--hermetic-suffix "anvil-fusion-longrun"
                  (allowed-tools-string))
(declare-function anvil-fusion-longrun--panel-step-p "anvil-fusion-longrun"
                  (step-panel step-panel-mode next-hard panel-budget-left))
(defvar anvil-fusion-longrun-hermetic-manifest-profile)
(defvar anvil-fusion-longrun-hermetic-disallowed-tools)
(defvar anvil-fusion-longrun-max-panel-steps)
(defvar anvil-orchestrator-manifest-profile)

(defvar anvil-fusion-longrun--jobs (make-hash-table :test 'equal)
  "Map job-id -> job plist for in-flight async quests.")

(defcustom anvil-fusion-longrun-async-panel-max-wait-sec 900
  "Upper bound for a synchronous async-panel step inside one poll advance.
When a Doc 61 Phase 8b panel step is chosen, `anvil-fusion-longrun-status'
blocks only long enough to call `anvil-fusion-ask' once, then resumes the
normal distill polling flow."
  :type 'integer :group 'anvil-fusion-longrun)

;;;; --- orchestrator helpers (non-blocking) ---------------------------------

(defun anvil-fusion-longrun--batch-terminal-p (batch-id)
  "Return non-nil when BATCH-ID has no running/queued tasks left."
  (let ((s (anvil-orchestrator-status batch-id)))
    (and s
         (zerop (or (plist-get s :running) 0))
         (zerop (or (plist-get s :queued) 0)))))

(defun anvil-fusion-longrun--batch-output (batch-id)
  "Return the full result text of BATCH-ID's first task."
  (let ((id (anvil-fusion--batch-first-task-id batch-id)))
    (or (plist-get (anvil-orchestrator-extract-result id t) :summary) "")))

(defun anvil-fusion-longrun--task (provider prompt name model cwd
                                            &optional allowed-tools manifest-profile)
  "Build a single orchestrator task plist (ALLOWED-TOOLS restricts the member).
When MANIFEST-PROFILE is set the task also requests skip-permissions +
a native-tool deny-list so the injected read-only MCP server is usable
in non-interactive print mode."
  (append (list :provider provider :prompt prompt :name name
                ;; Quest steps share one fixed cwd (see --run-one): suppress
                ;; auto-worktree so the child reads/writes the real tree, not a
                ;; per-step isolated copy that scopes file access away from it.
                :no-worktree t)
          (and model (list :model model))
          (and cwd (list :cwd cwd))
          (anvil-fusion-longrun--allowed-tools-plist allowed-tools)
          (and manifest-profile
               (list :manifest-profile manifest-profile
                     :skip-permissions t
                     :disallowed-tools anvil-fusion-longrun-hermetic-disallowed-tools))))

(defun anvil-fusion-longrun--submit-step (job)
  "Advance JOB into its next step submission.
Normal steps submit an orchestrator batch and return JOB at stage
`step-running'.  Panel steps synchronously call `anvil-fusion-ask'
within this poll advance (bounded by
`anvil-fusion-longrun-async-panel-max-wait-sec'), then immediately
submit the distill batch and return JOB at stage `distill-running'."
  (let* ((target  (1+ (plist-get job :step)))
         (sprompt (anvil-fusion-longrun--apply-suffix
                   (anvil-fusion-longrun-build-step-prompt
                    (plist-get job :goal) (plist-get job :digest)
                    target (plist-get job :maxn))
                   (plist-get job :step-suffix)))
         (want-panel
          (and (plist-get job :step-panel)
               (pcase (or (plist-get job :step-panel-mode) 'hard-only)
                 ('always t)
                 (_ (plist-get job :next-hard)))))
         (panelp (anvil-fusion-longrun--panel-step-p
                  (plist-get job :step-panel)
                  (plist-get job :step-panel-mode)
                  (plist-get job :next-hard)
                  (plist-get job :panel-budget-left))))
    (if panelp
        (progn
          (require 'anvil-fusion-ask)
          (setq job (plist-put job :panel-budget-left
                               (1- (plist-get job :panel-budget-left))))
          (setq job (plist-put job :panel-steps
                               (1+ (or (plist-get job :panel-steps) 0))))
          (setq job (plist-put job :last-panelp t))
          (anvil-fusion-longrun--submit-distill
           job
           (plist-get
            (anvil-fusion-ask
             sprompt
             :panel (plist-get job :step-panel)
             :verify (plist-get job :panel-verify)
             :cwd (plist-get job :cwd)
             :max-wait-sec anvil-fusion-longrun-async-panel-max-wait-sec)
            :answer)))
      (let* ((step-task (anvil-fusion-longrun--task
                         (plist-get job :prov) sprompt
                         (format "lr-step-%d" target)
                         (plist-get job :model) (plist-get job :cwd)
                         (plist-get job :step-allowed-tools)
                         (plist-get job :step-manifest-profile)))
             (batch (anvil-orchestrator-submit (list step-task))))
        (when (and want-panel
                   (<= (or (plist-get job :panel-budget-left) 0) 0)
                   (not (plist-get job :panel-budget-noted)))
          (setq job (plist-put job :panel-budget-noted t))
          (message "anvil-fusion-longrun-async: panel-step budget exhausted; falling back to single-provider steps"))
        (setq job (plist-put job :last-panelp nil))
        (setq job (plist-put job :batch batch))
        (setq job (plist-put job :stage 'step-running))
        job))))

(defun anvil-fusion-longrun--submit-distill (job step-output)
  "Submit the distill task folding STEP-OUTPUT for JOB; stage `distill-running'."
  (let* ((target  (1+ (plist-get job :step)))
         (dprompt (anvil-fusion-longrun-build-distill-prompt
                   (plist-get job :goal) (plist-get job :digest)
                   step-output (plist-get job :maxc)))
         (batch   (anvil-orchestrator-submit
                   (list (anvil-fusion-longrun--task
                          (plist-get job :dprov) dprompt
                          (format "lr-distill-%d" target)
                          (plist-get job :dmodel) (plist-get job :cwd))))))
    (setq job (plist-put job :last-output-chars (length (or step-output ""))))
    (setq job (plist-put job :batch batch))
    (setq job (plist-put job :stage 'distill-running))
    job))

;;;; --- start / resume ------------------------------------------------------

(cl-defun anvil-fusion-longrun-start-async
    (goal &key provider model distill-provider distill-model
          max-steps digest-max-chars cwd db hermetic step-allowed-tools
          step-manifest-profile step-panel (step-panel-mode 'hard-only)
          panel-verify)
  "Start a quest for GOAL and return a job plist immediately.
Creates the persisted quest and registers a poll-able job without
blocking the start call.  The first normal step batch is submitted on
the first poll; if a step is routed to a panel, that poll may block
once for `anvil-fusion-ask' (bounded by
`anvil-fusion-longrun-async-panel-max-wait-sec') before submitting the
distill batch.  DB defaults to the cached store.  Returns
(:job-id :quest-id :stage)."
  (require 'anvil-orchestrator)
  (let* ((db    (or db (anvil-fusion-longrun-store-default-db)))
         (prov  (or provider anvil-fusion-longrun-default-provider))
         (maxn  (or max-steps anvil-fusion-longrun-max-steps))
         (maxc  (or digest-max-chars anvil-fusion-longrun-digest-max-chars))
         (cwd   (or cwd anvil-fusion-longrun-default-cwd))
         (smp   (or step-manifest-profile
                    (and hermetic anvil-fusion-longrun-hermetic-manifest-profile)))
         (sat   (anvil-fusion-longrun--resolve-allowed-tools
                 step-allowed-tools hermetic smp))
         (suf   (and hermetic (anvil-fusion-longrun--hermetic-suffix sat)))
         (qid   (anvil-fusion-longrun-store-create
                 db :goal goal :provider prov :model model
                 :max-steps maxn :digest-max-chars maxc :cwd cwd))
         (job   (list :job-id (concat "lr-" qid) :quest-id qid :goal goal
                      :prov prov :model model
                      :dprov (or distill-provider prov)
                      :dmodel (or distill-model model)
                      :maxn maxn :maxc maxc :cwd cwd :db db
                      :step-allowed-tools sat :step-suffix suf
                      :step-manifest-profile smp
                      :step-panel step-panel
                      :step-panel-mode step-panel-mode
                      :panel-verify panel-verify
                      :panel-budget-left anvil-fusion-longrun-max-panel-steps
                      :panel-steps 0
                      :next-hard nil
                      :step 0 :digest nil :stage 'step-ready :batch nil)))
    (puthash (plist-get job :job-id) job anvil-fusion-longrun--jobs)
    (list :job-id (plist-get job :job-id) :quest-id qid
          :stage 'step-ready :panel-steps 0)))

(cl-defun anvil-fusion-longrun-resume-async
    (quest-id &key max-steps db hermetic step-allowed-tools
              step-manifest-profile step-panel (step-panel-mode 'hard-only)
              panel-verify)
  "Resume QUEST-ID from its checkpoint and return a job plist immediately.
Registers the next step without blocking the resume call.  The first
poll advances it as in `anvil-fusion-longrun-start-async'.  Returns
(:job-id :quest-id :stage [:resumed-from])."
  (require 'anvil-orchestrator)
  (let* ((db   (or db (anvil-fusion-longrun-store-default-db)))
         (q    (or (anvil-fusion-longrun-store-get db quest-id)
                   (user-error "anvil-fusion-longrun: unknown quest %s" quest-id)))
         (maxn (or max-steps (plist-get q :max-steps)))
         (prov (let ((p (plist-get q :provider))) (and p (intern p))))
         (smp  (or step-manifest-profile
                   (and hermetic anvil-fusion-longrun-hermetic-manifest-profile)))
         (sat  (anvil-fusion-longrun--resolve-allowed-tools
                step-allowed-tools hermetic smp))
         (suf  (and hermetic (anvil-fusion-longrun--hermetic-suffix sat)))
         (job  (list :job-id (concat "lr-" quest-id) :quest-id quest-id
                     :goal (plist-get q :goal)
                     :prov (or prov anvil-fusion-longrun-default-provider)
                     :model (plist-get q :model)
                     :dprov (or prov anvil-fusion-longrun-default-provider)
                     :dmodel (plist-get q :model)
                     :maxn maxn
                     :maxc (or (plist-get q :digest-max-chars)
                               anvil-fusion-longrun-digest-max-chars)
                     :cwd (or (plist-get q :cwd) anvil-fusion-longrun-default-cwd)
                     :db db
                     :step-allowed-tools sat :step-suffix suf
                     :step-manifest-profile smp
                     :step-panel step-panel
                     :step-panel-mode step-panel-mode
                     :panel-verify panel-verify
                     :panel-budget-left anvil-fusion-longrun-max-panel-steps
                     :panel-steps 0
                     :next-hard nil
                     :step (plist-get q :step) :digest (plist-get q :digest)
                     :stage 'step-ready :batch nil)))
    (if (>= (plist-get job :step) maxn)
        (list :job-id (plist-get job :job-id) :quest-id quest-id
              :stage 'done :note "already at budget")
      (progn
        (puthash (plist-get job :job-id) job anvil-fusion-longrun--jobs)
        (list :job-id (plist-get job :job-id) :quest-id quest-id
              :stage 'step-ready :resumed-from (plist-get q :step))))))

;;;; --- poll (advance one sub-stage) ----------------------------------------

(defun anvil-fusion-longrun--report-done (job)
  "Return the terminal status plist for JOB."
  (list :status 'done :job-id (plist-get job :job-id)
        :quest-id (plist-get job :quest-id)
        :answer (plist-get job :digest)
        :digest (plist-get job :digest)
        :steps (plist-get job :step)
        :panel-steps (or (plist-get job :panel-steps) 0)
        :stopped (or (plist-get job :stopped) 'done)))

(defun anvil-fusion-longrun--running-status (job)
  "Return the current non-terminal status plist for JOB."
  (list :status 'running
        :stage (plist-get job :stage)
        :job-id (plist-get job :job-id)
        :step (1+ (plist-get job :step))
        :panel-steps (or (plist-get job :panel-steps) 0)))

(defun anvil-fusion-longrun--advance-after-distill (job-id job)
  "Distill batch terminal: parse, checkpoint, then finish or submit next step."
  (let* ((prev   (plist-get job :digest))
         (draw   (anvil-fusion-longrun--batch-output (plist-get job :batch)))
         (parsed (anvil-fusion-longrun--parse-distill draw (plist-get job :maxc)))
         (digest (plist-get parsed :digest))
         (done   (plist-get parsed :done))
         (next-hard (plist-get parsed :next-hard))
         (pat    anvil-fusion-longrun-converge-patience)
         (streak (anvil-fusion-longrun--streak
                  (or (plist-get job :streak) 0) prev digest
                  anvil-fusion-longrun-converge-threshold pat))
         (converged (anvil-fusion-longrun--converged-p streak pat))
         (target (1+ (plist-get job :step)))
         (eot    (or done converged (>= target (plist-get job :maxn))))
         (meta   (list :step target
                       :panelp (plist-get job :last-panelp)
                       :output-chars (plist-get job :last-output-chars)
                       :digest-chars (length (or digest ""))
                       :digest-head (let ((d (or digest "")))
                                      (substring d 0 (min 80 (length d))))
                       :done (or done converged))))
    (setq job (plist-put job :step target))
    (setq job (plist-put job :digest digest))
    (setq job (plist-put job :next-hard next-hard))
    (setq job (plist-put job :streak streak))
    (anvil-fusion-longrun-store-checkpoint
     (plist-get job :db) (plist-get job :quest-id) target digest
     (if (or done converged) "done" "running") meta)
    (if eot
        (let ((stopped (cond (done 'done) (converged 'converged) (t 'budget))))
          (anvil-fusion-longrun-store-finish
           (plist-get job :db) (plist-get job :quest-id) target digest stopped)
          (setq job (plist-put job :stopped stopped))
          (setq job (plist-put job :stage 'done))
          (puthash job-id job anvil-fusion-longrun--jobs)
          (anvil-fusion-longrun--report-done job))
      (progn
        (setq job (anvil-fusion-longrun--submit-step job))
        (puthash job-id job anvil-fusion-longrun--jobs)
        (anvil-fusion-longrun--running-status job)))))

(defun anvil-fusion-longrun-status (job-id)
  "Advance and report async quest JOB-ID (non-blocking).
Each call advances at most one sub-stage; poll until :status is
`done' (or `error').  When done, :answer holds the distilled result."
  (require 'anvil-orchestrator)
  (let ((job (gethash job-id anvil-fusion-longrun--jobs)))
    (unless job
      (user-error "anvil-fusion-longrun-status: unknown job %s" job-id))
    (pcase (plist-get job :stage)
      ('done  (anvil-fusion-longrun--report-done job))
      ('error (list :status 'error :job-id job-id :error (plist-get job :error)))
      ('step-ready
       (setq job (anvil-fusion-longrun--submit-step job))
       (puthash job-id job anvil-fusion-longrun--jobs)
       (anvil-fusion-longrun--running-status job))
      ('step-running
       (if (anvil-fusion-longrun--batch-terminal-p (plist-get job :batch))
           (let ((out (anvil-fusion-longrun--batch-output (plist-get job :batch))))
             (setq job (anvil-fusion-longrun--submit-distill job out))
             (puthash job-id job anvil-fusion-longrun--jobs)
             (anvil-fusion-longrun--running-status job))
         (anvil-fusion-longrun--running-status job)))
      ('distill-running
       (if (anvil-fusion-longrun--batch-terminal-p (plist-get job :batch))
           (anvil-fusion-longrun--advance-after-distill job-id job)
         (anvil-fusion-longrun--running-status job))))))

(provide 'anvil-fusion-longrun-async)
;;; anvil-fusion-longrun-async.el ends here
