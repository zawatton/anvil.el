;;; anvil-fusion-longrun-mcp.el --- MCP tools for long-horizon quests -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion, long-horizon, mcp

;;; Commentary:
;;
;; Phase 3 of docs/design/02-longrun.org: register the long-horizon quest
;; runner as MCP tools under the "emacs-eval" server, so the parent
;; Claude session can fire a multi-step quest and only ever sees the job
;; id + the final distilled answer -- the intermediate step context never
;; enters the parent.
;;
;; A quest runs minutes (each step is an orchestrator subprocess), well
;; past the MCP dispatch timeout, so the tools are the async trio from
;; `anvil-fusion-longrun-async':
;;
;;   longrun-start  -> create + launch a quest, returns a job id + quest id
;;   longrun-resume -> continue a saved quest from its checkpoint
;;   longrun-status -> advance one sub-stage and report; poll until done
;;
;; Daemon-only: requires `anvil-server' and reuses the self-contained
;; JSON encoder from `anvil-fusion-mcp'.  Call `anvil-fusion-longrun-enable'
;; once (e.g. from init) to register.

;;; Code:

(require 'anvil-server)
(require 'anvil-fusion-mcp)               ; JSON encoder + arg-sym helper
(require 'anvil-fusion-longrun-async)

(defconst anvil-fusion-longrun--server-id "emacs-eval"
  "MCP server id the quest tools register under (matches the stdio shim).")

(defun anvil-fusion-longrun--arg-int (s)
  "Parse non-empty string S to an integer, else nil."
  (and (stringp s) (not (string-empty-p s)) (truncate (string-to-number s))))

;;;; --- tool wrappers -------------------------------------------------------

(defun anvil-fusion-longrun--tool-start
    (goal &optional provider model max_steps digest_max_chars hermetic)
  "Start a long-horizon quest for GOAL; return a job id to poll.

MCP Parameters:
  goal             - The long task to pursue, multi-step (string, required).
  provider         - Step model provider: \"codex\" (default), \"claude\",
                     \"ollama\", ...
  model            - Optional explicit model id for the provider.
  max_steps        - Hard cap on steps (integer string; default 8).
  digest_max_chars - Cap on the carried state digest (integer string;
                     default 4000).  This bounds per-step context.
  hermetic         - \"true\" to lock each step to read-only
                     progressive-disclosure tools (file-outline /
                     file-read-snippet / file-read / org-index-search /
                     defs-search): the step pulls only what it needs --
                     no writes, no shell, no unbounded native Read.

Returns a plist (:job-id :quest-id :stage).  Poll `longrun-status'
with the job id until :status is \"done\"; the quest checkpoints to
SQLite each step so it survives interruption (resume via
`longrun-resume')."
  (anvil-server-with-error-handling
   (apply #'anvil-fusion-longrun-start-async goal
          (append
           (let ((p (anvil-fusion--arg-sym provider))) (and p (list :provider p)))
           (and model (stringp model) (not (string-empty-p model))
                (list :model model))
           (let ((n (anvil-fusion-longrun--arg-int max_steps)))
             (and n (list :max-steps n)))
           (let ((c (anvil-fusion-longrun--arg-int digest_max_chars)))
             (and c (list :digest-max-chars c)))
           (and (stringp hermetic)
                (member (downcase hermetic) '("t" "true" "1" "yes"))
                (list :hermetic t))))))

(defun anvil-fusion-longrun--tool-resume (quest_id &optional max_steps)
  "Resume a saved quest from its checkpoint; return a job id to poll.

MCP Parameters:
  quest_id  - Quest id returned by `longrun-start' (string, required).
  max_steps - Optional new step cap (integer string).  Raise it to
              continue a quest that stopped at its previous budget.

Returns a plist (:job-id :quest-id :stage [:resumed-from])."
  (anvil-server-with-error-handling
   (apply #'anvil-fusion-longrun-resume-async quest_id
          (let ((n (anvil-fusion-longrun--arg-int max_steps)))
            (and n (list :max-steps n))))))

(defun anvil-fusion-longrun--tool-status (job_id)
  "Advance and report a quest job started by `longrun-start'/`-resume'.

MCP Parameters:
  job_id - Job id to poll (string, required).

Each call advances the quest at most one sub-stage (a step or a
distillation) while the orchestrator runs the subprocess in the
background.  Returns a plist whose :status is \"running\" / \"done\" /
\"error\"; when \"done\" it carries :answer (the distilled result),
:digest, :steps and :stopped."
  (anvil-server-with-error-handling
   (anvil-fusion-longrun-status job_id)))

;;;; --- lifecycle -----------------------------------------------------------

(defun anvil-fusion-longrun--register-tools ()
  "Register the long-horizon quest MCP tools under the emacs-eval server."
  (anvil-server-register-tool
   (anvil-fusion--encode-handler #'anvil-fusion-longrun--tool-start)
   :id "longrun-start"
   :server-id anvil-fusion-longrun--server-id
   :description
   "Start a long-horizon (multi-step) quest that carries a bounded
distilled state digest instead of the full history, so per-step
context stays O(1) -- recovering Fable-class sustained task execution
WITHOUT polluting the parent session's context.  Each step runs in an
isolated orchestrator subprocess and checkpoints to SQLite (resumable).
Returns a job id immediately; poll `longrun-status' until done.")

  (anvil-server-register-tool
   (anvil-fusion--encode-handler #'anvil-fusion-longrun--tool-resume)
   :id "longrun-resume"
   :server-id anvil-fusion-longrun--server-id
   :description
   "Resume a long-horizon quest from its last SQLite checkpoint (after a
daemon restart, kill, or hitting a previous step budget).  Returns a
job id; poll `longrun-status' until done.")

  (anvil-server-register-tool
   (anvil-fusion--encode-handler #'anvil-fusion-longrun--tool-status)
   :id "longrun-status"
   :server-id anvil-fusion-longrun--server-id
   :description
   "Poll a quest job from `longrun-start'/`longrun-resume'.  Each call
advances the quest at most one sub-stage and reports :status
running / done / error; when done, :answer holds the distilled result
plus :digest / :steps / :stopped."))

(defun anvil-fusion-longrun--unregister-tools ()
  "Unregister the long-horizon quest MCP tools."
  (dolist (id '("longrun-start" "longrun-resume" "longrun-status"))
    (anvil-server-unregister-tool id anvil-fusion-longrun--server-id)))

;;;###autoload
(defun anvil-fusion-longrun-enable ()
  "Enable anvil-fusion-longrun: register the quest MCP tools."
  (interactive)
  (anvil-fusion-longrun--register-tools))

(defun anvil-fusion-longrun-disable ()
  "Disable anvil-fusion-longrun: unregister the quest MCP tools."
  (interactive)
  (anvil-fusion-longrun--unregister-tools))

;; anvil-optional-modules entry point: expose this module as
;; `fusion-longrun-mcp' for `anvil--load-module' (aliases the real
;; enable/disable defined above).
(defalias 'anvil-fusion-longrun-mcp-enable #'anvil-fusion-longrun-enable)
(defalias 'anvil-fusion-longrun-mcp-disable #'anvil-fusion-longrun-disable)

(provide 'anvil-fusion-longrun-mcp)
;;; anvil-fusion-longrun-mcp.el ends here
