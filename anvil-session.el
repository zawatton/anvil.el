;;; anvil-session.el --- Session snapshots + Claude Code hooks  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;;; Commentary:

;; Doc 17 — Session snapshot / resume / Claude Code lifecycle hooks.
;;
;; This module answers "how does a resuming Claude pick up where the
;; previous session left off without re-reading half the project?"
;;
;; The surface has three layers (Doc 17 Phases 1 / 3 / 4):
;;
;;   Phase 1 (primitives):  `anvil-session-snapshot' / -resume / -list
;;                          / -delete.  Plist records stored in
;;                          anvil-state ns="session", TTL 14 days.
;;
;;   Phase 3 (hooks):       `anvil-session-hook-dispatch' routes
;;                          Claude Code lifecycle events (SessionStart
;;                          / PreCompact / PostToolUse /
;;                          UserPromptSubmit / SessionEnd) to snapshot
;;                          + event-index primitives.  Events land in
;;                          anvil-state ns="session-events" for
;;                          per-session audit + search.
;;
;;   Phase 4 (opt-in):      `anvil-session-hooks-enabled' defcustom +
;;                          `anvil-hook-install-settings' command that
;;                          writes the hook bindings into
;;                          `~/.claude/settings.json' (dry-run, install,
;;                          uninstall, preserves existing hooks).
;;
;; Snapshot payload shape (plist):
;;   :name STR :created-at FLOAT-TIME
;;   :branch STR :base-branch STR
;;   :task-summary STR :notes LIST
;;   :preamble-suggested STR
;;
;; Event row shape (plist):
;;   :session-id STR :ts FLOAT-TIME :kind STR :tool STR :summary STR
;;
;; Both are round-trippable through `prin1' / `read' so anvil-state's
;; serializer handles them with no extra ceremony.

;;; Code:

(require 'cl-lib)
(require 'anvil-state)
(require 'anvil-server)


;;;; --- group + constants --------------------------------------------------

(defgroup anvil-session nil
  "Session snapshot / resume / Claude Code hook integration (Doc 17)."
  :group 'anvil
  :prefix "anvil-session-")

(defconst anvil-session--server-id "emacs-eval"
  "Server ID under which session-* MCP tools are registered.")

(defconst anvil-session--snapshot-ns "session"
  "anvil-state namespace for snapshot storage.")

(defconst anvil-session--events-ns "session-events"
  "anvil-state namespace for per-session event log rows.
Row keys use the shape \"<session-id>/<zero-padded-seq>-<ts>\" so
`anvil-state-list-keys' returns events in insertion order for a
given session, and wildcards like \"SESSION/\" act as session
prefixes for search.")

(defcustom anvil-session-ttl-sec (* 14 24 60 60)
  "TTL (seconds) for snapshot rows before anvil-state purges them.
Default 14 days.  Event rows use `anvil-session-events-ttl-sec'."
  :type 'integer
  :group 'anvil-session)

(defcustom anvil-session-events-ttl-sec (* 14 24 60 60)
  "TTL (seconds) for event-log rows (`session-events' namespace).
Default 14 days."
  :type 'integer
  :group 'anvil-session)

(defcustom anvil-session-event-summary-max-chars 200
  "Upper bound on `:summary' chars recorded per event.
Matches the Doc 17 design target — keeps the FTS surface tight
without needing full payload replay."
  :type 'integer
  :group 'anvil-session)

(defcustom anvil-session-default-base-branch "main"
  "Fallback `:base-branch' for snapshots when the caller omits it."
  :type 'string
  :group 'anvil-session)

(defcustom anvil-session-hooks-enabled nil
  "Non-nil to opt into Claude Code lifecycle hook dispatch (Phase 4).
The defcustom itself does nothing — it exists for
`anvil-hook-install-settings' to short-circuit when the user has
not explicitly opted in, matching the Doc 17 safety-rail design."
  :type 'boolean
  :group 'anvil-session)


;;;; --- Phase 1: snapshot primitives ---------------------------------------

(defun anvil-session--git-branch ()
  "Return the current git branch name as a string, or nil.
Uses a plain `process-file' / `call-process' shell-out rather than
requiring magit — anvil-session has to work on machines without
heavy git helpers loaded (e.g. a fresh worktree in CI)."
  (let ((out (with-temp-buffer
               (when (zerop (or (ignore-errors
                                  (call-process
                                   "git" nil t nil
                                   "rev-parse" "--abbrev-ref" "HEAD"))
                                1))
                 (string-trim (buffer-string))))))
    (and (stringp out) (not (string-empty-p out)) out)))

(defun anvil-session--format-preamble (snap)
  "Build a resume-prompt string from SNAP's plist.
The Doc 17 contract: one preamble block that Claude can drop into
its first response without ≥N `Read' calls to rebuild context.
Only non-empty fields are surfaced; the caller may always fall
back to the raw plist via `anvil-session-resume'."
  (let ((name    (plist-get snap :name))
        (created (plist-get snap :created-at))
        (branch  (plist-get snap :branch))
        (base    (plist-get snap :base-branch))
        (task    (plist-get snap :task-summary))
        (notes   (plist-get snap :notes)))
    (concat
     (format "Resuming session %S (last active %s).\n"
             (or name "anon")
             (if (numberp created)
                 (format-time-string "%Y-%m-%d %H:%M" created)
               "?"))
     (format "Branch: %s (base %s).\n"
             (or branch "?") (or base "?"))
     (when (and (stringp task) (not (string-empty-p task)))
       (format "Last task: %s\n" task))
     (when notes
       (concat "Recent notes:\n"
               (mapconcat (lambda (n) (format "  - %s" n))
                          (if (listp notes) notes (list notes))
                          "\n")
               "\n")))))

;;;###autoload
(cl-defun anvil-session-snapshot (name &key branch base-branch
                                         task-summary notes)
  "Capture current state into a named snapshot NAME, persist, return plist.

Keyword arguments:
  :branch        — override autodetected git branch.
  :base-branch   — override `anvil-session-default-base-branch'.
  :task-summary  — human-readable \"what was I doing\" sentence.
  :notes         — list of bullet strings; single string wraps to list.

Writes into anvil-state namespace `session' with TTL
`anvil-session-ttl-sec'.  Returns the stored plist (including the
auto-generated `:preamble-suggested')."
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "anvil-session-snapshot: NAME must be a non-empty string"))
  (let* ((snap (list :name name
                     :created-at (float-time)
                     :branch (or branch (anvil-session--git-branch))
                     :base-branch (or base-branch
                                      anvil-session-default-base-branch)
                     :task-summary task-summary
                     :notes (cond ((null notes) nil)
                                  ((stringp notes) (list notes))
                                  ((listp notes) notes)
                                  (t (list (format "%s" notes))))))
         (snap (plist-put snap :preamble-suggested
                          (anvil-session--format-preamble snap))))
    (anvil-state-set name snap
                     :ns anvil-session--snapshot-ns
                     :ttl anvil-session-ttl-sec)
    snap))

;;;###autoload
(defun anvil-session-resume (name)
  "Return the snapshot plist stored under NAME, or nil when missing.
Does not mutate live state; the caller composes the resume flow
(e.g. feeding `:preamble-suggested' to `orchestrator-preamble-set')."
  (anvil-state-get name :ns anvil-session--snapshot-ns))

;;;###autoload
(defun anvil-session-list ()
  "Return a list of snapshot descriptor plists for every live row.
Each descriptor carries `:name', `:created-at', `:branch', and
`:task-summary-head' (first 80 chars of the stored summary, or
nil).  The full payload remains accessible via `session-resume'."
  (let (out)
    (dolist (k (anvil-state-list-keys :ns anvil-session--snapshot-ns))
      (let ((s (anvil-state-get k :ns anvil-session--snapshot-ns)))
        (when s
          (push (list :name (plist-get s :name)
                      :created-at (plist-get s :created-at)
                      :branch (plist-get s :branch)
                      :task-summary-head
                      (let ((t-sum (plist-get s :task-summary)))
                        (and (stringp t-sum)
                             (substring t-sum 0
                                        (min 80 (length t-sum))))))
                out))))
    (nreverse out)))

;;;###autoload
(defun anvil-session-delete (name)
  "Purge snapshot NAME from anvil-state; return t when a row was deleted."
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "anvil-session-delete: NAME must be a non-empty string"))
  (anvil-state-delete name :ns anvil-session--snapshot-ns))


;;;; --- MCP tool wrappers --------------------------------------------------

(defun anvil-session--tool-snapshot (name &optional task-summary notes
                                          branch base-branch)
  "MCP wrapper for `anvil-session-snapshot'.

MCP Parameters:
  name          - Non-empty snapshot identifier (string).
  task-summary  - Optional human-readable \"what was I doing\".
  notes         - Optional list of bullet strings (JSON array); a
                  single string wraps to `(list …)' automatically.
  branch        - Optional override for autodetected git branch.
  base-branch   - Optional base branch (default
                  `anvil-session-default-base-branch')."
  (let ((notes-list
         (cond ((null notes) nil)
               ((stringp notes)
                (if (string-empty-p notes) nil (list notes)))
               ((vectorp notes) (append notes nil))
               ((listp notes) notes)
               (t (list (format "%s" notes))))))
    (anvil-session-snapshot
     name
     :branch (and (stringp branch) (not (string-empty-p branch)) branch)
     :base-branch (and (stringp base-branch)
                       (not (string-empty-p base-branch)) base-branch)
     :task-summary (and (stringp task-summary)
                        (not (string-empty-p task-summary)) task-summary)
     :notes notes-list)))

(defun anvil-session--tool-resume (name)
  "MCP wrapper for `anvil-session-resume'.

MCP Parameters:
  name - Snapshot identifier to fetch.  Returns nil when absent
         (MCP layer turns nil into a JSON null)."
  (or (anvil-session-resume name)
      (list :error (format "anvil-session: no snapshot named %S" name))))

(defun anvil-session--tool-list ()
  "MCP wrapper for `anvil-session-list'.  Returns a vector of
descriptor plists so the JSON encoder renders an array rather
than an object.

MCP Parameters: none."
  (apply #'vector (anvil-session-list)))

(defun anvil-session--tool-delete (name)
  "MCP wrapper for `anvil-session-delete'.

MCP Parameters:
  name - Snapshot identifier to purge.  Returns {:deleted t} on
         success, {:deleted :false} when the row did not exist."
  (list :deleted (if (anvil-session-delete name) t :false)
        :name name))


;;;; --- module lifecycle ---------------------------------------------------

(defun anvil-session--register-tools ()
  "Register every session-* MCP tool under the shared server-id."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-session--tool-snapshot)
   :id "session-snapshot"
   :server-id anvil-session--server-id
   :description
   "Capture the current session state (branch + task-summary + notes)
into a named plist under anvil-state ns=session (TTL 14d).  Returns
the stored snapshot including `preamble-suggested' — a self-
contained resume block callers can drop into orchestrator-preamble
or an LLM re-entry prompt.  Write tool.")
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-session--tool-resume)
   :id "session-resume"
   :server-id anvil-session--server-id
   :description
   "Return the stored snapshot plist for NAME, or an error envelope
when none exists.  Read-only — does not mutate running state."
   :read-only t)
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-session--tool-list)
   :id "session-list"
   :server-id anvil-session--server-id
   :description
   "Return a JSON array of descriptor plists for every live snapshot
(name + created-at + branch + task-summary-head, head capped at 80
chars).  Use session-resume for the full payload."
   :read-only t)
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-session--tool-delete)
   :id "session-delete"
   :server-id anvil-session--server-id
   :description
   "Purge the snapshot stored under NAME.  Returns {deleted: bool,
name}.  Write tool."))

(defun anvil-session--unregister-tools ()
  "Remove every session-* MCP tool from the shared server."
  (dolist (id '("session-snapshot" "session-resume"
                "session-list"     "session-delete"))
    (ignore-errors
      (anvil-server-unregister-tool id anvil-session--server-id))))

;;;###autoload
(defun anvil-session-enable ()
  "Register session-* MCP tools and open the anvil-state backing store."
  (interactive)
  (anvil-state-enable)
  (anvil-session--register-tools))

(defun anvil-session-disable ()
  "Unregister session-* MCP tools."
  (interactive)
  (anvil-session--unregister-tools))


(provide 'anvil-session)
;;; anvil-session.el ends here
