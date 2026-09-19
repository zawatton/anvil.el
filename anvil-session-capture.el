;;; anvil-session-capture.el --- Structured hook capture for anvil  -*- lexical-binding: t; -*-

;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 63 Phase 2 — turn a Claude Code hook payload into structured
;; event rows.
;;
;; What this fixes.  The shipped PostToolUse wiring is
;;
;;     scripts/anvil-hook post-tool-use $CLAUDE_SESSION_ID $CLAUDE_TOOL_NAME
;;
;; so the only thing that reaches Emacs is the tool's *name*.  Which
;; file was edited, which command ran, whether it failed — none of it
;; is captured, which is why `anvil-session-events-search' can answer
;; "did I use Bash?" but not "when did I touch anvil-worker.el?".
;;
;; Claude Code already sends the full payload on the hook's stdin as
;; JSON (session_id, cwd, tool_name, tool_input, tool_response).  The
;; shipped script reads argv and ignores stdin.  This module consumes
;; that JSON instead and classifies it.
;;
;; Classification is a pure function, `anvil-session-capture-classify',
;; so the taxonomy can be tested without a hook, a daemon, or a
;; database.  It returns a plist ready for `anvil-session-store-put',
;; or nil for payloads not worth a row.
;;
;; The taxonomy is deliberately smaller than context-mode's 38 event
;; types.  Every kind below is one anvil's hooks can actually observe
;; today; a kind nothing emits is worse than a missing kind, because
;; it produces a snapshot section that is always empty.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'anvil-session-store)

(defgroup anvil-session-capture nil
  "Structured capture of Claude Code hook payloads."
  :group 'anvil
  :prefix "anvil-session-capture-")

(defcustom anvil-session-capture-enabled t
  "When non-nil, hook payloads are classified and written to the store."
  :type 'boolean
  :group 'anvil-session-capture)

(defcustom anvil-session-capture-command-max-chars 160
  "Characters of a shell command kept as the event's `data' field."
  :type 'integer
  :group 'anvil-session-capture)

;;;; --- payload accessors ---------------------------------------------------

(defun anvil-session-capture--get (obj key)
  "Return KEY from OBJ, which may be an alist or a hash table."
  (cond
   ((hash-table-p obj) (gethash key obj))
   ((and (listp obj) obj)
    (cdr (or (assoc key obj)
             (assq (intern key) obj))))
   (t nil)))

(defun anvil-session-capture--string (v)
  "Coerce V to a non-empty string, or nil."
  (cond
   ((and (stringp v) (not (string-empty-p v))) v)
   ((null v) nil)
   ((stringp v) nil)
   (t (let ((s (format "%s" v))) (and (not (string-empty-p s)) s)))))

(defun anvil-session-capture-parse (json-string)
  "Parse JSON-STRING into an alist, or nil when it is not an object."
  (condition-case nil
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'string))
        (json-read-from-string json-string))
    (error nil)))

;;;; --- classification ------------------------------------------------------

(defconst anvil-session-capture--git-subcommands
  '("commit" "push" "pull" "merge" "rebase" "checkout" "switch"
    "cherry-pick" "revert" "reset" "tag" "worktree" "stash")
  "Git subcommands that earn a `git' row rather than a generic one.

Read-only git (`status', `log', `diff') is left out on purpose: it
is the most frequent command in a session and carries no state
change, so indexing it would bury the commits under noise.")

(defun anvil-session-capture--segments (command)
  "Split COMMAND into the individual commands a shell would run.

Splitting on `;', `&&', `||', `|' and newlines is deliberately
approximate — it does not parse quoting — but it is enough to tell
\"this segment invokes git\" from \"this segment mentions git\".

Everything from the first heredoc operator onward is dropped: its
body is data, and the lines inside it look exactly like commands
once you split on newlines.  Erring toward \"not a command\" is the
right direction here, because the categories this feeds are P1."
  (let ((head (if (string-match "<<" command)
                  (substring command 0 (match-beginning 0))
                command)))
    (split-string head "\\(?:&&\\|||\\||\\|;\\|\n\\)" t)))

(defun anvil-session-capture--git-subcommand (command)
  "Return the state-changing git subcommand COMMAND runs, or nil.

Only a segment that *starts* with git counts.  Scanning the whole
string for `git <word>' anywhere files every command that merely
mentions one as a git operation: an echo of a JSON payload
containing \"git commit\", a grep for a git invocation, a heredoc
quoting one.  Observed on live data the day this shipped — an
`echo ... | anvil-capture-hook' round-trip was recorded as a P1
git row.  P1 is the tier that survives the snapshot budget, so a
false positive there costs a real commit its place."
  (when (stringp command)
    (catch 'hit
      (dolist (seg (anvil-session-capture--segments command))
        (let ((sub (anvil-session-capture--segment-git-subcommand seg)))
          (when (member sub anvil-session-capture--git-subcommands)
            (throw 'hit sub))))
      nil)))

(defconst anvil-session-capture--git-flags-with-value
  '("-C" "-c" "--git-dir" "--work-tree" "--namespace" "--exec-path")
  "Pre-subcommand git flags that consume the token after them.

Enumerated rather than guessed: a rule like \"a flag may take the
next token\" is ambiguous, and it swallows the subcommand itself in
`git --no-pager merge develop'.")

(defun anvil-session-capture--segment-git-subcommand (seg)
  "Return the git subcommand SEG invokes, or nil if it does not run git."
  (let ((tokens (split-string (string-trim seg) "[ \t]+" t)))
    ;; Skip `sudo' and any VAR=value prefixes; they leave git as the
    ;; command actually being run.
    (while (and tokens
                (or (equal (car tokens) "sudo")
                    (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" (car tokens))))
      (setq tokens (cdr tokens)))
    (when (and tokens
               (equal (file-name-nondirectory (car tokens)) "git"))
      (setq tokens (cdr tokens))
      ;; Walk past pre-subcommand flags to the first bare word.
      (while (and tokens (string-prefix-p "-" (car tokens)))
        (let ((flag (car tokens)))
          (setq tokens (cdr tokens))
          (when (and tokens (member flag
                                    anvil-session-capture--git-flags-with-value))
            (setq tokens (cdr tokens)))))
      (car tokens))))

(defun anvil-session-capture--response-error (response)
  "Return an error string from RESPONSE, or nil when it looks fine."
  (let* ((s (cond ((stringp response) response)
                  ((null response) nil)
                  (t (anvil-session-capture--get response "stderr"))))
         (interrupted (and (not (stringp response))
                           (anvil-session-capture--get response "interrupted")))
         (is-error (and (not (stringp response))
                        (anvil-session-capture--get response "is_error"))))
    (cond
     ((and is-error (not (eq is-error :json-false)))
      (or (anvil-session-capture--string s) "tool reported is_error"))
     ((and interrupted (not (eq interrupted :json-false)))
      "tool call interrupted")
     ((and (anvil-session-capture--string s)
           (string-match-p "\\(?:error\\|fatal\\|traceback\\|exception\\)"
                           (downcase s)))
      (anvil-session-capture--string s))
     (t nil))))

(defun anvil-session-capture--clamp (s n)
  "Return the first N characters of S, or nil when S is nil."
  (let ((s (anvil-session-capture--string s)))
    (and s (if (> (length s) n) (substring s 0 n) s))))

;;;###autoload
(defun anvil-session-capture-classify (payload)
  "Classify a Claude Code hook PAYLOAD into an event plist, or nil.

PAYLOAD is the parsed hook JSON (alist or hash table).  The return
value is a plist with :kind :category :priority :tool :data
:summary, suitable for `anvil-session-store-put'.

Returns nil when the payload carries nothing worth a row — an
unrecognised event with no tool name, or a tool whose input has no
identifying field.  A nil return is the normal outcome for noise,
not an error."
  (let* ((event (or (anvil-session-capture--string
                     (anvil-session-capture--get payload "hook_event_name"))
                    "PostToolUse"))
         (tool (anvil-session-capture--string
                (anvil-session-capture--get payload "tool_name")))
         (input (anvil-session-capture--get payload "tool_input"))
         (response (anvil-session-capture--get payload "tool_response"))
         (err (anvil-session-capture--response-error response)))
    (cond
     ;; A failing tool call is a P1 error row regardless of which tool
     ;; it was: "what went wrong last time" is the question a resuming
     ;; session asks first.
     (err
      (list :kind "error_tool" :category 'error :priority 1 :tool tool
            :data (or (anvil-session-capture--clamp
                       (anvil-session-capture--get input "file_path") 500)
                      (anvil-session-capture--clamp
                       (anvil-session-capture--get input "command")
                       anvil-session-capture-command-max-chars)
                      tool)
            :summary (anvil-session-capture--clamp err 200)))

     ((string= event "UserPromptSubmit")
      (let ((prompt (anvil-session-capture--string
                     (anvil-session-capture--get payload "prompt"))))
        (when prompt
          (list :kind "user-prompt" :category 'prompt :priority 2
                :tool nil :data nil
                :summary (anvil-session-capture--clamp prompt 200)))))

     ((null tool) nil)

     ((member tool '("Edit" "Write" "NotebookEdit"))
      (let ((path (anvil-session-capture--string
                   (anvil-session-capture--get input "file_path"))))
        (when path
          (list :kind (if (string= tool "Write") "file_write" "file_edit")
                :category 'file :priority 1 :tool tool :data path
                :summary (format "%s %s" (downcase tool)
                                 (file-name-nondirectory path))))))

     ((string= tool "Read")
      (let ((path (anvil-session-capture--string
                   (anvil-session-capture--get input "file_path"))))
        (when path
          ;; A CLAUDE.md / AGENTS.md read is the session picking up a
          ;; project rule, which outranks an ordinary file read.
          (let ((rule-p (string-match-p
                         "\\(?:CLAUDE\\|AGENTS\\|GEMINI\\)\\.md\\'" path)))
            (list :kind (if rule-p "rule" "file_read")
                  :category (if rule-p 'rule 'file)
                  :priority (if rule-p 1 3)
                  :tool tool :data path
                  :summary (format "read %s"
                                   (file-name-nondirectory path)))))))

     ((member tool '("Grep" "Glob"))
      (let ((pat (anvil-session-capture--string
                  (or (anvil-session-capture--get input "pattern")
                      (anvil-session-capture--get input "query")))))
        (when pat
          (list :kind "file_search" :category 'file :priority 3
                :tool tool :data (anvil-session-capture--clamp pat 200)
                :summary (format "%s %s" (downcase tool) pat)))))

     ((string= tool "Bash")
      (let* ((command (anvil-session-capture--string
                       (anvil-session-capture--get input "command")))
             (sub (anvil-session-capture--git-subcommand command)))
        (when command
          (list :kind (if sub "git" "bash_outcome")
                :category (if sub 'git 'tool-use)
                :priority (if sub 1 3)
                :tool tool
                :data (anvil-session-capture--clamp
                       command anvil-session-capture-command-max-chars)
                :summary (or (anvil-session-capture--string
                              (anvil-session-capture--get input "description"))
                             (anvil-session-capture--clamp command 200))))))

     ((member tool '("WebFetch" "WebSearch"))
      (let ((ref (anvil-session-capture--string
                  (or (anvil-session-capture--get input "url")
                      (anvil-session-capture--get input "query")))))
        (when ref
          (list :kind "external_ref" :category 'other :priority 3
                :tool tool :data (anvil-session-capture--clamp ref 500)
                :summary (format "fetched %s" ref)))))

     ((member tool '("Task" "Agent"))
      (let ((desc (anvil-session-capture--string
                   (or (anvil-session-capture--get input "description")
                       (anvil-session-capture--get input "prompt")))))
        (when desc
          (list :kind "subagent" :category 'task :priority 2
                :tool tool :data (anvil-session-capture--clamp desc 200)
                :summary (anvil-session-capture--clamp desc 200)))))

     ((string= tool "Skill")
      (let ((name (anvil-session-capture--string
                   (anvil-session-capture--get input "skill"))))
        (when name
          (list :kind "skill" :category 'skill :priority 2
                :tool tool :data name
                :summary (format "invoked skill %s" name)))))

     ((string-prefix-p "mcp__" tool)
      (list :kind "mcp_tool_call" :category 'mcp :priority 3
            :tool tool :data tool
            :summary (format "mcp call %s" tool)))

     (t
      (list :kind "tool-use" :category 'tool-use :priority 3
            :tool tool :data tool :summary (format "used %s" tool))))))

;;;; --- hook entry point ----------------------------------------------------

;;;###autoload
(defun anvil-session-capture-record (session-id json-string)
  "Classify JSON-STRING and record it against SESSION-ID.

Returns the stored row plist, or nil when the payload carried
nothing worth a row or capture is disabled.  Never signals: a hook
that fails is a hook Claude Code reports as an error on every tool
call, which is worse than a missing event."
  (when (and anvil-session-capture-enabled
             (stringp session-id) (not (string-empty-p session-id)))
    (condition-case err
        (let* ((payload (anvil-session-capture-parse json-string))
               (ev (and payload (anvil-session-capture-classify payload))))
          (when ev
            (anvil-session-store-put
             session-id (plist-get ev :kind)
             :category (plist-get ev :category)
             :priority (plist-get ev :priority)
             :tool (plist-get ev :tool)
             :data (plist-get ev :data)
             :summary (plist-get ev :summary))))
      (error
       (message "anvil-session-capture: skipped event: %s"
                (error-message-string err))
       nil))))

;;;###autoload
(defun anvil-session-capture-record-file (session-id path)
  "Read the hook payload from PATH and record it against SESSION-ID.

The hook wrapper spools stdin to a file rather than passing the
JSON as an argument: a tool_input can carry an arbitrarily large
`content' field, and an argv-sized payload would be truncated by
the OS long before Emacs saw it.  Returns the stored row or nil."
  (when (and (stringp path) (file-readable-p path))
    (anvil-session-capture-record
     session-id
     (with-temp-buffer
       (let ((coding-system-for-read 'utf-8))
         (insert-file-contents path))
       (buffer-string)))))

(provide 'anvil-session-capture)
;;; anvil-session-capture.el ends here
