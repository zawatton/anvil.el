;;; anvil-dashboard.el --- Local HTML dashboard for parallel AI workers -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A local, self-contained HTML dashboard for a user who runs many AI
;; workers in parallel (Claude Code sessions + subagents, Codex `codex
;; exec' runs, anvil orchestrator tasks) and wants one page to see what
;; is running, what finished, what is stuck, and project health.
;;
;; Public Elisp API:
;;   (anvil-dashboard-serve-start &key host port) -> (:host :port :process)
;;   (anvil-dashboard-serve-stop)                  -> port-or-nil
;;   (anvil-dashboard-snapshot)                    -> snapshot alist
;;
;; MCP tools (emacs-eval server):
;;   dashboard-serve-start  — start the HTTP server (loopback only)
;;   dashboard-serve-stop   — stop it (idempotent)
;;   dashboard-snapshot     — return the JSON snapshot without serving
;;
;; Routes:
;;   GET /              — the dashboard page (inline CSS/JS, no CDN)
;;   GET /api/snapshot   — the JSON snapshot below
;;
;; Snapshot schema (stable; extend by adding keys, not by renaming):
;;   { "generated_at": "<ISO-8601 UTC>",
;;     "panels": {
;;       "stats":    {ok,error,items:{
;;                      active_claude_sessions, running_subagents,
;;                      running_codex, stale_workers, failed_24h,
;;                      nelisp_gates_red, worklog_today,
;;                      emacsclient_hung }},
;;       "workers":  {ok,error,items:[{kind,model,name,workdir,
;;                      status,started,last_activity,
;;                      last_activity_seconds,message}, ...]},
;;                    kind    ∈ "Claude session" | "Claude subagent" |
;;                                "Codex exec" | "Codex interactive" |
;;                                "Orchestrator task"
;;                    status  ∈ "running" | "done" | "failed" |
;;                                "stale" | "stopped"
;;                    Rows are ranked (running, then stale/failed,
;;                    then done/stopped; most-recent activity first
;;                    within a tier) and capped per `kind' — see
;;                    `anvil-dashboard--rank-and-cap-workers' — so one
;;                    noisy kind cannot starve the others out.
;;       "progress": {ok,error,items:[{project,title,percent,
;;                      criteria_defined,counts,
;;                      stages:[{id,title,percent,criteria_defined},...],
;;                      failing_criteria:[{id,statement}, ...],
;;                      generated_at,generated_at_age}, ...]},
;;                    Mirrors bin/progress-meter.sh --json's schema
;;                    (overall.percent/criteria_defined/counts, stage
;;                    and criterion sub-objects); deduped by the
;;                    ledger's own source-org path via `file-truename'
;;                    so a symlinked Notes-root alias cannot double it.
;;       "nelisp":   {ok,error,items:{gates:[{name,status,age}, ...],
;;                      status:{branch,head,modified,untracked,
;;                                other_worktrees} | null}},
;;       "worklog":  {ok,error,items:[{date,title,machine}, ...]},
;;       "handoff":  {ok,error,items:[{name,modified}, ...]},
;;       "daemon":   {ok,error,items:{count,oldest_age_seconds,red,
;;                      processes:[{pid,age_seconds,cmd}, ...]}} } }
;;
;; Every panel is independently wrapped in `anvil-dashboard--panel' so
;; one broken collector reports {ok:false,error:"..."} without taking
;; down the rest of the snapshot.  Unknown/missing JSON fields in
;; collector inputs (gate files, dispatch meta.json, progress ledgers)
;; are tolerated; a field that can't be found is simply omitted/nil.
;;
;; Portability: this module must run on both GNU Emacs and the
;; NeLisp-backed anvil-standalone runtime.  It sticks to primitives
;; already used by anvil-memory.el's server (make-network-process,
;; directory-files, file-attributes, insert-file-contents with
;; BEG/END, call-process, json-encode/json-read-from-string) and
;; guards anything else (`with-timeout') with `fboundp'.
;;
;; No paths are hard-coded: the Notes workspace root comes from
;; `anvil-dashboard-notes-root' (env ANVIL_NOTES_DIR), and every other
;; root is derived from $HOME or an env var.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'anvil-server)

;; Soft dependency: when `anvil-worklog' is loaded, `anvil-worklog-list'
;; feeds the "Recent worklog" panel.  `declare-function' only silences
;; the byte-compiler; the actual call sites still guard with `fboundp'
;; so this module loads and works without anvil-worklog present.
(declare-function anvil-worklog-list "anvil-worklog" (&rest args))

;; Soft dependency: when `anvil-orchestrator' is loaded it defines this
;; hash table (id -> task plist).  `defvar' with no value form only
;; declares it special without touching an existing binding, so we can
;; read it (guarded by `boundp'/`hash-table-p') without requiring the
;; 217 KB orchestrator module just to look at one variable.
(defvar anvil-orchestrator--tasks)

;;;; --- group + defcustoms -------------------------------------------------

(defgroup anvil-dashboard nil
  "Local HTML dashboard for parallel AI workers."
  :group 'anvil
  :prefix "anvil-dashboard-")

(defconst anvil-dashboard--server-id "emacs-eval"
  "Server id under which dashboard-* MCP tools register.")

(defcustom anvil-dashboard-host "127.0.0.1"
  "Loopback bind address for the dashboard HTTP server.
Intentionally not user-configurable to anything but loopback;
`anvil-dashboard-serve-start' signals an error for any other value."
  :type 'string
  :group 'anvil-dashboard)

(defcustom anvil-dashboard-port 8731
  "Default TCP port for the dashboard HTTP server.
An integer, or `t' to let the kernel assign an ephemeral port."
  :type '(choice integer (const t))
  :group 'anvil-dashboard)

(defcustom anvil-dashboard-notes-root
  (or (getenv "ANVIL_NOTES_DIR")
      ;; A long-running Emacs daemon's environment can predate (and thus
      ;; lack) ANVIL_NOTES_DIR even though an interactive shell has it.
      ;; Fall back to ~/Cowork/Notes, but only when that directory
      ;; genuinely exists -- never assume it, and never hard-code a
      ;; /home/<user> path.
      (let ((cowork (expand-file-name "Cowork/Notes" "~")))
        (and (file-directory-p cowork) cowork)))
  "Root of the Notes workspace, or nil when unconfigured.
Feeds the NeLisp health, worklog, handoff, and project-progress
panels, and the codex-dispatch.sh script lookup; each degrades to
\"unconfigured\"/files-fallback when this is nil.  Prefer setting the
ANVIL_NOTES_DIR environment variable; see the default value form for
the ~/Cowork/Notes fallback used when it is absent."
  :type '(choice (const nil) directory)
  :group 'anvil-dashboard)

;;;; --- internal state ------------------------------------------------------

(defvar anvil-dashboard--proc nil
  "Network process for the dashboard HTTP server, or nil when stopped.")

(defvar anvil-dashboard--port nil
  "Port the dashboard server is currently bound to, or nil when stopped.")

(defvar anvil-dashboard--cache nil
  "Last computed snapshot alist, reused within `anvil-dashboard--cache-ttl'.")

(defvar anvil-dashboard--cache-time 0
  "`float-time' at which `anvil-dashboard--cache' was computed.")

(defconst anvil-dashboard--cache-ttl 5
  "Seconds a snapshot is reused before recomputation (per spec).")

(defconst anvil-dashboard--cap 80
  "Per-source and per-panel item cap, so one noisy source cannot
blow up snapshot size or latency.")

(defconst anvil-dashboard--max-file-bytes 65536
  "Read at most this many trailing bytes of any file a collector reads.")

;;;; --- small generic helpers ------------------------------------------------

(defun anvil-dashboard--home (&rest parts)
  "Join PARTS onto $HOME (via `expand-file-name \"~/\"', not getenv)."
  (expand-file-name (mapconcat #'identity parts "/") (expand-file-name "~/")))

(defun anvil-dashboard--take (list n)
  "Return the first N elements of LIST, or all of it if shorter."
  (if (or (null list) (<= n 0)) nil
    (cl-subseq list 0 (min n (length list)))))

(defun anvil-dashboard--vec (list)
  "Coerce LIST to a vector so `json-encode' always emits a JSON array
\(an elisp nil list would otherwise encode as JSON `null')."
  (vconcat list))

(defun anvil-dashboard--dir-names (dir)
  "Like `anvil-dashboard--dir-entries' but never stats entries.
Callers that only need to match against a name prefix/suffix (e.g.
scanning /tmp or ~/.cache/tmp for a handful of specially-named dirs
among many unrelated ones) should filter names first with this and
`file-directory-p'/`file-exists-p' only the survivors, instead of
paying a stat() for every sibling via `anvil-dashboard--dir-entries'."
  (condition-case nil
      (when (and (stringp dir) (file-directory-p dir))
        (directory-files dir t "\\`[^.]" t))
    (error nil)))

(defun anvil-dashboard--dir-entries (dir &optional dirs-only)
  "Return DIR's entries (absolute paths), skipping dotfiles.
Never signals; a missing/unreadable DIR yields nil.  With DIRS-ONLY,
only sub-directories are kept."
  (condition-case nil
      (when (and (stringp dir) (file-directory-p dir))
        (let (out)
          (dolist (f (directory-files dir t "\\`[^.]" t))
            (when (or (not dirs-only) (file-directory-p f))
              (push f out)))
          (nreverse out)))
    (error nil)))

(defun anvil-dashboard--file-size (file)
  (nth 7 (file-attributes file)))

(defun anvil-dashboard--mtime (file)
  "Return FILE's modification time as a float epoch, or nil."
  (let ((attrs (ignore-errors (file-attributes file))))
    (and attrs (float-time (nth 5 attrs)))))

(defun anvil-dashboard--read-tail (file)
  "Read at most the last `anvil-dashboard--max-file-bytes' of FILE."
  (condition-case nil
      (when (and (stringp file) (file-readable-p file))
        (let* ((size (or (anvil-dashboard--file-size file) 0))
               (start (max 0 (- size anvil-dashboard--max-file-bytes))))
          (with-temp-buffer
            (insert-file-contents file nil start size)
            (buffer-string))))
    (error nil)))

(defun anvil-dashboard--json (file)
  "Parse FILE's tail (see `anvil-dashboard--read-tail') as JSON.
Returns nil (never signals) when the file is missing, unreadable, or
not valid JSON — callers treat that as \"no data\", not an error."
  (let ((text (anvil-dashboard--read-tail file)))
    (and text (ignore-errors (json-read-from-string text)))))

(defun anvil-dashboard--age (epoch)
  "Render EPOCH (float `float-time' seconds, or nil) as a relative age."
  (if (not (numberp epoch)) "unknown"
    (let ((s (max 0 (round (- (float-time) epoch)))))
      (cond
       ((< s 60) (format "%ds ago" s))
       ((< s 3600) (format "%dm ago" (/ s 60)))
       ((< s 86400) (format "%dh ago" (/ s 3600)))
       (t (format "%dd ago" (/ s 86400)))))))

(defun anvil-dashboard--elapsed-seconds (epoch)
  "Seconds elapsed since EPOCH (float `float-time'), or nil when EPOCH
is not a number.  Companion to `anvil-dashboard--age' -- the raw
number backing that human string, carried in the schema too (as
`last_activity_seconds') so ranking/filtering never has to re-parse
an \"Ns ago\" display string."
  (and (numberp epoch) (max 0 (round (- (float-time) epoch)))))

(defun anvil-dashboard--iso-now ()
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

(defun anvil-dashboard--tail-slice (text &optional n)
  "Flatten TEXT to one line and keep at most the last N (default 200)
characters, so a single worker message never balloons the snapshot."
  (if (not (stringp text)) ""
    (let* ((flat (replace-regexp-in-string "[\n\r]+" " " (string-trim text)))
           (len (or n 200)))
      (if (> (length flat) len) (substring flat (- len)) flat))))

(defun anvil-dashboard--shorten (path)
  "Return a short display form of PATH: abbreviate $HOME to `~', and
keep only the last two components when still long.  Never signals."
  (if (not (stringp path)) ""
    (let* ((home (expand-file-name "~/"))
           (abbr (if (string-prefix-p home path)
                     (concat "~/" (substring path (length home)))
                   path)))
      (if (> (length abbr) 42)
          (let ((parts (split-string abbr "/" t)))
            (if (> (length parts) 2)
                (concat ".../" (mapconcat #'identity (last parts 2) "/"))
              abbr))
        abbr))))

(defun anvil-dashboard--pid-alive-p (pid)
  "Best-effort liveness check for PID.  Returns t when it cannot be
verified (no /proc, e.g. non-Linux) so callers do not over-classify
an unverifiable PID as dead."
  (let ((n (and (numberp pid) (round pid))))
    (cond
     ((not n) nil)
     ((file-exists-p "/proc") (file-directory-p (format "/proc/%d" n)))
     (t t))))

(defconst anvil-dashboard--stall-phrases
  '("standing by" "wait" "pick this back up" "hold here")
  "Phrases that mark a task transcript as stalled.
Mirrors bin/claude-agent-stall-check.sh's SIGNATURES list exactly, for
parity with that detector (including its accepted false-positive rate
on the broad word \"wait\").")

(defun anvil-dashboard--stall-p (text)
  "Non-nil when TEXT's last assistant turn ends on a stall phrase."
  (when (stringp text)
    (let ((lower (downcase text)))
      (cl-some (lambda (p) (string-match-p (regexp-quote p) lower))
               anvil-dashboard--stall-phrases))))

(defun anvil-dashboard--last-assistant-text (tail)
  "Scan TAIL (raw .jsonl bytes) backward for the last assistant turn's
text.  Returns nil when TAIL has no assistant turn (e.g. the last
turn was tool-use-only) — callers must not fall back to an earlier
turn, matching the reference detector's behavior."
  (when (stringp tail)
    (catch 'found
      (dolist (line (nreverse (split-string tail "\n" t)))
        (let ((line (string-trim line)))
          (when (> (length line) 0)
            (let ((entry (ignore-errors (json-read-from-string line))))
              (when (and (listp entry) (equal (cdr (assq 'type entry)) "assistant"))
                (let* ((msg (cdr (assq 'message entry)))
                       (content (cdr (assq 'content msg))))
                  (throw 'found
                         (cond
                          ((stringp content) content)
                          ((sequencep content)
                           (mapconcat
                            (lambda (b)
                              (or (and (listp b) (equal (cdr (assq 'type b)) "text")
                                       (cdr (assq 'text b)))
                                  ""))
                            content ""))
                          (t "")))))))))
      nil)))

;;;; --- panel wrapper --------------------------------------------------------

(defun anvil-dashboard--panel (fn &optional empty)
  "Call FN and wrap its result as a `{ok,error,items}' snapshot panel.
On error, ITEMS becomes EMPTY (default an empty vector) so one broken
collector never breaks the rest of the snapshot or removes a key the
HTML template expects."
  (condition-case err
      `((ok . t) (error . nil) (items . ,(funcall fn)))
    (error
     `((ok . :json-false)
       (error . ,(error-message-string err))
       (items . ,(or empty []))))))

;;;; --- collector: Claude Code sessions --------------------------------------

(defun anvil-dashboard--claude-sessions ()
  "One row per ~/.claude/projects/*/*.jsonl file (a Claude Code session)."
  (let (rows)
    (dolist (proj (anvil-dashboard--take
                   (anvil-dashboard--dir-entries
                    (anvil-dashboard--home ".claude" "projects") t)
                   anvil-dashboard--cap))
      (dolist (f (anvil-dashboard--take
                  (condition-case nil (directory-files proj t "\\.jsonl\\'" t)
                    (error nil))
                  anvil-dashboard--cap))
        (let* ((mt (anvil-dashboard--mtime f))
               (age (and mt (- (float-time) mt)))
               (tail (anvil-dashboard--read-tail f))
               (last-text (anvil-dashboard--last-assistant-text tail))
               (status (cond
                        ((anvil-dashboard--stall-p last-text) "stale")
                        ((and age (< age 600)) "running")
                        (t "stopped"))))
          (push `((kind . "Claude session")
                  (model . "")
                  (name . ,(file-name-base f))
                  (workdir . ,(anvil-dashboard--shorten (file-name-nondirectory proj)))
                  (status . ,status)
                  (started . nil)
                  (last_activity . ,(anvil-dashboard--age mt))
                  (last_activity_seconds . ,(anvil-dashboard--elapsed-seconds mt))
                  (message . ,(anvil-dashboard--tail-slice (or last-text tail))))
                rows))))
    (anvil-dashboard--take (nreverse rows) anvil-dashboard--cap)))

;;;; --- collector: Claude Code subagents -------------------------------------

(defun anvil-dashboard--claude-task-roots ()
  "Candidate TASK_ROOT dirs, per bin/claude-agent-stall-check.sh:
Claude Code's per-uid scratch root(s) named `claude-*' under /tmp and
under ~/.cache/tmp."
  (cl-remove-if-not
   #'file-directory-p
   (cl-remove-if-not
    (lambda (d) (string-prefix-p "claude-" (file-name-nondirectory
                                             (directory-file-name d))))
    (append (anvil-dashboard--dir-names "/tmp")
            (anvil-dashboard--dir-names (anvil-dashboard--home ".cache" "tmp"))))))

(defun anvil-dashboard--subagent-row (f)
  "Build a worker row for one TASK_ROOT/.../tasks/*.output entry F."
  (let* ((target (and (file-symlink-p f) (file-truename f)))
         (transcript (and target (string-suffix-p ".jsonl" target)
                           (file-exists-p target) target))
         (mt (anvil-dashboard--mtime (or transcript f)))
         (age (and mt (- (float-time) mt)))
         (text (and transcript
                    (anvil-dashboard--last-assistant-text
                     (anvil-dashboard--read-tail transcript))))
         (status (cond
                  ((anvil-dashboard--stall-p text) "stale")
                  ((and age (< age 300)) "running")
                  (t "done"))))
    `((kind . "Claude subagent")
      (model . "")
      (name . ,(file-name-base f))
      (workdir . "")
      (status . ,status)
      (started . nil)
      (last_activity . ,(anvil-dashboard--age mt))
      (last_activity_seconds . ,(anvil-dashboard--elapsed-seconds mt))
      (message . ,(anvil-dashboard--tail-slice (or text ""))))))

(defun anvil-dashboard--claude-subagents ()
  "One row per subagent task-output file under any Claude task root.
Layout is fixed (TASK_ROOT/<project>/<session>/tasks/*.output) so
this never needs an unbounded recursive walk."
  (let (rows)
    (dolist (root (anvil-dashboard--take (anvil-dashboard--claude-task-roots) 5))
      (dolist (proj (anvil-dashboard--take (anvil-dashboard--dir-entries root t) 40))
        (dolist (sess (anvil-dashboard--take (anvil-dashboard--dir-entries proj t) 40))
          (let ((tasks-dir (expand-file-name "tasks" sess)))
            (when (file-directory-p tasks-dir)
              (dolist (f (anvil-dashboard--take
                          (condition-case nil
                              (directory-files tasks-dir t "\\.output\\'" t)
                            (error nil))
                          anvil-dashboard--cap))
                (push (anvil-dashboard--subagent-row f) rows)))))))
    (anvil-dashboard--take (nreverse rows) anvil-dashboard--cap)))

;;;; --- collector: Codex interactive/exec sessions ---------------------------

(defun anvil-dashboard--codex-session-row (f)
  (let* ((head (condition-case nil
                   (with-temp-buffer (insert-file-contents f nil 0 4096) (buffer-string))
                 (error nil)))
         (first-line (and head (car (split-string head "\n" t))))
         (meta (and first-line (ignore-errors (json-read-from-string first-line))))
         (payload (and (listp meta) (cdr (assq 'payload meta))))
         (cwd (and (listp payload) (cdr (assq 'cwd payload))))
         (model (or (and (listp payload) (cdr (assq 'model payload)))
                    (and (listp payload) (cdr (assq 'model_provider payload)))
                    ""))
         (originator (and (listp payload) (cdr (assq 'originator payload))))
         (kind (if (equal originator "codex_exec") "Codex exec" "Codex interactive"))
         (tail (anvil-dashboard--read-tail f))
         (mt (anvil-dashboard--mtime f))
         (age (and mt (- (float-time) mt)))
         ;; Best-effort terminal-event heuristic (no confirmed-finished
         ;; sample file was available while writing this collector);
         ;; documented as a known limitation.
         (terminal (and tail
                        (string-match-p
                         "\"type\":\"\\(turn_complete\\|shutdown\\|session_end\\)\"" tail)))
         (status (cond (terminal "done") ((and age (< age 600)) "running") (t "done"))))
    `((kind . ,kind)
      (model . ,(format "%s" model))
      (name . ,(file-name-base f))
      (workdir . ,(anvil-dashboard--shorten cwd))
      (status . ,status)
      (started . nil)
      (last_activity . ,(anvil-dashboard--age mt))
      (last_activity_seconds . ,(anvil-dashboard--elapsed-seconds mt))
      (message . ""))))

(defun anvil-dashboard--codex-sessions ()
  "One row per ~/.codex/sessions/YYYY/MM/DD/*.jsonl for today/yesterday."
  (let (rows)
    (dolist (day (list (format-time-string "%Y/%m/%d" nil)
                        (format-time-string "%Y/%m/%d"
                                             (time-subtract (current-time) (days-to-time 1)))))
      (dolist (f (anvil-dashboard--take
                  (condition-case nil
                      (directory-files (anvil-dashboard--home ".codex" "sessions" day)
                                        t "\\.jsonl\\'" t)
                    (error nil))
                  anvil-dashboard--cap))
        (push (anvil-dashboard--codex-session-row f) rows)))
    (anvil-dashboard--take (nreverse rows) anvil-dashboard--cap)))

;;;; --- collector: codex-dispatch.sh wrapper + legacy *-run dirs -------------

(defun anvil-dashboard--dispatch-script ()
  "Path to bin/codex-dispatch.sh under the Notes root, or nil."
  (and anvil-dashboard-notes-root
       (let ((p (expand-file-name "bin/codex-dispatch.sh" anvil-dashboard-notes-root)))
         (and (file-executable-p p) p))))

(defun anvil-dashboard--dispatch-runs-root ()
  (or (getenv "CODEX_RUNS_ROOT") (anvil-dashboard--home ".cache" "tmp" "codex-runs")))

(defun anvil-dashboard--call-bounded (timeout program &rest args)
  "Run PROGRAM with ARGS, capturing stdout.  Returns (EXIT . OUTPUT),
or nil when PROGRAM cannot be run at all.  TIMEOUT (seconds) is
honored via `with-timeout' when available — a best-effort guard
against a hung child, not a hard kill of the process."
  (when (and (stringp program) (file-executable-p program))
    (with-temp-buffer
      (let ((code
             (condition-case nil
                 (if (fboundp 'with-timeout)
                     (with-timeout (timeout 124)
                       (apply #'call-process program nil t nil args))
                   (apply #'call-process program nil t nil args))
               (error 124))))
        (cons code (buffer-string))))))

(defun anvil-dashboard--dispatch-classify (status pid run-dir)
  "Re-derive STATUS for a dispatch run, correcting the known wrapper
bug where meta.json can still say \"running\" after the process has
already exited.  Evidence order: a DONE file or a trailing codex.log
\"EXIT=<n>\" line settle done-vs-failed from the recorded exit code;
otherwise a dead PID with neither is a crash before anything could be
recorded (`stale'), never `running'."
  (if (not (equal status "running"))
      status
    (let* ((log (and run-dir (anvil-dashboard--read-tail
                               (expand-file-name "codex.log" run-dir))))
           (done (and run-dir (file-exists-p (expand-file-name "DONE" run-dir))))
           (exit-match (and log (string-match "EXIT=\\([0-9]+\\)" log)))
           (exit-code (and exit-match (string-to-number (match-string 1 log))))
           (alive (anvil-dashboard--pid-alive-p pid)))
      (cond
       ((or done exit-match) (if (and exit-code (zerop exit-code)) "done" "failed"))
       ((not alive) "stale")
       (t "running")))))

(defun anvil-dashboard--last-line-file (dir)
  "Tail of DIR/last.txt as a single line, or nil."
  (and dir
       (let ((f (expand-file-name "last.txt" dir)))
         (and (file-exists-p f)
              (car (last (split-string
                          (string-trim (or (anvil-dashboard--read-tail f) "")) "\n" t)))))))

(defun anvil-dashboard--dispatch-row (entry)
  "Build a worker row from one dispatch ENTRY alist — either a
`status --json' element or a meta.json file parsed directly (both
share field names, see the script's own header schema comment)."
  (let* ((run-dir (cdr (assq 'run_dir entry)))
         (pid (cdr (assq 'pid entry)))
         (status (anvil-dashboard--dispatch-classify
                  (or (cdr (assq 'status entry)) "running") pid run-dir))
         (mtime (or (and run-dir (anvil-dashboard--mtime (expand-file-name "meta.json" run-dir)))
                    (anvil-dashboard--mtime run-dir))))
    `((kind . "Codex exec")
      (model . ,(or (cdr (assq 'model entry)) ""))
      (name . ,(or (cdr (assq 'name entry)) (cdr (assq 'run entry)) ""))
      (workdir . ,(anvil-dashboard--shorten (cdr (assq 'workdir entry))))
      (status . ,status)
      (started . ,(cdr (assq 'started_at entry)))
      (last_activity . ,(anvil-dashboard--age mtime))
      (last_activity_seconds . ,(anvil-dashboard--elapsed-seconds mtime))
      (message . ,(anvil-dashboard--tail-slice
                   (or (cdr (assq 'last_line entry))
                       (anvil-dashboard--last-line-file run-dir)
                       ""))))))

(defun anvil-dashboard--dispatch-cli-rows ()
  "Rows from `codex-dispatch.sh status --json', or nil if unavailable."
  (let* ((script (anvil-dashboard--dispatch-script))
         (result (and script (anvil-dashboard--call-bounded 3 script "status" "--json"))))
    (when (and result (zerop (car result)))
      (let ((parsed (ignore-errors (json-read-from-string (cdr result)))))
        (and (sequencep parsed) (append parsed nil))))))

(defun anvil-dashboard--dispatch-rows-from-files ()
  "Fallback: read every RUNS_ROOT/*/meta.json directly.
A meta.json that fails to parse (negative control: corrupted JSON)
is skipped entirely — it must never be reported as \"running\"."
  (let (rows)
    (dolist (dir (anvil-dashboard--take
                  (anvil-dashboard--dir-entries (anvil-dashboard--dispatch-runs-root) t)
                  anvil-dashboard--cap))
      (let* ((meta-file (expand-file-name "meta.json" dir))
             (meta (and (file-exists-p meta-file) (anvil-dashboard--json meta-file))))
        (when (and (listp meta) (stringp (cdr (assq 'name meta))))
          (push (anvil-dashboard--dispatch-row (cons (cons 'run_dir dir) meta)) rows))))
    (nreverse rows)))

(defun anvil-dashboard--legacy-run-row (dir)
  "Build a worker row for one legacy ~/.cache/tmp/*-run/ DIR
\(brief.md + codex.log, optionally + DONE/last.txt — no meta.json)."
  (let* ((log (or (anvil-dashboard--read-tail (expand-file-name "codex.log" dir)) ""))
         (exit-match (string-match "EXIT=\\([0-9]+\\)" log))
         (exit-code (and exit-match (string-to-number (match-string 1 log))))
         (done (file-exists-p (expand-file-name "DONE" dir)))
         (model (and (string-match "^model: *\\(.+\\)$" log) (match-string 1 log)))
         (workdir (and (string-match "^workdir: *\\(.+\\)$" log) (match-string 1 log)))
         (mt (anvil-dashboard--mtime (expand-file-name "codex.log" dir)))
         (status (cond
                  ((or done exit-match) (if (and exit-code (zerop exit-code)) "done" "failed"))
                  ((and mt (< (- (float-time) mt) 600)) "running")
                  (t "stale"))))
    `((kind . "Codex exec")
      (model . ,(or model ""))
      (name . ,(file-name-nondirectory (directory-file-name dir)))
      (workdir . ,(anvil-dashboard--shorten workdir))
      (status . ,status)
      (started . nil)
      (last_activity . ,(anvil-dashboard--age mt))
      (last_activity_seconds . ,(anvil-dashboard--elapsed-seconds mt))
      (message . ,(anvil-dashboard--tail-slice (or (anvil-dashboard--last-line-file dir) ""))))))

(defun anvil-dashboard--legacy-run-workers ()
  "One row per ~/.cache/tmp/*-run/ dir carrying brief.md + codex.log."
  (let (rows)
    (dolist (dir (anvil-dashboard--take
                  (cl-remove-if-not (lambda (d) (string-suffix-p "-run" d))
                                     (anvil-dashboard--dir-names
                                      (anvil-dashboard--home ".cache" "tmp")))
                  200))
      (when (and (file-directory-p dir)
                 (file-exists-p (expand-file-name "brief.md" dir))
                 (file-exists-p (expand-file-name "codex.log" dir)))
        (push (anvil-dashboard--legacy-run-row dir) rows)))
    (anvil-dashboard--take (nreverse rows) anvil-dashboard--cap)))

(defun anvil-dashboard--dispatch-workers ()
  "Codex dispatch-wrapper runs: prefer the live CLI, else meta.json
files directly; legacy *-run dirs are always scanned in addition
\(they never show up in either of those)."
  (let* ((cli (ignore-errors (anvil-dashboard--dispatch-cli-rows)))
         (rows (if cli
                   (anvil-dashboard--take (mapcar #'anvil-dashboard--dispatch-row cli)
                                           anvil-dashboard--cap)
                 (ignore-errors (anvil-dashboard--dispatch-rows-from-files)))))
    (append rows (ignore-errors (anvil-dashboard--legacy-run-workers)))))

;;;; --- collector: anvil orchestrator tasks ----------------------------------

(defun anvil-dashboard--orchestrator-workers ()
  "One row per in-memory orchestrator task, when the module is loaded."
  (unless (and (boundp 'anvil-orchestrator--tasks) (hash-table-p anvil-orchestrator--tasks))
    (error "anvil-orchestrator is not loaded"))
  (let (rows)
    (maphash
     (lambda (_id task)
       (push
        `((kind . "Orchestrator task")
          (model . ,(or (plist-get task :model) ""))
          (name . ,(or (plist-get task :name) (format "%s" (plist-get task :id))))
          (workdir . ,(anvil-dashboard--shorten (plist-get task :cwd)))
          (status . ,(let ((s (plist-get task :status))) (if s (symbol-name s) "unknown")))
          (started . ,(let ((s (plist-get task :started-at)))
                        (and (numberp s) (format-time-string "%Y-%m-%dT%H:%M:%SZ" s t))))
          (last_activity . ,(anvil-dashboard--age (plist-get task :started-at)))
          (last_activity_seconds . ,(anvil-dashboard--elapsed-seconds (plist-get task :started-at)))
          (message . ,(anvil-dashboard--tail-slice
                       (format "%s" (or (plist-get task :summary) (plist-get task :error) "")))))
        rows))
     anvil-orchestrator--tasks)
    (anvil-dashboard--take (nreverse rows) anvil-dashboard--cap)))

;;;; --- combined worker table ------------------------------------------------

(defconst anvil-dashboard--per-kind-cap 30
  "Max rows kept per worker `kind' after ranking (see
`anvil-dashboard--rank-and-cap-workers').  A single global cap
applied to the concatenated list let one noisy kind (e.g. 50+ old
Claude sessions, always appended first) silently starve out every
other kind -- including Codex runs -- before the cap ever saw them.
Capping per kind guarantees every kind that has any activity shows up.")

(defun anvil-dashboard--status-rank (status)
  "Sort priority for STATUS: running first, then stale/failed, then
everything else (done/stopped)."
  (cond ((equal status "running") 0)
        ((member status '("stale" "failed")) 1)
        (t 2)))

(defun anvil-dashboard--worker-sort-key (row)
  (cons (anvil-dashboard--status-rank (cdr (assq 'status row)))
        (or (cdr (assq 'last_activity_seconds row)) most-positive-fixnum)))

(defun anvil-dashboard--worker-less-p (a b)
  "Running first, then stale/failed, then done/stopped; within a tier,
most-recent activity (smallest elapsed seconds) first."
  (let ((ka (anvil-dashboard--worker-sort-key a))
        (kb (anvil-dashboard--worker-sort-key b)))
    (or (< (car ka) (car kb))
        (and (= (car ka) (car kb)) (< (cdr ka) (cdr kb))))))

(defun anvil-dashboard--rank-and-cap-workers (rows)
  "Sort ROWS per `anvil-dashboard--worker-less-p' and keep at most
`anvil-dashboard--per-kind-cap' rows per `kind', so every kind that
has any activity is represented in the final table instead of being
starved out by a single global cap."
  (let* ((sorted (sort (copy-sequence rows) #'anvil-dashboard--worker-less-p))
         (per-kind (make-hash-table :test 'equal))
         kept)
    (dolist (row sorted)
      (let* ((kind (cdr (assq 'kind row)))
             (n (or (gethash kind per-kind) 0)))
        (when (< n anvil-dashboard--per-kind-cap)
          (puthash kind (1+ n) per-kind)
          (push row kept))))
    (sort (nreverse kept) #'anvil-dashboard--worker-less-p)))

(defun anvil-dashboard--workers ()
  "Concatenate every worker source, then rank + per-kind-cap the
result so Codex/orchestrator rows are never starved out by a large
number of old Claude sessions.  Each source is individually
`ignore-errors'-guarded so one dead source contributes zero rows
instead of taking down the whole \"workers\" panel."
  (anvil-dashboard--rank-and-cap-workers
   (append (ignore-errors (anvil-dashboard--claude-sessions))
           (ignore-errors (anvil-dashboard--claude-subagents))
           (ignore-errors (anvil-dashboard--codex-sessions))
           (ignore-errors (anvil-dashboard--dispatch-workers))
           (ignore-errors (anvil-dashboard--orchestrator-workers)))))

;;;; --- collector: NeLisp health ---------------------------------------------

(defun anvil-dashboard--nelisp ()
  (unless anvil-dashboard-notes-root
    (error "ANVIL_NOTES_DIR is not configured"))
  (let* ((root (expand-file-name "dev/nelisp" anvil-dashboard-notes-root))
         (gates-dir (expand-file-name "target/gates" root))
         (status-file (expand-file-name "target/ai/STATUS.json" root))
         gates)
    (dolist (f (anvil-dashboard--take
                (condition-case nil (directory-files gates-dir t "\\.json\\'" t) (error nil))
                anvil-dashboard--cap))
      (let* ((j (anvil-dashboard--json f))
             (gstatus (and (listp j) (cdr (assq 'status j)))))
        (push `((name . ,(file-name-base f))
                (status . ,(if (equal gstatus "pass") "pass" "fail"))
                (age . ,(anvil-dashboard--age (anvil-dashboard--mtime f))))
              gates)))
    (let* ((st (and (file-exists-p status-file) (anvil-dashboard--json status-file)))
           (repo (and st (cdr (assq 'repo st)))))
      `((gates . ,(anvil-dashboard--vec (nreverse gates)))
        (status . ,(and repo
                         `((branch . ,(cdr (assq 'branch repo)))
                           (head . ,(cdr (assq 'head repo)))
                           (modified . ,(cdr (assq 'modified_tracked repo)))
                           (untracked . ,(cdr (assq 'untracked repo)))
                           (other_worktrees . ,(cdr (assq 'other_worktrees repo))))))))))

;;;; --- collector: worklog + handoff -----------------------------------------

(defun anvil-dashboard--worklog ()
  (unless (fboundp 'anvil-worklog-list)
    (error "anvil-worklog is not loaded"))
  (anvil-dashboard--vec
   (mapcar (lambda (e) `((date . ,(plist-get e :date))
                         (title . ,(plist-get e :title))
                         (machine . ,(plist-get e :machine))))
           (anvil-worklog-list :limit 10))))

(defun anvil-dashboard--worklog-today-count ()
  "Best-effort count of today's worklog entries.  Separate from the
`worklog' panel proper so one bad query here cannot blank that panel."
  (if (not (fboundp 'anvil-worklog-list)) 0
    (let ((today (format-time-string "%Y-%m-%d")))
      (or (cl-count-if (lambda (e) (equal (plist-get e :date) today))
                        (ignore-errors (anvil-worklog-list :limit 50)))
          0))))

(defun anvil-dashboard--handoff ()
  (unless anvil-dashboard-notes-root
    (error "ANVIL_NOTES_DIR is not configured"))
  (let* ((dir (expand-file-name "handoff" anvil-dashboard-notes-root))
         (files (anvil-dashboard--dir-entries dir))
         (sorted (sort (copy-sequence files)
                       (lambda (a b) (> (or (anvil-dashboard--mtime a) 0)
                                        (or (anvil-dashboard--mtime b) 0))))))
    (anvil-dashboard--vec
     (mapcar (lambda (f) `((name . ,(file-name-nondirectory f))
                           (modified . ,(anvil-dashboard--age (anvil-dashboard--mtime f)))))
             (anvil-dashboard--take sorted 10)))))

;;;; --- collector: emacsclient daemon health ---------------------------------

(defun anvil-dashboard--daemon ()
  "Queued/hung emacsclient processes: any emacsclient whose etimes
exceeds 60 s is treated as queued on a stuck daemon (a real failure
mode seen in production: a hung main daemon left 13 queued clients)."
  (unless (executable-find "ps") (error "ps(1) not found"))
  (with-temp-buffer
    (unless (zerop (call-process "ps" nil t nil "-eo" "pid=,etimes=,args="))
      (error "ps invocation failed"))
    (let (hits (max-age 0))
      (dolist (line (split-string (buffer-string) "\n" t))
        (when (string-match "\\`[ \t]*\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)\\'" line)
          (let ((pid (string-to-number (match-string 1 line)))
                (etimes (string-to-number (match-string 2 line)))
                (args (match-string 3 line)))
            (when (and (string-match-p "emacsclient" args) (> etimes 60))
              (setq max-age (max max-age etimes))
              (push `((pid . ,pid) (age_seconds . ,etimes)
                      (cmd . ,(anvil-dashboard--tail-slice args 80)))
                    hits)))))
      `((count . ,(length hits))
        (oldest_age_seconds . ,max-age)
        (red . ,(if hits t :json-false))
        (processes . ,(anvil-dashboard--vec (anvil-dashboard--take (nreverse hits) 20)))))))

;;;; --- collector: project-progress ledgers ----------------------------------

(defun anvil-dashboard--progress-roots ()
  "Every <notes-root>/dev/*/target/progress dir plus
<notes-root>/target/progress, per bin/progress-meter.sh's layout."
  (when anvil-dashboard-notes-root
    (let ((dev-projects (anvil-dashboard--dir-entries
                          (expand-file-name "dev" anvil-dashboard-notes-root) t)))
      (cons (expand-file-name "target/progress" anvil-dashboard-notes-root)
            (mapcar (lambda (d) (expand-file-name "target/progress" d)) dev-projects)))))

(defun anvil-dashboard--progress-stage-summary (stage)
  "Summarize one bin/progress-meter.sh STAGE alist for the panel."
  (and (listp stage)
       `((id . ,(cdr (assq 'id stage)))
         (title . ,(cdr (assq 'title stage)))
         (percent . ,(cdr (assq 'percent stage)))
         (criteria_defined . ,(cdr (assq 'criteria_defined stage))))))

(defun anvil-dashboard--progress-failing-criteria (stages)
  "Collect {id,statement} for every criterion with status \"FAIL\"
across STAGES (a list of bin/progress-meter.sh stage alists)."
  (let (out)
    (dolist (stage stages)
      (dolist (c (append (and (listp stage) (cdr (assq 'criteria stage))) nil))
        (when (and (listp c) (equal (cdr (assq 'status c)) "FAIL"))
          (push `((id . ,(cdr (assq 'id c))) (statement . ,(cdr (assq 'statement c)))) out))))
    (nreverse out)))

(defun anvil-dashboard--progress-item (f)
  "Parse F as a bin/progress-meter.sh `--json' ledger.  Returns
\(DEDUPE-KEY . ROW) or nil when F is missing/unparsable, or does not
carry the real schema -- notably progress-meter.sh's own sibling
`<ledger>.cache.json' per-criterion status cache (a flat {\"S1.1\":
{status,timestamp}, ...} map with no `overall'/`ledger' key at all)
must never be surfaced as a phantom duplicate ledger.  DEDUPE-KEY is
the ledger's source-org path resolved via `file-truename', so two
progress JSON files that describe the same underlying ledger through
different path aliases (e.g. ~/Notes vs ~/Cowork/Notes, one a symlink
to the other) collapse to a single panel item."
  (condition-case nil
      (let* ((j (anvil-dashboard--json f))
             (overall (and j (cdr (assq 'overall j))))
             (ledger-path (and j (cdr (assq 'ledger j)))))
        (when (and j overall ledger-path)
          (let* ((stages (append (cdr (assq 'stages j)) nil))
                 (key (or (ignore-errors (file-truename ledger-path))
                          (file-truename f)))
                 (project (or (cdr (assq 'project j)) (file-name-base f))))
            (cons key
                  `((project . ,project)
                    (title . ,project)
                    (percent . ,(cdr (assq 'percent overall)))
                    (criteria_defined . ,(cdr (assq 'criteria_defined overall)))
                    (counts . ,(cdr (assq 'counts overall)))
                    (stages . ,(anvil-dashboard--vec
                                (delq nil (mapcar #'anvil-dashboard--progress-stage-summary stages))))
                    (failing_criteria . ,(anvil-dashboard--vec
                                          (anvil-dashboard--progress-failing-criteria stages)))
                    (generated_at . ,(cdr (assq 'generated_at j)))
                    (generated_at_age . ,(anvil-dashboard--age (anvil-dashboard--mtime f))))))))
    (error nil)))

(defun anvil-dashboard--progress ()
  "Every ledger JSON under the roots in `anvil-dashboard--progress-roots',
deduped by the underlying ledger path (see `anvil-dashboard--progress-item').
Reads files only — never invokes progress-meter.sh itself.  Returns
an empty vector (not an error) when no ledgers exist yet."
  (let (items (seen (make-hash-table :test 'equal)))
    (dolist (dir (anvil-dashboard--progress-roots))
      (dolist (f (anvil-dashboard--take
                  (condition-case nil (directory-files dir t "\\.json\\'" t) (error nil))
                  anvil-dashboard--cap))
        (let ((pair (anvil-dashboard--progress-item f)))
          (when (and pair (not (gethash (car pair) seen)))
            (puthash (car pair) t seen)
            (push (cdr pair) items)))))
    (anvil-dashboard--vec (anvil-dashboard--take (nreverse items) anvil-dashboard--cap))))

;;;; --- stat cards ------------------------------------------------------------

(defun anvil-dashboard--stats (workers-items nelisp-items worklog-today daemon-items)
  "Aggregate the 8 stat-card counters from already-computed panels.
NOTE (known limitation): `failed_24h' counts every \"failed\" row
currently in the worker snapshot rather than strictly filtering to a
24 h window — the worker sources do not all expose a raw numeric
timestamp cheaply enough to filter without extra plumbing."
  (let ((gates (cdr (assq 'gates nelisp-items))))
    `((active_claude_sessions
       . ,(cl-count-if (lambda (r) (and (equal (cdr (assq 'kind r)) "Claude session")
                                        (equal (cdr (assq 'status r)) "running")))
                        workers-items))
      (running_subagents
       . ,(cl-count-if (lambda (r) (and (equal (cdr (assq 'kind r)) "Claude subagent")
                                        (equal (cdr (assq 'status r)) "running")))
                        workers-items))
      (running_codex
       . ,(cl-count-if (lambda (r) (and (member (cdr (assq 'kind r))
                                                '("Codex exec" "Codex interactive"))
                                        (equal (cdr (assq 'status r)) "running")))
                        workers-items))
      (stale_workers
       . ,(cl-count-if (lambda (r) (equal (cdr (assq 'status r)) "stale")) workers-items))
      (failed_24h
       . ,(cl-count-if (lambda (r) (equal (cdr (assq 'status r)) "failed")) workers-items))
      (nelisp_gates_red
       . ,(cl-count-if (lambda (g) (equal (cdr (assq 'status g)) "fail")) gates))
      (worklog_today . ,worklog-today)
      (emacsclient_hung . ,(or (cdr (assq 'count daemon-items)) 0)))))

;;;; --- top-level snapshot -----------------------------------------------------

(defun anvil-dashboard-snapshot ()
  "Return the dashboard snapshot as a JSON-ready alist.
See the file header for the authoritative schema.  Cached for
`anvil-dashboard--cache-ttl' seconds."
  (if (and anvil-dashboard--cache
           (< (- (float-time) anvil-dashboard--cache-time) anvil-dashboard--cache-ttl))
      anvil-dashboard--cache
    (let* ((workers (anvil-dashboard--panel
                     (lambda () (anvil-dashboard--vec (anvil-dashboard--workers)))))
           (progress (anvil-dashboard--panel #'anvil-dashboard--progress))
           (nelisp (anvil-dashboard--panel #'anvil-dashboard--nelisp
                                            '((gates . []) (status . nil))))
           (worklog (anvil-dashboard--panel #'anvil-dashboard--worklog))
           (handoff (anvil-dashboard--panel #'anvil-dashboard--handoff))
           (daemon (anvil-dashboard--panel
                    #'anvil-dashboard--daemon
                    '((count . 0) (oldest_age_seconds . 0)
                      (red . :json-false) (processes . []))))
           (worklog-today (or (ignore-errors (anvil-dashboard--worklog-today-count)) 0))
           (stats (anvil-dashboard--panel
                   (lambda ()
                     (anvil-dashboard--stats
                      (cdr (assq 'items workers))
                      (cdr (assq 'items nelisp))
                      worklog-today
                      (cdr (assq 'items daemon))))
                   '((active_claude_sessions . 0) (running_subagents . 0)
                     (running_codex . 0) (stale_workers . 0) (failed_24h . 0)
                     (nelisp_gates_red . 0) (worklog_today . 0) (emacsclient_hung . 0))))
           (snapshot
            `((generated_at . ,(anvil-dashboard--iso-now))
              (panels . ((stats . ,stats)
                         (workers . ,workers)
                         (progress . ,progress)
                         (nelisp . ,nelisp)
                         (worklog . ,worklog)
                         (handoff . ,handoff)
                         (daemon . ,daemon))))))
      (setq anvil-dashboard--cache snapshot
            anvil-dashboard--cache-time (float-time))
      snapshot)))

;;;; --- HTML page ---------------------------------------------------------

(defconst anvil-dashboard--html
  "<!doctype html>
<html lang='en'>
<head>
<meta charset='utf-8'>
<meta name='viewport' content='width=device-width, initial-scale=1'>
<meta name='color-scheme' content='light dark'>
<title>Anvil dashboard</title>
<style>
:root{
  --bg:#f4f5f7; --card:#ffffff; --fg:#20242a; --muted:#6b7280;
  --border:#e2e4e9; --badge:#e8edf4; --red:#c0392b; --red-bg:#fdecea;
  --green:#1e8e3e; --green-bg:#dff3e3;
}
@media (prefers-color-scheme: dark){
  :root{
    --bg:#16181d; --card:#212327; --fg:#eef0f2; --muted:#9aa1ab;
    --border:#33363c; --badge:#2b2f37; --red:#ff6b6b; --red-bg:#3a2224;
    --green:#63d17d; --green-bg:#1f3324;
  }
}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--fg);
     font:15px/1.45 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}
main{max-width:1120px;margin:0 auto;padding:20px 16px 60px}
header{display:flex;align-items:center;justify-content:space-between;
       gap:12px;flex-wrap:wrap;margin-bottom:16px}
h1{font-size:20px;margin:0}
h2{font-size:15px;margin:0 0 10px}
button{border:1px solid var(--border);background:var(--card);color:var(--fg);
       border-radius:8px;padding:6px 12px;cursor:pointer;font-size:13px}
.cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));
       gap:10px;margin-bottom:16px}
.card,section{background:var(--card);border:1px solid var(--border);
             border-radius:12px;padding:14px 16px;margin-bottom:14px}
.card .n{font-size:26px;font-weight:600;margin-top:4px}
.card.red .n{color:var(--red)}
table{width:100%;border-collapse:collapse;font-size:13px}
td,th{text-align:left;padding:7px 8px;border-bottom:1px solid var(--border);
      white-space:nowrap;overflow:hidden;text-overflow:ellipsis;max-width:260px}
.tablewrap{overflow-x:auto}
.badge{padding:2px 8px;border-radius:10px;background:var(--badge);font-size:12px}
.badge.running{background:var(--green-bg);color:var(--green)}
.badge.failed{background:var(--red-bg);color:var(--red)}
.muted{color:var(--muted);font-size:12px}
.bar{height:6px;border-radius:4px;background:var(--badge);overflow:hidden;margin:3px 0}
.bar i{display:block;height:100%;background:var(--green)}
.empty{color:var(--muted);font-size:13px;padding:6px 0}
</style>
</head>
<body>
<main>
  <header>
    <h1 data-en='Anvil dashboard' data-ja='Anvil ダッシュボード'>Anvil dashboard</h1>
    <button id='lang-toggle' type='button'>EN / 日本語</button>
  </header>

  <div class='cards' id='stat-cards'></div>

  <section>
    <div style='display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:8px'>
      <h2 style='margin:0' data-en='Parallel agents' data-ja='並列エージェント'>Parallel agents</h2>
      <button id='workers-show-all' type='button'>Show all</button>
    </div>
    <p class='muted' id='workers-hidden-note'></p>
    <div class='tablewrap'>
      <table>
        <thead>
          <tr>
            <th data-en='Kind' data-ja='種別'>Kind</th>
            <th data-en='Name' data-ja='名前'>Name</th>
            <th data-en='Model' data-ja='モデル'>Model</th>
            <th data-en='Status' data-ja='状態'>Status</th>
            <th data-en='Workdir' data-ja='作業場所'>Workdir</th>
            <th data-en='Last activity' data-ja='最終活動'>Last activity</th>
            <th data-en='Message' data-ja='最新メッセージ'>Message</th>
          </tr>
        </thead>
        <tbody id='worker-rows'></tbody>
      </table>
    </div>
  </section>

  <section>
    <h2 data-en='Project progress' data-ja='プロジェクト進捗'>Project progress</h2>
    <div id='progress-list'></div>
  </section>

  <section>
    <h2 data-en='NeLisp gates' data-ja='NeLisp ゲート'>NeLisp gates</h2>
    <div id='nelisp-panel'></div>
  </section>

  <section>
    <h2 data-en='Recent worklog' data-ja='最近のワークログ'>Recent worklog</h2>
    <div id='worklog-panel'></div>
  </section>

  <section>
    <h2 data-en='Handoff notes' data-ja='ハンドオフ'>Handoff notes</h2>
    <div id='handoff-panel'></div>
  </section>

  <section>
    <h2 data-en='Daemon health' data-ja='デーモン状態'>Daemon health</h2>
    <div id='daemon-panel'></div>
  </section>

  <p class='muted' id='footer-note'></p>
</main>

<script>
(function(){
  var DICT = {
    en:{
      active:'Active Claude', subagents:'Running subagents', codex:'Running Codex',
      stale:'Stale workers', failed:'Failed (24h)', gatesred:'NeLisp gates red',
      worklog:'Worklog today', hung:'Hung emacsclient',
      unavailable:'unavailable', noitems:'No items.', noledgers:'no ledgers yet',
      criteriaof:'of', criteria:'defined criteria', failing:'failing checks',
      hiddennote:'older finished items hidden -', showall:'Show all', showactive:'Show active only'
    },
    ja:{
      active:'実行中 Claude', subagents:'実行中サブエージェント', codex:'実行中 Codex',
      stale:'停滞ワーカー', failed:'失敗(24時間)', gatesred:'NeLisp赤ゲート',
      worklog:'本日のワークログ', hung:'滞留 emacsclient',
      unavailable:'利用不可', noitems:'データがありません。', noledgers:'まだ台帳がありません',
      criteriaof:'/', criteria:'件の定義済み基準', failing:'件の失敗チェック',
      hiddennote:'件の完了済み古い項目を非表示 -', showall:'すべて表示', showactive:'アクティブのみ表示'
    }
  };
  var lang = 'en';
  try { lang = localStorage.getItem('anvil-dashboard-lang') || 'en'; } catch (e) {}

  function t(key){ return (DICT[lang] && DICT[lang][key]) || key; }

  function esc(s){
    return String(s == null ? '' : s).replace(/[&<>]/g, function(c){
      return c === '&' ? '&amp;' : (c === '<' ? '&lt;' : '&gt;');
    });
  }

  function statusClass(s){
    if (s === 'running') return 'running';
    if (s === 'stale' || s === 'failed') return 'failed';
    return '';
  }

  function applyStaticLabels(){
    document.documentElement.lang = lang;
    document.querySelectorAll('[data-en]').forEach(function(el){
      var v = el.getAttribute('data-' + lang);
      if (v) el.textContent = v;
    });
  }

  function renderCards(panel){
    var items = (panel && panel.items) || {};
    var defs = [
      ['active','active_claude_sessions'], ['subagents','running_subagents'],
      ['codex','running_codex'], ['stale','stale_workers'],
      ['failed','failed_24h'], ['gatesred','nelisp_gates_red'],
      ['worklog','worklog_today'], ['hung','emacsclient_hung']
    ];
    var redKeys = ['stale_workers','failed_24h','nelisp_gates_red','emacsclient_hung'];
    document.getElementById('stat-cards').innerHTML = defs.map(function(d){
      var val = items[d[1]];
      var isRed = redKeys.indexOf(d[1]) >= 0 && (val || 0) > 0;
      var cls = 'card' + (isRed ? ' red' : '');
      return `<div class='${cls}'>${esc(t(d[0]))}<div class='n'>${val == null ? '?' : esc(val)}</div></div>`;
    }).join('');
  }

  var showAllWorkers = false;
  var lastWorkersPanel = null;

  function isOldFinished(w){
    return (w.status === 'done' || w.status === 'stopped') &&
      w.last_activity_seconds != null && w.last_activity_seconds > 86400;
  }

  function renderWorkers(panel){
    lastWorkersPanel = panel;
    var tbody = document.getElementById('worker-rows');
    var note = document.getElementById('workers-hidden-note');
    if (!panel || panel.ok === false){
      tbody.innerHTML = `<tr><td colspan='7' class='empty'>${esc(t('unavailable'))}: ${esc(panel && panel.error)}</td></tr>`;
      note.textContent = '';
      return;
    }
    var rows = panel.items || [];
    var visible = showAllWorkers ? rows : rows.filter(function(w){ return !isOldFinished(w); });
    var hiddenCount = rows.length - visible.length;
    note.textContent = hiddenCount > 0 ? (hiddenCount + ' ' + t('hiddennote')) : '';
    if (!visible.length){
      tbody.innerHTML = `<tr><td colspan='7' class='empty'>${esc(t('noitems'))}</td></tr>`;
      return;
    }
    tbody.innerHTML = visible.map(function(w){
      return `<tr><td>${esc(w.kind)}</td><td>${esc(w.name)}</td><td>${esc(w.model)}</td>` +
        `<td><span class='badge ${statusClass(w.status)}'>${esc(w.status)}</span></td>` +
        `<td>${esc(w.workdir)}</td><td>${esc(w.last_activity)}</td><td>${esc(w.message)}</td></tr>`;
    }).join('');
  }

  function renderProgress(panel){
    var el = document.getElementById('progress-list');
    if (!panel || panel.ok === false){
      el.innerHTML = `<p class='empty'>${esc(t('unavailable'))}: ${esc(panel && panel.error)}</p>`;
      return;
    }
    var items = panel.items || [];
    if (!items.length){
      el.innerHTML = `<p class='empty'>${esc(t('noledgers'))}</p>`;
      return;
    }
    el.innerHTML = items.map(function(p){
      var stages = (p.stages || []).map(function(s){
        var pct = Math.max(0, Math.min(100, s.percent || 0));
        return `<div class='muted'>${esc(s.title || s.id)}<div class='bar'><i style='width:${pct}%'></i></div></div>`;
      }).join('');
      var overall = p.percent == null ? '?' : Math.round(p.percent) + '%';
      var criteria = p.criteria_defined == null ? ''
        : (p.percent == null ? '?' : Math.round(p.percent)) + '% ' + t('criteriaof') + ' ' + p.criteria_defined + ' ' + t('criteria');
      var failing = (p.failing_criteria || []);
      var failingList = failing.length
        ? `<ul class='muted'>` +
          failing.map(function(c){ return `<li>${esc(c.id)}: ${esc(c.statement)}</li>`; }).join('') + '</ul>'
        : '';
      return `<div class='card' style='margin-bottom:10px'><b>${esc(p.title || p.project)}</b> - ${overall}` +
        `<div class='muted'>${esc(criteria)}</div>${stages}` +
        `<div class='muted'>${failing.length} ${esc(t('failing'))} - ${esc(p.generated_at_age)}</div>${failingList}</div>`;
    }).join('');
  }

  function renderNelisp(panel){
    var el = document.getElementById('nelisp-panel');
    if (!panel || panel.ok === false){
      el.innerHTML = `<p class='empty'>${esc(t('unavailable'))}: ${esc(panel && panel.error)}</p>`;
      return;
    }
    var items = panel.items || {};
    var gates = items.gates || [];
    var st = items.status;
    var head = st
      ? `<p class='muted'>${esc(st.branch)} @ ${esc(st.head)} - modified ${esc(st.modified)} - untracked ${esc(st.untracked)} - other worktrees ${esc(st.other_worktrees)}</p>`
      : '';
    var rows = gates.length
      ? gates.map(function(g){
          return `<span class='badge ${g.status === 'fail' ? 'failed' : 'running'}' style='margin:2px'>${esc(g.name)}</span>`;
        }).join('')
      : `<span class='empty'>${esc(t('noitems'))}</span>`;
    el.innerHTML = head + '<div>' + rows + '</div>';
  }

  function renderWorklog(panel){
    var el = document.getElementById('worklog-panel');
    if (!panel || panel.ok === false){
      el.innerHTML = `<p class='empty'>${esc(t('unavailable'))}: ${esc(panel && panel.error)}</p>`;
      return;
    }
    var items = panel.items || [];
    el.innerHTML = items.length
      ? '<ul>' + items.map(function(e){ return `<li>${esc(e.date)} - ${esc(e.title)}</li>`; }).join('') + '</ul>'
      : `<p class='empty'>${esc(t('noitems'))}</p>`;
  }

  function renderHandoff(panel){
    var el = document.getElementById('handoff-panel');
    if (!panel || panel.ok === false){
      el.innerHTML = `<p class='empty'>${esc(t('unavailable'))}: ${esc(panel && panel.error)}</p>`;
      return;
    }
    var items = panel.items || [];
    el.innerHTML = items.length
      ? '<ul>' + items.map(function(f){ return `<li>${esc(f.name)} - ${esc(f.modified)}</li>`; }).join('') + '</ul>'
      : `<p class='empty'>${esc(t('noitems'))}</p>`;
  }

  function renderDaemon(panel){
    var el = document.getElementById('daemon-panel');
    if (!panel || panel.ok === false){
      el.innerHTML = `<p class='empty'>${esc(t('unavailable'))}: ${esc(panel && panel.error)}</p>`;
      return;
    }
    var it = panel.items || {};
    var cls = it.red ? 'failed' : 'running';
    el.innerHTML = `<span class='badge ${cls}'>${esc(it.count)} ${esc(t('hung'))}</span> ` +
      `<span class='muted'>oldest ${esc(it.oldest_age_seconds)}s</span>`;
  }

  function load(){
    fetch('/api/snapshot').then(function(r){ return r.json(); }).then(function(d){
      var p = d.panels || {};
      renderCards(p.stats);
      renderWorkers(p.workers);
      renderProgress(p.progress);
      renderNelisp(p.nelisp);
      renderWorklog(p.worklog);
      renderHandoff(p.handoff);
      renderDaemon(p.daemon);
      document.getElementById('footer-note').textContent =
        (lang === 'ja' ? '更新: ' : 'updated: ') + d.generated_at;
    }).catch(function(){ /* transient fetch error: keep last render, retry next tick */ });
  }

  var timer = null;
  function schedule(){
    if (timer) clearInterval(timer);
    timer = setInterval(function(){ if (!document.hidden) load(); }, 10000);
  }

  document.getElementById('lang-toggle').addEventListener('click', function(){
    lang = (lang === 'en') ? 'ja' : 'en';
    try { localStorage.setItem('anvil-dashboard-lang', lang); } catch (e) {}
    applyStaticLabels();
    updateShowAllLabel();
    load();
  });

  var showAllBtn = document.getElementById('workers-show-all');
  function updateShowAllLabel(){
    showAllBtn.textContent = t(showAllWorkers ? 'showactive' : 'showall');
  }
  showAllBtn.addEventListener('click', function(){
    showAllWorkers = !showAllWorkers;
    updateShowAllLabel();
    renderWorkers(lastWorkersPanel);
  });
  updateShowAllLabel();

  applyStaticLabels();
  load();
  schedule();
})();
</script>
</body>
</html>
"
  "The dashboard's single HTML page: inline CSS/JS, no CDN dependency.")

;;;; --- HTTP server (pattern copied from anvil-memory.el) --------------------

(defun anvil-dashboard--running-p ()
  (and anvil-dashboard--proc (process-live-p anvil-dashboard--proc)))

(defun anvil-dashboard--status-line (status)
  (concat "HTTP/1.0 " (number-to-string status) " "
          (cond ((= status 200) "OK")
                ((= status 400) "Bad Request")
                ((= status 404) "Not Found")
                ((= status 405) "Method Not Allowed")
                (t "Internal Server Error"))
          "\r\n"))

(defun anvil-dashboard--http-response (status content-type body)
  (concat (anvil-dashboard--status-line status)
          (format "Content-Type: %s\r\n" content-type)
          (format "Content-Length: %d\r\n" (string-bytes body))
          "Connection: close\r\n"
          "Cache-Control: no-store\r\n"
          "\r\n"
          body))

(defun anvil-dashboard--parse-request-line (line)
  "Parse the first line of an HTTP request into (METHOD . PATH), else nil."
  (when (and line (string-match "\\`\\([A-Z]+\\) \\([^ ]+\\) HTTP/[0-9.]+\\'" line))
    (cons (match-string 1 line) (match-string 2 line))))

(defun anvil-dashboard--dispatch (method path)
  "Dispatch HTTP METHOD + PATH to a response string."
  (cond
   ((not (equal method "GET"))
    (anvil-dashboard--http-response 405 "text/plain; charset=utf-8"
                                     "Only GET is supported.\n"))
   ((equal path "/")
    (anvil-dashboard--http-response 200 "text/html; charset=utf-8" anvil-dashboard--html))
   ((equal path "/api/snapshot")
    (let ((json-encoding-pretty-print nil))
      (anvil-dashboard--http-response
       200 "application/json; charset=utf-8" (json-encode (anvil-dashboard-snapshot)))))
   (t (anvil-dashboard--http-response 404 "text/plain; charset=utf-8"
                                       (format "Not Found: %s\n" path)))))

(defun anvil-dashboard--handle-buffer (buf)
  "Parse BUF and return a response string, or nil while incomplete."
  (let ((split (string-match "\r\n\r\n" buf)))
    (when split
      (let* ((head (substring buf 0 split))
             (lines (split-string head "\r\n" t))
             (parsed (anvil-dashboard--parse-request-line (car lines))))
        (if parsed
            (anvil-dashboard--dispatch (car parsed) (cdr parsed))
          (anvil-dashboard--http-response 400 "text/plain; charset=utf-8" "Bad Request\n"))))))

(defun anvil-dashboard--filter (proc data)
  "Per-connection filter: buffer bytes, respond, close on completion."
  (let ((buf (concat (or (process-get proc :request-buffer) "") data)))
    (process-put proc :request-buffer buf)
    (let ((response
           (condition-case err
               (anvil-dashboard--handle-buffer buf)
             (error
              (anvil-dashboard--http-response
               500 "text/plain; charset=utf-8"
               (format "Server error: %s\n" (error-message-string err)))))))
      (when response
        (ignore-errors (process-send-string proc response))
        (ignore-errors (delete-process proc))))))

(defun anvil-dashboard--sentinel (_proc _event)
  "No-op: the filter owns response teardown." nil)

;;;###autoload
(cl-defun anvil-dashboard-serve-start (&key host port)
  "Start the local dashboard HTTP server; return (:host :port :process).
HOST defaults to `anvil-dashboard-host' and must be 127.0.0.1.  PORT
is an integer or `t' for an ephemeral OS-assigned port; defaults to
`anvil-dashboard-port'.  Signals `user-error' when already running."
  (interactive)
  (when (anvil-dashboard--running-p)
    (user-error "anvil-dashboard: already running on %s:%s"
                anvil-dashboard-host anvil-dashboard--port))
  (let ((h (or host anvil-dashboard-host))
        (p (or port anvil-dashboard-port)))
    (unless (equal h "127.0.0.1")
      (user-error "anvil-dashboard: host must be 127.0.0.1"))
    (let* ((proc (make-network-process
                  :name "anvil-dashboard"
                  :server t
                  :host h
                  :service p
                  :family 'ipv4
                  :coding 'binary
                  :noquery t
                  :filter #'anvil-dashboard--filter
                  :sentinel #'anvil-dashboard--sentinel))
           (assigned (process-contact proc :service)))
      (setq anvil-dashboard--proc proc
            anvil-dashboard--port (if (integerp assigned) assigned p))
      (list :host h :port anvil-dashboard--port :process proc))))

;;;###autoload
(defun anvil-dashboard-serve-stop ()
  "Stop the dashboard server, if running.  Returns the port it was
bound to, or nil when nothing was running.  Always safe to call."
  (interactive)
  (let ((port anvil-dashboard--port))
    (when (anvil-dashboard--running-p)
      (delete-process anvil-dashboard--proc))
    (setq anvil-dashboard--proc nil anvil-dashboard--port nil)
    port))

;;;; --- MCP tool registration --------------------------------------------

(defun anvil-dashboard--tool-serve-start (&optional port)
  "Start the local anvil dashboard HTTP server on loopback (127.0.0.1).
Returns the bound host/port.

MCP Parameters:
  port - TCP port to bind (integer), or true for an OS-assigned
    ephemeral port; omit to use `anvil-dashboard-port'."
  (anvil-server-with-error-handling
    ;; Drop the process object: it cannot be JSON-encoded, which made every
    ;; successful start report "Unknown JSON error: #<process ...>".
    (let ((r (anvil-dashboard-serve-start
              :port (if (stringp port) (string-to-number port) port))))
      (list :host (plist-get r :host) :port (plist-get r :port)))))

(defun anvil-dashboard--tool-serve-stop ()
  "Stop the local anvil dashboard HTTP server.  Idempotent."
  (anvil-server-with-error-handling
    (anvil-dashboard-serve-stop)))

(defun anvil-dashboard--tool-snapshot ()
  "Return the dashboard's current JSON snapshot without starting the
HTTP server (stats/workers/progress/nelisp/worklog/handoff/daemon)."
  (anvil-server-with-error-handling
    (anvil-dashboard-snapshot)))

(defconst anvil-dashboard--tool-specs
  `((,(anvil-server-encode-handler #'anvil-dashboard--tool-serve-start)
     :id "dashboard-serve-start"
     :description "Start the local anvil dashboard HTTP server on loopback (127.0.0.1). Returns the bound host/port.")
    (,(anvil-server-encode-handler #'anvil-dashboard--tool-serve-stop)
     :id "dashboard-serve-stop"
     :description "Stop the local anvil dashboard HTTP server. Idempotent.")
    (,(anvil-server-encode-handler #'anvil-dashboard--tool-snapshot)
     :id "dashboard-snapshot"
     :description "Return the dashboard's current JSON snapshot (stats/workers/progress/nelisp/worklog/handoff/daemon panels) without starting the HTTP server."
     :read-only t))
  "Spec list consumed by `anvil-server-register-tools'.")

(defun anvil-dashboard--register-tools ()
  (anvil-server-register-tools anvil-dashboard--server-id anvil-dashboard--tool-specs))

(defun anvil-dashboard--unregister-tools ()
  (anvil-server-unregister-tools anvil-dashboard--server-id anvil-dashboard--tool-specs))

;;;###autoload
(defun anvil-dashboard-enable ()
  "Register anvil-dashboard's MCP tools."
  (interactive)
  (anvil-dashboard--register-tools))

;;;###autoload
(defun anvil-dashboard-disable ()
  "Stop the dashboard server and unregister its MCP tools."
  (interactive)
  (anvil-dashboard-serve-stop)
  (anvil-dashboard--unregister-tools))

(provide 'anvil-dashboard)
;;; anvil-dashboard.el ends here
