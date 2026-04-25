;;; anvil-memory-obs.el --- Observation capture for session lifecycle  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 37 Phase 1 — observation capture layer.
;;
;; Records Claude Code lifecycle events (session-start /
;; user-prompt / post-tool-use / stop / session-end) into a
;; SQLite + FTS5 store so later phases can search, summarise and
;; surface relevant context across sessions.
;;
;; This module is an independent reimplementation inspired by the
;; public design of claude-mem (AGPL-3.0); it shares no source code
;; with that project and is licensed under the GPL-3.0-or-later that
;; covers anvil.el.  See `docs/design/37-claude-mem-bridge.org' for
;; the full design rationale.
;;
;; Phase split (Doc 37):
;;
;;   Phase 1 (this commit + follow-ups)  schema + record API +
;;                                       importance heuristic +
;;                                       redaction + purge.
;;   Phase 2                             AI compression worker
;;                                       backed by anvil-orchestrator.
;;   Phase 3                             3-layer search MCP tools.
;;   Phase 4                             SessionStart auto-inject.
;;   Phase 5                             Tabulated-list UI.
;;   Phase 6                             Promote to auto-memory.
;;   Phase 7 (deferred)                  Optional vector embedding.
;;
;; This module lives in `anvil-optional-modules' and is opt-in via
;; `anvil-memory-obs-enabled'; nothing in core anvil references it
;; directly, and all integration points (anvil-session) probe with
;; `fboundp' so a plain anvil install behaves exactly as before.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'anvil-server)

;; AI compression dispatches through anvil-orchestrator when
;; available.  The module is loaded lazily; rule-based fallback
;; works without it.
(declare-function anvil-orchestrator-submit-and-collect
                  "anvil-orchestrator")

;; Phase 6 promote-candidates uses anvil-memory-save-check to dedup
;; against the user's existing auto-memory store.  Soft dependency —
;; the dedup path is opt-in (`:check-against-memory t').
(declare-function anvil-memory-save-check "anvil-memory")

;; Phase 6 promote uses anvil-memory--effective-roots to pick a
;; default writable memory directory; soft-dep so an explicit
;; `:target-dir' continues to work without anvil-memory loaded.
(declare-function anvil-memory--effective-roots "anvil-memory")


;;;; --- group + defcustoms -------------------------------------------------

(defgroup anvil-memory-obs nil
  "Observation capture layer for Claude Code lifecycle events."
  :group 'anvil
  :prefix "anvil-memory-obs-")

(defconst anvil-memory-obs--server-id "emacs-eval"
  "Server id under which memory-obs-* MCP tools register (Phase 3+).")

(defcustom anvil-memory-obs-enabled nil
  "When non-nil, anvil-session lifecycle hooks record observations.
Default nil so installing this module is a no-op until the user
opts in explicitly."
  :type 'boolean
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-db-path
  (expand-file-name "anvil-memory-obs.db" user-emacs-directory)
  "SQLite file backing the observation log.
Kept separate from `anvil-memory-db-path' because observations have
a much shorter retention policy and a different schema."
  :type 'file
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-fts-tokenizer 'auto
  "Tokenizer used when (re-)creating the observation FTS5 tables.
Values mirror `anvil-memory-fts-tokenizer'.

  `auto'      probe the SQLite build; trigram when available.
  `trigram'   force the trigram tokenizer (CJK-friendly).
  `unicode61' force the legacy default."
  :type '(choice (const :tag "Auto (trigram if available)" auto)
                 (const :tag "Trigram (SQLite 3.34+)" trigram)
                 (const :tag "unicode61 (default)" unicode61))
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-redact-patterns
  '("password\\s-*=\\s-*[^[:space:]]+"
    "passwd\\s-*=\\s-*[^[:space:]]+"
    "api[_-]?key\\s-*[:=]\\s-*[^[:space:]]+"
    "secret\\s-*[:=]\\s-*[^[:space:]]+"
    "token\\s-*[:=]\\s-*[^[:space:]]+"
    "Bearer\\s-+[A-Za-z0-9._\\-]+"
    "ghp_[A-Za-z0-9]\\{36,\\}"
    "sk-[A-Za-z0-9]\\{20,\\}")
  "Regexp list applied to observation bodies before they are stored.
Matches are replaced with `[REDACTED]' so secrets that drift into
tool args / prompts are not persisted.  Patterns are applied in
order, case-insensitively."
  :type '(repeat regexp)
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-importance-rules
  '((:keyword "error\\|fail\\|exception\\|traceback" :delta 30)
    (:keyword "fix\\|fixed\\|patch\\|resolved"        :delta 20)
    (:keyword "todo\\|fixme\\|xxx"                    :delta 10)
    (:keyword "commit\\|merge"                        :delta 10))
  "Rule-based importance adjustments applied to observation bodies.
Each rule is a plist with `:keyword' (regexp, case-insensitive)
and `:delta' (integer added to importance when the regexp matches).
Phase 2 will replace this with an AI-derived score; the heuristic
is enough for Phase 1 surfacing."
  :type '(repeat (plist :options ((:keyword regexp) (:delta integer))))
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-purge-age-days 90
  "Observations older than this many days become eligible for purge.
Combined with `anvil-memory-obs-purge-importance-threshold' so
high-importance rows are kept even past the cutoff."
  :type 'integer
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-purge-importance-threshold 10
  "Importance below this is purged once a row reaches the age cutoff.
Set to nil to keep every row regardless of importance."
  :type '(choice (const :tag "Never (keep all)" nil)
                 integer)
  :group 'anvil-memory-obs)


;;;; --- Phase 2: compression knobs ---------------------------------------

(defcustom anvil-memory-obs-compress-min-observations 5
  "Skip session summarisation when fewer than this many obs rows exist.
Tiny sessions are not worth a summary row."
  :type 'integer
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-compress-rule-excerpt-chars 200
  "Characters retained from the head and tail when rule-based summary runs.
Total fallback length is roughly twice this value."
  :type 'integer
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-compress-fallback 'rule-based
  "Behaviour when AI compression is unavailable or skipped.
  `rule-based' — concat first/last excerpts of each observation.
  `none'       — skip; no summary row is created."
  :type '(choice (const :tag "Rule-based excerpt" rule-based)
                 (const :tag "No summary"         none))
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-use-ai-compression nil
  "When non-nil, `anvil-memory-obs-summarize-session' tries AI first.
The AI path requires `anvil-orchestrator-submit-and-collect' to be
loaded.  Failures fall back to the configured
`anvil-memory-obs-compress-fallback' silently."
  :type 'boolean
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-compress-provider "claude"
  "Default provider id passed to `anvil-orchestrator-submit-and-collect'."
  :type 'string
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-compress-model nil
  "Default model slug for AI compression (nil = provider default)."
  :type '(choice (const :tag "Provider default" nil) string)
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-compress-timeout-sec 60
  "Wall-clock cap for the AI compression call (`:collect-timeout-sec')."
  :type 'integer
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-compress-on-session-end nil
  "When non-nil, `anvil-memory-obs-record-session-end' auto-summarises.
Gated on `anvil-memory-obs-enabled' (which already gates the record
function itself); off by default so opting into observation capture
does not implicitly opt into LLM spend."
  :type 'boolean
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-compress-monthly-budget 100
  "Maximum number of AI summary calls allowed per calendar month.
When the count of `is_ai = 1' rows in `obs_summaries' for the
current month reaches this value, AI compression is skipped and
the rule-based fallback is used.  Set to nil to disable the guard."
  :type '(choice (const :tag "No budget guard" nil)
                 integer)
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-compress-prompt-template
  "You are summarising a software development session for later recall.
Read the observation log below and respond with **strict JSON only**:
{\"topic\": \"<= 8 words\", \"summary\": \"1-3 sentences\"}

Observations:
%s"
  "Prompt template; %s is replaced by the rendered observation list."
  :type 'string
  :group 'anvil-memory-obs)

(defconst anvil-memory-obs-supported
  '(schema record importance redact integration purge
           compress-rule-based compress-ai compress-auto budget
           search timeline get summary-search mcp-tools
           auto-inject auto-inject-integration
           promote-candidates promote)
  "Capability tags this module currently provides.
Tests `skip-unless' their tag is in this list so a half-shipped
feature never breaks CI.  Phase milestones append tags here.")


;;;; --- sqlite backend -----------------------------------------------------

(defvar anvil-memory-obs--db nil
  "Open handle on `anvil-memory-obs-db-path' (nil when idle).")

(defun anvil-memory-obs--require-sqlite ()
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error
     "anvil-memory-obs: Emacs SQLite backend unavailable (needs Emacs 29+)")))

(defun anvil-memory-obs--open ()
  "Open the backing DB and apply schema."
  (anvil-memory-obs--require-sqlite)
  (unless anvil-memory-obs--db
    (make-directory (file-name-directory anvil-memory-obs-db-path) t)
    (setq anvil-memory-obs--db (sqlite-open anvil-memory-obs-db-path))
    (anvil-memory-obs--ensure-schema))
  anvil-memory-obs--db)

(defun anvil-memory-obs--close ()
  (when anvil-memory-obs--db
    (ignore-errors (sqlite-close anvil-memory-obs--db))
    (setq anvil-memory-obs--db nil)))

(defun anvil-memory-obs--db ()
  (or anvil-memory-obs--db (anvil-memory-obs--open)))

(defun anvil-memory-obs--sqlite-supports-trigram-p (db)
  "Return non-nil when DB's SQLite build ships the FTS5 trigram tokenizer."
  (condition-case nil
      (progn
        (sqlite-execute
         db
         "CREATE VIRTUAL TABLE anvil_memory_obs_trigram_probe
            USING fts5(x, tokenize=trigram)")
        (sqlite-execute db "DROP TABLE anvil_memory_obs_trigram_probe")
        t)
    (error nil)))

(defun anvil-memory-obs--resolve-tokenizer (db)
  "Return the tokenizer symbol DB should use given the user's preference."
  (pcase anvil-memory-obs-fts-tokenizer
    ('trigram 'trigram)
    ('unicode61 'unicode61)
    ('auto (if (anvil-memory-obs--sqlite-supports-trigram-p db)
               'trigram
             'unicode61))
    (other (user-error
            "anvil-memory-obs: invalid `anvil-memory-obs-fts-tokenizer': %S"
            other))))

(defun anvil-memory-obs--tok-sql (tokenizer)
  "Return the FTS5 tokenize=... fragment for TOKENIZER (symbol)."
  (pcase tokenizer
    ('trigram   ", tokenize='trigram'")
    ('unicode61 "")
    (_ (user-error
        "anvil-memory-obs: invalid tokenizer: %S" tokenizer))))

(defun anvil-memory-obs--ensure-schema ()
  "Create tables / indexes / FTS5 virtual tables when missing."
  (let* ((db anvil-memory-obs--db)
         (tok (anvil-memory-obs--resolve-tokenizer db))
         (tok-sql (anvil-memory-obs--tok-sql tok)))
    (sqlite-execute db
                    "CREATE TABLE IF NOT EXISTS obs_sessions (
                       id           TEXT PRIMARY KEY,
                       started_at   INTEGER NOT NULL,
                       ended_at     INTEGER,
                       project_dir  TEXT,
                       meta_json    TEXT
                     )")
    (sqlite-execute db
                    "CREATE INDEX IF NOT EXISTS obs_sessions_started_idx
                       ON obs_sessions(started_at)")
    (sqlite-execute db
                    "CREATE TABLE IF NOT EXISTS obs_observations (
                       id            INTEGER PRIMARY KEY AUTOINCREMENT,
                       session_id    TEXT NOT NULL,
                       ts            INTEGER NOT NULL,
                       hook          TEXT NOT NULL,
                       tool_name     TEXT,
                       payload_json  TEXT,
                       body          TEXT,
                       importance    INTEGER NOT NULL DEFAULT 0,
                       is_compressed INTEGER NOT NULL DEFAULT 0
                     )")
    (sqlite-execute db
                    "CREATE INDEX IF NOT EXISTS obs_observations_session_idx
                       ON obs_observations(session_id)")
    (sqlite-execute db
                    "CREATE INDEX IF NOT EXISTS obs_observations_ts_idx
                       ON obs_observations(ts)")
    (sqlite-execute db
                    "CREATE INDEX IF NOT EXISTS obs_observations_hook_idx
                       ON obs_observations(hook)")
    (sqlite-execute db
                    "CREATE TABLE IF NOT EXISTS obs_summaries (
                       id            INTEGER PRIMARY KEY AUTOINCREMENT,
                       session_id    TEXT NOT NULL,
                       obs_start_id  INTEGER,
                       obs_end_id    INTEGER,
                       topic         TEXT,
                       summary       TEXT NOT NULL,
                       ts            INTEGER NOT NULL,
                       is_ai         INTEGER NOT NULL DEFAULT 0
                     )")
    (sqlite-execute db
                    "CREATE INDEX IF NOT EXISTS obs_summaries_session_idx
                       ON obs_summaries(session_id)")
    (sqlite-execute db
                    "CREATE INDEX IF NOT EXISTS obs_summaries_ts_idx
                       ON obs_summaries(ts)")
    ;; Forward-compat: a Phase 2 commit 1 DB lacks `is_ai'.  Add it
    ;; in place so the budget guard can run without requiring a
    ;; full rebuild.
    (unless (cl-some
             (lambda (col) (equal (nth 1 col) "is_ai"))
             (sqlite-select db "PRAGMA table_info('obs_summaries')"))
      (sqlite-execute
       db
       "ALTER TABLE obs_summaries
          ADD COLUMN is_ai INTEGER NOT NULL DEFAULT 0"))
    ;; Standalone FTS5 (no content= mode) so inserts are explicit and
    ;; trigger-free; rowid is aligned with the obs_observations.id by
    ;; the caller.  Mirrors the memory_body_fts approach in
    ;; anvil-memory.el for consistency.
    (sqlite-execute
     db
     (format "CREATE VIRTUAL TABLE IF NOT EXISTS obs_observations_fts
                USING fts5(body%s)"
             tok-sql))
    (sqlite-execute
     db
     (format "CREATE VIRTUAL TABLE IF NOT EXISTS obs_summaries_fts
                USING fts5(topic, summary%s)"
             tok-sql))))


;;;; --- redaction + importance --------------------------------------------

(defun anvil-memory-obs--redact (text)
  "Return TEXT with `anvil-memory-obs-redact-patterns' replaced by [REDACTED].
Returns TEXT unchanged when nil or empty."
  (if (or (null text) (string-empty-p text))
      text
    (let ((case-fold-search t)
          (out text))
      (dolist (pat anvil-memory-obs-redact-patterns)
        (setq out (replace-regexp-in-string pat "[REDACTED]" out)))
      out)))

(defun anvil-memory-obs--compute-importance (body)
  "Sum :delta from every `anvil-memory-obs-importance-rules' matching BODY.
Result is clamped to [0, 100]."
  (if (or (null body) (string-empty-p body))
      0
    (let ((case-fold-search t)
          (score 0))
      (dolist (rule anvil-memory-obs-importance-rules)
        (let ((kw (plist-get rule :keyword))
              (delta (plist-get rule :delta)))
          (when (and (stringp kw) (integerp delta)
                     (string-match-p kw body))
            (setq score (+ score delta)))))
      (max 0 (min 100 score)))))


;;;; --- encoder + insert helpers ------------------------------------------

(defun anvil-memory-obs--encode-payload (payload)
  "Encode PAYLOAD (plist or alist) to a string for the payload_json column.
Phase 1 stores `prin1-to-string'; Phase 2+ may switch to real JSON."
  (when payload
    (let ((print-length nil)
          (print-level nil))
      (prin1-to-string payload))))

(defun anvil-memory-obs--now ()
  "Return current unix epoch as integer."
  (truncate (time-to-seconds)))

(defun anvil-memory-obs--upsert-session (session-id &optional project-dir)
  "INSERT a session row if SESSION-ID is new; otherwise leave it untouched.
Returns the session id."
  (let ((db (anvil-memory-obs--db)))
    (sqlite-execute
     db
     "INSERT OR IGNORE INTO obs_sessions (id, started_at, project_dir)
        VALUES (?, ?, ?)"
     (list session-id (anvil-memory-obs--now) (or project-dir "")))
    session-id))

(defun anvil-memory-obs--insert-observation (&rest args)
  "Insert an observation row.  Plist ARGS:
  :session-id  required string
  :hook        required string (e.g. \"session-start\")
  :tool-name   optional string
  :body        optional string (redacted before storage)
  :payload     optional plist/alist (prin1'd into payload_json)
  :ts          optional integer epoch (default `anvil-memory-obs--now')
Returns the new row id (integer)."
  (let* ((db (anvil-memory-obs--db))
         (session-id (plist-get args :session-id))
         (hook (plist-get args :hook))
         (tool-name (plist-get args :tool-name))
         (raw-body (plist-get args :body))
         (body (anvil-memory-obs--redact raw-body))
         (payload (plist-get args :payload))
         (payload-json (or (anvil-memory-obs--encode-payload payload) ""))
         (importance (anvil-memory-obs--compute-importance body))
         (ts (or (plist-get args :ts) (anvil-memory-obs--now))))
    (unless (and (stringp session-id) (stringp hook))
      (user-error "anvil-memory-obs: :session-id and :hook are required"))
    (sqlite-execute
     db
     "INSERT INTO obs_observations
        (session_id, ts, hook, tool_name, payload_json, body, importance)
        VALUES (?, ?, ?, ?, ?, ?, ?)"
     (list session-id ts hook (or tool-name "")
           payload-json (or body "") importance))
    (let ((rowid (caar (sqlite-select db "SELECT last_insert_rowid()"))))
      (sqlite-execute
       db
       "INSERT INTO obs_observations_fts (rowid, body) VALUES (?, ?)"
       (list rowid (or body "")))
      rowid)))


;;;; --- public record API --------------------------------------------------

(defun anvil-memory-obs-record-session-start (session-id &optional project-dir)
  "Record a session-start observation.  No-op when disabled.
Returns the new observation row id, or nil when disabled."
  (when (and anvil-memory-obs-enabled (stringp session-id))
    (anvil-memory-obs--upsert-session session-id project-dir)
    (anvil-memory-obs--insert-observation
     :session-id session-id
     :hook "session-start"
     :body (format "session %s started" session-id)
     :payload (list :session-id session-id
                    :project-dir project-dir))))

(defun anvil-memory-obs-record-user-prompt (session-id prompt)
  "Record a user-prompt observation.  PROMPT is the (excerpt) text."
  (when (and anvil-memory-obs-enabled (stringp session-id))
    (anvil-memory-obs--upsert-session session-id)
    (anvil-memory-obs--insert-observation
     :session-id session-id
     :hook "user-prompt"
     :body (or prompt "")
     :payload (list :prompt prompt))))

(defun anvil-memory-obs-record-post-tool-use (session-id tool-name &optional summary)
  "Record a post-tool-use observation.
SUMMARY is the (truncated) tool result/argument summary."
  (when (and anvil-memory-obs-enabled (stringp session-id))
    (anvil-memory-obs--upsert-session session-id)
    (anvil-memory-obs--insert-observation
     :session-id session-id
     :hook "post-tool-use"
     :tool-name (or tool-name "")
     :body (or summary "")
     :payload (list :tool tool-name :summary summary))))

(defun anvil-memory-obs-record-stop (session-id &optional transcript-path)
  "Record a stop observation (Claude Code Stop hook)."
  (when (and anvil-memory-obs-enabled (stringp session-id))
    (anvil-memory-obs--upsert-session session-id)
    (anvil-memory-obs--insert-observation
     :session-id session-id
     :hook "stop"
     :body (or transcript-path "")
     :payload (list :transcript-path transcript-path))))

(defun anvil-memory-obs-record-session-end (session-id)
  "Record a session-end observation and stamp ended_at on the session row.
When `anvil-memory-obs-compress-on-session-end' is non-nil, also
trigger `anvil-memory-obs-summarize-session' after the row is
written; summarisation errors are swallowed so a flaky orchestrator
never breaks the session-end pipeline."
  (when (and anvil-memory-obs-enabled (stringp session-id))
    (let ((db (anvil-memory-obs--db))
          (now (anvil-memory-obs--now)))
      (anvil-memory-obs--upsert-session session-id)
      (sqlite-execute
       db
       "UPDATE obs_sessions SET ended_at = ? WHERE id = ?"
       (list now session-id))
      (let ((rowid (anvil-memory-obs--insert-observation
                    :session-id session-id
                    :hook "session-end"
                    :body (format "session %s ended" session-id)
                    :ts now)))
        (when anvil-memory-obs-compress-on-session-end
          (ignore-errors
            (anvil-memory-obs-summarize-session session-id)))
        rowid))))


;;;; --- purge --------------------------------------------------------------

(defun anvil-memory-obs-purge (&optional age-days importance-threshold)
  "Delete observation rows older than AGE-DAYS with importance below threshold.
AGE-DAYS defaults to `anvil-memory-obs-purge-age-days'.
IMPORTANCE-THRESHOLD defaults to
`anvil-memory-obs-purge-importance-threshold'.  When the threshold
is nil, no rows are deleted regardless of age.
Returns the number of observation rows removed."
  (let* ((db (anvil-memory-obs--db))
         (age (or age-days anvil-memory-obs-purge-age-days))
         (thr (or importance-threshold
                  anvil-memory-obs-purge-importance-threshold))
         (cutoff (- (anvil-memory-obs--now) (* age 24 60 60))))
    (if (null thr)
        0
      ;; Capture target ids first so the FTS5 mirror delete and the
      ;; main delete operate on the same set even if the clock ticks
      ;; between the two statements.
      (let* ((rows (sqlite-select
                    db
                    "SELECT id FROM obs_observations
                      WHERE ts < ? AND importance < ?"
                    (list cutoff thr)))
             (ids (mapcar #'car rows)))
        (when ids
          (let ((placeholders (mapconcat (lambda (_) "?") ids ",")))
            (sqlite-execute
             db
             (format "DELETE FROM obs_observations_fts WHERE rowid IN (%s)"
                     placeholders)
             ids)
            (sqlite-execute
             db
             (format "DELETE FROM obs_observations WHERE id IN (%s)"
                     placeholders)
             ids)))
        (length ids)))))


;;;; --- Phase 2: compression --------------------------------------------

(defun anvil-memory-obs--gather-observations (session-id)
  "Return rows (id ts hook tool body importance) for SESSION-ID, oldest first."
  (sqlite-select
   (anvil-memory-obs--db)
   "SELECT id, ts, hook, tool_name, body, importance
      FROM obs_observations
      WHERE session_id = ?
      ORDER BY id ASC"
   (list session-id)))

(defun anvil-memory-obs--rule-based-summary (obs-rows)
  "Build a deterministic summary from OBS-ROWS without an LLM.
Concatenates each row's hook + body excerpt; the result is one
short paragraph suitable as a temporary stand-in until Phase 2
ships the AI path."
  (let* ((cap anvil-memory-obs-compress-rule-excerpt-chars)
         (lines
          (cl-loop for row in obs-rows
                   for hook = (nth 2 row)
                   for body = (or (nth 4 row) "")
                   for excerpt = (if (<= (length body) cap)
                                     body
                                   (concat (substring body 0 cap)
                                           " […] "
                                           (substring body (- (length body)
                                                              cap))))
                   collect (format "[%s] %s" hook excerpt))))
    (string-join lines "\n")))

(defun anvil-memory-obs--rule-based-topic (obs-rows)
  "Pick a coarse topic for OBS-ROWS by majority hook + tool name."
  (let* ((tools (cl-loop for row in obs-rows
                         for tool = (nth 3 row)
                         when (and tool (not (string-empty-p tool)))
                         collect tool))
         (most-tool (when tools
                      (caar (sort
                             (mapcar (lambda (s) (cons s 1)) tools)
                             (lambda (a _) (stringp (car a)))))))
         (count (length obs-rows)))
    (if most-tool
        (format "%d obs (%s)" count most-tool)
      (format "%d obs" count))))

(defun anvil-memory-obs--insert-summary (session-id topic summary
                                                    obs-start-id obs-end-id
                                                    &optional is-ai)
  "Persist a summary row and its FTS5 mirror.  Returns the row id.
IS-AI is the integer flag (1 when AI produced the summary, 0 otherwise);
nil treats it as 0."
  (let* ((db (anvil-memory-obs--db))
         (ts (anvil-memory-obs--now)))
    (sqlite-execute
     db
     "INSERT INTO obs_summaries
        (session_id, obs_start_id, obs_end_id, topic, summary, ts, is_ai)
        VALUES (?, ?, ?, ?, ?, ?, ?)"
     (list session-id obs-start-id obs-end-id
           (or topic "") summary ts (if is-ai 1 0)))
    (let ((rowid (caar (sqlite-select db "SELECT last_insert_rowid()"))))
      (sqlite-execute
       db
       "INSERT INTO obs_summaries_fts (rowid, topic, summary)
          VALUES (?, ?, ?)"
       (list rowid (or topic "") summary))
      rowid)))

(defun anvil-memory-obs--current-month-start (&optional now)
  "Return the unix epoch of the first day of the month containing NOW.
NOW defaults to the current time (`anvil-memory-obs--now')."
  (let* ((t* (or now (anvil-memory-obs--now)))
         (dec (decode-time t*))
         (year (nth 5 dec))
         (month (nth 4 dec)))
    (truncate (time-to-seconds (encode-time 0 0 0 1 month year)))))

(defun anvil-memory-obs--ai-budget-exhausted-p ()
  "Non-nil when this calendar month's AI summary count >= budget."
  (when anvil-memory-obs-compress-monthly-budget
    (let* ((db (anvil-memory-obs--db))
           (since (anvil-memory-obs--current-month-start))
           (count (caar (sqlite-select
                         db
                         "SELECT COUNT(*) FROM obs_summaries
                            WHERE is_ai = 1 AND ts >= ?"
                         (list since)))))
      (>= count anvil-memory-obs-compress-monthly-budget))))

(defun anvil-memory-obs--render-obs-for-prompt (obs-rows)
  "Render OBS-ROWS as `[hook] body' lines for the AI prompt."
  (mapconcat
   (lambda (row)
     (format "[%s] %s" (nth 2 row) (or (nth 4 row) "")))
   obs-rows
   "\n"))

(defun anvil-memory-obs--strip-code-fences (text)
  "Drop a leading / trailing ```json fence around TEXT, if any."
  (let ((s text))
    (setq s (replace-regexp-in-string
             "\\`[[:space:]]*```[a-zA-Z]*\n?" "" s))
    (setq s (replace-regexp-in-string
             "\n?[[:space:]]*```[[:space:]]*\\'" "" s))
    s))

(defun anvil-memory-obs--parse-ai-response (text)
  "Extract (TOPIC . SUMMARY) from TEXT.
Tries strict JSON first; returns nil on any error so callers can
fall back to the rule-based path."
  (when (and (stringp text) (not (string-empty-p text)))
    (condition-case _err
        (let* ((cleaned (anvil-memory-obs--strip-code-fences text))
               (json-object-type 'plist)
               (json-key-type 'keyword)
               (json-array-type 'list)
               (parsed (json-read-from-string cleaned))
               (topic (plist-get parsed :topic))
               (summary (plist-get parsed :summary)))
          (when (and (stringp topic) (stringp summary)
                     (not (string-empty-p summary)))
            (cons topic summary)))
      (error nil))))

(defun anvil-memory-obs--ai-summary (obs-rows)
  "Submit OBS-ROWS to anvil-orchestrator and parse the JSON response.
Returns (TOPIC . SUMMARY) on success or nil on any failure path
(orchestrator unavailable, provider error, malformed JSON, timeout)."
  (when (and obs-rows
             (fboundp 'anvil-orchestrator-submit-and-collect))
    (condition-case _err
        (let* ((rendered (anvil-memory-obs--render-obs-for-prompt obs-rows))
               (prompt (format anvil-memory-obs-compress-prompt-template
                               rendered))
               (result (funcall (intern "anvil-orchestrator-submit-and-collect")
                                :provider anvil-memory-obs-compress-provider
                                :model anvil-memory-obs-compress-model
                                :prompt prompt
                                :collect-timeout-sec
                                anvil-memory-obs-compress-timeout-sec))
               (status (and (listp result) (plist-get result :status)))
               (text (when (eq status 'done)
                       (or (plist-get result :summary)
                           (plist-get result :text)))))
          (anvil-memory-obs--parse-ai-response text))
      (error nil))))

(defun anvil-memory-obs-summarize-session (session-id &optional force-fallback)
  "Compress SESSION-ID's observations into a single summary row.
Tries AI summarisation when
`anvil-memory-obs-use-ai-compression' is non-nil,
`anvil-orchestrator-submit-and-collect' is loaded, and the monthly
AI budget is not exhausted; otherwise (or when FORCE-FALLBACK is
non-nil) emits the rule-based summary.
Returns the new summary row id, or nil when the session is below
the size gate or summaries are disabled."
  (let* ((rows (anvil-memory-obs--gather-observations session-id))
         (n (length rows)))
    (cond
     ((< n anvil-memory-obs-compress-min-observations) nil)
     ((eq anvil-memory-obs-compress-fallback 'none)    nil)
     (t
      (let* ((ai-allowed (and (not force-fallback)
                              anvil-memory-obs-use-ai-compression
                              (not (anvil-memory-obs--ai-budget-exhausted-p))))
             (ai (when ai-allowed
                   (anvil-memory-obs--ai-summary rows)))
             (topic (or (car-safe ai)
                        (anvil-memory-obs--rule-based-topic rows)))
             (summary (or (cdr-safe ai)
                          (anvil-memory-obs--rule-based-summary rows)))
             (start-id (nth 0 (car rows)))
             (end-id   (nth 0 (car (last rows)))))
        (anvil-memory-obs--insert-summary
         session-id topic summary start-id end-id
         (when ai 1)))))))


;;;; --- Phase 3: 3-layer search APIs --------------------------------------

(defcustom anvil-memory-obs-search-preview-chars 80
  "Characters retained in the `:preview' field of Layer 1 search results.
Tuned so each row stays under ~100 tokens for cheap index scans."
  :type 'integer
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-get-max-ids 20
  "Maximum number of ids `anvil-memory-obs-get' accepts per call."
  :type 'integer
  :group 'anvil-memory-obs)

(cl-defun anvil-memory-obs-search (query &key limit hook session-id)
  "Layer 1 — FTS5 index search across observation bodies.
QUERY is required.  :LIMIT defaults to 10.  :HOOK restricts to a
specific event (e.g. `post-tool-use').  :SESSION-ID scopes to one
session.  Returns plist list ordered by FTS rank (best first):
  (:id :ts :session-id :hook :tool-name :preview :rank)
Empty / whitespace-only QUERY returns nil."
  (when (and query (stringp query)
             (not (string-empty-p (string-trim query))))
    (let* ((db (anvil-memory-obs--db))
           (limit (or limit 10))
           (clauses (list "obs_observations_fts MATCH ?"))
           (params (list query)))
      (when hook
        (push "o.hook = ?" clauses)
        (push hook params))
      (when session-id
        (push "o.session_id = ?" clauses)
        (push session-id params))
      (let* ((sql (format
                   "SELECT o.id, o.ts, o.session_id, o.hook, o.tool_name,
                          substr(o.body, 1, %d) AS preview,
                          obs_observations_fts.rank AS r
                     FROM obs_observations_fts
                     JOIN obs_observations o
                       ON o.id = obs_observations_fts.rowid
                    WHERE %s
                 ORDER BY r
                    LIMIT ?"
                   anvil-memory-obs-search-preview-chars
                   (string-join (nreverse clauses) " AND ")))
             (rows (ignore-errors
                     (sqlite-select
                      db sql
                      (append (nreverse params) (list limit))))))
        (mapcar (lambda (r)
                  (list :id         (nth 0 r)
                        :ts         (nth 1 r)
                        :session-id (nth 2 r)
                        :hook       (nth 3 r)
                        :tool-name  (nth 4 r)
                        :preview    (nth 5 r)
                        :rank       (nth 6 r)))
                rows)))))

(cl-defun anvil-memory-obs-timeline (anchor-id &key window)
  "Layer 2 — return ±WINDOW observations around ANCHOR-ID, same session.
WINDOW defaults to 5.  Returns plist list ordered by id ASC; the
anchor is included in the result.  Returns nil when ANCHOR-ID does
not exist."
  (let* ((db (anvil-memory-obs--db))
         (window (or window 5))
         (anchor (sqlite-select
                  db
                  "SELECT session_id FROM obs_observations WHERE id = ?"
                  (list anchor-id))))
    (when anchor
      (let* ((session-id (caar anchor))
             (rows (sqlite-select
                    db
                    "SELECT id, ts, hook, tool_name, body, importance
                       FROM obs_observations
                       WHERE session_id = ?
                         AND id >= ?
                         AND id <= ?
                       ORDER BY id ASC"
                    (list session-id
                          (- anchor-id window)
                          (+ anchor-id window)))))
        (mapcar (lambda (r)
                  (list :id         (nth 0 r)
                        :ts         (nth 1 r)
                        :hook       (nth 2 r)
                        :tool-name  (nth 3 r)
                        :body       (nth 4 r)
                        :importance (nth 5 r)))
                rows)))))

(defun anvil-memory-obs-get (ids)
  "Layer 3 — fetch full observation rows for IDS list.
IDS is a list of integers; max `anvil-memory-obs-get-max-ids' per
call (default 20).  Returns plist list ordered by id ASC:
  (:id :session-id :ts :hook :tool-name :body :payload-json :importance)"
  (when ids
    (when (> (length ids) anvil-memory-obs-get-max-ids)
      (user-error
       "anvil-memory-obs-get: max %d ids per call (got %d)"
       anvil-memory-obs-get-max-ids (length ids)))
    (let* ((db (anvil-memory-obs--db))
           (placeholders (mapconcat (lambda (_) "?") ids ","))
           (sql (format
                 "SELECT id, session_id, ts, hook, tool_name,
                        body, payload_json, importance
                   FROM obs_observations
                   WHERE id IN (%s)
                   ORDER BY id ASC"
                 placeholders))
           (rows (sqlite-select db sql ids)))
      (mapcar (lambda (r)
                (list :id           (nth 0 r)
                      :session-id   (nth 1 r)
                      :ts           (nth 2 r)
                      :hook         (nth 3 r)
                      :tool-name    (nth 4 r)
                      :body         (nth 5 r)
                      :payload-json (nth 6 r)
                      :importance   (nth 7 r)))
              rows))))

(cl-defun anvil-memory-obs-summary-search (query &key limit)
  "Topic-level search over `obs_summaries' via FTS5.
QUERY is required.  :LIMIT defaults to 10.  Returns plist list:
  (:id :session-id :topic :summary :ts :is-ai :rank)
Empty / whitespace-only QUERY returns nil."
  (when (and query (stringp query)
             (not (string-empty-p (string-trim query))))
    (let* ((db (anvil-memory-obs--db))
           (limit (or limit 10))
           (rows (ignore-errors
                   (sqlite-select
                    db
                    "SELECT s.id, s.session_id, s.topic, s.summary,
                            s.ts, s.is_ai, obs_summaries_fts.rank
                       FROM obs_summaries_fts
                       JOIN obs_summaries s
                         ON s.id = obs_summaries_fts.rowid
                      WHERE obs_summaries_fts MATCH ?
                   ORDER BY obs_summaries_fts.rank
                      LIMIT ?"
                    (list query limit)))))
      (mapcar (lambda (r)
                (list :id         (nth 0 r)
                      :session-id (nth 1 r)
                      :topic      (nth 2 r)
                      :summary    (nth 3 r)
                      :ts         (nth 4 r)
                      :is-ai      (and (nth 5 r) (= (nth 5 r) 1))
                      :rank       (nth 6 r)))
              rows))))


;;;; --- Phase 4: SessionStart auto-inject ---------------------------------

(defcustom anvil-memory-obs-auto-inject nil
  "When non-nil, SessionStart prepends related summaries to the preamble.
Off by default: opting into observation capture does not implicitly
inject any context until the user is happy with the budget knobs."
  :type 'boolean
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-auto-inject-window-days 7
  "Look back this many days when picking auto-inject candidates."
  :type 'integer
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-auto-inject-max-summaries 5
  "Cap on the number of summaries injected per SessionStart."
  :type 'integer
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-auto-inject-max-chars 4000
  "Hard cap on the rendered preamble length (~1000 tokens at 4 char/token)."
  :type 'integer
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-auto-inject-project-match 'prefix
  "Strategy for matching `obs_sessions.project_dir' to current project-dir.

  `exact'  — only when the strings are identical.
  `prefix' — either string is a prefix of the other (default).
  `any'    — accept every prior session (most aggressive).
  `none'   — disable project filtering entirely (alias of `any')."
  :type '(choice (const :tag "Exact match"           exact)
                 (const :tag "Prefix match (either)" prefix)
                 (const :tag "Any project"           any)
                 (const :tag "No filter"             none))
  :group 'anvil-memory-obs)

(defun anvil-memory-obs--project-match-p (current other)
  "Return non-nil when CURRENT and OTHER project dirs are considered related."
  (pcase anvil-memory-obs-auto-inject-project-match
    ((or 'any 'none) t)
    ('exact (and (stringp current) (stringp other) (equal current other)))
    ('prefix
     (when (and (stringp current) (stringp other)
                (not (string-empty-p current))
                (not (string-empty-p other)))
       (or (string-prefix-p current other)
           (string-prefix-p other current))))
    (_ nil)))

(defun anvil-memory-obs--auto-inject-candidates (current-project)
  "Return ranked summary candidate plists from the inject window.
Filters by `anvil-memory-obs-auto-inject-project-match' against
CURRENT-PROJECT.  Does not enforce the max-summaries cap; the
caller trims after de-duplicating."
  (let* ((db (anvil-memory-obs--db))
         (since (- (anvil-memory-obs--now)
                   (* anvil-memory-obs-auto-inject-window-days
                      24 60 60)))
         ;; Pull a wider window than max-summaries so the project
         ;; filter has room to discard misses.
         (raw-cap (* (max anvil-memory-obs-auto-inject-max-summaries 1)
                     4))
         (rows (sqlite-select
                db
                "SELECT s.id, s.session_id, s.topic, s.summary,
                        s.ts, s.is_ai,
                        COALESCE(sess.project_dir, '')
                   FROM obs_summaries s
                   LEFT JOIN obs_sessions sess
                     ON sess.id = s.session_id
                  WHERE s.ts >= ?
               ORDER BY (s.ts + s.is_ai * 86400) DESC
                  LIMIT ?"
                (list since raw-cap)))
         (rendered (mapcar
                    (lambda (r)
                      (list :id (nth 0 r)
                            :session-id (nth 1 r)
                            :topic (nth 2 r)
                            :summary (nth 3 r)
                            :ts (nth 4 r)
                            :is-ai (and (nth 5 r) (= (nth 5 r) 1))
                            :project-dir (nth 6 r)))
                    rows)))
    (cl-remove-if-not
     (lambda (r)
       (anvil-memory-obs--project-match-p
        current-project (plist-get r :project-dir)))
     rendered)))

(defun anvil-memory-obs--render-preamble (rows)
  "Format ROWS as a Markdown preamble block.
Caps at `anvil-memory-obs-auto-inject-max-chars'.  Empty ROWS
yields an empty string so callers can concat unconditionally."
  (if (null rows)
      ""
    (let* ((cap anvil-memory-obs-auto-inject-max-chars)
           (header
            "## Related context from prior anvil-memory-obs sessions\n\n")
           (lines
            (mapconcat
             (lambda (r)
               (format "- **%s%s** %s"
                       (or (plist-get r :topic) "(no topic)")
                       (if (plist-get r :is-ai) "" " [rule-based]")
                       (or (plist-get r :summary) "")))
             rows
             "\n"))
           (full (concat header lines "\n")))
      (if (<= (length full) cap)
          full
        (let* ((cut (or (and (>= cap 0)
                             (cl-search "\n" full
                                        :start2 0 :end2 cap
                                        :from-end t))
                        cap)))
          (concat (substring full 0 cut)
                  "\n[truncated]\n"))))))

(defun anvil-memory-obs-build-session-preamble (_session-id
                                                &optional project-dir)
  "Return a Markdown preamble (possibly empty) for SessionStart injection.
Empty when `anvil-memory-obs-auto-inject' is nil or no candidates
qualify.  PROJECT-DIR defaults to `default-directory'."
  (if (not anvil-memory-obs-auto-inject)
      ""
    (let* ((current (or project-dir default-directory))
           (candidates (anvil-memory-obs--auto-inject-candidates current))
           (capped (cl-subseq
                    candidates 0
                    (min (length candidates)
                         anvil-memory-obs-auto-inject-max-summaries))))
      (anvil-memory-obs--render-preamble capped))))


;;;; --- Phase 6: promote candidates ---------------------------------------

(defcustom anvil-memory-obs-promote-min-importance 30
  "Minimum aggregate observation importance for a promote candidate.
Sums `obs_observations.importance' across the underlying rows;
high-impact sessions clear this gate without manual triage."
  :type 'integer
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-promote-window-days 30
  "Look back this many days when ranking promote candidates."
  :type 'integer
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-promote-similar-threshold 0.5
  "Drop a candidate when a MEMORY.md row has Jaccard similarity >= this.
Only consulted when `:check-against-memory' is non-nil."
  :type 'number
  :group 'anvil-memory-obs)

(cl-defun anvil-memory-obs-promote-candidates
    (&key limit min-importance check-against-memory window-days)
  "Return ranked promote candidates from recent compressed summaries.
Each candidate plist:
  (:summary-id :session-id :topic :summary :is-ai
   :total-importance :similar)
:LIMIT defaults to 5.
:MIN-IMPORTANCE defaults to
  `anvil-memory-obs-promote-min-importance'.
:WINDOW-DAYS defaults to
  `anvil-memory-obs-promote-window-days'.
:CHECK-AGAINST-MEMORY (default nil) — when non-nil and
  `anvil-memory-save-check' is loaded, each candidate is scored
  against the auto-memory store and rows whose top match is at or
  above `anvil-memory-obs-promote-similar-threshold' are filtered
  out; the remaining rows carry the match list under `:similar'."
  (let* ((db (anvil-memory-obs--db))
         (limit (or limit 5))
         (min-imp (or min-importance
                      anvil-memory-obs-promote-min-importance))
         (window (or window-days
                     anvil-memory-obs-promote-window-days))
         (since (- (anvil-memory-obs--now) (* window 24 60 60)))
         (rows (sqlite-select
                db
                "SELECT s.id, s.session_id, s.topic, s.summary, s.is_ai,
                        COALESCE(SUM(o.importance), 0) AS imp
                   FROM obs_summaries s
                   LEFT JOIN obs_observations o
                     ON o.session_id = s.session_id
                    AND o.id >= s.obs_start_id
                    AND o.id <= s.obs_end_id
                  WHERE s.ts >= ?
               GROUP BY s.id
                 HAVING imp >= ?
               ORDER BY imp DESC, s.ts DESC
                  LIMIT ?"
                (list since min-imp limit)))
         (rendered (mapcar
                    (lambda (r)
                      (list :summary-id (nth 0 r)
                            :session-id (nth 1 r)
                            :topic (nth 2 r)
                            :summary (nth 3 r)
                            :is-ai (and (nth 4 r) (= (nth 4 r) 1))
                            :total-importance (nth 5 r)))
                    rows)))
    (if (and check-against-memory
             (fboundp 'anvil-memory-save-check))
        (delq nil
              (mapcar
               (lambda (cand)
                 (let* ((subject (or (plist-get cand :topic) ""))
                        (body (or (plist-get cand :summary) ""))
                        (raw (ignore-errors
                               (funcall (intern "anvil-memory-save-check")
                                        subject body)))
                        ;; raw may be (:candidates list) or a list directly.
                        (matches (cond
                                  ((null raw) nil)
                                  ((and (listp raw) (plist-get raw :candidates))
                                   (plist-get raw :candidates))
                                  ((listp raw) raw)
                                  (t nil)))
                        (top (when matches
                               (apply #'max
                                      (mapcar
                                       (lambda (m)
                                         (or (plist-get m :similarity)
                                             0.0))
                                       matches)))))
                   (cond
                    ((and top
                          (>= top
                              anvil-memory-obs-promote-similar-threshold))
                     nil)
                    (t
                     (append cand (list :similar matches))))))
               rendered))
      rendered)))


;;;; --- Phase 6: promote action -------------------------------------------

(defcustom anvil-memory-obs-promote-target-dir nil
  "Override directory for newly promoted memory files.
When nil, uses the first writable directory from
`anvil-memory--effective-roots' (when anvil-memory is loaded)."
  :type '(choice (const :tag "Auto (anvil-memory roots)" nil)
                 directory)
  :group 'anvil-memory-obs)

(defcustom anvil-memory-obs-promote-default-type 'feedback
  "Default memory type assigned when `--promote' is called without one."
  :type '(choice (const feedback) (const project) (const reference)
                 (const user) (const memo))
  :group 'anvil-memory-obs)

(defun anvil-memory-obs--slug (text)
  "Build a kebab-case slug from TEXT, capped at 30 characters.
Falls back to `untitled' for empty / all-punctuation inputs."
  (let* ((lower (downcase (or text "")))
         (cleaned (replace-regexp-in-string "[^a-z0-9]+" "-" lower))
         (trimmed (string-trim cleaned "-+" "-+"))
         (capped (if (> (length trimmed) 30)
                     (substring trimmed 0 30)
                   trimmed)))
    (if (string-empty-p capped) "untitled" capped)))

(defun anvil-memory-obs--resolve-target-dir (override)
  "Pick the directory new memory files are written into.
OVERRIDE wins when non-nil; falls back to
`anvil-memory-obs-promote-target-dir', then the first writable
entry from `anvil-memory--effective-roots'."
  (cond
   ((and override (stringp override) (not (string-empty-p override)))
    (expand-file-name override))
   ((and (stringp anvil-memory-obs-promote-target-dir)
         (not (string-empty-p anvil-memory-obs-promote-target-dir)))
    (expand-file-name anvil-memory-obs-promote-target-dir))
   ((fboundp 'anvil-memory--effective-roots)
    (let* ((roots (funcall (intern "anvil-memory--effective-roots")))
           (writable (cl-remove-if-not
                      (lambda (d)
                        (and (stringp d)
                             (file-directory-p d)
                             (file-writable-p d)))
                      roots)))
      (or (car writable)
          (user-error
           "anvil-memory-obs-promote: no writable memory dir found"))))
   (t
    (user-error
     "anvil-memory-obs-promote: :target-dir required (anvil-memory not loaded)"))))

(defun anvil-memory-obs--render-frontmatter (name description type body)
  "Return a Markdown string with frontmatter + BODY for promote output."
  (format "---\nname: %s\ndescription: %s\ntype: %s\n---\n\n%s\n"
          (or name "untitled")
          (or description "")
          (symbol-name type)
          (or body "")))

(defun anvil-memory-obs--memory-index-path (target-dir)
  (expand-file-name "MEMORY.md" target-dir))

(defun anvil-memory-obs--update-memory-index
    (target-dir filename name description)
  "Append a one-line entry to MEMORY.md, creating it when absent.
Returns t when an entry was added, nil when FILENAME was already
referenced (idempotent)."
  (let* ((index (anvil-memory-obs--memory-index-path target-dir))
         (line (format "- [%s](%s) — %s\n"
                       (or name "untitled")
                       filename
                       (or description ""))))
    (cond
     ((and (file-exists-p index)
           (with-temp-buffer
             (let ((coding-system-for-read 'utf-8))
               (insert-file-contents index))
             (search-forward (format "(%s)" filename) nil t)))
      nil)
     (t
      (let ((coding-system-for-write 'utf-8))
        (with-temp-buffer
          (when (file-exists-p index)
            (let ((coding-system-for-read 'utf-8))
              (insert-file-contents index)))
          (goto-char (point-max))
          (unless (or (= (point-max) 1)
                      (eq (char-before) ?\n))
            (insert "\n"))
          (insert line)
          (write-region (point-min) (point-max) index nil 'silent)))
      t))))

(cl-defun anvil-memory-obs-promote
    (summary-id &key target-type target-dir name description)
  "Write a new auto-memory `.md' file from `obs_summaries.id = SUMMARY-ID'.

Keys:
  :TARGET-TYPE  symbol (`feedback' / `project' / `reference' / `user'
                / `memo'), default `anvil-memory-obs-promote-default-type'.
  :TARGET-DIR   override target directory.
  :NAME         override the memory `name'; defaults to the summary topic.
  :DESCRIPTION  one-liner for the MEMORY.md index; defaults to the
                summary first line capped at ~100 chars.

Returns plist (:file PATH :status `created' :index-updated BOOL).
Errors when SUMMARY-ID does not exist or the target file collides."
  (let* ((db (anvil-memory-obs--db))
         (row (car (sqlite-select
                    db
                    "SELECT topic, summary FROM obs_summaries WHERE id = ?"
                    (list summary-id)))))
    (unless row
      (user-error "anvil-memory-obs-promote: summary id %s not found"
                  summary-id))
    (let* ((topic (or name (nth 0 row) "untitled"))
           (body (or (nth 1 row) ""))
           (default-desc
            (let* ((firstline (car (split-string body "\n" t)))
                   (cap 100))
              (if (and firstline (> (length firstline) cap))
                  (concat (substring firstline 0 cap) "…")
                (or firstline ""))))
           (desc (or description default-desc))
           (type (or target-type
                     anvil-memory-obs-promote-default-type))
           (dir (anvil-memory-obs--resolve-target-dir target-dir))
           (filename (format "%s_%s.md"
                             (symbol-name type)
                             (anvil-memory-obs--slug topic)))
           (filepath (expand-file-name filename dir))
           (content (anvil-memory-obs--render-frontmatter
                     topic desc type body)))
      (when (file-exists-p filepath)
        (user-error "anvil-memory-obs-promote: %s already exists" filepath))
      (let ((coding-system-for-write 'utf-8))
        (with-temp-buffer
          (insert content)
          (write-region (point-min) (point-max) filepath nil 'silent)))
      (let ((index-updated
             (anvil-memory-obs--update-memory-index
              dir filename topic desc)))
        (list :file filepath
              :status 'created
              :index-updated index-updated)))))


;;;; --- Phase 3: MCP tool wrappers ----------------------------------------

(defun anvil-memory-obs--coerce-int (v default)
  "Coerce V to integer; falls back to DEFAULT on a non-numeric value."
  (cond
   ((integerp v) v)
   ((and (stringp v) (string-match-p "\\`-?[0-9]+\\'" v))
    (string-to-number v))
   (t default)))

(defun anvil-memory-obs--coerce-string (v)
  "Return V when it is a non-empty string, otherwise nil."
  (when (and (stringp v) (not (string-empty-p v)))
    v))

(defun anvil-memory-obs--coerce-id-list (v)
  "Coerce V to a list of integers.
V may be a list, a JSON-style array string, or a comma / space
separated string of digits.  Non-numeric tokens are dropped."
  (cond
   ((null v) nil)
   ((listp v)
    (delq nil (mapcar (lambda (e) (anvil-memory-obs--coerce-int e nil)) v)))
   ((stringp v)
    (let* ((trimmed (string-trim
                     (replace-regexp-in-string "[][]" "" v)))
           (tokens (split-string trimmed "[, ]+" t)))
      (delq nil
            (mapcar (lambda (s) (anvil-memory-obs--coerce-int s nil))
                    tokens))))
   (t nil)))

(defun anvil-memory-obs--tool-search (query &optional limit hook session-id)
  "Layer-1 FTS5 index search across observation bodies.

MCP Parameters:
  query      - FTS5 query string.  Empty / whitespace returns (:rows nil).
  limit      - Optional max row count (default 10, digit-string accepted).
  hook       - Optional hook filter (e.g. \"post-tool-use\").
  session-id - Optional session id filter.

Returns (:rows ROWS) ordered by FTS rank (best first)."
  (anvil-server-with-error-handling
   (list :rows
         (anvil-memory-obs-search
          query
          :limit (anvil-memory-obs--coerce-int limit 10)
          :hook (anvil-memory-obs--coerce-string hook)
          :session-id (anvil-memory-obs--coerce-string session-id)))))

(defun anvil-memory-obs--tool-timeline (anchor-id &optional window)
  "Layer-2 timeline window around an anchor observation.

MCP Parameters:
  anchor-id - Anchor observation id (integer or digit-string).
  window    - Optional ± window size (default 5, digit-string accepted).

Returns (:rows ROWS) ordered by id ASC.  Empty when ANCHOR-ID is unknown."
  (anvil-server-with-error-handling
   (list :rows
         (anvil-memory-obs-timeline
          (anvil-memory-obs--coerce-int anchor-id 0)
          :window (anvil-memory-obs--coerce-int window 5)))))

(defun anvil-memory-obs--tool-get (ids)
  "Layer-3 full-row fetch by id list.

MCP Parameters:
  ids - Observation ids.  Accepts a list of integers, a JSON-style
        \"[1,2,3]\" string, or a comma / space separated digit
        string.  Capped at `anvil-memory-obs-get-max-ids' (default 20).

Returns (:rows ROWS) ordered by id ASC."
  (anvil-server-with-error-handling
   (list :rows
         (anvil-memory-obs-get
          (anvil-memory-obs--coerce-id-list ids)))))

(defun anvil-memory-obs--coerce-bool (v)
  "Coerce V (nil / bool / string) to `t' / nil."
  (cond
   ((null v) nil)
   ((eq v t) t)
   ((stringp v)
    (not (member (downcase v) '("" "false" "nil" "0" "no" "off"))))
   (t t)))

(defun anvil-memory-obs--tool-promote-candidates
    (&optional limit min_importance check_against_memory window_days)
  "List promote candidates ranked by aggregate observation importance.

MCP Parameters:
  limit                - Optional max candidate count (default 5).
  min_importance       - Optional importance floor (default 30).
  check_against_memory - Optional truthy flag.  When set, each
                         candidate is scored against MEMORY.md via
                         `anvil-memory-save-check'; rows with a top
                         match at or above
                         `anvil-memory-obs-promote-similar-threshold'
                         are filtered out and the remaining rows
                         carry the match list as `:similar'.
  window_days          - Optional look-back days (default 30).

Returns (:rows ROWS)."
  (anvil-server-with-error-handling
   (list :rows
         (anvil-memory-obs-promote-candidates
          :limit (anvil-memory-obs--coerce-int limit 5)
          :min-importance
          (anvil-memory-obs--coerce-int min_importance nil)
          :check-against-memory
          (anvil-memory-obs--coerce-bool check_against_memory)
          :window-days
          (anvil-memory-obs--coerce-int window_days nil)))))

(defun anvil-memory-obs--tool-promote
    (summary_id &optional target_type target_dir name description)
  "Promote a compressed summary into a permanent auto-memory file.

MCP Parameters:
  summary_id  - obs_summaries.id (integer or digit-string).
  target_type - One of feedback / project / reference / user / memo
                (default `feedback').
  target_dir  - Optional override directory (default: first writable
                anvil-memory root).
  name        - Optional name override (default: summary topic).
  description - Optional one-line index description.

Returns (:file PATH :status STATUS :index-updated BOOL)."
  (anvil-server-with-error-handling
   (let* ((id (anvil-memory-obs--coerce-int summary_id 0))
          (typ (let ((s (anvil-memory-obs--coerce-string target_type)))
                 (when s (intern s)))))
     (anvil-memory-obs-promote
      id
      :target-type typ
      :target-dir (anvil-memory-obs--coerce-string target_dir)
      :name (anvil-memory-obs--coerce-string name)
      :description (anvil-memory-obs--coerce-string description)))))

(defun anvil-memory-obs--tool-summary-search (query &optional limit)
  "Topic-level FTS5 search over compressed session summaries.

MCP Parameters:
  query - FTS5 query string.  Empty / whitespace returns (:rows nil).
  limit - Optional max row count (default 10, digit-string accepted).

Returns (:rows ROWS) ordered by FTS rank.  Each row carries an
`:is-ai' boolean so callers can prefer AI-derived summaries."
  (anvil-server-with-error-handling
   (list :rows
         (anvil-memory-obs-summary-search
          query
          :limit (anvil-memory-obs--coerce-int limit 10)))))


(defconst anvil-memory-obs--tool-specs
  `((,(anvil-server-encode-handler #'anvil-memory-obs--tool-search)
     :id "memory-obs-search"
     :intent '(memory observation search)
     :layer 'index
     :description
     "Layer 1 — FTS5 index search across observation bodies.  Returns
capped previews (~80 chars per row) so the result fits comfortably in a
single tool response.  Use `memory-obs-timeline' for surrounding context
and `memory-obs-get' for full bodies."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory-obs--tool-timeline)
     :id "memory-obs-timeline"
     :intent '(memory observation context)
     :layer 'context
     :description
     "Layer 2 — return ±WINDOW observations around an anchor id within the
same session, ordered by id ASC.  Use this after `memory-obs-search' to
recover context around a hit."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory-obs--tool-get)
     :id "memory-obs-get"
     :intent '(memory observation read)
     :layer 'detail
     :description
     "Layer 3 — fetch full observation rows (body, payload, importance,
hook, tool name) for an explicit id list.  Capped at 20 ids per call."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory-obs--tool-summary-search)
     :id "memory-obs-summary-search"
     :intent '(memory observation search)
     :layer 'workflow
     :description
     "FTS5 search over compressed session summaries (topic + summary).
Each row carries an `:is-ai' boolean so callers can prefer AI-derived
summaries when available."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory-obs--tool-promote-candidates)
     :id "memory-obs-promote-candidates"
     :intent '(memory observation promote)
     :layer 'workflow
     :description
     "List recent session summaries ranked by aggregate observation
importance.  Optional `check_against_memory' adds a Jaccard dedup
pass against MEMORY.md so already-known patterns drop out, leaving
only candidates worth turning into permanent auto-memory entries."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory-obs--tool-promote)
     :id "memory-obs-promote"
     :intent '(memory observation promote write)
     :layer 'workflow
     :description
     "Write a new auto-memory `.md' file from a compressed summary id and
append a one-line entry to MEMORY.md.  This is the write-side of the
Phase 6 loop — call after `memory-obs-promote-candidates' to surface
the candidate id and confirm the user wants to keep it permanently."))
  "Spec list consumed by `anvil-server-register-tools'.")

(defun anvil-memory-obs--register-tools ()
  (anvil-server-register-tools anvil-memory-obs--server-id
                               anvil-memory-obs--tool-specs))

(defun anvil-memory-obs--unregister-tools ()
  (anvil-server-unregister-tools anvil-memory-obs--server-id
                                 anvil-memory-obs--tool-specs))


;;;; --- enable / disable --------------------------------------------------

;;;###autoload
(defun anvil-memory-obs-enable ()
  "Open the obs DB (when enabled) and register memory-obs-* MCP tools."
  (interactive)
  (when anvil-memory-obs-enabled
    (ignore (anvil-memory-obs--db)))
  (anvil-memory-obs--register-tools))

;;;###autoload
(defun anvil-memory-obs-disable ()
  "Unregister memory-obs-* MCP tools and close the obs DB."
  (interactive)
  (anvil-memory-obs--unregister-tools)
  (anvil-memory-obs--close))


(provide 'anvil-memory-obs)
;;; anvil-memory-obs.el ends here
