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

(defconst anvil-memory-obs-supported
  '(schema)
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
                       ts            INTEGER NOT NULL
                     )")
    (sqlite-execute db
                    "CREATE INDEX IF NOT EXISTS obs_summaries_session_idx
                       ON obs_summaries(session_id)")
    (sqlite-execute
     db
     (format "CREATE VIRTUAL TABLE IF NOT EXISTS obs_observations_fts
                USING fts5(body, content='obs_observations',
                           content_rowid='id'%s)"
             tok-sql))
    (sqlite-execute
     db
     (format "CREATE VIRTUAL TABLE IF NOT EXISTS obs_summaries_fts
                USING fts5(topic, summary, content='obs_summaries',
                           content_rowid='id'%s)"
             tok-sql))))


(provide 'anvil-memory-obs)
;;; anvil-memory-obs.el ends here
