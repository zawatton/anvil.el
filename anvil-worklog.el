;;; anvil-worklog.el --- AI worklog org → SQLite index  -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 42 Phase 1 — read-only scanner + FTS5 index for the per-machine
;; AI worklog files (`capture/ai-logs-<platform>-<hostname>-<year>.org').
;;
;; The org files are canonical and append-only.  This module rebuilds a
;; SQLite index from them so a Linux daemon can search MEMO entries
;; written from a Windows machine, query by machine / date / year, and
;; pull a single entry's body without re-parsing the org tree.
;;
;; Public Elisp API:
;;   (anvil-worklog-effective-db-path)
;;   (anvil-worklog-scan &optional ROOTS)            -> count plist
;;   (anvil-worklog-prune &optional ROOTS)           -> int
;;   (anvil-worklog-search QUERY &key …)             -> list of plists
;;   (anvil-worklog-list &key …)                     -> list of plists
;;   (anvil-worklog-get FILE START-LINE)             -> plist or nil
;;
;; MCP tools (emacs-eval server):
;;   worklog-scan / worklog-prune / worklog-search /
;;   worklog-list / worklog-get
;;
;; Schema is independent from `anvil-memory' — separate DB at
;; <user-emacs-directory>/anvil-worklog-index.db (or
;; <root>/.anvil-worklog/anvil-worklog-index.db when shared via
;; `anvil-worklog-shared-db-roots').

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-server)


;;;; --- group + defcustoms -------------------------------------------------

(defgroup anvil-worklog nil
  "AI worklog org → SQLite index for anvil."
  :group 'anvil
  :prefix "anvil-worklog-")

(defconst anvil-worklog--server-id "emacs-eval"
  "Server id under which worklog-* MCP tools register.")

(defcustom anvil-worklog-db-path
  (expand-file-name "anvil-worklog-index.db" user-emacs-directory)
  "SQLite file backing the worklog index.

When left at its default value, `anvil-worklog--resolve-db-path' may
redirect the open DB to a shared location declared in
`anvil-worklog-shared-db-roots' or the `ANVIL_WORKLOG_DB' env var.
Setting this option to a path other than the default disables the
auto-detect chain and pins the DB to that path."
  :type 'file
  :group 'anvil-worklog)

(defcustom anvil-worklog-shared-db-roots nil
  "Directory roots to search for a shared anvil-worklog DB.

Each entry is a directory.  When `anvil-worklog-db-path' has its
default value, `anvil-worklog--open' probes each root for
`.anvil-worklog/anvil-worklog-index.db' and uses the first match.
Point this at a notes-style git repo to share worklog index across
machines that pull the same repo (e.g. Linux + Windows).

Resolution order at open time:
  1. `ANVIL_WORKLOG_DB' env var (if set and non-empty)
  2. Explicit `anvil-worklog-db-path' (if customized away from default)
  3. First existing file under any `anvil-worklog-shared-db-roots' entry
  4. Default: `<user-emacs-directory>/anvil-worklog-index.db'"
  :type '(repeat directory)
  :group 'anvil-worklog)

(defcustom anvil-worklog-roots nil
  "List of directories to scan for `ai-logs-*-YYYY.org' files.
When nil, `anvil-worklog-scan' uses `anvil-worklog-default-roots'."
  :type '(choice (const :tag "Auto-detect" nil)
                 (repeat directory))
  :group 'anvil-worklog)

(defcustom anvil-worklog-default-roots
  (list (expand-file-name "~/Cowork/Notes/capture")
        (expand-file-name "~/Notes/capture"))
  "Fallback directories scanned when `anvil-worklog-roots' is nil.
Non-existent directories are silently skipped."
  :type '(repeat directory)
  :group 'anvil-worklog)

(defcustom anvil-worklog-fts-tokenizer 'auto
  "Tokenizer used when (re-)creating the `worklog_fts' table.
`auto' picks `trigram' on SQLite 3.34+, else falls back to `unicode61'."
  :type '(choice (const :tag "Auto (trigram if available)" auto)
                 (const :tag "Trigram (SQLite 3.34+)" trigram)
                 (const :tag "unicode61 (default)" unicode61))
  :group 'anvil-worklog)

(defconst anvil-worklog-supported
  '(scan prune search list get add export)
  "Capability tags this module currently provides.
Tests gate `skip-unless' on membership here so a half-shipped
feature never breaks CI.")

(defconst anvil-worklog--synthetic-prefix "anvil-worklog:db:"
  "Sentinel prefix that marks DB-direct entries (Phase 2).
The `file' column for these rows is a synthetic identifier of the
shape `anvil-worklog:db:<machine>:<year>' so they coexist with
org-scanned rows under the same (file, start_line) primary key.
`anvil-worklog-prune' skips rows whose file starts with this prefix
because there is no on-disk file to check existence against.")

(defun anvil-worklog--synthetic-file-p (path)
  "Return non-nil when PATH is a Phase 2 DB-direct synthetic file id."
  (and (stringp path)
       (string-prefix-p anvil-worklog--synthetic-prefix path)))


;;;; --- sqlite backend ----------------------------------------------------

(defvar anvil-worklog--db nil
  "Open handle on the resolved DB path (nil when the module is idle).")

(defvar anvil-worklog--resolved-db-path nil
  "Cached path returned by `anvil-worklog--resolve-db-path' on first open.
Reset by `anvil-worklog--close' so a re-open re-runs detection.")

(defun anvil-worklog--default-db-path ()
  "Return the un-redirected default DB path."
  (expand-file-name "anvil-worklog-index.db" user-emacs-directory))

(defun anvil-worklog--shared-db-candidate (root)
  "Return `<ROOT>/.anvil-worklog/anvil-worklog-index.db' when it exists."
  (let ((cand (expand-file-name
               ".anvil-worklog/anvil-worklog-index.db"
               (expand-file-name root))))
    (and (file-exists-p cand) cand)))

(defun anvil-worklog--resolve-db-path ()
  "Return the effective DB path per the documented override order."
  (let ((env (getenv "ANVIL_WORKLOG_DB"))
        (default (anvil-worklog--default-db-path)))
    (cond
     ((and env (not (string-empty-p env)))
      (expand-file-name env))
     ((not (equal anvil-worklog-db-path default))
      anvil-worklog-db-path)
     (t
      (or (cl-some #'anvil-worklog--shared-db-candidate
                   anvil-worklog-shared-db-roots)
          default)))))

(defun anvil-worklog-effective-db-path ()
  "Return the path the DB is currently (or would be) opened at."
  (or anvil-worklog--resolved-db-path
      (anvil-worklog--resolve-db-path)))

(defun anvil-worklog--require-sqlite ()
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error
     "anvil-worklog: Emacs SQLite backend unavailable (needs Emacs 29+)")))

(defun anvil-worklog--sqlite-supports-trigram-p (db)
  "Return non-nil when DB's SQLite build ships the FTS5 trigram tokenizer."
  (condition-case nil
      (progn
        (sqlite-execute
         db
         "CREATE VIRTUAL TABLE anvil_worklog_trigram_probe
            USING fts5(x, tokenize=trigram)")
        (sqlite-execute db "DROP TABLE anvil_worklog_trigram_probe")
        t)
    (error nil)))

(defun anvil-worklog--resolve-tokenizer (db)
  "Return the tokenizer symbol DB should use."
  (pcase anvil-worklog-fts-tokenizer
    ('trigram 'trigram)
    ('unicode61 'unicode61)
    ('auto (if (anvil-worklog--sqlite-supports-trigram-p db)
               'trigram
             'unicode61))
    (other (user-error
            "anvil-worklog: invalid `anvil-worklog-fts-tokenizer': %S"
            other))))

(defun anvil-worklog--create-fts-table (db tokenizer)
  "Create `worklog_fts' on DB using TOKENIZER (symbol)."
  (let ((tok-sql (pcase tokenizer
                   ('trigram   ", tokenize='trigram'")
                   ('unicode61 "")
                   (_ (user-error
                       "anvil-worklog: invalid tokenizer: %S" tokenizer)))))
    (sqlite-execute
     db
     (format "CREATE VIRTUAL TABLE IF NOT EXISTS worklog_fts
                USING fts5(file UNINDEXED, start_line UNINDEXED,
                           machine UNINDEXED, date UNINDEXED,
                           title, body%s)"
             tok-sql))))

(defun anvil-worklog--ensure-schema ()
  (sqlite-execute anvil-worklog--db
                  "CREATE TABLE IF NOT EXISTS worklog_entry (
                     file        TEXT    NOT NULL,
                     start_line  INTEGER NOT NULL,
                     end_line    INTEGER NOT NULL,
                     machine     TEXT    NOT NULL,
                     year        INTEGER NOT NULL,
                     date        TEXT    NOT NULL,
                     title       TEXT    NOT NULL,
                     body        TEXT    NOT NULL,
                     digest      TEXT    NOT NULL,
                     scanned_at  INTEGER NOT NULL,
                     PRIMARY KEY (file, start_line)
                   )")
  (sqlite-execute anvil-worklog--db
                  "CREATE INDEX IF NOT EXISTS worklog_entry_date_idx
                     ON worklog_entry(date)")
  (sqlite-execute anvil-worklog--db
                  "CREATE INDEX IF NOT EXISTS worklog_entry_machine_idx
                     ON worklog_entry(machine)")
  (anvil-worklog--create-fts-table
   anvil-worklog--db
   (anvil-worklog--resolve-tokenizer anvil-worklog--db)))

(defun anvil-worklog--open ()
  "Open the backing DB and apply schema."
  (anvil-worklog--require-sqlite)
  (unless anvil-worklog--db
    (let ((path (anvil-worklog--resolve-db-path)))
      (setq anvil-worklog--resolved-db-path path)
      (make-directory (file-name-directory path) t)
      (setq anvil-worklog--db (sqlite-open path))
      (anvil-worklog--ensure-schema)))
  anvil-worklog--db)

(defun anvil-worklog--close ()
  (when anvil-worklog--db
    (ignore-errors (sqlite-close anvil-worklog--db))
    (setq anvil-worklog--db nil))
  (setq anvil-worklog--resolved-db-path nil))

(defun anvil-worklog--db ()
  (or anvil-worklog--db (anvil-worklog--open)))


;;;; --- parsing ----------------------------------------------------------

(defconst anvil-worklog--filename-re
  "\\`ai-logs-\\(.+\\)-\\([0-9]\\{4\\}\\)\\.org\\'"
  "Regex matching `ai-logs-<machine>-<year>.org' filenames.
Capture 1 = machine token (may contain dashes), capture 2 = year.")

(defconst anvil-worklog--memo-heading-re
  "^\\*\\*\\* MEMO AI:[ \t]*\\(.*?\\)[ \t]*$"
  "Regex matching a level-3 `MEMO AI:' heading.  Capture 1 = the
trailing portion (title plus any trailing inactive timestamp).")

(defconst anvil-worklog--note-heading-re
  "^\\*\\* NOTE 作業ログ[ \t]+<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
  "Regex matching a level-2 `NOTE 作業ログ <DATE>' heading.  Capture 1 = ISO date.")

(defconst anvil-worklog--inactive-ts-re
  "[ \t]*<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\(?:[ \t][^>]*\\)?>[ \t]*\\'"
  "Regex matching a trailing inactive timestamp on a heading line.
Capture 1 = ISO date.")

(defconst anvil-worklog--non-deeper-re
  "^\\*\\{1,3\\}[ \t]"
  "Regex matching any heading at level <=3 (used to find entry end).")

(defun anvil-worklog--parse-machine-year (filename)
  "Return (MACHINE . YEAR) extracted from FILENAME, or nil."
  (when (string-match anvil-worklog--filename-re filename)
    (cons (match-string 1 filename)
          (string-to-number (match-string 2 filename)))))

(defun anvil-worklog--split-title-date (raw)
  "Split RAW (heading tail after `MEMO AI:') into (TITLE . DATE-OR-NIL).
TITLE is trimmed; DATE is the ISO date from a trailing inactive
timestamp, or nil if none was present."
  (let ((s (string-trim raw)))
    (if (string-match anvil-worklog--inactive-ts-re s)
        (let ((date (match-string 1 s))
              (title (string-trim (substring s 0 (match-beginning 0)))))
          (cons title date))
      (cons s nil))))

(defun anvil-worklog--read-file-utf8 (path)
  "Return the UTF-8 decoded body of PATH as a string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun anvil-worklog--digest (s)
  "Return a sha1 hexdigest of string S."
  (secure-hash 'sha1 s))

(defun anvil-worklog--parse-file (path)
  "Parse PATH and return a list of entry plists.
Each plist has :file :start-line :end-line :machine :year :date
:title :body :digest.  Files whose name does not match
`anvil-worklog--filename-re' return nil."
  (let* ((base (file-name-nondirectory path))
         (my (anvil-worklog--parse-machine-year base)))
    (when my
      (let ((machine (car my))
            (year (cdr my))
            entries)
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8))
            (insert-file-contents path))
          (goto-char (point-min))
          (let ((parent-date nil))
            (while (not (eobp))
              (cond
               ((looking-at anvil-worklog--note-heading-re)
                (setq parent-date (match-string 1)))
               ((looking-at anvil-worklog--memo-heading-re)
                (let* ((start-line (line-number-at-pos))
                       (raw-tail (match-string 1))
                       (split (anvil-worklog--split-title-date raw-tail))
                       (title (car split))
                       (date (or (cdr split) parent-date))
                       (body-start (line-beginning-position 2))
                       (end-pos (save-excursion
                                  (forward-line 1)
                                  (if (re-search-forward
                                       anvil-worklog--non-deeper-re
                                       nil t)
                                      (line-beginning-position)
                                    (point-max))))
                       (end-line (save-excursion
                                   (goto-char end-pos)
                                   (line-number-at-pos)))
                       (body (buffer-substring-no-properties
                              body-start end-pos))
                       (body (replace-regexp-in-string
                              "[ \t\n]+\\'" "" body))
                       (digest (anvil-worklog--digest body)))
                  (when date
                    (push (list :file path
                                :start-line start-line
                                :end-line end-line
                                :machine machine
                                :year year
                                :date date
                                :title title
                                :body body
                                :digest digest)
                          entries)))))
              (forward-line 1))))
        (nreverse entries)))))


;;;; --- scan / prune -----------------------------------------------------

(defun anvil-worklog--effective-roots ()
  "Return the directory list to scan for ai-logs files.
Roots whose `file-truename' resolves to a path already seen are
dropped so symlink layouts (e.g. ~/Notes → ~/Cowork/Notes) do not
double-index every entry under both surface paths."
  (let ((seen (make-hash-table :test 'equal))
        result)
    (dolist (root (or anvil-worklog-roots anvil-worklog-default-roots))
      (when (file-directory-p root)
        (let ((canonical (file-truename (expand-file-name root))))
          (unless (gethash canonical seen)
            (puthash canonical t seen)
            (push root result)))))
    (nreverse result)))

(defun anvil-worklog--list-files (roots)
  "Return the list of `ai-logs-*-YYYY.org' paths under ROOTS.
Files whose `file-truename' has already been emitted are skipped
so a symlink chain pointing at the same backing org never produces
two index rows for one entry."
  (let ((seen (make-hash-table :test 'equal))
        acc)
    (dolist (root roots)
      (when (file-directory-p root)
        (dolist (path (directory-files root t "\\`ai-logs-.+-[0-9]\\{4\\}\\.org\\'"))
          (let ((canonical (file-truename path)))
            (unless (gethash canonical seen)
              (puthash canonical t seen)
              (push path acc))))))
    (nreverse acc)))

(defun anvil-worklog--upsert-entry (db entry now)
  "Upsert ENTRY into DB.  Returns one of `inserted' / `updated' /
`unchanged'."
  (let* ((file (plist-get entry :file))
         (start-line (plist-get entry :start-line))
         (digest (plist-get entry :digest))
         (existing (sqlite-select
                    db
                    "SELECT digest FROM worklog_entry
                       WHERE file = ?1 AND start_line = ?2"
                    (list file start-line))))
    (cond
     ((null existing)
      (sqlite-execute
       db
       "INSERT INTO worklog_entry
          (file, start_line, end_line, machine, year, date,
           title, body, digest, scanned_at)
          VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10)"
       (list file start-line
             (plist-get entry :end-line)
             (plist-get entry :machine)
             (plist-get entry :year)
             (plist-get entry :date)
             (plist-get entry :title)
             (plist-get entry :body)
             digest
             now))
      (sqlite-execute
       db
       "INSERT INTO worklog_fts
          (file, start_line, machine, date, title, body)
          VALUES (?1, ?2, ?3, ?4, ?5, ?6)"
       (list file start-line
             (plist-get entry :machine)
             (plist-get entry :date)
             (plist-get entry :title)
             (plist-get entry :body)))
      'inserted)
     ((equal (caar existing) digest)
      'unchanged)
     (t
      (sqlite-execute
       db
       "UPDATE worklog_entry
          SET end_line = ?3, machine = ?4, year = ?5, date = ?6,
              title = ?7, body = ?8, digest = ?9, scanned_at = ?10
          WHERE file = ?1 AND start_line = ?2"
       (list file start-line
             (plist-get entry :end-line)
             (plist-get entry :machine)
             (plist-get entry :year)
             (plist-get entry :date)
             (plist-get entry :title)
             (plist-get entry :body)
             digest
             now))
      (sqlite-execute
       db
       "DELETE FROM worklog_fts
          WHERE file = ?1 AND start_line = ?2"
       (list file start-line))
      (sqlite-execute
       db
       "INSERT INTO worklog_fts
          (file, start_line, machine, date, title, body)
          VALUES (?1, ?2, ?3, ?4, ?5, ?6)"
       (list file start-line
             (plist-get entry :machine)
             (plist-get entry :date)
             (plist-get entry :title)
             (plist-get entry :body)))
      'updated))))

(defun anvil-worklog--prune-stale-entries (db file current-start-lines)
  "Remove DB rows for FILE whose start_line is no longer in
CURRENT-START-LINES (a list of integers).  Keeps FTS in sync.
Returns the number of rows deleted."
  (let* ((existing (sqlite-select
                    db
                    "SELECT start_line FROM worklog_entry WHERE file = ?1"
                    (list file)))
         (n 0))
    (dolist (row existing)
      (let ((sl (car row)))
        (unless (memq sl current-start-lines)
          (sqlite-execute
           db
           "DELETE FROM worklog_entry
              WHERE file = ?1 AND start_line = ?2"
           (list file sl))
          (sqlite-execute
           db
           "DELETE FROM worklog_fts
              WHERE file = ?1 AND start_line = ?2"
           (list file sl))
          (cl-incf n))))
    n))

;;;###autoload
(defun anvil-worklog-scan (&optional roots)
  "Scan ROOTS (or `anvil-worklog--effective-roots') for `ai-logs-*'
org files and (re-)populate the worklog index.  Returns a plist
=(:files N :entries M :inserted I :updated U :unchanged S :deleted D)=."
  (let* ((roots* (or roots (anvil-worklog--effective-roots)))
         (db (anvil-worklog--db))
         (files (anvil-worklog--list-files roots*))
         (now (truncate (float-time)))
         (n-files 0) (n-entries 0)
         (n-ins 0) (n-upd 0) (n-unch 0) (n-del 0))
    (dolist (path files)
      (cl-incf n-files)
      (let* ((entries (anvil-worklog--parse-file path))
             (current-sls (mapcar (lambda (e) (plist-get e :start-line))
                                  entries)))
        (dolist (entry entries)
          (cl-incf n-entries)
          (pcase (anvil-worklog--upsert-entry db entry now)
            ('inserted  (cl-incf n-ins))
            ('updated   (cl-incf n-upd))
            ('unchanged (cl-incf n-unch))))
        (cl-incf n-del
                 (anvil-worklog--prune-stale-entries db path current-sls))))
    (list :files n-files
          :entries n-entries
          :inserted n-ins
          :updated n-upd
          :unchanged n-unch
          :deleted n-del)))

;;;###autoload
(defun anvil-worklog-prune (&optional roots)
  "Remove worklog rows whose backing org file no longer exists.
When ROOTS is non-nil, only rows whose file lives under one of those
directories are inspected.  Keeps FTS in sync.  Returns the number
of rows deleted.

Phase 2: rows whose `file' starts with `anvil-worklog--synthetic-prefix'
are skipped because they are DB-direct entries with no on-disk
counterpart — pruning them by missing-file check would erase every
worklog-add row."
  (let* ((db (anvil-worklog--db))
         (rows (sqlite-select
                db
                "SELECT DISTINCT file FROM worklog_entry"))
         (roots* (mapcar (lambda (r)
                           (file-name-as-directory (expand-file-name r)))
                         roots))
         (n 0))
    (dolist (row rows)
      (let* ((path (car row))
             (path-abs (expand-file-name path))
             (in-scope (or (null roots*)
                           (cl-some (lambda (r)
                                      (string-prefix-p r path-abs))
                                    roots*))))
        (when (and in-scope
                   (not (anvil-worklog--synthetic-file-p path))
                   (not (file-exists-p path)))
          (let ((deleted (sqlite-select
                          db
                          "SELECT COUNT(*) FROM worklog_entry WHERE file = ?1"
                          (list path))))
            (cl-incf n (or (caar deleted) 0)))
          (sqlite-execute
           db
           "DELETE FROM worklog_entry WHERE file = ?1"
           (list path))
          (sqlite-execute
           db
           "DELETE FROM worklog_fts WHERE file = ?1"
           (list path)))))
    n))


;;;; --- DB-direct add (Phase 2) ----------------------------------------

(defcustom anvil-worklog-platform
  (cond ((memq system-type '(gnu/linux gnu)) "linux")
        ((memq system-type '(darwin)) "macos")
        ((memq system-type '(windows-nt cygwin ms-dos)) "windows")
        (t (symbol-name system-type)))
  "Platform token for the current machine, used by `anvil-worklog-add'.
Mirrors the convention in CLAUDE.md (linux / macos / windows)."
  :type 'string
  :group 'anvil-worklog)

(defcustom anvil-worklog-hostname nil
  "Host token for the current machine, or nil to derive from `system-name'.
When nil, the leading component of `system-name' (everything before
the first dot) is used."
  :type '(choice (const :tag "Auto (system-name)" nil) string)
  :group 'anvil-worklog)

(defun anvil-worklog--current-hostname ()
  "Return the configured or auto-derived hostname token."
  (or anvil-worklog-hostname
      (let ((sn (system-name)))
        (if (string-match "\\`\\([^.]+\\)" sn)
            (match-string 1 sn)
          sn))))

(defun anvil-worklog--current-machine-token ()
  "Return the `<platform>-<host>' token used as default for new entries."
  (format "%s-%s" anvil-worklog-platform (anvil-worklog--current-hostname)))

(defun anvil-worklog--today-iso ()
  "Return today's date in ISO `YYYY-MM-DD' (local time)."
  (format-time-string "%Y-%m-%d"))

(defun anvil-worklog--year-from-date (date)
  "Return the integer year parsed from ISO DATE string (YYYY-MM-DD)."
  (and (stringp date)
       (string-match "\\`\\([0-9]\\{4\\}\\)" date)
       (string-to-number (match-string 1 date))))

(defun anvil-worklog--synthetic-file (machine year)
  "Return the synthetic file id for (MACHINE, YEAR) DB-direct rows."
  (format "%s%s:%d" anvil-worklog--synthetic-prefix machine year))

(defun anvil-worklog--next-start-line (db file)
  "Return the next auto-increment start_line for FILE in DB."
  (let ((rows (sqlite-select
               db
               "SELECT COALESCE(MAX(start_line), 0) + 1
                  FROM worklog_entry
                  WHERE file = ?1"
               (list file))))
    (or (caar rows) 1)))

;;;###autoload
(cl-defun anvil-worklog-add (title body &key date machine year)
  "Insert a DB-direct worklog entry and return its identity plist.

TITLE / BODY are required strings.  Optional keys:
  :date    — ISO `YYYY-MM-DD' (default: today)
  :machine — `<platform>-<host>' token (default: current machine)
  :year    — integer (default: derived from DATE)

The row is stored with a synthetic file id of the shape
`anvil-worklog:db:<machine>:<year>' and a start_line that
auto-increments within that namespace, so DB-direct rows coexist with
Phase 1 org-scanned rows under the existing primary key.

Returns =(:file SYNTHETIC :start-line N :machine M :year Y :date D
:digest SHA1)=."
  (unless (and (stringp title) (not (string-empty-p title)))
    (user-error "anvil-worklog-add: TITLE must be a non-empty string"))
  (unless (stringp body)
    (user-error "anvil-worklog-add: BODY must be a string"))
  (let* ((db (anvil-worklog--db))
         (date* (or date (anvil-worklog--today-iso)))
         (machine* (or machine (anvil-worklog--current-machine-token)))
         (year* (or year (anvil-worklog--year-from-date date*)
                    (string-to-number (format-time-string "%Y"))))
         (file (anvil-worklog--synthetic-file machine* year*))
         (start-line (anvil-worklog--next-start-line db file))
         (body-trimmed (replace-regexp-in-string "[ \t\n]+\\'" "" body))
         (digest (anvil-worklog--digest body-trimmed))
         (now (truncate (float-time))))
    (sqlite-execute
     db
     "INSERT INTO worklog_entry
        (file, start_line, end_line, machine, year, date,
         title, body, digest, scanned_at)
        VALUES (?1, ?2, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)"
     (list file start-line machine* year* date*
           title body-trimmed digest now))
    (sqlite-execute
     db
     "INSERT INTO worklog_fts
        (file, start_line, machine, date, title, body)
        VALUES (?1, ?2, ?3, ?4, ?5, ?6)"
     (list file start-line machine* date* title body-trimmed))
    (list :file file
          :start-line start-line
          :machine machine*
          :year year*
          :date date*
          :digest digest)))


;;;; --- search / list / get ---------------------------------------------

(defun anvil-worklog--row->plist (row)
  "Convert a 9-column worklog_entry ROW into a plist."
  (list :file (nth 0 row)
        :start-line (nth 1 row)
        :end-line (nth 2 row)
        :machine (nth 3 row)
        :year (nth 4 row)
        :date (nth 5 row)
        :title (nth 6 row)
        :body (nth 7 row)
        :digest (nth 8 row)))

(defun anvil-worklog--apply-filters (where args machine since until year)
  "Append common filter clauses to WHERE/ARGS lists; return updated cons."
  (when (and machine (not (string-empty-p machine)))
    (push "machine = ?" where)
    (push machine args))
  (when (and since (not (string-empty-p since)))
    (push "date >= ?" where)
    (push since args))
  (when (and until (not (string-empty-p until)))
    (push "date <= ?" where)
    (push until args))
  (when year
    (push "year = ?" where)
    (push year args))
  (cons where args))

(cl-defun anvil-worklog-list (&key limit machine since until year)
  "Return up to LIMIT worklog entries filtered by MACHINE / SINCE /
UNTIL / YEAR, ordered by date DESC then start_line ASC.  Default
LIMIT = 20."
  (let* ((db (anvil-worklog--db))
         (where '())
         (args '())
         (filtered (anvil-worklog--apply-filters where args
                                                 machine since until year))
         (where (car filtered))
         (args (cdr filtered))
         (lim (or limit 20))
         (sql (concat
               "SELECT file, start_line, end_line, machine, year,
                       date, title, body, digest
                  FROM worklog_entry"
               (if where
                   (concat " WHERE " (mapconcat #'identity (nreverse where) " AND "))
                 "")
               " ORDER BY date DESC, start_line ASC LIMIT "
               (number-to-string lim)))
         (rows (apply #'sqlite-select db sql
                      (if args (list (nreverse args)) nil))))
    (mapcar #'anvil-worklog--row->plist rows)))

(cl-defun anvil-worklog-search (query &key limit machine since until year)
  "FTS5 search the worklog index.  Returns up to LIMIT entries
ordered by FTS rank (best first).  Empty / whitespace QUERY returns nil."
  (let* ((q (and (stringp query) (string-trim query)))
         (db (anvil-worklog--db)))
    (when (and q (not (string-empty-p q)))
      (let* ((where (list "worklog_fts MATCH ?"))
             (args (list q))
             (filtered (anvil-worklog--apply-filters where args
                                                     machine since until year))
             (where (car filtered))
             (args (cdr filtered))
             (lim (or limit 20))
             ;; Subquery to surface (file, start_line) keys, then JOIN
             ;; back to worklog_entry for the full row.  Rank ordering
             ;; is preserved via the LIMIT on the inner query.
             (sql (concat
                   "SELECT e.file, e.start_line, e.end_line, e.machine,
                           e.year, e.date, e.title, e.body, e.digest
                      FROM worklog_entry e
                      JOIN (SELECT file, start_line, rank
                              FROM worklog_fts
                              WHERE " (mapconcat #'identity (nreverse where) " AND ")
                   "      ORDER BY rank LIMIT " (number-to-string lim) ") f
                        ON f.file = e.file AND f.start_line = e.start_line
                      ORDER BY f.rank"))
             (rows (apply #'sqlite-select db sql
                          (if args (list (nreverse args)) nil))))
        (mapcar #'anvil-worklog--row->plist rows)))))

(defun anvil-worklog-get (file start-line)
  "Return the worklog entry plist for (FILE, START-LINE) or nil."
  (let* ((db (anvil-worklog--db))
         (rows (sqlite-select
                db
                "SELECT file, start_line, end_line, machine, year,
                        date, title, body, digest
                   FROM worklog_entry
                   WHERE file = ?1 AND start_line = ?2"
                (list file start-line))))
    (and rows (anvil-worklog--row->plist (car rows)))))

(defun anvil-worklog-get-by-digest (digest)
  "Return the worklog entry plist whose body sha1 equals DIGEST, or nil.
When DIGEST matches more than one row (would only happen if two
entries have byte-identical bodies in distinct (machine, year)
namespaces) the first match is returned — callers wanting full
disambiguation should fall back to `anvil-worklog-list'."
  (when (and (stringp digest) (not (string-empty-p digest)))
    (let* ((db (anvil-worklog--db))
           (rows (sqlite-select
                  db
                  "SELECT file, start_line, end_line, machine, year,
                          date, title, body, digest
                     FROM worklog_entry
                     WHERE digest = ?1 LIMIT 1"
                  (list digest))))
      (and rows (anvil-worklog--row->plist (car rows))))))


;;;; --- DB → org export (Phase 2, optional) ---------------------------

(defun anvil-worklog--default-export-path (machine year)
  "Return the conventional `capture/ai-logs-<MACHINE>-<YEAR>.org' path.
The returned path lives under the first existing directory in
`anvil-worklog-default-roots'."
  (let ((dir (cl-some (lambda (root)
                        (and (file-directory-p root) root))
                      anvil-worklog-default-roots)))
    (and dir
         (expand-file-name
          (format "ai-logs-%s-%d.org" machine year)
          dir))))

(defun anvil-worklog--render-org (rows machine year)
  "Render ROWS (list of plists, sorted ASC by date) as an org buffer
string for (MACHINE, YEAR).  Inserts `** NOTE 作業ログ <DATE>'
section headers when the date changes, and one `*** MEMO AI:'
heading per row."
  (let ((header (format "#+title: AI 作業ログ %d (%s)\n#+date: %d-01-01\n#+startup: overview\n\nClaude による作業ログ (anvil-worklog DB → org export)。\n"
                        year machine year))
        (current-date nil)
        (buf (generate-new-buffer " *anvil-worklog-export*")))
    (unwind-protect
        (with-current-buffer buf
          (insert header)
          (dolist (row rows)
            (let ((date (plist-get row :date))
                  (title (plist-get row :title))
                  (body (plist-get row :body)))
              (unless (equal date current-date)
                (insert (format "\n** NOTE 作業ログ <%s>\n" date))
                (setq current-date date))
              (insert (format "*** MEMO AI: %s <%s>\n" title date))
              (when (and (stringp body) (not (string-empty-p body)))
                (insert body)
                (unless (string-suffix-p "\n" body)
                  (insert "\n")))))
          (buffer-substring-no-properties (point-min) (point-max)))
      (kill-buffer buf))))

;;;###autoload
(cl-defun anvil-worklog-export-org (&key path machine year since until)
  "Export worklog entries to an org file (DB → org, one-way).

MACHINE and YEAR are required; SINCE / UNTIL clip by date.  PATH
defaults to the conventional
`capture/ai-logs-<MACHINE>-<YEAR>.org' under the first existing
`anvil-worklog-default-roots' directory.

Existing PATH is overwritten — this is a snapshot, not an append.
Returns =(:path PATH :entries N)=."
  (unless machine
    (user-error "anvil-worklog-export-org: :machine is required"))
  (unless year
    (user-error "anvil-worklog-export-org: :year is required"))
  (let* ((db (anvil-worklog--db))
         (where (list "machine = ?" "year = ?"))
         (args (list machine year)))
    (when (and since (not (string-empty-p since)))
      (push "date >= ?" where) (push since args))
    (when (and until (not (string-empty-p until)))
      (push "date <= ?" where) (push until args))
    (let* ((sql (concat
                 "SELECT file, start_line, end_line, machine, year,
                         date, title, body, digest
                    FROM worklog_entry
                    WHERE " (mapconcat #'identity (nreverse where) " AND ")
                 " ORDER BY date ASC, start_line ASC"))
           (rows (mapcar #'anvil-worklog--row->plist
                         (sqlite-select db sql (nreverse args))))
           (resolved-path (or path
                              (anvil-worklog--default-export-path machine year)))
           (content (anvil-worklog--render-org rows machine year)))
      (unless resolved-path
        (user-error "anvil-worklog-export-org: no PATH and no default root exists"))
      (make-directory (file-name-directory resolved-path) t)
      (with-temp-file resolved-path
        (let ((coding-system-for-write 'utf-8))
          (insert content)))
      (list :path resolved-path :entries (length rows)))))


;;;; --- MCP wrappers ----------------------------------------------------

(defun anvil-worklog--coerce-int (v default)
  "Coerce V (integer / digit-string / nil) to an integer, else DEFAULT."
  (cond ((integerp v) v)
        ((and (stringp v) (string-match "\\`[0-9]+\\'" v))
         (string-to-number v))
        (t default)))

(defun anvil-worklog--coerce-string (v)
  "Return V as a non-empty string, or nil."
  (cond ((null v) nil)
        ((and (stringp v) (string-empty-p v)) nil)
        ((stringp v) v)
        (t nil)))

(defun anvil-worklog--coerce-roots (v)
  "Coerce V (nil / list / colon-separated string) to a roots list or nil."
  (cond ((null v) nil)
        ((listp v) v)
        ((and (stringp v) (string-empty-p v)) nil)
        ((stringp v) (split-string v ":" t))
        (t nil)))

(defun anvil-worklog--tool-scan (&optional roots)
  "(Re-)populate the worklog index from `ai-logs-*-YYYY.org' files.

MCP Parameters:
  roots - Optional colon-separated directory list.  Empty / omitted
          delegates to `anvil-worklog-default-roots'.

Returns =(:files N :entries M :inserted I :updated U :unchanged S
:deleted D)=."
  (anvil-server-with-error-handling
   (anvil-worklog-scan (anvil-worklog--coerce-roots roots))))

(defun anvil-worklog--tool-prune (&optional roots)
  "Drop worklog rows whose backing org file is missing.

MCP Parameters:
  roots - Optional colon-separated directory list.  When set, only
          rows under those roots are inspected.

Returns =(:pruned N :roots DIRS-OR-ALL)=."
  (anvil-server-with-error-handling
   (let* ((roots* (anvil-worklog--coerce-roots roots))
          (n (anvil-worklog-prune roots*)))
     (list :pruned n :roots (or roots* 'all)))))

(defun anvil-worklog--tool-search (query &optional limit machine since until year)
  "Full-text search the worklog index.

MCP Parameters:
  query   - FTS5 query string.  Empty / whitespace returns (:rows nil).
  limit   - Optional max result count (default 20).
  machine - Optional machine token equality filter (e.g. linux-debian).
  since   - Optional ISO date (inclusive lower bound).
  until   - Optional ISO date (inclusive upper bound).
  year    - Optional 4-digit year filter (digit-string accepted).

Returns =(:rows ROWS)=."
  (anvil-server-with-error-handling
   (list :rows
         (anvil-worklog-search
          (or query "")
          :limit (anvil-worklog--coerce-int limit 20)
          :machine (anvil-worklog--coerce-string machine)
          :since (anvil-worklog--coerce-string since)
          :until (anvil-worklog--coerce-string until)
          :year (anvil-worklog--coerce-int year nil)))))

(defun anvil-worklog--tool-list (&optional limit machine since until year)
  "List worklog entries ordered by date DESC.

MCP Parameters:
  limit   - Optional max result count (default 20).
  machine - Optional machine token equality filter.
  since   - Optional ISO date (inclusive lower bound).
  until   - Optional ISO date (inclusive upper bound).
  year    - Optional 4-digit year filter.

Returns =(:rows ROWS)=."
  (anvil-server-with-error-handling
   (list :rows
         (anvil-worklog-list
          :limit (anvil-worklog--coerce-int limit 20)
          :machine (anvil-worklog--coerce-string machine)
          :since (anvil-worklog--coerce-string since)
          :until (anvil-worklog--coerce-string until)
          :year (anvil-worklog--coerce-int year nil)))))

(defun anvil-worklog--tool-get (&optional file start_line digest)
  "Return one worklog entry by primary key OR digest.

MCP Parameters:
  file       - Optional absolute path to the source
               `ai-logs-*-YYYY.org' file.  Pair with `start_line'
               for the natural (file, start_line) primary key.
               For DB-direct rows pass the synthetic id
               `anvil-worklog:db:<machine>:<year>'.
  start_line - Optional 1-indexed line of the `*** MEMO AI:'
               heading.  Required when `file' is given.
  digest     - Optional sha1 hex of the entry body.  When set,
               `file' / `start_line' are ignored and a digest
               lookup is performed instead — convenient when the
               caller only kept the digest from a previous
               `worklog-add' / `worklog-search' result.

Exactly one of (file + start_line) or (digest) should be provided.
Returns =(:entry PLIST-OR-NIL)=."
  (anvil-server-with-error-handling
   (let* ((dig (anvil-worklog--coerce-string digest))
          (entry
           (cond
            ((and dig (not (string-empty-p dig)))
             (anvil-worklog-get-by-digest dig))
            (t
             (let ((sl (anvil-worklog--coerce-int start_line nil)))
               (and file sl (anvil-worklog-get file sl)))))))
     (list :entry entry))))

(defun anvil-worklog--tool-add (title body &optional date machine year)
  "Insert a DB-direct worklog entry (Phase 2 write path).

MCP Parameters:
  title   - Required heading text (e.g. `anvil-worklog Phase 2 着地ログ').
  body    - Required body block (org-formatted text; embedded newlines
            and `**' subsections are kept verbatim).
  date    - Optional ISO YYYY-MM-DD (default: today).
  machine - Optional `<platform>-<host>' override (default:
            `anvil-worklog-platform' + `system-name').
  year    - Optional 4-digit year (default: derived from DATE).

Returns =(:file SYNTHETIC :start-line N :machine M :year Y :date D
:digest SHA1)=."
  (anvil-server-with-error-handling
   (anvil-worklog-add
    (or title "")
    (or body "")
    :date (anvil-worklog--coerce-string date)
    :machine (anvil-worklog--coerce-string machine)
    :year (anvil-worklog--coerce-int year nil))))

(defun anvil-worklog--tool-export-org (machine year &optional path since until)
  "Export worklog entries for (MACHINE, YEAR) to an org file.

MCP Parameters:
  machine - Required `<platform>-<host>' token (equality filter).
  year    - Required 4-digit year.
  path    - Optional output path (default:
            `<roots>/capture/ai-logs-<MACHINE>-<YEAR>.org').
  since   - Optional ISO date lower bound.
  until   - Optional ISO date upper bound.

Existing path is overwritten — this is a snapshot, not an append.
Returns =(:path PATH :entries N)=."
  (anvil-server-with-error-handling
   (let* ((m (anvil-worklog--coerce-string machine))
          (y (anvil-worklog--coerce-int year nil))
          (p (anvil-worklog--coerce-string path))
          (s (anvil-worklog--coerce-string since))
          (u (anvil-worklog--coerce-string until)))
     (anvil-worklog-export-org
      :machine m :year y :path p :since s :until u))))


;;;; --- module lifecycle ------------------------------------------------

(defconst anvil-worklog--tool-specs
  `((,(anvil-server-encode-handler #'anvil-worklog--tool-scan)
     :id "worklog-scan"
     :intent '(worklog admin)
     :layer 'workflow
     :description
     "Scan capture/ai-logs-<machine>-<year>.org files and (re-)populate
the worklog index.  Body changes are detected via sha1 digest so a
re-scan is cheap.  Default roots: ~/Cowork/Notes/capture and
~/Notes/capture.  Pass `roots' as a colon-separated absolute-path list
to override.  Returns counts (:files / :entries / :inserted /
:updated / :unchanged / :deleted).")

    (,(anvil-server-encode-handler #'anvil-worklog--tool-prune)
     :id "worklog-prune"
     :intent '(worklog admin)
     :layer 'workflow
     :description
     "Drop worklog rows whose backing org file no longer exists.  Pair
with `worklog-scan' (which only adds / refreshes) to keep the index
consistent after a file move.  Empty `roots' prunes globally.")

    (,(anvil-server-encode-handler #'anvil-worklog--tool-search)
     :id "worklog-search"
     :intent '(worklog)
     :layer 'workflow
     :description
     "Full-text search the worklog FTS5 index.  CJK-friendly (trigram)
when SQLite supports it.  Filters: `machine' (equality), `since' /
`until' (ISO YYYY-MM-DD, inclusive), `year' (4-digit).  Returns
(:rows ROWS) ordered by FTS rank (best first)."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-worklog--tool-list)
     :id "worklog-list"
     :intent '(worklog)
     :layer 'workflow
     :description
     "Return recent worklog entries ordered by date DESC.  Same filter
set as `worklog-search' but no FTS5 query.  Default limit 20.  Use
this to answer 'what did I do last week' style queries."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-worklog--tool-get)
     :id "worklog-get"
     :intent '(worklog)
     :layer 'workflow
     :description
     "Return a single worklog entry plist for the (file, start_line)
key, or by `digest' (sha1 hex) when only the digest is on hand.
Pair with `worklog-search' / `worklog-list' / `worklog-add'
(which return both keys) when a downstream tool needs the full
body without re-parsing the source org.  Exactly one of
(file + start_line) or (digest) should be provided."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-worklog--tool-add)
     :id "worklog-add"
     :intent '(worklog)
     :layer 'workflow
     :description
     "Phase 2 DB-direct write path — insert a `*** MEMO AI:' entry
straight into the SQLite index without touching the org file.  Used
as the primary write API in place of editing capture/ai-logs-*.org.
Required: `title', `body'.  Optional: `date' (default today),
`machine' (default `<platform>-<host>'), `year' (default derived
from date).  Body is stored verbatim and indexed in FTS5 immediately,
so `worklog-search' returns the new row in the same session.
Returns the synthetic (file, start_line) key so the caller can fetch
the row back via `worklog-get'.")

    (,(anvil-server-encode-handler #'anvil-worklog--tool-export-org)
     :id "worklog-export-org"
     :intent '(worklog admin)
     :layer 'workflow
     :description
     "Phase 2 optional: render worklog entries for one (machine, year)
as an org file (`** NOTE 作業ログ <DATE>' + `*** MEMO AI:' tree).
DB-direct (worklog-add) and org-scanned rows are merged in date order.
Caller-driven — no automatic hook / cron writes through this tool.
Used to keep a human-readable git snapshot of the SQLite index when
desired.  Required: `machine', `year'.  Optional: `path' (default:
capture/ai-logs-<machine>-<year>.org under the first existing
`anvil-worklog-default-roots' entry), `since' / `until' to clip the
date range.  Existing path is overwritten."))
  "Spec list consumed by `anvil-server-register-tools'.")

(defun anvil-worklog--register-tools ()
  (anvil-server-register-tools anvil-worklog--server-id
                               anvil-worklog--tool-specs))

(defun anvil-worklog--unregister-tools ()
  (anvil-server-unregister-tools anvil-worklog--server-id
                                 anvil-worklog--tool-specs))

;;;###autoload
(defun anvil-worklog-enable ()
  "Open the worklog DB and register worklog-* MCP tools."
  (interactive)
  (anvil-worklog--open)
  (anvil-worklog--register-tools))

;;;###autoload
(defun anvil-worklog-disable ()
  "Unregister worklog-* MCP tools and close the worklog DB."
  (interactive)
  (anvil-worklog--unregister-tools)
  (anvil-worklog--close))


(provide 'anvil-worklog)

;;; anvil-worklog.el ends here
