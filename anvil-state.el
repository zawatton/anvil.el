;;; anvil-state.el --- Persistent KV store for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Lightweight SQLite-backed key-value store shared across anvil
;; modules.  Designed so anvil-browser, anvil-offload and future
;; anvil-http can stop rolling their own in-memory caches that evaporate
;; on daemon restart.
;;
;; Design doc: docs/design/08-state.org.
;;
;; Public Elisp API (MCP tools intentionally NOT exposed, Q1 resolved):
;;   (anvil-state-set KEY VAL &key ns ttl)
;;   (anvil-state-get KEY &key ns default)
;;   (anvil-state-delete KEY &key ns)
;;   (anvil-state-delete-ns NS)
;;   (anvil-state-list-ns)
;;   (anvil-state-count &optional ns)
;;   (anvil-state-vacuum)
;;
;; Values are serialized via `prin1-to-string' and restored with
;; `read-from-string'.  Anything printable+readable works (strings,
;; numbers, symbols, conses, vectors, hash-tables, plists, alists).
;; Non-readable values (buffers, markers, processes) fail fast at SET
;; time with a user-error.
;;
;; Namespaces are flat strings.  Convention: use the module name as the
;; namespace (e.g. "browser", "offload", "http").  "default" is reserved
;; for ad-hoc use.
;;
;; TTL is stored as an absolute unix timestamp in the expires_at column.
;; Lazy expiry happens on GET.  `anvil-state-vacuum' removes expired
;; rows physically and runs SQLite VACUUM.  No automatic timer runs
;; (Q3 resolved): the caller or a cron job triggers it.
;;
;; Concurrency: Phase 1 assumes a single writer (main daemon).  Worker
;; sub-daemons should read only.  Phase 4 will enable WAL mode for
;; safe multi-writer access.  Requires Emacs 29+ (built-in sqlite).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nelisp-state nil 'noerror)

;;; Customization

(defgroup anvil-state nil
  "Anvil persistent KV store."
  :group 'anvil
  :prefix "anvil-state-")

(defcustom anvil-state-db-path
  (expand-file-name "anvil-state.db" user-emacs-directory)
  "Path to the SQLite database file backing `anvil-state'."
  :type 'file
  :group 'anvil-state)

(defconst anvil-state-schema-version 1
  "Current schema version of the anvil-state database.
Bump on incompatible changes; add a migration branch in
`anvil-state--migrate' at the same time.")

(defconst anvil-state-default-namespace "default"
  "Namespace used when the caller omits `:ns'.")

;;; Internal state

(defvar anvil-state--db nil
  "Open SQLite handle, or nil when `anvil-state-enable' has not run yet.")

;;; Backend plumbing

(defun anvil-state--require-sqlite ()
  "Signal `user-error' unless built-in sqlite (Emacs 29+) is available.

Delegates to `nelisp-state--require-sqlite' when available."
  (if (fboundp 'nelisp-state--require-sqlite)
      (nelisp-state--require-sqlite)
    (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
      (user-error
       "anvil-state: built-in sqlite not available (Emacs 29+ required)"))))

(defun anvil-state--open ()
  "Open the database at `anvil-state-db-path' and cache the handle."
  (anvil-state--require-sqlite)
  (unless anvil-state--db
    (let ((dir (file-name-directory anvil-state-db-path)))
      (unless (file-directory-p dir) (make-directory dir t)))
    (setq anvil-state--db (sqlite-open anvil-state-db-path))
    (anvil-state--ensure-schema anvil-state--db))
  anvil-state--db)

(defun anvil-state--close ()
  "Close the cached DB handle (if any)."
  (when (and anvil-state--db (sqlitep anvil-state--db))
    (ignore-errors (sqlite-close anvil-state--db)))
  (setq anvil-state--db nil))

(defun anvil-state--db ()
  "Return the live DB handle, opening one on first call."
  (or anvil-state--db (anvil-state--open)))

(defmacro anvil-state--with-transaction (db &rest body)
  "Run BODY inside a BEGIN / COMMIT transaction on DB.
Mirrors the portable pattern used by `anvil-org-index' — avoids
`with-sqlite-transaction' which is not available on every target."
  (declare (indent 1) (debug t))
  (let ((db-sym (make-symbol "db")))
    `(let ((,db-sym ,db))
       (sqlite-execute ,db-sym "BEGIN")
       (condition-case err
           (prog1 (progn ,@body)
             (sqlite-execute ,db-sym "COMMIT"))
         (error
          (ignore-errors (sqlite-execute ,db-sym "ROLLBACK"))
          (signal (car err) (cdr err)))))))

;;; Schema

(defconst anvil-state--ddl
  '("CREATE TABLE IF NOT EXISTS schema_meta (
       version INTEGER PRIMARY KEY)"

    "CREATE TABLE IF NOT EXISTS kv (
       ns          TEXT NOT NULL,
       k           TEXT NOT NULL,
       v           TEXT NOT NULL,
       created_at  INTEGER NOT NULL,
       updated_at  INTEGER NOT NULL,
       expires_at  INTEGER,
       PRIMARY KEY (ns, k))"

    "CREATE INDEX IF NOT EXISTS idx_kv_expires
       ON kv(expires_at) WHERE expires_at IS NOT NULL")
  "DDL statements executed by `anvil-state--ensure-schema'.")

(defun anvil-state--ensure-schema (db)
  "Create the schema on DB when missing and run migrations.
Phase 1 only knows schema v1."
  (dolist (stmt anvil-state--ddl)
    (sqlite-execute db stmt))
  (let* ((row (car (sqlite-select
                    db "SELECT version FROM schema_meta LIMIT 1")))
         (current (and row (car row))))
    (cond
     ((null current)
      (sqlite-execute db "INSERT INTO schema_meta(version) VALUES (?1)"
                      (list anvil-state-schema-version)))
     ((= current anvil-state-schema-version)
      nil)
     (t
      (anvil-state--migrate db current anvil-state-schema-version)))))

(defun anvil-state--migrate (_db from to)
  "Migrate schema from FROM to TO.  Phase 1 has no upgrades defined."
  (error "anvil-state: no migration path from schema v%d to v%d" from to))

;;; Serialization

(defun anvil-state--serialize (val)
  "Return a printed, re-readable representation of VAL.
Signals `user-error' when VAL contains non-readable objects.

Delegates to `nelisp-state--serialize' when available."
  (if (fboundp 'nelisp-state--serialize)
      (nelisp-state--serialize val)
    (let ((printed (let ((print-length nil)
                         (print-level nil)
                         (print-circle t))
                     (prin1-to-string val))))
      (condition-case err
          (let ((round (car (read-from-string printed))))
            (ignore round)
            printed)
        (error
         (user-error
          "anvil-state: value is not readable (%s): %S"
          (error-message-string err) val))))))

(defun anvil-state--deserialize (text)
  "Restore the Lisp value stored as TEXT.

Delegates to `nelisp-state--deserialize' when available."
  (if (fboundp 'nelisp-state--deserialize)
      (nelisp-state--deserialize text)
    (car (read-from-string text))))

;;; Key helpers

(defun anvil-state--check-string (name val)
  "Signal `user-error' unless VAL is a non-empty string.
NAME is the argument name used in the error message.

Delegates to `nelisp-state--check-string' when available."
  (if (fboundp 'nelisp-state--check-string)
      (nelisp-state--check-string name val)
    (unless (and (stringp val) (not (string-empty-p val)))
      (user-error "anvil-state: %s must be a non-empty string, got %S"
                  name val))))

(defun anvil-state--ns (opts)
  "Pick the namespace out of OPTS, defaulting to `anvil-state-default-namespace'."
  (let ((ns (or (plist-get opts :ns) anvil-state-default-namespace)))
    (anvil-state--check-string ":ns" ns)
    ns))

(defun anvil-state--expires-at (opts)
  "Return an absolute unix-timestamp for `:ttl' in OPTS, or nil.

Delegates to `nelisp-state--expires-at' when available."
  (if (fboundp 'nelisp-state--expires-at)
      (nelisp-state--expires-at opts)
    (let ((ttl (plist-get opts :ttl)))
      (cond
       ((null ttl) nil)
       ((and (integerp ttl) (> ttl 0))
        (+ (truncate (float-time)) ttl))
       ((and (numberp ttl) (> ttl 0))
        (+ (truncate (float-time)) (truncate ttl)))
       (t
        (user-error
         "anvil-state: :ttl must be a positive number of seconds, got %S"
         ttl))))))

;;; Public API

;;;###autoload
(cl-defun anvil-state-set (key val &key ns ttl)
  "Store VAL under KEY in namespace NS (default \"default\").

MCP-style keyword args:
  :ns STRING  - Namespace.  Conventionally the module name.
  :ttl NUMBER - Seconds until expiry.  nil means never.

VAL must round-trip through `prin1' / `read'; non-readable values
raise `user-error' before the DB is touched.  Returns VAL."
  (anvil-state--check-string "KEY" key)
  (let* ((db (anvil-state--db))
         (ns* (anvil-state--ns (list :ns ns)))
         (now (truncate (float-time)))
         (exp (anvil-state--expires-at (list :ttl ttl)))
         (serialized (anvil-state--serialize val)))
    (sqlite-execute
     db
     "INSERT INTO kv(ns, k, v, created_at, updated_at, expires_at)
        VALUES (?1, ?2, ?3, ?4, ?4, ?5)
        ON CONFLICT(ns, k) DO UPDATE SET
          v = excluded.v,
          updated_at = excluded.updated_at,
          expires_at = excluded.expires_at"
     (list ns* key serialized now exp))
    val))

;;;###autoload
(cl-defun anvil-state-get (key &key ns default)
  "Return the value stored under KEY in namespace NS, or DEFAULT.

MCP-style keyword args:
  :ns STRING      - Namespace (default \"default\").
  :default VALUE  - Returned when KEY is absent or expired.

Expired rows are deleted lazily during the GET that notices them."
  (anvil-state--check-string "KEY" key)
  (let* ((db (anvil-state--db))
         (ns* (anvil-state--ns (list :ns ns)))
         (now (truncate (float-time)))
         (row (car (sqlite-select
                    db
                    "SELECT v, expires_at FROM kv
                       WHERE ns = ?1 AND k = ?2 LIMIT 1"
                    (list ns* key)))))
    (cond
     ((null row) default)
     ((and (nth 1 row) (<= (nth 1 row) now))
      (sqlite-execute db "DELETE FROM kv WHERE ns = ?1 AND k = ?2"
                      (list ns* key))
      default)
     (t (anvil-state--deserialize (nth 0 row))))))

;;;###autoload
(cl-defun anvil-state-delete (key &key ns)
  "Remove KEY from namespace NS.  Return t when a row was deleted."
  (anvil-state--check-string "KEY" key)
  (let* ((db (anvil-state--db))
         (ns* (anvil-state--ns (list :ns ns)))
         (before (car (sqlite-select
                       db "SELECT changes()"))))
    (ignore before)
    (sqlite-execute db "DELETE FROM kv WHERE ns = ?1 AND k = ?2"
                    (list ns* key))
    (let ((changed (car (car (sqlite-select db "SELECT changes()")))))
      (and (integerp changed) (> changed 0)))))

;;;###autoload
(defun anvil-state-delete-ns (ns)
  "Remove every row in namespace NS.  Returns the deletion count."
  (anvil-state--check-string "NS" ns)
  (let ((db (anvil-state--db)))
    (anvil-state--with-transaction db
      (sqlite-execute db "DELETE FROM kv WHERE ns = ?1" (list ns))
      (or (car (car (sqlite-select db "SELECT changes()"))) 0))))

;;;###autoload
(defun anvil-state-list-ns ()
  "Return the sorted list of namespaces currently holding rows."
  (mapcar #'car
          (sqlite-select
           (anvil-state--db)
           "SELECT DISTINCT ns FROM kv ORDER BY ns")))

;;;###autoload
(cl-defun anvil-state-list-keys (&key ns limit)
  "Return the sorted list of keys under namespace NS.
With :limit N, return at most N keys.  Expired rows are skipped so
callers see only live keys; run `anvil-state-vacuum' to physically
remove them.  Omitting :ns is an error — enumerating the entire DB
would cross tenant boundaries and is not supported."
  (unless ns
    (error "anvil-state-list-keys: :ns is required"))
  (anvil-state--check-string "NS" ns)
  (let* ((db (anvil-state--db))
         (now (truncate (float-time)))
         (sql (if limit
                  "SELECT k FROM kv
                     WHERE ns = ?1
                       AND (expires_at IS NULL OR expires_at > ?2)
                     ORDER BY k
                     LIMIT ?3"
                "SELECT k FROM kv
                     WHERE ns = ?1
                       AND (expires_at IS NULL OR expires_at > ?2)
                     ORDER BY k"))
         (params (if limit (list ns now limit) (list ns now))))
    (mapcar #'car (sqlite-select db sql params))))

;;;###autoload
(defun anvil-state-count (&optional ns)
  "Return the number of live rows (optionally in NS only).
Expired rows are counted until `anvil-state-vacuum' physically
removes them."
  (let* ((db (anvil-state--db))
         (row (if ns
                  (progn
                    (anvil-state--check-string "NS" ns)
                    (car (sqlite-select
                          db "SELECT COUNT(*) FROM kv WHERE ns = ?1"
                          (list ns))))
                (car (sqlite-select db "SELECT COUNT(*) FROM kv")))))
    (or (car row) 0)))

;;;###autoload
(defun anvil-state-vacuum ()
  "Delete expired rows and run SQLite VACUUM.
Returns a plist with `:expired' (rows removed) and `:size-before'
/ `:size-after' byte counts of the DB file."
  (let* ((db (anvil-state--db))
         (now (truncate (float-time)))
         (size-before (nth 7 (file-attributes anvil-state-db-path)))
         (expired 0))
    (anvil-state--with-transaction db
      (sqlite-execute
       db
       "DELETE FROM kv WHERE expires_at IS NOT NULL AND expires_at <= ?1"
       (list now))
      (setq expired (or (car (car (sqlite-select db "SELECT changes()")))
                        0)))
    (sqlite-execute db "VACUUM")
    (list :expired expired
          :size-before size-before
          :size-after (nth 7 (file-attributes anvil-state-db-path)))))

;;; Lifecycle

;;;###autoload
(defun anvil-state-enable ()
  "Initialize the anvil-state DB.  No MCP tools are registered."
  (interactive)
  (anvil-state--open)
  nil)

(defun anvil-state-disable ()
  "Close the anvil-state DB handle.  Safe to call repeatedly."
  (interactive)
  (anvil-state--close)
  nil)

(provide 'anvil-state)
;;; anvil-state.el ends here
