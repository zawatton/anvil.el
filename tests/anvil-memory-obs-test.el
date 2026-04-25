;;; anvil-memory-obs-test.el --- Tests for observation capture  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 37 Phase 1 — observation capture layer
;; (`anvil-memory-obs').
;;
;; Each test isolates itself with a fresh `anvil-memory-obs-db-path'
;; temp SQLite file; the user's real DB at
;; `~/.emacs.d/anvil-memory-obs.db' is never touched.
;;
;; Tests `skip-unless' their capability tag is in
;; `anvil-memory-obs-supported' so a TDD-lock commit can land
;; before the implementation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'anvil-memory-obs nil t)


;;;; --- fixture helpers ----------------------------------------------------

(defun anvil-memory-obs-test--supported-p (tag)
  "Return non-nil when TAG is in `anvil-memory-obs-supported'."
  (and (boundp 'anvil-memory-obs-supported)
       (memq tag anvil-memory-obs-supported)))

(defmacro anvil-memory-obs-test--with-env (&rest body)
  "Run BODY with a fresh, isolated obs DB."
  (declare (indent 0))
  `(let* ((tmp-db (make-temp-file "anvil-memory-obs-test-" nil ".db"))
          (anvil-memory-obs-db-path tmp-db))
     (unwind-protect
         (progn
           (when (fboundp 'anvil-memory-obs--close)
             (anvil-memory-obs--close))
           ,@body)
       (when (fboundp 'anvil-memory-obs--close)
         (anvil-memory-obs--close))
       (when (file-exists-p tmp-db)
         (delete-file tmp-db)))))


;;;; --- schema tests -------------------------------------------------------

(ert-deftest anvil-memory-obs-schema-creates-required-tables ()
  "Opening a fresh DB creates sessions / observations / summaries tables."
  (skip-unless (anvil-memory-obs-test--supported-p 'schema))
  (anvil-memory-obs-test--with-env
    (let* ((db (anvil-memory-obs--db))
           (rows (sqlite-select
                  db
                  "SELECT name FROM sqlite_master
                    WHERE type='table' AND name LIKE 'obs_%'
                    ORDER BY name"))
           (names (mapcar #'car rows)))
      (should (member "obs_sessions" names))
      (should (member "obs_observations" names))
      (should (member "obs_summaries" names)))))

(ert-deftest anvil-memory-obs-schema-creates-fts5-virtual-tables ()
  "FTS5 virtual tables for observations and summaries are present."
  (skip-unless (anvil-memory-obs-test--supported-p 'schema))
  (anvil-memory-obs-test--with-env
    (let* ((db (anvil-memory-obs--db))
           (rows (sqlite-select
                  db
                  "SELECT name FROM sqlite_master
                    WHERE name IN ('obs_observations_fts',
                                   'obs_summaries_fts')
                    ORDER BY name")))
      (should (= (length rows) 2)))))

(ert-deftest anvil-memory-obs-schema-creates-indexes ()
  "Required indexes on observations are created."
  (skip-unless (anvil-memory-obs-test--supported-p 'schema))
  (anvil-memory-obs-test--with-env
    (let* ((db (anvil-memory-obs--db))
           (rows (sqlite-select
                  db
                  "SELECT name FROM sqlite_master
                    WHERE type='index' AND name LIKE 'obs_observations_%'
                    ORDER BY name"))
           (names (mapcar #'car rows)))
      (should (member "obs_observations_session_idx" names))
      (should (member "obs_observations_ts_idx" names))
      (should (member "obs_observations_hook_idx" names)))))

(ert-deftest anvil-memory-obs-schema-is-idempotent ()
  "Calling --open twice on the same DB does not error."
  (skip-unless (anvil-memory-obs-test--supported-p 'schema))
  (anvil-memory-obs-test--with-env
    (anvil-memory-obs--open)
    (anvil-memory-obs--close)
    (anvil-memory-obs--open)
    (should anvil-memory-obs--db)))

(ert-deftest anvil-memory-obs-schema-supports-row-insert ()
  "After schema setup, an observation row can be inserted and read back."
  (skip-unless (anvil-memory-obs-test--supported-p 'schema))
  (anvil-memory-obs-test--with-env
    (let ((db (anvil-memory-obs--db)))
      (sqlite-execute
       db
       "INSERT INTO obs_sessions (id, started_at) VALUES (?, ?)"
       (list "test-sess-1" 1700000000))
      (sqlite-execute
       db
       "INSERT INTO obs_observations
          (session_id, ts, hook, tool_name, payload_json, body, importance)
          VALUES (?, ?, ?, ?, ?, ?, ?)"
       (list "test-sess-1" 1700000001 "post-tool-use" "Read"
             "{}" "read foo.el line 1-50" 0))
      (let ((rows (sqlite-select
                   db
                   "SELECT session_id, hook, body
                      FROM obs_observations WHERE session_id = ?"
                   (list "test-sess-1"))))
        (should (= (length rows) 1))
        (should (equal (nth 0 (car rows)) "test-sess-1"))
        (should (equal (nth 1 (car rows)) "post-tool-use"))
        (should (equal (nth 2 (car rows)) "read foo.el line 1-50"))))))


;;;; --- redaction tests ----------------------------------------------------

(ert-deftest anvil-memory-obs-redact-replaces-secrets ()
  "Common secret patterns are replaced by [REDACTED]."
  (skip-unless (anvil-memory-obs-test--supported-p 'redact))
  (should (string-match-p
           "\\[REDACTED\\]"
           (anvil-memory-obs--redact "api_key=abc123def")))
  (should (string-match-p
           "\\[REDACTED\\]"
           (anvil-memory-obs--redact "password=hunter2")))
  (should (string-match-p
           "\\[REDACTED\\]"
           (anvil-memory-obs--redact "Authorization: Bearer eyJ.abc.def")))
  (should (string-match-p
           "\\[REDACTED\\]"
           (anvil-memory-obs--redact "ghp_abcdefghijklmnopqrstuvwxyz0123456789")))
  (should (string-match-p
           "\\[REDACTED\\]"
           (anvil-memory-obs--redact "sk-abcdefghijklmnopqrst"))))

(ert-deftest anvil-memory-obs-redact-passes-through-clean-text ()
  "Plain text without secrets is returned unchanged."
  (skip-unless (anvil-memory-obs-test--supported-p 'redact))
  (let ((clean "read foo.el line 42 and it returned a list"))
    (should (equal (anvil-memory-obs--redact clean) clean))))

(ert-deftest anvil-memory-obs-redact-handles-nil-and-empty ()
  "nil and empty bodies pass through without error."
  (skip-unless (anvil-memory-obs-test--supported-p 'redact))
  (should (null (anvil-memory-obs--redact nil)))
  (should (equal (anvil-memory-obs--redact "") "")))


;;;; --- importance tests ---------------------------------------------------

(ert-deftest anvil-memory-obs-importance-applies-rules ()
  "Each matching rule contributes its :delta to the score."
  (skip-unless (anvil-memory-obs-test--supported-p 'importance))
  ;; Default rules: "error" +30, "fix" +20.
  (should (= (anvil-memory-obs--compute-importance "got an error here") 30))
  (should (= (anvil-memory-obs--compute-importance "applied a fix") 20))
  (should (= (anvil-memory-obs--compute-importance "error fixed") 50)))

(ert-deftest anvil-memory-obs-importance-clamps-to-100 ()
  "Importance is clamped to the [0, 100] range."
  (skip-unless (anvil-memory-obs-test--supported-p 'importance))
  (let ((anvil-memory-obs-importance-rules
         '((:keyword "x" :delta 200))))
    (should (= (anvil-memory-obs--compute-importance "x") 100))))

(ert-deftest anvil-memory-obs-importance-handles-empty-body ()
  "Empty / nil body yields 0."
  (skip-unless (anvil-memory-obs-test--supported-p 'importance))
  (should (= (anvil-memory-obs--compute-importance "") 0))
  (should (= (anvil-memory-obs--compute-importance nil) 0)))


;;;; --- record API tests ---------------------------------------------------

(ert-deftest anvil-memory-obs-record-is-noop-when-disabled ()
  "Record functions are no-ops when `anvil-memory-obs-enabled' is nil."
  (skip-unless (anvil-memory-obs-test--supported-p 'record))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled nil))
      (should (null (anvil-memory-obs-record-session-start "sess-x")))
      (should (null (anvil-memory-obs-record-user-prompt "sess-x" "hi"))))))

(ert-deftest anvil-memory-obs-record-session-start-inserts-row ()
  "record-session-start creates a session and an observation."
  (skip-unless (anvil-memory-obs-test--supported-p 'record))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs-record-session-start "sess-1" "/tmp/proj")
      (let* ((db (anvil-memory-obs--db))
             (sess (sqlite-select
                    db "SELECT id, project_dir FROM obs_sessions"))
             (obs (sqlite-select
                   db "SELECT session_id, hook FROM obs_observations")))
        (should (= (length sess) 1))
        (should (equal (nth 0 (car sess)) "sess-1"))
        (should (equal (nth 1 (car sess)) "/tmp/proj"))
        (should (= (length obs) 1))
        (should (equal (nth 1 (car obs)) "session-start"))))))

(ert-deftest anvil-memory-obs-record-session-start-is-idempotent-on-id ()
  "Calling record-session-start twice for the same id keeps started_at."
  (skip-unless (anvil-memory-obs-test--supported-p 'record))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs-record-session-start "sess-2")
      (let* ((db (anvil-memory-obs--db))
             (started1 (caar (sqlite-select
                              db "SELECT started_at FROM obs_sessions
                                   WHERE id = 'sess-2'"))))
        ;; Wait 1+ second so a buggy re-insert would change started_at.
        (sleep-for 1)
        (anvil-memory-obs-record-session-start "sess-2")
        (let ((started2 (caar (sqlite-select
                               db "SELECT started_at FROM obs_sessions
                                    WHERE id = 'sess-2'"))))
          (should (= started1 started2)))))))

(ert-deftest anvil-memory-obs-record-user-prompt-stores-importance ()
  "user-prompt body containing a keyword bumps importance."
  (skip-unless (anvil-memory-obs-test--supported-p 'record))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs-record-user-prompt "sess-3" "got an error in the build")
      (let* ((db (anvil-memory-obs--db))
             (rows (sqlite-select
                    db
                    "SELECT body, importance FROM obs_observations")))
        (should (= (length rows) 1))
        (should (string-match-p "error" (nth 0 (car rows))))
        (should (>= (nth 1 (car rows)) 30))))))

(ert-deftest anvil-memory-obs-record-post-tool-use-sets-tool-name ()
  "post-tool-use stores the tool name."
  (skip-unless (anvil-memory-obs-test--supported-p 'record))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs-record-post-tool-use "sess-4" "Read" "foo.el line 1-50")
      (let ((rows (sqlite-select
                   (anvil-memory-obs--db)
                   "SELECT tool_name, hook FROM obs_observations")))
        (should (= (length rows) 1))
        (should (equal (nth 0 (car rows)) "Read"))
        (should (equal (nth 1 (car rows)) "post-tool-use"))))))

(ert-deftest anvil-memory-obs-record-session-end-stamps-ended-at ()
  "session-end updates ended_at on the session row and logs an obs row."
  (skip-unless (anvil-memory-obs-test--supported-p 'record))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs-record-session-start "sess-5")
      (anvil-memory-obs-record-session-end "sess-5")
      (let* ((db (anvil-memory-obs--db))
             (sess (sqlite-select
                    db "SELECT ended_at FROM obs_sessions WHERE id = 'sess-5'"))
             (obs (sqlite-select
                   db "SELECT hook FROM obs_observations
                        WHERE session_id = 'sess-5' ORDER BY id")))
        (should (integerp (caar sess)))
        (should (equal (mapcar #'car obs)
                       '("session-start" "session-end")))))))

(ert-deftest anvil-memory-obs-record-redacts-secret-in-prompt ()
  "Secrets in user-prompt body are redacted before storage."
  (skip-unless (anvil-memory-obs-test--supported-p 'record))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs-record-user-prompt "sess-6" "set api_key=abcdef123456")
      (let ((body (caar (sqlite-select
                         (anvil-memory-obs--db)
                         "SELECT body FROM obs_observations"))))
        (should (string-match-p "\\[REDACTED\\]" body))
        (should-not (string-match-p "abcdef123456" body))))))

(ert-deftest anvil-memory-obs-record-fts-roundtrip ()
  "Observation body is searchable via FTS5 after insert."
  (skip-unless (anvil-memory-obs-test--supported-p 'record))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs-record-user-prompt
       "sess-7" "implementing claude-mem inspired observation layer")
      (let* ((db (anvil-memory-obs--db))
             (rows (sqlite-select
                    db
                    "SELECT rowid FROM obs_observations_fts
                      WHERE obs_observations_fts MATCH 'observation'")))
        (should (>= (length rows) 1))))))


(provide 'anvil-memory-obs-test)
;;; anvil-memory-obs-test.el ends here
