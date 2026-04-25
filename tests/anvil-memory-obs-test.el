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


(provide 'anvil-memory-obs-test)
;;; anvil-memory-obs-test.el ends here
