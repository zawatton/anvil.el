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


;;;; --- Phase 2: rule-based compression tests -----------------------------

(defun anvil-memory-obs-test--seed-session (session-id n)
  "Insert N observations for SESSION-ID and return the row ids list."
  (anvil-memory-obs--upsert-session session-id)
  (cl-loop for i from 1 to n
           collect (anvil-memory-obs--insert-observation
                    :session-id session-id
                    :hook (if (= i 1) "session-start" "post-tool-use")
                    :tool-name (if (cl-evenp i) "Read" "Bash")
                    :body (format "obs-body-%d details on this step" i))))

(ert-deftest anvil-memory-obs-summarize-skips-tiny-sessions ()
  "summarize-session returns nil when below the min-observations gate."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-rule-based))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 5))
      (anvil-memory-obs-test--seed-session "tiny" 3)
      (should (null (anvil-memory-obs-summarize-session "tiny")))
      (should (zerop (length
                      (sqlite-select (anvil-memory-obs--db)
                                     "SELECT id FROM obs_summaries")))))))

(ert-deftest anvil-memory-obs-summarize-inserts-rule-based-summary ()
  "summarize-session writes a summary row with correct id range."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-rule-based))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      (let* ((ids (anvil-memory-obs-test--seed-session "comp1" 5))
             (sid (anvil-memory-obs-summarize-session "comp1")))
        (should (integerp sid))
        (let ((row (car (sqlite-select
                         (anvil-memory-obs--db)
                         "SELECT obs_start_id, obs_end_id, summary
                            FROM obs_summaries WHERE id = ?"
                         (list sid)))))
          (should (= (nth 0 row) (car ids)))
          (should (= (nth 1 row) (car (last ids))))
          (should (string-match-p "obs-body-1" (nth 2 row)))
          (should (string-match-p "obs-body-5" (nth 2 row))))))))

(ert-deftest anvil-memory-obs-summarize-mirrors-into-fts ()
  "Summary body is searchable through the FTS5 mirror."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-rule-based))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      (anvil-memory-obs-test--seed-session "comp2" 4)
      (anvil-memory-obs-summarize-session "comp2")
      ;; Use a hyphen-free token so unicode61 / trigram tokenisers
      ;; behave the same way; the body fixture contains "details on
      ;; this step" verbatim.
      (let ((rows (sqlite-select
                   (anvil-memory-obs--db)
                   "SELECT rowid FROM obs_summaries_fts
                     WHERE obs_summaries_fts MATCH 'details'")))
        (should (>= (length rows) 1))))))

(ert-deftest anvil-memory-obs-summarize-respects-none-fallback ()
  "fallback = none => no summary row even with enough observations."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-rule-based))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3)
          (anvil-memory-obs-compress-fallback 'none))
      (anvil-memory-obs-test--seed-session "comp3" 5)
      (should (null (anvil-memory-obs-summarize-session "comp3")))
      (should (zerop (length
                      (sqlite-select (anvil-memory-obs--db)
                                     "SELECT id FROM obs_summaries")))))))

(ert-deftest anvil-memory-obs-summarize-truncates-long-bodies ()
  "Bodies longer than rule-excerpt-chars * 2 are summarised with `[…]'."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-rule-based))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3)
          (anvil-memory-obs-compress-rule-excerpt-chars 10))
      (anvil-memory-obs--upsert-session "comp4")
      (dotimes (_ 4)
        (anvil-memory-obs--insert-observation
         :session-id "comp4" :hook "post-tool-use"
         :body (concat "HEAD"
                       (make-string 200 ?x)
                       "TAIL")))
      (anvil-memory-obs-summarize-session "comp4")
      (let ((summary (caar (sqlite-select
                            (anvil-memory-obs--db)
                            "SELECT summary FROM obs_summaries"))))
        (should (string-match-p "HEAD" summary))
        (should (string-match-p "TAIL" summary))
        (should (string-match-p "\\[…\\]" summary))))))


;;;; --- Phase 2: AI compression tests -------------------------------------

(ert-deftest anvil-memory-obs-parse-ai-response-strict-json ()
  "parse-ai-response extracts (topic . summary) from clean JSON."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-ai))
  (let ((parsed (anvil-memory-obs--parse-ai-response
                 "{\"topic\":\"refactor parser\",\"summary\":\"Cleaned up parsing.\"}")))
    (should (equal (car parsed) "refactor parser"))
    (should (equal (cdr parsed) "Cleaned up parsing."))))

(ert-deftest anvil-memory-obs-parse-ai-response-strips-fences ()
  "Markdown ```json fences around the JSON are tolerated."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-ai))
  (let ((parsed (anvil-memory-obs--parse-ai-response
                 "```json\n{\"topic\":\"X\",\"summary\":\"Y\"}\n```")))
    (should (equal (car parsed) "X"))
    (should (equal (cdr parsed) "Y"))))

(ert-deftest anvil-memory-obs-parse-ai-response-rejects-garbage ()
  "Non-JSON or missing-field bodies return nil."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-ai))
  (should (null (anvil-memory-obs--parse-ai-response "not json at all")))
  (should (null (anvil-memory-obs--parse-ai-response
                 "{\"topic\":\"X\"}")))                ; no summary
  (should (null (anvil-memory-obs--parse-ai-response ""))))

(ert-deftest anvil-memory-obs-summarize-uses-ai-when-enabled ()
  "use-ai-compression=t routes through the orchestrator stub."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-ai))
  (anvil-memory-obs-test--with-env
    (cl-letf (((symbol-function 'anvil-orchestrator-submit-and-collect)
               (lambda (&rest _args)
                 (list :status 'done
                       :summary
                       "{\"topic\":\"AI topic\",\"summary\":\"AI body.\"}"))))
      (let ((anvil-memory-obs-enabled t)
            (anvil-memory-obs-use-ai-compression t)
            (anvil-memory-obs-compress-min-observations 3))
        (anvil-memory-obs-test--seed-session "ai1" 4)
        (anvil-memory-obs-summarize-session "ai1")
        (let ((row (car (sqlite-select
                         (anvil-memory-obs--db)
                         "SELECT topic, summary FROM obs_summaries"))))
          (should (equal (nth 0 row) "AI topic"))
          (should (equal (nth 1 row) "AI body.")))))))

(ert-deftest anvil-memory-obs-summarize-falls-back-on-orchestrator-error ()
  "AI failure falls back to rule-based without raising."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-ai))
  (anvil-memory-obs-test--with-env
    (cl-letf (((symbol-function 'anvil-orchestrator-submit-and-collect)
               (lambda (&rest _args)
                 (error "boom"))))
      (let ((anvil-memory-obs-enabled t)
            (anvil-memory-obs-use-ai-compression t)
            (anvil-memory-obs-compress-min-observations 3))
        (anvil-memory-obs-test--seed-session "ai2" 4)
        (anvil-memory-obs-summarize-session "ai2")
        (let ((summary (caar (sqlite-select
                              (anvil-memory-obs--db)
                              "SELECT summary FROM obs_summaries"))))
          (should (string-match-p "obs-body-1" summary))
          (should (string-match-p "obs-body-4" summary)))))))

(ert-deftest anvil-memory-obs-summarize-falls-back-on-malformed-json ()
  "Garbage AI body yields a rule-based summary instead."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-ai))
  (anvil-memory-obs-test--with-env
    (cl-letf (((symbol-function 'anvil-orchestrator-submit-and-collect)
               (lambda (&rest _args)
                 (list :status 'done :summary "definitely not json"))))
      (let ((anvil-memory-obs-enabled t)
            (anvil-memory-obs-use-ai-compression t)
            (anvil-memory-obs-compress-min-observations 3))
        (anvil-memory-obs-test--seed-session "ai3" 4)
        (anvil-memory-obs-summarize-session "ai3")
        (let ((summary (caar (sqlite-select
                              (anvil-memory-obs--db)
                              "SELECT summary FROM obs_summaries"))))
          (should (string-match-p "obs-body" summary)))))))

(ert-deftest anvil-memory-obs-summarize-skips-ai-when-flag-off ()
  "use-ai-compression=nil never calls the orchestrator."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-ai))
  (anvil-memory-obs-test--with-env
    (let ((called nil))
      (cl-letf (((symbol-function 'anvil-orchestrator-submit-and-collect)
                 (lambda (&rest _args)
                   (setq called t)
                   (list :status 'done :summary
                         "{\"topic\":\"X\",\"summary\":\"Y\"}"))))
        (let ((anvil-memory-obs-enabled t)
              (anvil-memory-obs-use-ai-compression nil)
              (anvil-memory-obs-compress-min-observations 3))
          (anvil-memory-obs-test--seed-session "ai4" 4)
          (anvil-memory-obs-summarize-session "ai4")
          (should (null called)))))))

(ert-deftest anvil-memory-obs-summarize-force-fallback-bypasses-ai ()
  "FORCE-FALLBACK=t skips AI even when use-ai-compression=t."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-ai))
  (anvil-memory-obs-test--with-env
    (let ((called nil))
      (cl-letf (((symbol-function 'anvil-orchestrator-submit-and-collect)
                 (lambda (&rest _args)
                   (setq called t)
                   (list :status 'done :summary
                         "{\"topic\":\"X\",\"summary\":\"Y\"}"))))
        (let ((anvil-memory-obs-enabled t)
              (anvil-memory-obs-use-ai-compression t)
              (anvil-memory-obs-compress-min-observations 3))
          (anvil-memory-obs-test--seed-session "ai5" 4)
          (anvil-memory-obs-summarize-session "ai5" t)
          (should (null called)))))))


;;;; --- Phase 2: AI budget guard tests ------------------------------------

(ert-deftest anvil-memory-obs-summary-flag-records-is-ai ()
  "Successful AI summarisation stores `is_ai=1'."
  (skip-unless (anvil-memory-obs-test--supported-p 'budget))
  (anvil-memory-obs-test--with-env
    (cl-letf (((symbol-function 'anvil-orchestrator-submit-and-collect)
               (lambda (&rest _args)
                 (list :status 'done :summary
                       "{\"topic\":\"X\",\"summary\":\"Y.\"}"))))
      (let ((anvil-memory-obs-enabled t)
            (anvil-memory-obs-use-ai-compression t)
            (anvil-memory-obs-compress-monthly-budget nil)
            (anvil-memory-obs-compress-min-observations 3))
        (anvil-memory-obs-test--seed-session "bud1" 4)
        (anvil-memory-obs-summarize-session "bud1")
        (let ((flag (caar (sqlite-select
                           (anvil-memory-obs--db)
                           "SELECT is_ai FROM obs_summaries"))))
          (should (= flag 1)))))))

(ert-deftest anvil-memory-obs-summary-flag-rule-based-is-zero ()
  "Rule-based summarisation stores `is_ai=0'."
  (skip-unless (anvil-memory-obs-test--supported-p 'budget))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-use-ai-compression nil)
          (anvil-memory-obs-compress-min-observations 3))
      (anvil-memory-obs-test--seed-session "bud2" 4)
      (anvil-memory-obs-summarize-session "bud2")
      (let ((flag (caar (sqlite-select
                         (anvil-memory-obs--db)
                         "SELECT is_ai FROM obs_summaries"))))
        (should (= flag 0))))))

(ert-deftest anvil-memory-obs-budget-exhaustion-skips-ai ()
  "When monthly budget is reached, AI is skipped (rule-based runs)."
  (skip-unless (anvil-memory-obs-test--supported-p 'budget))
  (anvil-memory-obs-test--with-env
    (let ((called nil))
      (cl-letf (((symbol-function 'anvil-orchestrator-submit-and-collect)
                 (lambda (&rest _args)
                   (setq called t)
                   (list :status 'done :summary
                         "{\"topic\":\"X\",\"summary\":\"Y.\"}"))))
        (let ((anvil-memory-obs-enabled t)
              (anvil-memory-obs-use-ai-compression t)
              (anvil-memory-obs-compress-monthly-budget 1)
              (anvil-memory-obs-compress-min-observations 3))
          ;; Pre-populate the budget by inserting a fake AI summary row.
          (anvil-memory-obs--upsert-session "seed")
          (anvil-memory-obs--insert-summary
           "seed" "T" "S" 1 1 1)
          (anvil-memory-obs-test--seed-session "bud3" 4)
          (anvil-memory-obs-summarize-session "bud3")
          (should (null called))
          (let ((flag (caar (sqlite-select
                             (anvil-memory-obs--db)
                             "SELECT is_ai FROM obs_summaries
                               WHERE session_id = 'bud3'"))))
            (should (= flag 0))))))))

(ert-deftest anvil-memory-obs-budget-nil-disables-guard ()
  "Setting the budget to nil never blocks AI."
  (skip-unless (anvil-memory-obs-test--supported-p 'budget))
  (anvil-memory-obs-test--with-env
    (let ((called nil))
      (cl-letf (((symbol-function 'anvil-orchestrator-submit-and-collect)
                 (lambda (&rest _args)
                   (setq called t)
                   (list :status 'done :summary
                         "{\"topic\":\"X\",\"summary\":\"Y.\"}"))))
        (let ((anvil-memory-obs-enabled t)
              (anvil-memory-obs-use-ai-compression t)
              (anvil-memory-obs-compress-monthly-budget nil)
              (anvil-memory-obs-compress-min-observations 3))
          ;; Even with many existing AI summaries, no gate.
          (anvil-memory-obs--upsert-session "seed")
          (dotimes (_ 50)
            (anvil-memory-obs--insert-summary "seed" "T" "S" 1 1 1))
          (anvil-memory-obs-test--seed-session "bud4" 4)
          (anvil-memory-obs-summarize-session "bud4")
          (should called))))))

(ert-deftest anvil-memory-obs-month-start-is-deterministic ()
  "current-month-start matches the local-time first-of-month."
  (skip-unless (anvil-memory-obs-test--supported-p 'budget))
  (let* ((ts (time-to-seconds (encode-time 30 14 10 15 6 2026)))
         (start (anvil-memory-obs--current-month-start (truncate ts)))
         (start-time (decode-time start)))
    (should (= (nth 3 start-time) 1))   ; day
    (should (= (nth 4 start-time) 6))   ; month
    (should (= (nth 5 start-time) 2026)))) ; year


;;;; --- Phase 3: search API tests -----------------------------------------

(ert-deftest anvil-memory-obs-search-returns-fts-hits ()
  "search returns plist rows ordered by FTS rank."
  (skip-unless (anvil-memory-obs-test--supported-p 'search))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs-test--seed-session "s1" 5)
      (let ((rows (anvil-memory-obs-search "details")))
        (should (>= (length rows) 1))
        (should (plist-get (car rows) :id))
        (should (plist-get (car rows) :preview))
        (should (string-match-p "details" (plist-get (car rows) :preview)))))))

(ert-deftest anvil-memory-obs-search-honors-limit ()
  "search :limit caps the returned row count."
  (skip-unless (anvil-memory-obs-test--supported-p 'search))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs-test--seed-session "s2" 8)
      (let ((rows (anvil-memory-obs-search "details" :limit 3)))
        (should (<= (length rows) 3))))))

(ert-deftest anvil-memory-obs-search-filters-by-hook ()
  "search :hook scopes the result to a single hook event."
  (skip-unless (anvil-memory-obs-test--supported-p 'search))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs-test--seed-session "s3" 6)
      (let ((rows (anvil-memory-obs-search "details" :hook "post-tool-use")))
        (should (cl-every (lambda (r)
                            (equal (plist-get r :hook) "post-tool-use"))
                          rows))))))

(ert-deftest anvil-memory-obs-search-filters-by-session-id ()
  "search :session-id scopes the result."
  (skip-unless (anvil-memory-obs-test--supported-p 'search))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs-test--seed-session "s4a" 4)
      (anvil-memory-obs-test--seed-session "s4b" 4)
      (let ((rows (anvil-memory-obs-search "details" :session-id "s4a")))
        (should (cl-every (lambda (r)
                            (equal (plist-get r :session-id) "s4a"))
                          rows))))))

(ert-deftest anvil-memory-obs-search-empty-query-returns-nil ()
  "Whitespace / empty / nil queries short-circuit to nil."
  (skip-unless (anvil-memory-obs-test--supported-p 'search))
  (anvil-memory-obs-test--with-env
    (anvil-memory-obs--db)
    (should (null (anvil-memory-obs-search "")))
    (should (null (anvil-memory-obs-search "   ")))
    (should (null (anvil-memory-obs-search nil)))))

(ert-deftest anvil-memory-obs-search-preview-respects-cap ()
  "preview is truncated to `anvil-memory-obs-search-preview-chars'."
  (skip-unless (anvil-memory-obs-test--supported-p 'search))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-search-preview-chars 20))
      (anvil-memory-obs--upsert-session "s5")
      (anvil-memory-obs--insert-observation
       :session-id "s5" :hook "post-tool-use"
       :body (concat "details "
                     (make-string 200 ?x)))
      (let* ((rows (anvil-memory-obs-search "details"))
             (preview (plist-get (car rows) :preview)))
        (should (<= (length preview) 20))))))


;;;; --- Phase 3: timeline tests --------------------------------------------

(ert-deftest anvil-memory-obs-timeline-includes-anchor ()
  "timeline always includes the anchor id in its result."
  (skip-unless (anvil-memory-obs-test--supported-p 'timeline))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (let* ((ids (anvil-memory-obs-test--seed-session "tl1" 5))
             (anchor (nth 2 ids))
             (rows (anvil-memory-obs-timeline anchor :window 1)))
        (should (member anchor (mapcar (lambda (r) (plist-get r :id)) rows)))))))

(ert-deftest anvil-memory-obs-timeline-respects-window ()
  "timeline returns at most 2*WINDOW + 1 rows."
  (skip-unless (anvil-memory-obs-test--supported-p 'timeline))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (let* ((ids (anvil-memory-obs-test--seed-session "tl2" 7))
             (anchor (nth 3 ids))
             (rows (anvil-memory-obs-timeline anchor :window 1)))
        (should (<= (length rows) 3))))))

(ert-deftest anvil-memory-obs-timeline-stays-within-session ()
  "timeline never crosses session boundaries even with id-adjacent rows."
  (skip-unless (anvil-memory-obs-test--supported-p 'timeline))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (let* ((a-ids (anvil-memory-obs-test--seed-session "tl3a" 3))
             (_b-ids (anvil-memory-obs-test--seed-session "tl3b" 3))
             (anchor (nth 1 a-ids))
             (rows (anvil-memory-obs-timeline anchor :window 10)))
        (should (cl-every (lambda (r)
                            (member (plist-get r :id) a-ids))
                          rows))))))

(ert-deftest anvil-memory-obs-timeline-unknown-anchor-is-nil ()
  "An anchor id that does not exist returns nil."
  (skip-unless (anvil-memory-obs-test--supported-p 'timeline))
  (anvil-memory-obs-test--with-env
    (anvil-memory-obs--db)
    (should (null (anvil-memory-obs-timeline 9999)))))


;;;; --- Phase 3: get tests -------------------------------------------------

(ert-deftest anvil-memory-obs-get-returns-full-rows ()
  "get returns plist rows with body / payload / importance."
  (skip-unless (anvil-memory-obs-test--supported-p 'get))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (let* ((ids (anvil-memory-obs-test--seed-session "g1" 3))
             (rows (anvil-memory-obs-get ids)))
        (should (= (length rows) 3))
        (should (plist-get (car rows) :body))
        (should (plist-get (car rows) :payload-json))))))

(ert-deftest anvil-memory-obs-get-rejects-too-many-ids ()
  "get errors when given more than `anvil-memory-obs-get-max-ids' ids."
  (skip-unless (anvil-memory-obs-test--supported-p 'get))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-get-max-ids 2))
      (anvil-memory-obs-test--seed-session "g2" 4)
      (should-error
       (anvil-memory-obs-get '(1 2 3))
       :type 'user-error))))

(ert-deftest anvil-memory-obs-get-empty-ids-returns-nil ()
  "An empty ids list returns nil without touching the DB."
  (skip-unless (anvil-memory-obs-test--supported-p 'get))
  (anvil-memory-obs-test--with-env
    (anvil-memory-obs--db)
    (should (null (anvil-memory-obs-get nil)))))


;;;; --- Phase 3: summary-search tests --------------------------------------

(ert-deftest anvil-memory-obs-summary-search-finds-summary-text ()
  "summary-search hits text in the summary column."
  (skip-unless (anvil-memory-obs-test--supported-p 'summary-search))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      (anvil-memory-obs-test--seed-session "ss1" 4)
      (anvil-memory-obs-summarize-session "ss1")
      (let ((rows (anvil-memory-obs-summary-search "details")))
        (should (>= (length rows) 1))
        (should (string-match-p "details" (plist-get (car rows) :summary)))))))

(ert-deftest anvil-memory-obs-summary-search-preserves-is-ai-flag ()
  "summary-search exposes :is-ai correctly for both paths."
  (skip-unless (anvil-memory-obs-test--supported-p 'summary-search))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs--upsert-session "ss2")
      ;; IS-AI follows the 6th-arg contract: nil → 0, truthy → 1.
      ;; (Elisp 0 is truthy, so "rule-based" must pass nil here.)
      (anvil-memory-obs--insert-summary "ss2" "tA" "uniqueAtopic" 1 1 1)
      (anvil-memory-obs--insert-summary "ss2" "tB" "uniqueBtopic" 1 1 nil)
      (let ((rows (anvil-memory-obs-summary-search "uniqueAtopic")))
        (should (eq (plist-get (car rows) :is-ai) t)))
      (let ((rows (anvil-memory-obs-summary-search "uniqueBtopic")))
        (should (eq (plist-get (car rows) :is-ai) nil))))))

(ert-deftest anvil-memory-obs-summary-search-empty-query-returns-nil ()
  "Empty query returns nil."
  (skip-unless (anvil-memory-obs-test--supported-p 'summary-search))
  (anvil-memory-obs-test--with-env
    (anvil-memory-obs--db)
    (should (null (anvil-memory-obs-summary-search "")))))


;;;; --- Phase 3: MCP tool wrapper tests -----------------------------------

(ert-deftest anvil-memory-obs-tool-search-returns-rows-plist ()
  "`--tool-search' returns (:rows ROWS) with FTS hits."
  (skip-unless (anvil-memory-obs-test--supported-p 'mcp-tools))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs-test--seed-session "tw1" 5)
      (let ((result (anvil-memory-obs--tool-search "details" "3")))
        (should (plist-get result :rows))
        (should (<= (length (plist-get result :rows)) 3))))))

(ert-deftest anvil-memory-obs-tool-timeline-coerces-digit-strings ()
  "`--tool-timeline' accepts digit-string anchor / window args."
  (skip-unless (anvil-memory-obs-test--supported-p 'mcp-tools))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (let* ((ids (anvil-memory-obs-test--seed-session "tw2" 6))
             (anchor (nth 2 ids))
             (result (anvil-memory-obs--tool-timeline
                      (number-to-string anchor) "1")))
        (should (plist-get result :rows))
        (should (member anchor
                        (mapcar (lambda (r) (plist-get r :id))
                                (plist-get result :rows))))))))

(ert-deftest anvil-memory-obs-tool-get-accepts-comma-string ()
  "`--tool-get' accepts a comma-separated id string."
  (skip-unless (anvil-memory-obs-test--supported-p 'mcp-tools))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (let* ((ids (anvil-memory-obs-test--seed-session "tw3" 4))
             (id-str (string-join (mapcar #'number-to-string
                                          (cl-subseq ids 0 3))
                                  ", "))
             (result (anvil-memory-obs--tool-get id-str)))
        (should (= (length (plist-get result :rows)) 3))))))

(ert-deftest anvil-memory-obs-tool-get-accepts-list ()
  "`--tool-get' accepts a plain list of integers."
  (skip-unless (anvil-memory-obs-test--supported-p 'mcp-tools))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (let* ((ids (anvil-memory-obs-test--seed-session "tw4" 3))
             (result (anvil-memory-obs--tool-get ids)))
        (should (= (length (plist-get result :rows)) 3))))))

(ert-deftest anvil-memory-obs-tool-summary-search-basic ()
  "`--tool-summary-search' returns (:rows ...) for matched summaries."
  (skip-unless (anvil-memory-obs-test--supported-p 'mcp-tools))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      (anvil-memory-obs-test--seed-session "tw5" 4)
      (anvil-memory-obs-summarize-session "tw5")
      (let ((result (anvil-memory-obs--tool-summary-search "details")))
        (should (>= (length (plist-get result :rows)) 1))))))

(ert-deftest anvil-memory-obs-coerce-int-handles-edges ()
  "Integer / digit-string / garbage all coerce predictably."
  (skip-unless (anvil-memory-obs-test--supported-p 'mcp-tools))
  (should (= (anvil-memory-obs--coerce-int 5 0) 5))
  (should (= (anvil-memory-obs--coerce-int "42" 0) 42))
  (should (= (anvil-memory-obs--coerce-int "not-a-number" 7) 7))
  (should (= (anvil-memory-obs--coerce-int nil 9) 9)))

(ert-deftest anvil-memory-obs-coerce-id-list-accepts-various-shapes ()
  "List, JSON-array string, and comma string all parse to int lists."
  (skip-unless (anvil-memory-obs-test--supported-p 'mcp-tools))
  (should (equal (anvil-memory-obs--coerce-id-list '(1 2 3))
                 '(1 2 3)))
  (should (equal (anvil-memory-obs--coerce-id-list "[1,2,3]")
                 '(1 2 3)))
  (should (equal (anvil-memory-obs--coerce-id-list "1, 2, 3")
                 '(1 2 3)))
  (should (null (anvil-memory-obs--coerce-id-list nil))))

(ert-deftest anvil-memory-obs-enable-disable-roundtrip ()
  "enable / disable register and unregister cleanly without errors."
  (skip-unless (anvil-memory-obs-test--supported-p 'mcp-tools))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-memory-obs-enable)
      (anvil-memory-obs-disable)
      ;; Re-running both is idempotent.
      (anvil-memory-obs-enable)
      (anvil-memory-obs-disable)
      (should t))))


;;;; --- Phase 4: auto-inject tests ----------------------------------------

(cl-defun anvil-memory-obs-test--seed-summary
    (session-id project-dir &key topic summary ts is-ai)
  "Seed a session + summary fixture for auto-inject tests."
  (let ((db (anvil-memory-obs--db)))
    (sqlite-execute
     db
     "INSERT OR IGNORE INTO obs_sessions (id, started_at, project_dir)
        VALUES (?, ?, ?)"
     (list session-id (or ts (anvil-memory-obs--now)) (or project-dir "")))
    (sqlite-execute
     db
     "INSERT INTO obs_summaries
        (session_id, obs_start_id, obs_end_id, topic, summary, ts, is_ai)
        VALUES (?, ?, ?, ?, ?, ?, ?)"
     (list session-id 1 1
           (or topic "tt") (or summary "ss")
           (or ts (anvil-memory-obs--now))
           (if is-ai 1 0)))))

(ert-deftest anvil-memory-obs-auto-inject-off-returns-empty ()
  "Default flag off → preamble is empty."
  (skip-unless (anvil-memory-obs-test--supported-p 'auto-inject))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-auto-inject nil))
      (anvil-memory-obs-test--seed-summary
       "ai-off" "/tmp/proj" :topic "T" :summary "S")
      (should (equal
               (anvil-memory-obs-build-session-preamble "x" "/tmp/proj")
               "")))))

(ert-deftest anvil-memory-obs-auto-inject-no-candidates-returns-empty ()
  "On but DB empty → empty preamble."
  (skip-unless (anvil-memory-obs-test--supported-p 'auto-inject))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-auto-inject t))
      (anvil-memory-obs--db)
      (should (equal
               (anvil-memory-obs-build-session-preamble "x" "/tmp/proj")
               "")))))

(ert-deftest anvil-memory-obs-auto-inject-renders-summary-block ()
  "Matching candidate yields a preamble containing topic + summary."
  (skip-unless (anvil-memory-obs-test--supported-p 'auto-inject))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-auto-inject t)
          (anvil-memory-obs-auto-inject-project-match 'exact))
      (anvil-memory-obs-test--seed-summary
       "ai-1" "/tmp/proj" :topic "refactor" :summary "Tidied up parser." :is-ai t)
      (let ((pre (anvil-memory-obs-build-session-preamble "x" "/tmp/proj")))
        (should (string-match-p "refactor" pre))
        (should (string-match-p "Tidied up parser" pre))
        (should-not (string-match-p "rule-based" pre))))))

(ert-deftest anvil-memory-obs-auto-inject-marks-rule-based ()
  "Rule-based summaries get a `[rule-based]' marker."
  (skip-unless (anvil-memory-obs-test--supported-p 'auto-inject))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-auto-inject t)
          (anvil-memory-obs-auto-inject-project-match 'any))
      (anvil-memory-obs-test--seed-summary
       "ai-2" "" :topic "X" :summary "Y" :is-ai nil)
      (let ((pre (anvil-memory-obs-build-session-preamble "x" "/tmp")))
        (should (string-match-p "rule-based" pre))))))

(ert-deftest anvil-memory-obs-auto-inject-project-match-exact-rejects-others ()
  "Match=exact filters out unrelated project_dir rows."
  (skip-unless (anvil-memory-obs-test--supported-p 'auto-inject))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-auto-inject t)
          (anvil-memory-obs-auto-inject-project-match 'exact))
      (anvil-memory-obs-test--seed-summary
       "ai-3a" "/tmp/A" :topic "topic-a" :summary "S")
      (anvil-memory-obs-test--seed-summary
       "ai-3b" "/tmp/B" :topic "topic-b" :summary "S")
      (let ((pre (anvil-memory-obs-build-session-preamble "x" "/tmp/A")))
        (should (string-match-p "topic-a" pre))
        (should-not (string-match-p "topic-b" pre))))))

(ert-deftest anvil-memory-obs-auto-inject-project-match-prefix-allows-parent ()
  "Match=prefix accepts current as a prefix of the stored project-dir."
  (skip-unless (anvil-memory-obs-test--supported-p 'auto-inject))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-auto-inject t)
          (anvil-memory-obs-auto-inject-project-match 'prefix))
      (anvil-memory-obs-test--seed-summary
       "ai-4" "/tmp/parent/child" :topic "topic-pref" :summary "S")
      (let ((pre (anvil-memory-obs-build-session-preamble "x" "/tmp/parent")))
        (should (string-match-p "topic-pref" pre))))))

(ert-deftest anvil-memory-obs-auto-inject-window-filters-old ()
  "Summaries older than window-days do not appear."
  (skip-unless (anvil-memory-obs-test--supported-p 'auto-inject))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-auto-inject t)
          (anvil-memory-obs-auto-inject-window-days 7)
          (anvil-memory-obs-auto-inject-project-match 'any))
      (let ((old (- (anvil-memory-obs--now) (* 30 24 60 60))))
        (anvil-memory-obs-test--seed-summary
         "ai-5" "" :topic "topic-old" :summary "S" :ts old))
      (let ((pre (anvil-memory-obs-build-session-preamble "x" "/")))
        (should (equal pre ""))))))

(ert-deftest anvil-memory-obs-auto-inject-respects-max-summaries ()
  "Result is capped at `auto-inject-max-summaries'."
  (skip-unless (anvil-memory-obs-test--supported-p 'auto-inject))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-auto-inject t)
          (anvil-memory-obs-auto-inject-project-match 'any)
          (anvil-memory-obs-auto-inject-max-summaries 2))
      (dotimes (i 5)
        (anvil-memory-obs-test--seed-summary
         (format "ai-6-%d" i) "" :topic (format "T%d" i) :summary "S"))
      (let* ((pre (anvil-memory-obs-build-session-preamble "x" "/"))
             ;; Count list-item bullets to confirm the cap.
             (bullets (with-temp-buffer
                        (insert pre)
                        (goto-char (point-min))
                        (let ((n 0))
                          (while (re-search-forward "^- \\*\\*" nil t)
                            (cl-incf n))
                          n))))
        (should (= bullets 2))))))

(ert-deftest anvil-memory-obs-auto-inject-truncates-over-cap ()
  "Output longer than `auto-inject-max-chars' ends with `[truncated]'."
  (skip-unless (anvil-memory-obs-test--supported-p 'auto-inject))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-auto-inject t)
          (anvil-memory-obs-auto-inject-project-match 'any)
          (anvil-memory-obs-auto-inject-max-summaries 50)
          (anvil-memory-obs-auto-inject-max-chars 200))
      (dotimes (i 20)
        (anvil-memory-obs-test--seed-summary
         (format "ai-7-%d" i)
         "" :topic (format "T%d" i)
         :summary (concat "summary "
                          (make-string 50 ?x))))
      (let ((pre (anvil-memory-obs-build-session-preamble "x" "/")))
        (should (<= (length pre) 220))     ; cap + truncation marker
        (should (string-match-p "truncated" pre))))))


;;;; --- Phase 6: promote candidate tests ----------------------------------

(defun anvil-memory-obs-test--seed-high-importance-session (sid n)
  "Seed N observations with `error' keyword (importance bump) under SID."
  (anvil-memory-obs--upsert-session sid)
  (cl-loop for i from 1 to n
           collect (anvil-memory-obs--insert-observation
                    :session-id sid
                    :hook "post-tool-use"
                    :tool-name "Bash"
                    :body (format "error %d in build details" i))))

(ert-deftest anvil-memory-obs-promote-candidates-finds-high-importance ()
  "High-importance summaries surface above the min-importance gate."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote-candidates))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      (anvil-memory-obs-test--seed-high-importance-session "pc1" 4)
      (anvil-memory-obs-summarize-session "pc1")
      (let ((cands (anvil-memory-obs-promote-candidates
                    :min-importance 30)))
        (should (>= (length cands) 1))
        (should (>= (plist-get (car cands) :total-importance) 30))))))

(ert-deftest anvil-memory-obs-promote-candidates-skips-low-importance ()
  "Low-importance sessions do not appear as candidates."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote-candidates))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      ;; seed-session uses bodies without importance keywords -> imp = 0.
      (anvil-memory-obs-test--seed-session "pc2" 4)
      (anvil-memory-obs-summarize-session "pc2")
      (should (null (anvil-memory-obs-promote-candidates
                     :min-importance 30))))))

(ert-deftest anvil-memory-obs-promote-candidates-window-filters-old ()
  "Summaries older than window-days are dropped."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote-candidates))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      (let ((ids (anvil-memory-obs-test--seed-high-importance-session
                  "pc3" 4)))
        (anvil-memory-obs--insert-summary
         "pc3" "stale topic" "stale summary"
         (car ids) (car (last ids)) 1)
        ;; Backdate the summary to 60 days ago.
        (sqlite-execute
         (anvil-memory-obs--db)
         "UPDATE obs_summaries SET ts = ? WHERE session_id = 'pc3'"
         (list (- (anvil-memory-obs--now) (* 60 24 60 60)))))
      (should (null (anvil-memory-obs-promote-candidates
                     :min-importance 30
                     :window-days 7))))))

(ert-deftest anvil-memory-obs-promote-candidates-honours-limit ()
  "Result is capped at :limit."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote-candidates))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      (dotimes (i 5)
        (anvil-memory-obs-test--seed-high-importance-session
         (format "pc4-%d" i) 4)
        (anvil-memory-obs-summarize-session (format "pc4-%d" i)))
      (let ((cands (anvil-memory-obs-promote-candidates
                    :min-importance 30 :limit 2)))
        (should (= (length cands) 2))))))

(ert-deftest anvil-memory-obs-promote-candidates-dedup-drops-similar ()
  "When :check-against-memory and save-check returns a high-similarity
match, the candidate is filtered out."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote-candidates))
  (anvil-memory-obs-test--with-env
    (cl-letf (((symbol-function 'anvil-memory-save-check)
               (lambda (&rest _args)
                 (list :candidates
                       (list (list :file "feedback_x.md"
                                   :similarity 0.9))))))
      (let ((anvil-memory-obs-enabled t)
            (anvil-memory-obs-compress-min-observations 3)
            (anvil-memory-obs-promote-similar-threshold 0.5))
        (anvil-memory-obs-test--seed-high-importance-session "pc5" 4)
        (anvil-memory-obs-summarize-session "pc5")
        (should (null (anvil-memory-obs-promote-candidates
                       :min-importance 30
                       :check-against-memory t)))))))

(ert-deftest anvil-memory-obs-promote-candidates-dedup-keeps-low-similarity ()
  "Low-similarity matches are reported under :similar but not filtered."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote-candidates))
  (anvil-memory-obs-test--with-env
    (cl-letf (((symbol-function 'anvil-memory-save-check)
               (lambda (&rest _args)
                 (list :candidates
                       (list (list :file "feedback_x.md"
                                   :similarity 0.1))))))
      (let ((anvil-memory-obs-enabled t)
            (anvil-memory-obs-compress-min-observations 3)
            (anvil-memory-obs-promote-similar-threshold 0.5))
        (anvil-memory-obs-test--seed-high-importance-session "pc6" 4)
        (anvil-memory-obs-summarize-session "pc6")
        (let ((cands (anvil-memory-obs-promote-candidates
                      :min-importance 30
                      :check-against-memory t)))
          (should (= (length cands) 1))
          (should (plist-get (car cands) :similar)))))))

(ert-deftest anvil-memory-obs-tool-promote-candidates-coerces-args ()
  "MCP wrapper accepts digit-string args + truthy strings."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote-candidates))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      (anvil-memory-obs-test--seed-high-importance-session "pc7" 4)
      (anvil-memory-obs-summarize-session "pc7")
      (let ((result (anvil-memory-obs--tool-promote-candidates
                     "5" "30" nil "30")))
        (should (plist-get result :rows))))))


;;;; --- Phase 6: promote action tests -------------------------------------

(defmacro anvil-memory-obs-test--with-promote-env (&rest body)
  "Run BODY with a fresh obs DB and a temp memory dir."
  (declare (indent 0))
  `(let* ((tmp-db (make-temp-file "anvil-memory-obs-test-" nil ".db"))
          (anvil-memory-obs-db-path tmp-db)
          (mem-dir (file-name-as-directory
                    (make-temp-file "anvil-memory-obs-mem-" t)))
          (anvil-memory-obs-promote-target-dir mem-dir))
     (unwind-protect
         (progn
           (when (fboundp 'anvil-memory-obs--close)
             (anvil-memory-obs--close))
           ,@body)
       (when (fboundp 'anvil-memory-obs--close)
         (anvil-memory-obs--close))
       (when (file-exists-p tmp-db) (delete-file tmp-db))
       (when (file-directory-p mem-dir)
         (delete-directory mem-dir t)))))

(ert-deftest anvil-memory-obs-promote-creates-md-file ()
  "promote writes a new feedback_*.md with frontmatter + body."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote))
  (anvil-memory-obs-test--with-promote-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      (anvil-memory-obs-test--seed-high-importance-session "pp1" 4)
      (let* ((sid (anvil-memory-obs-summarize-session "pp1"))
             (result (anvil-memory-obs-promote sid)))
        (should (eq (plist-get result :status) 'created))
        (should (file-exists-p (plist-get result :file)))
        (let ((body (with-temp-buffer
                      (insert-file-contents (plist-get result :file))
                      (buffer-string))))
          (should (string-match-p "^name: " body))
          (should (string-match-p "^type: feedback" body))
          (should (string-match-p "error" body)))))))

(ert-deftest anvil-memory-obs-promote-updates-memory-index ()
  "promote appends a new line to MEMORY.md (creates it when absent)."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote))
  (anvil-memory-obs-test--with-promote-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      (anvil-memory-obs-test--seed-high-importance-session "pp2" 4)
      (let* ((sid (anvil-memory-obs-summarize-session "pp2"))
             (result (anvil-memory-obs-promote
                      sid :name "memorable name"
                      :description "one-line desc"))
             (index (anvil-memory-obs--memory-index-path
                     anvil-memory-obs-promote-target-dir)))
        (should (plist-get result :index-updated))
        (should (file-exists-p index))
        (let ((index-body (with-temp-buffer
                            (insert-file-contents index)
                            (buffer-string))))
          (should (string-match-p "memorable name" index-body))
          (should (string-match-p "one-line desc" index-body)))))))

(ert-deftest anvil-memory-obs-promote-respects-target-type ()
  "Explicit :target-type lands in both filename prefix and frontmatter."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote))
  (anvil-memory-obs-test--with-promote-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      (anvil-memory-obs-test--seed-high-importance-session "pp3" 4)
      (let* ((sid (anvil-memory-obs-summarize-session "pp3"))
             (result (anvil-memory-obs-promote sid :target-type 'project))
             (filename (file-name-nondirectory
                        (plist-get result :file))))
        (should (string-prefix-p "project_" filename))
        (let ((body (with-temp-buffer
                      (insert-file-contents (plist-get result :file))
                      (buffer-string))))
          (should (string-match-p "^type: project" body)))))))

(ert-deftest anvil-memory-obs-promote-errors-on-unknown-id ()
  "Promoting a non-existent summary id signals a user-error."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote))
  (anvil-memory-obs-test--with-promote-env
    (anvil-memory-obs--db)
    (should-error (anvil-memory-obs-promote 9999) :type 'user-error)))

(ert-deftest anvil-memory-obs-promote-rejects-collision ()
  "Promoting twice with the same name + type errors on the second call."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote))
  (anvil-memory-obs-test--with-promote-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      (anvil-memory-obs-test--seed-high-importance-session "pp4" 4)
      (let* ((sid (anvil-memory-obs-summarize-session "pp4")))
        (anvil-memory-obs-promote sid :name "fixed-slug")
        (should-error
         (anvil-memory-obs-promote sid :name "fixed-slug")
         :type 'user-error)))))

(ert-deftest anvil-memory-obs-promote-memory-index-idempotent ()
  "Second --update with same filename returns nil (no duplicate line)."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote))
  (anvil-memory-obs-test--with-promote-env
    (let ((dir anvil-memory-obs-promote-target-dir))
      (should
       (eq t (anvil-memory-obs--update-memory-index
              dir "feedback_x.md" "X" "desc")))
      (should
       (null (anvil-memory-obs--update-memory-index
              dir "feedback_x.md" "X" "desc")))
      (let* ((index (anvil-memory-obs--memory-index-path dir))
             (lines (with-temp-buffer
                      (insert-file-contents index)
                      (split-string (buffer-string) "\n" t))))
        (should (= (length (cl-remove-if-not
                            (lambda (l) (string-match-p "feedback_x.md" l))
                            lines))
                   1))))))

(ert-deftest anvil-memory-obs-tool-promote-coerces-args ()
  "MCP wrapper accepts digit-string id + string target-type."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote))
  (anvil-memory-obs-test--with-promote-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-min-observations 3))
      (anvil-memory-obs-test--seed-high-importance-session "pp5" 4)
      (let* ((sid (anvil-memory-obs-summarize-session "pp5"))
             (result (anvil-memory-obs--tool-promote
                      (number-to-string sid) "feedback")))
        (should (eq (plist-get result :status) 'created))
        (should (file-exists-p (plist-get result :file)))))))

(ert-deftest anvil-memory-obs-slug-is-stable ()
  "slug helper produces predictable kebab-case output."
  (skip-unless (anvil-memory-obs-test--supported-p 'promote))
  (should (equal (anvil-memory-obs--slug "Refactor parser") "refactor-parser"))
  (should (equal (anvil-memory-obs--slug "  multi   word  ") "multi-word"))
  (should (equal (anvil-memory-obs--slug "____") "untitled"))
  (should (equal (anvil-memory-obs--slug nil) "untitled"))
  (should (<= (length (anvil-memory-obs--slug (make-string 200 ?a))) 30)))


;;;; --- purge tests --------------------------------------------------------

(ert-deftest anvil-memory-obs-purge-removes-old-low-importance ()
  "Old observations with importance below the threshold are deleted."
  (skip-unless (anvil-memory-obs-test--supported-p 'purge))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-purge-age-days 30)
          (anvil-memory-obs-purge-importance-threshold 10))
      (let* ((db (anvil-memory-obs--db))
             (very-old (- (anvil-memory-obs--now) (* 60 24 60 60)))
             (yesterday (- (anvil-memory-obs--now) (* 1 24 60 60))))
        (anvil-memory-obs--upsert-session "p1")
        (anvil-memory-obs--insert-observation
         :session-id "p1" :hook "post-tool-use"
         :body "trivial read" :ts very-old)
        (anvil-memory-obs--insert-observation
         :session-id "p1" :hook "user-prompt"
         :body "fresh prompt" :ts yesterday)
        (let ((deleted (anvil-memory-obs-purge)))
          (should (= deleted 1)))
        (let ((rows (sqlite-select
                     db "SELECT body FROM obs_observations")))
          (should (= (length rows) 1))
          (should (equal (nth 0 (car rows)) "fresh prompt")))))))

(ert-deftest anvil-memory-obs-purge-keeps-high-importance ()
  "Old observations with importance at or above threshold are retained."
  (skip-unless (anvil-memory-obs-test--supported-p 'purge))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-purge-age-days 30)
          (anvil-memory-obs-purge-importance-threshold 10))
      (let* ((db (anvil-memory-obs--db))
             (very-old (- (anvil-memory-obs--now) (* 60 24 60 60))))
        (anvil-memory-obs--upsert-session "p2")
        ;; "error" keyword bumps importance to 30, above threshold 10.
        (anvil-memory-obs--insert-observation
         :session-id "p2" :hook "user-prompt"
         :body "saw a critical error in build" :ts very-old)
        (should (zerop (anvil-memory-obs-purge)))
        (should (= (length
                    (sqlite-select db "SELECT id FROM obs_observations"))
                   1))))))

(ert-deftest anvil-memory-obs-purge-cleans-fts ()
  "Purged rows are also removed from the FTS5 mirror."
  (skip-unless (anvil-memory-obs-test--supported-p 'purge))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-purge-age-days 30)
          (anvil-memory-obs-purge-importance-threshold 10))
      (let* ((db (anvil-memory-obs--db))
             (very-old (- (anvil-memory-obs--now) (* 60 24 60 60))))
        (anvil-memory-obs--upsert-session "p3")
        (anvil-memory-obs--insert-observation
         :session-id "p3" :hook "post-tool-use"
         :body "uniquetokenxyz" :ts very-old)
        (anvil-memory-obs-purge)
        (let ((rows (sqlite-select
                     db
                     "SELECT rowid FROM obs_observations_fts
                       WHERE obs_observations_fts MATCH 'uniquetokenxyz'")))
          (should (zerop (length rows))))))))

(ert-deftest anvil-memory-obs-purge-with-nil-threshold-keeps-all ()
  "Threshold = nil disables purge."
  (skip-unless (anvil-memory-obs-test--supported-p 'purge))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-purge-age-days 1)
          (anvil-memory-obs-purge-importance-threshold nil))
      (anvil-memory-obs--upsert-session "p4")
      (anvil-memory-obs--insert-observation
       :session-id "p4" :hook "user-prompt"
       :body "ancient" :ts 0)
      (should (zerop (anvil-memory-obs-purge)))
      (should (= (length
                  (sqlite-select (anvil-memory-obs--db)
                                 "SELECT id FROM obs_observations"))
                 1)))))


;;;; --- Phase 2: auto-compression-on-session-end tests --------------------

(ert-deftest anvil-memory-obs-auto-compress-off-by-default ()
  "Default flag off: session-end alone never creates a summary row."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-auto))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-on-session-end nil)
          (anvil-memory-obs-compress-min-observations 3))
      (anvil-memory-obs-test--seed-session "auto-off" 5)
      (anvil-memory-obs-record-session-end "auto-off")
      (should (zerop (length
                      (sqlite-select (anvil-memory-obs--db)
                                     "SELECT id FROM obs_summaries")))))))

(ert-deftest anvil-memory-obs-auto-compress-creates-summary ()
  "Flag on: session-end populates obs_summaries with a rule-based row."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-auto))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-on-session-end t)
          (anvil-memory-obs-use-ai-compression nil)
          (anvil-memory-obs-compress-min-observations 3))
      (anvil-memory-obs-test--seed-session "auto-on" 5)
      (anvil-memory-obs-record-session-end "auto-on")
      (let ((rows (sqlite-select
                   (anvil-memory-obs--db)
                   "SELECT session_id FROM obs_summaries")))
        (should (= (length rows) 1))
        (should (equal (caar rows) "auto-on"))))))

(ert-deftest anvil-memory-obs-auto-compress-respects-min-observations ()
  "Even with the flag on, sessions below min-observations skip summary."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-auto))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-compress-on-session-end t)
          (anvil-memory-obs-compress-min-observations 5))
      (anvil-memory-obs-test--seed-session "auto-tiny" 3)
      (anvil-memory-obs-record-session-end "auto-tiny")
      (should (zerop (length
                      (sqlite-select (anvil-memory-obs--db)
                                     "SELECT id FROM obs_summaries")))))))

(ert-deftest anvil-memory-obs-auto-compress-swallows-errors ()
  "A flaky AI path does not break the session-end pipeline."
  (skip-unless (anvil-memory-obs-test--supported-p 'compress-auto))
  (anvil-memory-obs-test--with-env
    (cl-letf (((symbol-function 'anvil-orchestrator-submit-and-collect)
               (lambda (&rest _args)
                 (error "boom"))))
      (let ((anvil-memory-obs-enabled t)
            (anvil-memory-obs-compress-on-session-end t)
            (anvil-memory-obs-use-ai-compression t)
            (anvil-memory-obs-compress-min-observations 3))
        (anvil-memory-obs-test--seed-session "auto-flaky" 5)
        ;; should-not error => the call must complete
        (anvil-memory-obs-record-session-end "auto-flaky")
        ;; rule-based fallback wrote a summary anyway
        (should (= (length
                    (sqlite-select (anvil-memory-obs--db)
                                   "SELECT id FROM obs_summaries"))
                   1))))))


;;;; --- Phase 4 auto-inject integration tests -----------------------------

(ert-deftest anvil-memory-obs-session-start-injects-preamble ()
  "session-start dispatch returns preamble built from prior summaries."
  (skip-unless (anvil-memory-obs-test--supported-p 'auto-inject-integration))
  (skip-unless (fboundp 'anvil-session-hook-dispatch))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-auto-inject t)
          (anvil-memory-obs-auto-inject-project-match 'any)
          (default-directory "/tmp/inject-proj"))
      (anvil-memory-obs-test--seed-summary
       "ai-int-1" "/tmp/inject-proj"
       :topic "Phase 4 hook" :summary "Earlier session highlights." :is-ai t)
      (let ((preamble (anvil-session-hook-dispatch
                       'session-start "ai-int-new")))
        (should (stringp preamble))
        (should (string-match-p "Phase 4 hook" preamble))))))

(ert-deftest anvil-memory-obs-session-start-empty-when-flag-off ()
  "Flag off: session-start preamble does not contain obs content."
  (skip-unless (anvil-memory-obs-test--supported-p 'auto-inject-integration))
  (skip-unless (fboundp 'anvil-session-hook-dispatch))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t)
          (anvil-memory-obs-auto-inject nil)
          (default-directory "/tmp/inject-proj"))
      (anvil-memory-obs-test--seed-summary
       "ai-int-2" "/tmp/inject-proj"
       :topic "should-not-appear" :summary "x")
      (let ((preamble (anvil-session-hook-dispatch
                       'session-start "ai-int-2-new")))
        (should (stringp preamble))
        (should-not (string-match-p "should-not-appear" preamble))))))


;;;; --- anvil-session integration tests ------------------------------------

(require 'anvil-session nil t)

(ert-deftest anvil-memory-obs-integration-session-start ()
  "Dispatching session-start through anvil-session creates an obs row."
  (skip-unless (anvil-memory-obs-test--supported-p 'integration))
  (skip-unless (fboundp 'anvil-session-hook-dispatch))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-session-hook-dispatch 'session-start "int-1")
      (let ((rows (sqlite-select
                   (anvil-memory-obs--db)
                   "SELECT hook FROM obs_observations
                     WHERE session_id = 'int-1'")))
        (should (member "session-start" (mapcar #'car rows)))))))

(ert-deftest anvil-memory-obs-integration-user-prompt ()
  "Dispatching user-prompt records the prompt body."
  (skip-unless (anvil-memory-obs-test--supported-p 'integration))
  (skip-unless (fboundp 'anvil-session-hook-dispatch))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-session-hook-dispatch 'user-prompt "int-2" "investigate slow build")
      (let ((rows (sqlite-select
                   (anvil-memory-obs--db)
                   "SELECT body FROM obs_observations
                     WHERE session_id = 'int-2' AND hook = 'user-prompt'")))
        (should (= (length rows) 1))
        (should (string-match-p "slow build" (caar rows)))))))

(ert-deftest anvil-memory-obs-integration-post-tool-use ()
  "Dispatching post-tool-use stores the tool name."
  (skip-unless (anvil-memory-obs-test--supported-p 'integration))
  (skip-unless (fboundp 'anvil-session-hook-dispatch))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-session-hook-dispatch 'post-tool-use "int-3" "Bash" "ls /tmp")
      (let ((rows (sqlite-select
                   (anvil-memory-obs--db)
                   "SELECT tool_name FROM obs_observations
                     WHERE session_id = 'int-3' AND hook = 'post-tool-use'")))
        (should (equal (mapcar #'car rows) '("Bash")))))))

(ert-deftest anvil-memory-obs-integration-session-end ()
  "Dispatching session-end records the obs row and stamps ended_at."
  (skip-unless (anvil-memory-obs-test--supported-p 'integration))
  (skip-unless (fboundp 'anvil-session-hook-dispatch))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-session-hook-dispatch 'session-start "int-4")
      (anvil-session-hook-dispatch 'session-end "int-4")
      (let* ((db (anvil-memory-obs--db))
             (sess (sqlite-select
                    db
                    "SELECT ended_at FROM obs_sessions WHERE id = 'int-4'"))
             (obs (sqlite-select
                   db
                   "SELECT hook FROM obs_observations
                     WHERE session_id = 'int-4' ORDER BY id")))
        (should (integerp (caar sess)))
        (should (member "session-end" (mapcar #'car obs)))))))

(ert-deftest anvil-memory-obs-integration-stop ()
  "Dispatching stop records the transcript path as obs body."
  (skip-unless (anvil-memory-obs-test--supported-p 'integration))
  (skip-unless (fboundp 'anvil-session-hook-dispatch))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled t))
      (anvil-session-hook-dispatch 'stop "int-5" "/tmp/transcript-int-5.json")
      (let ((rows (sqlite-select
                   (anvil-memory-obs--db)
                   "SELECT body FROM obs_observations
                     WHERE session_id = 'int-5' AND hook = 'stop'")))
        (should (= (length rows) 1))
        (should (string-match-p "transcript-int-5" (caar rows)))))))

(ert-deftest anvil-memory-obs-integration-disabled-is-noop ()
  "When the module is loaded but disabled, dispatch creates no obs rows."
  (skip-unless (anvil-memory-obs-test--supported-p 'integration))
  (skip-unless (fboundp 'anvil-session-hook-dispatch))
  (anvil-memory-obs-test--with-env
    (let ((anvil-memory-obs-enabled nil))
      (anvil-session-hook-dispatch 'session-start "int-6")
      (anvil-session-hook-dispatch 'user-prompt "int-6" "hello")
      (let ((db (anvil-memory-obs--db)))
        (should
         (zerop
          (caar (sqlite-select
                 db "SELECT COUNT(*) FROM obs_observations"))))))))


(provide 'anvil-memory-obs-test)
;;; anvil-memory-obs-test.el ends here
