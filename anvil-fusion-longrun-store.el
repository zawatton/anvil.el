;;; anvil-fusion-longrun-store.el --- SQLite persistence + resume for quests -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion, long-horizon, sqlite

;;; Commentary:
;;
;; Phase 2 of docs/design/02-longrun.org: persist each quest's bounded
;; state digest to SQLite so a long-horizon quest survives interruption
;; (daemon restart, kill, error) and can be resumed from its last
;; checkpoint.  The schema is independent (its own DB), mirroring the
;; anvil-worklog / anvil-memory DB-per-subsystem convention.
;;
;; A checkpoint is written after every step, so the worst-case loss on
;; interruption is one in-flight step.  `anvil-fusion-longrun-start' runs
;; a fresh quest with checkpointing wired in; `anvil-fusion-longrun-resume'
;; continues a saved quest from its stored (step, digest).
;;
;; Store fns take an explicit DB handle so ERT runs against a temp file;
;; the high-level start / resume use the cached default DB.

;;; Code:

(require 'cl-lib)
(require 'anvil-fusion-longrun)

(defcustom anvil-fusion-longrun-store-db-path
  (expand-file-name "~/Cowork/Notes/.anvil-fusion/anvil-fusion-longrun.db")
  "Path to the SQLite DB backing long-horizon quest persistence."
  :type 'string :group 'anvil-fusion-longrun)

(defvar anvil-fusion-longrun-store--default-db nil
  "Cached handle for the default quest store DB.")

(defconst anvil-fusion-longrun-store--schema
  '("CREATE TABLE IF NOT EXISTS quest (
       id TEXT PRIMARY KEY,
       goal TEXT NOT NULL,
       provider TEXT,
       model TEXT,
       max_steps INTEGER,
       digest_max_chars INTEGER,
       cwd TEXT,
       step INTEGER NOT NULL DEFAULT 0,
       digest TEXT,
       status TEXT NOT NULL DEFAULT 'running',
       created INTEGER,
       updated INTEGER)"
    "CREATE TABLE IF NOT EXISTS quest_step (
       quest_id TEXT NOT NULL,
       step INTEGER NOT NULL,
       output_chars INTEGER,
       digest_chars INTEGER,
       digest_head TEXT,
       done INTEGER,
       created INTEGER,
       PRIMARY KEY (quest_id, step))")
  "DDL statements applied on open.")

;;;; --- open / schema -------------------------------------------------------

(defun anvil-fusion-longrun-store--now ()
  "Return the current epoch seconds as an integer."
  (truncate (float-time)))

(defun anvil-fusion-longrun-store--gen-id ()
  "Return a fresh quest id."
  (format "q-%s-%04x"
          (format-time-string "%Y%m%d-%H%M%S")
          (random (* 16 16 16 16))))

(defun anvil-fusion-longrun-store-open (path)
  "Open the quest store at PATH (creating parent dir + schema); return handle."
  (unless (sqlite-available-p)
    (error "anvil-fusion-longrun-store: sqlite not available in this Emacs"))
  (let ((dir (file-name-directory (expand-file-name path))))
    (unless (file-directory-p dir) (make-directory dir t)))
  (let ((db (sqlite-open (expand-file-name path))))
    (sqlite-execute db "PRAGMA journal_mode=WAL")
    (dolist (ddl anvil-fusion-longrun-store--schema)
      (sqlite-execute db ddl))
    db))

(defun anvil-fusion-longrun-store-default-db ()
  "Return the cached default store DB, opening it on first use."
  (or anvil-fusion-longrun-store--default-db
      (setq anvil-fusion-longrun-store--default-db
            (anvil-fusion-longrun-store-open anvil-fusion-longrun-store-db-path))))

(defun anvil-fusion-longrun-store-close ()
  "Close and forget the cached default store DB."
  (when anvil-fusion-longrun-store--default-db
    (ignore-errors (sqlite-close anvil-fusion-longrun-store--default-db))
    (setq anvil-fusion-longrun-store--default-db nil)))

;;;; --- rows ----------------------------------------------------------------

(cl-defun anvil-fusion-longrun-store-create
    (db &key id goal provider model max-steps digest-max-chars cwd)
  "Insert a new quest row into DB and return its id."
  (let ((qid (or id (anvil-fusion-longrun-store--gen-id)))
        (now (anvil-fusion-longrun-store--now)))
    (sqlite-execute
     db (concat "INSERT INTO quest "
                "(id,goal,provider,model,max_steps,digest_max_chars,cwd,"
                "step,digest,status,created,updated) "
                "VALUES (?,?,?,?,?,?,?,0,NULL,'running',?,?)")
     (list qid goal
           (and provider (format "%s" provider))
           (and model (format "%s" model))
           max-steps digest-max-chars cwd now now))
    qid))

(defun anvil-fusion-longrun-store-checkpoint (db id step digest status meta)
  "Persist STEP for quest ID: update the quest row and insert a step row.
META is the per-step plist produced by `anvil-fusion-longrun-run'."
  (let ((now (anvil-fusion-longrun-store--now)))
    (sqlite-execute
     db "UPDATE quest SET step=?, digest=?, status=?, updated=? WHERE id=?"
     (list step digest status now id))
    (sqlite-execute
     db (concat "INSERT OR REPLACE INTO quest_step "
                "(quest_id,step,output_chars,digest_chars,digest_head,done,created) "
                "VALUES (?,?,?,?,?,?,?)")
     (list id step
           (plist-get meta :output-chars)
           (plist-get meta :digest-chars)
           (plist-get meta :digest-head)
           (if (plist-get meta :done) 1 0)
           now))))

(defun anvil-fusion-longrun-store-finish (db id step digest stopped)
  "Mark quest ID finished with STOPPED reason (a symbol)."
  (sqlite-execute
   db "UPDATE quest SET step=?, digest=?, status=?, updated=? WHERE id=?"
   (list step digest (format "%s" stopped)
         (anvil-fusion-longrun-store--now) id)))

(defun anvil-fusion-longrun-store-get (db id)
  "Return quest ID from DB as a plist, or nil when absent."
  (let ((row (car (sqlite-select
                   db (concat "SELECT id,goal,provider,model,max_steps,"
                              "digest_max_chars,cwd,step,digest,status,"
                              "created,updated FROM quest WHERE id=?")
                   (list id)))))
    (when row
      (cl-destructuring-bind (qid goal provider model max-steps dmc cwd
                                  step digest status created updated) row
        (list :id qid :goal goal :provider provider :model model
              :max-steps max-steps :digest-max-chars dmc :cwd cwd
              :step step :digest digest :status status
              :created created :updated updated)))))

(cl-defun anvil-fusion-longrun-store-list (db &key status (limit 20))
  "Return recent quests from DB as plists (newest first)."
  (let* ((sql (if status
                  (concat "SELECT id,status,step,goal,updated FROM quest "
                          "WHERE status=? ORDER BY updated DESC LIMIT ?")
                (concat "SELECT id,status,step,goal,updated FROM quest "
                        "ORDER BY updated DESC LIMIT ?")))
         (args (if status (list status limit) (list limit))))
    (mapcar (lambda (r)
              (cl-destructuring-bind (id status step goal updated) r
                (list :id id :status status :step step
                      :goal-head (substring goal 0 (min 60 (length goal)))
                      :updated updated)))
            (sqlite-select db sql args))))

(defun anvil-fusion-longrun-store-steps (db id)
  "Return quest ID's step rows from DB as plists, ascending."
  (mapcar (lambda (r)
            (cl-destructuring-bind (step output-chars digest-chars digest-head done) r
              (list :step step :output-chars output-chars
                    :digest-chars digest-chars :digest-head digest-head
                    :done (and (numberp done) (= done 1)))))
          (sqlite-select
           db (concat "SELECT step,output_chars,digest_chars,digest_head,done "
                      "FROM quest_step WHERE quest_id=? ORDER BY step ASC")
           (list id))))

;;;; --- helpers -------------------------------------------------------------

(defun anvil-fusion-longrun-store--strip-keys (plist keys)
  "Return PLIST with any key in KEYS (and its value) removed."
  (let (out)
    (while plist
      (if (memq (car plist) keys)
          (setq plist (cddr plist))
        (push (car plist) out)
        (push (cadr plist) out)
        (setq plist (cddr plist))))
    (nreverse out)))

(defun anvil-fusion-longrun-store--checkpoint-cb (db)
  "Return an on-step callback persisting to DB."
  (lambda (qid step digest done meta)
    (anvil-fusion-longrun-store-checkpoint
     db qid step digest (if done "done" "running") meta)))

(defun anvil-fusion-longrun-store--finish-cb (db)
  "Return an on-finish callback persisting to DB."
  (lambda (qid step digest stopped)
    (anvil-fusion-longrun-store-finish db qid step digest stopped)))

;;;; --- high-level: start / resume ------------------------------------------

(cl-defun anvil-fusion-longrun-start
    (goal &rest run-args
          &key db provider model max-steps digest-max-chars cwd
          &allow-other-keys)
  "Run a fresh quest for GOAL with SQLite checkpointing wired in.
DB defaults to the cached store.  Remaining RUN-ARGS pass through to
`anvil-fusion-longrun-run' (e.g. :step-fn / :distill-fn for tests).
Returns the run plist, which carries :id."
  (let* ((db (or db (anvil-fusion-longrun-store-default-db)))
         (id (anvil-fusion-longrun-store-create
              db :goal goal :provider provider :model model
              :max-steps max-steps :digest-max-chars digest-max-chars :cwd cwd)))
    (apply #'anvil-fusion-longrun-run goal
           (append
            (list :id id
                  :on-step (anvil-fusion-longrun-store--checkpoint-cb db)
                  :on-finish (anvil-fusion-longrun-store--finish-cb db))
            (anvil-fusion-longrun-store--strip-keys run-args '(:db))))))

(cl-defun anvil-fusion-longrun-resume
    (id &rest run-args &key db max-steps &allow-other-keys)
  "Resume quest ID from its last checkpoint.
DB defaults to the cached store.  MAX-STEPS (or the stored cap)
bounds the continued run.  Remaining RUN-ARGS pass through to
`anvil-fusion-longrun-run'.  Returns the run plist."
  (let* ((db (or db (anvil-fusion-longrun-store-default-db)))
         (q  (or (anvil-fusion-longrun-store-get db id)
                 (user-error "anvil-fusion-longrun: unknown quest %s" id)))
         (maxn (or max-steps (plist-get q :max-steps))))
    (apply #'anvil-fusion-longrun-run (plist-get q :goal)
           (append
            (list :id id
                  :resume-step (plist-get q :step)
                  :resume-digest (plist-get q :digest)
                  :max-steps maxn
                  :provider (let ((p (plist-get q :provider)))
                              (and p (intern p)))
                  :model (plist-get q :model)
                  :digest-max-chars (plist-get q :digest-max-chars)
                  :cwd (plist-get q :cwd)
                  :on-step (anvil-fusion-longrun-store--checkpoint-cb db)
                  :on-finish (anvil-fusion-longrun-store--finish-cb db))
            (anvil-fusion-longrun-store--strip-keys run-args '(:db :max-steps))))))

(provide 'anvil-fusion-longrun-store)
;;; anvil-fusion-longrun-store.el ends here
