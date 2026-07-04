;;; anvil-fusion-traj.el --- Verified trajectory exemplar store -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Codex
;; Keywords: ai, fusion, sqlite

;;; Commentary:
;;
;; Doc 61 Phase 9: a small exemplar library for verified Fusion wins.
;; The store admits only clean verification wins by default: a non-empty
;; answer, non-nil claims, at least one confirmed claim, and zero
;; refuted claims.  That policy intentionally rejects trajectories whose
;; judge had to refute any content; callers can bypass it with :force
;; when they need a manual escape hatch, and the policy may be tuned
;; later as the library evolves.
;;
;; SQLite is preferred when available.  The default DB lives outside the
;; repository and is never committed.  If SQLite is unavailable, or the
;; DB cannot be opened, the module degrades silently to an in-memory list
;; store with the same public API.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-fusion nil t)

(declare-function anvil-fusion--jaccard "anvil-fusion" (a b))

(defcustom anvil-fusion-traj-db-path
  (expand-file-name "anvil-fusion-traj.db" user-emacs-directory)
  "Path to the per-machine verified trajectory SQLite database.
This DB lives outside the repository and is never committed."
  :type 'string
  :group 'anvil-fusion)

(defvar anvil-fusion-traj--default-db nil
  "Cached handle for the default trajectory DB.")

(defvar anvil-fusion-traj--resolved-db-path nil
  "Expanded path corresponding to `anvil-fusion-traj--default-db'.")

(defvar anvil-fusion-traj--memory-store nil
  "Fallback in-memory trajectory rows.")

(defvar anvil-fusion-traj--memory-next-id 1
  "Next fallback in-memory trajectory id.")

(defconst anvil-fusion-traj--schema
  '("CREATE TABLE IF NOT EXISTS traj (
       id INTEGER PRIMARY KEY,
       created REAL,
       question TEXT,
       answer TEXT,
       claims TEXT,
       panel TEXT,
       tags TEXT,
       meta TEXT)"
    "CREATE VIRTUAL TABLE IF NOT EXISTS traj_fts
       USING fts5(id UNINDEXED, question, answer, tokenize=trigram)")
  "DDL statements applied when the SQLite backend opens.")

(defun anvil-fusion-traj--sqlite-available-p ()
  "Return non-nil when builtin SQLite is available."
  (and (fboundp 'sqlite-available-p) (sqlite-available-p)))

(defun anvil-fusion-traj--nonempty-string-p (value)
  "Return non-nil when VALUE is a non-blank string."
  (and (stringp value) (not (string-empty-p (string-trim value)))))

(defun anvil-fusion-traj--head (text max-chars)
  "Return TEXT clamped to MAX-CHARS characters."
  (let* ((s (or text ""))
         (n (max 0 (or max-chars 0))))
    (if (<= (length s) n) s (substring s 0 n))))

(defun anvil-fusion-traj--read-sexp (string)
  "Read STRING as a Lisp object, returning nil on blank or invalid input."
  (when (anvil-fusion-traj--nonempty-string-p string)
    (condition-case nil
        (car (read-from-string string))
      (error nil))))

(defun anvil-fusion-traj--print-sexp (value)
  "Return VALUE encoded as a printed sexp string."
  (when value (prin1-to-string value)))

(defun anvil-fusion-traj--strip-result-meta (result)
  "Return the stored meta plist derived from RESULT."
  (let ((plist result)
        out)
    (while plist
      (let ((key (car plist))
            (value (cadr plist)))
        (unless (memq key '(:answer :claims :panel :candidates))
          (setq out (plist-put out key value)))
        (setq plist (cddr plist))))
    out))

(defun anvil-fusion-traj--sqlite-open (path)
  "Open PATH as a trajectory DB and apply the schema."
  (unless (anvil-fusion-traj--sqlite-available-p)
    (error "anvil-fusion-traj: sqlite unavailable"))
  (let ((dir (file-name-directory (expand-file-name path))))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (let ((db (sqlite-open (expand-file-name path))))
    (sqlite-execute db "PRAGMA journal_mode=WAL")
    (dolist (ddl anvil-fusion-traj--schema)
      (sqlite-execute db ddl))
    db))

(defun anvil-fusion-traj--memory-row->plist (row)
  "Return ROW normalized as a public plist."
  (list :id (plist-get row :id)
        :created (plist-get row :created)
        :question (plist-get row :question)
        :answer (plist-get row :answer)
        :claims (plist-get row :claims)
        :panel (plist-get row :panel)
        :tags (plist-get row :tags)
        :meta (plist-get row :meta)))

(defun anvil-fusion-traj--default-db ()
  "Return the cached SQLite DB, or the symbol `memory' on fallback."
  (let ((path (expand-file-name anvil-fusion-traj-db-path)))
    (when (and anvil-fusion-traj--default-db
               (not (equal path anvil-fusion-traj--resolved-db-path)))
      (ignore-errors (sqlite-close anvil-fusion-traj--default-db))
      (setq anvil-fusion-traj--default-db nil
            anvil-fusion-traj--resolved-db-path nil))
    (or anvil-fusion-traj--default-db
        (condition-case err
            (progn
              (setq anvil-fusion-traj--default-db
                    (anvil-fusion-traj--sqlite-open path)
                    anvil-fusion-traj--resolved-db-path path)
              anvil-fusion-traj--default-db)
          (error
           (message "anvil-fusion-traj: using in-memory fallback (%s)"
                    (error-message-string err))
           'memory)))))

(defun anvil-fusion-traj--sqlite-row->plist (row)
  "Return public plist decoded from SQLite ROW."
  (cl-destructuring-bind (id created question answer claims panel tags meta) row
    (list :id id
          :created created
          :question question
          :answer answer
          :claims (anvil-fusion-traj--read-sexp claims)
          :panel panel
          :tags (anvil-fusion-traj--read-sexp tags)
          :meta (anvil-fusion-traj--read-sexp meta))))

(defun anvil-fusion-traj--with-sqlite-tx (db fn)
  "Run FN inside a raw SQLite transaction on DB."
  (sqlite-execute db "BEGIN IMMEDIATE")
  (condition-case err
      (prog1 (funcall fn)
        (sqlite-execute db "COMMIT"))
    (error
     (ignore-errors (sqlite-execute db "ROLLBACK"))
     (signal (car err) (cdr err)))))

(defun anvil-fusion-traj--rejection-reason (result)
  "Return a rejection reason string for RESULT, or nil when admissible."
  (cond
   ((not (anvil-fusion-traj--nonempty-string-p (plist-get result :answer)))
    "empty answer")
   ((null (plist-get result :claims))
    "missing claims")
   ((not (cl-some (lambda (claim)
                    (eq (plist-get claim :verdict) 'confirmed))
                  (plist-get result :claims)))
    "no confirmed claims")
   ((cl-some (lambda (claim)
               (eq (plist-get claim :verdict) 'refuted))
             (plist-get result :claims))
    "refuted claims present")
   (t nil)))

(defun anvil-fusion-traj-admit-p (result)
  "Return non-nil when RESULT is a clean verified win worth storing.
The shipped policy requires a non-empty answer, non-nil claims, at
least one confirmed claim, and zero refuted claims."
  (null (anvil-fusion-traj--rejection-reason result)))

(cl-defun anvil-fusion-traj-store (question result &key tags force)
  "Store QUESTION + RESULT in the verified trajectory library.
Returns the stored id, or nil on rejection/error.  TAGS is stored as a
printed sexp when non-nil.  FORCE is an escape hatch: it bypasses the
verification gate and admits any RESULT with a non-empty answer."
  (let* ((answer (plist-get result :answer))
         (reason (unless force
                   (anvil-fusion-traj--rejection-reason result))))
    (cond
     ((not (anvil-fusion-traj--nonempty-string-p answer))
      (message "anvil-fusion-traj-store: rejected (%s)" "empty answer")
      nil)
     (reason
      (message "anvil-fusion-traj-store: rejected (%s)" reason)
      nil)
     (t
      (let* ((backend (anvil-fusion-traj--default-db))
             (created (float-time))
             (claims (anvil-fusion-traj--print-sexp (plist-get result :claims)))
             (panel (let ((p (plist-get result :panel)))
                      (and p (format "%s" p))))
             (tags-text (anvil-fusion-traj--print-sexp tags))
             (meta (anvil-fusion-traj--print-sexp
                    (anvil-fusion-traj--strip-result-meta result))))
        (condition-case err
            (if (eq backend 'memory)
                (let ((id anvil-fusion-traj--memory-next-id)
                      (row nil))
                  (setq anvil-fusion-traj--memory-next-id (1+ id))
                  (setq row (list :id id :created created
                                  :question question :answer answer
                                  :claims (plist-get result :claims)
                                  :panel panel :tags tags :meta
                                  (anvil-fusion-traj--strip-result-meta result)))
                  (push row anvil-fusion-traj--memory-store)
                  id)
              (anvil-fusion-traj--with-sqlite-tx
               backend
               (lambda ()
                 (sqlite-execute
                  backend
                  (concat "INSERT INTO traj "
                          "(created,question,answer,claims,panel,tags,meta) "
                          "VALUES (?,?,?,?,?,?,?)")
                  (list created question answer claims panel tags-text meta))
                 (let ((id (caar (sqlite-select
                                  backend "SELECT last_insert_rowid()"))))
                   (sqlite-execute
                    backend
                    "INSERT INTO traj_fts(rowid,id,question,answer) VALUES (?,?,?,?)"
                    (list id id question answer))
                   id))))
          (error
           (message "anvil-fusion-traj-store: %s" (error-message-string err))
           nil)))))))

(cl-defun anvil-fusion-traj-search (query &key (k 3))
  "Return up to K exemplar hits for QUERY, or nil when QUERY is empty.
Each hit is a plist with :id :score :question and :answer-head."
  (when (anvil-fusion-traj--nonempty-string-p query)
    (let ((backend (anvil-fusion-traj--default-db))
          (limit (max 1 (or k 3))))
      (condition-case err
          (if (eq backend 'memory)
              (seq-take
               (sort
                (mapcar
                 (lambda (row)
                   (list :id (plist-get row :id)
                         :score (if (fboundp 'anvil-fusion--jaccard)
                                    (anvil-fusion--jaccard query (plist-get row :question))
                                  0.0)
                         :question (plist-get row :question)
                         :answer-head (anvil-fusion-traj--head
                                       (plist-get row :answer) 200)))
                 anvil-fusion-traj--memory-store)
                (lambda (a b) (> (plist-get a :score) (plist-get b :score))))
               limit)
            (let ((rows
                   (or (ignore-errors
                         (sqlite-select
                          backend
                          (concat "SELECT id, bm25(traj_fts), question, answer "
                                  "FROM traj_fts WHERE traj_fts MATCH ? "
                                  "ORDER BY bm25(traj_fts), rowid LIMIT ?")
                          (list query limit)))
                       (sqlite-select
                        backend
                        (concat "SELECT id, rowid, question, answer "
                                "FROM traj_fts WHERE traj_fts MATCH ? "
                                "ORDER BY rowid LIMIT ?")
                        (list query limit)))))
              (mapcar
               (lambda (row)
                 (cl-destructuring-bind (id score question answer) row
                   (list :id id
                         :score score
                         :question question
                         :answer-head (anvil-fusion-traj--head answer 200))))
               rows)))
        (error
         (message "anvil-fusion-traj-search: %s" (error-message-string err))
         nil)))))

(defun anvil-fusion-traj-get (id)
  "Return full trajectory row plist for ID, or nil when absent."
  (let ((backend (anvil-fusion-traj--default-db)))
    (condition-case err
        (if (eq backend 'memory)
            (when-let ((row (seq-find (lambda (it) (= (plist-get it :id) id))
                                      anvil-fusion-traj--memory-store)))
              (anvil-fusion-traj--memory-row->plist row))
          (when-let ((row (car (sqlite-select
                                backend
                                (concat "SELECT id,created,question,answer,"
                                        "claims,panel,tags,meta "
                                        "FROM traj WHERE id=?")
                                (list id)))))
            (anvil-fusion-traj--sqlite-row->plist row)))
      (error
       (message "anvil-fusion-traj-get: %s" (error-message-string err))
       nil))))

(defun anvil-fusion-traj-count ()
  "Return the number of stored trajectories."
  (let ((backend (anvil-fusion-traj--default-db)))
    (condition-case err
        (if (eq backend 'memory)
            (length anvil-fusion-traj--memory-store)
          (or (caar (sqlite-select backend "SELECT COUNT(*) FROM traj")) 0))
      (error
       (message "anvil-fusion-traj-count: %s" (error-message-string err))
       0))))

(defun anvil-fusion-traj-delete (id)
  "Delete trajectory ID.  Return non-nil when something changed."
  (let ((backend (anvil-fusion-traj--default-db)))
    (condition-case err
        (if (eq backend 'memory)
            (let ((before (length anvil-fusion-traj--memory-store)))
              (setq anvil-fusion-traj--memory-store
                    (seq-remove (lambda (row) (= (plist-get row :id) id))
                                anvil-fusion-traj--memory-store))
              (< (length anvil-fusion-traj--memory-store) before))
          (anvil-fusion-traj--with-sqlite-tx
           backend
           (lambda ()
             (sqlite-execute backend "DELETE FROM traj WHERE id=?" (list id))
             (sqlite-execute backend "DELETE FROM traj_fts WHERE rowid=?" (list id))
             (> (or (caar (sqlite-select backend "SELECT changes()")) 0) 0))))
      (error
       (message "anvil-fusion-traj-delete: %s" (error-message-string err))
       nil))))

(cl-defun anvil-fusion-traj-exemplar-block (query &key (k 2) (max-chars 3000))
  "Return a Japanese exemplar block for QUERY, or nil when no hits fit."
  (when (anvil-fusion-traj--nonempty-string-p query)
    (let* ((header "# 過去の検証済み事例（参考。事実確認済みの回答例）\n")
           (room (max 0 (or max-chars 3000)))
           (hits (anvil-fusion-traj-search query :k k))
           (block header))
      (when (<= (length header) room)
        (dolist (hit hits)
          (when-let* ((row (anvil-fusion-traj-get (plist-get hit :id)))
                      (entry
                       (format "## 事例: %s\n%s\n"
                               (anvil-fusion-traj--head
                                (plist-get row :question) 80)
                               (anvil-fusion-traj--head
                                (plist-get row :answer) 1200))))
            (when (<= (+ (length block) (length entry)) room)
              (setq block (concat block entry)))))
        (unless (equal block header)
          block)))))

(provide 'anvil-fusion-traj)
;;; anvil-fusion-traj.el ends here
