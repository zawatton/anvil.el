;;; anvil-semantic.el --- Local full-text (FTS5) search MCP tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; Author: zawatton
;; Keywords: tools, mcp, claude, search
;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; anvil-audit: tools-wrapped-at-registration

;;; Commentary:

;; A pure-Emacs-Lisp retrieval engine over a configurable corpus of org
;; / text / code files, backed by the built-in SQLite FTS5 full-text
;; index (Emacs 29+).  No Python, no embedding SDK, no cloud — the
;; content stays local and only the calling agent (Claude) reads the
;; returned excerpts.
;;
;; This replaces the earlier `anvil-semantic.el' that shelled out to
;; tools/semantic-search/semantic.py.  The two MCP tool ids are kept for
;; continuity:
;;
;;   semantic-search       Hybrid relevance search.  Combines FTS5 BM25
;;                         ranking with lexical term-overlap (RRF fused).
;;                         "Semantic" here means meaning-via-retrieval:
;;                         there are no vector embeddings; the agent does
;;                         the conceptual matching by reading excerpts.
;;   notes-lexical-search  Term-overlap search with caller-supplied
;;                         synonym expansion (`terms'), tuned for the
;;                         retrieve-then-read flow.
;;
;; Design notes:
;;
;; * Tokenizer.  The FTS5 `trigram' tokenizer (SQLite 3.34+) gives
;;   substring matching over CJK text — e.g. 漏洩電流 matches a query of
;;   漏電検出 via shared trigrams — which the ASCII-only default
;;   (`unicode61') cannot.  `auto' probes the build and falls back to
;;   `unicode61' when trigram is unavailable.  Mirrors `anvil-memory'.
;;
;; * The 2-char gap.  Trigram MATCH needs at least 3 characters, so
;;   2-char kanji compounds (整定, 漏電, 接地 …) — extremely common in
;;   Japanese — would be invisible to a pure MATCH query.  Retrieval
;;   therefore unions an FTS5 MATCH pass (>=3-char terms; fast, indexed,
;;   BM25-ranked) with a LIKE-scan pass (2-char terms; correct), then
;;   ranks the merged candidate set in Lisp.
;;
;; * Embeddings (opt-in vector layer).  A local-embedding hybrid layer
;;   joins a `chunk_vec' table on the stored `digest'.  Build it with
;;   the `semantic-embed-index' tool (provider selected by
;;   `anvil-semantic-embed-provider': `ollama' by default = fully
;;   local, `gemini' / `openai' are opt-in cloud).  Search then fuses
;;   three signals — FTS5 BM25, lexical term-overlap and embedding
;;   cosine — via RRF.  With an empty `chunk_vec' table the engine
;;   behaves exactly as the pure-FTS5 core, so the vector layer stays
;;   inert until you build it.  See Doc 18 (Option B).
;;
;; Registered under server-id "emacs-eval" (same as `anvil-file' and
;; `anvil-org-index') so the emacs-eval stdio client sees the tools.
;; Add `semantic' to `anvil-optional-modules' and run `anvil-enable'
;; (or restart the daemon) to activate, then build the index once with
;; the `semantic-reindex' tool (or M-x `anvil-semantic-rebuild').

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'anvil-server)   ; `anvil-server-register-tool', `anvil-server-with-error-handling'

;;; Customization

(defgroup anvil-semantic nil
  "Anvil local full-text search."
  :group 'anvil
  :prefix "anvil-semantic-")

(defconst anvil-semantic--server-id "emacs-eval"
  "MCP server id this module registers its tools under.")

(defcustom anvil-semantic-db-path
  (expand-file-name ".anvil-semantic/index.db" user-emacs-directory)
  "Path to the SQLite FTS5 index database file."
  :type 'file
  :group 'anvil-semantic)

(defcustom anvil-semantic-roots nil
  "List of directories whose files are indexed for search.
Each entry is a directory path; it is walked recursively for files
whose name matches `anvil-semantic-file-regexp'.  Non-existent
entries are skipped with a message.  Empty by default — set this to
the corpora you want searchable, e.g.

  (setq anvil-semantic-roots \\='(\"~/Cowork/Notes/node\"
                                \"~/Cowork/Notes/capture\"))"
  :type '(repeat directory)
  :group 'anvil-semantic)

(defcustom anvil-semantic-file-regexp
  "\\.\\(org\\|md\\|markdown\\|txt\\|rst\\|tex\\|el\\|py\\|js\\|ts\\|tsx\\|jsx\\|c\\|h\\|cc\\|cpp\\|hpp\\|rs\\|go\\|java\\|rb\\|sh\\|sql\\|json\\|ya?ml\\|toml\\)\\'"
  "Regexp matched against absolute file names to decide what to index.
Files under `anvil-semantic-roots' whose name matches are ingested;
`.org' files are chunked by heading, everything else by line window."
  :type 'regexp
  :group 'anvil-semantic)

(defcustom anvil-semantic-exclude-patterns
  '("/\\.git/" "/archive/" "/\\.cache/" "/node_modules/"
    "/\\.anvil-" "/photos/")
  "Regexps for absolute paths to exclude from indexing."
  :type '(repeat regexp)
  :group 'anvil-semantic)

(defcustom anvil-semantic-fts-tokenizer 'auto
  "Tokenizer used when creating the `chunk_fts' FTS5 table.

  `auto'      Probe the SQLite build: use `trigram' when the FTS5
              trigram tokenizer is available (SQLite 3.34+), else
              fall back to `unicode61'.
  `trigram'   Force trigram (substring/CJX matching, needs 3+ chars).
  `unicode61' Force the SQLite default tokenizer (ASCII word tokens).

Changing this on an existing DB has no effect until the table is
rebuilt — call `anvil-semantic-reindex'."
  :type '(choice (const :tag "Auto (trigram if available)" auto)
                 (const :tag "Trigram (SQLite 3.34+)" trigram)
                 (const :tag "unicode61 (default)" unicode61))
  :group 'anvil-semantic)

(defcustom anvil-semantic-max-chars 6000
  "Truncate a chunk's stored text to at most this many characters."
  :type 'integer
  :group 'anvil-semantic)

(defcustom anvil-semantic-min-chars 12
  "Skip chunks whose body is shorter than this many characters."
  :type 'integer
  :group 'anvil-semantic)

(defcustom anvil-semantic-chunk-lines 50
  "Line-window size for chunking non-org (plain text / code) files."
  :type 'integer
  :group 'anvil-semantic)

(defcustom anvil-semantic-default-k 8
  "Default number of hits returned when the caller omits `k'."
  :type 'integer
  :group 'anvil-semantic)

(defcustom anvil-semantic-lexical-default-k 15
  "Default number of hits for `notes-lexical-search'."
  :type 'integer
  :group 'anvil-semantic)

(defcustom anvil-semantic-excerpt-chars 600
  "Maximum excerpt length for `notes-lexical-search' results."
  :type 'integer
  :group 'anvil-semantic)

(defcustom anvil-semantic-snippet-chars 240
  "Maximum snippet length for `semantic-search' results."
  :type 'integer
  :group 'anvil-semantic)

(defcustom anvil-semantic-candidate-limit 400
  "Maximum candidate rows fetched per retrieval pass before ranking.
Bounds the cost of the LIKE-scan fallback and the in-Lisp re-rank."
  :type 'integer
  :group 'anvil-semantic)

;;; Embedding layer (opt-in vector search) — customization

(defcustom anvil-semantic-embed-provider 'ollama
  "Embedding backend for vector (meaning-based) search.

  `ollama'  Local HTTP embeddings (default): chunk text never leaves the
            machine.  Requires an ollama server at
            `anvil-semantic-ollama-host'.
  `gemini'  Google Generative Language `batchEmbedContents' (cloud — sends
            chunk text off-machine; opt-in only).
  `openai'  OpenAI embeddings endpoint (cloud; opt-in only).

The vector layer is inert until you build the index with the
`semantic-embed-index' tool (or M-x `anvil-semantic-embed-index'); with an
empty `chunk_vec' table search behaves exactly as the pure-FTS5 core, so
the local-only default is preserved until you opt in."
  :type '(choice (const :tag "ollama (local, default)" ollama)
                 (const :tag "Gemini (cloud, opt-in)" gemini)
                 (const :tag "OpenAI (cloud, opt-in)" openai))
  :group 'anvil-semantic)

(defcustom anvil-semantic-embed-model nil
  "Embedding model name, or nil to use the provider default.
Defaults: ollama = \"nomic-embed-text\", gemini =
\"models/gemini-embedding-001\", openai = \"text-embedding-3-small\"."
  :type '(choice (const :tag "Provider default" nil) string)
  :group 'anvil-semantic)

(defcustom anvil-semantic-ollama-host "http://localhost:11434"
  "Base URL of the local ollama server used when provider is `ollama'."
  :type 'string
  :group 'anvil-semantic)

(defcustom anvil-semantic-embed-dim 768
  "Requested output dimensionality for providers that support it
\(gemini `outputDimensionality', openai `dimensions').  Ignored by ollama,
which returns its model's native dimension."
  :type 'integer
  :group 'anvil-semantic)

(defcustom anvil-semantic-embed-batch 64
  "Number of chunks embedded per provider request during indexing."
  :type 'integer
  :group 'anvil-semantic)

(defcustom anvil-semantic-embed-timeout 120
  "Per-request timeout (seconds) for embedding HTTP calls."
  :type 'integer
  :group 'anvil-semantic)

(defcustom anvil-semantic-embed-max-retries 4
  "Maximum retries (exponential backoff) per embedding request on
transient network / API errors."
  :type 'integer
  :group 'anvil-semantic)

;;; Database handle

(defvar anvil-semantic--db nil
  "Open SQLite database handle, or nil.")

(defun anvil-semantic--available-p ()
  "Return non-nil when built-in SQLite is usable."
  (and (fboundp 'sqlite-available-p) (sqlite-available-p)))

(defun anvil-semantic--ensure-db ()
  "Open the index DB (creating schema) if not already open.  Return it."
  (unless (anvil-semantic--available-p)
    (user-error "anvil-semantic: built-in SQLite (Emacs 29+) is required"))
  (unless (and anvil-semantic--db (sqlitep anvil-semantic--db))
    (let ((dir (file-name-directory anvil-semantic-db-path)))
      (unless (file-directory-p dir) (make-directory dir t)))
    (setq anvil-semantic--db (sqlite-open anvil-semantic-db-path))
    (anvil-semantic--apply-schema anvil-semantic--db))
  anvil-semantic--db)

(defmacro anvil-semantic--with-transaction (db &rest body)
  "Run BODY inside a BEGIN/COMMIT transaction on DB, ROLLBACK on error."
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

;;; Tokenizer selection

(defun anvil-semantic--supports-trigram-p (db)
  "Return non-nil when DB's SQLite build ships the FTS5 trigram tokenizer."
  (condition-case nil
      (progn
        (sqlite-execute
         db "CREATE VIRTUAL TABLE anvil_semantic_trigram_probe
               USING fts5(x, tokenize=trigram)")
        (sqlite-execute db "DROP TABLE anvil_semantic_trigram_probe")
        t)
    (error nil)))

(defun anvil-semantic--resolve-tokenizer (db)
  "Return the tokenizer symbol (`trigram' / `unicode61') DB should use."
  (pcase anvil-semantic-fts-tokenizer
    ('trigram 'trigram)
    ('unicode61 'unicode61)
    ('auto (if (anvil-semantic--supports-trigram-p db) 'trigram 'unicode61))
    (other (user-error "anvil-semantic: invalid tokenizer %S" other))))

;;; Schema

(defun anvil-semantic--apply-schema (db)
  "Apply pragmas and create the meta + FTS5 tables on DB if absent."
  (sqlite-execute db "PRAGMA journal_mode = WAL")
  (sqlite-execute
   db "CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT)")
  (unless (anvil-semantic--fts-exists-p db)
    (anvil-semantic--create-fts db (anvil-semantic--resolve-tokenizer db)))
  (anvil-semantic--ensure-vec-schema db))

(defun anvil-semantic--fts-exists-p (db)
  "Return non-nil when the `chunk_fts' table exists in DB."
  (caar (sqlite-select
         db "SELECT 1 FROM sqlite_master WHERE type='table' AND name='chunk_fts'")))

(defun anvil-semantic--create-fts (db tokenizer)
  "Create the `chunk_fts' FTS5 table in DB using TOKENIZER, record it."
  ;; Only `text' is indexed; the rest are UNINDEXED metadata columns,
  ;; retrievable but excluded from MATCH and from bm25() scoring.
  (sqlite-execute
   db (format "CREATE VIRTUAL TABLE chunk_fts USING fts5(
                 file UNINDEXED,
                 heading UNINDEXED,
                 start_line UNINDEXED,
                 digest UNINDEXED,
                 mtime UNINDEXED,
                 text,
                 tokenize = %s)"
              (symbol-name tokenizer)))
  (sqlite-execute
   db "INSERT OR REPLACE INTO meta(key, value) VALUES ('tokenizer', ?)"
   (list (symbol-name tokenizer))))

(defun anvil-semantic--current-tokenizer (db)
  "Return the tokenizer symbol recorded for DB's FTS table, or nil."
  (let ((v (caar (sqlite-select
                  db "SELECT value FROM meta WHERE key='tokenizer'"))))
    (and (stringp v) (intern v))))

;;; Embedding schema + availability gate

(defun anvil-semantic--embed-model ()
  "Return the active embedding model name (configured or provider default)."
  (or (and (stringp anvil-semantic-embed-model)
           (not (string-empty-p anvil-semantic-embed-model))
           anvil-semantic-embed-model)
      (pcase anvil-semantic-embed-provider
        ('ollama "nomic-embed-text")
        ('gemini "models/gemini-embedding-001")
        ('openai "text-embedding-3-small")
        (p (user-error "anvil-semantic: unknown embed provider %S" p)))))

(defun anvil-semantic--ensure-vec-schema (db)
  "Create the `chunk_vec' embedding table + model index on DB if absent.
Vectors are stored normalized as a `prin1' float vector in a TEXT column,
so cosine similarity reduces to a plain dot product; `file' is
denormalized in so the root-prefix filter need not join `chunk_fts'."
  (sqlite-execute
   db "CREATE TABLE IF NOT EXISTS chunk_vec(
         digest TEXT,
         model  TEXT,
         file   TEXT,
         dim    INTEGER,
         vec    TEXT,
         PRIMARY KEY (digest, model))")
  (sqlite-execute
   db "CREATE INDEX IF NOT EXISTS idx_chunk_vec_model ON chunk_vec(model)"))

(defun anvil-semantic--vec-table-exists-p (db)
  "Return non-nil when the `chunk_vec' table exists in DB."
  (caar (sqlite-select
         db "SELECT 1 FROM sqlite_master WHERE type='table' AND name='chunk_vec'")))

(defun anvil-semantic--embed-available-p (db &optional model)
  "Return non-nil when DB holds at least one embedding row for MODEL.
MODEL defaults to the active `anvil-semantic--embed-model'.  This is the
opt-in gate: an empty `chunk_vec' keeps search on the pure-FTS5 path."
  (and (anvil-semantic--vec-table-exists-p db)
       (let ((n (caar (sqlite-select
                       db "SELECT COUNT(*) FROM chunk_vec WHERE model = ?"
                       (list (or model (anvil-semantic--embed-model)))))))
         (and (numberp n) (> n 0)))))

;;; Term extraction

(defconst anvil-semantic--term-regexp
  "[A-Za-z0-9][A-Za-z0-9_-]+\\|[ァ-ヶー]\\{2,\\}\\|[一-鿿々]\\{2,\\}"
  "Salient surface terms: alnum identifiers, katakana runs, kanji runs.
Mirrors the Python backend's TERM_RE — hiragana runs are excluded as
low-signal.")

(defun anvil-semantic--terms (query)
  "Return a de-duplicated list of lowercased salient terms in QUERY."
  (let ((case-fold-search nil)
        (start 0) (acc nil))
    (while (string-match anvil-semantic--term-regexp query start)
      (let ((m (downcase (match-string 0 query))))
        (when (>= (length m) 2) (cl-pushnew m acc :test #'string=)))
      (setq start (match-end 0)))
    (nreverse acc)))

(defun anvil-semantic--merge-terms (query extra)
  "Return salient terms from QUERY plus comma-separated EXTRA synonyms."
  (let ((terms (anvil-semantic--terms query)))
    (when (and (stringp extra) (not (string-empty-p extra)))
      (dolist (raw (split-string extra "," t "[ \t]+"))
        (let ((tm (downcase (string-trim raw))))
          (when (and (>= (length tm) 2)
                     (not (member tm terms)))
            (setq terms (append terms (list tm)))))))
    terms))

;;; Chunking

(defun anvil-semantic--file-lines (path)
  "Return the lines of PATH (decoded as UTF-8) as a list of strings."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (split-string (buffer-string) "\n")))

(defun anvil-semantic--clip (s)
  "Truncate string S to `anvil-semantic-max-chars'."
  (if (> (length s) anvil-semantic-max-chars)
      (substring s 0 anvil-semantic-max-chars)
    s))

(defun anvil-semantic--chunk-org (path)
  "Chunk org file PATH by heading.
Return a list of (HEADING START-LINE TEXT); each TEXT is prefixed
with a `title :: breadcrumb' context line."
  (let* ((lines (anvil-semantic--file-lines path))
         (title (file-name-base path))
         (chunks nil)
         (stack nil)                    ; list of (LEVEL . HEADING), shallow→deep
         (cur-head "(preamble)")
         (cur-start 1)
         (cur-lines nil)                ; accumulated in reverse
         (idx 0))
    ;; Title from a #+TITLE: line in the first 40 lines.
    (let ((case-fold-search t) (n 0))
      (catch 'found
        (dolist (ln lines)
          (when (>= n 40) (throw 'found nil))
          (when (string-match "\\`#\\+TITLE:[ \t]*\\(.*\\)" ln)
            (let ((tt (string-trim (match-string 1 ln))))
              (unless (string-empty-p tt) (setq title tt)))
            (throw 'found nil))
          (setq n (1+ n)))))
    (cl-flet ((flush ()
                (let ((body (string-trim
                             (mapconcat #'identity (nreverse cur-lines) "\n"))))
                  (setq cur-lines nil)
                  (when (>= (length body) anvil-semantic-min-chars)
                    (let* ((crumb (mapconcat #'cdr stack " / "))
                           (ctx (string-trim (concat title " :: " crumb)
                                             " :"))
                           (text (anvil-semantic--clip (concat ctx "\n" body))))
                      (push (list cur-head cur-start text) chunks))))))
      (dolist (ln lines)
        (setq idx (1+ idx))
        (if (string-match "\\`\\(\\*+\\)[ \t]+\\(.*\\)" ln)
            (let ((level (length (match-string 1 ln)))
                  (head (string-trim (match-string 2 ln))))
              (flush)
              (setq stack (cl-remove-if-not (lambda (s) (< (car s) level)) stack))
              (setq stack (append stack (list (cons level head))))
              (setq cur-head head cur-start idx cur-lines (list ln)))
          (push ln cur-lines)))
      (flush))
    (nreverse chunks)))

(defun anvil-semantic--chunk-text (path)
  "Chunk a non-org file PATH into fixed line windows.
Return a list of (HEADING START-LINE TEXT) with HEADING nil."
  (let* ((lines (anvil-semantic--file-lines path))
         (total (length lines))
         (win (max 1 anvil-semantic-chunk-lines))
         (chunks nil)
         (i 0))
    (while (< i total)
      (let* ((slice (cl-subseq lines i (min total (+ i win))))
             (body (string-trim (mapconcat #'identity slice "\n"))))
        (when (>= (length body) anvil-semantic-min-chars)
          (push (list nil (1+ i) (anvil-semantic--clip body)) chunks)))
      (setq i (+ i win)))
    (nreverse chunks)))

(defun anvil-semantic--chunk-file (path)
  "Dispatch PATH to the org or plain-text chunker by extension."
  (if (string-match-p "\\.org\\'" path)
      (anvil-semantic--chunk-org path)
    (anvil-semantic--chunk-text path)))

;;; File discovery

(defun anvil-semantic--excluded-p (path)
  "Return non-nil if PATH matches any `anvil-semantic-exclude-patterns'."
  (cl-some (lambda (re) (string-match-p re path))
           anvil-semantic-exclude-patterns))

(defun anvil-semantic--collect-files (&optional roots)
  "Return absolute paths of indexable files under ROOTS.
ROOTS defaults to `anvil-semantic-roots'."
  (let ((acc nil))
    (dolist (root (or roots anvil-semantic-roots))
      (let ((abs (expand-file-name root)))
        (cond
         ((file-regular-p abs)
          (when (and (string-match-p anvil-semantic-file-regexp abs)
                     (not (anvil-semantic--excluded-p abs)))
            (push abs acc)))
         ((file-directory-p abs)
          (dolist (f (directory-files-recursively
                      abs anvil-semantic-file-regexp nil))
            (let ((ef (expand-file-name f)))
              (unless (anvil-semantic--excluded-p ef) (push ef acc)))))
         (t (message "anvil-semantic: skipping missing root %s" abs)))))
    (nreverse (delete-dups acc))))

;;; Ingestion

(defun anvil-semantic--digest (file head text)
  "Return a stable SHA1 identity for a chunk (FILE HEAD TEXT)."
  (secure-hash 'sha1 (concat file "\0" (or head "") "\0" text)))

(defun anvil-semantic--mtime (path)
  "Return PATH's modification time as integer seconds, or 0."
  (let ((attrs (file-attributes path)))
    (if attrs (floor (float-time (file-attribute-modification-time attrs))) 0)))

(defun anvil-semantic--delete-file-rows (db file)
  "Remove all indexed chunks for FILE from DB."
  (sqlite-execute db "DELETE FROM chunk_fts WHERE file = ?" (list file)))

(defun anvil-semantic--ingest-file (db path)
  "Insert chunks for PATH into DB.  Return the number of chunks inserted."
  (let ((mtime (anvil-semantic--mtime path))
        (n 0))
    (dolist (chunk (anvil-semantic--chunk-file path))
      (let* ((head (nth 0 chunk))
             (start (nth 1 chunk))
             (text (nth 2 chunk))
             (digest (anvil-semantic--digest path head text)))
        (sqlite-execute
         db "INSERT INTO chunk_fts(file, heading, start_line, digest, mtime, text)
              VALUES (?,?,?,?,?,?)"
         (list path head start digest mtime text))
        (setq n (1+ n))))
    n))

;;; Public: rebuild / refresh

;;;###autoload
(defun anvil-semantic-rebuild (&optional roots)
  "Rebuild the whole index from scratch over ROOTS.
ROOTS defaults to `anvil-semantic-roots'.  Returns a summary plist."
  (interactive)
  (let* ((db (anvil-semantic--ensure-db))
         (files (anvil-semantic--collect-files roots))
         (start (float-time))
         (chunks 0))
    (anvil-semantic--with-transaction db
      (sqlite-execute db "DELETE FROM chunk_fts")
      (dolist (f files)
        (cl-incf chunks (anvil-semantic--ingest-file db f))))
    (let ((elapsed (- (float-time) start)))
      (message "anvil-semantic: indexed %d chunk(s) from %d file(s) in %.2fs"
               chunks (length files) elapsed)
      (list :files (length files) :chunks chunks :elapsed-sec elapsed))))

;;;###autoload
(defun anvil-semantic-refresh-if-stale (&optional roots)
  "Incrementally refresh the index by mtime over ROOTS.
Adds new files, re-ingests changed ones, drops rows for files no
longer on disk.  Returns a summary plist."
  (interactive)
  (let* ((db (anvil-semantic--ensure-db))
         (start (float-time))
         (disk (anvil-semantic--collect-files roots))
         (disk-set (let ((h (make-hash-table :test 'equal)))
                     (dolist (f disk) (puthash f t h)) h))
         (idx (let ((h (make-hash-table :test 'equal)))
                (dolist (row (sqlite-select
                              db "SELECT DISTINCT file, mtime FROM chunk_fts"))
                  (puthash (car row)
                           (if (numberp (cadr row)) (cadr row)
                             (string-to-number (format "%s" (cadr row))))
                           h))
                h))
         (added 0) (reindexed 0) (removed 0) (unchanged 0))
    (anvil-semantic--with-transaction db
      (dolist (f disk)
        (let ((disk-mt (anvil-semantic--mtime f))
              (idx-mt (gethash f idx)))
          (cond
           ((null idx-mt)
            (anvil-semantic--ingest-file db f) (cl-incf added))
           ((/= disk-mt idx-mt)
            (anvil-semantic--delete-file-rows db f)
            (anvil-semantic--ingest-file db f) (cl-incf reindexed))
           (t (cl-incf unchanged)))))
      (maphash (lambda (f _mt)
                 (unless (gethash f disk-set)
                   (anvil-semantic--delete-file-rows db f)
                   (cl-incf removed)))
               idx))
    (let ((elapsed (- (float-time) start)))
      (message "anvil-semantic: refresh +%d ~%d -%d =%d in %.2fs"
               added reindexed removed unchanged elapsed)
      (list :scanned (length disk) :added added :reindexed reindexed
            :removed removed :unchanged unchanged :elapsed-sec elapsed))))

;;;###autoload
(defun anvil-semantic-reindex (&optional tokenizer)
  "Drop and rebuild the FTS table, optionally switching TOKENIZER.
TOKENIZER is a symbol (`trigram' / `unicode61' / `auto'); nil keeps
`anvil-semantic-fts-tokenizer'.  Then rebuilds the corpus index."
  (interactive)
  (let ((db (anvil-semantic--ensure-db))
        (anvil-semantic-fts-tokenizer
         (or tokenizer anvil-semantic-fts-tokenizer)))
    (sqlite-execute db "DROP TABLE IF EXISTS chunk_fts")
    (anvil-semantic--create-fts db (anvil-semantic--resolve-tokenizer db))
    (anvil-semantic-rebuild)))

;;; Embedding providers (opt-in vector layer)

(declare-function anvil-http-post "anvil-http")

(defun anvil-semantic--api-key (provider)
  "Return the API key for cloud PROVIDER, or signal a `user-error'.
Looked up first in the env var (GEMINI_API_KEY / OPENAI_API_KEY), then in
<db-dir>/<provider>.key.  Never needed for the `ollama' provider."
  (let* ((env (pcase provider
                ('gemini "GEMINI_API_KEY")
                ('openai "OPENAI_API_KEY")
                (_ nil)))
         (val (and env (getenv env)))
         (kf (expand-file-name (format "%s.key" provider)
                               (file-name-directory anvil-semantic-db-path))))
    (cond
     ((and val (not (string-empty-p (string-trim val)))) (string-trim val))
     ((file-readable-p kf)
      (string-trim (with-temp-buffer (insert-file-contents kf) (buffer-string))))
     (t (user-error "anvil-semantic: no API key for %s (set %s env or write %s)"
                    provider (or env "<env>") kf)))))

(defun anvil-semantic--http-json (url payload &optional bearer)
  "POST PAYLOAD (a json.el-encodable value) as JSON to URL.
Return the parsed response as an alist (JSON arrays as vectors).  BEARER,
when non-nil, is sent as an Authorization: Bearer header.  Retries
transient failures with exponential backoff up to
`anvil-semantic-embed-max-retries'."
  (require 'anvil-http)
  (let ((body (json-encode payload))
        (attempt 0)
        (last-err nil))
    (catch 'done
      (while (<= attempt anvil-semantic-embed-max-retries)
        (condition-case err
            (let* ((resp (anvil-http-post
                          url
                          :body body
                          :content-type "application/json"
                          :auth (and bearer (list :bearer bearer))
                          :timeout-sec anvil-semantic-embed-timeout
                          :skip-robots-check t))
                   (status (plist-get resp :status))
                   (rbody (or (plist-get resp :body) "")))
              (if (and (integerp status) (<= 200 status 299))
                  (throw 'done (json-parse-string rbody
                                                  :object-type 'alist
                                                  :array-type 'array))
                (error "HTTP %s: %s" status
                       (substring rbody 0 (min 300 (length rbody))))))
          (error
           (setq last-err err)
           (if (>= attempt anvil-semantic-embed-max-retries)
               (signal (car err) (cdr err))
             (let ((wait (expt 2 attempt)))
               (message "anvil-semantic: embed retry %d (%s); sleeping %ds"
                        attempt (error-message-string err) wait)
               (sleep-for wait))
             (setq attempt (1+ attempt))))))
      (when last-err (signal (car last-err) (cdr last-err))))))

;;;; Vector math + (de)serialization

(defun anvil-semantic--to-fvec (seq)
  "Coerce SEQ (a vector/list of numbers) to a fresh vector of floats."
  (let* ((lst (append seq nil))
         (out (make-vector (length lst) 0.0))
         (i 0))
    (dolist (x lst out) (aset out i (float x)) (setq i (1+ i)))))

(defun anvil-semantic--normalize (v)
  "Return V scaled to unit L2 norm (a fresh float vector)."
  (let ((n (length v)) (sum 0.0))
    (dotimes (i n) (setq sum (+ sum (* (aref v i) (aref v i)))))
    (let ((norm (sqrt sum)))
      (if (<= norm 1e-12)
          v
        (let ((out (make-vector n 0.0)))
          (dotimes (i n) (aset out i (/ (aref v i) norm)))
          out)))))

(defun anvil-semantic--dot (a b)
  "Dot product of float vectors A and B (cosine when both are unit norm)."
  (let ((n (min (length a) (length b))) (s 0.0))
    (dotimes (i n) (setq s (+ s (* (aref a i) (aref b i)))))
    s))

(defun anvil-semantic--vec->text (v)
  "Serialize float vector V to a `read'-able string for storage."
  (prin1-to-string v))

(defun anvil-semantic--text->vec (s)
  "Parse a stored vector string S (see `anvil-semantic--vec->text')."
  (read s))

;;;; Provider request shapes

(defun anvil-semantic--embed-ollama (texts _task)
  "Embed TEXTS via the local ollama /api/embed endpoint."
  (let* ((url (concat (string-trim-right anvil-semantic-ollama-host "/")
                      "/api/embed"))
         (resp (anvil-semantic--http-json
                url
                (list (cons "model" (anvil-semantic--embed-model))
                      (cons "input" (vconcat texts)))))
         (embs (alist-get 'embeddings resp)))
    (unless (and embs (> (length embs) 0))
      (error "anvil-semantic: ollama returned no embeddings"))
    (mapcar #'anvil-semantic--to-fvec (append embs nil))))

(defun anvil-semantic--embed-gemini (texts task)
  "Embed TEXTS via Gemini batchEmbedContents.  TASK is `document'/`query'."
  (let* ((model (anvil-semantic--embed-model))
         (key (anvil-semantic--api-key 'gemini))
         (url (format "https://generativelanguage.googleapis.com/v1beta/%s:batchEmbedContents?key=%s"
                      model (url-hexify-string key)))
         (tt (if (eq task 'query) "RETRIEVAL_QUERY" "RETRIEVAL_DOCUMENT"))
         (requests (vconcat
                    (mapcar
                     (lambda (txt)
                       (list (cons "model" model)
                             (cons "content"
                                   (list (cons "parts"
                                               (vector (list (cons "text" txt))))))
                             (cons "taskType" tt)
                             (cons "outputDimensionality"
                                   anvil-semantic-embed-dim)))
                     texts)))
         (resp (anvil-semantic--http-json url (list (cons "requests" requests))))
         (embs (alist-get 'embeddings resp)))
    (unless (and embs (> (length embs) 0))
      (error "anvil-semantic: gemini returned no embeddings"))
    (mapcar (lambda (e) (anvil-semantic--to-fvec (alist-get 'values e)))
            (append embs nil))))

(defun anvil-semantic--embed-openai (texts _task)
  "Embed TEXTS via the OpenAI embeddings endpoint."
  (let* ((model (anvil-semantic--embed-model))
         (key (anvil-semantic--api-key 'openai))
         (resp (anvil-semantic--http-json
                "https://api.openai.com/v1/embeddings"
                (list (cons "model" model)
                      (cons "input" (vconcat texts))
                      (cons "dimensions" anvil-semantic-embed-dim))
                key))
         (data (alist-get 'data resp)))
    (unless (and data (> (length data) 0))
      (error "anvil-semantic: openai returned no embeddings"))
    (mapcar (lambda (d) (anvil-semantic--to-fvec (alist-get 'embedding d)))
            (append data nil))))

(defun anvil-semantic--embed (texts task)
  "Return a list of normalized float vectors for TEXTS.
TASK is `document' (indexing) or `query' (search); only Gemini
distinguishes them.  Dispatches on `anvil-semantic-embed-provider'."
  (when texts
    (let ((raw (pcase anvil-semantic-embed-provider
                 ('ollama (anvil-semantic--embed-ollama texts task))
                 ('gemini (anvil-semantic--embed-gemini texts task))
                 ('openai (anvil-semantic--embed-openai texts task))
                 (p (user-error "anvil-semantic: unknown embed provider %S" p)))))
      (mapcar #'anvil-semantic--normalize raw))))

(defun anvil-semantic--embed-batch (texts task)
  "Embed TEXTS in provider batches of `anvil-semantic-embed-batch'.
Return a list of normalized float vectors in input order."
  (let ((bs (max 1 anvil-semantic-embed-batch))
        (total (length texts))
        (out nil) (i 0))
    (while (< i total)
      (let ((slice (cl-subseq texts i (min total (+ i bs)))))
        (setq out (nconc out (anvil-semantic--embed slice task)))
        (setq i (+ i bs))))
    out))

(defun anvil-semantic--query-vec (query)
  "Return the normalized embedding of QUERY, or nil if embedding fails.
Failure is downgraded to a message so search degrades to the FTS path
rather than erroring when the embedding backend is unreachable."
  (condition-case err
      (car (anvil-semantic--embed (list query) 'query))
    (error
     (message "anvil-semantic: query embed failed (%s); using FTS only"
              (error-message-string err))
     nil)))

;;; Public: embedding index build

;;;###autoload
(defun anvil-semantic-embed-index (&optional roots)
  "Build/update the vector embedding index over the FTS corpus.
Refreshes the FTS index first (so new files are chunked), then embeds
every chunk lacking a vector for the active model via
`anvil-semantic-embed-provider', and prunes vectors whose chunk is gone.
ROOTS, when given, scopes the FTS step to a full rebuild of that subtree.
Returns a summary plist."
  (interactive)
  (let* ((db (anvil-semantic--ensure-db))
         (model (anvil-semantic--embed-model))
         (start (float-time)))
    (anvil-semantic--ensure-vec-schema db)
    (if roots (anvil-semantic-rebuild roots) (anvil-semantic-refresh-if-stale))
    (let* ((rows (sqlite-select
                  db "SELECT cf.digest, cf.file, cf.text
                        FROM chunk_fts cf
                       WHERE NOT EXISTS (SELECT 1 FROM chunk_vec cv
                                          WHERE cv.digest = cf.digest
                                            AND cv.model = ?)"
                  (list model)))
           (seen (make-hash-table :test 'equal))
           (pending nil)
           (embedded 0) (dim 0))
      ;; de-duplicate by digest (identical chunks share one vector)
      (dolist (r rows)
        (let ((d (nth 0 r)))
          (unless (gethash d seen) (puthash d t seen) (push r pending))))
      (setq pending (nreverse pending))
      (when pending
        (let* ((texts (mapcar (lambda (r) (nth 2 r)) pending))
               (vecs (anvil-semantic--embed-batch texts 'document)))
          (anvil-semantic--with-transaction db
            (cl-loop for r in pending for v in vecs do
                     (setq dim (length v))
                     (sqlite-execute
                      db "INSERT OR REPLACE INTO chunk_vec
                            (digest, model, file, dim, vec)
                          VALUES (?,?,?,?,?)"
                      (list (nth 0 r) model (nth 1 r) dim
                            (anvil-semantic--vec->text v)))
                     (cl-incf embedded)))))
      (let ((pruned (caar (sqlite-select
                           db "SELECT COUNT(*) FROM chunk_vec
                                WHERE model = ?
                                  AND digest NOT IN (SELECT digest FROM chunk_fts)"
                           (list model)))))
        (anvil-semantic--with-transaction db
          (sqlite-execute
           db "DELETE FROM chunk_vec
                WHERE model = ?
                  AND digest NOT IN (SELECT digest FROM chunk_fts)"
           (list model)))
        (let ((elapsed (- (float-time) start)))
          (message "anvil-semantic: embedded %d chunk(s) [%s/%s dim=%d] -%d in %.2fs"
                   embedded anvil-semantic-embed-provider model dim
                   (or pruned 0) elapsed)
          (list :provider anvil-semantic-embed-provider
                :model model :embedded embedded :dim dim
                :pruned (or pruned 0) :elapsed-sec elapsed))))))

;;; Search core

(defun anvil-semantic--fts-quote (term)
  "Return TERM as a quoted FTS5 string literal."
  (concat "\"" (replace-regexp-in-string "\"" "\"\"" term) "\""))

(defun anvil-semantic--like-escape (term)
  "Escape LIKE wildcards in TERM (for use with ESCAPE '\\')."
  (replace-regexp-in-string "\\([\\%_]\\)" "\\\\\\1" term))

(defun anvil-semantic--row->plist (row)
  "Turn a raw chunk_fts ROW into a normalized plist.
ROW is (FILE HEADING START-LINE DIGEST MTIME TEXT [BM25])."
  (list :file (nth 0 row)
        :heading (let ((h (nth 1 row))) (and (stringp h) (not (string-empty-p h)) h))
        :line (let ((l (nth 2 row))) (if (numberp l) l (string-to-number (format "%s" l))))
        :digest (nth 3 row)
        :text (nth 5 row)
        :bm25 (when (nth 6 row)
                (let ((b (nth 6 row)))
                  (if (numberp b) b (string-to-number (format "%s" b)))))))

(defun anvil-semantic--root-clause (root params)
  "Return an extra SQL clause restricting `file' to the ROOT prefix.
Returns (CLAUSE . NEW-PARAMS).  Uses `instr(file, ?) = 1' rather than
LIKE: on an FTS5 UNINDEXED column LIKE/GLOB do not match, but instr
and `=' do."
  (if (and (stringp root) (not (string-empty-p root)))
      (cons " AND instr(file, ?) = 1"
            (append params (list (expand-file-name root))))
    (cons "" params)))

(defun anvil-semantic--match-candidates (db terms root limit)
  "FTS5 MATCH pass: rows matching any TERM of length >= 3.
Returns a list of plists carrying :bm25.  TERMS are lowercased."
  (let ((long (cl-remove-if (lambda (tm) (< (length tm) 3)) terms)))
    (when long
      (let* ((expr (mapconcat #'anvil-semantic--fts-quote long " OR "))
             (rc (anvil-semantic--root-clause root (list expr)))
             (sql (format "SELECT file, heading, start_line, digest, mtime, text,
                                  bm25(chunk_fts)
                             FROM chunk_fts
                            WHERE chunk_fts MATCH ?%s
                            ORDER BY bm25(chunk_fts)
                            LIMIT %d"
                          (car rc) limit)))
        (mapcar #'anvil-semantic--row->plist
                (sqlite-select db sql (cdr rc)))))))

(defun anvil-semantic--like-candidates (db terms root limit)
  "LIKE-scan pass: rows containing any TERM (handles 2-char CJK).
Returns a list of plists (no :bm25).  TERMS are lowercased."
  (when terms
    (let* ((clauses (mapconcat (lambda (_tm) "lower(text) LIKE ? ESCAPE '\\'")
                               terms " OR "))
           (params (mapcar (lambda (tm)
                             (concat "%" (anvil-semantic--like-escape tm) "%"))
                           terms))
           (rc (anvil-semantic--root-clause root params))
           (sql (format "SELECT file, heading, start_line, digest, mtime, text
                           FROM chunk_fts
                          WHERE (%s)%s
                          LIMIT %d"
                        clauses (car rc) limit)))
      (mapcar #'anvil-semantic--row->plist (sqlite-select db sql (cdr rc))))))

(defun anvil-semantic--lex-overlap (text terms)
  "Return how many distinct TERMS occur as substrings of TEXT."
  (let ((low (downcase text)) (n 0))
    (dolist (tm terms n)
      (when (string-search tm low) (setq n (1+ n))))))

(defun anvil-semantic--fetch-meta (db digests)
  "Return a hash DIGEST -> (FILE HEADING LINE TEXT) for DIGESTS from chunk_fts."
  (let ((h (make-hash-table :test 'equal)))
    (when digests
      (let* ((ph (mapconcat (lambda (_) "?") digests ","))
             (rows (sqlite-select
                    db (format "SELECT digest, file, heading, start_line, text
                                  FROM chunk_fts WHERE digest IN (%s)" ph)
                    digests)))
        (dolist (r rows)
          (puthash (nth 0 r)
                   (list (nth 1 r)
                         (let ((hd (nth 2 r)))
                           (and (stringp hd) (not (string-empty-p hd)) hd))
                         (let ((l (nth 3 r)))
                           (if (numberp l) l (string-to-number (format "%s" l))))
                         (nth 4 r))
                   h))))
    h))

(defun anvil-semantic--vec-candidates (db qvec model root limit)
  "Cosine pass: rank chunk_vec rows for MODEL against QVEC.
Return up to LIMIT plists carrying :cos and chunk metadata.  ROOT, when
non-nil, restricts to files under that prefix (via the shared
`instr(file, ?)' clause; the `file' column is denormalized into chunk_vec)."
  (when qvec
    (let* ((rc (anvil-semantic--root-clause root (list model)))
           (rows (sqlite-select
                  db (concat "SELECT digest, vec FROM chunk_vec WHERE model = ?"
                             (car rc))
                  (cdr rc)))
           (scored nil))
      (dolist (r rows)
        (let ((v (ignore-errors (anvil-semantic--text->vec (nth 1 r)))))
          (when (vectorp v)
            (push (cons (anvil-semantic--dot qvec v) (nth 0 r)) scored))))
      (setq scored (sort scored (lambda (a b) (> (car a) (car b)))))
      (let* ((top (cl-subseq scored 0 (min limit (length scored))))
             (meta (anvil-semantic--fetch-meta db (mapcar #'cdr top))))
        (delq nil
              (mapcar
               (lambda (pair)
                 (let ((m (gethash (cdr pair) meta)))
                   (when m
                     (list :file (nth 0 m) :heading (nth 1 m)
                           :line (nth 2 m) :digest (cdr pair)
                           :text (nth 3 m) :cos (car pair)))))
               top))))))

(defun anvil-semantic--merge-candidates (lists)
  "Merge candidate plist LISTS, de-duplicated by :digest.
Fields from later passes fill gaps, so one row can carry both :bm25 and
:cos when it surfaced in the FTS and the vector passes."
  (let ((h (make-hash-table :test 'equal)) (order nil))
    (dolist (lst lists)
      (dolist (r lst)
        (let* ((d (plist-get r :digest))
               (ex (gethash d h)))
          (if (not ex)
              (progn (puthash d (copy-sequence r) h) (push d order))
            (dolist (key '(:bm25 :cos :text :heading :line :file))
              (when (and (plist-get r key) (not (plist-get ex key)))
                (puthash d (plist-put ex key (plist-get r key)) h)))))))
    (nreverse (mapcar (lambda (d) (gethash d h)) order))))

(defun anvil-semantic--rrf (orders &optional k)
  "Reciprocal-rank-fuse list-of-lists ORDERS into one ordered list.
Each element is compared with `eq'.  K defaults to 60."
  (let ((k (or k 60)) (score (make-hash-table :test 'eq)) (items nil))
    (dolist (order orders)
      (let ((rank 0))
        (dolist (it order)
          (unless (gethash it score) (push it items))
          (puthash it (+ (gethash it score 0.0)
                         (/ 1.0 (+ k rank 1)))
                   score)
          (setq rank (1+ rank)))))
    (sort (nreverse items)
          (lambda (a b) (> (gethash a score) (gethash b score))))))

(defun anvil-semantic--rank (candidates terms mode k)
  "Rank CANDIDATES (plists) by MODE, return the top K, annotated with :lex.
MODE is one of \"hybrid\" / \"semantic\" / \"lexical\" / \"vector\".  Signals:
BM25 (:bm25, FTS5), embedding cosine (:cos), lexical overlap (:lex).
\"semantic\" prefers cosine when embeddings are present, else falls back
to BM25 (the pure-FTS5 behavior); \"hybrid\" RRF-fuses all three."
  (dolist (c candidates)
    (plist-put c :lex (anvil-semantic--lex-overlap (plist-get c :text) terms)))
  (let* ((sem (sort (cl-remove-if-not (lambda (c) (plist-get c :bm25))
                                      (copy-sequence candidates))
                    ;; bm25: more negative = more relevant.
                    (lambda (a b) (< (plist-get a :bm25) (plist-get b :bm25)))))
         (vec (sort (cl-remove-if-not (lambda (c) (plist-get c :cos))
                                      (copy-sequence candidates))
                    ;; cosine: higher = more relevant.
                    (lambda (a b) (> (plist-get a :cos) (plist-get b :cos)))))
         (lex (sort (copy-sequence candidates)
                    (lambda (a b)
                      (let ((la (plist-get a :lex)) (lb (plist-get b :lex)))
                        (if (= la lb)
                            (< (length (plist-get a :text))
                               (length (plist-get b :text)))
                          (> la lb))))))
         (ranked (pcase mode
                   ("vector" (or vec lex))
                   ("semantic" (or vec sem lex))
                   ("lexical" lex)
                   (_ (anvil-semantic--rrf (list sem vec lex))))))
    (cl-subseq ranked 0 (min k (length ranked)))))

(defun anvil-semantic--search (query k root mode terms)
  "Core search.  Return a list of ranked result plists.
TERMS overrides the salient terms extracted from QUERY (for synonym
expansion); pass nil to derive them from QUERY.  When the embedding index
is populated and MODE wants meaning (hybrid/semantic/vector), the query is
embedded and a cosine pass joins the FTS candidate set."
  (let* ((db (anvil-semantic--ensure-db))
         (terms (or terms (anvil-semantic--terms query)))
         (model (anvil-semantic--embed-model))
         (limit anvil-semantic-candidate-limit)
         (want-vec (and (member mode '("hybrid" "semantic" "vector"))
                        (stringp query)
                        (not (string-empty-p (string-trim query)))
                        (anvil-semantic--embed-available-p db model)))
         (qvec (and want-vec (anvil-semantic--query-vec query))))
    (when (or terms qvec)
      (let* ((match (and terms (anvil-semantic--match-candidates db terms root limit)))
             (like (and terms (anvil-semantic--like-candidates db terms root limit)))
             (vec (and qvec (anvil-semantic--vec-candidates db qvec model root limit)))
             (cands (anvil-semantic--merge-candidates (list match like vec))))
        (anvil-semantic--rank cands terms mode k)))))

;;; Result formatting

(defun anvil-semantic--collapse (s n)
  "Collapse whitespace in S and truncate to N characters."
  (let ((c (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " s))))
    (if (> (length c) n) (substring c 0 n) c)))

(defun anvil-semantic--json (rows)
  "Encode ROWS (a list of alists) as a pretty UTF-8 JSON array string."
  (let ((json-encoding-pretty-print t))
    (json-encode (apply #'vector rows))))

(defun anvil-semantic--search-result (hit)
  "Format a ranked HIT plist as a `semantic-search' result alist."
  `((file . ,(abbreviate-file-name (plist-get hit :file)))
    (heading . ,(or (plist-get hit :heading) :null))
    (line . ,(plist-get hit :line))
    (score . ,(let ((b (plist-get hit :bm25)))
                (if b (/ (fround (* (- b) 10000.0)) 10000.0) 0)))
    (cos . ,(let ((c (plist-get hit :cos)))
              (if c (/ (fround (* c 10000.0)) 10000.0) :null)))
    (lex . ,(plist-get hit :lex))
    (snippet . ,(anvil-semantic--collapse (plist-get hit :text)
                                          anvil-semantic-snippet-chars))))

(defun anvil-semantic--lexical-result (hit)
  "Format a ranked HIT plist as a `notes-lexical-search' result alist."
  `((file . ,(abbreviate-file-name (plist-get hit :file)))
    (heading . ,(or (plist-get hit :heading) :null))
    (line . ,(plist-get hit :line))
    (lex . ,(plist-get hit :lex))
    (excerpt . ,(anvil-semantic--collapse (plist-get hit :text)
                                          anvil-semantic-excerpt-chars))))

;;; Tool argument helpers

(defun anvil-semantic--arg-k (k default)
  "Coerce K (string / integer / nil) to a positive integer, else DEFAULT."
  (cond ((integerp k) (if (> k 0) k default))
        ((and (stringp k) (string-match-p "\\`[0-9]+\\'" k))
         (let ((n (string-to-number k))) (if (> n 0) n default)))
        (t default)))

(defun anvil-semantic--arg-mode (mode)
  "Normalize MODE to hybrid / semantic / lexical / vector (default hybrid)."
  (let ((m (and (stringp mode) (downcase (string-trim mode)))))
    (if (member m '("hybrid" "semantic" "lexical" "vector")) m "hybrid")))

(defun anvil-semantic--arg-str (s)
  "Return S when a non-empty string, else nil."
  (and (stringp s) (not (string-empty-p (string-trim s))) (string-trim s)))

;;; MCP tool handlers

(defun anvil-semantic--tool-search (query &optional k root mode)
  "Meaning-via-retrieval search over the indexed corpus.

Combines FTS5 BM25 ranking with lexical term-overlap (reciprocal
rank fusion).  There are no vector embeddings: \"semantic\" means the
caller reads the returned excerpts and does the conceptual matching.
Returns a JSON array of hits, each with file, heading, line, score
(BM25-derived; higher = better), lex (matched terms) and snippet.

MCP Parameters:
  query - Natural-language or keyword query (Japanese OK)
  k     - Optional max number of hits (digit string, default 8)
  root  - Optional path prefix to restrict results (e.g. a directory)
  mode  - Optional: hybrid (default) / semantic / lexical / vector
          (semantic & vector use embeddings when the index is built)"
  (anvil-server-with-error-handling
    (let* ((q (or (anvil-semantic--arg-str query) ""))
           (hits (and (not (string-empty-p q))
                      (anvil-semantic--search
                       q
                       (anvil-semantic--arg-k k anvil-semantic-default-k)
                       (anvil-semantic--arg-str root)
                       (anvil-semantic--arg-mode mode)
                       nil))))
      (anvil-semantic--json (mapcar #'anvil-semantic--search-result hits)))))

(defun anvil-semantic--tool-lexical-search (query &optional terms k root)
  "Local term-overlap search over the indexed corpus (no cloud).

Ranks chunks by how many query terms (plus caller-supplied synonyms)
occur, and returns readable excerpts for the retrieve-then-read flow:
expand the query into domain synonyms, pass them via `terms', read the
excerpts, keep the relevant ones.  Returns JSON: file, heading, line,
lex (matched terms), excerpt.

MCP Parameters:
  query - Natural-language or keyword query (Japanese OK)
  terms - Optional comma-separated synonym terms to broaden recall
  k     - Optional max candidates (digit string, default 15)
  root  - Optional path prefix to restrict results"
  (anvil-server-with-error-handling
    (let* ((q (or (anvil-semantic--arg-str query) ""))
           (merged (anvil-semantic--merge-terms q (anvil-semantic--arg-str terms)))
           (hits (and merged
                      (anvil-semantic--search
                       q
                       (anvil-semantic--arg-k k anvil-semantic-lexical-default-k)
                       (anvil-semantic--arg-str root)
                       "lexical"
                       merged))))
      (anvil-semantic--json (mapcar #'anvil-semantic--lexical-result hits)))))

(defun anvil-semantic--tool-reindex (&optional root)
  "Rebuild the search index from disk (incremental when ROOT omitted).

With no argument, runs an mtime-based incremental refresh over
`anvil-semantic-roots' (cheap).  With ROOT (a directory path) rebuilds
that subtree from scratch.  Returns a printed summary plist.

MCP Parameters:
  root - Optional directory to fully rebuild; omit for a cheap
         incremental refresh over all configured roots"
  (anvil-server-with-error-handling
    (format "%S"
            (if (anvil-semantic--arg-str root)
                (anvil-semantic-rebuild (list (anvil-semantic--arg-str root)))
              (anvil-semantic-refresh-if-stale)))))

(defun anvil-semantic--tool-status ()
  "Report index statistics: db path, tokenizer, FTS counts, and the
embedding (vector) layer — active provider/model plus per-model chunk_vec
counts and dimension."
  (anvil-server-with-error-handling
    (let ((db (anvil-semantic--ensure-db)))
      (format "%S"
              (list :db anvil-semantic-db-path
                    :tokenizer (anvil-semantic--current-tokenizer db)
                    :roots anvil-semantic-roots
                    :files (caar (sqlite-select
                                  db "SELECT COUNT(DISTINCT file) FROM chunk_fts"))
                    :chunks (caar (sqlite-select
                                   db "SELECT COUNT(*) FROM chunk_fts"))
                    :embed-provider anvil-semantic-embed-provider
                    :embed-model (anvil-semantic--embed-model)
                    :vectors (mapcar
                              (lambda (row)
                                (list :model (nth 0 row)
                                      :count (nth 1 row)
                                      :dim (nth 2 row)))
                              (and (anvil-semantic--vec-table-exists-p db)
                                   (sqlite-select
                                    db "SELECT model, COUNT(*), MIN(dim)
                                          FROM chunk_vec GROUP BY model"))))))))

(defun anvil-semantic--tool-embed-index (&optional root)
  "Build or update the embedding (vector) search index.

With no argument, refreshes the FTS index incrementally then embeds any
chunk that lacks a vector for the active model (cheap after the first
run).  With ROOT (a directory path), fully rebuilds that subtree's FTS
chunks first.  Embeddings come from `anvil-semantic-embed-provider'
\(ollama local by default; gemini / openai are opt-in cloud).  Returns a
printed summary plist.

MCP Parameters:
  root - Optional directory to fully rebuild before embedding; omit for a
         cheap incremental embed over all configured roots"
  (anvil-server-with-error-handling
    (format "%S"
            (anvil-semantic-embed-index
             (and (anvil-semantic--arg-str root)
                  (list (anvil-semantic--arg-str root)))))))

;;; Module enable / disable

(defun anvil-semantic--register-tools ()
  "Register the anvil-semantic MCP tools.  Idempotent."
  (anvil-server-register-tool
   #'anvil-semantic--tool-search
   :id "semantic-search"
   :intent '(search retrieval)
   :layer 'core
   :read-only t
   :server-id anvil-semantic--server-id
   :description
   "Meaning-via-retrieval search over the user's local corpus (org
notes, text, code under `anvil-semantic-roots').  Fuses FTS5 BM25
ranking with lexical term-overlap (RRF), plus optional embedding cosine.
Returns a JSON array of hits: file, heading, line, score (BM25-derived,
higher = better), lex (matched terms), snippet.  Use for vague /
conceptual / synonym queries where exact grep misses (e.g. 漏電をどう
見つけるか surfacing 漏洩電流 / 漏電検出).  Modes: hybrid (default) /
semantic / lexical / vector.  hybrid fuses BM25 + lexical + embedding
cosine (RRF); semantic = cosine when the embedding index is built, else
BM25; vector = pure cosine.  Backed by built-in SQLite FTS5 (trigram)
plus an optional local `chunk_vec' embedding layer (see
`semantic-embed-index'; ollama by default = fully local).")
  (anvil-server-register-tool
   #'anvil-semantic--tool-lexical-search
   :id "notes-lexical-search"
   :intent '(search retrieval)
   :layer 'core
   :read-only t
   :server-id anvil-semantic--server-id
   :description
   "Local term-overlap search over the user's corpus (no embeddings, no
cloud; content stays local, only the caller reads it).  Ranks chunks by
query-term overlap and returns readable excerpts for retrieve-then-read:
expand the query into domain synonyms, pass them via `terms', read the
excerpts, keep only the relevant ones.  Returns JSON: file, heading,
line, lex (matched terms), excerpt.  The default note-search tool.")
  (anvil-server-register-tool
   #'anvil-semantic--tool-reindex
   :id "semantic-reindex"
   :intent '(search admin)
   :layer 'workflow
   :server-id anvil-semantic--server-id
   :description
   "Refresh the local search index.  No argument → cheap mtime-based
incremental refresh over `anvil-semantic-roots'.  With a `root'
directory → full rebuild of that subtree.  Run once after enabling the
module to populate the index.")
  (anvil-server-register-tool
   #'anvil-semantic--tool-embed-index
   :id "semantic-embed-index"
   :intent '(search admin)
   :layer 'workflow
   :server-id anvil-semantic--server-id
   :description
   "Build / update the optional embedding (vector) index that powers
meaning-based search.  No argument → incremental embed of new chunks;
with a `root' directory → rebuild that subtree first.  Embeddings come
from `anvil-semantic-embed-provider' (ollama local by default; gemini /
openai are opt-in cloud).  Until this is run, search stays on the
pure-FTS5 path.  Run after `semantic-reindex'.")
  (anvil-server-register-tool
   #'anvil-semantic--tool-status
   :id "semantic-status"
   :intent '(search diag)
   :layer 'core
   :read-only t
   :server-id anvil-semantic--server-id
   :description
   "Report search-index stats: db path, FTS5 tokenizer, configured
roots, and indexed file / chunk counts."))

(defun anvil-semantic--unregister-tools ()
  "Unregister the anvil-semantic MCP tools.  Safe when not registered."
  (dolist (id '("semantic-search" "notes-lexical-search"
                "semantic-reindex" "semantic-embed-index" "semantic-status"))
    (ignore-errors
      (anvil-server-unregister-tool id anvil-semantic--server-id))))

;;;###autoload
(defun anvil-semantic-enable ()
  "Enable anvil-semantic: open the index DB and register MCP tools.
Idempotent.  Does not build the index — run `semantic-reindex' (or
`anvil-semantic-rebuild') once after configuring `anvil-semantic-roots'."
  (interactive)
  (when (anvil-semantic--available-p)
    (ignore-errors (anvil-semantic--ensure-db)))
  (anvil-semantic--register-tools)
  (message "anvil-semantic: enabled (db=%s)" anvil-semantic-db-path))

;;;###autoload
(defun anvil-semantic-disable ()
  "Disable anvil-semantic: unregister tools and close the DB."
  (interactive)
  (anvil-semantic--unregister-tools)
  (when (and anvil-semantic--db (sqlitep anvil-semantic--db))
    (ignore-errors (sqlite-close anvil-semantic--db)))
  (setq anvil-semantic--db nil)
  (message "anvil-semantic: disabled"))

(provide 'anvil-semantic)
;;; anvil-semantic.el ends here
