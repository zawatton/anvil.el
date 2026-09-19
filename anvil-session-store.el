;;; anvil-session-store.el --- FTS5 session event store for anvil  -*- lexical-binding: t; -*-

;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 63 Phase 1 — the FTS5-backed session event log that Doc 17
;; Phase 3 designed but shipped as a linear anvil-state LIKE scan.
;;
;; `anvil-session-events-search' says so in its own docstring:
;;
;;     Doc 17 design used FTS5 but the anvil-state simple-LIKE path
;;     is adequate for the <=500-event/session volume anticipated;
;;     swap in a dedicated FTS5 DB later if hot.
;;
;; This module is that swap.  It is additive: anvil-session keeps its
;; anvil-state rows, and this store runs alongside as the indexed,
;; rankable view.  Callers that want ranking ask here; callers that
;; want the raw chronological plist pool keep using anvil-session.
;;
;; Three things separate this from the anvil-state path:
;;
;;   1. Structured rows.  A row carries `category' and `priority'
;;      alongside kind/tool/summary, so a snapshot builder can spend
;;      a byte budget on P1 rows and drop P3 ones.  The anvil-state
;;      row has no such axis, which is why the current PreCompact
;;      snapshot can only cap by count.
;;
;;   2. Ranked retrieval.  FTS5 MATCH + bm25(), with the identifier
;;      column (`data' — a path, a command, a ref) weighted 5x, the
;;      way context-mode weights markdown headings 5x.  A LIKE pass
;;      runs alongside for 1-2 char CJK terms that the tokenizer
;;      cannot produce, and the two are merged by Reciprocal Rank
;;      Fusion, then reranked on term proximity.
;;
;;   3. Reference snapshots.  `anvil-session-store-snapshot-ref'
;;      builds a table of contents, not a digest: each section names
;;      what happened and embeds a runnable `session-events-search'
;;      call that pulls the full rows back on demand.  Nothing is
;;      truncated mid-record; whole low-priority sections are dropped
;;      instead, so a row is either intact or absent.
;;
;; Dual-target (Emacs / NeLisp standalone) comes for free: every call
;; below goes through the plain `sqlite-*' surface, which NeLisp
;; provides via `nelisp-emacs/src/emacs-sqlite.el' forwarders to
;; `nelisp-sqlite-*'.  No `fboundp' guard is needed, matching how
;; `anvil-semantic.el' already runs on both runtimes.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)

(defgroup anvil-session-store nil
  "FTS5-backed session event store."
  :group 'anvil
  :prefix "anvil-session-store-")

(defconst anvil-session-store--server-id "emacs-eval"
  "Server ID this module registers its MCP tools under.
Must match the `--server-id' passed to the stdio shim.")

(defcustom anvil-session-store-db-path
  (expand-file-name "anvil-session-events.db" user-emacs-directory)
  "Path to the SQLite database holding the session event log."
  :type 'file
  :group 'anvil-session-store)

(defcustom anvil-session-store-tokenizer 'auto
  "FTS5 tokenizer for the event index.

`auto'      Use `trigram' when the SQLite build has it, else `unicode61'.
`trigram'   Force trigram — substring matching, works for CJK.
`unicode61' Force the SQLite default (ASCII word tokens).

Left at `auto' on a modern SQLite this resolves to `trigram', which
is what makes Japanese summaries searchable at all."
  :type '(choice (const auto) (const trigram) (const unicode61))
  :group 'anvil-session-store)

(defcustom anvil-session-store-summary-max-chars 200
  "Hard cap on a stored event summary, in characters."
  :type 'integer
  :group 'anvil-session-store)

(defcustom anvil-session-store-ttl-sec (* 14 24 60 60)
  "Age in seconds after which `anvil-session-store-prune' drops a row."
  :type 'integer
  :group 'anvil-session-store)

(defcustom anvil-session-store-snapshot-max-bytes 2048
  "Byte budget for `anvil-session-store-snapshot-ref' output.

The reference snapshot is a table of contents, so this budget is
spent on section headers, per-item lines and the embedded search
calls — never on event bodies, which stay in the database.  When
the budget runs out, whole sections are dropped lowest-priority
first; individual records are never cut in half."
  :type 'integer
  :group 'anvil-session-store)

(defvar anvil-session-store--db nil
  "Open database handle, or nil when the store has not been opened.")

;;;; --- categories ---------------------------------------------------------

(defconst anvil-session-store-categories
  '((file      . 1)
    (git       . 1)
    (error     . 1)
    (decision  . 1)
    (rule      . 1)
    (task      . 2)
    (plan      . 2)
    (blocker   . 1)
    (constraint . 2)
    (skill     . 2)
    (mcp       . 3)
    (tool-use  . 3)
    (prompt    . 2)
    (other     . 3))
  "Alist of (CATEGORY . DEFAULT-PRIORITY).

Priority 1 survives every budget, 3 is dropped first.  The set is
deliberately smaller than context-mode's 25 categories: anvil only
captures what its hooks actually observe, and an empty category is
worse than no category — it invites a snapshot section that always
renders blank.  Grow this list when a producer starts emitting the
kind, not before.")

(defun anvil-session-store-default-priority (category)
  "Return the default priority for CATEGORY, or 3 when unknown."
  (or (cdr (assq (anvil-session-store--category-symbol category)
                 anvil-session-store-categories))
      3))

(defun anvil-session-store--category-symbol (category)
  "Coerce CATEGORY to a symbol."
  (cond ((symbolp category) category)
        ((stringp category) (intern category))
        (t 'other)))

;;;; --- schema -------------------------------------------------------------

(defun anvil-session-store--supports-trigram-p (db)
  "Return non-nil when DB's SQLite build ships the FTS5 trigram tokenizer."
  (condition-case nil
      (progn
        (sqlite-execute
         db "CREATE VIRTUAL TABLE anvil_session_trigram_probe
               USING fts5(x, tokenize=trigram)")
        (sqlite-execute db "DROP TABLE anvil_session_trigram_probe")
        t)
    (error nil)))

(defun anvil-session-store--resolve-tokenizer (db)
  "Return the tokenizer symbol DB should use."
  (pcase anvil-session-store-tokenizer
    ('trigram 'trigram)
    ('unicode61 'unicode61)
    ('auto (if (anvil-session-store--supports-trigram-p db) 'trigram 'unicode61))
    (other (user-error "anvil-session-store: invalid tokenizer %S" other))))

(defun anvil-session-store--fts-exists-p (db)
  "Return non-nil when the `session_events' table exists in DB."
  (caar (sqlite-select
         db "SELECT 1 FROM sqlite_master
              WHERE type='table' AND name='session_events'")))

(defun anvil-session-store--apply-schema (db)
  "Create the event table and metadata table on DB when absent."
  (sqlite-execute db "PRAGMA journal_mode = WAL")
  (sqlite-execute
   db "CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT)")
  (unless (anvil-session-store--fts-exists-p db)
    (let ((tok (anvil-session-store--resolve-tokenizer db)))
      ;; Column order is load-bearing: `anvil-session-store--bm25-weights'
      ;; supplies one weight per column, positionally.
      (sqlite-execute
       db (format "CREATE VIRTUAL TABLE session_events USING fts5(
                     session_id UNINDEXED,
                     seq        UNINDEXED,
                     ts         UNINDEXED,
                     kind       UNINDEXED,
                     category   UNINDEXED,
                     priority   UNINDEXED,
                     tool,
                     data,
                     summary,
                     tokenize = %s)"
                  (symbol-name tok)))
      (sqlite-execute
       db "INSERT OR REPLACE INTO meta(key, value) VALUES ('tokenizer', ?)"
       (list (symbol-name tok))))))

(defconst anvil-session-store--bm25-weights
  "0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 5.0, 1.0"
  "Positional bm25() weights for the `session_events' columns.

The six UNINDEXED columns contribute nothing and take 0.0.  `data'
gets 5.0 — it holds the identifier a human would search by (a file
path, a command, a commit ref), so it plays the role context-mode
gives markdown headings.  `tool' gets 2.0, prose `summary' 1.0.")

;;;###autoload
(defun anvil-session-store-available-p ()
  "Return non-nil when this runtime can back the store with SQLite."
  (and (fboundp 'sqlite-open)
       (or (not (fboundp 'sqlite-available-p))
           (sqlite-available-p))))

(defun anvil-session-store--db ()
  "Return the open database handle, opening and migrating it if needed."
  (unless (anvil-session-store-available-p)
    (user-error "anvil-session-store: this Emacs/NeLisp build has no SQLite"))
  (unless anvil-session-store--db
    (let ((dir (file-name-directory
                (expand-file-name anvil-session-store-db-path))))
      (when (and dir (not (file-directory-p dir)))
        (make-directory dir t)))
    (setq anvil-session-store--db
          (sqlite-open (expand-file-name anvil-session-store-db-path)))
    (anvil-session-store--apply-schema anvil-session-store--db))
  anvil-session-store--db)

;;;###autoload
(defun anvil-session-store-close ()
  "Close the database handle if one is open."
  (when anvil-session-store--db
    (ignore-errors (sqlite-close anvil-session-store--db))
    (setq anvil-session-store--db nil)))

;;;; --- writing ------------------------------------------------------------

(defun anvil-session-store--clamp (s max-chars)
  "Return S clamped to MAX-CHARS characters, or nil when S is nil."
  (cond
   ((null s) nil)
   ((not (stringp s)) (anvil-session-store--clamp (format "%s" s) max-chars))
   ((> (length s) max-chars) (substring s 0 max-chars))
   (t s)))

(defun anvil-session-store--next-seq (db session-id)
  "Return the next per-session sequence number for SESSION-ID in DB."
  (let ((row (caar (sqlite-select
                    db "SELECT MAX(CAST(seq AS INTEGER)) FROM session_events
                         WHERE session_id = ?"
                    (list session-id)))))
    (if (numberp row) (1+ row) 1)))

;;;###autoload
(cl-defun anvil-session-store-put (session-id kind
                                              &key category priority
                                              tool data summary ts)
  "Append one structured event row and return it as a plist.

SESSION-ID scopes the row to one Claude conversation.  KIND is the
raw event name (`file_edit', `user-prompt', ...).

Keyword arguments:
  :category  Grouping symbol from `anvil-session-store-categories'.
             Defaults to `other'.
  :priority  1 (keep under any budget) .. 3 (drop first).  Defaults
             to the category's entry.
  :tool      Tool name that produced the event, when there was one.
  :data      The identifying payload — a path, a command, a ref.
             This is the bm25-5x column, so put the thing a human
             would search for here rather than in :summary.
  :summary   Prose digest, clamped to
             `anvil-session-store-summary-max-chars'.
  :ts        Override timestamp (defaults to `float-time')."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (user-error "anvil-session-store-put: SESSION-ID required"))
  (let* ((db (anvil-session-store--db))
         (cat (anvil-session-store--category-symbol (or category 'other)))
         (prio (or priority (anvil-session-store-default-priority cat)))
         (ts* (or ts (float-time)))
         (seq (anvil-session-store--next-seq db session-id))
         (kind-str (if (stringp kind) kind (format "%s" kind)))
         (summary* (anvil-session-store--clamp
                    summary anvil-session-store-summary-max-chars))
         (data* (anvil-session-store--clamp data 500)))
    (sqlite-execute
     db "INSERT INTO session_events
           (session_id, seq, ts, kind, category, priority, tool, data, summary)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
     (list session-id seq ts* kind-str (symbol-name cat) prio
           (or tool "") (or data* "") (or summary* "")))
    (list :session-id session-id :seq seq :ts ts* :kind kind-str
          :category cat :priority prio :tool tool
          :data data* :summary summary*)))

;;;###autoload
(defun anvil-session-store-prune (&optional ttl-sec)
  "Delete rows older than TTL-SEC (default `anvil-session-store-ttl-sec').
Returns the number of rows removed."
  (let* ((db (anvil-session-store--db))
         (cutoff (- (float-time) (or ttl-sec anvil-session-store-ttl-sec)))
         (before (caar (sqlite-select
                        db "SELECT COUNT(*) FROM session_events"))))
    (sqlite-execute db "DELETE FROM session_events WHERE CAST(ts AS REAL) < ?"
                    (list cutoff))
    (let ((after (caar (sqlite-select
                        db "SELECT COUNT(*) FROM session_events"))))
      (max 0 (- (or before 0) (or after 0))))))

;;;; --- reading ------------------------------------------------------------

(defun anvil-session-store--row->plist (row)
  "Convert a raw SQL ROW into an event plist."
  (list :session-id (nth 0 row)
        :seq        (anvil-session-store--as-number (nth 1 row))
        :ts         (anvil-session-store--as-number (nth 2 row))
        :kind       (nth 3 row)
        :category   (anvil-session-store--category-symbol (nth 4 row))
        :priority   (or (anvil-session-store--as-number (nth 5 row)) 3)
        :tool       (nth 6 row)
        :data       (nth 7 row)
        :summary    (nth 8 row)))

(defun anvil-session-store--as-number (v)
  "Coerce V to a number when possible, else nil."
  (cond ((numberp v) v)
        ((and (stringp v) (not (string-empty-p v))) (string-to-number v))
        (t nil)))

(defconst anvil-session-store--select-columns
  "session_id, seq, ts, kind, category, priority, tool, data, summary"
  "Column list matching `anvil-session-store--row->plist' positions.")

;;;###autoload
(cl-defun anvil-session-store-recent (&key session-id limit)
  "Return the last LIMIT events (default 20) as plists, newest last."
  (let* ((db (anvil-session-store--db))
         (limit (or limit 20))
         (rows (if session-id
                   (sqlite-select
                    db (format "SELECT %s FROM session_events
                                 WHERE session_id = ?
                                 ORDER BY CAST(ts AS REAL) DESC LIMIT %d"
                               anvil-session-store--select-columns limit)
                    (list session-id))
                 (sqlite-select
                  db (format "SELECT %s FROM session_events
                               ORDER BY CAST(ts AS REAL) DESC LIMIT %d"
                             anvil-session-store--select-columns limit)))))
    (nreverse (mapcar #'anvil-session-store--row->plist rows))))

(defun anvil-session-store--fts-quote (term)
  "Return TERM as a quoted FTS5 string literal."
  (concat "\"" (replace-regexp-in-string "\"" "\"\"" term) "\""))

(defun anvil-session-store--like-escape (s)
  "Escape LIKE wildcards in S for use with ESCAPE '\\'."
  (replace-regexp-in-string "\\([\\%_]\\)" "\\\\\\1" s))

(defun anvil-session-store--terms (query)
  "Split QUERY into lowercased search terms."
  (cl-remove-if #'string-empty-p
                (split-string (downcase (or query "")) "[ \t\n]+" t)))

;;;; --- ranking ------------------------------------------------------------

(defconst anvil-session-store--stopwords
  '("the" "a" "an" "and" "or" "of" "to" "in" "on" "for" "is" "it"
    "with" "that" "this" "was" "be" "at" "by" "from")
  "English stopwords excluded from proximity scoring.

No Japanese list is applied: the language's high-frequency tokens
are particles that the trigram tokenizer folds into surrounding
text anyway, so removing them here would only drop real matches.")

(defun anvil-session-store--positions (text term)
  "Return every start offset of TERM in TEXT, ascending."
  (let ((out nil) (start 0) (n (length term)))
    (when (> n 0)
      (while (and (<= start (length text))
                  (string-match (regexp-quote term) text start))
        (push (match-beginning 0) out)
        (setq start (1+ (match-beginning 0)))))
    (nreverse out)))

(defun anvil-session-store--min-span (position-lists)
  "Return the width of the tightest window covering one entry per list.

POSITION-LISTS is a list of ascending offset lists, one per term.
Returns nil when any list is empty.  This is the anvil port of
context-mode's `findMinSpan': the score depends on the single
tightest co-occurrence, so repetition does not move it."
  (if (or (null position-lists) (cl-some #'null position-lists))
      nil
    (let ((cursors (make-vector (length position-lists) 0))
          (best most-positive-fixnum)
          (done nil))
      (while (not done)
        (let ((lo most-positive-fixnum) (hi -1) (lo-idx 0))
          (dotimes (i (length position-lists))
            (let ((p (nth (aref cursors i) (nth i position-lists))))
              (when (< p lo) (setq lo p lo-idx i))
              (when (> p hi) (setq hi p))))
          (setq best (min best (- hi lo)))
          ;; Advance the list holding the leftmost position; when it is
          ;; exhausted no tighter window remains.
          (let ((next (1+ (aref cursors lo-idx))))
            (if (>= next (length (nth lo-idx position-lists)))
                (setq done t)
              (aset cursors lo-idx next)))))
      best)))

(defun anvil-session-store--adjacent-pairs (position-lists terms)
  "Count how often consecutive TERMS occur adjacently, per POSITION-LISTS."
  (let ((pairs 0))
    (dotimes (i (1- (length terms)))
      (let ((left (nth i position-lists))
            (right (nth (1+ i) position-lists))
            (len (length (nth i terms))))
        (dolist (l left)
          (when (cl-some (lambda (r) (<= (abs (- r (+ l len 1))) 1)) right)
            (setq pairs (1+ pairs))))))
    pairs))

(defun anvil-session-store--proximity-boost (row terms)
  "Return the proximity/identifier boost for ROW against TERMS.

Mirrors context-mode's reranker with anvil's column semantics: the
`data' column stands in for the title, and `summary' for the body."
  (let* ((useful (or (cl-remove-if
                      (lambda (tm)
                        (member tm anvil-session-store--stopwords))
                      terms)
                     terms))
         (data (downcase (or (plist-get row :data) "")))
         (summary (downcase (or (plist-get row :summary) "")))
         (hits (cl-count-if (lambda (tm) (string-match-p (regexp-quote tm) data))
                            useful))
         (data-boost (if (> hits 0)
                         (* 0.6 (/ (float hits) (max 1 (length useful))))
                       0.0))
         (prox 0.0)
         (phrase 0.0))
    (when (>= (length useful) 2)
      (let ((positions (mapcar (lambda (tm)
                                 (anvil-session-store--positions summary tm))
                               useful)))
        (unless (cl-some #'null positions)
          (let ((span (anvil-session-store--min-span positions)))
            (when span
              (setq prox (/ 1.0 (+ 1.0 (/ (float span)
                                          (max 1 (length summary))))))))
          (setq phrase
                (* 0.5 (min 1.0 (/ (float (anvil-session-store--adjacent-pairs
                                           positions useful))
                                   4.0)))))))
    (+ data-boost prox phrase)))

(defun anvil-session-store--rrf (lists k)
  "Fuse ranked LISTS by Reciprocal Rank Fusion with constant K.

Each list is a list of event plists in rank order.  Returns one
list ordered by descending fused score.  Identity is (session-id,
seq), which is unique per row."
  (let ((scores (make-hash-table :test #'equal))
        (rows (make-hash-table :test #'equal))
        (out nil))
    (dolist (lst lists)
      (let ((i 0))
        (dolist (row lst)
          (let ((key (cons (plist-get row :session-id) (plist-get row :seq))))
            (puthash key row rows)
            (puthash key (+ (gethash key scores 0.0)
                            (/ 1.0 (+ k i 1)))
                     scores))
          (setq i (1+ i)))))
    (maphash (lambda (key score) (push (cons score (gethash key rows)) out))
             scores)
    (mapcar #'cdr (sort out (lambda (a b) (> (car a) (car b)))))))

(defun anvil-session-store--match-candidates (db terms session-id limit)
  "FTS5 MATCH pass over TERMS of length >= 3, ranked by bm25."
  (let ((long (cl-remove-if (lambda (tm) (< (length tm) 3)) terms)))
    (when long
      (let* ((expr (mapconcat #'anvil-session-store--fts-quote long " OR "))
             (params (list expr))
             (scope ""))
        (when session-id
          (setq scope " AND session_id = ?"
                params (append params (list session-id))))
        (mapcar #'anvil-session-store--row->plist
                (sqlite-select
                 db (format "SELECT %s FROM session_events
                              WHERE session_events MATCH ?%s
                              ORDER BY bm25(session_events, %s)
                              LIMIT %d"
                            anvil-session-store--select-columns
                            scope anvil-session-store--bm25-weights limit)
                 params))))))

(defun anvil-session-store--like-candidates (db terms session-id limit)
  "LIKE pass over TERMS — catches the 1-2 char CJK the tokenizer drops."
  (when terms
    (let* ((clauses
            (mapconcat (lambda (_tm)
                         "(lower(summary) LIKE ? ESCAPE '\\'
                           OR lower(data) LIKE ? ESCAPE '\\')")
                       terms " OR "))
           (params (apply #'append
                          (mapcar (lambda (tm)
                                    (let ((p (concat "%"
                                                     (anvil-session-store--like-escape tm)
                                                     "%")))
                                      (list p p)))
                                  terms)))
           (scope ""))
      (when session-id
        (setq scope " AND session_id = ?"
              params (append params (list session-id))))
      (mapcar #'anvil-session-store--row->plist
              (sqlite-select
               db (format "SELECT %s FROM session_events
                            WHERE (%s)%s
                            ORDER BY CAST(ts AS REAL) DESC
                            LIMIT %d"
                          anvil-session-store--select-columns
                          clauses scope limit)
               params)))))

;;;###autoload
(cl-defun anvil-session-store-search (query &key session-id limit)
  "Return events matching QUERY, best first.

Retrieval runs two passes and fuses them:

  1. FTS5 MATCH on terms of 3+ characters, ordered by bm25() with
     `data' weighted 5x.
  2. A LIKE scan over every term, which is the only pass that can
     match a 1-2 character CJK query, since neither trigram nor
     unicode61 emits a token that short.

The two ranked lists are merged by Reciprocal Rank Fusion (K=60),
then reranked on proximity: identifier hits, the tightest window
covering all terms, and adjacent-term pairs.

SESSION-ID scopes to one conversation.  LIMIT defaults to 20."
  (let* ((db (anvil-session-store--db))
         (limit (or limit 20))
         (terms (anvil-session-store--terms query)))
    (if (null terms)
        (anvil-session-store-recent :session-id session-id :limit limit)
      (let* ((pool (max (* limit 2) 20))
             (matched (anvil-session-store--match-candidates
                       db terms session-id pool))
             (liked (anvil-session-store--like-candidates
                     db terms session-id pool))
             (fused (anvil-session-store--rrf
                     (delq nil (list matched liked)) 60)))
        (cl-subseq
         (sort fused
               (lambda (a b)
                 (> (anvil-session-store--proximity-boost a terms)
                    (anvil-session-store--proximity-boost b terms))))
         0 (min limit (length fused)))))))

;;;; --- reference snapshot -------------------------------------------------

(defun anvil-session-store--search-call (query session-id)
  "Render the runnable retrieval call embedded in a snapshot section."
  (format "session-events-search(query: %S, session_id: %S)"
          query (or session-id "")))

(defun anvil-session-store--section (category rows session-id max-items)
  "Render one snapshot section for CATEGORY over ROWS.
Returns nil when ROWS is empty."
  (when rows
    (let* ((shown (last rows (min max-items (length rows))))
           (lines (mapcar
                   (lambda (r)
                     (let ((data (or (plist-get r :data) ""))
                           (summary (or (plist-get r :summary) "")))
                       (format "    %s" (if (string-empty-p data)
                                            summary
                                          data))))
                   shown))
           (query (mapconcat (lambda (r)
                               (or (plist-get r :data)
                                   (plist-get r :summary) ""))
                             (last shown (min 2 (length shown)))
                             " ")))
      (string-join
       (append
        (list (format "  <%s count=\"%d\">" category (length rows)))
        lines
        (list (format "    full rows: %s"
                      (anvil-session-store--search-call
                       (if (string-empty-p (string-trim query))
                           (format "%s" category)
                         (string-trim query))
                       session-id))
              (format "  </%s>" category)))
       "\n"))))

;;;###autoload
(cl-defun anvil-session-store-snapshot-ref (session-id &key max-bytes max-items)
  "Build a reference snapshot of SESSION-ID's events.

The result is a table of contents, not a compressed transcript.
Each section names a category, lists the most recent identifiers in
it, and embeds a `session-events-search' call that retrieves the
full rows.  Event bodies stay in the database, so nothing is
truncated mid-record.

MAX-BYTES defaults to `anvil-session-store-snapshot-max-bytes'.
When the budget is tight, whole sections are dropped in descending
priority order — a section is either rendered intact or omitted, so
the caller never reads a half-written record and mistakes it for
the whole story.  MAX-ITEMS caps the lines shown per section
\(default 8)."
  (let* ((budget (or max-bytes anvil-session-store-snapshot-max-bytes))
         (max-items (or max-items 8))
         (rows (anvil-session-store-recent :session-id session-id :limit 500))
         (by-cat (make-hash-table :test #'eq)))
    (dolist (r rows)
      (let ((c (plist-get r :category)))
        (puthash c (append (gethash c by-cat) (list r)) by-cat)))
    (let (groups)
      (maphash (lambda (cat rs)
                 (push (list (apply #'min (mapcar (lambda (r)
                                                    (plist-get r :priority))
                                                  rs))
                             cat rs)
                       groups))
               by-cat)
      ;; Lowest priority number first = most important rendered first, and
      ;; therefore most likely to fit.
      (setq groups (sort groups (lambda (a b) (< (car a) (car b)))))
      (let* ((header (format "<session_resume id=\"%s\" events=\"%d\">"
                             (or session-id "") (length rows)))
             (footer "</session_resume>")
             (used (+ (string-bytes header) (string-bytes footer) 2))
             (body nil)
             (dropped nil))
        (dolist (g groups)
          (let* ((section (anvil-session-store--section
                           (nth 1 g) (nth 2 g) session-id max-items))
                 (cost (and section (+ (string-bytes section) 1))))
            (cond
             ((null section) nil)
             ((<= (+ used cost) budget)
              (setq used (+ used cost))
              (push section body))
             (t (push (symbol-name (nth 1 g)) dropped)))))
        (when dropped
          (let ((note (format "  <omitted categories=\"%s\">retrieve with %s</omitted>"
                              (string-join (nreverse dropped) ",")
                              (anvil-session-store--search-call
                               "" session-id))))
            ;; The note is the one thing allowed to exceed the budget: a
            ;; snapshot that silently forgets a category is worse than one
            ;; a few bytes over that says what it left out.
            (push note body)))
        (string-join (append (list header) (nreverse body) (list footer))
                     "\n")))))

;;;; --- MCP tools ----------------------------------------------------------

(defun anvil-session-store--tool-search (query &optional session-id limit)
  "Ranked search over the indexed session event log.

MCP Parameters:
  query       - Search text; terms are OR-ed.  Empty falls back to recent.
  session-id  - Optional Claude session id to scope the search to.
  limit       - Optional maximum rows to return (default 20)."
  (anvil-session-store-search
   query
   :session-id (and (stringp session-id) (not (string-empty-p session-id))
                    session-id)
   :limit (and limit (anvil-session-store--as-number limit))))

(defun anvil-session-store--tool-recent (&optional session-id limit)
  "Return the most recent indexed session events, newest last.

MCP Parameters:
  session-id  - Optional Claude session id to scope the result to.
  limit       - Optional maximum rows to return (default 20)."
  (anvil-session-store-recent
   :session-id (and (stringp session-id) (not (string-empty-p session-id))
                    session-id)
   :limit (and limit (anvil-session-store--as-number limit))))

(defun anvil-session-store--tool-snapshot (session-id &optional max-bytes)
  "Build a reference snapshot of one session's event log.

MCP Parameters:
  session-id  - Claude session id to summarise.
  max-bytes   - Optional byte budget for the output (default 2048)."
  (anvil-session-store-snapshot-ref
   session-id
   :max-bytes (and max-bytes (anvil-session-store--as-number max-bytes))))

(defun anvil-session-store--register-tools ()
  "Register the session-store MCP tools."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-session-store--tool-search)
   :id "session-events-search-ranked"
   :intent '(session)
   :layer 'workflow
   :server-id anvil-session-store--server-id
   :description
   "Ranked search over the session event log.  FTS5 MATCH (bm25, the
`data' identifier column weighted 5x) fused with a LIKE pass that
catches 1-2 character CJK queries, then reranked on term proximity.
Prefer this over session-events-search when the query has more than
one term.  SESSION-ID scopes to one conversation; LIMIT defaults to
20.  Read-only."
   :read-only t)
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-session-store--tool-recent)
   :id "session-events-recent-indexed"
   :intent '(session)
   :layer 'workflow
   :server-id anvil-session-store--server-id
   :description
   "Return the last LIMIT indexed events (default 20), newest last,
as structured rows carrying category + priority.  Read-only."
   :read-only t)
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-session-store--tool-snapshot)
   :id "session-snapshot-ref"
   :intent '(session)
   :layer 'workflow
   :server-id anvil-session-store--server-id
   :description
   "Build a reference snapshot for SESSION-ID: a table of contents
listing each event category, its most recent identifiers, and a
runnable session-events-search call that retrieves the full rows.
Event bodies stay in the database — nothing is truncated mid-record.
MAX-BYTES bounds the output (default 2048); when it binds, whole
low-priority sections are dropped and named in an <omitted> line.
Read-only."
   :read-only t))

(defun anvil-session-store--unregister-tools ()
  "Remove the session-store MCP tools."
  (dolist (id '("session-events-search-ranked"
                "session-events-recent-indexed"
                "session-snapshot-ref"))
    (ignore-errors
      (anvil-server-unregister-tool id anvil-session-store--server-id))))

;;;###autoload
(defun anvil-session-store-enable ()
  "Register the session-store MCP tools."
  (interactive)
  (anvil-session-store--register-tools))

(defun anvil-session-store-disable ()
  "Unregister the session-store MCP tools and close the database."
  (interactive)
  (anvil-session-store--unregister-tools)
  (anvil-session-store-close))

(provide 'anvil-session-store)
;;; anvil-session-store.el ends here
