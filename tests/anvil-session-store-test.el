;;; anvil-session-store-test.el --- Tests for anvil-session-store -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 63 Phase 1.  Each test runs against a private SQLite file so
;; the user's real event log is never touched.
;;
;; The tests that matter are the ones that fail when a specific
;; mechanism is removed, so they are written against mechanisms
;; rather than against output shape:
;;
;;   - `...-cjk-two-char-needs-like-pass' fails if the LIKE pass is
;;     dropped, because neither trigram nor unicode61 emits a token
;;     short enough to match a 2-character query.
;;   - `...-identifier-column-outranks-prose' fails if the bm25
;;     weights stop favouring `data'.
;;   - `...-proximity-prefers-tighter-span' fails if the reranker is
;;     removed, since both rows carry the same terms.
;;   - `...-budget-drops-whole-sections' fails if the snapshot starts
;;     truncating records instead of omitting categories.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-session-store)

(defmacro anvil-session-store-test--with-db (&rest body)
  "Run BODY against a fresh, private session-store database."
  (declare (indent 0))
  `(let* ((path (make-temp-file "anvil-session-store-test-" nil ".db"))
          (anvil-session-store-db-path path)
          (anvil-session-store--db nil))
     (unwind-protect
         (progn ,@body)
       (anvil-session-store-close)
       (dolist (suffix '("" "-wal" "-shm"))
         (let ((f (concat path suffix)))
           (when (file-exists-p f) (ignore-errors (delete-file f))))))))

(defun anvil-session-store-test--ids (rows)
  "Return the `data' field of each row in ROWS."
  (mapcar (lambda (r) (plist-get r :data)) rows))

;;;; --- write / read roundtrip ---------------------------------------------

(ert-deftest anvil-session-store-test-put-and-recent ()
  "Rows come back newest-last with their structured fields intact."
  (skip-unless (anvil-session-store-available-p))
  (anvil-session-store-test--with-db
    (anvil-session-store-put "s1" "file_edit"
                             :category 'file :data "anvil-session.el"
                             :summary "edited the dispatch table" :ts 100.0)
    (anvil-session-store-put "s1" "git_commit"
                             :category 'git :data "abc1234"
                             :summary "commit on develop" :ts 200.0)
    (let ((rows (anvil-session-store-recent :session-id "s1")))
      (should (equal (anvil-session-store-test--ids rows)
                     '("anvil-session.el" "abc1234")))
      (should (eq (plist-get (car rows) :category) 'file))
      ;; file is priority 1 in the default table.
      (should (= (plist-get (car rows) :priority) 1))
      (should (= (plist-get (car rows) :seq) 1))
      (should (= (plist-get (cadr rows) :seq) 2)))))

(ert-deftest anvil-session-store-test-sessions-are-scoped ()
  "A session-scoped read never returns another session's rows."
  (skip-unless (anvil-session-store-available-p))
  (anvil-session-store-test--with-db
    (anvil-session-store-put "s1" "file_edit" :category 'file :data "mine.el")
    (anvil-session-store-put "s2" "file_edit" :category 'file :data "theirs.el")
    (should (equal (anvil-session-store-test--ids
                    (anvil-session-store-recent :session-id "s1"))
                   '("mine.el")))
    (should (equal (anvil-session-store-test--ids
                    (anvil-session-store-search "el" :session-id "s2"))
                   '("theirs.el")))))

;;;; --- retrieval mechanisms -----------------------------------------------

(ert-deftest anvil-session-store-test-cjk-two-char-needs-like-pass ()
  "A 2-character CJK query matches — the FTS5 MATCH pass alone cannot.

This is the regression guard for the LIKE pass.  Terms under 3
characters are filtered out before MATCH runs, so if the LIKE pass
is deleted this search returns nothing."
  (skip-unless (anvil-session-store-available-p))
  (anvil-session-store-test--with-db
    (anvil-session-store-put "s1" "user-prompt"
                             :category 'prompt
                             :data "capture/todo.org"
                             :summary "点検の予定を確認した")
    (anvil-session-store-put "s1" "file_edit"
                             :category 'file
                             :data "README.org"
                             :summary "unrelated english row")
    (let ((hits (anvil-session-store-search "点検" :session-id "s1")))
      (should (= (length hits) 1))
      (should (equal (plist-get (car hits) :data) "capture/todo.org")))))

(ert-deftest anvil-session-store-test-identifier-column-outranks-prose ()
  "A hit in `data' beats a hit of the same term in `summary' alone.

The expected winner is inserted FIRST on purpose.  The LIKE pass
orders by `ts DESC', so recency alone would rank it last — only the
bm25 identifier weight and the data-boost can pull it to the front.
Insert it second and this test passes even with ranking disabled."
  (skip-unless (anvil-session-store-available-p))
  (anvil-session-store-test--with-db
    (anvil-session-store-put "s1" "file_edit"
                             :category 'file
                             :data "anvil-worker.el"
                             :summary "some other prose entirely"
                             :ts 100.0)
    (anvil-session-store-put "s1" "file_edit"
                             :category 'file
                             :data "unrelated-path.el"
                             :summary "mentions worker in prose only"
                             :ts 200.0)
    (let ((hits (anvil-session-store-search "worker" :session-id "s1")))
      (should (equal (plist-get (car hits) :data) "anvil-worker.el")))))

(ert-deftest anvil-session-store-test-proximity-prefers-tighter-span ()
  "Both rows carry both terms; the one where they sit together wins.

As above, the expected winner is the OLDER row, so recency ordering
works against it and only the proximity rerank can surface it."
  (skip-unless (anvil-session-store-available-p))
  (anvil-session-store-test--with-db
    (anvil-session-store-put
     "s1" "error" :category 'error :data "near.el"
     :summary "the worker retry cap fired immediately"
     :ts 100.0)
    (anvil-session-store-put
     "s1" "error" :category 'error :data "far.el"
     :summary (concat "retry cap exceeded and then a long stretch of "
                      "unrelated narrative filler text before we finally "
                      "mention the worker subsystem at the very end")
     :ts 200.0)
    (let ((hits (anvil-session-store-search "worker retry" :session-id "s1")))
      (should (equal (plist-get (car hits) :data) "near.el")))))

(ert-deftest anvil-session-store-test-min-span-is-tightest-window ()
  "`--min-span' returns the tightest window, not the first or the average."
  ;; term A at 0 and 100, term B at 98 -> tightest window is 98..100 = 2.
  (should (= (anvil-session-store--min-span '((0 100) (98))) 2))
  ;; A single unmatched term makes the span undefined.
  (should (null (anvil-session-store--min-span '((0 100) ()))))
  ;; Repetition must not change the answer.
  (should (= (anvil-session-store--min-span '((0 5 5 5) (6)))
             (anvil-session-store--min-span '((0 5) (6))))))

(ert-deftest anvil-session-store-test-empty-query-falls-back-to-recent ()
  "An empty query returns recent rows rather than erroring or matching all."
  (skip-unless (anvil-session-store-available-p))
  (anvil-session-store-test--with-db
    (anvil-session-store-put "s1" "file_edit" :category 'file :data "a.el")
    (should (equal (anvil-session-store-test--ids
                    (anvil-session-store-search "" :session-id "s1"))
                   '("a.el")))))

;;;; --- reference snapshot -------------------------------------------------

(ert-deftest anvil-session-store-test-snapshot-embeds-retrieval-call ()
  "Every rendered section carries a runnable retrieval call."
  (skip-unless (anvil-session-store-available-p))
  (anvil-session-store-test--with-db
    (anvil-session-store-put "s1" "file_edit" :category 'file
                             :data "anvil-session.el" :summary "edited")
    (anvil-session-store-put "s1" "git_commit" :category 'git
                             :data "abc1234" :summary "committed")
    (let ((snap (anvil-session-store-snapshot-ref "s1")))
      (should (string-match-p "<file count=\"1\">" snap))
      (should (string-match-p "<git count=\"1\">" snap))
      (should (string-match-p "anvil-session\\.el" snap))
      ;; The point of a reference snapshot: the way back to full rows.
      (should (string-match-p "session-events-search(" snap)))))

(ert-deftest anvil-session-store-test-budget-drops-whole-sections ()
  "Under a tight budget, sections are omitted and named — never truncated.

The `error' category is priority 1 and `mcp' is priority 3, so the
error section must survive and the mcp section must be the one
named in <omitted>."
  (skip-unless (anvil-session-store-available-p))
  (anvil-session-store-test--with-db
    (dotimes (i 6)
      (anvil-session-store-put "s1" "mcp" :category 'mcp
                               :data (format "mcp-call-%d-with-a-long-name" i)
                               :summary "a low priority mcp round trip"))
    (anvil-session-store-put "s1" "error" :category 'error
                             :data "anvil-worker.el:441"
                             :summary "retry cap exceeded")
    (let ((snap (anvil-session-store-snapshot-ref "s1" :max-bytes 260)))
      (should (string-match-p "<error count=\"1\">" snap))
      (should (string-match-p "<omitted categories=\"[^\"]*mcp" snap))
      ;; A dropped section leaves no partial rendering behind.
      (should-not (string-match-p "<mcp count=" snap))
      ;; And no individual identifier got cut in half.
      (should-not (string-match-p "mcp-call-0-with-a-lo\\'" snap)))))

(ert-deftest anvil-session-store-test-snapshot-respects-byte-budget ()
  "A generous budget renders every section and stays within it."
  (skip-unless (anvil-session-store-available-p))
  (anvil-session-store-test--with-db
    (anvil-session-store-put "s1" "file_edit" :category 'file :data "a.el")
    (anvil-session-store-put "s1" "git_commit" :category 'git :data "abc1234")
    (let ((snap (anvil-session-store-snapshot-ref "s1" :max-bytes 4096)))
      (should (<= (string-bytes snap) 4096))
      (should (string-match-p "<file count=" snap))
      (should (string-match-p "<git count=" snap))
      (should-not (string-match-p "<omitted" snap)))))

;;;; --- maintenance ---------------------------------------------------------

(ert-deftest anvil-session-store-test-prune-drops-only-old-rows ()
  "Prune removes rows past the TTL and leaves fresh ones alone."
  (skip-unless (anvil-session-store-available-p))
  (anvil-session-store-test--with-db
    (anvil-session-store-put "s1" "file_edit" :category 'file :data "old.el"
                             :ts (- (float-time) (* 30 24 60 60)))
    (anvil-session-store-put "s1" "file_edit" :category 'file :data "new.el")
    (should (= (anvil-session-store-prune (* 14 24 60 60)) 1))
    (should (equal (anvil-session-store-test--ids
                    (anvil-session-store-recent :session-id "s1"))
                   '("new.el")))))

(provide 'anvil-session-store-test)
;;; anvil-session-store-test.el ends here
