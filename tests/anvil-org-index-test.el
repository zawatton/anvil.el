;;; anvil-org-index-test.el --- Tests for anvil-org-index -*- lexical-binding: t; -*-

;;; Commentary:

;; Scanner unit tests and a rebuild smoke test that writes a temporary
;; org file, ingests it, and verifies the resulting rows.  The rebuild
;; test is skipped gracefully on Emacs < 29 without built-in sqlite.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil-org-index)

;;;; Scanner unit tests

(defun anvil-org-index-test--scan (content)
  "Return the headline plist list produced by scanning CONTENT."
  (with-temp-buffer
    (insert content)
    (anvil-org-index--scan-buffer)))

(ert-deftest anvil-org-index-test-scan-basic-headline ()
  "A single top-level headline is captured with level and title."
  (let* ((res (anvil-org-index-test--scan "* Hello world\n"))
         (hl  (car res)))
    (should (= 1 (length res)))
    (should (= 1 (plist-get hl :level)))
    (should (equal "Hello world" (plist-get hl :title)))
    (should (null (plist-get hl :todo)))
    (should (null (plist-get hl :tags)))))

(ert-deftest anvil-org-index-test-scan-todo-priority-tags ()
  "TODO keyword, priority cookie and trailing tags are stripped from title."
  (let* ((res (anvil-org-index-test--scan
               "** TODO [#A] Ship the feature   :work:urgent:\n"))
         (hl  (car res)))
    (should (= 2 (plist-get hl :level)))
    (should (equal "TODO" (plist-get hl :todo)))
    (should (equal "A"    (plist-get hl :priority)))
    (should (equal "Ship the feature" (plist-get hl :title)))
    (should (equal '("work" "urgent") (plist-get hl :tags)))))

(ert-deftest anvil-org-index-test-scan-done-keyword ()
  "DONE is recognized as a TODO state."
  (let* ((res (anvil-org-index-test--scan "* DONE Finished task\n"))
         (hl  (car res)))
    (should (equal "DONE" (plist-get hl :todo)))
    (should (equal "Finished task" (plist-get hl :title)))))

(ert-deftest anvil-org-index-test-scan-non-todo-uppercase-not-stripped ()
  "Arbitrary leading uppercase word that is not a TODO keyword stays in title."
  (let* ((res (anvil-org-index-test--scan "* FOO matters\n"))
         (hl  (car res)))
    (should (null (plist-get hl :todo)))
    (should (equal "FOO matters" (plist-get hl :title)))))

(ert-deftest anvil-org-index-test-scan-properties-and-id ()
  "PROPERTIES drawer entries are collected; :ID: also lifted to :org-id."
  (let* ((content "* Item
:PROPERTIES:
:ID:       abc-123
:CATEGORY: work
:END:
")
         (res (anvil-org-index-test--scan content))
         (hl  (car res))
         (props (plist-get hl :properties)))
    (should (equal "abc-123" (plist-get hl :org-id)))
    (should (equal "abc-123" (cdr (assoc "ID" props))))
    (should (equal "work"    (cdr (assoc "CATEGORY" props))))))

(ert-deftest anvil-org-index-test-scan-scheduled-deadline ()
  "SCHEDULED and DEADLINE timestamps attach to the current headline."
  (let* ((content "* Task
SCHEDULED: <2026-04-15 Wed> DEADLINE: <2026-04-20 Mon>
"))
    (let ((hl (car (anvil-org-index-test--scan content))))
      (should (equal "<2026-04-15 Wed>" (plist-get hl :scheduled)))
      (should (equal "<2026-04-20 Mon>" (plist-get hl :deadline))))))

(ert-deftest anvil-org-index-test-scan-multiple-headlines-nested ()
  "Level order and count are preserved across nested siblings."
  (let* ((content "* Parent
** Child 1
** Child 2
*** Grandchild
* Sibling
")
         (res (anvil-org-index-test--scan content))
         (levels (mapcar (lambda (p) (plist-get p :level)) res)))
    (should (= 5 (length res)))
    (should (equal '(1 2 2 3 1) levels))))

(ert-deftest anvil-org-index-test-scan-line-ranges ()
  "line-start and line-end bracket the headline body correctly."
  (let* ((content "* One\nbody\n* Two\nmore\n"))
    (let ((res (anvil-org-index-test--scan content)))
      (should (= 1 (plist-get (nth 0 res) :line-start)))
      (should (= 2 (plist-get (nth 0 res) :line-end)))
      (should (= 3 (plist-get (nth 1 res) :line-start))))))

;;;; Parent-child linking via stack

(ert-deftest anvil-org-index-test-parent-link-from-ingest ()
  "Stack-based parent resolution matches expected parent indices.
This exercises the same traversal logic used in
`anvil-org-index--ingest-file' without requiring a DB."
  (let* ((content "* A
** A1
*** A1a
** A2
* B
")
         (headlines (anvil-org-index-test--scan content))
         (parents nil)
         (stack nil)
         (i 0))
    (dolist (hl headlines)
      (let ((level (plist-get hl :level)))
        (while (and stack (>= (caar stack) level))
          (pop stack))
        (push (and stack (cdar stack)) parents)
        (push (cons level i) stack)
        (cl-incf i)))
    (setq parents (nreverse parents))
    ;; indices:   0=A  1=A1  2=A1a  3=A2  4=B
    ;; parents:  nil    0     1      0   nil
    (should (equal '(nil 0 1 0 nil) parents))))

;;;; DB smoke test — skipped if no sqlite backend

(defun anvil-org-index-test--have-sqlite ()
  (and (fboundp 'sqlite-available-p) (sqlite-available-p)))

(ert-deftest anvil-org-index-test-rebuild-roundtrip ()
  "Write temp org, rebuild index, verify counts via status queries."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (let* ((tmpdir (make-temp-file "anvil-org-index-test-" t))
         (orgfile (expand-file-name "sample.org" tmpdir))
         (dbfile (expand-file-name "test-index.db" tmpdir))
         (anvil-org-index-db-path dbfile)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region
             "* TODO [#B] One :alpha:
:PROPERTIES:
:ID: root-1
:END:
SCHEDULED: <2026-04-15 Wed>
** DONE Two :alpha:beta:
* Sibling
"
             nil orgfile nil 'silent))
          (anvil-org-index-enable)
          (let ((summary (anvil-org-index-rebuild)))
            (should (= 1 (plist-get summary :files)))
            (should (= 3 (plist-get summary :headlines))))
          (let ((rows (anvil-org-index--select
                       anvil-org-index--db
                       "SELECT title, todo, priority, org_id FROM headline ORDER BY line_start")))
            (should (equal '("One" "TODO" "B" "root-1") (car rows)))
            (should (equal '("Two" "DONE" nil nil)      (cadr rows)))
            (should (equal '("Sibling" nil nil nil)     (caddr rows))))
          (let ((tag-rows (anvil-org-index--select
                           anvil-org-index--db
                           "SELECT DISTINCT tag FROM tag ORDER BY tag")))
            (should (equal '(("alpha") ("beta")) tag-rows))))
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (delete-directory tmpdir t)))))

;;;; Phase 2 — incremental refresh

(defun anvil-org-index-test--write (path content)
  "Write CONTENT to PATH as UTF-8."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil path nil 'silent)))

(defun anvil-org-index-test--touch-future (path seconds)
  "Set PATH mtime to SECONDS in the future of its current mtime.
Uses a shell `touch' via `set-file-times' for portability."
  (let* ((attrs (file-attributes path))
         (now   (float-time (file-attribute-modification-time attrs)))
         (new   (seconds-to-time (+ now seconds))))
    (set-file-times path new new)))

(ert-deftest anvil-org-index-test-refresh-unchanged-is-noop ()
  "refresh-if-stale on an unchanged file reports outcome=unchanged."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (let* ((tmpdir (make-temp-file "anvil-idx-" t))
         (f (expand-file-name "a.org" tmpdir))
         (db (expand-file-name "i.db" tmpdir))
         (anvil-org-index-db-path db)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil))
    (unwind-protect
        (progn
          (anvil-org-index-test--write f "* One\n")
          (anvil-org-index-enable)
          (anvil-org-index-rebuild)
          (let ((res (anvil-org-index-refresh-if-stale f)))
            (should (eq 'unchanged (plist-get res :outcome)))))
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest anvil-org-index-test-refresh-detects-modification ()
  "Touching a file to a newer mtime triggers reindex."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (let* ((tmpdir (make-temp-file "anvil-idx-" t))
         (f (expand-file-name "a.org" tmpdir))
         (db (expand-file-name "i.db" tmpdir))
         (anvil-org-index-db-path db)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil))
    (unwind-protect
        (progn
          (anvil-org-index-test--write f "* One\n")
          (anvil-org-index-enable)
          (anvil-org-index-rebuild)
          (should (= 1 (caar (anvil-org-index--select
                              anvil-org-index--db
                              "SELECT COUNT(*) FROM headline"))))
          (anvil-org-index-test--write f "* One\n* Two\n")
          (anvil-org-index-test--touch-future f 5)
          (let ((res (anvil-org-index-refresh-if-stale f)))
            (should (eq 'reindexed (plist-get res :outcome))))
          (should (= 2 (caar (anvil-org-index--select
                              anvil-org-index--db
                              "SELECT COUNT(*) FROM headline")))))
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest anvil-org-index-test-refresh-removes-deleted-file ()
  "Deleting a file from disk then refreshing drops its rows."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (let* ((tmpdir (make-temp-file "anvil-idx-" t))
         (f (expand-file-name "a.org" tmpdir))
         (db (expand-file-name "i.db" tmpdir))
         (anvil-org-index-db-path db)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil))
    (unwind-protect
        (progn
          (anvil-org-index-test--write f "* One\n")
          (anvil-org-index-enable)
          (anvil-org-index-rebuild)
          (delete-file f)
          (let ((res (anvil-org-index-refresh-if-stale)))
            (should (= 0 (plist-get res :scanned)))
            (should (= 1 (plist-get res :removed))))
          (should (= 0 (caar (anvil-org-index--select
                              anvil-org-index--db
                              "SELECT COUNT(*) FROM file")))))
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest anvil-org-index-test-refresh-all-mixed-changes ()
  "refresh-all handles added / reindexed / removed / unchanged together."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (let* ((tmpdir (make-temp-file "anvil-idx-" t))
         (a (expand-file-name "a.org" tmpdir))  ; unchanged
         (b (expand-file-name "b.org" tmpdir))  ; will be modified
         (c (expand-file-name "c.org" tmpdir))  ; will be deleted
         (d (expand-file-name "d.org" tmpdir))  ; will be added
         (db (expand-file-name "i.db" tmpdir))
         (anvil-org-index-db-path db)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil))
    (unwind-protect
        (progn
          (anvil-org-index-test--write a "* A\n")
          (anvil-org-index-test--write b "* B\n")
          (anvil-org-index-test--write c "* C\n")
          (anvil-org-index-enable)
          (anvil-org-index-rebuild)
          ;; mutate
          (anvil-org-index-test--write b "* B\n* B2\n")
          (anvil-org-index-test--touch-future b 5)
          (delete-file c)
          (anvil-org-index-test--write d "* D\n")
          (let ((res (anvil-org-index-refresh-if-stale)))
            (should (= 3 (plist-get res :scanned)))  ; a b d on disk
            (should (= 1 (plist-get res :added)))
            (should (= 1 (plist-get res :reindexed)))
            (should (= 1 (plist-get res :removed)))
            (should (= 1 (plist-get res :unchanged)))))
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (delete-directory tmpdir t)))))

;;;; Phase 3 — filesystem watcher

(ert-deftest anvil-org-index-test-collect-dirs-includes-only-org-bearing ()
  "collect-dirs returns directories that hold .org files and skips others."
  (let* ((root (make-temp-file "anvil-idx-watch-" t))
         (with-org   (expand-file-name "a" root))
         (with-org-2 (expand-file-name "a/b" root))
         (empty      (expand-file-name "empty" root))
         (anvil-org-index-paths (list root)))
    (unwind-protect
        (progn
          (make-directory with-org-2 t)
          (make-directory empty t)
          (anvil-org-index-test--write
           (expand-file-name "top.org" with-org) "* x\n")
          (anvil-org-index-test--write
           (expand-file-name "nested.org" with-org-2) "* y\n")
          (let* ((dirs (mapcar #'file-name-as-directory
                               (anvil-org-index--collect-dirs)))
                 (want (list (file-name-as-directory with-org)
                             (file-name-as-directory with-org-2))))
            (should (cl-every (lambda (d) (member d dirs)) want))
            (should-not
             (member (file-name-as-directory empty) dirs))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest anvil-org-index-test-schedule-refresh-coalesces ()
  "Two scheduled refreshes for the same path leave one active timer."
  (let ((anvil-org-index--refresh-timer-map (make-hash-table :test 'equal))
        (anvil-org-index-watch-debounce-seconds 60.0)
        (path "/tmp/nonexistent-test-path.org"))
    (unwind-protect
        (progn
          (anvil-org-index--schedule-refresh path)
          (let ((first (gethash path anvil-org-index--refresh-timer-map)))
            (should (timerp first))
            (anvil-org-index--schedule-refresh path)
            (let ((second (gethash path
                                   anvil-org-index--refresh-timer-map)))
              (should (timerp second))
              (should-not (eq first second)))))
      (maphash (lambda (_k tm) (when (timerp tm) (cancel-timer tm)))
               anvil-org-index--refresh-timer-map))))

(ert-deftest anvil-org-index-test-on-file-event-triggers-schedule ()
  "A synthesized .org file-notify event pushes a timer into the map."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (let* ((tmpdir (make-temp-file "anvil-idx-evt-" t))
         (f (expand-file-name "watched.org" tmpdir))
         (db (expand-file-name "i.db" tmpdir))
         (anvil-org-index-db-path db)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index-watch-debounce-seconds 60.0)
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil)
         (anvil-org-index--refresh-timer-map
          (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (anvil-org-index-test--write f "* one\n")
          (anvil-org-index-enable)
          (anvil-org-index-rebuild)
          (anvil-org-index--on-file-event (list 'dummy 'changed f))
          (should (gethash (expand-file-name f)
                           anvil-org-index--refresh-timer-map)))
      (maphash (lambda (_k tm) (when (timerp tm) (cancel-timer tm)))
               anvil-org-index--refresh-timer-map)
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest anvil-org-index-test-on-file-event-ignores-non-org ()
  "Events for non-.org files do not schedule a refresh."
  (let ((anvil-org-index--refresh-timer-map (make-hash-table :test 'equal)))
    (anvil-org-index--on-file-event
     (list 'dummy 'changed "/tmp/readme.md"))
    (should (zerop (hash-table-count
                    anvil-org-index--refresh-timer-map)))))

(ert-deftest anvil-org-index-test-perform-refresh-reindexes ()
  "perform-refresh picks up on-disk changes without waiting on the timer."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (let* ((tmpdir (make-temp-file "anvil-idx-perf-" t))
         (f (expand-file-name "a.org" tmpdir))
         (db (expand-file-name "i.db" tmpdir))
         (anvil-org-index-db-path db)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil)
         (anvil-org-index--refresh-timer-map
          (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (anvil-org-index-test--write f "* one\n")
          (anvil-org-index-enable)
          (anvil-org-index-rebuild)
          (anvil-org-index-test--write f "* one\n* two\n")
          (anvil-org-index-test--touch-future f 5)
          (anvil-org-index--perform-refresh f)
          (should (= 2 (caar (anvil-org-index--select
                              anvil-org-index--db
                              "SELECT COUNT(*) FROM headline"))))
          (should (zerop (hash-table-count
                          anvil-org-index--refresh-timer-map))))
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest anvil-org-index-test-watch-and-unwatch-lifecycle ()
  "watch creates descriptors; unwatch clears them and the timer map."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (skip-unless (require 'filenotify nil t))
  (let* ((tmpdir (make-temp-file "anvil-idx-watch-lc-" t))
         (db (expand-file-name "i.db" tmpdir))
         (anvil-org-index-db-path db)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index-periodic-scan-seconds 0)
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil)
         (anvil-org-index--watch-descriptors nil)
         (anvil-org-index--refresh-timer-map
          (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (anvil-org-index-test--write
           (expand-file-name "one.org" tmpdir) "* one\n")
          (let ((dirs (anvil-org-index-watch)))
            (should (>= (length dirs) 1))
            (should (>= (length anvil-org-index--watch-descriptors) 1)))
          (anvil-org-index-unwatch)
          (should-not anvil-org-index--watch-descriptors)
          (should (zerop (hash-table-count
                          anvil-org-index--refresh-timer-map))))
      (ignore-errors (anvil-org-index-unwatch))
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (delete-directory tmpdir t)))))

;;;; Phase 4a — fast-path readers

(defun anvil-org-index-test--seed (root)
  "Write a small org file under ROOT and return its absolute path."
  (let ((f (expand-file-name "fp.org" root)))
    (anvil-org-index-test--write
     f
     "* Alpha
:PROPERTIES:
:ID: id-alpha
:END:
Alpha body
** Beta
:PROPERTIES:
:ID: id-beta
:END:
Beta body
** Beta
Duplicate title, no ID
* Gamma
:PROPERTIES:
:ID: id-gamma
:END:
Gamma body line
")
    f))

(defmacro anvil-org-index-test--with-seeded (body-var &rest body)
  "Seed a temp index into DB and bind BODY-VAR to the seeded org path, then run BODY."
  (declare (indent 1))
  `(let* ((tmpdir (make-temp-file "anvil-idx-fp-" t))
          (db (expand-file-name "i.db" tmpdir))
          (,body-var nil)
          (anvil-org-index-db-path db)
          (anvil-org-index-paths (list tmpdir))
          (anvil-org-index--backend nil)
          (anvil-org-index--db nil))
     (unwind-protect
         (progn
           (setq ,body-var (anvil-org-index-test--seed tmpdir))
           (anvil-org-index-enable)
           (anvil-org-index-rebuild)
           ,@body)
       (ignore-errors (anvil-org-index-disable))
       (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest anvil-org-index-test-read-by-id-returns-body ()
  "read-by-id returns the body section including properties drawer."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-seeded _f
    (let ((txt (anvil-org-index-read-by-id "id-beta")))
      (should (string-match-p "^\\*\\* Beta" txt))
      (should (string-match-p "Beta body" txt))
      (should-not (string-match-p "Gamma" txt)))))

(ert-deftest anvil-org-index-test-read-by-id-missing-errors ()
  "read-by-id signals user-error on unknown ID."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-seeded _f
    (should-error (anvil-org-index-read-by-id "no-such-id"))))

(ert-deftest anvil-org-index-test-read-headline-top-level ()
  "read-headline resolves a top-level title and returns its body."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-seeded f
    (let ((txt (anvil-org-index-read-headline f "Alpha")))
      (should (string-match-p "^\\* Alpha" txt))
      (should (string-match-p "Alpha body" txt))
      ;; Alpha's subtree includes both Beta children
      (should (string-match-p "^\\*\\* Beta" txt)))))

(ert-deftest anvil-org-index-test-read-headline-nested ()
  "read-headline walks a nested slash-separated path."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-seeded f
    ;; With the duplicate "Beta", the top-level Alpha/Beta path is
    ;; ambiguous — the tool must signal rather than guess.
    (should-error (anvil-org-index-read-headline f "Alpha/Beta"))))

(ert-deftest anvil-org-index-test-read-outline-orders-and-tags ()
  "read-outline returns a level/title list in document order."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-seeded f
    (let* ((outline (anvil-org-index-read-outline f))
           (levels  (mapcar (lambda (p) (plist-get p :level)) outline))
           (titles  (mapcar (lambda (p) (plist-get p :title)) outline)))
      (should (equal '(1 2 2 1) levels))
      (should (equal '("Alpha" "Beta" "Beta" "Gamma") titles)))))

(ert-deftest anvil-org-index-test-read-outline-missing-errors ()
  "read-outline signals when the file is not indexed."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-seeded _f
    (should-error
     (anvil-org-index-read-outline
      (expand-file-name "never-indexed.org" temporary-file-directory)))))

;;;; Phase 4c: outline -> JSON tree builder

(defun anvil-org-index-test--flat (&rest entries)
  "Build a flat plist list from ENTRIES of (LEVEL TITLE)."
  (mapcar (lambda (e) (list :level (nth 0 e) :title (nth 1 e))) entries))

(ert-deftest anvil-org-index-test-outline-tree-empty ()
  "An empty flat list produces an empty children vector."
  (let ((tree (anvil-org-index--outline-build-tree nil)))
    (should (vectorp tree))
    (should (= 0 (length tree)))))

(ert-deftest anvil-org-index-test-outline-tree-siblings ()
  "Same-level siblings land under the same parent."
  (let* ((flat (anvil-org-index-test--flat '(1 "A") '(1 "B") '(1 "C")))
         (tree (anvil-org-index--outline-build-tree flat)))
    (should (= 3 (length tree)))
    (should (equal "A" (alist-get 'title (aref tree 0))))
    (should (equal "C" (alist-get 'title (aref tree 2))))
    (should (= 0 (length (alist-get 'children (aref tree 1)))))))

(ert-deftest anvil-org-index-test-outline-tree-full-depth ()
  "Nesting is populated at full depth, not clipped to two levels."
  (let* ((flat (anvil-org-index-test--flat
                '(1 "A") '(2 "A1") '(3 "A1a") '(3 "A1b") '(2 "A2")))
         (tree (anvil-org-index--outline-build-tree flat))
         (a  (aref tree 0))
         (a1 (aref (alist-get 'children a) 0))
         (a2 (aref (alist-get 'children a) 1)))
    (should (equal "A"  (alist-get 'title a)))
    (should (equal "A1" (alist-get 'title a1)))
    (should (equal "A2" (alist-get 'title a2)))
    (should (= 2 (length (alist-get 'children a1))))
    (should (equal "A1a"
                   (alist-get 'title
                              (aref (alist-get 'children a1) 0))))))

(ert-deftest anvil-org-index-test-outline-tree-skip-levels ()
  "Jumping from level 1 to level 3 still nests under level 1."
  (let* ((flat (anvil-org-index-test--flat '(1 "A") '(3 "deep") '(1 "B")))
         (tree (anvil-org-index--outline-build-tree flat))
         (a (aref tree 0)))
    (should (= 2 (length tree)))
    (should (= 1 (length (alist-get 'children a))))
    (should (equal "deep"
                   (alist-get 'title
                              (aref (alist-get 'children a) 0))))))

(ert-deftest anvil-org-index-test-read-outline-json-shape ()
  "read-outline-json returns a JSON object with a headings vector."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-seeded f
    (let* ((json-object-type 'alist)
           (json-array-type  'vector)
           (obj  (json-read-from-string
                  (anvil-org-index-read-outline-json f)))
           (hds  (alist-get 'headings obj))
           (alpha (aref hds 0))
           (gamma (aref hds 1)))
      (should (= 2 (length hds)))
      (should (equal "Alpha" (alist-get 'title alpha)))
      (should (equal "Gamma" (alist-get 'title gamma)))
      (should (= 2 (length (alist-get 'children alpha))))
      (should (equal "Beta"
                     (alist-get 'title
                                (aref (alist-get 'children alpha) 0)))))))

;;;; Phase 4c extension: max-depth filter

(ert-deftest anvil-org-index-test-read-outline-max-depth-1 ()
  "max-depth=1 drops level-2 headlines from the flat list."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-seeded f
    (let* ((rows   (anvil-org-index-read-outline f 1))
           (levels (mapcar (lambda (p) (plist-get p :level)) rows)))
      (should (cl-every (lambda (l) (<= l 1)) levels))
      (should (equal '(1 1) levels)))))

(ert-deftest anvil-org-index-test-read-outline-max-depth-2-matches-default ()
  "max-depth=2 equals the unfiltered read for the seeded fixture."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-seeded f
    (should (equal (anvil-org-index-read-outline f)
                   (anvil-org-index-read-outline f 2)))))

(ert-deftest anvil-org-index-test-read-outline-json-max-depth-1 ()
  "max-depth=1 JSON outline has no children even for Alpha."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-seeded f
    (let* ((json-object-type 'alist)
           (json-array-type  'vector)
           (obj  (json-read-from-string
                  (anvil-org-index-read-outline-json f 1)))
           (hds  (alist-get 'headings obj))
           (alpha (aref hds 0)))
      (should (= 2 (length hds)))
      (should (equal "Alpha" (alist-get 'title alpha)))
      (should (= 0 (length (alist-get 'children alpha)))))))

;;;; Phase 4c+: org-index-search

(defun anvil-org-index-test--seed-search (root)
  "Write a richer fixture for search tests and return its path."
  (let ((f (expand-file-name "search.org" root)))
    (anvil-org-index-test--write
     f
     "* TODO Build invoice :work:billing:
SCHEDULED: <2026-04-18 Sat>
DEADLINE: <2026-04-20 Mon>
Body of invoice task.
* DONE Ship Q4 report :work:report:
SCHEDULED: <2026-03-30 Mon>
Closed already.
* NEXT Review contract :work:legal:
DEADLINE: <2026-05-01 Fri>
Lawyer review pending.
* Personal errand :home:
* TODO Fix broken sensor :work:hw:
DEADLINE: <2026-04-10 Fri>
")
    f))

(defmacro anvil-org-index-test--with-search-seed (body-var &rest body)
  "Seed the search fixture and run BODY with BODY-VAR bound to its path."
  (declare (indent 1))
  `(let* ((tmpdir (make-temp-file "anvil-idx-sr-" t))
          (db (expand-file-name "i.db" tmpdir))
          (,body-var nil)
          (anvil-org-index-db-path db)
          (anvil-org-index-paths (list tmpdir))
          (anvil-org-index--backend nil)
          (anvil-org-index--db nil))
     (unwind-protect
         (progn
           (setq ,body-var (anvil-org-index-test--seed-search tmpdir))
           (anvil-org-index-enable)
           (anvil-org-index-rebuild)
           ,@body)
       (ignore-errors (anvil-org-index-disable))
       (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest anvil-org-index-test-search-title-like ()
  "Title LIKE matches case-sensitive SQL semantics; auto-wraps %..%."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-search-seed _f
    (let* ((res (anvil-org-index-search :title-like "invoice"))
           (rows (plist-get res :rows)))
      (should (= 1 (plist-get res :count)))
      (should (string-match-p "invoice" (plist-get (car rows) :title))))))

(ert-deftest anvil-org-index-test-search-todo-filter ()
  "TODO filter supports a single keyword or comma-separated OR."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-search-seed _f
    (let ((only-todo (plist-get (anvil-org-index-search :todo "TODO") :rows))
          (todo-next (plist-get (anvil-org-index-search
                                 :todo '("TODO" "NEXT")) :rows)))
      (should (= 2 (length only-todo)))
      (should (cl-every (lambda (r) (equal (plist-get r :todo) "TODO"))
                        only-todo))
      (should (= 3 (length todo-next)))
      (should (cl-every (lambda (r)
                          (member (plist-get r :todo) '("TODO" "NEXT")))
                        todo-next)))))

(ert-deftest anvil-org-index-test-search-tag-and ()
  "Tag filter requires ALL tags to match (AND-of-EXISTS)."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-search-seed _f
    (let ((work       (plist-get (anvil-org-index-search :tag "work") :rows))
          (work+billing (plist-get (anvil-org-index-search
                                    :tag '("work" "billing"))
                                   :rows))
          (nohit (plist-get (anvil-org-index-search
                             :tag '("work" "nosuchtag"))
                            :rows)))
      (should (= 4 (length work)))
      (should (= 1 (length work+billing)))
      (should (equal "Build invoice"
                     (plist-get (car work+billing) :title)))
      (should (null nohit)))))

(ert-deftest anvil-org-index-test-search-scheduled-range ()
  "SCHEDULED is compared as the YYYY-MM-DD slice of the raw timestamp."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-search-seed _f
    (let ((early (plist-get (anvil-org-index-search
                             :scheduled-before "2026-03-31")
                            :rows))
          (april (plist-get (anvil-org-index-search
                             :scheduled-after "2026-04-01")
                            :rows)))
      (should (= 1 (length early)))
      (should (equal "Ship Q4 report"
                     (plist-get (car early) :title)))
      (should (= 1 (length april)))
      (should (equal "Build invoice"
                     (plist-get (car april) :title))))))

(ert-deftest anvil-org-index-test-search-deadline-before ()
  "DEADLINE bounds drop entries with no DEADLINE and ones past the bound."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-search-seed _f
    (let ((res (plist-get (anvil-org-index-search
                           :deadline-before "2026-04-30")
                          :rows)))
      (should (cl-every (lambda (r) (plist-get r :deadline)) res))
      (should (= 2 (length res)))
      (should (member "Build invoice"
                      (mapcar (lambda (r) (plist-get r :title)) res)))
      (should (member "Fix broken sensor"
                      (mapcar (lambda (r) (plist-get r :title)) res))))))

(ert-deftest anvil-org-index-test-search-limit-offset ()
  "Limit + offset paginate; truncated flag surfaces extra rows."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-search-seed _f
    (let* ((page1 (anvil-org-index-search :limit 2))
           (page2 (anvil-org-index-search :limit 2 :offset 2)))
      (should (= 2 (plist-get page1 :count)))
      (should (eq t (plist-get page1 :truncated)))
      (should (= 2 (plist-get page2 :count)))
      (should-not (equal (mapcar (lambda (r) (plist-get r :title))
                                 (plist-get page1 :rows))
                         (mapcar (lambda (r) (plist-get r :title))
                                 (plist-get page2 :rows)))))))

(ert-deftest anvil-org-index-test-search-empty-result ()
  "An impossible filter returns :count 0 :rows nil, not an error."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-search-seed _f
    (let ((res (anvil-org-index-search :title-like "zzzzz-no-hit")))
      (should (= 0 (plist-get res :count)))
      (should (null (plist-get res :rows)))
      (should-not (plist-get res :truncated)))))

;;; anvil-org-index-test.el ends here

;;;; Phase 4c++: property-key / property-value search

(defun anvil-org-index-test--seed-property-search (root)
  "Seed a fixture with :PROPERTIES: drawers and return its path."
  (let ((f (expand-file-name "prop.org" root)))
    (anvil-org-index-test--write
     f
     "* Alpha task
:PROPERTIES:
:CATEGORY: finance
:OWNER: alice
:END:
Body alpha.
* Bravo task
:PROPERTIES:
:CATEGORY: finance
:OWNER: bob
:END:
Body bravo.
* Charlie task
:PROPERTIES:
:CATEGORY: ops
:OWNER: carol
:END:
Body charlie.
* Delta task
Body delta (no drawer).
")
    f))

(defmacro anvil-org-index-test--with-property-seed (body-var &rest body)
  "Seed the property fixture and run BODY with BODY-VAR bound to its path."
  (declare (indent 1))
  `(let* ((tmpdir (make-temp-file "anvil-idx-pr-" t))
          (db (expand-file-name "i.db" tmpdir))
          (,body-var nil)
          (anvil-org-index-db-path db)
          (anvil-org-index-paths (list tmpdir))
          (anvil-org-index--backend nil)
          (anvil-org-index--db nil))
     (unwind-protect
         (progn
           (setq ,body-var (anvil-org-index-test--seed-property-search tmpdir))
           (anvil-org-index-enable)
           (anvil-org-index-rebuild)
           ,@body)
       (ignore-errors (anvil-org-index-disable))
       (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest anvil-org-index-test-search-property-key-only ()
  "Filtering by :property-key returns every headline that has that key."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-property-seed _f
    (let* ((res   (anvil-org-index-search :property-key "CATEGORY"))
           (rows  (plist-get res :rows))
           (names (mapcar (lambda (r) (plist-get r :title)) rows)))
      (should (= 3 (plist-get res :count)))
      (should (member "Alpha task"   names))
      (should (member "Bravo task"   names))
      (should (member "Charlie task" names))
      (should-not (member "Delta task" names)))))

(ert-deftest anvil-org-index-test-search-property-key-and-value ()
  "Combining :property-key and :property-value matches the same drawer row."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-property-seed _f
    (let* ((res  (anvil-org-index-search
                  :property-key   "CATEGORY"
                  :property-value "finance"))
           (rows (plist-get res :rows))
           (names (mapcar (lambda (r) (plist-get r :title)) rows)))
      (should (= 2 (plist-get res :count)))
      (should (member "Alpha task" names))
      (should (member "Bravo task" names))
      (should-not (member "Charlie task" names)))))

(ert-deftest anvil-org-index-test-search-property-key-mismatched-value ()
  "Key+value must live on the SAME row: OWNER=alice + CATEGORY key fails."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-property-seed _f
    ;; The value "alice" exists on the OWNER row, not on CATEGORY.
    (let* ((res (anvil-org-index-search
                 :property-key "CATEGORY" :property-value "alice")))
      (should (= 0 (plist-get res :count)))
      (should (null (plist-get res :rows))))))

(ert-deftest anvil-org-index-test-search-property-value-only ()
  "Bare :property-value matches any key with the given value."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-property-seed _f
    (let* ((res   (anvil-org-index-search :property-value "bob"))
           (rows  (plist-get res :rows))
           (names (mapcar (lambda (r) (plist-get r :title)) rows)))
      (should (= 1 (plist-get res :count)))
      (should (equal "Bravo task" (car names))))))

(ert-deftest anvil-org-index-test-search-property-combines-with-tag ()
  "Property filters AND together with other predicates like :title-like."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (anvil-org-index-test--with-property-seed _f
    (let* ((res (anvil-org-index-search
                 :title-like   "Bravo"
                 :property-key "OWNER"))
           (rows (plist-get res :rows)))
      (should (= 1 (plist-get res :count)))
      (should (equal "Bravo task" (plist-get (car rows) :title))))))
