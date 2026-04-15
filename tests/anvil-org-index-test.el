;;; anvil-org-index-test.el --- Tests for anvil-org-index -*- lexical-binding: t; -*-

;;; Commentary:

;; Scanner unit tests and a rebuild smoke test that writes a temporary
;; org file, ingests it, and verifies the resulting rows.  The rebuild
;; test is skipped gracefully on Emacs < 29 without built-in sqlite.

;;; Code:

(require 'ert)
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

;;; anvil-org-index-test.el ends here
