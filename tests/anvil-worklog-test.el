;;; anvil-worklog-test.el --- Tests for anvil-worklog -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 42 Phase 1 — anvil-worklog org → SQLite scanner,
;; FTS5 search, list, prune, and DB path resolver.
;;
;; Each test isolates itself with a fresh `anvil-worklog-db-path' temp
;; SQLite file and a temp `capture/' directory populated with
;; ai-logs-*.org fixtures so the user's real notes are never touched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'anvil-worklog nil t)


;;;; --- fixture helpers ----------------------------------------------------

(defun anvil-worklog-test--supported-p (tag)
  (and (boundp 'anvil-worklog-supported)
       (memq tag anvil-worklog-supported)))

(defun anvil-worklog-test--write (path content)
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (let ((coding-system-for-write 'utf-8))
      (insert content))))

(defmacro anvil-worklog-test--with-env (&rest body)
  "Run BODY with a fresh temp DB + temp capture/ dir.
Binds:
  `anvil-worklog-db-path' — fresh temp .db
  `anvil-worklog-roots'   — list of the single temp capture dir
  root                    — the temp dir path"
  (declare (indent 0))
  `(let* ((root (make-temp-file "anvil-wltest-" t))
          (anvil-worklog-db-path (make-temp-file "anvil-wlidx-" nil ".db"))
          (anvil-worklog--db nil)
          (anvil-worklog--resolved-db-path nil)
          (anvil-worklog-shared-db-roots nil)
          (anvil-worklog-roots (list root)))
     (unwind-protect
         (progn
           (when (fboundp 'anvil-worklog-enable)
             (anvil-worklog-enable))
           ,@body)
       (when (fboundp 'anvil-worklog-disable)
         (anvil-worklog-disable))
       (ignore-errors (delete-file anvil-worklog-db-path))
       (ignore-errors (delete-directory root t)))))

(defconst anvil-worklog-test--sample-linux
  (concat
   "#+title: AI 作業ログ 2026 (Linux/debian)\n"
   "#+date: 2026-01-01\n"
   "\n"
   "** NOTE 作業ログ <2026-04-28 Tue>\n"
   "*** MEMO AI: 古い案件 <2026-04-28 Tue>\n"
   "**** 実施内容\n"
   "- foo bar\n"
   "**** 成果物\n"
   "- old-thing.el\n"
   "\n"
   "** NOTE 作業ログ <2026-04-29 Wed>\n"
   "*** MEMO AI: anvil-worklog 設計 <2026-04-29 Wed>\n"
   "   Entered on [2026-04-29 Wed 13:30]\n"
   "**** 実施内容\n"
   "- Doc 42 を書いた\n"
   "- 三相誘導電動機の話とは無関係\n"
   "**** 次のアクション\n"
   "- 実装\n"
   "\n"
   "*** MEMO AI: 親 date を継承するエントリ\n"
   "**** 実施内容\n"
   "- date が level-3 に無いので親から fallback\n"))

(defconst anvil-worklog-test--sample-windows
  (concat
   "#+title: AI 作業ログ 2026 (Windows/ThinkPad-E14-Gen5)\n"
   "\n"
   "** NOTE 作業ログ <2026-04-29 Wed>\n"
   "*** MEMO AI: 三相誘導電動機 教材作成 <2026-04-29 Wed>\n"
   "- pptx 生成\n"
   "- 講義スライド完成\n"))

(defun anvil-worklog-test--seed (root)
  (anvil-worklog-test--write
   (expand-file-name "ai-logs-linux-debian-2026.org" root)
   anvil-worklog-test--sample-linux)
  (anvil-worklog-test--write
   (expand-file-name "ai-logs-windows-ThinkPad-E14-Gen5-2026.org" root)
   anvil-worklog-test--sample-windows)
  ;; Decoy: not an ai-logs file, must be ignored.
  (anvil-worklog-test--write
   (expand-file-name "todo.org" root)
   "* TODO not a worklog\n"))


;;;; --- scan ---------------------------------------------------------------

(ert-deftest anvil-worklog-test/scan-detects-memo-entries ()
  "scan extracts every level-3 MEMO AI block as one entry."
  (skip-unless (anvil-worklog-test--supported-p 'scan))
  (anvil-worklog-test--with-env
    (anvil-worklog-test--seed root)
    (let ((counts (anvil-worklog-scan)))
      (should (= 2 (plist-get counts :files)))
      ;; linux fixture has 3 MEMO entries, windows has 1.
      (should (= 4 (plist-get counts :entries)))
      (should (= 4 (plist-get counts :inserted))))))

(ert-deftest anvil-worklog-test/scan-fallback-date-from-parent ()
  "When level-3 has no inactive timestamp, parent ** NOTE date is used."
  (skip-unless (anvil-worklog-test--supported-p 'scan))
  (anvil-worklog-test--with-env
    (anvil-worklog-test--seed root)
    (anvil-worklog-scan)
    (let* ((rows (anvil-worklog-list :limit 50))
           (titles (mapcar (lambda (r) (plist-get r :title)) rows))
           (idx (cl-position "親 date を継承するエントリ" titles
                             :test #'equal))
           (row (nth idx rows)))
      (should row)
      (should (equal "2026-04-29" (plist-get row :date))))))

(ert-deftest anvil-worklog-test/scan-skips-unrelated-org ()
  "scan only ingests files matching `ai-logs-*-YYYY.org'."
  (skip-unless (anvil-worklog-test--supported-p 'scan))
  (anvil-worklog-test--with-env
    (anvil-worklog-test--seed root)
    (let ((counts (anvil-worklog-scan)))
      ;; todo.org must not have been scanned.
      (should (= 2 (plist-get counts :files))))))

(ert-deftest anvil-worklog-test/scan-machine-token-extraction ()
  "Filenames map to machine + year correctly."
  (skip-unless (anvil-worklog-test--supported-p 'scan))
  (anvil-worklog-test--with-env
    (anvil-worklog-test--seed root)
    (anvil-worklog-scan)
    (let* ((rows (anvil-worklog-list :limit 50))
           (machines (delete-dups
                      (mapcar (lambda (r) (plist-get r :machine)) rows))))
      (should (member "linux-debian" machines))
      (should (member "windows-ThinkPad-E14-Gen5" machines))
      (dolist (r rows)
        (should (= 2026 (plist-get r :year)))))))

(ert-deftest anvil-worklog-test/scan-upsert-digest-skip ()
  "Re-running scan with no body change reports zero inserts/updates."
  (skip-unless (anvil-worklog-test--supported-p 'scan))
  (anvil-worklog-test--with-env
    (anvil-worklog-test--seed root)
    (anvil-worklog-scan)
    (let ((second (anvil-worklog-scan)))
      (should (= 0 (plist-get second :inserted)))
      (should (= 0 (plist-get second :updated)))
      (should (= 4 (plist-get second :unchanged))))))

(ert-deftest anvil-worklog-test/scan-upsert-on-body-change ()
  "Editing an entry's body causes scan to UPDATE the row + FTS."
  (skip-unless (anvil-worklog-test--supported-p 'scan))
  (anvil-worklog-test--with-env
    (anvil-worklog-test--seed root)
    (anvil-worklog-scan)
    (let* ((linux-path (expand-file-name "ai-logs-linux-debian-2026.org" root))
           (edited (replace-regexp-in-string
                    "Doc 42 を書いた"
                    "Doc 42 を書き直した（差分検出用）"
                    anvil-worklog-test--sample-linux)))
      (anvil-worklog-test--write linux-path edited)
      (let ((second (anvil-worklog-scan)))
        (should (= 1 (plist-get second :updated)))
        (should (= 3 (plist-get second :unchanged))))
      ;; FTS now finds the new wording, not the old one.
      (let ((hits (anvil-worklog-search "書き直した")))
        (should hits)
        (should (cl-some (lambda (r)
                           (string-match-p "書き直した" (plist-get r :body)))
                         hits))))))


;;;; --- search ------------------------------------------------------------

(ert-deftest anvil-worklog-test/search-fts-matches ()
  "FTS5 trigram (or unicode61) finds substring matches."
  (skip-unless (anvil-worklog-test--supported-p 'search))
  (anvil-worklog-test--with-env
    (anvil-worklog-test--seed root)
    (anvil-worklog-scan)
    (let ((hits (anvil-worklog-search "三相誘導電動機")))
      (should hits)
      (should (cl-some (lambda (r)
                         (string-match-p "三相誘導電動機" (plist-get r :title)))
                       hits)))))

(ert-deftest anvil-worklog-test/search-machine-filter ()
  ":machine restricts hits to one host."
  (skip-unless (anvil-worklog-test--supported-p 'search))
  (anvil-worklog-test--with-env
    (anvil-worklog-test--seed root)
    (anvil-worklog-scan)
    ;; "Doc" lives only in the linux fixture's body.
    (let ((linux (anvil-worklog-search "Doc"
                                       :machine "linux-debian")))
      (should linux)
      (dolist (r linux)
        (should (equal "linux-debian" (plist-get r :machine)))))
    (let ((win (anvil-worklog-search "Doc"
                                     :machine "windows-ThinkPad-E14-Gen5")))
      (should-not win))))

(ert-deftest anvil-worklog-test/search-since-until-filter ()
  ":since / :until bound the date range."
  (skip-unless (anvil-worklog-test--supported-p 'search))
  (anvil-worklog-test--with-env
    (anvil-worklog-test--seed root)
    (anvil-worklog-scan)
    (let ((only-29 (anvil-worklog-search "実施内容"
                                         :since "2026-04-29"
                                         :until "2026-04-29")))
      (should only-29)
      (dolist (r only-29)
        (should (equal "2026-04-29" (plist-get r :date)))))))


;;;; --- list -------------------------------------------------------------

(ert-deftest anvil-worklog-test/list-recent-by-date-desc ()
  "list orders entries newest-first."
  (skip-unless (anvil-worklog-test--supported-p 'list))
  (anvil-worklog-test--with-env
    (anvil-worklog-test--seed root)
    (anvil-worklog-scan)
    (let* ((rows (anvil-worklog-list :limit 50))
           (dates (mapcar (lambda (r) (plist-get r :date)) rows)))
      (should (>= (length dates) 4))
      (should (equal dates (sort (copy-sequence dates) #'string>))))))


;;;; --- symlink dedup -----------------------------------------------------

(ert-deftest anvil-worklog-test/scan-dedupes-symlink-roots ()
  "When two roots resolve to the same canonical path, scan visits once."
  (skip-unless (anvil-worklog-test--supported-p 'scan))
  ;; Build root + a sibling symlink pointing at it; pass both as roots.
  ;; Without dedup each entry would be inserted twice (once per surface
  ;; path) and the second pass would update the first instead of being
  ;; a no-op, so :unchanged would never reach the expected count.
  (let* ((root (make-temp-file "anvil-wldedup-" t))
         (link (concat root "-link"))
         (anvil-worklog-db-path (make-temp-file "anvil-wldedup-" nil ".db"))
         (anvil-worklog--db nil)
         (anvil-worklog--resolved-db-path nil)
         (anvil-worklog-shared-db-roots nil)
         (anvil-worklog-roots (list root link)))
    (unwind-protect
        (progn
          (when (fboundp 'anvil-worklog-enable)
            (anvil-worklog-enable))
          (anvil-worklog-test--seed root)
          (make-symbolic-link root link)
          (let ((counts (anvil-worklog-scan)))
            (should (= 2 (plist-get counts :files)))
            (should (= 4 (plist-get counts :inserted)))))
      (when (fboundp 'anvil-worklog-disable)
        (anvil-worklog-disable))
      (ignore-errors (delete-file anvil-worklog-db-path))
      (ignore-errors (delete-file link))
      (ignore-errors (delete-directory root t)))))


;;;; --- prune ------------------------------------------------------------

(ert-deftest anvil-worklog-test/prune-removes-missing-file-rows ()
  "prune drops rows whose backing org file was removed."
  (skip-unless (anvil-worklog-test--supported-p 'prune))
  (anvil-worklog-test--with-env
    (anvil-worklog-test--seed root)
    (anvil-worklog-scan)
    (let ((linux-path (expand-file-name "ai-logs-linux-debian-2026.org" root)))
      (delete-file linux-path)
      (let ((n (anvil-worklog-prune (list root))))
        (should (= 3 n)))
      ;; Only the windows entry remains.
      (let ((rows (anvil-worklog-list :limit 50)))
        (should (= 1 (length rows)))
        (should (equal "windows-ThinkPad-E14-Gen5"
                       (plist-get (car rows) :machine))))
      ;; FTS row for the deleted entries is gone too.  "Doc" lives
      ;; only in the linux fixture body, so a query post-prune is empty.
      (let ((hits (anvil-worklog-search "Doc")))
        (should-not hits)))))


;;;; --- Phase 2: DB-direct add ------------------------------------------

(ert-deftest anvil-worklog-test/add-inserts-row ()
  "worklog-add stores a row + FTS body and returns its synthetic key."
  (skip-unless (anvil-worklog-test--supported-p 'add))
  (anvil-worklog-test--with-env
    (let ((id (anvil-worklog-add
               "Phase 2 着地ログ"
               "**実施内容**\n- データベース primary に切替\n"
               :date "2026-04-29"
               :machine "linux-debian"
               :year 2026)))
      (should (string-prefix-p "anvil-worklog:db:" (plist-get id :file)))
      (should (= 1 (plist-get id :start-line)))
      (should (equal "linux-debian" (plist-get id :machine)))
      (should (= 2026 (plist-get id :year))))
    (let ((rows (anvil-worklog-list :limit 5 :machine "linux-debian")))
      (should (= 1 (length rows)))
      (should (equal "Phase 2 着地ログ" (plist-get (car rows) :title))))
    (let ((hits (anvil-worklog-search "データベース")))
      (should hits))))

(ert-deftest anvil-worklog-test/add-defaults-machine-and-date ()
  "machine + date default to the current host + today when omitted."
  (skip-unless (anvil-worklog-test--supported-p 'add))
  (anvil-worklog-test--with-env
    (let* ((today (format-time-string "%Y-%m-%d"))
           (anvil-worklog-platform "linux")
           (anvil-worklog-hostname "test-host")
           (id (anvil-worklog-add "Default test" "body")))
      (should (equal today (plist-get id :date)))
      (should (equal "linux-test-host" (plist-get id :machine))))))

(ert-deftest anvil-worklog-test/add-auto-increments-start-line ()
  "Successive calls with the same (machine, year) get consecutive
start_line values; switching namespace resets the counter."
  (skip-unless (anvil-worklog-test--supported-p 'add))
  (anvil-worklog-test--with-env
    (let ((a (anvil-worklog-add "a" "x" :date "2026-04-29"
                                :machine "m1" :year 2026))
          (b (anvil-worklog-add "b" "y" :date "2026-04-29"
                                :machine "m1" :year 2026))
          (c (anvil-worklog-add "c" "z" :date "2026-04-29"
                                :machine "m2" :year 2026)))
      (should (= 1 (plist-get a :start-line)))
      (should (= 2 (plist-get b :start-line)))
      (should (= 1 (plist-get c :start-line))))))


;;;; --- Phase 2: prune skips synthetic ----------------------------------

(ert-deftest anvil-worklog-test/prune-skips-synthetic-paths ()
  "DB-direct rows must survive a global prune even though their `file'
sentinel does not exist on disk."
  (skip-unless (and (anvil-worklog-test--supported-p 'prune)
                    (anvil-worklog-test--supported-p 'add)))
  (anvil-worklog-test--with-env
    (anvil-worklog-add "DB row" "body" :date "2026-04-29"
                       :machine "linux-debian" :year 2026)
    (let ((before (length (anvil-worklog-list :limit 50)))
          (n (anvil-worklog-prune)))
      (should (= 1 before))
      (should (= 0 n)))
    (should (= 1 (length (anvil-worklog-list :limit 50))))))


;;;; --- Phase 2: export-org ----------------------------------------------

(ert-deftest anvil-worklog-test/export-org-roundtrip ()
  "export-org emits an org file whose entries match the DB rows."
  (skip-unless (and (anvil-worklog-test--supported-p 'export)
                    (anvil-worklog-test--supported-p 'add)))
  (anvil-worklog-test--with-env
    (anvil-worklog-add "First" "body 1\n" :date "2026-04-28"
                       :machine "test-m" :year 2026)
    (anvil-worklog-add "Second" "body 2\n" :date "2026-04-29"
                       :machine "test-m" :year 2026)
    (let* ((path (expand-file-name "out.org" root))
           (result (anvil-worklog-export-org
                    :machine "test-m" :year 2026 :path path))
           (content (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-substring-no-properties (point-min) (point-max)))))
      (should (= 2 (plist-get result :entries)))
      (should (string-match-p "\\*\\* NOTE 作業ログ <2026-04-28>" content))
      (should (string-match-p "\\*\\* NOTE 作業ログ <2026-04-29>" content))
      (should (string-match-p "\\*\\*\\* MEMO AI: First <2026-04-28>" content))
      (should (string-match-p "\\*\\*\\* MEMO AI: Second <2026-04-29>" content))
      (should (string-match-p "body 1" content))
      (should (string-match-p "body 2" content)))))

(ert-deftest anvil-worklog-test/export-org-merges-org-and-db-rows ()
  "Org-scanned and DB-direct rows for the same (machine, year) appear in
one export, ordered by date ASC."
  (skip-unless (and (anvil-worklog-test--supported-p 'export)
                    (anvil-worklog-test--supported-p 'add)
                    (anvil-worklog-test--supported-p 'scan)))
  (anvil-worklog-test--with-env
    ;; Seed an org-scanned row from a real ai-logs file (linux 2026).
    (anvil-worklog-test--seed root)
    (anvil-worklog-scan)
    ;; Add a DB-direct row for the same (machine, year).
    (anvil-worklog-add "Phase 2 entry" "from worklog-add"
                       :date "2026-04-29"
                       :machine "linux-debian"
                       :year 2026)
    (let* ((path (expand-file-name "merged.org" root))
           (result (anvil-worklog-export-org
                    :machine "linux-debian" :year 2026 :path path))
           (content (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-substring-no-properties (point-min) (point-max)))))
      (should (>= (plist-get result :entries) 4))
      (should (string-match-p "Phase 2 entry" content))
      (should (string-match-p "from worklog-add" content))
      (should (string-match-p "anvil-worklog 設計" content)))))


;;;; --- DB path resolver -------------------------------------------------

(ert-deftest anvil-worklog-test/effective-db-path-shared-root ()
  "shared-db-roots auto-detects an existing DB beneath ROOT."
  (skip-unless (and (fboundp 'anvil-worklog-effective-db-path)
                    (anvil-worklog-test--supported-p 'scan)))
  (let* ((root (make-temp-file "anvil-wlsh-" t))
         (cand-dir (expand-file-name ".anvil-worklog" root))
         (cand (expand-file-name "anvil-worklog-index.db" cand-dir))
         (anvil-worklog-db-path (anvil-worklog--default-db-path))
         (anvil-worklog-shared-db-roots (list root))
         (anvil-worklog--db nil)
         (anvil-worklog--resolved-db-path nil))
    (unwind-protect
        (progn
          (make-directory cand-dir t)
          (with-temp-file cand (insert ""))
          (should (equal cand (anvil-worklog-effective-db-path))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest anvil-worklog-test/effective-db-path-env-wins ()
  "ANVIL_WORKLOG_DB env var beats every other source."
  (skip-unless (fboundp 'anvil-worklog-effective-db-path))
  (let* ((env-path (make-temp-file "anvil-wl-env-" nil ".db"))
         (process-environment
          (cons (concat "ANVIL_WORKLOG_DB=" env-path) process-environment))
         (root (make-temp-file "anvil-wlsh-" t))
         (cand-dir (expand-file-name ".anvil-worklog" root))
         (cand (expand-file-name "anvil-worklog-index.db" cand-dir))
         (anvil-worklog-db-path "/tmp/explicit-anvil-wl.db")
         (anvil-worklog-shared-db-roots (list root))
         (anvil-worklog--db nil)
         (anvil-worklog--resolved-db-path nil))
    (unwind-protect
        (progn
          (make-directory cand-dir t)
          (with-temp-file cand (insert ""))
          (should (equal env-path (anvil-worklog-effective-db-path))))
      (ignore-errors (delete-file env-path))
      (ignore-errors (delete-directory root t)))))



;;;; --- Phase 2: get-by-digest (key-flexible read) ---------------------

(ert-deftest anvil-worklog-test/get-by-digest-roundtrip ()
  "anvil-worklog-get-by-digest looks up an entry by its sha1 digest."
  (skip-unless (and (anvil-worklog-test--supported-p 'add)
                    (anvil-worklog-test--supported-p 'get)))
  (anvil-worklog-test--with-env
    (let* ((id (anvil-worklog-add
                "Digest target"
                "**実施内容**\n- digest 経由で引きたい\n"
                :date "2026-04-29"
                :machine "linux-debian"
                :year 2026))
           (digest (plist-get id :digest))
           (entry (anvil-worklog-get-by-digest digest)))
      (should entry)
      (should (equal "Digest target" (plist-get entry :title)))
      (should (equal digest (plist-get entry :digest))))))

(ert-deftest anvil-worklog-test/get-by-digest-unknown-returns-nil ()
  "Unknown / blank digest returns nil rather than erroring."
  (skip-unless (anvil-worklog-test--supported-p 'get))
  (anvil-worklog-test--with-env
    (should-not (anvil-worklog-get-by-digest
                 "0000000000000000000000000000000000000000"))
    (should-not (anvil-worklog-get-by-digest ""))
    (should-not (anvil-worklog-get-by-digest nil))))


(provide 'anvil-worklog-test)

;;; anvil-worklog-test.el ends here
