;;; anvil-fusion-traj-test.el --- ERT for verified trajectory store -*- lexical-binding: t; -*-

;;; Commentary:
;; Phase 9 tests for the verified-trajectory exemplar library.  SQLite
;; mode uses a fresh temp DB per test; fallback mode forces
;; `sqlite-available-p' to nil and exercises the in-memory backend with
;; the same public API.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-fusion-traj)

(defun anvil-fusion-traj-test--sqlite-mode-supported-p ()
  "Return non-nil when this Emacs can run the SQLite-mode tests."
  (and (fboundp 'sqlite-available-p)
       (sqlite-available-p)
       (condition-case nil
           (let* ((tmp (make-temp-file "anvil-fusion-traj-probe-" nil ".db"))
                  (db (sqlite-open tmp)))
             (unwind-protect
                 (progn
                   (sqlite-execute
                    db
                    "CREATE VIRTUAL TABLE probe USING fts5(x, tokenize=trigram)")
                   t)
               (ignore-errors (sqlite-close db))
               (ignore-errors (delete-file tmp))))
         (error nil))))

(defun anvil-fusion-traj-test--reset-state ()
  "Reset trajectory module process state between tests."
  (when (and anvil-fusion-traj--default-db
             (not (eq anvil-fusion-traj--default-db 'memory)))
    (ignore-errors (sqlite-close anvil-fusion-traj--default-db)))
  (setq anvil-fusion-traj--default-db nil
        anvil-fusion-traj--resolved-db-path nil
        anvil-fusion-traj--memory-store nil
        anvil-fusion-traj--memory-next-id 1))

(defmacro anvil-fusion-traj-test--with-backend (backend &rest body)
  "Run BODY with trajectory BACKEND bound to a fresh store."
  (declare (indent 1) (debug t))
  `(let ((anvil-fusion-traj-db-path
          (make-temp-file "anvil-fusion-traj-test-" nil ".db")))
     (unwind-protect
         (progn
           (anvil-fusion-traj-test--reset-state)
           ,(if (eq backend 'memory)
                `(cl-letf (((symbol-function 'sqlite-available-p)
                            (lambda () nil)))
                   ,@body)
              `(progn
                 (skip-unless (anvil-fusion-traj-test--sqlite-mode-supported-p))
                 ,@body)))
       (anvil-fusion-traj-test--reset-state)
       (ignore-errors (delete-file anvil-fusion-traj-db-path)))))

(defun anvil-fusion-traj-test--clean-result ()
  "Return a passing result plist."
  (list :answer "DGR を使う。"
        :claims (list (list :claim "DGR を使う"
                            :verdict 'confirmed
                            :evidence "kb:1"))
        :panel 'sovereign
        :rounds 0
        :exec-results (list (list :name "m1" :status 'pass))))

(defun anvil-fusion-traj-test--search-fixtures ()
  "Store three exemplar rows and return their ids."
  (list
   (anvil-fusion-traj-store
    "地絡保護の継電器は何を使うべきか"
    (list :answer "地絡保護は DGR を使う。"
          :claims (list (list :claim "DGR"
                              :verdict 'confirmed
                              :evidence "kb:1"))
          :panel 'quality))
   (anvil-fusion-traj-store
    "受電設備の停電作業手順を簡潔に述べよ"
    (list :answer "停電作業は手順書と検電を前提に進める。"
          :claims (list (list :claim "停電作業"
                              :verdict 'confirmed
                              :evidence "kb:2"))
          :panel 'quality))
   (anvil-fusion-traj-store
    "高圧ケーブル端末の点検項目を挙げよ"
    (list :answer "端末処理の劣化と発熱痕を確認する。"
          :claims (list (list :claim "端末処理"
                              :verdict 'confirmed
                              :evidence "kb:3"))
          :panel 'quality))))

(ert-deftest anvil-fusion-traj-test-admit-p ()
  (should-not (anvil-fusion-traj-admit-p (list :answer "A" :claims nil)))
  (should-not (anvil-fusion-traj-admit-p
               (list :answer "A"
                     :claims (list (list :claim "x" :verdict 'unverified)))))
  (should-not (anvil-fusion-traj-admit-p
               (list :answer "A"
                     :claims (list (list :claim "x" :verdict 'confirmed)
                                   (list :claim "y" :verdict 'refuted)))))
  (should-not (anvil-fusion-traj-admit-p
               (list :answer "   "
                     :claims (list (list :claim "x" :verdict 'confirmed)))))
  (should (anvil-fusion-traj-admit-p (anvil-fusion-traj-test--clean-result))))

(ert-deftest anvil-fusion-traj-test-store-get-roundtrip-sqlite ()
  (anvil-fusion-traj-test--with-backend sqlite
    (let* ((result (list :answer "地絡保護は DGR を使う。"
                         :claims (list (list :claim "DGR を使う"
                                             :verdict 'confirmed
                                             :evidence "kb:1")
                                       (list :claim "OCGR は不可"
                                             :verdict 'unverified
                                             :evidence "kb:2"))
                         :panel 'sovereign
                         :rounds 1
                         :exec-results (list (list :name "m1" :status 'pass))))
           (id (anvil-fusion-traj-store "地絡保護の継電器は？" result
                                        :tags '("verify" "dgr")))
           (row (anvil-fusion-traj-get id)))
      (should (integerp id))
      (should (equal "地絡保護の継電器は？" (plist-get row :question)))
      (should (equal (plist-get result :answer) (plist-get row :answer)))
      (should (equal (plist-get result :claims) (plist-get row :claims)))
      (should (equal "sovereign" (plist-get row :panel)))
      (should (equal '("verify" "dgr") (plist-get row :tags)))
      (should (equal (list :rounds 1
                           :exec-results (list (list :name "m1" :status 'pass)))
                     (plist-get row :meta))))))

(ert-deftest anvil-fusion-traj-test-store-get-roundtrip-fallback ()
  (anvil-fusion-traj-test--with-backend memory
    (let* ((id (anvil-fusion-traj-store "Q" (anvil-fusion-traj-test--clean-result)
                                        :tags '("x")))
           (row (anvil-fusion-traj-get id)))
      (should (= 1 id))
      (should (equal "Q" (plist-get row :question)))
      (should (equal '("x") (plist-get row :tags)))
      (should (equal (plist-get (anvil-fusion-traj-test--clean-result) :claims)
                     (plist-get row :claims))))))

(ert-deftest anvil-fusion-traj-test-store-rejects-and-force-overrides ()
  (dolist (backend '(sqlite memory))
    (anvil-fusion-traj-test--with-backend backend
      (let ((bad (list :answer "仮回答"
                       :claims (list (list :claim "x" :verdict 'refuted)))))
        (should-not (anvil-fusion-traj-store "Q-bad" bad))
        (should (= 0 (anvil-fusion-traj-count)))
        (should (integerp (anvil-fusion-traj-store "Q-force" bad :force t)))
        (should (= 1 (anvil-fusion-traj-count)))))))

(ert-deftest anvil-fusion-traj-test-search-sqlite ()
  (anvil-fusion-traj-test--with-backend sqlite
    (let ((ids (anvil-fusion-traj-test--search-fixtures)))
      (should (= 3 (length ids)))
      (let ((hits (anvil-fusion-traj-search "地絡保護 継電器" :k 2)))
        (should (<= (length hits) 2))
        (should hits)
        (should (= (car ids) (plist-get (car hits) :id)))
        (should (string-match-p "DGR" (plist-get (car hits) :answer-head))))
      (should-not (anvil-fusion-traj-search "" :k 2))
      (should-not (anvil-fusion-traj-search nil :k 2)))))

(ert-deftest anvil-fusion-traj-test-search-fallback ()
  (anvil-fusion-traj-test--with-backend memory
    (let ((ids (anvil-fusion-traj-test--search-fixtures)))
      (let ((hits (anvil-fusion-traj-search "地絡保護 継電器" :k 1)))
        (should (= 1 (length hits)))
        (should (= (car ids) (plist-get (car hits) :id))))
      (should-not (anvil-fusion-traj-search "" :k 1)))))

(ert-deftest anvil-fusion-traj-test-exemplar-block ()
  (dolist (backend '(sqlite memory))
    (anvil-fusion-traj-test--with-backend backend
      (anvil-fusion-traj-store
       "地絡保護の継電器は何を使うべきか"
       (list :answer "地絡保護は DGR を使う。"
             :claims (list (list :claim "DGR"
                                 :verdict 'confirmed
                                 :evidence "kb:1"))))
      (anvil-fusion-traj-store
       "地絡保護で OCGR を採用してよいか"
       (list :answer "OCGR はこの条件では不可。"
             :claims (list (list :claim "OCGR"
                                 :verdict 'confirmed
                                 :evidence "kb:2"))))
      (let ((block (anvil-fusion-traj-exemplar-block
                    "地絡保護 継電器" :k 2 :max-chars 120)))
        (should (string-match-p "過去の検証済み事例" block))
        (should (string-match-p "地絡保護は DGR を使う。" block))
        (should-not (string-match-p "OCGR はこの条件では不可。" block))))
    (anvil-fusion-traj-test--with-backend backend
      (should-not (anvil-fusion-traj-exemplar-block "地絡保護" :k 2 :max-chars 120)))))

(ert-deftest anvil-fusion-traj-test-count-delete ()
  (dolist (backend '(sqlite memory))
    (anvil-fusion-traj-test--with-backend backend
      (let ((id1 (anvil-fusion-traj-store "Q1" (anvil-fusion-traj-test--clean-result)))
            (id2 (anvil-fusion-traj-store
                  "Q2"
                  (list :answer "別解"
                        :claims (list (list :claim "別解"
                                            :verdict 'confirmed
                                            :evidence "kb:2"))))))
        (should (= 2 (anvil-fusion-traj-count)))
        (should (anvil-fusion-traj-delete id1))
        (should (= 1 (anvil-fusion-traj-count)))
        (should-not (anvil-fusion-traj-get id1))
        (should (anvil-fusion-traj-get id2))))))

(provide 'anvil-fusion-traj-test)
;;; anvil-fusion-traj-test.el ends here
