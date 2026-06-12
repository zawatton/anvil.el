;;; anvil-org-read-by-id-depth-test.el --- ERT for bounded read-by-id -*- lexical-binding: t; -*-

;;; Commentary:

;; `org-read-by-id' used to dump the entire subtree, which wastes
;; context on large headings.  The bounded variant caps the response
;; at `max_depth' heading levels and always appends a "Child
;; headlines" footer naming the immediate children, so callers know
;; which subtrees they could descend into next.
;;
;; Two backing paths must agree on output shape: the SQL-indexed fast
;; path (`anvil-org-index-read-by-id-with-children') and the
;; regex-truncating fallback over the full-subtree text
;; (`anvil-org--bounded-subtree-from-text').  Both are pinned here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil)
(require 'anvil-org)
(require 'anvil-org-index)

(defconst anvil-org-read-by-id-depth-test--uuid
  "aaaaaaaa-1111-2222-3333-444444444444")

(defconst anvil-org-read-by-id-depth-test--content
  (concat "* Root\n"
          ":PROPERTIES:\n"
          ":ID: " anvil-org-read-by-id-depth-test--uuid "\n"
          ":END:\n"
          "Root body.\n"
          "** Child One :tagged:\n"
          "Child one body.\n"
          "*** Grandchild\n"
          "Deep body.\n"
          "** Child Two\n"
          "* Sibling\n"))

;;;; Unit: max_depth parsing

(ert-deftest anvil-org-read-by-id-depth-test-parse-max-depth ()
  "String depths parse; zero/negative mean full subtree (nil)."
  (should (= 1 (anvil-org--parse-max-depth nil 1)))
  (should (= 1 (anvil-org--parse-max-depth "" 1)))
  (should (= 2 (anvil-org--parse-max-depth "2" 1)))
  (should (= 3 (anvil-org--parse-max-depth 3 1)))
  (should-not (anvil-org--parse-max-depth "0" 1))
  (should-not (anvil-org--parse-max-depth "-1" 1)))

;;;; Unit: text fallback truncation

(ert-deftest anvil-org-read-by-id-depth-test-bounded-text-depth-1 ()
  "Depth 1 keeps the heading and body only; children listed, tags stripped."
  (let* ((bc (anvil-org--bounded-subtree-from-text
              anvil-org-read-by-id-depth-test--content 1))
         (bounded (car bc))
         (children (cdr bc)))
    (should (string-match-p "Root body" bounded))
    (should-not (string-match-p "Child one body" bounded))
    (should-not (string-match-p "Grandchild" bounded))
    (should (equal '("Child One" "Child Two") children))))

(ert-deftest anvil-org-read-by-id-depth-test-bounded-text-depth-2 ()
  "Depth 2 includes children but not grandchildren."
  (let* ((bc (anvil-org--bounded-subtree-from-text
              anvil-org-read-by-id-depth-test--content 2))
         (bounded (car bc)))
    (should (string-match-p "Child one body" bounded))
    (should-not (string-match-p "Grandchild" bounded))))

(ert-deftest anvil-org-read-by-id-depth-test-bounded-text-full ()
  "nil depth returns the input untouched."
  (let ((bc (anvil-org--bounded-subtree-from-text
             anvil-org-read-by-id-depth-test--content nil)))
    (should (equal (car bc) anvil-org-read-by-id-depth-test--content))))

;;;; Unit: footer rendering

(ert-deftest anvil-org-read-by-id-depth-test-footer ()
  "Footer names count, depth, and truncation; (none) for leaves."
  (should (string-match-p
           "------ Child headlines (2), max_depth=1 (subtrees truncated) ------"
           (anvil-org--format-read-by-id-result "body" '("A" "B") 1 t)))
  (should (string-match-p
           "------ Child headlines (0) ------\n(none)"
           (anvil-org--format-read-by-id-result "body" nil nil nil))))

;;;; Index fast path

(defun anvil-org-read-by-id-depth-test--have-sqlite ()
  (and (fboundp 'sqlite-available-p) (sqlite-available-p)))

(defmacro anvil-org-read-by-id-depth-test--with-index (&rest body)
  "Run BODY with a fresh index over the test content."
  (declare (indent 0))
  `(let* ((tmpdir (make-temp-file "anvil-rbid-test-" t))
          (orgfile (expand-file-name "sample.org" tmpdir))
          (dbfile (expand-file-name "test-index.db" tmpdir))
          (anvil-org-index-db-path dbfile)
          (anvil-org-index-paths (list tmpdir))
          (anvil-org-index--backend nil)
          (anvil-org-index--db nil))
     (unwind-protect
         (progn
           (let ((coding-system-for-write 'utf-8-unix))
             (write-region anvil-org-read-by-id-depth-test--content
                           nil orgfile nil 'silent))
           (anvil-org-index-enable)
           (anvil-org-index-rebuild)
           ,@body)
       (ignore-errors (anvil-org-index-disable))
       (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest anvil-org-read-by-id-depth-test-index-with-children ()
  "Index path: bounded body, ordered children, truncated flag."
  (skip-unless (anvil-org-read-by-id-depth-test--have-sqlite))
  (anvil-org-read-by-id-depth-test--with-index
    (let ((p1 (anvil-org-index-read-by-id-with-children
               anvil-org-read-by-id-depth-test--uuid 1)))
      (should (string-match-p "Root body" (plist-get p1 :body)))
      (should-not (string-match-p "Child one body" (plist-get p1 :body)))
      (should (equal '("Child One" "Child Two") (plist-get p1 :children)))
      (should (plist-get p1 :truncated)))
    (let ((p2 (anvil-org-index-read-by-id-with-children
               anvil-org-read-by-id-depth-test--uuid 2)))
      (should (string-match-p "Child one body" (plist-get p2 :body)))
      (should-not (string-match-p "Deep body" (plist-get p2 :body)))
      (should (plist-get p2 :truncated)))
    (let ((pf (anvil-org-index-read-by-id-with-children
               anvil-org-read-by-id-depth-test--uuid nil)))
      (should (string-match-p "Deep body" (plist-get pf :body)))
      (should-not (plist-get pf :truncated)))))

(ert-deftest anvil-org-read-by-id-depth-test-index-read-by-id-depth ()
  "anvil-org-index-read-by-id honours its new max-depth argument."
  (skip-unless (anvil-org-read-by-id-depth-test--have-sqlite))
  (anvil-org-read-by-id-depth-test--with-index
    (let ((bounded (anvil-org-index-read-by-id
                    anvil-org-read-by-id-depth-test--uuid 1))
          (full (anvil-org-index-read-by-id
                 anvil-org-read-by-id-depth-test--uuid)))
      (should-not (string-match-p "Child one body" bounded))
      (should (string-match-p "Deep body" full)))))

;;;; Tool integration via the text fallback

(ert-deftest anvil-org-read-by-id-depth-test-tool-fallback ()
  "Without an index, the tool truncates via text and appends the footer."
  (let* ((path (make-temp-file "anvil-rbid-tool-" nil ".org"
                               anvil-org-read-by-id-depth-test--content))
         (anvil-org-allowed-files (list path))
         (anvil-org-allowed-files-enabled t))
    (unwind-protect
        (cl-letf (((symbol-function 'anvil-org--index-available-p)
                   (lambda () nil)))
          (let ((default-result (anvil-org--tool-read-by-id
                                 anvil-org-read-by-id-depth-test--uuid)))
            ;; default depth 1: body only + footer with both children
            (should (string-match-p "Root body" default-result))
            (should-not (string-match-p "Child one body" default-result))
            (should (string-match-p
                     "------ Child headlines (2), max_depth=1 (subtrees truncated) ------"
                     default-result))
            (should (string-match-p "^- Child One$" default-result))
            (should (string-match-p "^- Child Two$" default-result)))
          (let ((full-result (anvil-org--tool-read-by-id
                              anvil-org-read-by-id-depth-test--uuid "0")))
            (should (string-match-p "Deep body" full-result))
            (should (string-match-p "------ Child headlines (2) ------"
                                    full-result)))))
      (delete-file path)))

(provide 'anvil-org-read-by-id-depth-test)
;;; anvil-org-read-by-id-depth-test.el ends here
