;;; anvil-org-todo-keywords-test.el --- ERT for per-file TODO keywords -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO-state validation must use the keywords in effect in the
;; *target file* -- including in-buffer `#+TODO:' / `#+SEQ_TODO:' lines
;; and `#+SETUPFILE:' -- not only the global `org-todo-keywords'.
;; Before the fix, a file declaring `#+SEQ_TODO: WAITING | DONE' had
;; `WAITING' rejected by both `org-update-todo-state' and `org-add-todo'
;; whenever the global default was left at TODO/DONE.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil)
(require 'anvil-org)

(defmacro anvil-org-todo-keywords-test--with-org (content &rest body)
  "Run BODY with PATH bound to a temp org file holding CONTENT.
The global `org-todo-keywords' is pinned to Org's default."
  (declare (indent 1))
  `(let* ((path (make-temp-file "anvil-todo-kw-test" nil ".org" ,content))
          (anvil-org-allowed-files (list path))
          (anvil-org-allowed-files-enabled t)
          (org-todo-keywords '((sequence "TODO" "DONE"))))
     (unwind-protect
         (progn ,@body)
       (delete-file path))))

(defun anvil-org-todo-keywords-test--read (path)
  "Return PATH's contents."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defconst anvil-org-todo-keywords-test--file
  (concat "#+SEQ_TODO: TODO | DONE\n"
          "#+SEQ_TODO: WAITING | CANCELLED\n"
          "* TODO Task\n"
          ":PROPERTIES:\n:ID: 44444444-aaaa-bbbb-cccc-000000000001\n:END:\n"
          "* Parent\n"
          ":PROPERTIES:\n:ID: 44444444-aaaa-bbbb-cccc-000000000002\n:END:\n")
  "Org file whose extra keywords exist only in-buffer.")

(ert-deftest anvil-org-todo-keywords-test-update-accepts-in-buffer-keyword ()
  "`update-todo-state' accepts a keyword declared only in the file."
  (anvil-org-todo-keywords-test--with-org anvil-org-todo-keywords-test--file
    (anvil-org--tool-update-todo-state
     "org-id://44444444-aaaa-bbbb-cccc-000000000001" "TODO" "WAITING")
    (should (string-match-p "^\\* WAITING Task"
                            (anvil-org-todo-keywords-test--read path)))))

(ert-deftest anvil-org-todo-keywords-test-add-accepts-in-buffer-keyword ()
  "`add-todo' accepts a keyword declared only in the target file."
  (anvil-org-todo-keywords-test--with-org anvil-org-todo-keywords-test--file
    (anvil-org--tool-add-todo
     "Child" "WAITING" "org-id://44444444-aaaa-bbbb-cccc-000000000002")
    (should (string-match-p "^\\*\\* WAITING Child"
                            (anvil-org-todo-keywords-test--read path)))))

(ert-deftest anvil-org-todo-keywords-test-update-rejects-unknown ()
  "An undeclared keyword is still rejected, and the file is unchanged."
  (anvil-org-todo-keywords-test--with-org anvil-org-todo-keywords-test--file
    (should-error
     (anvil-org--tool-update-todo-state
      "org-id://44444444-aaaa-bbbb-cccc-000000000001" "TODO" "BOGUS"))
    (should (equal (anvil-org-todo-keywords-test--read path)
                   anvil-org-todo-keywords-test--file))))

(ert-deftest anvil-org-todo-keywords-test-add-rejects-unknown ()
  "`add-todo' rejects an undeclared keyword and leaves the file alone."
  (anvil-org-todo-keywords-test--with-org anvil-org-todo-keywords-test--file
    (should-error
     (anvil-org--tool-add-todo
      "Child" "BOGUS" "org-id://44444444-aaaa-bbbb-cccc-000000000002"))
    (should (equal (anvil-org-todo-keywords-test--read path)
                   anvil-org-todo-keywords-test--file))))

(ert-deftest anvil-org-todo-keywords-test-global-keywords-still-work ()
  "Files without in-buffer keywords keep using the global list."
  (anvil-org-todo-keywords-test--with-org
      "* TODO Task\n:PROPERTIES:\n:ID: 44444444-aaaa-bbbb-cccc-000000000003\n:END:\n"
    (anvil-org--tool-update-todo-state
     "org-id://44444444-aaaa-bbbb-cccc-000000000003" "TODO" "DONE")
    (should (string-match-p "^\\* DONE Task"
                            (anvil-org-todo-keywords-test--read path)))))

(provide 'anvil-org-todo-keywords-test)
;;; anvil-org-todo-keywords-test.el ends here
