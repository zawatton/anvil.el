;;; anvil-org-test.el --- Tests for anvil-org -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused contract tests for org-mode MCP tool wrappers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-org)

(defun anvil-org-test--with-temp-org (content fn)
  "Write CONTENT to a temporary Org file and call FN with its path."
  (let ((path (make-temp-file "anvil-org-test-" nil ".org")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region content nil path nil 'silent))
          (funcall fn path))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest anvil-org-test-modify-errors-do-not-prompt-on-kill ()
  "Failed modify tools must not ask to kill their temp file buffer."
  (anvil-org-test--with-temp-org
   "* TODO Root :test:\nBody line.\n** Child\nChild body.\n"
   (lambda (path)
     (let ((anvil-org-allowed-files (list path))
           (anvil-org-allowed-files-enabled t)
           (root-uri (concat "org-headline://" path "#Root"))
           (child-uri (concat "org-headline://" path "#Root/Child")))
       (dolist (case
                `((update
                   . ,(lambda ()
                        (anvil-org--tool-update-todo-state
                         child-uri "TODO" "DONE")))
                  (rename
                   . ,(lambda ()
                        (anvil-org--tool-rename-headline
                         root-uri "Wrong Root" "Root Renamed")))
                  (edit-body
                   . ,(lambda ()
                        (anvil-org--tool-edit-body
                         root-uri "missing body" "replacement" nil)))))
         (let (prompts)
           (cl-letf (((symbol-function 'read-multiple-choice)
                      (lambda (prompt _choices &rest _args)
                        (push prompt prompts)
                        (list ?y "yes"))))
             (should-error (funcall (cdr case))
                           :type 'anvil-server-tool-error))
           (should-not prompts)))
       (should-not
        (cl-some
         (lambda (buf)
           (with-current-buffer buf
             (and buffer-file-name
                  (string= (file-truename buffer-file-name)
                           (file-truename path)))))
         (buffer-list)))))))

(ert-deftest anvil-org-test-tool-read-by-id-rejects-org-id-resource-uri ()
  "The Layer-3 tool accepts org:// citations, not org-id:// resources."
  (let ((err
         (should-error
          (anvil-org--tool-read-by-id
           "org-id://550e8400-e29b-41d4-a716-446655440000")
          :type 'anvil-server-tool-error)))
    (should
     (string-match-p
      "Parameter uuid does not accept org-id:// resource URIs"
      (cadr err)))
    (should
     (string-match-p
      "Use the raw UUID"
      (cadr err)))
    (should
     (string-match-p
      "org://550e8400-e29b-41d4-a716-446655440000"
      (cadr err)))))

;;; anvil-org-test.el ends here
