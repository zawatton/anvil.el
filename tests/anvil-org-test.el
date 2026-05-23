;;; anvil-org-test.el --- Tests for anvil-org -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused contract tests for org-mode MCP tool wrappers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil)
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

(defun anvil-org-test--read (path)
  "Return PATH's contents as a UTF-8 string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (buffer-string)))

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

(ert-deftest anvil-org-test-read-by-id-scans-allowed-files ()
  "Read an ID property from allowed files when org-id has no DB entry."
  (let ((id (concat "mcp-test-root-id-"
                    (substring (md5 (number-to-string (float-time))) 0 8))))
    (anvil-org-test--with-temp-org
     (format "* TODO Root\n:PROPERTIES:\n:ID: %s\n:END:\nBody line.\n" id)
     (lambda (path)
       (let ((anvil-org-allowed-files (list path))
             (anvil-org-allowed-files-enabled t)
             (anvil-org-use-index nil))
         (should-not (org-id-find-id-file id))
         (let ((content (anvil-org--tool-read-by-id id)))
           (should (string-match-p "\\* TODO Root" content))
           (should (string-match-p "Body line." content))))))))

(ert-deftest anvil-org-test-edit-body-returns-target-headline-id ()
  "Editing parent body must return the parent URI, not a child URI."
  (anvil-org-test--with-temp-org
   "* TODO Root :test:\n:PROPERTIES:\n:ID: root-id-for-edit-body-test\n:END:\nBody line.\n** Child\nChild body.\n"
   (lambda (path)
     (let* ((anvil-org-allowed-files (list path))
            (anvil-org-allowed-files-enabled t)
            (root-uri (concat "org-headline://" path "#Root"))
            (response
             (json-parse-string
              (anvil-org--tool-edit-body
               root-uri "Body line." "Body line edited." nil)
              :object-type 'alist))
            (content (anvil-org-test--read path)))
       (should
        (equal "org-id://root-id-for-edit-body-test"
               (alist-get 'uri response)))
       (should (string-match-p "Body line edited." content))
       (should-not (string-match-p "^\\*\\* Child\n:PROPERTIES:" content))))))

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

(ert-deftest anvil-org-test-update-todo-state-no-state-transition ()
  "Empty `current_state' must match a headline that has no TODO keyword."
  (anvil-org-test--with-temp-org
   "* Plain :test:\nBody.\n"
   (lambda (path)
     (let* ((anvil-org-allowed-files (list path))
            (anvil-org-allowed-files-enabled t)
            (uri (concat "org-headline://" path "#Plain"))
            (response
             (json-parse-string
              (anvil-org--tool-update-todo-state uri "" "TODO")
              :object-type 'alist))
            (content (anvil-org-test--read path)))
       (should (eq t (alist-get 'success response)))
       (should (string-match-p "^\\* TODO Plain" content))))))

(ert-deftest anvil-org-test-update-todo-state-errors-when-blocked ()
  "Blocked transitions (org-enforce-todo-dependencies) must surface as errors.

A parent with an unfinished child TODO cannot be marked DONE while
`org-enforce-todo-dependencies' is non-nil.  `org-todo' silently
no-ops in that case; the tool must detect the no-op and throw."
  (anvil-org-test--with-temp-org
   "* TODO Parent\n** TODO Child\n"
   (lambda (path)
     (let* ((anvil-org-allowed-files (list path))
            (anvil-org-allowed-files-enabled t)
            (org-enforce-todo-dependencies t)
            (uri (concat "org-headline://" path "#Parent"))
            (err (should-error
                  (anvil-org--tool-update-todo-state uri "TODO" "DONE")
                  :type 'error))
            (content (anvil-org-test--read path)))
       ;; Error message names the blocked transition.
       (should (string-match-p "blocked" (error-message-string err)))
       (should (string-match-p "DONE" (error-message-string err)))
       ;; Disk must still show the parent as TODO, not DONE.
       (should (string-match-p "^\\* TODO Parent" content))
       (should-not (string-match-p "^\\* DONE Parent" content))))))


(ert-deftest anvil-org-test-add-todo-empty-tags-string ()
  "Empty `tags' string must mean no tags, not a single empty tag."
  (anvil-org-test--with-temp-org
   "* Parent\nBody.\n"
   (lambda (path)
     (let* ((anvil-org-allowed-files (list path))
            (anvil-org-allowed-files-enabled t)
            (parent-uri (concat "org-headline://" path "#Parent"))
            (response
             (json-parse-string
              (anvil-org--tool-add-todo
               "Child" "TODO" "" "Child body." parent-uri)
              :object-type 'alist))
            (content (anvil-org-test--read path)))
       (should (eq t (alist-get 'success response)))
       (should (string-match-p "^\\*\\* TODO Child$" content))
       (should-not (string-match-p "^\\*\\* TODO Child[ \t]+:" content))))))

(ert-deftest anvil-org-test-normalize-tags-json-array-string ()
  "MCP callers must be able to send multiple tags as a JSON array string."
  (should (equal '("work" "urgent")
                 (anvil-org--normalize-tags-to-list
                  "[\"work\",\"urgent\"]")))
  (should (equal '("solo")
                 (anvil-org--normalize-tags-to-list "[\"solo\"]")))
  (should-not (anvil-org--normalize-tags-to-list "[]"))
  (should-not (anvil-org--normalize-tags-to-list "  []  ")))

(ert-deftest anvil-org-test-normalize-tags-json-malformed ()
  "Malformed JSON array strings must surface a validation error."
  (should-error (anvil-org--normalize-tags-to-list "[unterminated")
                :type 'anvil-server-tool-error)
  (should-error (anvil-org--normalize-tags-to-list "[\"a\",\"b\"")
                :type 'anvil-server-tool-error))

(ert-deftest anvil-org-test-normalize-tags-single-string-preserved ()
  "Single-tag plain strings keep wrapping behavior unchanged."
  (should (equal '("urgent")
                 (anvil-org--normalize-tags-to-list "urgent")))
  (should-not (anvil-org--normalize-tags-to-list ""))
  (should-not (anvil-org--normalize-tags-to-list "   ")))

(ert-deftest anvil-org-test-add-todo-json-array-tags ()
  "Add-todo accepts a JSON array literal as the tags argument."
  (anvil-org-test--with-temp-org
   "* Parent\nBody.\n"
   (lambda (path)
     (let* ((anvil-org-allowed-files (list path))
            (anvil-org-allowed-files-enabled t)
            (parent-uri (concat "org-headline://" path "#Parent"))
            (response
             (json-parse-string
              (anvil-org--tool-add-todo
               "Child" "TODO" "[\"work\",\"urgent\"]"
               "Child body." parent-uri)
              :object-type 'alist))
            (content (anvil-org-test--read path)))
       (should (eq t (alist-get 'success response)))
       (should (string-match-p
                "^\\*\\* TODO Child[ \t]+:work:urgent:$"
                content))))))

;;; Buffer-first modify tests

(ert-deftest anvil-org-test-buffer-first-viable-returns-nil-by-default ()
  "`anvil--buffer-first-viable-p' returns nil when the mode list is nil."
  (anvil-org-test--with-temp-org "* Test\nBody."
    (lambda (path)
      (find-file-noselect path)
      (unwind-protect
          (should-not (anvil--buffer-first-viable-p path))
        (kill-buffer (find-buffer-visiting path))))))

(ert-deftest anvil-org-test-buffer-first-viable-returns-cons-when-matching ()
  "`anvil--buffer-first-viable-p' returns a cons when the mode matches."
  (let ((anvil-modes-allow-buffer-modify '(org-mode)))
    (anvil-org-test--with-temp-org "* Test\nBody."
      (lambda (path)
        (find-file-noselect path)
        (unwind-protect
            (let ((result (anvil--buffer-first-viable-p path)))
              (should result)
              (should (bufferp (car result)))
              (should-not (cdr result)))
          (kill-buffer (find-buffer-visiting path)))))))

(ert-deftest anvil-org-test-buffer-first-viable-detects-modified ()
  "`anvil--buffer-first-viable-p' correctly reports modified state."
  (let ((anvil-modes-allow-buffer-modify '(org-mode)))
    (anvil-org-test--with-temp-org "* Test\nBody."
      (lambda (path)
        (let ((buf (find-file-noselect path)))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (insert "unsaved change")
                  (let ((result (anvil--buffer-first-viable-p path)))
                    (should result)
                    (should (eq buf (car result)))
                    (should (cdr result))))
                (with-current-buffer buf
                  (set-buffer-modified-p nil)))
            (kill-buffer buf)))))))

(ert-deftest anvil-org-test-buffer-first-preserves-unsaved-modifications ()
  "Buffer-first editing does NOT save when the buffer was already modified."
  (let ((anvil-modes-allow-buffer-modify '(org-mode)))
    (anvil-org-test--with-temp-org
     "* Before\nOriginal content.\n"
     (lambda (path)
       (let ((buf (find-file-noselect path)))
         (unwind-protect
             (let ((anvil-org-allowed-files (list path))
                   (anvil-org-allowed-files-enabled t))
               (with-current-buffer buf
                 (goto-char (point-max))
                 (insert "User's unsaved work."))
               (anvil-org--modify path "update"
                   '((previous_state . "") (new_state . "DONE"))
                 (goto-char (point-min))
                 (org-todo "DONE"))
               (with-current-buffer buf
                 (should (buffer-modified-p))
                 (goto-char (point-min))
                 (should (search-forward "User's unsaved work" nil t))
                 (goto-char (point-min))
                 (should (search-forward "DONE" nil t)))
               (let ((disk-content (anvil-org-test--read path)))
                 (should-not (string-match-p "DONE" disk-content))
                 (should (string-match-p "Before" disk-content))))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (set-buffer-modified-p nil))
             (kill-buffer buf))))))))

(ert-deftest anvil-org-test-buffer-first-saves-when-clean ()
  "Buffer-first editing DOES save when the buffer was clean."
  (let ((anvil-modes-allow-buffer-modify '(org-mode)))
    (anvil-org-test--with-temp-org
     "* Before\nOriginal content.\n"
     (lambda (path)
       (let ((buf (find-file-noselect path)))
         (unwind-protect
             (let ((anvil-org-allowed-files (list path))
                   (anvil-org-allowed-files-enabled t))
               (anvil-org--modify path "update"
                   '((previous_state . "") (new_state . "DONE"))
                 (goto-char (point-min))
                 (org-todo "DONE"))
               (with-current-buffer buf
                 (should-not (buffer-modified-p)))
               (let ((disk-content (anvil-org-test--read path)))
                 (should (string-match-p "DONE" disk-content))
                 (should (string-match-p "Before" disk-content))))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (set-buffer-modified-p nil))
             (kill-buffer buf))))))))

(ert-deftest anvil-org-test-fundamental-mode-matches-everything ()
  "`fundamental-mode' in the mode list matches org-mode buffers."
  (let ((anvil-modes-allow-buffer-modify '(fundamental-mode)))
    (anvil-org-test--with-temp-org "* Test\nBody."
      (lambda (path)
        (find-file-noselect path)
        (unwind-protect
            (let ((result (anvil--buffer-first-viable-p path)))
              (should result)
              (should (bufferp (car result))))
          (kill-buffer (find-buffer-visiting path)))))))

;;; anvil-org-test.el ends here
