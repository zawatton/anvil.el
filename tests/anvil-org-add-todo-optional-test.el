;;; anvil-org-add-todo-optional-test.el --- ERT for optional add-todo args -*- lexical-binding: t; -*-

;;; Commentary:

;; Three pinned behaviors for `anvil-org--tool-add-todo':
;;
;; 1. `tags' and `body' are optional in the generated MCP schema.
;;    Previously they sat before `&optional' in the arglist, so the
;;    schema marked them required and clients had to send empty
;;    placeholder values on every call.
;;
;; 2. Body spacing: exactly one blank line separates the generated
;;    :PROPERTIES: drawer from the body, and one blank line follows,
;;    instead of the body abutting the :END: line.
;;
;; 3. Point discipline: when the parent's subtree is not at the end of
;;    the file, the body insert used to leave point at the beginning
;;    of the *next* heading; `org-back-to-heading' stays put there and
;;    the completion step's `org-id-get-create' returned (or minted)
;;    the neighbouring heading's ID while the new task got no ID
;;    drawer at all.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil)
(require 'anvil-org)

(defmacro anvil-org-add-todo-optional-test--with-org (content &rest body)
  "Run BODY with PATH bound to a temp org file holding CONTENT."
  (declare (indent 1))
  `(let* ((path (make-temp-file "anvil-add-todo-test" nil ".org" ,content))
          (anvil-org-allowed-files (list path))
          (anvil-org-allowed-files-enabled t))
     (unwind-protect
         (progn ,@body)
       (delete-file path))))

(defun anvil-org-add-todo-optional-test--read (path)
  "Return PATH's contents."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(ert-deftest anvil-org-add-todo-optional-test-schema-required-set ()
  "Only title, todo_state and parent_uri are required in the schema."
  (let ((schema (anvil-server--generate-schema-from-function
                 #'anvil-org--tool-add-todo)))
    (should (equal (alist-get 'required schema)
                   ["title" "todo_state" "parent_uri"]))))

(ert-deftest anvil-org-add-todo-optional-test-omitted-tags-and-body ()
  "A call without tags and body succeeds and creates a clean headline."
  (anvil-org-add-todo-optional-test--with-org "* Parent\nText.\n"
    (let* ((response
            (json-parse-string
             (anvil-org--tool-add-todo
              "Child" "TODO" (concat "org-headline://" path "#Parent"))
             :object-type 'alist))
           (content (anvil-org-add-todo-optional-test--read path)))
      (should (eq t (alist-get 'success response)))
      (should (string-match-p "^\\*\\* TODO Child$" content)))))

(ert-deftest anvil-org-add-todo-optional-test-body-spacing ()
  "One blank line separates the :END: drawer line from the body."
  (anvil-org-add-todo-optional-test--with-org "* Parent\nText.\n"
    (anvil-org--tool-add-todo
     "Child" "TODO" (concat "org-headline://" path "#Parent")
     "First line.\nSecond line.")
    (let ((content (anvil-org-add-todo-optional-test--read path)))
      (should (string-match-p ":END:\n\nFirst line\\.\nSecond line\\.\n"
                              content))
      (should-not (string-match-p ":END:\nFirst line\\." content)))))

(ert-deftest anvil-org-add-todo-optional-test-id-on-new-heading ()
  "With a following sibling heading, the new task gets its own fresh ID."
  (anvil-org-add-todo-optional-test--with-org
      "* Parent\n:PROPERTIES:\n:ID:       11111111-aaaa-bbbb-cccc-000000000001\n:END:\nText.\n* Next section\n:PROPERTIES:\n:ID:       22222222-aaaa-bbbb-cccc-000000000002\n:END:\nOther text.\n"
    (let* ((response
            (json-parse-string
             (anvil-org--tool-add-todo
              "Child" "TODO"
              "org-id://11111111-aaaa-bbbb-cccc-000000000001"
              "Body line.")
             :object-type 'alist))
           (uri (alist-get 'uri response))
           (content (anvil-org-add-todo-optional-test--read path)))
      (should (eq t (alist-get 'success response)))
      ;; The returned URI is neither the parent's nor the sibling's ID.
      (should-not (string-match-p "11111111-aaaa-bbbb-cccc-000000000001" uri))
      (should-not (string-match-p "22222222-aaaa-bbbb-cccc-000000000002" uri))
      ;; The new heading carries its own ID drawer.
      (should (string-match-p
               "\\*\\* TODO Child\n:PROPERTIES:\n:ID:" content)))))

(ert-deftest anvil-org-add-todo-optional-test-position-first-prepends ()
  "POSITION=\"first\" places the new heading before existing children."
  (anvil-org-add-todo-optional-test--with-org
      "* Parent\n:PROPERTIES:\n:ID: 33333333-aaaa-bbbb-cccc-000000000001\n:END:\n** Existing child\n"
    (anvil-org--tool-add-todo
     "New first" "TODO"
     "org-id://33333333-aaaa-bbbb-cccc-000000000001"
     nil nil nil "first")
    (should (string-match-p
             "\\*\\* TODO New first[^\n]*\n[^*]*\\*\\* Existing child"
             (anvil-org-add-todo-optional-test--read path)))))

(ert-deftest anvil-org-add-todo-optional-test-position-first-rejects-after-uri ()
  "POSITION=\"first\" combined with after_uri is a validation error."
  (anvil-org-add-todo-optional-test--with-org
      "* Parent\n:PROPERTIES:\n:ID: 33333333-aaaa-bbbb-cccc-000000000002\n:END:\n** Sibling\n:PROPERTIES:\n:ID: 33333333-aaaa-bbbb-cccc-000000000003\n:END:\n"
    (should-error
     (anvil-org--tool-add-todo
      "New" "TODO"
      "org-id://33333333-aaaa-bbbb-cccc-000000000002"
      nil
      "org-id://33333333-aaaa-bbbb-cccc-000000000003"
      nil "first"))))

(ert-deftest anvil-org-add-todo-optional-test-position-rejects-unknown ()
  "Unknown POSITION values are rejected instead of silently appending."
  (anvil-org-add-todo-optional-test--with-org
      "* Parent\n:PROPERTIES:\n:ID: 33333333-aaaa-bbbb-cccc-000000000005\n:END:\n"
    (should-error
     (anvil-org--tool-add-todo
      "New" "TODO"
      "org-id://33333333-aaaa-bbbb-cccc-000000000005"
      nil nil nil "frist"))))

(ert-deftest anvil-org-add-todo-optional-test-position-omitted-defaults-to-last ()
  "Omitting POSITION preserves the append-as-last-child default."
  (anvil-org-add-todo-optional-test--with-org
      "* Parent\n:PROPERTIES:\n:ID: 33333333-aaaa-bbbb-cccc-000000000004\n:END:\n** First\n** Middle\n"
    (anvil-org--tool-add-todo
     "Newcomer" "TODO"
     "org-id://33333333-aaaa-bbbb-cccc-000000000004")
    (should (string-match-p
             "\\*\\* Middle\n[^*]*\\*\\* TODO Newcomer"
             (anvil-org-add-todo-optional-test--read path)))))

(provide 'anvil-org-add-todo-optional-test)
;;; anvil-org-add-todo-optional-test.el ends here
