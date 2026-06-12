;;; anvil-org-edit-body-optional-test.el --- ERT for optional replace_all -*- lexical-binding: t; -*-

;;; Commentary:

;; `anvil-org--tool-edit-body' declared `replace_all' as a required
;; positional parameter even though its docstring calls it optional,
;; so the generated MCP schema forced clients to send a value on every
;; call.  These tests pin the `&optional' arglist: the schema's
;; required set excludes `replace_all', and a call omitting it behaves
;; like `replace_all' false (single, unique replacement).

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil)
(require 'anvil-org)

(ert-deftest anvil-org-edit-body-optional-test-schema-required-set ()
  "replace_all is not in the schema's required parameters."
  (let ((schema (anvil-server--generate-schema-from-function
                 #'anvil-org--tool-edit-body)))
    (should (equal (alist-get 'required schema)
                   ["resource_uri" "old_body" "new_body"]))))

(ert-deftest anvil-org-edit-body-optional-test-omitted-defaults-to-single ()
  "Omitting replace_all performs a single unique replacement."
  (let* ((path (make-temp-file "anvil-edit-body-test" nil ".org"
                               "* Heading\nalpha beta gamma\n"))
         (anvil-org-allowed-files (list path))
         (anvil-org-allowed-files-enabled t))
    (unwind-protect
        (let ((response
               (json-parse-string
                (anvil-org--tool-edit-body
                 (concat "org-headline://" path "#Heading")
                 "beta" "BETA")
                :object-type 'alist)))
          (should (eq t (alist-get 'success response)))
          (with-temp-buffer
            (insert-file-contents path)
            (should (string-match-p "alpha BETA gamma" (buffer-string)))))
      (delete-file path))))

(provide 'anvil-org-edit-body-optional-test)
;;; anvil-org-edit-body-optional-test.el ends here
