;;; anvil-server-schema-types-test.el --- ERT for docstring-driven schema types -*- lexical-binding: t; -*-

;;; Commentary:

;; `anvil-server--generate-schema-from-function' hard-coded every
;; parameter's JSON schema type to "string".  Clients then send
;; booleans as the strings "true"/"false" and arrays as ad-hoc
;; encodings, and every handler grows its own normalisation shims.
;; The `:schema' override exists but requires hand-writing the whole
;; schema for one mistyped parameter.
;;
;; These tests pin the new inference: a description in the docstring's
;; "MCP Parameters:" section may start with a "(TYPE) " prefix —
;; (boolean) (integer) (number) (array) (object) — which sets the
;; parameter's schema type and is stripped from the client-facing
;; description.  No prefix, or an unrecognised one, keeps "string".

;;; Code:

(require 'ert)
(require 'anvil)
(require 'anvil-server)

(defun anvil-server-schema-types-test--handler (name count enabled &optional tags)
  "Test handler.

MCP Parameters:
  name - plain string parameter
  count - (integer) how many items to return
  enabled - (boolean) whether the feature is on
  tags - (array) tags to attach"
  (list name count enabled tags))

(defun anvil-server-schema-types-test--unrecognised (value)
  "Test handler with an unrecognised prefix.

MCP Parameters:
  value - (frob) a description that keeps its odd prefix"
  value)

(defun anvil-server-schema-types-test--prop (schema name key)
  "Return KEY of parameter NAME in SCHEMA's properties."
  (alist-get key (cdr (assoc name (alist-get 'properties schema)))))

(ert-deftest anvil-server-schema-types-test-inferred-types ()
  "Recognised (TYPE) prefixes set the schema type and are stripped."
  (let ((schema (anvil-server--generate-schema-from-function
                 #'anvil-server-schema-types-test--handler)))
    (should (equal (anvil-server-schema-types-test--prop schema "name" 'type)
                   "string"))
    (should (equal (anvil-server-schema-types-test--prop schema "count" 'type)
                   "integer"))
    (should (equal (anvil-server-schema-types-test--prop schema "enabled" 'type)
                   "boolean"))
    (should (equal (anvil-server-schema-types-test--prop schema "tags" 'type)
                   "array"))
    ;; Prefix stripped from the client-facing description.
    (should (equal (anvil-server-schema-types-test--prop
                    schema "count" 'description)
                   "how many items to return"))
    (should (equal (anvil-server-schema-types-test--prop
                    schema "enabled" 'description)
                   "whether the feature is on"))
    ;; Required/optional split unchanged.
    (should (equal (alist-get 'required schema) ["name" "count" "enabled"]))))

(ert-deftest anvil-server-schema-types-test-unrecognised-prefix-kept ()
  "An unrecognised (foo) prefix keeps type string and full description."
  (let ((schema (anvil-server--generate-schema-from-function
                 #'anvil-server-schema-types-test--unrecognised)))
    (should (equal (anvil-server-schema-types-test--prop schema "value" 'type)
                   "string"))
    (should (equal (anvil-server-schema-types-test--prop
                    schema "value" 'description)
                   "(frob) a description that keeps its odd prefix"))))

(provide 'anvil-server-schema-types-test)
;;; anvil-server-schema-types-test.el ends here
