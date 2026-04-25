;;; anvil-server-cons-fix-test.el --- ERT for return-shape contract widen -*- lexical-binding: t; -*-

;;; Commentary:

;; T118 = T97-FOLLOWUP-1.
;;
;; `anvil-server' historically required tool handlers to return a
;; string or nil.  Handlers that returned plists / lists / hash-tables
;; (e.g. `anvil-defs--tool-index-status') tripped the
;; `anvil-server-invalid-params' guard, which surfaced as a WARN in the
;; T97 M1 / T110 M2 boot smokes.
;;
;; This file pins the widened contract:
;;   string / nil          → pass through (legacy)
;;   plist / list / cons   → JSON-encode at the boundary
;;   hash-table / vector   → JSON-encode at the boundary
;;   symbol / function /
;;   buffer / process /
;;   marker                → reject (cannot be wire-encoded)
;;
;; Also re-exercises the legacy string-return path so existing handlers
;; remain regression-free.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil)
(require 'anvil-server)
(require 'anvil-server-metrics)

;;;; --- helper: invoke tools/call and decode -----------------------------

(defun anvil-server-cons-fix-test--call (tool-id &optional args)
  "Dispatch TOOL-ID via `tools/call' on server-id `anvil-test'.
ARGS is an alist of `(symbol . value)' pairs forwarded as JSON arguments.
Returns the decoded JSON-RPC response (alist)."
  (let* ((params `((name . ,tool-id)
                   (arguments . ,(or args '()))))
         (resp (anvil-server--handle-tools-call
                "t-cons-fix" params
                (make-anvil-server-metrics) "anvil-test")))
    (json-read-from-string resp)))

(defun anvil-server-cons-fix-test--text-of (decoded)
  "Return the first text-content string from DECODED tools/call response."
  (let* ((result (alist-get 'result decoded))
         (content (alist-get 'content result)))
    (alist-get 'text (aref content 0))))

(defun anvil-server-cons-fix-test--register (tool-id handler)
  "Register HANDLER as TOOL-ID on the `anvil-test' server-id."
  (anvil-server-register-tool
   handler
   :id tool-id
   :description "T118 cons-fix test handler"
   :server-id "anvil-test"))

;;;; --- positive cases: widened return shapes ---------------------------

(ert-deftest anvil-server-handler-accepts-plist-return ()
  "A handler returning a plist must succeed and JSON-encode at the boundary."
  (defun anvil-server-cons-fix-test--plist-tool ()
    "Plist-returning tool.

MCP Parameters:
  (none)"
    (list :status "ok" :files 12 :defs 34))
  (let ((tool-id "anvil-server-cons-fix-plist"))
    (unwind-protect
        (progn
          (anvil-server-cons-fix-test--register
           tool-id #'anvil-server-cons-fix-test--plist-tool)
          (let* ((decoded (anvil-server-cons-fix-test--call tool-id))
                 (result (alist-get 'result decoded))
                 (text (anvil-server-cons-fix-test--text-of decoded)))
            (should result)
            (should (eq :json-false (alist-get 'isError result)))
            (should (stringp text))
            (let ((parsed (json-parse-string text :object-type 'alist)))
              (should (equal "ok" (alist-get 'status parsed)))
              (should (equal 12 (alist-get 'files parsed)))
              (should (equal 34 (alist-get 'defs parsed))))))
      (anvil-server-unregister-tool tool-id "anvil-test"))))

(ert-deftest anvil-server-handler-accepts-list-return ()
  "A handler returning a plain list must encode as a JSON array."
  (defun anvil-server-cons-fix-test--list-tool ()
    "Plain-list-returning tool.

MCP Parameters:
  (none)"
    '("a" "b" "c"))
  (let ((tool-id "anvil-server-cons-fix-list"))
    (unwind-protect
        (progn
          (anvil-server-cons-fix-test--register
           tool-id #'anvil-server-cons-fix-test--list-tool)
          (let* ((decoded (anvil-server-cons-fix-test--call tool-id))
                 (text (anvil-server-cons-fix-test--text-of decoded)))
            (should (stringp text))
            (let ((parsed (json-parse-string text :array-type 'list)))
              (should (equal '("a" "b" "c") parsed)))))
      (anvil-server-unregister-tool tool-id "anvil-test"))))

(ert-deftest anvil-server-handler-accepts-hash-table-return ()
  "A handler returning a hash-table must encode as a JSON object."
  (defun anvil-server-cons-fix-test--ht-tool ()
    "Hash-table-returning tool.

MCP Parameters:
  (none)"
    (let ((h (make-hash-table :test 'equal)))
      (puthash "alpha" 1 h)
      (puthash "beta" 2 h)
      h))
  (let ((tool-id "anvil-server-cons-fix-ht"))
    (unwind-protect
        (progn
          (anvil-server-cons-fix-test--register
           tool-id #'anvil-server-cons-fix-test--ht-tool)
          (let* ((decoded (anvil-server-cons-fix-test--call tool-id))
                 (text (anvil-server-cons-fix-test--text-of decoded)))
            (should (stringp text))
            (let ((parsed (json-parse-string text :object-type 'hash-table)))
              (should (= 1 (gethash "alpha" parsed)))
              (should (= 2 (gethash "beta" parsed))))))
      (anvil-server-unregister-tool tool-id "anvil-test"))))

(ert-deftest anvil-server-handler-accepts-vector-return ()
  "A handler returning a vector must encode as a JSON array."
  (defun anvil-server-cons-fix-test--vec-tool ()
    "Vector-returning tool.

MCP Parameters:
  (none)"
    [1 2 3])
  (let ((tool-id "anvil-server-cons-fix-vec"))
    (unwind-protect
        (progn
          (anvil-server-cons-fix-test--register
           tool-id #'anvil-server-cons-fix-test--vec-tool)
          (let* ((decoded (anvil-server-cons-fix-test--call tool-id))
                 (text (anvil-server-cons-fix-test--text-of decoded)))
            (should (stringp text))
            (let ((parsed (json-parse-string text :array-type 'list)))
              (should (equal '(1 2 3) parsed)))))
      (anvil-server-unregister-tool tool-id "anvil-test"))))

;;;; --- negative case: still reject opaque types ------------------------

(ert-deftest anvil-server-handler-still-rejects-other-types ()
  "Non-serialisable returns (symbol / function / buffer) still reject."
  (defun anvil-server-cons-fix-test--bad-tool ()
    "Bad: returns a function value.

MCP Parameters:
  (none)"
    #'identity)
  (let ((tool-id "anvil-server-cons-fix-bad"))
    (unwind-protect
        (progn
          (anvil-server-cons-fix-test--register
           tool-id #'anvil-server-cons-fix-test--bad-tool)
          (let* ((decoded (anvil-server-cons-fix-test--call tool-id))
                 (err (alist-get 'error decoded))
                 (msg (alist-get 'message err)))
            ;; Must be a JSON-RPC error, not a successful result.
            (should err)
            (should (null (alist-get 'result decoded)))
            (should (stringp msg))
            (should (string-match-p "JSON-serialisable" msg))))
      (anvil-server-unregister-tool tool-id "anvil-test"))))

;;;; --- regression: legacy string / nil returns unchanged ---------------

(ert-deftest anvil-server-existing-string-handlers-regression-free ()
  "String- and nil-returning handlers must keep their legacy wire shape."
  (defun anvil-server-cons-fix-test--string-tool ()
    "String tool.

MCP Parameters:
  (none)"
    "ok-string")
  (defun anvil-server-cons-fix-test--nil-tool ()
    "Nil tool.

MCP Parameters:
  (none)"
    nil)
  (let ((s-id "anvil-server-cons-fix-string")
        (n-id "anvil-server-cons-fix-nil"))
    (unwind-protect
        (progn
          (anvil-server-cons-fix-test--register
           s-id #'anvil-server-cons-fix-test--string-tool)
          (anvil-server-cons-fix-test--register
           n-id #'anvil-server-cons-fix-test--nil-tool)
          (let* ((d-s (anvil-server-cons-fix-test--call s-id))
                 (t-s (anvil-server-cons-fix-test--text-of d-s)))
            (should (equal "ok-string" t-s))
            (should (eq :json-false
                        (alist-get 'isError (alist-get 'result d-s)))))
          (let* ((d-n (anvil-server-cons-fix-test--call n-id))
                 (t-n (anvil-server-cons-fix-test--text-of d-n)))
            (should (equal "" t-n))
            (should (eq :json-false
                        (alist-get 'isError (alist-get 'result d-n))))))
      (anvil-server-unregister-tool s-id "anvil-test")
      (anvil-server-unregister-tool n-id "anvil-test"))))

;;;; --- integration: defs-style plist tool round-trip -------------------

(ert-deftest anvil-server-defs-style-plist-roundtrip ()
  "Mimic `anvil-defs--tool-index-status' shape end-to-end.

This is the original boot-smoke WARN trigger: a tool handler returning
`(:db-path PATH :files N :defs M ...)' from `anvil-server-with-error-handling'.
Pre-T118 this hit the \"must return string or nil\" reject."
  (defun anvil-server-cons-fix-test--defs-style ()
    "Defs-style tool returning a plist.

MCP Parameters:
  (none)"
    (anvil-server-with-error-handling
     (list :db-path "/tmp/x.db"
           :files 17
           :defs 42
           :refs 7
           :features 3
           :schema-version 5)))
  (let ((tool-id "anvil-server-cons-fix-defs-style"))
    (unwind-protect
        (progn
          (anvil-server-cons-fix-test--register
           tool-id #'anvil-server-cons-fix-test--defs-style)
          (let* ((decoded (anvil-server-cons-fix-test--call tool-id))
                 (result (alist-get 'result decoded))
                 (text (anvil-server-cons-fix-test--text-of decoded)))
            (should result)
            (should (eq :json-false (alist-get 'isError result)))
            (let ((parsed (json-parse-string text :object-type 'alist)))
              (should (equal "/tmp/x.db" (alist-get 'db-path parsed)))
              (should (= 17 (alist-get 'files parsed)))
              (should (= 42 (alist-get 'defs parsed)))
              (should (= 5 (alist-get 'schema-version parsed))))))
      (anvil-server-unregister-tool tool-id "anvil-test"))))

(provide 'anvil-server-cons-fix-test)

;;; anvil-server-cons-fix-test.el ends here
