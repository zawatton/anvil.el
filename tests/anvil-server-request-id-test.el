;;; anvil-server-request-id-test.el --- ERT for error response request id -*- lexical-binding: t; -*-

;;; Commentary:

;; JSON-RPC 2.0 requires error responses to echo the id of the request
;; they answer.  When request dispatch signals (an uncaught `error' or
;; `quit' inside `anvil-server--validate-and-dispatch-request'), the
;; fallback `anvil-server--handle-error' used to hard-code `id: null';
;; an MCP client cannot match `id: null' against its pending numeric id
;; and waits forever.
;;
;; These tests pin the fix: `anvil-server-process-jsonrpc' binds
;; `anvil-server--current-request-id' from the parsed request before
;; dispatching, and `anvil-server--handle-error' echoes it.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil)
(require 'anvil-server)

(defun anvil-server-request-id-test--dispatch-error (request)
  "Process REQUEST with dispatch forced to signal; return parsed response."
  (let ((anvil-server--running t))
    (cl-letf (((symbol-function 'anvil-server--validate-and-dispatch-request)
               (lambda (&rest _) (error "boom"))))
      (json-read-from-string
       (anvil-server-process-jsonrpc request "request-id-test")))))

(ert-deftest anvil-server-request-id-test-error-echoes-numeric-id ()
  "A dispatch error response carries the numeric request id."
  (let ((resp (anvil-server-request-id-test--dispatch-error
               "{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"tools/list\"}")))
    (should (equal (alist-get 'id resp) 42))
    (should (alist-get 'error resp))))

(ert-deftest anvil-server-request-id-test-error-echoes-string-id ()
  "A dispatch error response carries a string request id unchanged."
  (let ((resp (anvil-server-request-id-test--dispatch-error
               "{\"jsonrpc\":\"2.0\",\"id\":\"req-7\",\"method\":\"tools/list\"}")))
    (should (equal (alist-get 'id resp) "req-7"))
    (should (alist-get 'error resp))))

(ert-deftest anvil-server-request-id-test-handle-error-defaults-nil ()
  "Outside dispatch, `anvil-server--handle-error' still emits id null."
  (let* ((anvil-server--current-request-id nil)
         (resp (json-read-from-string
                (anvil-server--handle-error (list 'error "boom")))))
    ;; json.el reads JSON null as nil by default.
    (should (null (alist-get 'id resp)))
    (should (string-match-p "\"id\":null"
                            (anvil-server--handle-error (list 'error "boom"))))))

(provide 'anvil-server-request-id-test)
;;; anvil-server-request-id-test.el ends here
