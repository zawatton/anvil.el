;;; anvil-server-dispatch-guards-test.el --- ERT for dispatch guards -*- lexical-binding: t; -*-

;;; Commentary:

;; Two opt-in guards for `anvil-server-process-jsonrpc', both nil by
;; default (historical behaviour preserved):
;;
;; * `anvil-server-autostart-on-request' — restart a stopped server
;;   instead of signalling.  For stdio transports the signalled error
;;   surfaces as empty emacsclient output, which the client cannot
;;   match to its request: an effective hang.
;;
;; * `anvil-server-dispatch-timeout' — answer an over-long request
;;   with a JSON-RPC internal error carrying the request id instead
;;   of blocking the transport indefinitely.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil)
(require 'anvil-server)

(ert-deftest anvil-server-dispatch-guards-test-stopped-server-errors ()
  "Default: a stopped server still signals."
  (let ((anvil-server--running nil)
        (anvil-server-autostart-on-request nil))
    (should-error
     (anvil-server-process-jsonrpc
      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"
      "guards-test"))))

(ert-deftest anvil-server-dispatch-guards-test-autostart ()
  "With autostart enabled, a stopped server is started transparently."
  (let ((anvil-server--running nil)
        (anvil-server-autostart-on-request t)
        (started nil))
    (cl-letf (((symbol-function 'anvil-server-start)
               (lambda (&rest _) (setq started t anvil-server--running t)))
              ((symbol-function 'anvil-server--validate-and-dispatch-request)
               (lambda (&rest _) "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}")))
      (anvil-server-process-jsonrpc
       "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"
       "guards-test")
      (should started))))

(ert-deftest anvil-server-dispatch-guards-test-timeout-carries-id ()
  "An over-long dispatch yields a timeout error with the request id."
  (let ((anvil-server--running t)
        (anvil-server-dispatch-timeout 0.05))
    (cl-letf (((symbol-function 'anvil-server--validate-and-dispatch-request)
               (lambda (&rest _)
                 ;; sit-for yields to the event loop so with-timeout fires.
                 (sit-for 1)
                 "{\"jsonrpc\":\"2.0\",\"id\":7,\"result\":{}}")))
      (let ((resp (json-read-from-string
                   (anvil-server-process-jsonrpc
                    "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/list\"}"
                    "guards-test"))))
        (should (equal (alist-get 'id resp) 7))
        (should (string-match-p "timed out"
                                (alist-get 'message
                                           (alist-get 'error resp))))))))

(ert-deftest anvil-server-dispatch-guards-test-no-timeout-by-default ()
  "With the default nil timeout, dispatch is not wrapped."
  (let ((anvil-server--running t)
        (anvil-server-dispatch-timeout nil))
    (cl-letf (((symbol-function 'anvil-server--validate-and-dispatch-request)
               (lambda (&rest _)
                 "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{}}")))
      (let ((resp (json-read-from-string
                   (anvil-server-process-jsonrpc
                    "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/list\"}"
                    "guards-test"))))
        (should (equal (alist-get 'id resp) 3))
        ;; json-read maps the empty result object to nil; presence of
        ;; the key (and absence of error) is what matters.
        (should (assq 'result resp))
        (should-not (assq 'error resp))))))

(provide 'anvil-server-dispatch-guards-test)
;;; anvil-server-dispatch-guards-test.el ends here
