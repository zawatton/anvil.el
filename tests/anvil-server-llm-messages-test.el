;;; anvil-server-llm-messages-test.el --- ERT for LLM-facing error messages -*- lexical-binding: t; -*-

;;; Commentary:

;; Tool errors are read by LLM clients, which act on what the message
;; tells them.  Two messages are made actionable:
;;
;; * Missing required parameter: LLM clients sometimes emit XML-style
;;   tool-call syntax (</param>, <parameter name="...">) embedded
;;   inside another parameter's JSON string value; the result parses
;;   as JSON with a missing field and a corrupted sibling value.  The
;;   bare "Missing required parameter: x" leads the model to resend
;;   the same confabulated shape; naming the failure pattern breaks
;;   the loop.
;;
;; * Quit: "Quit: (quit)" does not tell the model what happened or
;;   what to change.  Saying the user interrupted with C-g (and to
;;   avoid UI-blocking code) does.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil)
(require 'anvil-server)
(require 'anvil-server-metrics)

(defun anvil-server-llm-messages-test--tool (alpha beta)
  "Test tool.

MCP Parameters:
  alpha - first value
  beta - second value"
  (concat alpha beta))

(ert-deftest anvil-server-llm-messages-test-missing-param-names-pattern ()
  "The missing-parameter error names the XML-embedding failure mode."
  (anvil-server-register-tool
   #'anvil-server-llm-messages-test--tool
   :id "llm-messages-test-tool"
   :description "Test tool"
   :server-id "llm-messages-test")
  (unwind-protect
      (let* ((resp (anvil-server--handle-tools-call
                    9 '((name . "llm-messages-test-tool")
                        (arguments . ((alpha . "a"))))
                    (make-anvil-server-metrics)
                    "llm-messages-test"))
             (parsed (json-read-from-string resp))
             (msg (alist-get 'message (alist-get 'error parsed))))
        (should (string-match-p "Missing required parameter: beta" msg))
        (should (string-match-p "do not embed" msg))
        (should (string-match-p "<parameter name=\"beta\">" msg)))
    (anvil-server-unregister-tool "llm-messages-test-tool"
                                  "llm-messages-test")))

(ert-deftest anvil-server-llm-messages-test-quit-message-actionable ()
  "A quit inside a tool handler reports the C-g interruption."
  (let ((err (should-error
              (anvil-server-with-error-handling
                (signal 'quit nil))
              :type 'anvil-server-tool-error)))
    (should (string-match-p "C-g" (cadr err)))
    (should (string-match-p "Interrupted" (cadr err)))))

(provide 'anvil-server-llm-messages-test)
;;; anvil-server-llm-messages-test.el ends here
