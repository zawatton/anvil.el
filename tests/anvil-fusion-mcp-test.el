;;; anvil-fusion-mcp-test.el --- ERT for anvil-fusion MCP tool wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Doc 61 Phase 6d' (docs/design/61-fusion-verify.org §2 6d') gave the
;; `fusion-ask' MCP tool a `verify' boolean parameter.  This is the
;; first ERT file for anvil-fusion-mcp.el: prior fusion MCP wrappers had
;; no dedicated test file (`anvil-fusion-mcp.el' is daemon-only per its
;; own Commentary, and none of the peer `*-mcp.el' modules in this repo
;; have one either), but the new boolean-coercion + default-flag logic
;; is plain Elisp worth covering directly.
;;
;; `anvil-server' loads cleanly standalone (requires only cl-lib / json
;; / anvil-server-metrics, no daemon/network side effects at load time),
;; so these tests call the tool handler functions directly -- the same
;; "raw handler" `anvil-server-register-tool' would introspect for its
;; schema -- rather than round-tripping through the JSON-RPC transport.
;; `anvil-fusion-ask-async' is stubbed via `cl-letf' so no orchestrator
;; is needed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-fusion-mcp)

;;;; --- anvil-fusion--arg-bool (pure) -----------------------------------------

(ert-deftest anvil-fusion-mcp-test-arg-bool-omitted-uses-default ()
  "An omitted argument (nil, no explicit value sent) falls back to DEFAULT."
  (should (eq t (anvil-fusion--arg-bool nil t)))
  (should (eq nil (anvil-fusion--arg-bool nil nil))))

(ert-deftest anvil-fusion-mcp-test-arg-bool-explicit-false ()
  "Explicit JSON false (either wire encoding) normalizes to nil,
overriding a truthy DEFAULT."
  (should (eq nil (anvil-fusion--arg-bool :false t)))
  (should (eq nil (anvil-fusion--arg-bool "false" t))))

(ert-deftest anvil-fusion-mcp-test-arg-bool-explicit-true ()
  "Any other explicitly-provided value is truthy, overriding a nil DEFAULT."
  (should (eq t (anvil-fusion--arg-bool t nil)))
  (should (eq t (anvil-fusion--arg-bool "true" nil))))

;;;; --- fusion-ask tool: verify passthrough + default --------------------

(defmacro anvil-fusion-mcp-test--capture-ask-async (&rest body)
  "Run BODY with `anvil-fusion-ask-async' stubbed to capture its
keyword args into `captured-kwargs' and return a canned job plist."
  (declare (indent 0) (debug t))
  `(let (captured-kwargs)
     (cl-letf (((symbol-function 'anvil-fusion-ask-async)
                (lambda (_prompt &rest kwargs)
                  (setq captured-kwargs kwargs)
                  (list :job-id "fusion-b1" :stage 'members
                        :panel 'sovereign :egress 'local-only))))
       ,@body)))

(ert-deftest anvil-fusion-mcp-test-tool-ask-verify-true-passthrough ()
  "verify \"true\" (any client-supplied truthy value) reaches
`anvil-fusion-ask-async' as :verify t, regardless of the defcustom."
  (let ((anvil-fusion-verify-by-default nil))
    (anvil-fusion-mcp-test--capture-ask-async
      (anvil-fusion--tool-ask "Q" nil nil nil nil nil t)
      (should (eq t (plist-get captured-kwargs :verify))))))

(ert-deftest anvil-fusion-mcp-test-tool-ask-verify-false-passthrough ()
  "An explicit JSON false overrides a t defcustom default."
  (let ((anvil-fusion-verify-by-default t))
    (anvil-fusion-mcp-test--capture-ask-async
      (anvil-fusion--tool-ask "Q" nil nil nil nil nil :false)
      (should (eq nil (plist-get captured-kwargs :verify))))))

(ert-deftest anvil-fusion-mcp-test-tool-ask-verify-omitted-follows-defcustom-nil ()
  "Omitting verify follows `anvil-fusion-verify-by-default' -- nil case."
  (let ((anvil-fusion-verify-by-default nil))
    (anvil-fusion-mcp-test--capture-ask-async
      (anvil-fusion--tool-ask "Q")
      (should (eq nil (plist-get captured-kwargs :verify))))))

(ert-deftest anvil-fusion-mcp-test-tool-ask-verify-omitted-follows-defcustom-t ()
  "Omitting verify follows `anvil-fusion-verify-by-default' -- t case."
  (let ((anvil-fusion-verify-by-default t))
    (anvil-fusion-mcp-test--capture-ask-async
      (anvil-fusion--tool-ask "Q")
      (should (eq t (plist-get captured-kwargs :verify))))))

(ert-deftest anvil-fusion-mcp-test-verify-by-default-ships-nil ()
  "The mechanism ships conservative: default nil until the Doc 61
6e effect-measurement battery justifies flipping it (see the
defcustom's docstring in anvil-fusion-async.el)."
  (should (eq nil anvil-fusion-verify-by-default)))

(ert-deftest anvil-fusion-mcp-test-tool-ask-other-args-still-forwarded ()
  "Pre-existing args (panel/fidelity/lenses/judge/extra) keep reaching
`anvil-fusion-ask-async' unchanged alongside the new :verify."
  (anvil-fusion-mcp-test--capture-ask-async
    (anvil-fusion--tool-ask "Q" "quality" "full" "domain,correctness"
                            "claude" "120字以内。" t)
    (should (eq 'quality (plist-get captured-kwargs :panel)))
    (should (eq 'full (plist-get captured-kwargs :fidelity)))
    (should (equal '(domain correctness) (plist-get captured-kwargs :lenses)))
    (should (eq 'claude (plist-get captured-kwargs :judge)))
    (should (equal "120字以内。" (plist-get captured-kwargs :extra)))
    (should (eq t (plist-get captured-kwargs :verify)))))

(provide 'anvil-fusion-mcp-test)
;;; anvil-fusion-mcp-test.el ends here
