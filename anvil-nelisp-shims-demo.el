;;; anvil-nelisp-shims-demo.el --- Minimal Pattern C round-trip demo  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Phase 1 — synthetic module proving the shim accumulator + Rust
;; AnvilModuleRegistry dispatch works end-to-end without depending on
;; NeLisp-interpreter coverage of `getenv' / `expand-file-name' /
;; `featurep' (= currently-missing builtins that block real modules like
;; `anvil-state' / `anvil-memory' from loading via Pattern C).
;;
;; Loaded after `anvil-nelisp-shims.el' so the stub
;; `anvil-server-register-tools' is in place.  Registers a single
;; zero-arg tool `nelisp-shims-demo-ping' that returns the constant
;; string "pong".  An MCP `tools/list' must include it; an MCP
;; `tools/call' must execute the handler and return the string.

;;; Code:

(defun anvil-nelisp-shims-demo--ping ()
  "Return the constant string \"pong\" — Phase 1 round-trip proof."
  "pong")

(anvil-server-register-tools
 "default"
 (list (list :id "nelisp-shims-demo-ping"
             :handler 'anvil-nelisp-shims-demo--ping
             :description "Phase 1 PoC — returns the constant \"pong\"."
             :title "Demo Ping"
             :read-only t)))

(provide 'anvil-nelisp-shims-demo)

;;; anvil-nelisp-shims-demo.el ends here
