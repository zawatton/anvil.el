;;; anvil-nelisp-shims.el --- NeLisp standalone compatibility shims  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 51 — Pattern C bridge: lets standard anvil modules
;; (`anvil-memory', `anvil-worklog', `anvil-orchestrator', ...) run
;; under NeLisp standalone (= the Rust `anvil-runtime' binary spawned
;; by `bin/anvil mcp serve' without an Emacs subprocess).
;;
;; Two shim families:
;;
;;   1. SQLite API rename — anvil's modules call Emacs 29's built-in
;;      `sqlite-*' functions; NeLisp ships them as `nelisp-sqlite-*'
;;      via the `nelisp-sqlite-rs' extension crate.  Aliases below
;;      let the modules' code stay untouched.  (Phase 2: requires
;;      `defalias' / `fset' in the bootstrap evaluator — currently
;;      stubbed out as forwarding `defun's instead.)
;;
;;   2. anvil-server-register-tool[s] accumulator — under Emacs the
;;      real `anvil-server' module wires registered tools into the MCP
;;      transport.  Under NeLisp standalone the Rust runtime owns the
;;      MCP transport, so the shim just collects each module's spec
;;      list into `anvil-nelisp-shims--collected-specs'.  After all
;;      modules load, `AnvilModuleRegistry::collect' (Rust side)
;;      drains the accumulator via `anvil-nelisp-shims-drain' and
;;      registers the tools with the MCP framework.
;;
;; This file is loaded BEFORE any anvil module that calls these APIs.
;; All shim definitions use only bootstrap-evaluator primitives —
;; `defun', `defvar', `setq', `let', `let*', `if', `when', `unless',
;; `progn', `cons', `car', `cdr', `append', `fboundp', `boundp', `not',
;; `equal' — so the load completes even when the full NeLisp interpreter
;; coverage is incomplete.

;;; Code:

(defconst anvil-nelisp-shims-active-p (featurep 'nelisp)
  "Non-nil iff this shim layer is meaningful in the current runtime.
Captured at load time.  Informational only — the actual shim
definitions below do NOT gate on this constant; they use per-symbol
`unless (boundp ...)' / `unless (fboundp ...)' guards instead so
they remain inert under regular Emacs but can still install
unconditionally under NeLisp standalone, where bootstrap timing
makes `(featurep 'nelisp)' unreliable at shim-load time.")

(defvar anvil-nelisp-shims--collected-specs nil
  "Reverse-order alist of (SERVER-ID . SPEC-LIST) collected by the shim.
Populated by every `anvil-server-register-tool[s]' call during module
load.  Drained once by `anvil-nelisp-shims-drain' which the Rust
`AnvilModuleRegistry' invokes after the load sequence completes.")


;;;; --- Emacs builtin function polyfills -----------------------------------

;; Doc 51 Phase 1.5 — anvil.el provides polyfills for Emacs builtins that
;; the NeLisp interpreter's standard surface does not currently expose.
;; The user's directive (= 2026-05-02): NeLisp stays a MINIMAL Elisp
;; runtime.  Filling Emacs-compat gaps belongs in anvil.el, not in NeLisp.
;;
;; Each polyfill is gated on the Emacs name being unbound; under regular
;; Emacs the real implementation wins and the polyfill is skipped.  Each
;; polyfill is written using only bootstrap-eval primitives so the file
;; loads even when NeLisp's full interpreter coverage is incomplete.

;; `declare-function' — Emacs's bytecomp hint.  Emit a no-op macro so the
;; load-time form does not error.  Safe under both runtimes because the
;; macro's body is `nil' (= return value ignored).
(unless (fboundp 'declare-function)
  (defmacro declare-function (fn file &optional arglist fileonly)
    "Polyfill: no-op macro (NeLisp standalone has no byte compiler)."
    nil))

;; `getenv' — environment variable lookup.  NeLisp's bootstrap evaluator
;; lacks OS env access, and the full interpreter's `getenv' is not yet
;; reachable at module-load time.  Phase 1 polyfill returns nil so callers
;; fall through to their default branch (e.g. `anvil-memory-effective-db-path'
;; uses the default `~/.emacs.d/anvil-memory-index.db' path when
;; ANVIL_MEMORY_DB is unset).  Phase 2+ enhancement: bridge to NeLisp's
;; syscall extension via `nelisp-syscall-types-getenv' once it is loaded
;; in the standard module init sequence.
(unless (fboundp 'getenv)
  (defun getenv (variable &optional frame)
    "Polyfill: returns nil under NeLisp standalone (Phase 1 limitation)."
    nil))

;; `mapcar' — apply FUNCTION to each element of SEQUENCE, return list.
;; Implemented manually because the bootstrap evaluator does not provide
;; it.  Walks SEQUENCE forward, builds result in reverse via `cons', then
;; reverses (also manually because `nreverse' is not available).
(unless (fboundp 'mapcar)
  (defun mapcar (function sequence)
    "Polyfill: apply FUNCTION to each element of SEQUENCE."
    (let ((result nil)
          (cur sequence))
      (while cur
        (setq result (cons (funcall function (car cur)) result))
        (setq cur (cdr cur)))
      (let ((reversed nil))
        (while result
          (setq reversed (cons (car result) reversed))
          (setq result (cdr result)))
        reversed))))

;; `plist-get' — scan PLIST (alternating key/value list) for PROPERTY.
;; Returns the value cell or nil.  Uses `eq' for key comparison since
;; that is the Emacs `plist-get' default (keyword symbols are interned
;; so `eq' works correctly).
(unless (fboundp 'plist-get)
  (defun plist-get (plist property)
    "Polyfill: scan PLIST for PROPERTY using eq comparison."
    (let ((cur plist)
          (found nil)
          (result nil))
      (while (and cur (not found))
        (if (eq (car cur) property)
            (progn (setq result (car (cdr cur)))
                   (setq found t))
          (setq cur (cdr (cdr cur)))))
      result)))


;;;; --- Emacs builtin variable shims ---------------------------------------

;; Variables Emacs ships out of the box that anvil modules consume.
;; Hard-coded sensible defaults — Phase 2+ will dynamic-resolve via
;; `(getenv "HOME")' once that built-in lands in the bootstrap eval.

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Shim: NeLisp standalone fallback for Emacs' user-emacs-directory."))

(unless (boundp 'temporary-file-directory)
  (defvar temporary-file-directory "/tmp/"
    "Shim: NeLisp standalone fallback for Emacs' temporary-file-directory."))

(unless (boundp 'locale-coding-system)
  (defvar locale-coding-system 'utf-8
    "Shim: NeLisp standalone forces utf-8."))

(unless (boundp 'system-type)
  (defvar system-type 'gnu/linux
    "Shim: NeLisp standalone defaults to gnu/linux."))


;;;; --- SQLite API forwarding ---------------------------------------------

;; `defalias' is not in the bootstrap evaluator; we instead define
;; forwarding `defun's that take any args and pass them through via
;; `apply'.  Each forwarder is gated on the Emacs name being unbound —
;; the regular Emacs path provides the real `sqlite-*' so the forwarders
;; stay no-ops there.

(unless (fboundp 'sqlite-open)
  (defun sqlite-open (path)
    "Shim: forward to `nelisp-sqlite-open'."
    (nelisp-sqlite-open path)))

(unless (fboundp 'sqlite-close)
  (defun sqlite-close (db)
    "Shim: forward to `nelisp-sqlite-close'."
    (nelisp-sqlite-close db)))

(unless (fboundp 'sqlite-execute)
  (defun sqlite-execute (db query &optional values)
    "Shim: forward to `nelisp-sqlite-execute'."
    (nelisp-sqlite-execute db query values)))

(unless (fboundp 'sqlite-select)
  (defun sqlite-select (db query &optional values return-type)
    "Shim: forward to `nelisp-sqlite-select'."
    (nelisp-sqlite-select db query values return-type)))

(unless (fboundp 'sqlitep)
  (defun sqlitep (object)
    "Shim: forward to `nelisp-sqlitep'."
    (nelisp-sqlitep object)))

(unless (fboundp 'sqlite-pragma)
  (defun sqlite-pragma (db pragma-clause)
    "Shim: forward to `nelisp-sqlite-pragma'."
    (nelisp-sqlite-pragma db pragma-clause)))

(unless (fboundp 'sqlite-transaction)
  (defun sqlite-transaction (db)
    "Shim: forward to `nelisp-sqlite-transaction'."
    (nelisp-sqlite-transaction db)))

(unless (fboundp 'sqlite-commit)
  (defun sqlite-commit (db)
    "Shim: forward to `nelisp-sqlite-commit'."
    (nelisp-sqlite-commit db)))

(unless (fboundp 'sqlite-rollback)
  (defun sqlite-rollback (db)
    "Shim: forward to `nelisp-sqlite-rollback'."
    (nelisp-sqlite-rollback db)))

(unless (fboundp 'sqlite-available-p)
  (defun sqlite-available-p ()
    "Shim: NeLisp SQLite extension presence check."
    (if (fboundp 'nelisp-sqlite-available-p)
        (nelisp-sqlite-available-p)
      nil)))

(unless (fboundp 'sqlite-supports-trigram-p)
  (defun sqlite-supports-trigram-p (&optional db)
    "Shim: NeLisp does not yet expose a trigram-presence probe.
Returns nil so anvil-memory falls back to unicode61 / porter."
    nil))


;;;; --- anvil-server registration accumulator ------------------------------

(unless (fboundp 'anvil-server-register-tools)
  (defun anvil-server-register-tools (server-id specs)
    "Standalone shim — accumulate SPECS for SERVER-ID.
Returns SPECS unchanged so callers that chain on the return value
behave the same as under the real `anvil-server'."
    (setq anvil-nelisp-shims--collected-specs
          (cons (cons server-id specs)
                anvil-nelisp-shims--collected-specs))
    specs))

(unless (fboundp 'anvil-server-unregister-tools)
  (defun anvil-server-unregister-tools (server-id specs)
    "Standalone shim — no-op; the Rust runtime owns the MCP transport."
    nil))

(unless (fboundp 'anvil-server-register-tool)
  (defun anvil-server-register-tool (handler &rest properties)
    "Standalone shim — synthesize a 1-element spec from HANDLER and
PROPERTIES, then route through `anvil-server-register-tools'.
Uses manual plist scan because the bootstrap evaluator does not
provide `plist-get'."
    (let ((id nil)
          (server-id "default")
          (description nil)
          (title nil)
          (read-only nil)
          (rest properties))
      (while rest
        (let ((key (car rest))
              (val (car (cdr rest))))
          (if (equal key :id)            (setq id val)
            (if (equal key :server-id)   (setq server-id val)
              (if (equal key :description) (setq description val)
                (if (equal key :title)     (setq title val)
                  (if (equal key :read-only) (setq read-only val)))))))
        (setq rest (cdr (cdr rest))))
      (anvil-server-register-tools
       server-id
       (cons (list :id id
                   :handler handler
                   :description description
                   :title title
                   :read-only read-only)
             nil)))))

(unless (fboundp 'anvil-server-unregister-tool)
  (defun anvil-server-unregister-tool (&rest args) nil))

;; Provide the feature so modules' `(require 'anvil-server)' is satisfied.
(unless (featurep 'anvil-server)
  (provide 'anvil-server))


;;;; --- drain helper --------------------------------------------------------

(defun anvil-nelisp-shims-drain ()
  "Return all collected tool specs as a flat list and reset the accumulator.
Specs are returned in registration order (= forward, oldest first).
Implementation uses only bootstrap-eval primitives (no `nreverse' /
`mapcar')."
  ;; Step 1: reverse the accumulator (newest-first → oldest-first) into
  ;; `forward' using a manual cons loop.
  (let ((forward nil)
        (cur anvil-nelisp-shims--collected-specs))
    (while cur
      (setq forward (cons (car cur) forward))
      (setq cur (cdr cur)))
    ;; Step 2: walk `forward' and `append' each cell's spec-list.
    (let ((result nil)
          (rcur forward))
      (while rcur
        (setq result (append result (cdr (car rcur))))
        (setq rcur (cdr rcur)))
      (setq anvil-nelisp-shims--collected-specs nil)
      result)))

(defun anvil-nelisp-shims-collected-count ()
  "Return the total number of tool specs currently accumulated."
  (let ((total 0)
        (cur anvil-nelisp-shims--collected-specs))
    (while cur
      ;; Add length of cdr of (server-id . spec-list).
      (let ((specs (cdr (car cur))))
        (while specs
          (setq total (+ total 1))
          (setq specs (cdr specs))))
      (setq cur (cdr cur)))
    total))


(provide 'anvil-nelisp-shims)

;;; anvil-nelisp-shims.el ends here
