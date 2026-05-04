;;; anvil-nelisp-shims.el --- anvil-server runtime stub for NeLisp standalone  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 51 Phase 1.6 — Layer 3 (= application-level shim).
;;
;; Under regular Emacs the real `anvil-server' module wires
;; registered tools into the MCP transport.  Under NeLisp standalone
;; the Rust `anvil-runtime' binary owns the MCP transport, so the
;; shim here just collects each module's spec list into
;; `anvil-nelisp-shims--collected-specs' and lets the Rust side
;; drain it via `anvil-nelisp-shims-drain' (= called from
;; `AnvilModuleRegistry::new` after the load sequence completes).
;;
;; Layer placement note: the Emacs C-core compatibility shims that
;; this file used to host (= mapcar / plist-get / getenv /
;; declare-function / sqlite-* forwarders / user-emacs-directory
;; defvars) moved to `nelisp-emacs/src/emacs-{fns,callproc,eval,
;; vars,sqlite}.el' (Doc 51 Phase 1.6 layer split).  This file now
;; contains ONLY the anvil-application-specific shim — the
;; `anvil-server' MCP-transport replacement.
;;
;; Load order (NeLisp standalone):
;;   1. NeLisp Rust core bootstrap
;;   2. nelisp-emacs/src/emacs-init.el     ← Layer 2 polyfills
;;   3. anvil-nelisp-shims.el (this file)  ← Layer 3 anvil-server stub
;;   4. anvil-state.el / anvil-memory.el / ... (= real anvil modules)

;;; Code:

(defconst anvil-nelisp-shims-active-p (featurep 'nelisp)
  "Non-nil iff this shim layer is meaningful in the current runtime.
Captured at load time.  Informational only — the actual shim
definitions below do NOT gate on this constant; they use per-symbol
`unless (fboundp ...)' guards instead so they remain inert under
regular Emacs but install unconditionally under NeLisp standalone.")

(defvar anvil-nelisp-shims--collected-specs nil
  "Reverse-order alist of (SERVER-ID . SPEC-LIST) collected by the shim.
Populated by every `anvil-server-register-tool[s]' call during module
load.  Drained once by `anvil-nelisp-shims-drain' which the Rust
`AnvilModuleRegistry' invokes after the load sequence completes.")


;;;; --- anvil-server registration accumulator ------------------------------

(defun anvil-nelisp-shims--normalize-spec (spec)
  "Normalize SPEC to a plist starting with `:id'.

Anvil module spec forms come in two shapes:
  * `(HANDLER :id ID :description ... :read-only t)' — handler-first form
    used by `anvil-memory--tool-specs', `anvil-worklog--tool-specs', etc.
  * `(:id ID :handler HANDLER :description ...)' — already-plist form
    produced by `anvil-server-register-tool' (singular).

The Rust drain parser (`parse_drained_specs') only understands the
plist form, so we re-shape handler-first specs by splicing
`:handler HANDLER' into the plist tail."
  (cond
   ;; Already-plist form: starts with :id keyword (a symbol whose name
   ;; begins with `:').
   ((and (consp spec)
         (symbolp (car spec))
         (let ((sn (symbol-name (car spec))))
           (and (> (length sn) 0) (eq (aref sn 0) ?:))))
    spec)
   ;; Handler-first form: prepend (:handler HANDLER) to the rest.
   ((and (consp spec) (consp (cdr spec)))
    (cons :handler (cons (car spec) (cdr spec))))
   ;; Unknown shape: pass through untouched (= Rust will skip it).
   (t spec)))

(unless (fboundp 'anvil-server-register-tools)
  (defun anvil-server-register-tools (server-id specs)
    "Standalone shim — accumulate SPECS for SERVER-ID.
Returns SPECS unchanged so callers that chain on the return value
behave the same as under the real `anvil-server'.

SPECS may be in either handler-first or plist form; each spec is
normalized to plist form before storage so the Rust drain parser can
consume them uniformly."
    (let ((norm nil)
          (cur specs))
      (while cur
        (setq norm (cons (anvil-nelisp-shims--normalize-spec (car cur))
                         norm))
        (setq cur (cdr cur)))
      ;; Reverse `norm' back to original order.
      (let ((forward nil))
        (while norm
          (setq forward (cons (car norm) forward))
          (setq norm (cdr norm)))
        (setq anvil-nelisp-shims--collected-specs
              (cons (cons server-id forward)
                    anvil-nelisp-shims--collected-specs))))
    specs))

(unless (fboundp 'anvil-server-unregister-tools)
  (defun anvil-server-unregister-tools (server-id specs)
    "Standalone shim — no-op; the Rust runtime owns the MCP transport."
    (ignore server-id specs)
    nil))

(unless (fboundp 'anvil-server-register-tool)
  (defun anvil-server-register-tool (handler &rest properties)
    "Standalone shim — synthesize a 1-element spec from HANDLER and
PROPERTIES, then route through `anvil-server-register-tools'.
Manual plist scan because the bootstrap evaluator may not yet provide
`plist-get' (Layer 2 emacs-fns.el normally does, but the shim does not
take a hard dependency on its load order)."
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
  (defun anvil-server-unregister-tool (&rest args)
    (ignore args)
    nil))

(unless (fboundp 'anvil-server-encode-handler)
  (defun anvil-server-encode-handler (handler &rest opts)
    "Standalone shim — return HANDLER unchanged.
The Rust `anvil-runtime' owns MCP-side JSON encoding, so wrapping the
handler with the host `anvil-server' encoder is unnecessary here."
    (ignore opts)
    handler))

(unless (fboundp 'anvil-server-with-error-handling)
  (defmacro anvil-server-with-error-handling (&rest body)
    "Standalone shim — eval BODY without the host's MCP error wrapper.
The Rust runtime translates Elisp errors into JSON-RPC error responses
on its own, so the wrapping macro is a passthrough here."
    (cons 'progn body)))

(unless (fboundp 'anvil-server-tool-throw)
  (defun anvil-server-tool-throw (msg &optional data)
    "Standalone shim — signal an error.
The Rust runtime catches it and translates to a JSON-RPC error."
    (ignore data)
    (error "%s" msg)))

;; Provide the feature so modules' `(require 'anvil-server)' is satisfied.
(unless (featurep 'anvil-server)
  (provide 'anvil-server))


;;;; --- drain helper --------------------------------------------------------

(defun anvil-nelisp-shims-drain ()
  "Return all collected tool specs as a flat list and reset the accumulator.
Specs are returned in registration order (= forward, oldest first).
Implementation uses only bootstrap-eval primitives so it loads even
when Layer 2's `mapcar' / `nreverse' polyfills are absent."
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
      (let ((specs (cdr (car cur))))
        (while specs
          (setq total (+ total 1))
          (setq specs (cdr specs))))
      (setq cur (cdr cur)))
    total))


(provide 'anvil-nelisp-shims)

;;; anvil-nelisp-shims.el ends here
