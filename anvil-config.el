;;; anvil-config.el --- User config loader for NeLisp standalone path -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Bridges the gap between two anvil entry paths:
;;
;;   * Emacs path (`emacs --daemon' + `emacsclient'):
;;     The user's `init.el' is the canonical place to override anvil
;;     defcustoms (e.g. `(setq anvil-orchestrator-codex-sandbox
;;     "workspace-write")').  This loader is a no-op there — `init.el'
;;     already ran and won.
;;
;;   * NeLisp standalone path (`bin/anvil mcp serve [--no-emacs]'):
;;     There is NO `init.el' — the Rust `anvil-runtime' binary boots
;;     a NeLisp interpreter and `eval's `anvil-host.el' directly.
;;     User config has nowhere to live.  This loader resolves
;;     `$ANVIL_CONFIG_DIR/config.el' (with sensible XDG fallbacks) and
;;     loads it after bootstrap so defcustom overrides take effect.
;;
;; Detection: `(featurep 'nelisp)' is the marker.  The Rust bootstrap
;; provides `nelisp' / `nelisp-load' / `nelisp-eval' so the feature is
;; t in the standalone path.  Pure Emacs never loads those, so it is
;; nil there.  This keeps the branching contract entirely on the
;; anvil.el side — no nelisp / Rust changes required.
;;
;; Resolution priority for the config directory:
;;
;;   1. `$ANVIL_CONFIG_DIR'                       — explicit override
;;   2. `$XDG_CONFIG_HOME/anvil'                  — XDG default
;;   3. `~/.config/anvil'                         — POSIX fallback
;;
;; The file inside that directory is always `config.el'.  Missing file
;; is silent (= optional).  Parse / eval failure is logged via
;; `display-warning' but does not abort bootstrap — a broken user
;; config must never wedge the MCP server.

;;; Code:

(defgroup anvil-config nil
  "User configuration loader for the anvil NeLisp standalone path."
  :group 'anvil
  :prefix "anvil-config-")

(defcustom anvil-config-file-name "config.el"
  "Basename of the user config file inside `anvil-config-dir'."
  :type 'string
  :group 'anvil-config)

(defun anvil-config--xdg-config-home ()
  "Return the effective `XDG_CONFIG_HOME', defaulting to `~/.config'."
  (or (getenv "XDG_CONFIG_HOME")
      (expand-file-name "~/.config")))

(defun anvil-config-dir ()
  "Resolve the effective anvil user config directory.
Priority order: `$ANVIL_CONFIG_DIR' > `$XDG_CONFIG_HOME/anvil' >
`~/.config/anvil'.  The directory itself does NOT need to exist —
this only computes the path."
  (or (getenv "ANVIL_CONFIG_DIR")
      (expand-file-name "anvil" (anvil-config--xdg-config-home))))

(defun anvil-config-file ()
  "Return the absolute path to the user config file.
The file may or may not exist — callers should `file-readable-p'
before loading."
  (expand-file-name anvil-config-file-name (anvil-config-dir)))

(defun anvil-config-active-p ()
  "Return non-nil when the config loader should fire.
True iff the NeLisp standalone path is in effect, detected via
`(featurep \\='nelisp)'.  Under regular Emacs the feature is not
provided, so the loader stays inert and `init.el' remains the
canonical config source."
  (featurep 'nelisp))

;;;###autoload
(defun anvil-load-user-config ()
  "Load the anvil user config file when running under NeLisp standalone.

Returns the absolute path of the file that was loaded, or nil when
either (a) we are running under regular Emacs (= `init.el' wins),
or (b) the resolved config file does not exist.

A broken config file (parse / eval error) is logged via
`display-warning' under the `anvil-config' tag and the function
returns nil — the standalone MCP server must never refuse to boot
because of a user-side config typo."
  (when (anvil-config-active-p)
    (let ((file (anvil-config-file)))
      (when (file-readable-p file)
        (condition-case err
            (progn
              (load file nil t nil)
              file)
          (error
           (display-warning
            'anvil-config
            (format "failed to load %s: %s"
                    file (error-message-string err))
            :warning)
           nil))))))

;; Auto-fire on load.  In Emacs this is a cheap no-op (featurep check
;; returns nil immediately).  In NeLisp standalone this is the only
;; place the user config gets a chance to run before module init code.
(anvil-load-user-config)

(provide 'anvil-config)

;;; anvil-config.el ends here
