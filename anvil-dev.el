;;; anvil-dev.el --- Developer / ops helpers for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Small tools that are *about* anvil rather than part of its
;; operation.  Currently:
;;
;;   - `anvil-self-sync-check'
;;     — inspect the git state of the anvil clone that Emacs loaded,
;;       and optionally compare it to a separate dev checkout so the
;;       "installed tree silently diverged from the dev tree" bug
;;       (2026-04-16) is caught in one MCP call.
;;
;; Enable via `(add-to-list 'anvil-optional-modules 'dev)' in init.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)

(defgroup anvil-dev nil
  "Developer / ops helpers for anvil."
  :group 'anvil
  :prefix "anvil-dev-")

(defcustom anvil-dev-source-path nil
  "Optional path to a separate dev checkout of anvil.el.
When non-nil, `anvil-self-sync-check' fetches its HEAD and
compares it against the installed clone's HEAD so `git pull'
gaps surface before the next daemon restart."
  :type '(choice (const :tag "No dev clone" nil) directory)
  :group 'anvil-dev)

(defconst anvil-dev--server-id "emacs-eval"
  "Server ID for the dev-* MCP tools.")

;;;; --- internal ------------------------------------------------------------

(defun anvil-dev--git-at (dir &rest args)
  "Run `git ARGS' inside DIR and return trimmed stdout, or nil on failure."
  (when (and dir (file-directory-p dir))
    (with-temp-buffer
      (let* ((default-directory (file-name-as-directory dir))
             (status (apply #'call-process "git" nil t nil args)))
        (when (and (integerp status) (zerop status))
          (string-trim (buffer-string)))))))

(defun anvil-dev--short-sha (sha)
  "Return the first 7 chars of SHA, or SHA itself if shorter / nil."
  (and sha (stringp sha)
       (if (> (length sha) 7) (substring sha 0 7) sha)))

(defun anvil-dev--git-state (dir)
  "Return a plist describing the git state of DIR (nil if not a worktree).
Keys: :head :branch :dirty-count."
  (let ((head (anvil-dev--git-at dir "rev-parse" "HEAD")))
    (when head
      (let* ((branch (anvil-dev--git-at dir "rev-parse" "--abbrev-ref" "HEAD"))
             (porc (anvil-dev--git-at dir "status" "--porcelain"))
             (dirty (if (and porc (not (string-empty-p porc)))
                        (length (split-string porc "\n" t))
                      0)))
        (list :head head :branch branch :dirty-count dirty)))))

(defun anvil-dev--derive-warning (src-dir installed-state dev-dir dev-state)
  "Produce a human-readable warning string, or nil when all is well."
  (cond
   ((null src-dir)
    "anvil-server not located — library not loaded")
   ((null installed-state)
    (format "installed dir %s is not a git worktree" src-dir))
   ((and dev-dir (null dev-state))
    (format "dev-source-path %s is not a git worktree" dev-dir))
   ((and dev-state
         (not (equal (plist-get installed-state :head)
                     (plist-get dev-state :head))))
    (format "installed HEAD %s ≠ dev HEAD %s — run `git pull' in %s"
            (anvil-dev--short-sha (plist-get installed-state :head))
            (anvil-dev--short-sha (plist-get dev-state :head))
            src-dir))))

;;;; --- public --------------------------------------------------------------

;;;###autoload
(defun anvil-self-sync-check ()
  "Report anvil's installed git state, and mismatch vs the dev checkout.

Returns a plist:
  :installed-dir         where `anvil-server' was loaded from
  :installed-head        HEAD SHA of that worktree (nil if not a git repo)
  :installed-branch      current branch of the installed clone
  :installed-dirty-count number of modified / untracked files
  :dev-dir               `anvil-dev-source-path' (or nil)
  :dev-head              HEAD SHA of the dev clone (nil / not set)
  :dev-branch            branch of the dev clone
  :in-sync               t when HEADs match OR when no dev-dir is configured
  :warning               short human string when something is off (nil = OK)

Motivation: 2026-04-16 reproduced the \"old anvil-worker loaded\"
trap where the running daemon read an outdated default because a
second clone (`external-packages/anvil.el/`) stayed behind the
dev tree.  One call to this helper now surfaces that mismatch."
  (let* ((src-file (locate-library "anvil-server"))
         (src-dir  (and src-file (file-name-directory src-file)))
         (installed (and src-dir (anvil-dev--git-state src-dir)))
         (dev-dir  anvil-dev-source-path)
         (dev-st   (and dev-dir (anvil-dev--git-state dev-dir)))
         (in-sync  (or (null dev-dir)
                       (and installed dev-st
                            (equal (plist-get installed :head)
                                   (plist-get dev-st :head)))))
         (warning  (anvil-dev--derive-warning
                    src-dir installed dev-dir dev-st)))
    (list :installed-dir         src-dir
          :installed-head        (plist-get installed :head)
          :installed-branch      (plist-get installed :branch)
          :installed-dirty-count (or (plist-get installed :dirty-count) 0)
          :dev-dir               dev-dir
          :dev-head              (plist-get dev-st :head)
          :dev-branch            (plist-get dev-st :branch)
          :in-sync               (and in-sync t)
          :warning               warning)))

(defun anvil-dev--tool-self-sync-check ()
  "MCP wrapper for `anvil-self-sync-check'.

MCP Parameters: none.  Returns a printed plist comparing the
installed anvil clone's git HEAD with `anvil-dev-source-path'."
  (anvil-server-with-error-handling
   (format "%S" (anvil-self-sync-check))))

;;;; --- module lifecycle ----------------------------------------------------

;;;###autoload
(defun anvil-dev-enable ()
  "Register the dev-* MCP tools."
  (anvil-server-register-tool
   #'anvil-dev--tool-self-sync-check
   :id "anvil-self-sync-check"
   :server-id anvil-dev--server-id
   :description
   "Report the installed anvil clone's git HEAD + branch + dirty
count, and (when `anvil-dev-source-path' is set) compare against
the dev checkout to flag unpushed / unpulled divergence before it
causes a silent \"old code loaded\" bug after a daemon restart."
   :read-only t)
)

(defun anvil-dev-disable ()
  "Unregister the dev-* MCP tools."
  (anvil-server-unregister-tool "anvil-self-sync-check"
                                anvil-dev--server-id))

(provide 'anvil-dev)
;;; anvil-dev.el ends here
