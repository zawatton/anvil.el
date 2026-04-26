;;; anvil-agent.el --- Compact agent dispatch prompts -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Agent-spec loader: turn a (path-to-spec, brief context) pair into a
;; compact dispatch prompt for Claude Code's `Agent' tool / orchestrator
;; sub-sessions.  The point is to push the long spec body out of the
;; main session's context — the agent loads it via Read on its own
;; first action — while still capturing the standard contract
;; (commit format, ship target, acceptance items) once per call.
;;
;; Typical usage from the main session:
;;
;;   (anvil-agent-spec-loader "docs/specs/T162.md"
;;                            :goal "Implement T162 backend gap fix"
;;                            :ship-to "main direct commit"
;;                            :acceptance '("make test PASS"
;;                                          "value-correct gate 3/3"))
;;
;; returns a ~500-byte prompt string (vs ~3KB if the spec body were
;; pasted inline).  Across a 12-agent dispatch session that's ~30KB
;; saved on the main context window.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)

(defgroup anvil-agent nil
  "Compact agent dispatch prompts."
  :group 'anvil
  :prefix "anvil-agent-")

(defconst anvil-agent--server-id "emacs-eval"
  "Server id under which anvil-agent MCP tools register.")

(defcustom anvil-agent-default-ship-to "develop direct commit + push"
  "Default ship target line included in dispatch prompts."
  :type 'string
  :group 'anvil-agent)

(defcustom anvil-agent-default-coauthor
  "Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
  "Standard co-author trailer line for commits made by dispatched agents."
  :type 'string
  :group 'anvil-agent)

(defcustom anvil-agent-spec-root nil
  "Optional root directory to resolve relative SPEC-PATH against.
When nil the path is taken verbatim.  Useful when callers refer to
specs by `T162.md' instead of `docs/specs/T162.md'."
  :type '(choice (const :tag "no root" nil) directory)
  :group 'anvil-agent)


;;;; --- core formatter -----------------------------------------------------

(defun anvil-agent--resolve-spec-path (spec-path)
  "Resolve SPEC-PATH against `anvil-agent-spec-root' when relative."
  (cond
   ((file-name-absolute-p spec-path) spec-path)
   ((and anvil-agent-spec-root
         (file-exists-p (expand-file-name spec-path anvil-agent-spec-root)))
    (expand-file-name spec-path anvil-agent-spec-root))
   (t spec-path)))

(defun anvil-agent--bullet-list (items)
  "Return ITEMS rendered as a hyphen-bullet list, one per line."
  (mapconcat (lambda (s) (format "- %s" s)) items "\n"))

(cl-defun anvil-agent-spec-loader (spec-path
                                   &key goal ship-to acceptance preamble
                                   coauthor)
  "Build a compact agent dispatch prompt that loads the spec at SPEC-PATH.

SPEC-PATH is the file the agent will Read first.  It can be relative
to `anvil-agent-spec-root' (when set) or absolute.  Returns a string
ready to pass as the Agent tool's `prompt' field; raises when the
spec file is unreadable so the caller never dispatches an agent that
will immediately fail.

Plist keys:
  :goal       — 1-2 sentence summary of what the agent should accomplish.
  :ship-to    — branch / repo / commit target line (default
                `anvil-agent-default-ship-to').
  :acceptance — list of strings; rendered as a checklist.
  :preamble   — extra free-form text inserted before the standard contract.
  :coauthor   — override the Co-Authored-By trailer
                (default `anvil-agent-default-coauthor')."
  (let* ((resolved (anvil-agent--resolve-spec-path spec-path))
         (ship (or ship-to anvil-agent-default-ship-to))
         (coauth (or coauthor anvil-agent-default-coauthor)))
    (unless (file-readable-p resolved)
      (signal 'file-missing
              (list "anvil-agent-spec-loader: spec not readable" resolved)))
    (concat
     "First action: Read the full spec at " resolved ".\n"
     "\n"
     (when goal (concat "Goal: " goal "\n\n"))
     (when acceptance
       (concat "Acceptance:\n" (anvil-agent--bullet-list acceptance) "\n\n"))
     "Ship target: " ship "\n\n"
     (when (and preamble (not (string-empty-p preamble)))
       (concat preamble "\n\n"))
     "Standard contract:\n"
     "- Trailer every commit with: " coauth "\n"
     "- Run the project's test gate (e.g. make test) before shipping.\n"
     "- If you hit a blocker, return early with a clear explanation\n"
     "  rather than an ad-hoc workaround that might mask the issue.\n"
     "- Commit messages: WHY over WHAT, focused, no marketing prose.\n")))


;;;; --- MCP tool handler ---------------------------------------------------

(defun anvil-agent--tool-spec-loader
    (spec_path &optional goal ship_to acceptance preamble coauthor)
  "MCP wrapper for `anvil-agent-spec-loader'.

MCP Parameters:
  spec_path  - Path to spec markdown (required).  Resolved against
               `anvil-agent-spec-root' when relative + the variable is set.
  goal       - 1-2 sentence summary of what the agent should accomplish.
  ship_to    - Branch / repo / commit target line.  Defaults to
               `anvil-agent-default-ship-to'.
  acceptance - String of comma-separated acceptance items.  Each item
               is rendered as a `- <item>' bullet in the prompt.
  preamble   - Extra free-form text inserted before the standard contract.
  coauthor   - Override the standard Co-Authored-By trailer.

Returns (:prompt :spec-path :spec-bytes :prompt-bytes :coauthor) — caller
uses :prompt as the Agent tool's `prompt' field.  Pre-checks that the
spec file exists so a malformed dispatch fails here rather than after
agent spin-up."
  (anvil-server-with-error-handling
   (let* ((acc-list (and (stringp acceptance)
                         (not (string-empty-p acceptance))
                         (mapcar #'string-trim
                                 (split-string acceptance "," t "[ \t]+"))))
          (prompt (anvil-agent-spec-loader
                   spec_path
                   :goal goal
                   :ship-to (and (stringp ship_to)
                                 (not (string-empty-p ship_to))
                                 ship_to)
                   :acceptance acc-list
                   :preamble preamble
                   :coauthor (and (stringp coauthor)
                                  (not (string-empty-p coauthor))
                                  coauthor)))
          (resolved (anvil-agent--resolve-spec-path spec_path))
          (spec-bytes (and (file-readable-p resolved)
                           (nth 7 (file-attributes resolved)))))
     (list :prompt prompt
           :spec-path resolved
           :spec-bytes (or spec-bytes 0)
           :prompt-bytes (length prompt)
           :coauthor (or coauthor anvil-agent-default-coauthor)))))


;;;; --- module lifecycle ---------------------------------------------------

(defun anvil-agent--register-tools ()
  "Register anvil-agent MCP tools."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-agent--tool-spec-loader)
   :id "agent-spec-loader"
   :intent '(agent dispatch)
   :layer 'orchestration
   :server-id anvil-agent--server-id
   :description
   "Build a compact agent dispatch prompt that loads a spec file by
path rather than embedding the full body inline.  Returns a ~500B
prompt string suitable for the Agent tool's `prompt' field — the
agent's first action will be Read on the spec.  Pre-checks spec
file readability so a malformed dispatch fails here instead of
after agent spin-up.  Optional `goal' / `acceptance' /
`ship_to' / `preamble' / `coauthor' shape the standard contract
trailer.  Cuts ~3KB of repeated boilerplate per dispatch when used
across a 10+ agent parallel batch."
   :read-only t))

(defun anvil-agent--unregister-tools ()
  (ignore-errors
    (anvil-server-unregister-tool "agent-spec-loader"
                                  anvil-agent--server-id)))

;;;###autoload
(defun anvil-agent-enable ()
  "Register the anvil-agent MCP tools."
  (interactive)
  (anvil-agent--register-tools))

;;;###autoload
(defun anvil-agent-disable ()
  "Unregister the anvil-agent MCP tools."
  (interactive)
  (anvil-agent--unregister-tools))

(provide 'anvil-agent)

;;; anvil-agent.el ends here
