;;; anvil-autoresearch.el --- Local autoresearch candidate scanner -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 43 — local scanner/planner/apply-eval/orchestration primitives.
;;
;; The full autonomous Doc 43 loop across many worktree candidates is
;; still intentionally deferred.  This module ships the safe primitives:
;; scan local web / X / YouTube summaries plus design docs, rank weak MCP
;; descriptions, generate rewrite plans, produce dry-run patches, submit
;; an LLM proposal task through the orchestrator, and apply a reviewed
;; docstring rewrite with inner/holdout rollback gates.  It also exposes
;; a lightweight tabulated-list dashboard over autoresearch orchestrator
;; tasks.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'pp)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'anvil-server)

(declare-function anvil-orchestrator-submit "anvil-orchestrator" (tasks))
(declare-function anvil-orchestrator-collect "anvil-orchestrator"
                  (batch-id &rest args))
(declare-function anvil-orchestrator-select-provider
                  "anvil-orchestrator-routing" (&rest args))
(declare-function anvil-memory-add "anvil-memory"
                  (name type body &rest args))

(defvar anvil-orchestrator--tasks nil
  "Orchestrator task table, read opportunistically by the dashboard.")


;;;; --- customization ------------------------------------------------------

(defgroup anvil-autoresearch nil
  "Read-only autoresearch candidate discovery for anvil."
  :group 'anvil
  :prefix "anvil-autoresearch-")

(defconst anvil-autoresearch--server-id "emacs-eval"
  "Server id under which autoresearch-* MCP tools register.")

(defcustom anvil-autoresearch-roots nil
  "Directories or files scanned by `anvil-autoresearch-scan'.
When nil, `anvil-autoresearch--default-roots' is used.  Missing
paths are ignored."
  :type '(repeat string)
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-file-extensions '("org" "md" "txt")
  "File extensions scanned by `anvil-autoresearch-scan'."
  :type '(repeat string)
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-max-files 500
  "Maximum number of files scanned in one autoresearch pass."
  :type 'integer
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-max-file-bytes (* 512 1024)
  "Maximum bytes read from each file during an autoresearch scan."
  :type 'integer
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-default-limit 20
  "Default number of candidates returned by `anvil-autoresearch-scan'."
  :type 'integer
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-docstring-short-description-chars 80
  "Description length below which a registered tool is treated as weak."
  :type 'integer
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-docstring-proposal-provider 'codex
  "Default orchestrator provider for docstring proposal tasks."
  :type 'symbol
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-docstring-proposal-model nil
  "Optional model name for docstring proposal tasks.
Nil delegates model selection to the orchestrator provider default."
  :type '(choice (const :tag "Provider default" nil) string)
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-docstring-proposal-budget-usd 0.10
  "Default per-task budget for docstring proposal tasks."
  :type 'number
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-docstring-proposal-timeout-sec 120
  "Default wall-clock timeout for docstring proposal tasks."
  :type 'integer
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-docstring-proposal-heartbeat-timeout-sec 60
  "Default no-output heartbeat timeout for docstring proposal tasks."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-docstring-proposal-sandbox "workspace-write"
  "Default Codex sandbox mode for docstring proposal tasks."
  :type 'string
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-docstring-proposal-batch-count 3
  "Default number of proposal tasks submitted by docstring batch propose."
  :type 'integer
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-docstring-run-max-candidates 5
  "Default maximum candidate descriptions tried by docstring run."
  :type 'integer
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-routing-policies
  '(balanced speed cost quality)
  "Routing policies evaluated by `anvil-autoresearch-routing-evaluate'."
  :type '(repeat symbol)
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-nelisp-root "~/Cowork/Notes/dev/nelisp"
  "Default NeLisp repository root for Doc 43 Phase 5 codegen tasks."
  :type 'directory
  :group 'anvil-autoresearch)

(defcustom anvil-autoresearch-nelisp-codegen-target-files
  '("lisp/nelisp-phase47-compiler.el"
    "test/nelisp-phase47-compiler-test.el"
    "docs/design/97-phase-47-sexp-to-asm-compiler.org"
    "docs/design/129-phase47-user-el-aot-compile.org")
  "NeLisp files used as Doc 43 Phase 5 codegen target context."
  :type '(repeat string)
  :group 'anvil-autoresearch)


;;;; --- scoring ------------------------------------------------------------

(defconst anvil-autoresearch--signals
  `((,(regexp-quote "未実装") . 10)
    (,(regexp-quote "実装候補") . 9)
    (,(regexp-quote "実装") . 4)
    (,(regexp-quote "改善") . 4)
    (,(regexp-quote "提案") . 3)
    ("\\bTODO\\b" . 7)
    ("\\bDRAFT\\b" . 6)
    ("\\bPhase[ -]?[0-9A-Za-z]+" . 4)
    ("\\bMCP\\b" . 5)
    ("\\btool\\b" . 3)
    ("\\bdocstring\\b" . 4)
    ("\\bagent\\b" . 3)
    ("\\bAI\\b" . 2)
    ("\\btoken\\b" . 3)
    ("\\bbudget\\b" . 4)
    ("\\bheartbeat\\b" . 4)
    ("\\bTACO\\b" . 5)
    ("\\bPaperclip\\b" . 4)
    ("\\bWHISPER\\b" . 4)
    ("\\bSubconscious\\b" . 4)
    ("\\bKarpathy\\b" . 4)
    ("\\bautoresearch\\b" . 7)
    ("\\bruntime harness\\b" . 4)
    ("\\bskill\\b" . 3)
    ("\\bClaude Code\\b" . 3)
    ("\\bCodex\\b" . 3))
  "Regex signal table used to score autoresearch candidates.")

(defun anvil-autoresearch--module-directory ()
  "Return the directory where this module is loaded from."
  (file-name-directory
   (or load-file-name
       (and buffer-file-name (file-truename buffer-file-name))
       default-directory)))

(defun anvil-autoresearch--default-roots ()
  "Return default local archive/design roots, keeping only existing paths."
  (let* ((repo (anvil-autoresearch--module-directory))
         (paths (list "~/Cowork/Notes/capture/web"
                      "~/Cowork/Notes/capture/yt"
                      (expand-file-name "docs/design" repo))))
    (seq-filter #'file-exists-p
                (delete-dups (mapcar #'expand-file-name paths)))))

(defun anvil-autoresearch--coerce-limit (limit)
  "Return LIMIT as a positive integer, or the configured default."
  (let ((n (cond
            ((integerp limit) limit)
            ((and (numberp limit) (> limit 0)) (truncate limit))
            ((and (stringp limit) (not (string-empty-p limit)))
             (string-to-number limit))
            (t anvil-autoresearch-default-limit))))
    (max 1 (min 200 n))))

(defun anvil-autoresearch--coerce-number (value default name)
  "Return VALUE as a number, DEFAULT when blank, or signal for NAME."
  (cond
   ((or (null value)
        (and (stringp value) (string-empty-p (string-trim value))))
    default)
   ((numberp value) value)
   ((and (stringp value)
         (string-match-p
          "\\`[ \t\n\r]*[-+]?[0-9]+\\(?:\\.[0-9]+\\)?[ \t\n\r]*\\'"
          value))
    (string-to-number value))
   (t (error "%s must be a number" name))))

(defun anvil-autoresearch--coerce-provider (value default)
  "Return VALUE as an orchestrator provider symbol, using DEFAULT if blank."
  (cond
   ((or (null value)
        (and (stringp value) (string-empty-p (string-trim value))))
    default)
   ((symbolp value) value)
   ((stringp value) (intern (string-trim value)))
   (t (error "provider must be a symbol or string"))))

(defun anvil-autoresearch--coerce-string (value default)
  "Return VALUE as a trimmed string, DEFAULT when blank."
  (cond
   ((or (null value)
        (and (stringp value) (string-empty-p (string-trim value))))
    default)
   ((stringp value) (string-trim value))
   (t (format "%s" value))))

(defun anvil-autoresearch--repo-root (&optional cwd)
  "Return an existing CWD or the repository root containing this module."
  (let* ((base (file-name-as-directory
                (expand-file-name
                 (or (and (stringp cwd) (not (string-empty-p cwd)) cwd)
                     (anvil-autoresearch--module-directory)))))
         (root (or (locate-dominating-file base ".git")
                   (anvil-autoresearch--module-directory))))
    (unless (file-directory-p root)
      (error "cwd does not exist: %s" root))
    (directory-file-name (expand-file-name root))))

(defun anvil-autoresearch--blank-string-p (value)
  "Return non-nil when VALUE is nil or an empty string."
  (or (null value)
      (and (stringp value) (string-empty-p (string-trim value)))))

(defun anvil-autoresearch--roots-from-json (roots-json)
  "Parse ROOTS-JSON into a list of existing file names.
ROOTS-JSON must be a JSON array of strings when present."
  (if (anvil-autoresearch--blank-string-p roots-json)
      nil
    (let ((decoded (json-parse-string roots-json
                                      :array-type 'list
                                      :object-type 'alist)))
      (unless (listp decoded)
        (error "roots_json must be a JSON array of path strings"))
      (cl-loop for path in decoded
               unless (stringp path)
               do (error "roots_json must contain only path strings")
               collect (expand-file-name path)))))

(defun anvil-autoresearch--roots (&optional roots-json)
  "Return effective scan roots from ROOTS-JSON or defaults."
  (let ((roots (or (anvil-autoresearch--roots-from-json roots-json)
                   anvil-autoresearch-roots
                   (anvil-autoresearch--default-roots))))
    (seq-filter #'file-exists-p
                (delete-dups (mapcar #'expand-file-name roots)))))

(defun anvil-autoresearch--extension-regexp ()
  "Return a regexp matching configured scan file extensions."
  (format "\\.\\(%s\\)\\'"
          (mapconcat #'regexp-quote
                     anvil-autoresearch-file-extensions
                     "\\|")))

(defun anvil-autoresearch--collect-files (roots)
  "Collect scan files from ROOTS, bounded by `anvil-autoresearch-max-files'."
  (let ((regexp (anvil-autoresearch--extension-regexp))
        files)
    (dolist (root roots)
      (cond
       ((and (file-regular-p root) (string-match-p regexp root))
        (push root files))
       ((file-directory-p root)
        (setq files
              (nconc files
                     (ignore-errors
                       (directory-files-recursively root regexp nil)))))))
    (setq files (sort (delete-dups (mapcar #'expand-file-name files))
                      #'string<))
    (cl-subseq files 0 (min (length files)
                            (max 0 anvil-autoresearch-max-files)))))

(defun anvil-autoresearch--read-prefix (file)
  "Return the initial readable content of FILE."
  (with-temp-buffer
    (insert-file-contents-literally
     file nil 0 (and anvil-autoresearch-max-file-bytes
                     (max 0 anvil-autoresearch-max-file-bytes)))
    (buffer-string)))

(defun anvil-autoresearch--heading-at-line (line fallback)
  "Return heading text from LINE, or FALLBACK."
  (cond
   ((string-match "^\\*+[ \t]+\\(.+\\)$" line)
    (string-trim (match-string 1 line)))
   ((string-match "^#+[ \t]+\\(.+\\)$" line)
    (string-trim (match-string 1 line)))
   (t fallback)))

(defun anvil-autoresearch--source-kind (file)
  "Infer archive/source kind from FILE."
  (let ((path (replace-regexp-in-string "\\\\" "/" file)))
    (cond
     ((string-match-p "/docs/design/" path) "design-doc")
     ((string-match-p "/capture/yt/" path) "youtube-summary")
     ((or (string-match-p "/capture/web/.*_x_" path)
          (string-match-p "/capture/web/.*twitter" path))
      "x-post")
     ((string-match-p "/capture/web/" path) "web-archive")
     (t "archive"))))

(defun anvil-autoresearch--line-score (line file)
  "Return heuristic autoresearch score for LINE from FILE."
  (let ((case-fold-search t)
        (score 0))
    (dolist (entry anvil-autoresearch--signals)
      (when (string-match-p (car entry) line)
        (cl-incf score (cdr entry))))
    (when (and (> score 0)
               (string-match-p "/docs/design/" file))
      (cl-incf score 2))
    score))

(defun anvil-autoresearch--suggested-action (text kind)
  "Return a compact implementation action hint for TEXT from KIND."
  (let ((case-fold-search t))
    (cond
     ((string-match-p "\\bMCP\\b\\|\\btool\\b\\|\\bdocstring\\b" text)
      "candidate-mcp-tool")
     ((string-match-p "\\btoken\\b\\|\\bTACO\\b\\|compact\\|圧縮" text)
      "token-efficiency")
     ((string-match-p "\\bbudget\\b\\|\\bheartbeat\\b\\|timeout" text)
      "orchestrator-safety")
     ((string-match-p "\\bDRAFT\\b\\|\\bTODO\\b\\|未実装" text)
      "draft-to-phase-0")
     ((string= kind "youtube-summary")
      "video-idea-triage")
     (t "review-and-scope"))))

(defun anvil-autoresearch--query-match-p (candidate query)
  "Return non-nil when CANDIDATE matches QUERY, or QUERY is blank."
  (if (anvil-autoresearch--blank-string-p query)
      t
    (let* ((q (regexp-quote (string-trim query)))
           (haystack (mapconcat
                      #'identity
                      (delq nil
                            (list (plist-get candidate :file)
                                  (plist-get candidate :heading)
                                  (plist-get candidate :kind)
                                  (plist-get candidate :evidence)
                                  (plist-get candidate :suggested-action)))
                      "\n"))
           (case-fold-search t))
      (string-match-p q haystack))))

(defun anvil-autoresearch--scan-file (file)
  "Return candidate plists found in FILE."
  (let ((content (ignore-errors (anvil-autoresearch--read-prefix file)))
        (heading nil)
        candidates)
    (when content
      (cl-loop for line in (split-string content "\n")
               for line-number from 1
               do
               (setq heading
                     (anvil-autoresearch--heading-at-line line heading))
               (let ((score (anvil-autoresearch--line-score line file)))
                 (when (> score 0)
                   (let* ((kind (anvil-autoresearch--source-kind file))
                          (evidence (truncate-string-to-width
                                     (string-trim line) 220 nil nil t))
                          (candidate
                           (list :file (abbreviate-file-name file)
                                 :line line-number
                                 :heading (or heading
                                              (file-name-base file))
                                 :kind kind
                                 :score score
                                 :evidence evidence
                                 :suggested-action
                                 (anvil-autoresearch--suggested-action
                                  evidence kind))))
                     (push candidate candidates))))))
    (nreverse candidates)))

(defun anvil-autoresearch-scan (&optional query roots-json limit)
  "Scan local archives/design docs for anvil improvement candidates.

MCP Parameters:
  query      - Optional substring filter.  Matching is case-insensitive
               across file, heading, source kind, evidence, and
               suggested action.  Empty / nil returns all candidates.
  roots_json - Optional JSON array of directories/files to scan instead
               of `anvil-autoresearch-roots' or the default roots.
  limit      - Optional positive cap on returned candidates (default
               `anvil-autoresearch-default-limit', hard-capped at 200).

This is read-only.  It does not call an LLM, mutate source files, or
start the Doc 43 experiment loop."
  (let* ((roots (anvil-autoresearch--roots roots-json))
         (files (anvil-autoresearch--collect-files roots))
         (limit (anvil-autoresearch--coerce-limit limit))
         (candidates
          (cl-loop for file in files
                   nconc (anvil-autoresearch--scan-file file))))
    (setq candidates
          (seq-filter (lambda (candidate)
                        (anvil-autoresearch--query-match-p candidate query))
                      candidates))
    (setq candidates
          (sort candidates
                (lambda (a b)
                  (let ((sa (plist-get a :score))
                        (sb (plist-get b :score)))
                    (if (= sa sb)
                        (string< (format "%s:%06d"
                                         (plist-get a :file)
                                         (plist-get a :line))
                                 (format "%s:%06d"
                                         (plist-get b :file)
                                         (plist-get b :line)))
                      (> sa sb))))))
    (list :roots (apply #'vector roots)
          :files-scanned (length files)
          :query (or query "")
          :limit limit
          :candidate-count (length candidates)
          :candidates (apply #'vector
                             (cl-subseq candidates 0
                                        (min (length candidates) limit))))))

(defun anvil-autoresearch--tool-scan (&optional query roots_json limit)
  "MCP wrapper for `anvil-autoresearch-scan'.

MCP Parameters:
  query      - Optional substring filter.
  roots_json - Optional JSON array of directories/files to scan.
  limit      - Optional positive cap on returned candidates."
  (anvil-autoresearch-scan query roots_json limit))


;;;; --- docstring target scoring ------------------------------------------

(defun anvil-autoresearch--schema-field (schema field)
  "Return FIELD from SCHEMA, accepting symbol or string alist keys."
  (or (alist-get field schema nil nil #'eq)
      (alist-get (symbol-name field) schema nil nil #'equal)))

(defun anvil-autoresearch--schema-properties (schema)
  "Return input-schema property alist from SCHEMA."
  (or (anvil-autoresearch--schema-field schema 'properties)
      nil))

(defun anvil-autoresearch--param-description (param-schema)
  "Return PARAM-SCHEMA description, or nil."
  (anvil-autoresearch--schema-field param-schema 'description))

(defun anvil-autoresearch--word-set (text)
  "Return normalized content words from TEXT."
  (let (words)
    (dolist (word (split-string (downcase (or text "")) "[^[:alnum:]_-]+" t))
      (when (>= (length word) 4)
        (push word words)))
    (delete-dups (nreverse words))))

(defun anvil-autoresearch--id-token-coverage (id description)
  "Return non-nil when DESCRIPTION mentions a useful token from ID."
  (let* ((tokens (anvil-autoresearch--word-set
                  (replace-regexp-in-string "[-_]" " " id)))
         (desc (downcase (or description ""))))
    (or (null tokens)
        (cl-some (lambda (token)
                   (string-match-p (regexp-quote token) desc))
                 tokens))))

(defun anvil-autoresearch--generic-description-p (description)
  "Return non-nil when DESCRIPTION is mostly a generic verb phrase."
  (and (stringp description)
       (string-match-p
        "\\`[ \t\n\r]*\\(return\\|returns\\|get\\|list\\|run\\|read\\|write\\|create\\|update\\|delete\\|report\\)\\b"
        (downcase description))
       (< (length description) 140)))

(defun anvil-autoresearch--guidance-p (description)
  "Return non-nil when DESCRIPTION gives usage guidance, not just action."
  (and (stringp description)
       (let ((case-fold-search t))
         (string-match-p
          "\\b\\(use\\|when\\|optional\\|required\\|filter\\|returns\\|read-only\\|write\\|pair with\\|instead\\|after\\|before\\)\\b"
          description))))

(defun anvil-autoresearch--docstring-issues (id tool)
  "Return (SCORE . ISSUES) for registered TOOL with ID."
  (let* ((description (or (plist-get tool :description) ""))
         (schema (or (plist-get tool :schema) '((type . "object"))))
         (properties (anvil-autoresearch--schema-properties schema))
         (score 0)
         issues)
    (when (< (length (string-trim description))
             anvil-autoresearch-docstring-short-description-chars)
      (cl-incf score 20)
      (push "short-description" issues))
    (when (anvil-autoresearch--generic-description-p description)
      (cl-incf score 10)
      (push "generic-description" issues))
    (unless (anvil-autoresearch--id-token-coverage id description)
      (cl-incf score 8)
      (push "id-token-missing" issues))
    (unless (anvil-autoresearch--guidance-p description)
      (cl-incf score 6)
      (push "usage-guidance-missing" issues))
    (dolist (entry properties)
      (let* ((param (format "%s" (car entry)))
             (param-schema (cdr entry))
             (param-desc (anvil-autoresearch--param-description
                          param-schema)))
        (cond
         ((anvil-autoresearch--blank-string-p param-desc)
          (cl-incf score 15)
          (push (format "param-description-missing:%s" param) issues))
         ((< (length (string-trim param-desc)) 12)
          (cl-incf score 8)
          (push (format "param-description-short:%s" param) issues)))))
    (cons score (nreverse issues))))

(defun anvil-autoresearch--registered-tools (&optional server-id)
  "Return registered tool rows, optionally restricted to SERVER-ID."
  (let (rows)
    (maphash
     (lambda (sid table)
       (when (or (anvil-autoresearch--blank-string-p server-id)
                 (string= sid server-id))
         (maphash
          (lambda (id tool)
            (push (list :server-id sid :id id :tool tool) rows))
          table)))
     anvil-server--tools)
    (sort rows
          (lambda (a b)
            (string< (format "%s/%s"
                             (plist-get a :server-id)
                             (plist-get a :id))
                     (format "%s/%s"
                             (plist-get b :server-id)
                             (plist-get b :id)))))))

(defun anvil-autoresearch--docstring-match-p (candidate query)
  "Return non-nil when docstring CANDIDATE matches QUERY."
  (if (anvil-autoresearch--blank-string-p query)
      t
    (let ((case-fold-search t)
          (q (regexp-quote (string-trim query)))
          (haystack
           (mapconcat
            #'identity
            (delq nil
                  (list (plist-get candidate :id)
                        (plist-get candidate :server-id)
                        (plist-get candidate :handler)
                        (plist-get candidate :description)
                        (mapconcat #'identity
                                   (append (plist-get candidate :issues)
                                           nil)
                                   " ")))
            "\n")))
      (string-match-p q haystack))))

(defun anvil-autoresearch-docstrings (&optional query server-id limit)
  "Rank registered MCP tools whose descriptions are weak rewrite targets.

MCP Parameters:
  query     - Optional substring filter over id, server, handler,
              description, and issue names.
  server-id - Optional MCP server id to restrict the scan.  Empty / nil
              scans every registered server table.
  limit     - Optional positive cap on returned candidates (default
              `anvil-autoresearch-default-limit', hard-capped at 200).

This is the Doc 43 Phase 1a evaluator for Q1-A (MCP tool docstring
autoresearch).  It is read-only: it does not rewrite descriptions,
call an LLM, or run the experiment loop."
  (let* ((limit (anvil-autoresearch--coerce-limit limit))
         (rows (anvil-autoresearch--registered-tools server-id))
         candidates)
    (dolist (row rows)
      (let* ((id (plist-get row :id))
             (tool (plist-get row :tool))
             (score+issues (anvil-autoresearch--docstring-issues id tool))
             (score (car score+issues))
             (issues (cdr score+issues)))
        (when (> score 0)
          (let* ((handler (plist-get tool :handler))
                 (description (or (plist-get tool :description) ""))
                 (schema (or (plist-get tool :schema) '((type . "object"))))
                 (properties (anvil-autoresearch--schema-properties schema))
                 (candidate
                  (list :server-id (plist-get row :server-id)
                        :id id
                        :handler (if (symbolp handler)
                                     (symbol-name handler)
                                   (format "%S" handler))
                        :score score
                        :issues (apply #'vector issues)
                        :description-length (length description)
                        :param-count (length properties)
                        :description
                        (truncate-string-to-width
                         (string-trim description) 260 nil nil t)
                        :suggested-action
                        "rewrite-mcp-description")))
            (when (anvil-autoresearch--docstring-match-p candidate query)
              (push candidate candidates))))))
    (setq candidates
          (sort candidates
                (lambda (a b)
                  (let ((sa (plist-get a :score))
                        (sb (plist-get b :score)))
                    (if (= sa sb)
                        (string< (plist-get a :id)
                                 (plist-get b :id))
                      (> sa sb))))))
    (list :server-id (or server-id "")
          :query (or query "")
          :registered-count (length rows)
          :candidate-count (length candidates)
          :limit limit
          :candidates (apply #'vector
                             (cl-subseq candidates 0
                                        (min (length candidates) limit))))))

(defun anvil-autoresearch--tool-docstrings
    (&optional query server_id limit)
  "MCP wrapper for `anvil-autoresearch-docstrings'.

MCP Parameters:
  query     - Optional substring filter.
  server_id - Optional MCP server id to restrict the scan.
  limit     - Optional positive cap on returned candidates."
  (anvil-autoresearch-docstrings query server_id limit))


;;;; --- docstring rewrite planner -----------------------------------------

(defun anvil-autoresearch--find-tool (tool-id &optional server-id)
  "Return the registered row for TOOL-ID, optionally scoped to SERVER-ID."
  (unless (and (stringp tool-id) (not (string-empty-p tool-id)))
    (error "tool_id is required"))
  (let ((matches
         (seq-filter
          (lambda (row)
            (and (string= (plist-get row :id) tool-id)
                 (or (anvil-autoresearch--blank-string-p server-id)
                     (string= (plist-get row :server-id) server-id))))
          (anvil-autoresearch--registered-tools server-id))))
    (cond
     ((null matches)
      (error "No registered MCP tool found for id=%s server=%s"
             tool-id (or server-id "")))
     ((cdr matches)
      (error "Multiple registered MCP tools found for id=%s; pass server_id"
             tool-id))
     (t (car matches)))))

(defun anvil-autoresearch--schema-params (tool)
  "Return parameter plists for TOOL."
  (let* ((schema (or (plist-get tool :schema) '((type . "object"))))
         (properties (anvil-autoresearch--schema-properties schema)))
    (cl-loop for (name . param-schema) in properties
             collect
             (list :name (format "%s" name)
                   :description
                   (or (anvil-autoresearch--param-description
                        param-schema)
                       "")
                   :type
                   (or (anvil-autoresearch--schema-field
                        param-schema 'type)
                       "string")))))

(defun anvil-autoresearch--line-snippet (file line &optional radius)
  "Return a small line snippet around FILE LINE."
  (let ((radius (or radius 2))
        rows)
    (let ((start (max 1 (- line radius)))
          (end (+ line radius)))
      (with-temp-buffer
        (insert-file-contents-literally file)
        (goto-char (point-min))
        (forward-line (1- start))
        (cl-loop for n from start to end
                 while (not (eobp))
                 do
                 (push (format "%d:%s"
                               n
                               (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
                       rows)
                 (forward-line 1))))
    (mapconcat #'identity (nreverse rows) "\n")))

(defun anvil-autoresearch--find-registration-site (tool-id)
  "Return the first repo registration site for TOOL-ID, or nil."
  (let* ((repo (anvil-autoresearch--module-directory))
         (needle (format ":id[ \t\n\r]+%S" tool-id))
         (files (directory-files repo t "\\`anvil-.*\\.el\\'"))
         hit)
    (while (and files (null hit))
      (let ((file (pop files)))
        (with-temp-buffer
          (insert-file-contents-literally file)
          (goto-char (point-min))
          (when (re-search-forward needle nil t)
            (let ((line (line-number-at-pos)))
              (setq hit
                    (list :file (abbreviate-file-name file)
                          :line line
                          :snippet
                          (anvil-autoresearch--line-snippet
                           file line 3))))))))
    hit))

(defun anvil-autoresearch--find-description-site (tool-id)
  "Return source bounds for TOOL-ID's registered `:description' string."
  (let* ((repo (anvil-autoresearch--module-directory))
         (needle (format ":id[ \t\n\r]+%S" tool-id))
         (files (directory-files repo t "\\`anvil-.*\\.el\\'"))
         hit)
    (while (and files (null hit))
      (let ((file (pop files)))
        (with-temp-buffer
          (insert-file-contents-literally file)
          (goto-char (point-min))
          (when (re-search-forward needle nil t)
            (when (re-search-forward ":description\\_>" nil t)
              (skip-chars-forward " \t\n\r")
              (let* ((start (point))
                     (start-line (line-number-at-pos))
                     (value (read (current-buffer)))
                     (end (point))
                     (end-line (line-number-at-pos end)))
                (unless (stringp value)
                  (error "Tool %s has non-string :description form"
                         tool-id))
                (setq hit
                      (list :file (abbreviate-file-name file)
                            :absolute-file file
                            :start-offset start
                            :end-offset end
                            :start-line start-line
                            :end-line end-line
                            :old-description value
                            :old-literal
                            (buffer-substring-no-properties
                             start end)))))))))
    hit))

(defun anvil-autoresearch--string-literal (value)
  "Return VALUE as an Elisp string literal."
  (prin1-to-string value))

(defun anvil-autoresearch--relative-repo-file (file)
  "Return FILE relative to the anvil module directory."
  (file-relative-name file (anvil-autoresearch--module-directory)))

(defun anvil-autoresearch--unified-replacement
    (file start-line old-literal new-literal)
  "Return a small unified-diff style replacement hunk."
  (let* ((rel (anvil-autoresearch--relative-repo-file file))
         (old-lines (split-string old-literal "\n"))
         (new-lines (split-string new-literal "\n"))
         (old-count (length old-lines))
         (new-count (length new-lines)))
    (concat
     (format "--- a/%s\n+++ b/%s\n" rel rel)
     (format "@@ -%d,%d +%d,%d @@\n"
             start-line old-count start-line new-count)
     (mapconcat (lambda (line) (concat "-" line)) old-lines "\n")
     "\n"
     (mapconcat (lambda (line) (concat "+" line)) new-lines "\n")
     "\n")))

(defun anvil-autoresearch--draft-description
    (tool-id tool issues params)
  "Return a deterministic draft description for TOOL-ID."
  (let* ((raw-intent (or (plist-get tool :intent) '(general)))
         (intent (if (and (consp raw-intent)
                          (eq (car raw-intent) 'quote))
                     (cadr raw-intent)
                   raw-intent))
         (intent-list (if (listp intent) intent (list intent)))
         (layer (or (plist-get tool :layer) 'core))
         (read-only (plist-get tool :read-only))
         (param-names (mapcar (lambda (p) (plist-get p :name)) params)))
    (string-join
     (delq nil
           (list
            (format "Use `%s' when a caller needs the %s/%s workflow this MCP tool exposes."
                    tool-id
                    (mapconcat #'symbol-name intent-list ",")
                    layer)
            (when param-names
              (format "Parameters: %s."
                      (mapconcat #'identity param-names ", ")))
            (if read-only
                "Read-only; returns a compact result for follow-up."
              "May mutate state; state the expected side effect before calling.")
            (when issues
              (format "Rewrite should address: %s."
                      (mapconcat #'identity issues ", ")))))
     " ")))

(defun anvil-autoresearch--rewrite-prompt
    (tool-id server-id description issues params)
  "Return a prompt for a future docstring rewrite step."
  (string-join
   (list
    (format "Rewrite the MCP :description for tool `%s' on server `%s'."
            tool-id server-id)
    "Keep the rewrite factual and concise, but include when to use the tool, what it returns, and any important read/write side effect."
    "Do not change handler code or schema in this step."
    (format "Current description:\n%s" (or description ""))
    (format "Issues to fix: %s"
            (if issues (mapconcat #'identity issues ", ") "none"))
    (format "Parameters:\n%s"
            (if params
                (mapconcat
                 (lambda (p)
                   (format "- %s (%s): %s"
                           (plist-get p :name)
                           (plist-get p :type)
                           (plist-get p :description)))
                 params
                 "\n")
              "- none")))
   "\n\n"))

(defun anvil-autoresearch-docstring-plan (&optional tool-id server-id)
  "Return a read-only rewrite plan for one MCP tool description.

MCP Parameters:
  tool-id   - Optional MCP tool id.  When omitted, the highest-scored
              candidate from `anvil-autoresearch-docstrings' is used.
  server-id - Optional MCP server id.  Required only when TOOL-ID is
              ambiguous across servers.

This is Doc 43 Phase 1b planning only.  It returns the current
description, issues, registration-site hint, deterministic draft
description, and rewrite prompt.  It does not edit files, call an LLM,
or run tests."
  (let* ((selected
          (if (anvil-autoresearch--blank-string-p tool-id)
              (let* ((ranked (anvil-autoresearch-docstrings
                              nil server-id 1))
                     (candidates (append (plist-get ranked :candidates)
                                         nil)))
                (unless candidates
                  (error "No docstring rewrite candidates found"))
                (car candidates))
            (list :id tool-id :server-id (or server-id ""))))
         (id (plist-get selected :id))
         (sid (plist-get selected :server-id))
         (row (anvil-autoresearch--find-tool id sid))
         (tool (plist-get row :tool))
         (description (or (plist-get tool :description) ""))
         (score+issues (anvil-autoresearch--docstring-issues id tool))
         (score (car score+issues))
         (issues (cdr score+issues))
         (handler (plist-get tool :handler))
         (params (anvil-autoresearch--schema-params tool))
         (registration (anvil-autoresearch--find-registration-site id))
         (draft (anvil-autoresearch--draft-description
                 id tool issues params))
         (prompt (anvil-autoresearch--rewrite-prompt
                  id (plist-get row :server-id)
                  description issues params)))
    (list :server-id (plist-get row :server-id)
          :id id
          :handler (if (symbolp handler)
                       (symbol-name handler)
                     (format "%S" handler))
          :score score
          :issues (apply #'vector issues)
          :current-description description
          :params (apply #'vector params)
          :registration-site registration
          :draft-description draft
          :rewrite-prompt prompt
          :suggested-action "apply-description-rewrite")))

(defun anvil-autoresearch--tool-docstring-plan
    (&optional tool_id server_id)
  "MCP wrapper for `anvil-autoresearch-docstring-plan'.

MCP Parameters:
  tool_id   - Optional MCP tool id.  Omit to plan for the current top
              `autoresearch-docstrings' candidate.
  server_id - Optional MCP server id to disambiguate or restrict."
  (anvil-autoresearch-docstring-plan tool_id server_id))

(defun anvil-autoresearch-docstring-patch
    (&optional tool-id server-id new-description)
  "Return a dry-run patch candidate for one MCP tool description.

MCP Parameters:
  tool-id         - Optional MCP tool id.  When omitted, uses the same
                    top-candidate selection as
                    `anvil-autoresearch-docstring-plan'.
  server-id       - Optional MCP server id to disambiguate or restrict.
  new-description - Optional replacement description.  Empty / nil uses
                    the deterministic draft from the planner.

The returned patch is a candidate only.  This function does not write
the file, call an LLM, or run tests."
  (let* ((plan (anvil-autoresearch-docstring-plan tool-id server-id))
         (id (plist-get plan :id))
         (site (anvil-autoresearch--find-description-site id)))
    (unless site
      (error "No string :description site found for tool %s" id))
    (let* ((replacement
            (if (anvil-autoresearch--blank-string-p new-description)
                (plist-get plan :draft-description)
              new-description))
           (new-literal (anvil-autoresearch--string-literal replacement))
           (absolute-file (plist-get site :absolute-file))
           (patch
            (anvil-autoresearch--unified-replacement
             absolute-file
             (plist-get site :start-line)
             (plist-get site :old-literal)
             new-literal)))
      (list :dry-run t
            :server-id (plist-get plan :server-id)
            :id id
            :file (plist-get site :file)
            :start-line (plist-get site :start-line)
            :end-line (plist-get site :end-line)
            :old-description (plist-get site :old-description)
            :new-description replacement
            :old-literal (plist-get site :old-literal)
            :new-literal new-literal
            :patch patch
            :plan plan
            :suggested-action "review-dry-run-patch"))))

(defun anvil-autoresearch--tool-docstring-patch
    (&optional tool_id server_id new_description)
  "MCP wrapper for `anvil-autoresearch-docstring-patch'.

MCP Parameters:
  tool_id         - Optional MCP tool id.  Omit to patch-plan the
                    current top `autoresearch-docstrings' candidate.
  server_id       - Optional MCP server id to disambiguate or restrict.
  new_description - Optional replacement description.  Empty / nil uses
                    the deterministic planner draft."
  (anvil-autoresearch-docstring-patch
   tool_id server_id new_description))


;;;; --- docstring apply/eval loop -----------------------------------------

(defun anvil-autoresearch--coerce-bool (value)
  "Return non-nil when VALUE is truthy."
  (cond
   ((null value) nil)
   ((eq value t) t)
   ((stringp value)
    (not (member (downcase (string-trim value))
                 '("" "0" "false" "nil" "no" "off"))))
   (t t)))

(defun anvil-autoresearch--validate-description (description)
  "Signal when DESCRIPTION is not safe as an MCP description rewrite."
  (unless (and (stringp description)
               (not (string-empty-p (string-trim description))))
    (error "new_description must be a non-empty string"))
  (when (> (length description) 1200)
    (error "new_description is too long (>1200 chars)"))
  description)

(defun anvil-autoresearch--replace-region-in-file
    (file start-offset end-offset expected replacement)
  "Replace FILE region START-OFFSET..END-OFFSET with REPLACEMENT.
EXPECTED must match the current region exactly.  Returns the original
file content so callers can revert after a failed eval."
  (let ((content (with-temp-buffer
                   (insert-file-contents-literally file)
                   (buffer-string))))
    (unless (and (<= 1 start-offset)
                 (<= start-offset end-offset)
                 (<= end-offset (1+ (length content))))
      (error "replacement bounds are outside file"))
    (let ((current (substring content (1- start-offset) (1- end-offset))))
      (unless (string= current expected)
        (error "description site is stale; current source no longer matches"))
      (with-temp-file file
        (insert (substring content 0 (1- start-offset)))
        (insert replacement)
        (insert (substring content (1- end-offset))))
      content)))

(defun anvil-autoresearch--restore-file (file content)
  "Restore FILE to CONTENT."
  (with-temp-file file
    (insert content)))

(defun anvil-autoresearch--run-eval-command (command)
  "Run shell COMMAND and return a compact result plist."
  (if (anvil-autoresearch--blank-string-p command)
      (list :status "skipped"
            :exit-code nil
            :command "")
    (let ((outbuf (generate-new-buffer " *anvil-autoresearch-eval*"))
          (errfile (make-temp-file "anvil-autoresearch-eval-stderr-"))
          exit stdout stderr)
      (unwind-protect
          (progn
            (setq exit (call-process-shell-command
                        command nil (list outbuf errfile) nil))
            (setq stdout
                  (with-current-buffer outbuf
                    (buffer-substring-no-properties (point-min) (point-max))))
            (setq stderr
                  (with-temp-buffer
                    (insert-file-contents-literally errfile)
                    (buffer-string)))
            (list :status (if (zerop exit) "passed" "failed")
                  :exit-code exit
                  :command command
                  :stdout (truncate-string-to-width stdout 4000 nil nil t)
                  :stderr (truncate-string-to-width stderr 4000 nil nil t)))
        (when (buffer-live-p outbuf)
          (kill-buffer outbuf))
        (ignore-errors (delete-file errfile))))))

(defun anvil-autoresearch--docstring-score-description
    (tool-id tool description)
  "Return issue score plist for TOOL-ID if TOOL used DESCRIPTION."
  (let* ((tool* (copy-sequence tool))
         (_ (setq tool* (plist-put tool* :description description)))
         (score+issues (anvil-autoresearch--docstring-issues tool-id tool*)))
    (list :score (car score+issues)
          :issues (apply #'vector (cdr score+issues)))))

(defun anvil-autoresearch-docstring-apply
    (&optional tool-id server-id new-description eval-command
               keep-on-eval-failure holdout-command)
  "Apply one MCP tool description rewrite and optionally evaluate it.

MCP Parameters:
  tool-id              - Optional MCP tool id.  When omitted, uses the
                         same top-candidate selection as
                         `anvil-autoresearch-docstring-plan'.
  server-id            - Optional MCP server id to disambiguate or
                         restrict.
  new-description      - Optional replacement description.  Empty /
                         nil uses the deterministic planner draft.
  eval-command         - Optional shell command run after applying the
                         change.  A non-zero exit reverts the file
                         unless KEEP-ON-EVAL-FAILURE is truthy.
  keep-on-eval-failure - Optional truthy flag.  Defaults to nil.
  holdout-command      - Optional shell command run after a kept inner
                         eval.  A non-zero exit always reverts the
                         file as a retroactive holdout gate.

This is the Doc 43 local apply/eval/holdout loop for Q1-A docstring
autoresearch.  It edits exactly one registered `:description' string
literal after checking stale bounds.  If EVAL-COMMAND fails, the file
is restored to its original content before returning unless
KEEP-ON-EVAL-FAILURE is truthy.  If HOLDOUT-COMMAND fails after the
inner gate kept the change, the file is restored regardless."
  (let* ((patch (anvil-autoresearch-docstring-patch
                 tool-id server-id new-description))
         (site (anvil-autoresearch--find-description-site
                (plist-get patch :id)))
         (file (plist-get site :absolute-file))
         (replacement (anvil-autoresearch--validate-description
                       (plist-get patch :new-description)))
         (new-literal (anvil-autoresearch--string-literal replacement))
         (keep-failed (anvil-autoresearch--coerce-bool
                       keep-on-eval-failure))
         original eval-result holdout-result kept reverted revert-reason)
    (setq original
          (anvil-autoresearch--replace-region-in-file
           file
           (plist-get site :start-offset)
           (plist-get site :end-offset)
           (plist-get site :old-literal)
           new-literal))
    (setq eval-result (anvil-autoresearch--run-eval-command eval-command))
    (setq kept (not (and (string= (plist-get eval-result :status) "failed")
                         (not keep-failed))))
    (unless kept
      (setq revert-reason "eval-failed")
      (anvil-autoresearch--restore-file file original)
      (setq reverted t))
    (when kept
      (setq holdout-result
            (anvil-autoresearch--run-eval-command holdout-command))
      (when (string= (plist-get holdout-result :status) "failed")
        (setq kept nil
              reverted t
              revert-reason "holdout-failed")
        (anvil-autoresearch--restore-file file original)))
    (unless holdout-result
      (setq holdout-result (list :status "skipped"
                                 :exit-code nil
                                 :command "")))
    (append
     (list :dry-run nil
           :server-id (plist-get patch :server-id)
           :id (plist-get patch :id)
           :file (plist-get patch :file)
           :start-line (plist-get patch :start-line)
           :end-line (plist-get patch :end-line)
           :old-description (plist-get patch :old-description)
           :new-description replacement
           :eval eval-result
           :holdout holdout-result
           :kept kept
           :reverted (and reverted t)
           :revert-reason revert-reason
           :suggested-action
           (if kept "keep-docstring-rewrite"
             "reverted-docstring-rewrite"))
     (list :patch (plist-get patch :patch)))))

(defun anvil-autoresearch--tool-docstring-apply
    (&optional tool_id server_id new_description eval_command
              keep_on_eval_failure holdout_command)
  "MCP wrapper for `anvil-autoresearch-docstring-apply'.

MCP Parameters:
  tool_id              - Optional MCP tool id.  Omit to select the
                         current top `autoresearch-docstrings'
                         candidate.
  server_id            - Optional MCP server id to disambiguate or
                         restrict.
  new_description      - Optional replacement description.  Empty /
                         nil uses the deterministic planner draft.
  eval_command         - Optional shell command run after applying the
                         rewrite.
  keep_on_eval_failure - Optional truthy flag.  When nil, a failing
                         eval command reverts the file.
  holdout_command      - Optional shell command run after a kept inner
                         eval.  A failing holdout command always
                         reverts the file."
  (anvil-autoresearch-docstring-apply
   tool_id server_id new_description eval_command keep_on_eval_failure
   holdout_command))


;;;; --- docstring multi-candidate run -------------------------------------

(defun anvil-autoresearch--coerce-max-candidates (value)
  "Return VALUE as a bounded positive candidate count."
  (let ((n (truncate
            (anvil-autoresearch--coerce-number
             value
             anvil-autoresearch-docstring-run-max-candidates
             "max-candidates"))))
    (max 1 (min 50 n))))

(defun anvil-autoresearch--coerce-proposal-count (value)
  "Return VALUE as a bounded positive proposal task count."
  (let ((n (truncate
            (anvil-autoresearch--coerce-number
             value
             anvil-autoresearch-docstring-proposal-batch-count
             "count"))))
    (max 1 (min 20 n))))

(defun anvil-autoresearch--candidate-description (entry)
  "Return the description string from JSON candidate ENTRY."
  (cond
   ((stringp entry) entry)
   ((listp entry)
    (or (alist-get 'new_description entry)
        (alist-get 'new-description entry)
        (alist-get 'description entry)
        (alist-get "new_description" entry nil nil #'equal)
        (alist-get "new-description" entry nil nil #'equal)
        (alist-get "description" entry nil nil #'equal)))
   (t nil)))

(defun anvil-autoresearch--candidate-label (entry index)
  "Return a stable label for JSON candidate ENTRY at INDEX."
  (or (and (listp entry)
           (or (alist-get 'label entry)
               (alist-get "label" entry nil nil #'equal)))
      (format "candidate-%d" index)))

(defun anvil-autoresearch--docstring-candidates
    (candidates-json plan max-candidates)
  "Return normalized docstring candidates from CANDIDATES-JSON."
  (let ((entries
         (if (anvil-autoresearch--blank-string-p candidates-json)
             (list `((label . "draft")
                     (description . ,(plist-get plan :draft-description))))
           (let ((decoded (json-parse-string candidates-json
                                             :array-type 'list
                                             :object-type 'alist)))
             (unless (listp decoded)
               (error "candidates_json must be a JSON array"))
             decoded)))
        (index 0)
        candidates)
    (dolist (entry entries)
      (cl-incf index)
      (let ((description (anvil-autoresearch--candidate-description entry)))
        (unless (stringp description)
          (error "candidate %d must be a string or object with description"
                 index))
        (push (list :label (anvil-autoresearch--candidate-label entry index)
                    :description
                    (anvil-autoresearch--validate-description description))
              candidates)))
    (setq candidates (nreverse candidates))
    (cl-subseq candidates 0 (min (length candidates) max-candidates))))

(defun anvil-autoresearch--attempt-better-p (attempt best)
  "Return non-nil when ATTEMPT is a better passing result than BEST."
  (and (plist-get attempt :passed)
       (or (null best)
           (< (plist-get attempt :post-score)
              (plist-get best :post-score))
           (and (= (plist-get attempt :post-score)
                   (plist-get best :post-score))
                (< (plist-get attempt :index)
                   (plist-get best :index))))))

(defun anvil-autoresearch-docstring-run
    (&optional tool-id server-id candidates-json eval-command holdout-command
               max-candidates)
  "Try multiple MCP description rewrites and keep the best passing one.

MCP Parameters:
  tool-id         - Optional MCP tool id.  When omitted, uses the
                    current top `autoresearch-docstrings' candidate.
  server-id       - Optional MCP server id to disambiguate or restrict.
  candidates-json - Optional JSON array of candidate descriptions.
                    Each entry may be a string, or an object with
                    `description' / `new_description' and optional
                    `label'.  Empty / nil uses the deterministic draft.
  eval-command    - Optional shell command run for each candidate after
                    applying it.  Non-zero exit rejects that candidate.
  holdout-command - Optional shell command run after a candidate passes
                    the inner eval.  Non-zero exit rejects it.
  max-candidates  - Optional cap on tried candidates.  Defaults to
                    `anvil-autoresearch-docstring-run-max-candidates'.

Each candidate is applied to the same original source snapshot, then
the file is restored before trying the next one.  Passing candidates
are ranked by the remaining docstring issue score; the best candidate
is applied once at the end.  If no candidate passes, the original file
is restored and the result reports `:kept nil'."
  (let* ((plan (anvil-autoresearch-docstring-plan tool-id server-id))
         (id (plist-get plan :id))
         (sid (plist-get plan :server-id))
         (row (anvil-autoresearch--find-tool id sid))
         (tool (plist-get row :tool))
         (site (anvil-autoresearch--find-description-site id)))
    (unless site
      (error "No string :description site found for tool %s" id))
    (let* ((file (plist-get site :absolute-file))
           (max* (anvil-autoresearch--coerce-max-candidates max-candidates))
           (candidates
            (anvil-autoresearch--docstring-candidates
             candidates-json plan max*))
           (original (with-temp-buffer
                       (insert-file-contents-literally file)
                       (buffer-string)))
           attempts best)
      (cl-loop for candidate in candidates
               for index from 1
               do
               (let* ((description (plist-get candidate :description))
                      (new-literal
                       (anvil-autoresearch--string-literal description))
                      eval-result holdout-result attempt)
                 (condition-case err
                     (progn
                       (anvil-autoresearch--replace-region-in-file
                        file
                        (plist-get site :start-offset)
                        (plist-get site :end-offset)
                        (plist-get site :old-literal)
                        new-literal)
                       (setq eval-result
                             (anvil-autoresearch--run-eval-command
                              eval-command))
                       (if (string= (plist-get eval-result :status)
                                    "failed")
                           (setq attempt
                                 (list :index index
                                       :label (plist-get candidate :label)
                                       :description description
                                       :passed nil
                                       :reject-reason "eval-failed"
                                       :eval eval-result
                                       :holdout
                                       (list :status "skipped"
                                             :exit-code nil
                                             :command "")))
                         (setq holdout-result
                               (anvil-autoresearch--run-eval-command
                                holdout-command))
                         (if (string= (plist-get holdout-result :status)
                                      "failed")
                             (setq attempt
                                   (list :index index
                                         :label (plist-get candidate :label)
                                         :description description
                                         :passed nil
                                         :reject-reason "holdout-failed"
                                         :eval eval-result
                                         :holdout holdout-result))
                           (let ((score
                                  (anvil-autoresearch--docstring-score-description
                                   id tool description)))
                             (setq attempt
                                   (append
                                    (list :index index
                                          :label
                                          (plist-get candidate :label)
                                          :description description
                                          :passed t
                                          :reject-reason nil
                                          :eval eval-result
                                          :holdout holdout-result
                                          :post-score
                                          (plist-get score :score))
                                    (list :post-issues
                                          (plist-get score :issues))))))))
                   (error
                    (setq attempt
                          (list :index index
                                :label (plist-get candidate :label)
                                :description description
                                :passed nil
                                :reject-reason "error"
                                :error (error-message-string err)
                                :eval
                                (list :status "skipped"
                                      :exit-code nil
                                      :command "")
                                :holdout
                                (list :status "skipped"
                                      :exit-code nil
                                      :command "")))))
                 (push attempt attempts)
                 (when (anvil-autoresearch--attempt-better-p attempt best)
                   (setq best attempt))
                 (anvil-autoresearch--restore-file file original)))
      (if best
          (progn
            (anvil-autoresearch--replace-region-in-file
             file
             (plist-get site :start-offset)
             (plist-get site :end-offset)
             (plist-get site :old-literal)
             (anvil-autoresearch--string-literal
              (plist-get best :description)))
            (list :server-id sid
                  :id id
                  :file (plist-get site :file)
                  :candidate-count (length candidates)
                  :attempts (apply #'vector (nreverse attempts))
                  :selected best
                  :kept t
                  :reverted nil
                  :suggested-action "kept-best-docstring-candidate"))
        (anvil-autoresearch--restore-file file original)
        (list :server-id sid
              :id id
              :file (plist-get site :file)
              :candidate-count (length candidates)
              :attempts (apply #'vector (nreverse attempts))
              :selected nil
              :kept nil
              :reverted t
              :suggested-action "no-docstring-candidate-passed")))))

(defun anvil-autoresearch--tool-docstring-run
    (&optional tool_id server_id candidates_json eval_command holdout_command
              max_candidates)
  "MCP wrapper for `anvil-autoresearch-docstring-run'.

MCP Parameters:
  tool_id         - Optional MCP tool id.
  server_id       - Optional MCP server id.
  candidates_json - Optional JSON array of candidate descriptions.
  eval_command    - Optional shell command for the inner eval gate.
  holdout_command - Optional shell command for the holdout gate.
  max_candidates  - Optional maximum number of candidates to try."
  (anvil-autoresearch-docstring-run
   tool_id server_id candidates_json eval_command holdout_command
   max_candidates))


;;;; --- docstring orchestrator proposal -----------------------------------

(defun anvil-autoresearch--ensure-orchestrator ()
  "Ensure `anvil-orchestrator-submit' is available."
  (unless (or (fboundp 'anvil-orchestrator-submit)
              (require 'anvil-orchestrator nil t))
    (error "anvil-orchestrator is required for proposal submission"))
  (unless (fboundp 'anvil-orchestrator-submit)
    (error "anvil-orchestrator-submit is unavailable")))

(defun anvil-autoresearch--ensure-orchestrator-collect ()
  "Ensure `anvil-orchestrator-collect' is available."
  (unless (or (fboundp 'anvil-orchestrator-collect)
              (require 'anvil-orchestrator nil t))
    (error "anvil-orchestrator is required for proposal harvest"))
  (unless (fboundp 'anvil-orchestrator-collect)
    (error "anvil-orchestrator-collect is unavailable")))

(defun anvil-autoresearch--docstring-proposal-prompt
    (plan &optional index count)
  "Return a strict proposal prompt from docstring rewrite PLAN.
When INDEX and COUNT are non-nil, add variation guidance for one
candidate inside a multi-proposal batch."
  (string-join
   (delq
    nil
    (list
     "You are running Doc 43 autoresearch Phase 1e for anvil.el."
     "Task: propose one improved MCP :description string for the target tool."
     (when (and index count)
       (format
        "This is candidate %d of %d; make it independently useful and avoid copying other candidates verbatim."
        index count))
     "Do not edit files, run commands, or change schemas in this proposal step."
     "Output only the replacement description text.  No markdown, no bullets, no explanation."
     "The description must be factual, concise, and include when to use the tool, what it returns, and important read/write side effects."
     (format "Tool id: %s" (plist-get plan :id))
     (format "Server id: %s" (plist-get plan :server-id))
     (format "Current description:\n%s"
             (plist-get plan :current-description))
     (format "Issues to fix: %s"
             (mapconcat #'identity
                        (append (plist-get plan :issues) nil)
                        ", "))
     (format "Parameters:\n%s"
             (let ((params (append (plist-get plan :params) nil)))
               (if params
                   (mapconcat
                    (lambda (p)
                      (format "- %s (%s): %s"
                              (plist-get p :name)
                              (plist-get p :type)
                              (plist-get p :description)))
                    params
                    "\n")
                 "- none")))
     (format "Deterministic draft for reference:\n%s"
             (plist-get plan :draft-description))))
   "\n\n"))

(defun anvil-autoresearch--docstring-proposal-task
    (plan provider model budget timeout heartbeat cwd sandbox
          no-worktree worktree-name &optional index count)
  "Return one orchestrator task plist for PLAN.
INDEX and COUNT identify one slot in a multi-proposal batch."
  (let* ((id (plist-get plan :id))
         (batch-slot (and index count))
         (name (if batch-slot
                   (format "autoresearch-docstring-%s-%02d-of-%02d"
                           id index count)
                 (format "autoresearch-docstring-%s" id))))
    (append
     (list :name name
           :provider provider
           :prompt (anvil-autoresearch--docstring-proposal-prompt
                    plan index count)
           :cwd cwd
           :budget-usd budget
           :timeout-sec timeout)
     (when heartbeat
       (list :heartbeat-timeout-sec heartbeat))
     (when model
       (list :model model))
     (when sandbox
       (list :sandbox sandbox))
     (when no-worktree
       (list :no-worktree t))
     (when (and worktree-name (not no-worktree))
       (list :worktree-name worktree-name)))))

(defun anvil-autoresearch-docstring-propose
    (&optional tool-id server-id provider model budget-usd timeout-sec
               heartbeat-timeout-sec cwd sandbox no-worktree worktree-name)
  "Submit one orchestrator task to propose an MCP description rewrite.

MCP Parameters:
  tool-id               - Optional MCP tool id.  When omitted, uses the
                          same top-candidate selection as
                          `anvil-autoresearch-docstring-plan'.
  server-id             - Optional MCP server id to disambiguate or
                          restrict.
  provider              - Optional orchestrator provider.  Defaults to
                          `anvil-autoresearch-docstring-proposal-provider'.
  model                 - Optional provider model string.
  budget-usd            - Optional per-task budget.  Defaults to
                          `anvil-autoresearch-docstring-proposal-budget-usd'.
  timeout-sec           - Optional wall-clock timeout.  Defaults to
                          `anvil-autoresearch-docstring-proposal-timeout-sec'.
  heartbeat-timeout-sec - Optional no-output timeout.  Defaults to the
                          docstring proposal heartbeat defcustom.
  cwd                   - Optional repo/worktree directory.  Defaults to
                          this anvil.el repository root.
  sandbox               - Optional provider sandbox, used by Codex.
  no-worktree           - Optional truthy flag.  When truthy, pass
                          `:no-worktree t' to the orchestrator task.
  worktree-name         - Optional worktree name hint for providers /
                          orchestrator paths that support it.

This is the Doc 43 Phase 1e proposal entrance.  It does not apply a
patch itself; it submits a bounded orchestrator task with budget,
timeout, heartbeat timeout, cwd, sandbox, and optional worktree
metadata, then returns the batch id plus the exact task plist submitted."
  (anvil-autoresearch--ensure-orchestrator)
  (let* ((plan (anvil-autoresearch-docstring-plan tool-id server-id))
         (provider* (anvil-autoresearch--coerce-provider
                     provider
                     anvil-autoresearch-docstring-proposal-provider))
         (model* (anvil-autoresearch--coerce-string
                  model
                  anvil-autoresearch-docstring-proposal-model))
         (budget* (anvil-autoresearch--coerce-number
                   budget-usd
                   anvil-autoresearch-docstring-proposal-budget-usd
                   "budget-usd"))
         (timeout* (anvil-autoresearch--coerce-number
                    timeout-sec
                    anvil-autoresearch-docstring-proposal-timeout-sec
                    "timeout-sec"))
         (heartbeat* (anvil-autoresearch--coerce-number
                      heartbeat-timeout-sec
                      anvil-autoresearch-docstring-proposal-heartbeat-timeout-sec
                      "heartbeat-timeout-sec"))
         (cwd* (anvil-autoresearch--repo-root cwd))
         (sandbox* (anvil-autoresearch--coerce-string
                    sandbox
                    anvil-autoresearch-docstring-proposal-sandbox))
         (no-worktree* (anvil-autoresearch--coerce-bool no-worktree))
         (worktree-name* (anvil-autoresearch--coerce-string
                          worktree-name nil))
         (task (anvil-autoresearch--docstring-proposal-task
                plan provider* model* budget* timeout* heartbeat* cwd*
                sandbox* no-worktree* worktree-name*))
         (batch-id (anvil-orchestrator-submit (list task))))
    (list :submitted t
          :batch-id batch-id
          :task task
          :plan plan
          :suggested-action
          "poll-orchestrator-batch-then-review-description")))

(defun anvil-autoresearch--tool-docstring-propose
    (&optional tool_id server_id provider model budget_usd timeout_sec
              heartbeat_timeout_sec cwd sandbox no_worktree worktree_name)
  "MCP wrapper for `anvil-autoresearch-docstring-propose'.

MCP Parameters:
  tool_id               - Optional MCP tool id.
  server_id             - Optional MCP server id.
  provider              - Optional orchestrator provider string.
  model                 - Optional provider model string.
  budget_usd            - Optional per-task budget.
  timeout_sec           - Optional wall-clock timeout.
  heartbeat_timeout_sec - Optional no-output heartbeat timeout.
  cwd                   - Optional repo/worktree directory.
  sandbox               - Optional provider sandbox string.
  no_worktree           - Optional truthy worktree opt-out flag.
  worktree_name         - Optional explicit worktree name hint."
  (anvil-autoresearch-docstring-propose
   tool_id server_id provider model budget_usd timeout_sec
   heartbeat_timeout_sec cwd sandbox no_worktree worktree_name))

(defun anvil-autoresearch-docstring-propose-batch
    (&optional tool-id server-id count provider model budget-usd timeout-sec
               heartbeat-timeout-sec cwd sandbox no-worktree worktree-name)
  "Submit multiple orchestrator proposal tasks for one MCP description.

MCP Parameters:
  tool-id               - Optional MCP tool id.  When omitted, uses the
                          same top-candidate selection as
                          `anvil-autoresearch-docstring-plan'.
  server-id             - Optional MCP server id to disambiguate or
                          restrict.
  count                 - Optional number of proposal tasks.  Defaults
                          to the proposal batch count defcustom.
  provider              - Optional orchestrator provider.  Defaults to
                          `anvil-autoresearch-docstring-proposal-provider'.
  model                 - Optional provider model string.
  budget-usd            - Optional per-task budget.
  timeout-sec           - Optional wall-clock timeout per task.
  heartbeat-timeout-sec - Optional no-output timeout per task.
  cwd                   - Optional repo/worktree directory.
  sandbox               - Optional provider sandbox, used by Codex.
  no-worktree           - Optional truthy flag.  When truthy, every
                          task carries `:no-worktree t'.
  worktree-name         - Optional base worktree name hint.  With
                          COUNT > 1, `-NN-of-MM' is appended.

This is the Doc 43 proposal fan-out helper.  It submits COUNT bounded
proposal tasks in one orchestrator batch, with unique task names and a
candidate-number hint in each prompt.  It carries optional worktree
metadata into each task.  It does not apply patches; pass the returned
`:batch-id' to `autoresearch-docstring-harvest' to collect and evaluate
the proposals."
  (anvil-autoresearch--ensure-orchestrator)
  (let* ((plan (anvil-autoresearch-docstring-plan tool-id server-id))
         (count* (anvil-autoresearch--coerce-proposal-count count))
         (provider* (anvil-autoresearch--coerce-provider
                     provider
                     anvil-autoresearch-docstring-proposal-provider))
         (model* (anvil-autoresearch--coerce-string
                  model
                  anvil-autoresearch-docstring-proposal-model))
         (budget* (anvil-autoresearch--coerce-number
                   budget-usd
                   anvil-autoresearch-docstring-proposal-budget-usd
                   "budget-usd"))
         (timeout* (anvil-autoresearch--coerce-number
                    timeout-sec
                    anvil-autoresearch-docstring-proposal-timeout-sec
                    "timeout-sec"))
         (heartbeat* (anvil-autoresearch--coerce-number
                      heartbeat-timeout-sec
                      anvil-autoresearch-docstring-proposal-heartbeat-timeout-sec
                      "heartbeat-timeout-sec"))
         (cwd* (anvil-autoresearch--repo-root cwd))
         (sandbox* (anvil-autoresearch--coerce-string
                    sandbox
                    anvil-autoresearch-docstring-proposal-sandbox))
         (no-worktree* (anvil-autoresearch--coerce-bool no-worktree))
         (base-worktree-name (anvil-autoresearch--coerce-string
                              worktree-name nil))
         (tasks
          (cl-loop for index from 1 to count*
                   for wt-name = (and base-worktree-name
                                      (format "%s-%02d-of-%02d"
                                              base-worktree-name
                                              index count*))
                   collect
                   (anvil-autoresearch--docstring-proposal-task
                    plan provider* model* budget* timeout* heartbeat*
                    cwd* sandbox* no-worktree* wt-name index count*)))
         (batch-id (anvil-orchestrator-submit tasks)))
    (list :submitted t
          :batch-id batch-id
          :count count*
          :tasks (apply #'vector tasks)
          :plan plan
          :suggested-action
          "harvest-orchestrator-proposal-batch")))

(defun anvil-autoresearch--tool-docstring-propose-batch
    (&optional tool_id server_id count provider model budget_usd timeout_sec
              heartbeat_timeout_sec cwd sandbox no_worktree worktree_name)
  "MCP wrapper for `anvil-autoresearch-docstring-propose-batch'.

MCP Parameters:
  tool_id               - Optional MCP tool id.
  server_id             - Optional MCP server id.
  count                 - Optional number of proposal tasks.
  provider              - Optional orchestrator provider string.
  model                 - Optional provider model string.
  budget_usd            - Optional per-task budget.
  timeout_sec           - Optional wall-clock timeout per task.
  heartbeat_timeout_sec - Optional no-output heartbeat timeout.
  cwd                   - Optional repo/worktree directory.
  sandbox               - Optional provider sandbox string.
  no_worktree           - Optional truthy worktree opt-out flag.
  worktree_name         - Optional explicit worktree base name hint."
  (anvil-autoresearch-docstring-propose-batch
   tool_id server_id count provider model budget_usd timeout_sec
   heartbeat_timeout_sec cwd sandbox no_worktree worktree_name))


;;;; --- docstring proposal harvest ----------------------------------------

(defun anvil-autoresearch--clean-proposal-description (summary)
  "Return SUMMARY as a candidate description, or nil when unusable."
  (when (stringp summary)
    (let ((text (string-trim summary)))
      (when (string-match
             "\\`[ \t\n\r]*```[[:alnum:]_-]*[ \t\n\r]*\\(\\(?:.\\|\n\\)*?\\)[ \t\n\r]*```[ \t\n\r]*\\'"
             text)
        (setq text (string-trim (match-string 1 text))))
      (when (and (> (length text) 1)
                 (or (and (string-prefix-p "\"" text)
                          (string-suffix-p "\"" text))
                     (and (string-prefix-p "'" text)
                          (string-suffix-p "'" text))))
        (let ((read-value (ignore-errors (read text))))
          (when (stringp read-value)
            (setq text (string-trim read-value)))))
      (unless (string-empty-p text)
        text))))

(defun anvil-autoresearch--proposal-candidates (results max-candidates)
  "Return normalized candidate plists from orchestrator RESULTS."
  (let (candidates)
    (dolist (result results)
      (let ((description
             (and (eq (plist-get result :status) 'done)
                  (anvil-autoresearch--clean-proposal-description
                   (plist-get result :summary)))))
        (when description
          (push `((label . ,(or (plist-get result :name)
                                (plist-get result :id)
                                "proposal"))
                  (description . ,description))
                candidates))))
    (setq candidates (nreverse candidates))
    (cl-subseq candidates 0 (min (length candidates) max-candidates))))

(defun anvil-autoresearch-docstring-harvest
    (batch-id &optional tool-id server-id eval-command holdout-command wait
              max-candidates)
  "Harvest proposal batch results and run the local candidate loop.

MCP Parameters:
  batch-id        - Orchestrator batch id returned by
                    `autoresearch-docstring-propose' or a compatible
                    proposal batch.
  tool-id         - Optional MCP tool id.  When omitted, uses the
                    current top `autoresearch-docstrings' candidate.
  server-id       - Optional MCP server id to disambiguate or restrict.
  eval-command    - Optional shell command for the inner eval gate.
  holdout-command - Optional shell command for the holdout gate.
  wait            - Optional truthy flag.  When truthy, wait for the
                    orchestrator batch to become terminal before
                    collecting summaries.
  max-candidates  - Optional cap on harvested candidates.

Only successful orchestrator task summaries become candidate
descriptions.  If no usable summaries exist, this function does not
edit files and returns `:kept nil'.  Otherwise it delegates to
`anvil-autoresearch-docstring-run', which applies and keeps only the
best eval/holdout-passing description."
  (unless (and (stringp batch-id)
               (not (string-empty-p (string-trim batch-id))))
    (error "batch_id is required"))
  (anvil-autoresearch--ensure-orchestrator-collect)
  (let* ((max* (anvil-autoresearch--coerce-max-candidates max-candidates))
         (results (anvil-orchestrator-collect
                   batch-id
                   :wait (anvil-autoresearch--coerce-bool wait)))
         (candidates
          (anvil-autoresearch--proposal-candidates results max*)))
    (if (null candidates)
        (list :batch-id batch-id
              :candidate-count 0
              :results (apply #'vector results)
              :run nil
              :kept nil
              :reverted nil
              :suggested-action "no-proposal-candidates")
      (let* ((candidates-json (json-encode (apply #'vector candidates)))
             (run (anvil-autoresearch-docstring-run
                   tool-id server-id candidates-json eval-command
                   holdout-command max*)))
        (append
         (list :batch-id batch-id
               :candidate-count (length candidates)
               :results (apply #'vector results)
               :candidates (apply #'vector candidates)
               :run run)
         (list :kept (plist-get run :kept)
               :reverted (plist-get run :reverted)
               :suggested-action
               (if (plist-get run :kept)
                   "harvested-and-kept-best-docstring-candidate"
                 "harvested-no-docstring-candidate-kept")))))))

(defun anvil-autoresearch--tool-docstring-harvest
    (batch_id &optional tool_id server_id eval_command holdout_command wait
              max_candidates)
  "MCP wrapper for `anvil-autoresearch-docstring-harvest'.

MCP Parameters:
  batch_id        - Orchestrator proposal batch id.
  tool_id         - Optional MCP tool id.
  server_id       - Optional MCP server id.
  eval_command    - Optional shell command for the inner eval gate.
  holdout_command - Optional shell command for the holdout gate.
  wait            - Optional truthy flag to wait before collecting.
  max_candidates  - Optional maximum number of proposals to evaluate."
  (anvil-autoresearch-docstring-harvest
   batch_id tool_id server_id eval_command holdout_command wait
   max_candidates))


;;;; --- memory promotion ---------------------------------------------------

(defun anvil-autoresearch--ensure-memory ()
  "Ensure `anvil-memory-add' is available."
  (unless (or (fboundp 'anvil-memory-add)
              (require 'anvil-memory nil t))
    (error "anvil-memory is required for autoresearch memory promotion"))
  (unless (fboundp 'anvil-memory-add)
    (error "anvil-memory-add is unavailable")))

(defun anvil-autoresearch--json-field (object key)
  "Return KEY from decoded JSON OBJECT, accepting symbol/string keys."
  (or (and (listp object)
           (alist-get key object nil nil #'eq))
      (and (listp object)
           (alist-get (symbol-name key) object nil nil #'equal))
      (and (listp object)
           (alist-get (replace-regexp-in-string
                       "_" "-" (symbol-name key))
                      object nil nil #'equal))))

(defun anvil-autoresearch--parse-json-object (json name)
  "Parse JSON object string for NAME."
  (unless (and (stringp json)
               (not (string-empty-p (string-trim json))))
    (error "%s must be a non-empty JSON object string" name))
  (let ((decoded (json-parse-string json
                                    :array-type 'list
                                    :object-type 'alist
                                    :null-object nil
                                    :false-object nil)))
    (unless (listp decoded)
      (error "%s must parse to a JSON object" name))
    decoded))

(defun anvil-autoresearch--promote-run-object (object)
  "Return a normalized run object from decoded promotion OBJECT."
  (or (anvil-autoresearch--json-field object 'run)
      object))

(defun anvil-autoresearch--parse-tags-json (tags-json)
  "Parse optional TAGS-JSON into a string list."
  (if (anvil-autoresearch--blank-string-p tags-json)
      nil
    (let ((decoded (json-parse-string tags-json
                                      :array-type 'list
                                      :object-type 'alist)))
      (unless (and (listp decoded)
                   (cl-every #'stringp decoded))
        (error "tags_json must be a JSON array of strings"))
      decoded)))

(defun anvil-autoresearch--coerce-memory-type (value)
  "Return VALUE as an `anvil-memory-add' type symbol."
  (let ((type (anvil-autoresearch--coerce-provider value 'memo)))
    (unless (memq type '(user feedback project reference memo))
      (error "type must be one of user/feedback/project/reference/memo"))
    type))

(defun anvil-autoresearch--memory-name (run selected name)
  "Return memory NAME or a deterministic default from RUN and SELECTED."
  (or (anvil-autoresearch--coerce-string name nil)
      (format "autoresearch_docstring_%s_%s"
              (or (anvil-autoresearch--json-field run 'id)
                  (anvil-autoresearch--json-field selected 'label)
                  "candidate")
              (format-time-string "%Y%m%d_%H%M%S"))))

(defun anvil-autoresearch--memory-body (run selected)
  "Return an org-ish memory body for RUN and SELECTED."
  (let ((description (anvil-autoresearch--json-field selected 'description)))
    (string-join
     (delq
      nil
      (list
       "* Doc43 autoresearch accepted docstring"
       (format "- Server: %s"
               (or (anvil-autoresearch--json-field run 'server-id) ""))
       (format "- Tool: %s"
               (or (anvil-autoresearch--json-field run 'id) ""))
       (format "- Source file: %s"
               (or (anvil-autoresearch--json-field run 'file) ""))
       (format "- Selected label: %s"
               (or (anvil-autoresearch--json-field selected 'label) ""))
       (format "- Post score: %s"
               (or (anvil-autoresearch--json-field selected 'post-score)
                   ""))
       "** Accepted description"
       description))
     "\n")))

(defun anvil-autoresearch-docstring-promote-memory
    (run-json &optional name type memory-description tags-json)
  "Promote an accepted docstring autoresearch run into anvil-memory.

MCP Parameters:
  run-json           - JSON object returned by `autoresearch-docstring-run'
                       or `autoresearch-docstring-harvest'.  Harvest
                       results are accepted; their nested `run' object
                       is used automatically.
  name               - Optional memory basename.  Defaults to
                       autoresearch_docstring_<tool>_<timestamp>.
  type               - Optional memory type: user, feedback, project,
                       reference, or memo.  Defaults to memo.
  memory-description - Optional short memory description.  Defaults to
                       \"Doc43 accepted docstring: <tool>\".
  tags-json          - Optional JSON array of string tags.

The run must contain a selected candidate with a description.  This
function writes one DB-direct memory through `anvil-memory-add' and
does not edit source files."
  (anvil-autoresearch--ensure-memory)
  (let* ((object (anvil-autoresearch--parse-json-object run-json
                                                        "run_json"))
         (run (anvil-autoresearch--promote-run-object object))
         (selected (anvil-autoresearch--json-field run 'selected))
         (accepted (and selected
                        (anvil-autoresearch--json-field selected
                                                        'description))))
    (unless accepted
      (error "run_json does not contain a selected candidate description"))
    (let* ((type* (anvil-autoresearch--coerce-memory-type type))
           (name* (anvil-autoresearch--memory-name run selected name))
           (tool-id (or (anvil-autoresearch--json-field run 'id) "tool"))
           (memory-description*
            (or (anvil-autoresearch--coerce-string memory-description nil)
                (format "Doc43 accepted docstring: %s" tool-id)))
           (tags (or (anvil-autoresearch--parse-tags-json tags-json)
                     '("doc43" "autoresearch" "docstring")))
           (body (anvil-autoresearch--memory-body run selected))
           (row (anvil-memory-add name* type* body
                                  :description memory-description*
                                  :tags tags)))
      (list :promoted t
            :memory row
            :name name*
            :type type*
            :tool-id tool-id
            :selected-description accepted
            :suggested-action "review-or-export-promoted-memory"))))

(defun anvil-autoresearch--tool-docstring-promote-memory
    (run_json &optional name type memory_description tags_json)
  "MCP wrapper for `anvil-autoresearch-docstring-promote-memory'.

MCP Parameters:
  run_json           - JSON result from docstring run or harvest.
  name               - Optional memory basename.
  type               - Optional memory type.
  memory_description - Optional short memory description.
  tags_json          - Optional JSON array of string tags."
  (anvil-autoresearch-docstring-promote-memory
   run_json name type memory_description tags_json))


;;;; --- routing autoresearch ----------------------------------------------

(defun anvil-autoresearch--ensure-routing ()
  "Ensure `anvil-orchestrator-select-provider' is available."
  (unless (or (fboundp 'anvil-orchestrator-select-provider)
              (require 'anvil-orchestrator-routing nil t))
    (error "anvil-orchestrator-routing is required for routing autoresearch"))
  (unless (fboundp 'anvil-orchestrator-select-provider)
    (error "anvil-orchestrator-select-provider is unavailable")))

(defun anvil-autoresearch--parse-symbol-list (value default name)
  "Parse VALUE as a comma/space separated symbol list, or DEFAULT.
NAME is used in error messages."
  (cond
   ((anvil-autoresearch--blank-string-p value) default)
   ((listp value)
    (unless (cl-every #'symbolp value)
      (error "%s must be a symbol list" name))
    value)
   ((stringp value)
    (mapcar #'intern (split-string value "[ ,]+" t "[ \t\n\r]+")))
   (t (error "%s must be a string or symbol list" name))))

(defun anvil-autoresearch--routing-issues
    (decision min-samples)
  "Return issue strings for routing DECISION."
  (let (issues)
    (unless (plist-get decision :provider)
      (push "no-provider" issues))
    (when (plist-get decision :cold-start)
      (push "cold-start" issues))
    (unless (plist-get decision :per-candidate)
      (push "no-sampled-candidates" issues))
    (when (and min-samples
               (plist-get decision :per-candidate)
               (< (length (plist-get decision :per-candidate)) 2))
      (push "single-sampled-provider" issues))
    (nreverse issues)))

(defun anvil-autoresearch--routing-score (issues)
  "Return heuristic routing autoresearch SCORE for ISSUES."
  (let ((score 0))
    (dolist (issue issues)
      (cl-incf score
               (pcase issue
                 ("no-provider" 30)
                 ("cold-start" 20)
                 ("no-sampled-candidates" 15)
                 ("single-sampled-provider" 8)
                 (_ 1))))
    score))

(defun anvil-autoresearch-routing-evaluate
    (&optional prompt policies candidates since-sec min-samples)
  "Evaluate orchestrator routing decisions as Q1-B autoresearch targets.

MCP Parameters:
  prompt      - Optional representative prompt text for function
                policies and report context.
  policies    - Optional comma-separated policy names.  Defaults to
                `anvil-autoresearch-routing-policies'.
  candidates  - Optional comma-separated provider list.  Empty / nil
                lets routing consider every registered provider.
  since-sec   - Optional lookback window in seconds.  Empty / nil
                delegates the default to routing.
  min-samples - Optional per-provider sample floor.  Empty / nil
                delegates the default to routing.

This is read-only.  It calls `anvil-orchestrator-select-provider' for
each policy and scores weak routing states such as cold-start,
no-provider, and insufficient sampled candidates."
  (anvil-autoresearch--ensure-routing)
  (let* ((policies* (anvil-autoresearch--parse-symbol-list
                    policies
                    anvil-autoresearch-routing-policies
                    "policies"))
         (candidates* (anvil-autoresearch--parse-symbol-list
                       candidates nil "candidates"))
         (since* (and (not (anvil-autoresearch--blank-string-p since-sec))
                      (- (float-time)
                         (anvil-autoresearch--coerce-number
                          since-sec nil "since-sec"))))
         (min* (and (not (anvil-autoresearch--blank-string-p min-samples))
                    (truncate
                     (anvil-autoresearch--coerce-number
                      min-samples nil "min-samples"))))
         decisions)
    (dolist (policy policies*)
      (let* ((decision (anvil-orchestrator-select-provider
                        :prompt prompt
                        :policy policy
                        :candidates candidates*
                        :since since*
                        :min-samples min*))
             (issues (anvil-autoresearch--routing-issues decision min*))
             (score (anvil-autoresearch--routing-score issues)))
        (push (append
               (list :policy policy
                     :score score
                     :issues (apply #'vector issues))
               decision)
              decisions)))
    (setq decisions (nreverse decisions))
    (let* ((providers (delete-dups
                       (delq nil
                             (mapcar (lambda (d)
                                       (plist-get d :provider))
                                     decisions))))
           (disagreement (> (length providers) 1))
           (total-score (+ (if disagreement 10 0)
                           (apply #'+
                                  (mapcar (lambda (d)
                                            (plist-get d :score))
                                          decisions)))))
      (list :prompt (or prompt "")
            :policies (apply #'vector policies*)
            :candidates (apply #'vector (or candidates* '()))
            :since-sec (or since-sec "")
            :min-samples (or min-samples "")
            :policy-disagreement disagreement
            :score total-score
            :decisions (apply #'vector decisions)
            :suggested-action
            (if (> total-score 0)
                "plan-routing-policy-improvement"
              "routing-policy-looks-stable")))))

(defun anvil-autoresearch--routing-plan-prompt (evaluation)
  "Return a proposal prompt from routing EVALUATION."
  (string-join
   (list
    "You are running Doc 43 autoresearch Phase 3 for anvil.el."
    "Task: propose one small improvement to orchestrator provider routing."
    "Do not edit files in this proposal step. Output a concise plan only."
    "Focus on policy defaults, candidate sets, min-samples, cold-start behavior, or stats interpretation."
    (format "Prompt context:\n%s" (plist-get evaluation :prompt))
    (format "Score: %s" (plist-get evaluation :score))
    (format "Policy disagreement: %s"
            (plist-get evaluation :policy-disagreement))
    (format "Decisions:\n%s"
            (mapconcat
             (lambda (decision)
               (format "- policy=%s provider=%s score=%s issues=%s reason=%s"
                       (plist-get decision :policy)
                       (plist-get decision :provider)
                       (plist-get decision :score)
                       (mapconcat #'identity
                                  (append (plist-get decision :issues) nil)
                                  ",")
                       (plist-get decision :reason)))
             (append (plist-get evaluation :decisions) nil)
             "\n")))
   "\n\n"))

(defun anvil-autoresearch-routing-plan
    (&optional prompt policies candidates since-sec min-samples)
  "Return a read-only Q1-B routing improvement plan.

MCP Parameters:
  prompt      - Optional representative prompt text.
  policies    - Optional comma-separated policy names.
  candidates  - Optional comma-separated provider list.
  since-sec   - Optional lookback window in seconds.
  min-samples - Optional per-provider sample floor.

This does not submit tasks or mutate routing settings.  It packages
`anvil-autoresearch-routing-evaluate' into a prompt suitable for a
future orchestrator proposal."
  (let ((evaluation (anvil-autoresearch-routing-evaluate
                     prompt policies candidates since-sec min-samples)))
    (list :evaluation evaluation
          :rewrite-prompt
          (anvil-autoresearch--routing-plan-prompt evaluation)
          :suggested-action "submit-routing-improvement-proposal")))

(defun anvil-autoresearch--routing-proposal-task
    (plan provider model budget timeout heartbeat cwd sandbox
          no-worktree worktree-name)
  "Return one orchestrator task plist for routing improvement PLAN."
  (append
   (list :name "autoresearch-routing-improvement"
         :provider provider
         :prompt (plist-get plan :rewrite-prompt)
         :cwd cwd
         :budget-usd budget
         :timeout-sec timeout)
   (when heartbeat
     (list :heartbeat-timeout-sec heartbeat))
   (when model
     (list :model model))
   (when sandbox
     (list :sandbox sandbox))
   (when no-worktree
     (list :no-worktree t))
   (when (and worktree-name (not no-worktree))
     (list :worktree-name worktree-name))))

(defun anvil-autoresearch-routing-propose
    (&optional prompt policies candidates since-sec min-samples provider model
               budget-usd timeout-sec heartbeat-timeout-sec cwd sandbox
               no-worktree worktree-name)
  "Submit one orchestrator task to propose a routing improvement.

MCP Parameters:
  prompt                - Optional representative prompt text.
  policies              - Optional comma-separated policy names.
  candidates            - Optional comma-separated provider list.
  since-sec             - Optional lookback window in seconds.
  min-samples           - Optional per-provider sample floor.
  provider              - Optional orchestrator provider.  Defaults to
                          `anvil-autoresearch-docstring-proposal-provider'.
  model                 - Optional provider model string.
  budget-usd            - Optional per-task budget.  Defaults to
                          `anvil-autoresearch-docstring-proposal-budget-usd'.
  timeout-sec           - Optional wall-clock timeout.  Defaults to
                          `anvil-autoresearch-docstring-proposal-timeout-sec'.
  heartbeat-timeout-sec - Optional no-output timeout.  Defaults to the
                          docstring proposal heartbeat defcustom.
  cwd                   - Optional repo/worktree directory.  Defaults to
                          this anvil.el repository root.
  sandbox               - Optional provider sandbox, used by Codex.
  no-worktree           - Optional truthy flag.  When truthy, pass
                          `:no-worktree t' to the orchestrator task.
  worktree-name         - Optional worktree name hint for providers /
                          orchestrator paths that support it.

This is the Doc 43 Phase 3a Q1-B proposal entrance.  It packages
`anvil-autoresearch-routing-plan' as one bounded async orchestrator
task.  It does not mutate routing settings or apply any proposal."
  (anvil-autoresearch--ensure-orchestrator)
  (let* ((plan (anvil-autoresearch-routing-plan
                prompt policies candidates since-sec min-samples))
         (provider* (anvil-autoresearch--coerce-provider
                     provider
                     anvil-autoresearch-docstring-proposal-provider))
         (model* (anvil-autoresearch--coerce-string
                  model
                  anvil-autoresearch-docstring-proposal-model))
         (budget* (anvil-autoresearch--coerce-number
                   budget-usd
                   anvil-autoresearch-docstring-proposal-budget-usd
                   "budget-usd"))
         (timeout* (anvil-autoresearch--coerce-number
                    timeout-sec
                    anvil-autoresearch-docstring-proposal-timeout-sec
                    "timeout-sec"))
         (heartbeat* (anvil-autoresearch--coerce-number
                      heartbeat-timeout-sec
                      anvil-autoresearch-docstring-proposal-heartbeat-timeout-sec
                      "heartbeat-timeout-sec"))
         (cwd* (anvil-autoresearch--repo-root cwd))
         (sandbox* (anvil-autoresearch--coerce-string
                    sandbox
                    anvil-autoresearch-docstring-proposal-sandbox))
         (no-worktree* (anvil-autoresearch--coerce-bool no-worktree))
         (worktree-name* (anvil-autoresearch--coerce-string
                          worktree-name nil))
         (task (anvil-autoresearch--routing-proposal-task
                plan provider* model* budget* timeout* heartbeat* cwd*
                sandbox* no-worktree* worktree-name*))
         (batch-id (anvil-orchestrator-submit (list task))))
    (list :submitted t
          :batch-id batch-id
          :task task
          :plan plan
          :suggested-action "poll-routing-proposal-batch")))

(defun anvil-autoresearch--routing-proposal-score (text)
  "Return a heuristic usefulness score for routing proposal TEXT."
  (let ((score 0)
        (case-fold-search t))
    (when (> (length text) 80)
      (cl-incf score 8))
    (when (> (length text) 220)
      (cl-incf score 8))
    (dolist (pattern '("routing" "provider" "policy" "candidate"
                       "min-samples" "cold-start" "stats" "test"
                       "defcustom" "anvil-orchestrator"))
      (when (string-match-p (regexp-quote pattern) text)
        (cl-incf score 5)))
    (when (string-match-p "\\b\\(speed\\|cost\\|quality\\|balanced\\)\\b" text)
      (cl-incf score 5))
    (when (string-match-p "\\b\\(ert\\|test\\|byte-compile\\)\\b" text)
      (cl-incf score 5))
    (when (string-match-p "\\b\\(edit\\|change\\|patch\\|implement\\)\\b" text)
      (cl-incf score 3))
    score))

(defun anvil-autoresearch--routing-proposal-candidates
    (results max-candidates)
  "Return ranked routing proposal candidates from orchestrator RESULTS."
  (let (candidates)
    (dolist (result results)
      (let ((proposal
             (and (eq (plist-get result :status) 'done)
                  (anvil-autoresearch--clean-proposal-description
                   (plist-get result :summary)))))
        (when proposal
          (push (list :label (or (plist-get result :name)
                                 (plist-get result :id)
                                 "routing-proposal")
                      :proposal proposal
                      :score (anvil-autoresearch--routing-proposal-score
                              proposal)
                      :task-id (plist-get result :id)
                      :provider (plist-get result :provider))
                candidates))))
    (setq candidates
          (sort (nreverse candidates)
                (lambda (a b)
                  (> (plist-get a :score)
                     (plist-get b :score)))))
    (cl-subseq candidates 0 (min (length candidates) max-candidates))))

(defun anvil-autoresearch-routing-harvest
    (batch-id &optional wait max-candidates)
  "Harvest routing proposal batch results and select a review candidate.

MCP Parameters:
  batch-id       - Orchestrator batch id returned by
                   `autoresearch-routing-propose' or a compatible
                   routing proposal batch.
  wait           - Optional truthy flag.  When truthy, wait for the
                   orchestrator batch to become terminal before
                   collecting summaries.
  max-candidates - Optional maximum number of routing proposals to rank.

Only successful orchestrator task summaries become candidates.  This
function ranks text proposals heuristically and returns the best
candidate for human review.  It does not mutate routing settings,
source files, provider registries, or task queues."
  (unless (and (stringp batch-id)
               (not (string-empty-p (string-trim batch-id))))
    (error "batch_id is required"))
  (anvil-autoresearch--ensure-orchestrator-collect)
  (let* ((max* (anvil-autoresearch--coerce-max-candidates max-candidates))
         (results (anvil-orchestrator-collect
                   batch-id
                   :wait (anvil-autoresearch--coerce-bool wait)))
         (candidates
          (anvil-autoresearch--routing-proposal-candidates results max*))
         (selected (car candidates)))
    (list :batch-id batch-id
          :candidate-count (length candidates)
          :results (apply #'vector results)
          :candidates (apply #'vector candidates)
          :selected selected
          :kept nil
          :reverted nil
          :suggested-action
          (if selected
              "review-selected-routing-proposal"
            "no-routing-proposal-candidates"))))

(defun anvil-autoresearch--routing-object-field (object key)
  "Return KEY from routing OBJECT, accepting plist or decoded JSON alist."
  (or (and (listp object)
           (plist-get object
                      (intern (concat ":" (symbol-name key)))))
      (anvil-autoresearch--json-field object key)))

(defun anvil-autoresearch--routing-selected-proposal (object)
  "Return selected routing proposal plist from OBJECT."
  (let ((selected (or (anvil-autoresearch--routing-object-field
                       object 'selected)
                      object)))
    (when (and selected
               (anvil-autoresearch--routing-object-field
                selected 'proposal))
      selected)))

(defun anvil-autoresearch--routing-patch-prompt (proposal)
  "Return a dry-run implementation prompt for routing PROPOSAL."
  (string-join
   (list
    "You are implementing Doc 43 autoresearch Phase 3c for anvil.el."
    "Task: turn the reviewed routing proposal into one minimal patch."
    "Keep the change focused on Q1-B orchestrator provider routing."
    "Primary target: anvil-orchestrator-routing.el."
    "Likely secondary target: tests/anvil-orchestrator-test.el."
    "Only touch anvil-orchestrator.el if dispatch metadata or auto-provider integration is directly required."
    "Preserve existing defcustom names and public MCP contracts unless the proposal explicitly requires a compatible extension."
    "Add or adjust ERT coverage for policy defaults, candidate sets, min-samples, cold-start behavior, or stats interpretation touched by the patch."
    "Run byte-compile and the orchestrator ERT suite before accepting the patch."
    (format "Reviewed routing proposal:\n%s" proposal))
   "\n\n"))

(defun anvil-autoresearch-routing-patch-plan
    (&optional proposal-json batch-id wait max-candidates)
  "Return a dry-run patch plan for a selected routing proposal.

MCP Parameters:
  proposal-json  - Optional JSON object from
                   `autoresearch-routing-harvest' or one selected
                   candidate object with a `proposal' field.
  batch-id       - Optional orchestrator batch id.  Used only when
                   `proposal-json' is empty; the batch is harvested
                   with `autoresearch-routing-harvest'.
  wait           - Optional truthy flag passed to batch harvest.
  max-candidates - Optional maximum number of harvested proposals.

This function does not edit files and does not apply routing changes.
It turns a reviewed or harvested proposal into a deterministic
implementation prompt, target-file hints, and verification commands for
the next patch step."
  (let* ((source (if (anvil-autoresearch--blank-string-p proposal-json)
                     (progn
                       (unless (and (stringp batch-id)
                                    (not (string-empty-p
                                          (string-trim batch-id))))
                         (error "proposal_json or batch_id is required"))
                       (anvil-autoresearch-routing-harvest
                        batch-id wait max-candidates))
                   (anvil-autoresearch--parse-json-object
                    proposal-json "proposal_json")))
         (selected (anvil-autoresearch--routing-selected-proposal source))
         (proposal (and selected
                        (anvil-autoresearch--routing-object-field
                         selected 'proposal))))
    (unless (and (stringp proposal)
                 (not (string-empty-p (string-trim proposal))))
      (error "no selected routing proposal found"))
    (list :dry-run t
          :proposal proposal
          :selected selected
          :source source
          :target-files
          ["anvil-orchestrator-routing.el"
           "tests/anvil-orchestrator-test.el"
           "anvil-orchestrator.el"]
          :implementation-prompt
          (anvil-autoresearch--routing-patch-prompt proposal)
          :eval-command
          "emacs --batch -Q -L . -L tests -f batch-byte-compile anvil-orchestrator-routing.el anvil-orchestrator.el tests/anvil-orchestrator-test.el"
          :holdout-command
          "emacs --batch -Q -L . -L tests -l tests/anvil-orchestrator-test.el -f ert-run-tests-batch-and-exit"
          :suggested-action
          "review-routing-patch-plan-then-implement")))

(defun anvil-autoresearch--routing-implementation-task
    (plan provider model budget timeout heartbeat cwd sandbox
          no-worktree worktree-name)
  "Return one orchestrator implementation task plist for routing PLAN."
  (append
   (list :name "autoresearch-routing-implementation"
         :provider provider
         :prompt (plist-get plan :implementation-prompt)
         :cwd cwd
         :budget-usd budget
         :timeout-sec timeout)
   (when heartbeat
     (list :heartbeat-timeout-sec heartbeat))
   (when model
     (list :model model))
   (when sandbox
     (list :sandbox sandbox))
   (when no-worktree
     (list :no-worktree t))
   (when (and worktree-name (not no-worktree))
     (list :worktree-name worktree-name))))

(defun anvil-autoresearch-routing-implement
    (&optional proposal-json batch-id wait max-candidates provider model
               budget-usd timeout-sec heartbeat-timeout-sec cwd sandbox
               no-worktree worktree-name)
  "Submit one orchestrator task to implement a routing proposal.

MCP Parameters:
  proposal-json         - Optional JSON object from
                          `autoresearch-routing-harvest' or one
                          selected candidate object with a `proposal'
                          field.
  batch-id              - Optional routing proposal batch id.  Used
                          only when `proposal-json' is empty.
  wait                  - Optional truthy flag passed to batch harvest.
  max-candidates        - Optional maximum number of harvested
                          proposals.
  provider              - Optional orchestrator provider.  Defaults to
                          `anvil-autoresearch-docstring-proposal-provider'.
  model                 - Optional provider model string.
  budget-usd            - Optional per-task budget.  Defaults to
                          `anvil-autoresearch-docstring-proposal-budget-usd'.
  timeout-sec           - Optional wall-clock timeout.  Defaults to
                          `anvil-autoresearch-docstring-proposal-timeout-sec'.
  heartbeat-timeout-sec - Optional no-output timeout.  Defaults to the
                          docstring proposal heartbeat defcustom.
  cwd                   - Optional repo/worktree directory.  Defaults to
                          this anvil.el repository root.
  sandbox               - Optional provider sandbox, used by Codex.
  no-worktree           - Optional truthy flag.  When truthy, pass
                          `:no-worktree t' to the orchestrator task.
  worktree-name         - Optional worktree name hint for providers /
                          orchestrator paths that support it.

This is the Doc 43 Phase 3d Q1-B implementation entrance.  It uses
`anvil-autoresearch-routing-patch-plan' to build a focused
implementation prompt, then submits it as one bounded async
orchestrator task.  This function itself does not edit source files."
  (anvil-autoresearch--ensure-orchestrator)
  (let* ((plan (anvil-autoresearch-routing-patch-plan
                proposal-json batch-id wait max-candidates))
         (provider* (anvil-autoresearch--coerce-provider
                     provider
                     anvil-autoresearch-docstring-proposal-provider))
         (model* (anvil-autoresearch--coerce-string
                  model
                  anvil-autoresearch-docstring-proposal-model))
         (budget* (anvil-autoresearch--coerce-number
                   budget-usd
                   anvil-autoresearch-docstring-proposal-budget-usd
                   "budget-usd"))
         (timeout* (anvil-autoresearch--coerce-number
                    timeout-sec
                    anvil-autoresearch-docstring-proposal-timeout-sec
                    "timeout-sec"))
         (heartbeat* (anvil-autoresearch--coerce-number
                      heartbeat-timeout-sec
                      anvil-autoresearch-docstring-proposal-heartbeat-timeout-sec
                      "heartbeat-timeout-sec"))
         (cwd* (anvil-autoresearch--repo-root cwd))
         (sandbox* (anvil-autoresearch--coerce-string
                    sandbox
                    anvil-autoresearch-docstring-proposal-sandbox))
         (no-worktree* (anvil-autoresearch--coerce-bool no-worktree))
         (worktree-name* (anvil-autoresearch--coerce-string
                          worktree-name nil))
         (task (anvil-autoresearch--routing-implementation-task
                plan provider* model* budget* timeout* heartbeat* cwd*
                sandbox* no-worktree* worktree-name*))
         (batch-id* (anvil-orchestrator-submit (list task))))
    (list :submitted t
          :batch-id batch-id*
          :task task
          :plan plan
          :suggested-action "poll-routing-implementation-batch")))

(defun anvil-autoresearch--routing-implementation-score (text)
  "Return a heuristic review score for routing implementation TEXT."
  (let ((score 0)
        (case-fold-search t))
    (when (> (length text) 120)
      (cl-incf score 8))
    (when (> (length text) 320)
      (cl-incf score 8))
    (dolist (pattern '("anvil-orchestrator-routing.el"
                       "tests/anvil-orchestrator-test.el"
                       "routing" "provider" "policy" "candidate"
                       "min-samples" "cold-start" "stats"
                       "defcustom" "ert" "byte-compile"
                       "test" "diff" "patch" "changed"))
      (when (string-match-p (regexp-quote pattern) text)
        (cl-incf score 4)))
    (when (string-match-p "\\b\\(passed\\|success\\|green\\)\\b" text)
      (cl-incf score 5))
    (when (string-match-p "\\b\\(failed\\|error\\|blocked\\)\\b" text)
      (cl-decf score 8))
    score))

(defun anvil-autoresearch--routing-implementation-candidates
    (results max-candidates)
  "Return ranked routing implementation candidates from RESULTS."
  (let (candidates)
    (dolist (result results)
      (let ((summary
             (and (eq (plist-get result :status) 'done)
                  (anvil-autoresearch--clean-proposal-description
                   (plist-get result :summary)))))
        (when summary
          (push (list :label (or (plist-get result :name)
                                 (plist-get result :id)
                                 "routing-implementation")
                      :summary summary
                      :score (anvil-autoresearch--routing-implementation-score
                              summary)
                      :task-id (plist-get result :id)
                      :provider (plist-get result :provider))
                candidates))))
    (setq candidates
          (sort (nreverse candidates)
                (lambda (a b)
                  (> (plist-get a :score)
                     (plist-get b :score)))))
    (cl-subseq candidates 0 (min (length candidates) max-candidates))))

(defun anvil-autoresearch-routing-implementation-harvest
    (batch-id &optional wait max-candidates)
  "Harvest routing implementation batch results for review.

MCP Parameters:
  batch-id       - Orchestrator batch id returned by
                   `autoresearch-routing-implement' or a compatible
                   implementation batch.
  wait           - Optional truthy flag.  When truthy, wait for the
                   orchestrator batch to become terminal before
                   collecting summaries.
  max-candidates - Optional maximum number of implementation summaries
                   to rank.

Only successful orchestrator task summaries become candidates.  This
function does not apply patches, edit files, or mutate routing
settings; it returns the selected implementation summary plus review
checklist items for manual adoption."
  (unless (and (stringp batch-id)
               (not (string-empty-p (string-trim batch-id))))
    (error "batch_id is required"))
  (anvil-autoresearch--ensure-orchestrator-collect)
  (let* ((max* (anvil-autoresearch--coerce-max-candidates max-candidates))
         (results (anvil-orchestrator-collect
                   batch-id
                   :wait (anvil-autoresearch--coerce-bool wait)))
         (candidates
          (anvil-autoresearch--routing-implementation-candidates
           results max*))
         (selected (car candidates)))
    (list :batch-id batch-id
          :candidate-count (length candidates)
          :results (apply #'vector results)
          :candidates (apply #'vector candidates)
          :selected selected
          :review-checklist
          ["inspect-diff-for-routing-scope"
           "run-byte-compile-command"
           "run-orchestrator-ert-holdout"
           "verify-no-unrelated-source-changes"
           "decide-apply-or-reject"]
          :kept nil
          :reverted nil
          :suggested-action
          (if selected
              "review-selected-routing-implementation"
            "no-routing-implementation-candidates"))))

(defun anvil-autoresearch--routing-implementation-memory-name
    (object selected name)
  "Return routing implementation memory NAME or a generated default."
  (or (anvil-autoresearch--coerce-string name nil)
      (format "autoresearch_routing_impl_%s_%s"
              (or (anvil-autoresearch--routing-object-field selected 'label)
                  (anvil-autoresearch--routing-object-field selected 'task-id)
                  (anvil-autoresearch--routing-object-field object 'batch-id)
                  "candidate")
              (format-time-string "%Y%m%d_%H%M%S"))))

(defun anvil-autoresearch--routing-implementation-memory-body
    (object selected)
  "Return an org-ish memory body for routing implementation OBJECT."
  (let ((summary (anvil-autoresearch--routing-object-field selected
                                                           'summary)))
    (string-join
     (delq
      nil
      (list
       "* Doc43 autoresearch accepted routing implementation"
       (format "- Batch: %s"
               (or (anvil-autoresearch--routing-object-field object
                                                             'batch-id)
                   ""))
       (format "- Selected label: %s"
               (or (anvil-autoresearch--routing-object-field selected
                                                             'label)
                   ""))
       (format "- Task id: %s"
               (or (anvil-autoresearch--routing-object-field selected
                                                             'task-id)
                   ""))
       (format "- Provider: %s"
               (or (anvil-autoresearch--routing-object-field selected
                                                             'provider)
                   ""))
       (format "- Score: %s"
               (or (anvil-autoresearch--routing-object-field selected
                                                             'score)
                   ""))
       "** Accepted implementation summary"
       summary))
     "\n")))

(defun anvil-autoresearch-routing-implementation-promote-memory
    (run-json &optional name type memory-description tags-json)
  "Promote an accepted routing implementation summary into anvil-memory.

MCP Parameters:
  run-json           - JSON object returned by
                       `autoresearch-routing-implementation-harvest',
                       or one selected candidate object with a
                       `summary' field.
  name               - Optional memory basename.  Defaults to
                       autoresearch_routing_impl_<label>_<timestamp>.
  type               - Optional memory type: user, feedback, project,
                       reference, or memo.  Defaults to memo.
  memory-description - Optional short memory description.  Defaults to
                       \"Doc43 accepted routing implementation\".
  tags-json          - Optional JSON array of string tags.

The run must contain a selected implementation candidate with a
summary.  This function writes one DB-direct memory through
`anvil-memory-add' and does not edit source files or routing settings."
  (anvil-autoresearch--ensure-memory)
  (let* ((object (anvil-autoresearch--parse-json-object run-json
                                                        "run_json"))
         (selected (or (anvil-autoresearch--routing-object-field
                        object 'selected)
                       object))
         (summary (and selected
                       (anvil-autoresearch--routing-object-field
                        selected 'summary))))
    (unless (and (stringp summary)
                 (not (string-empty-p (string-trim summary))))
      (error "run_json does not contain a selected implementation summary"))
    (let* ((type* (anvil-autoresearch--coerce-memory-type type))
           (name* (anvil-autoresearch--routing-implementation-memory-name
                   object selected name))
           (memory-description*
            (or (anvil-autoresearch--coerce-string memory-description nil)
                "Doc43 accepted routing implementation"))
           (tags (or (anvil-autoresearch--parse-tags-json tags-json)
                     '("doc43" "autoresearch" "routing"
                       "implementation")))
           (body (anvil-autoresearch--routing-implementation-memory-body
                  object selected))
           (row (anvil-memory-add name* type* body
                                  :description memory-description*
                                  :tags tags)))
      (list :promoted t
            :memory row
            :name name*
            :type type*
            :selected-summary summary
            :suggested-action
            "review-or-export-promoted-routing-memory"))))


;;;; --- NeLisp Phase 47 codegen -------------------------------------------

(defun anvil-autoresearch--nelisp-root (&optional root)
  "Return the expanded NeLisp ROOT directory or signal an error."
  (let ((dir (file-name-as-directory
              (expand-file-name
               (anvil-autoresearch--coerce-string
                root anvil-autoresearch-nelisp-root)))))
    (unless (file-directory-p dir)
      (error "NeLisp root does not exist: %s" dir))
    dir))

(defun anvil-autoresearch--nelisp-codegen-files (root)
  "Return plist of existing and missing codegen files under ROOT."
  (let (present missing)
    (dolist (rel anvil-autoresearch-nelisp-codegen-target-files)
      (let ((abs (expand-file-name rel root)))
        (if (file-exists-p abs)
            (push rel present)
          (push rel missing))))
    (list :present (apply #'vector (nreverse present))
          :missing (apply #'vector (nreverse missing)))))

(defun anvil-autoresearch--nelisp-codegen-prompt
    (focus root files eval-command holdout-command)
  "Return an implementation prompt for NeLisp codegen FOCUS."
  (string-join
   (list
    "Task: implement one focused NeLisp Phase 47 codegen improvement."
    ""
    "Work in the NeLisp repository only."
    (format "Repository root: %s" root)
    (format "Focus: %s" focus)
    ""
    "Primary context files:"
    (mapconcat (lambda (file) (format "- %s" file))
               (append (plist-get files :present) nil)
               "\n")
    ""
    "Candidate improvement dimensions:"
    "- Phase 47 compiler pass ordering."
    "- register/frame-slot allocation heuristics."
    "- byte-length invariant preservation."
    "- generated object/runtime smoke coverage."
    ""
    "Constraints:"
    "- Keep the patch small and reviewable."
    "- Preserve existing Phase 47 byte-pattern and e2e tests."
    "- Add or update focused tests for any behavior change."
    "- Do not bypass failing tests or weaken existing invariants."
    ""
    "Verification commands:"
    (format "- eval: %s" eval-command)
    (format "- holdout: %s" holdout-command)
    ""
    "Return a concise implementation summary, files changed, and exact verification results.")
   "\n"))

(defun anvil-autoresearch-nelisp-codegen-plan (&optional focus root)
  "Return a Doc 43 Phase 5 plan for NeLisp Phase 47 codegen work.

MCP Parameters:
  focus - Optional natural-language focus for the codegen experiment.
  root  - Optional NeLisp repository root.  Defaults to
          `anvil-autoresearch-nelisp-root'.

This is read-only.  It resolves the NeLisp root, reports target files,
and returns eval / holdout commands plus the prompt used by
`anvil-autoresearch-nelisp-codegen-implement'."
  (let* ((root* (anvil-autoresearch--nelisp-root root))
         (focus* (anvil-autoresearch--coerce-string
                  focus
                  "improve Phase 47 codegen while preserving byte identity"))
         (files (anvil-autoresearch--nelisp-codegen-files root*))
         (eval-command
          "emacs --batch -Q -L lisp -L test -f batch-byte-compile lisp/nelisp-phase47-compiler.el test/nelisp-phase47-compiler-test.el")
         (holdout-command
          "emacs --batch -Q -L lisp -L test -l test/nelisp-phase47-compiler-test.el -f ert-run-tests-batch-and-exit"))
    (list :target "nelisp-phase47-codegen"
          :focus focus*
          :root root*
          :target-files (plist-get files :present)
          :missing-files (plist-get files :missing)
          :eval-command eval-command
          :holdout-command holdout-command
          :implementation-prompt
          (anvil-autoresearch--nelisp-codegen-prompt
           focus* root* files eval-command holdout-command)
          :suggested-action
          "submit-nelisp-codegen-implementation-task")))

(defun anvil-autoresearch--nelisp-codegen-task
    (plan provider model budget timeout heartbeat sandbox
          no-worktree worktree-name)
  "Return one orchestrator implementation task plist from PLAN."
  (append
   (list :name "autoresearch-nelisp-codegen-implementation"
         :provider provider
         :prompt (plist-get plan :implementation-prompt)
         :cwd (plist-get plan :root)
         :budget-usd budget
         :timeout-sec timeout)
   (when heartbeat
     (list :heartbeat-timeout-sec heartbeat))
   (when model
     (list :model model))
   (when sandbox
     (list :sandbox sandbox))
   (when no-worktree
     (list :no-worktree t))
   (when (and worktree-name (not no-worktree))
     (list :worktree-name worktree-name))))

(defun anvil-autoresearch-nelisp-codegen-implement
    (&optional focus root provider model budget-usd timeout-sec
               heartbeat-timeout-sec sandbox no-worktree worktree-name)
  "Submit one bounded task for NeLisp Phase 47 codegen improvement.

MCP Parameters:
  focus                 - Optional natural-language codegen focus.
  root                  - Optional NeLisp repository root.
  provider              - Optional orchestrator provider.  Defaults to
                          `anvil-autoresearch-docstring-proposal-provider'.
  model                 - Optional provider model string.
  budget-usd            - Optional per-task budget.
  timeout-sec           - Optional wall-clock timeout.
  heartbeat-timeout-sec - Optional no-output timeout.
  sandbox               - Optional provider sandbox, used by Codex.
  no-worktree           - Optional truthy flag.  When truthy, pass
                          `:no-worktree t' to the orchestrator task.
  worktree-name         - Optional worktree name hint.

The tool itself does not edit NeLisp.  It packages the Phase 5 plan as
one orchestrator task with explicit budget / timeout / heartbeat / cwd
metadata."
  (anvil-autoresearch--ensure-orchestrator)
  (let* ((plan (anvil-autoresearch-nelisp-codegen-plan focus root))
         (provider* (anvil-autoresearch--coerce-provider
                     provider
                     anvil-autoresearch-docstring-proposal-provider))
         (model* (anvil-autoresearch--coerce-string
                  model
                  anvil-autoresearch-docstring-proposal-model))
         (budget* (anvil-autoresearch--coerce-number
                   budget-usd
                   anvil-autoresearch-docstring-proposal-budget-usd
                   "budget-usd"))
         (timeout* (anvil-autoresearch--coerce-number
                    timeout-sec
                    anvil-autoresearch-docstring-proposal-timeout-sec
                    "timeout-sec"))
         (heartbeat* (anvil-autoresearch--coerce-number
                      heartbeat-timeout-sec
                      anvil-autoresearch-docstring-proposal-heartbeat-timeout-sec
                      "heartbeat-timeout-sec"))
         (sandbox* (anvil-autoresearch--coerce-string
                    sandbox
                    anvil-autoresearch-docstring-proposal-sandbox))
         (no-worktree* (anvil-autoresearch--coerce-bool no-worktree))
         (worktree-name* (anvil-autoresearch--coerce-string
                          worktree-name nil))
         (task (anvil-autoresearch--nelisp-codegen-task
                plan provider* model* budget* timeout* heartbeat*
                sandbox* no-worktree* worktree-name*))
         (batch-id (anvil-orchestrator-submit (list task))))
    (list :submitted t
          :batch-id batch-id
          :task task
          :plan plan
          :suggested-action "poll-nelisp-codegen-implementation-batch")))

(defun anvil-autoresearch--nelisp-codegen-score (text)
  "Return a heuristic review score for NeLisp codegen TEXT."
  (let ((score 0)
        (case-fold-search t))
    (when (> (length text) 100)
      (cl-incf score 8))
    (when (> (length text) 260)
      (cl-incf score 8))
    (dolist (pattern '("phase 47" "nelisp-phase47-compiler"
                       "codegen" "byte" "identity" "pass"
                       "register" "frame" "heuristic" "ert"
                       "byte-compile" "test"))
      (when (string-match-p (regexp-quote pattern) text)
        (cl-incf score 5)))
    (when (string-match-p "\\b\\(passed\\|failed\\|verified\\)\\b" text)
      (cl-incf score 5))
    score))

(defun anvil-autoresearch--nelisp-codegen-candidates
    (results max-candidates)
  "Return ranked NeLisp codegen implementation candidates from RESULTS."
  (let (candidates)
    (dolist (result results)
      (let ((summary
             (and (eq (plist-get result :status) 'done)
                  (anvil-autoresearch--clean-proposal-description
                   (plist-get result :summary)))))
        (when summary
          (push (list :label (or (plist-get result :name)
                                 (plist-get result :id)
                                 "nelisp-codegen-implementation")
                      :summary summary
                      :score (anvil-autoresearch--nelisp-codegen-score
                              summary)
                      :task-id (plist-get result :id)
                      :provider (plist-get result :provider))
                candidates))))
    (setq candidates
          (sort (nreverse candidates)
                (lambda (a b)
                  (> (plist-get a :score)
                     (plist-get b :score)))))
    (cl-subseq candidates 0 (min (length candidates) max-candidates))))

(defun anvil-autoresearch-nelisp-codegen-harvest
    (batch-id &optional wait max-candidates)
  "Harvest a NeLisp codegen implementation batch for manual review.

MCP Parameters:
  batch-id       - Orchestrator batch id returned by
                   `autoresearch-nelisp-codegen-implement'.
  wait           - Optional truthy flag to wait before collecting.
  max-candidates - Optional maximum number of summaries to rank.

This is read-only.  It does not apply patches or mutate NeLisp; it
returns ranked implementation summaries plus a review checklist."
  (unless (and (stringp batch-id)
               (not (string-empty-p (string-trim batch-id))))
    (error "batch_id is required"))
  (anvil-autoresearch--ensure-orchestrator-collect)
  (let* ((max* (anvil-autoresearch--coerce-max-candidates max-candidates))
         (results (anvil-orchestrator-collect
                   batch-id
                   :wait (anvil-autoresearch--coerce-bool wait)))
         (candidates
          (anvil-autoresearch--nelisp-codegen-candidates results max*))
         (selected (car candidates)))
    (list :batch-id batch-id
          :results (apply #'vector results)
          :candidate-count (length candidates)
          :candidates (apply #'vector candidates)
          :selected selected
          :review-checklist
          ["Confirm Phase 47 byte identity / byte-length invariants remain covered."
           "Confirm compiler changes are focused on pass ordering or codegen heuristics."
           "Confirm nelisp-phase47-compiler-test ERT and byte-compile commands were run."
           "Confirm no unrelated NeLisp runtime or Rust migration work slipped in."]
          :suggested-action
          (if selected
              "review-nelisp-codegen-implementation-summary"
            "no-nelisp-codegen-candidates"))))


;;;; --- dashboard ----------------------------------------------------------

(defun anvil-autoresearch--truncate (text width)
  "Return TEXT truncated to WIDTH display columns."
  (let ((s (if text (format "%s" text) "")))
    (truncate-string-to-width s width nil nil t)))

(defun anvil-autoresearch--task-autoresearch-p (task)
  "Return non-nil when TASK is an autoresearch orchestrator task."
  (let ((name (plist-get task :name)))
    (and (stringp name)
         (string-prefix-p "autoresearch-" name))))

(defun anvil-autoresearch--task-stage (task)
  "Return a dashboard stage label for autoresearch TASK."
  (let ((name (or (plist-get task :name) "")))
    (cond
     ((string-prefix-p "autoresearch-docstring-" name)
      "docstring proposal")
     ((string= name "autoresearch-routing-improvement")
      "routing proposal")
     ((string= name "autoresearch-routing-implementation")
      "routing implementation")
     ((string= name "autoresearch-nelisp-codegen-implementation")
      "nelisp codegen")
     (t "autoresearch"))))

(defun anvil-autoresearch--task-target (task)
  "Return a compact target label for autoresearch TASK."
  (let ((name (or (plist-get task :name) "")))
    (cond
     ((string-match
       "\\`autoresearch-docstring-\\(.+?\\)\\(?:-[0-9][0-9]-of-[0-9][0-9]\\)?\\'"
       name)
      (match-string 1 name))
     ((string-prefix-p "autoresearch-routing-" name)
      "orchestrator routing")
     ((string-prefix-p "autoresearch-nelisp-codegen-" name)
      "Phase 47 codegen")
     (t name))))

(defun anvil-autoresearch--dashboard-entries ()
  "Return tabulated-list entries for autoresearch orchestrator tasks."
  (let (entries)
    (when (hash-table-p anvil-orchestrator--tasks)
      (maphash
       (lambda (id task)
         (when (anvil-autoresearch--task-autoresearch-p task)
           (let* ((batch-id (or (plist-get task :batch-id) ""))
                  (summary (or (plist-get task :summary)
                               (plist-get task :error)
                               ""))
                  (row
                   (vector
                    (anvil-autoresearch--truncate
                     (anvil-autoresearch--task-stage task) 22)
                    (anvil-autoresearch--truncate
                     (anvil-autoresearch--task-target task) 28)
                    (format "%s" (or (plist-get task :status) ""))
                    (format "%s" (or (plist-get task :provider) ""))
                    (or (plist-get task :model) "")
                    (if (plist-get task :elapsed-ms)
                        (number-to-string (plist-get task :elapsed-ms))
                      "")
                    (if (plist-get task :cost-usd)
                        (format "$%.4f" (plist-get task :cost-usd))
                      "")
                    (substring batch-id 0 (min 8 (length batch-id)))
                    (anvil-autoresearch--truncate summary 64))))
             (push (list id row) entries))))
       anvil-orchestrator--tasks))
    (sort entries
          (lambda (a b)
            (string< (format "%s" (car a))
                     (format "%s" (car b)))))))

(defvar anvil-autoresearch-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'anvil-autoresearch-dashboard-refresh)
    (define-key map (kbd "RET") #'anvil-autoresearch-dashboard-show-task)
    map)
  "Keymap for `anvil-autoresearch-dashboard-mode'.")

(define-derived-mode anvil-autoresearch-dashboard-mode tabulated-list-mode
  "Anvil-AutoResearch"
  "Major mode for the Doc 43 autoresearch dashboard."
  (setq tabulated-list-format
        [("Stage"    22 t)
         ("Target"   28 t)
         ("Status"   10 t)
         ("Provider" 10 t)
         ("Model"    12 t)
         ("Elapsed"  10 t :right-align t)
         ("Cost"      8 t :right-align t)
         ("Batch"    10 t)
         ("Summary"  64 nil)]
        tabulated-list-padding 1
        tabulated-list-sort-key '("Stage" . nil))
  (tabulated-list-init-header))

;;;###autoload
(defun anvil-autoresearch-dashboard ()
  "Open a tabulated-list dashboard for current autoresearch tasks."
  (interactive)
  (let ((buf (get-buffer-create "*Anvil Autoresearch*")))
    (with-current-buffer buf
      (anvil-autoresearch-dashboard-mode)
      (anvil-autoresearch-dashboard-refresh))
    (pop-to-buffer buf)))

(defun anvil-autoresearch-dashboard-refresh ()
  "Refresh the current autoresearch dashboard buffer."
  (interactive)
  (unless (derived-mode-p 'anvil-autoresearch-dashboard-mode)
    (user-error "Not in an autoresearch dashboard buffer"))
  (setq tabulated-list-entries (anvil-autoresearch--dashboard-entries))
  (tabulated-list-print t))

(defun anvil-autoresearch-dashboard-show-task ()
  "Show the full orchestrator task plist at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (task (and id
                    (hash-table-p anvil-orchestrator--tasks)
                    (gethash id anvil-orchestrator--tasks))))
    (unless task
      (user-error "No autoresearch task at point"))
    (let ((buf (get-buffer-create "*Anvil Autoresearch Task*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "id: %s\n\n" id))
          (insert (pp-to-string task))
          (goto-char (point-min))
          (view-mode 1)))
      (pop-to-buffer buf))))


;;;; --- NeLisp codegen wrappers -------------------------------------------

(defun anvil-autoresearch--tool-nelisp-codegen-plan
    (&optional focus root)
  "MCP wrapper for `anvil-autoresearch-nelisp-codegen-plan'.

MCP Parameters:
  focus - Optional natural-language Phase 47 codegen focus.
  root  - Optional NeLisp repository root."
  (anvil-autoresearch-nelisp-codegen-plan focus root))

(defun anvil-autoresearch--tool-nelisp-codegen-implement
    (&optional focus root provider model budget_usd timeout_sec
              heartbeat_timeout_sec sandbox no_worktree worktree_name)
  "MCP wrapper for `anvil-autoresearch-nelisp-codegen-implement'.

MCP Parameters:
  focus                 - Optional natural-language Phase 47 codegen focus.
  root                  - Optional NeLisp repository root.
  provider              - Optional orchestrator provider string.
  model                 - Optional provider model string.
  budget_usd            - Optional per-task budget.
  timeout_sec           - Optional wall-clock timeout.
  heartbeat_timeout_sec - Optional no-output timeout.
  sandbox               - Optional provider sandbox string.
  no_worktree           - Optional truthy worktree opt-out flag.
  worktree_name         - Optional explicit worktree name hint."
  (anvil-autoresearch-nelisp-codegen-implement
   focus root provider model budget_usd timeout_sec heartbeat_timeout_sec
   sandbox no_worktree worktree_name))

(defun anvil-autoresearch--tool-nelisp-codegen-harvest
    (batch_id &optional wait max_candidates)
  "MCP wrapper for `anvil-autoresearch-nelisp-codegen-harvest'.

MCP Parameters:
  batch_id       - Orchestrator implementation batch id.
  wait           - Optional truthy flag to wait before collecting.
  max_candidates - Optional maximum number of summaries to rank."
  (anvil-autoresearch-nelisp-codegen-harvest
   batch_id wait max_candidates))


(defun anvil-autoresearch--tool-routing-evaluate
    (&optional prompt policies candidates since_sec min_samples)
  "MCP wrapper for `anvil-autoresearch-routing-evaluate'.

MCP Parameters:
  prompt      - Optional representative prompt text.
  policies    - Optional comma-separated policy names.
  candidates  - Optional comma-separated provider list.
  since_sec   - Optional lookback window in seconds.
  min_samples - Optional per-provider sample floor."
  (anvil-autoresearch-routing-evaluate
   prompt policies candidates since_sec min_samples))

(defun anvil-autoresearch--tool-routing-plan
    (&optional prompt policies candidates since_sec min_samples)
  "MCP wrapper for `anvil-autoresearch-routing-plan'.

MCP Parameters:
  prompt      - Optional representative prompt text.
  policies    - Optional comma-separated policy names.
  candidates  - Optional comma-separated provider list.
  since_sec   - Optional lookback window in seconds.
  min_samples - Optional per-provider sample floor."
  (anvil-autoresearch-routing-plan
   prompt policies candidates since_sec min_samples))

(defun anvil-autoresearch--tool-routing-propose
    (&optional prompt policies candidates since_sec min_samples provider model
              budget_usd timeout_sec heartbeat_timeout_sec cwd sandbox
              no_worktree worktree_name)
  "MCP wrapper for `anvil-autoresearch-routing-propose'.

MCP Parameters:
  prompt                - Optional representative prompt text.
  policies              - Optional comma-separated policy names.
  candidates            - Optional comma-separated provider list.
  since_sec             - Optional lookback window in seconds.
  min_samples           - Optional per-provider sample floor.
  provider              - Optional orchestrator provider string.
  model                 - Optional provider model string.
  budget_usd            - Optional per-task budget.
  timeout_sec           - Optional wall-clock timeout.
  heartbeat_timeout_sec - Optional no-output heartbeat timeout.
  cwd                   - Optional repo/worktree directory.
  sandbox               - Optional provider sandbox string.
  no_worktree           - Optional truthy worktree opt-out flag.
  worktree_name         - Optional explicit worktree name hint."
  (anvil-autoresearch-routing-propose
   prompt policies candidates since_sec min_samples provider model
   budget_usd timeout_sec heartbeat_timeout_sec cwd sandbox
   no_worktree worktree_name))

(defun anvil-autoresearch--tool-routing-harvest
    (batch_id &optional wait max_candidates)
  "MCP wrapper for `anvil-autoresearch-routing-harvest'.

MCP Parameters:
  batch_id       - Orchestrator routing proposal batch id.
  wait           - Optional truthy flag to wait before collecting.
  max_candidates - Optional maximum number of proposals to rank."
  (anvil-autoresearch-routing-harvest
   batch_id wait max_candidates))

(defun anvil-autoresearch--tool-routing-patch-plan
    (&optional proposal_json batch_id wait max_candidates)
  "MCP wrapper for `anvil-autoresearch-routing-patch-plan'.

MCP Parameters:
  proposal_json  - Optional harvest JSON or selected proposal JSON.
  batch_id       - Optional routing proposal batch id.
  wait           - Optional truthy flag for batch harvest.
  max_candidates - Optional maximum number of proposals to rank."
  (anvil-autoresearch-routing-patch-plan
   proposal_json batch_id wait max_candidates))

(defun anvil-autoresearch--tool-routing-implement
    (&optional proposal_json batch_id wait max_candidates provider model
              budget_usd timeout_sec heartbeat_timeout_sec cwd sandbox
              no_worktree worktree_name)
  "MCP wrapper for `anvil-autoresearch-routing-implement'.

MCP Parameters:
  proposal_json         - Optional harvest JSON or selected proposal JSON.
  batch_id              - Optional routing proposal batch id.
  wait                  - Optional truthy flag for batch harvest.
  max_candidates        - Optional maximum number of proposals to rank.
  provider              - Optional orchestrator provider string.
  model                 - Optional provider model string.
  budget_usd            - Optional per-task budget.
  timeout_sec           - Optional wall-clock timeout.
  heartbeat_timeout_sec - Optional no-output heartbeat timeout.
  cwd                   - Optional repo/worktree directory.
  sandbox               - Optional provider sandbox string.
  no_worktree           - Optional truthy worktree opt-out flag.
  worktree_name         - Optional explicit worktree name hint."
  (anvil-autoresearch-routing-implement
   proposal_json batch_id wait max_candidates provider model
   budget_usd timeout_sec heartbeat_timeout_sec cwd sandbox
   no_worktree worktree_name))

(defun anvil-autoresearch--tool-routing-implementation-harvest
    (batch_id &optional wait max_candidates)
  "MCP wrapper for `anvil-autoresearch-routing-implementation-harvest'.

MCP Parameters:
  batch_id       - Orchestrator routing implementation batch id.
  wait           - Optional truthy flag to wait before collecting.
  max_candidates - Optional maximum number of summaries to rank."
  (anvil-autoresearch-routing-implementation-harvest
   batch_id wait max_candidates))

(defun anvil-autoresearch--tool-routing-implementation-promote-memory
    (run_json &optional name type memory_description tags_json)
  "MCP wrapper for `anvil-autoresearch-routing-implementation-promote-memory'.

MCP Parameters:
  run_json           - JSON result from routing implementation harvest.
  name               - Optional memory basename.
  type               - Optional memory type.
  memory_description - Optional short memory description.
  tags_json          - Optional JSON array of string tags."
  (anvil-autoresearch-routing-implementation-promote-memory
   run_json name type memory_description tags_json))


;;;; --- MCP registration ---------------------------------------------------

(defconst anvil-autoresearch--tool-specs
  `((,(anvil-server-encode-handler #'anvil-autoresearch--tool-scan)
     :id "autoresearch-scan"
     :intent (research discovery)
     :layer workflow
     :description
     "Read-only Doc 43 Phase 0 scanner for local web / X / YouTube
summaries and anvil design docs.  Returns ranked implementation
candidates with file, line, source kind, evidence, score, and a
suggested action.  Optional `query' filters the result; optional
`roots_json' overrides scan roots with a JSON path array; optional
`limit' caps returned candidates.  No LLM calls and no file mutation."
     :read-only t)
    (,(anvil-server-encode-handler #'anvil-autoresearch--tool-docstrings)
     :id "autoresearch-docstrings"
     :intent (research discovery)
     :layer workflow
     :description
     "Read-only Doc 43 Phase 1a evaluator for MCP tool docstring
autoresearch.  Scans the registered MCP tool manifest and ranks
weak description/docstring targets by short descriptions, generic
phrasing, missing usage guidance, missing id-token coverage, and
missing/short parameter descriptions.  Optional `query' filters
the result; optional `server_id' restricts to one MCP server;
optional `limit' caps returned candidates.  No LLM calls and no
file mutation."
     :read-only t)
    (,(anvil-server-encode-handler #'anvil-autoresearch--tool-docstring-plan)
     :id "autoresearch-docstring-plan"
     :intent (research discovery)
     :layer workflow
     :description
     "Read-only Doc 43 Phase 1b planner for MCP tool docstring
autoresearch.  Given `tool_id' plus optional `server_id' (or no
tool id, which selects the top `autoresearch-docstrings' candidate),
returns current description, scored issues, parameter docs,
registration-site hint, deterministic draft description, and a
rewrite prompt.  No LLM calls, no patch application, and no file
mutation."
     :read-only t)
    (,(anvil-server-encode-handler #'anvil-autoresearch--tool-docstring-patch)
     :id "autoresearch-docstring-patch"
     :intent (research discovery)
     :layer workflow
     :description
     "Read-only Doc 43 Phase 1c dry-run patch generator for MCP tool
docstring autoresearch.  Given optional `tool_id' / `server_id' /
`new_description', locates the registered `:description' string
in source and returns replacement bounds, old/new Elisp string
literals, and a unified-diff style patch candidate.  Empty
`new_description' uses the deterministic draft from
`autoresearch-docstring-plan'.  No file mutation, no LLM calls,
and no tests are run."
     :read-only t)
    (,(anvil-server-encode-handler #'anvil-autoresearch--tool-docstring-apply)
     :id "autoresearch-docstring-apply"
     :intent (research edit)
     :layer workflow
     :description
     "Doc 43 Phase 2 local apply/eval/holdout loop for MCP tool docstring
autoresearch.  Given optional `tool_id' / `server_id' /
`new_description', applies exactly one registered `:description'
string literal rewrite, then optionally runs `eval_command' and
`holdout_command'.  A non-zero eval exit restores the original
file unless `keep_on_eval_failure' is truthy; a non-zero holdout
exit always restores it.  Use after reviewing
`autoresearch-docstring-patch'; this tool mutates source files but
does not call an LLM."
     :read-only nil)
    (,(anvil-server-encode-handler #'anvil-autoresearch--tool-docstring-run)
     :id "autoresearch-docstring-run"
     :intent (research edit)
     :layer workflow
     :description
     "Doc 43 Phase 2a multi-candidate local loop for MCP tool docstring
autoresearch.  Given optional `tool_id' / `server_id' /
`candidates_json', tries each candidate description against the same
original source snapshot, runs optional `eval_command' and
`holdout_command' gates, scores passing rewrites by remaining
docstring issues, and keeps only the best passing candidate.  Empty
`candidates_json' uses the deterministic draft.  This tool mutates
source files but does not call an LLM."
     :read-only nil)
    (,(anvil-server-encode-handler #'anvil-autoresearch--tool-docstring-propose)
     :id "autoresearch-docstring-propose"
     :intent (research orchestration)
     :layer workflow
     :description
     "Doc 43 Phase 1e proposal entrance for MCP tool docstring
autoresearch.  Given optional `tool_id' / `server_id', builds the
same rewrite plan as `autoresearch-docstring-plan' and submits one
bounded orchestrator task to propose a replacement description.
Optional `provider', `model', `budget_usd', `timeout_sec',
`heartbeat_timeout_sec', `cwd', `sandbox', `no_worktree', and
`worktree_name' override the
defaults.  Returns the orchestrator batch id and exact task plist.
This tool starts an async orchestrator task but does not apply a
patch itself."
     :read-only nil)
    (,(anvil-server-encode-handler
       #'anvil-autoresearch--tool-docstring-propose-batch)
     :id "autoresearch-docstring-propose-batch"
     :intent (research orchestration)
     :layer workflow
     :description
     "Doc 43 Phase 2c proposal fan-out for MCP tool docstring
autoresearch.  Given optional `tool_id' / `server_id' / `count',
submits multiple bounded orchestrator tasks in one batch, with unique
task names and candidate-number prompt hints.  Budget, timeout,
heartbeat timeout, cwd, sandbox, no-worktree, and worktree-name
parameters apply per task.  Pass the returned `batch_id' to
`autoresearch-docstring-harvest' to collect and evaluate proposals.
This tool starts async orchestrator tasks but does not apply patches
itself."
     :read-only nil)
    (,(anvil-server-encode-handler #'anvil-autoresearch--tool-docstring-harvest)
     :id "autoresearch-docstring-harvest"
     :intent (research edit orchestration)
     :layer workflow
     :description
     "Doc 43 Phase 2b proposal harvest for MCP tool docstring
autoresearch.  Given an orchestrator `batch_id', collects successful
task summaries as candidate descriptions, optionally waits for the
batch, then delegates to `autoresearch-docstring-run' so each proposal
is checked by eval / holdout gates and only the lowest issue-score
passing rewrite is kept.  If no usable proposal summaries exist, no
source file is edited.  This tool may mutate source files but does
not itself call an LLM."
     :read-only nil)
    (,(anvil-server-encode-handler
       #'anvil-autoresearch--tool-docstring-promote-memory)
     :id "autoresearch-docstring-promote-memory"
     :intent (research memory)
     :layer workflow
     :description
     "Doc 43 Phase 2e memory promotion for accepted MCP tool docstring
autoresearch results.  Given a JSON result from
`autoresearch-docstring-run' or `autoresearch-docstring-harvest',
extracts the selected candidate description and writes one DB-direct
anvil-memory entry via `anvil-memory-add'.  Optional `name', `type',
`memory_description', and `tags_json' control the stored memory row.
This tool writes memory state but does not edit source files or call
an LLM."
     :read-only nil)
    (,(anvil-server-encode-handler #'anvil-autoresearch--tool-routing-evaluate)
     :id "autoresearch-routing-evaluate"
     :intent (research discovery orchestration)
     :layer workflow
     :description
     "Read-only Doc 43 Phase 3 evaluator for Q1-B orchestrator routing
autoresearch.  Runs `anvil-orchestrator-select-provider' across one
or more policies and reports provider choices, cold-start/no-provider
issues, sampled-candidate weakness, policy disagreement, and a compact
score.  Optional `prompt', `policies', `candidates', `since_sec', and
`min_samples' scope the dry run.  No routing settings are changed and
no orchestrator task is submitted."
     :read-only t)
    (,(anvil-server-encode-handler #'anvil-autoresearch--tool-routing-plan)
     :id "autoresearch-routing-plan"
     :intent (research discovery orchestration)
     :layer workflow
     :description
     "Read-only Doc 43 Phase 3 planner for Q1-B orchestrator routing
autoresearch.  Wraps `autoresearch-routing-evaluate' and returns a
prompt for a future proposal to improve routing defaults, candidate
sets, min-samples, cold-start handling, or stats interpretation.  It
does not submit tasks and does not mutate routing settings."
     :read-only t)
    (,(anvil-server-encode-handler #'anvil-autoresearch--tool-routing-propose)
     :id "autoresearch-routing-propose"
     :intent (research orchestration)
     :layer workflow
     :description
     "Doc 43 Phase 3a proposal entrance for Q1-B orchestrator routing
autoresearch.  Wraps `autoresearch-routing-plan' and submits one
bounded async orchestrator task to propose a small routing improvement
around policy defaults, candidate sets, min-samples, cold-start
handling, or stats interpretation.  Optional provider, model, budget,
timeout, heartbeat timeout, cwd, sandbox, no-worktree, and
worktree-name parameters override the proposal defaults.  It returns
the orchestrator batch id and exact task plist, but does not mutate
routing settings or apply any proposal."
     :read-only nil)
    (,(anvil-server-encode-handler #'anvil-autoresearch--tool-routing-harvest)
     :id "autoresearch-routing-harvest"
     :intent (research discovery orchestration)
     :layer workflow
     :description
     "Read-only Doc 43 Phase 3b harvest/decision helper for Q1-B
orchestrator routing autoresearch.  Given a batch id from
`autoresearch-routing-propose' or a compatible proposal batch, collects
successful task summaries, ranks routing improvement proposals with a
small deterministic heuristic, and returns the selected review
candidate.  It does not mutate routing settings, source files, provider
registries, or task queues."
     :read-only t)
    (,(anvil-server-encode-handler
       #'anvil-autoresearch--tool-routing-patch-plan)
     :id "autoresearch-routing-patch-plan"
     :intent (research discovery orchestration)
     :layer workflow
     :description
     "Read-only Doc 43 Phase 3c dry-run patch planner for Q1-B
orchestrator routing autoresearch.  Given a selected proposal JSON
object from `autoresearch-routing-harvest', or a batch id to harvest
first, returns target-file hints, an implementation prompt, and
byte-compile / ERT verification commands for a focused routing patch.
It does not edit source files, mutate routing settings, or apply the
proposal."
     :read-only t)
    (,(anvil-server-encode-handler #'anvil-autoresearch--tool-routing-implement)
     :id "autoresearch-routing-implement"
     :intent (research orchestration)
     :layer workflow
     :description
     "Doc 43 Phase 3d implementation entrance for Q1-B orchestrator
routing autoresearch.  Builds the same dry-run plan as
`autoresearch-routing-patch-plan' from selected proposal JSON or a
proposal batch id, then submits one bounded async orchestrator task
with provider/model/budget/timeout/heartbeat/cwd/sandbox/worktree
metadata.  It returns the implementation batch id, exact task plist,
and plan.  This tool starts an async implementation task but does not
edit source files itself."
     :read-only nil)
    (,(anvil-server-encode-handler
       #'anvil-autoresearch--tool-routing-implementation-harvest)
     :id "autoresearch-routing-implementation-harvest"
     :intent (research discovery orchestration)
     :layer workflow
     :description
     "Read-only Doc 43 Phase 3e implementation harvest/review helper
for Q1-B orchestrator routing autoresearch.  Given a batch id from
`autoresearch-routing-implement' or a compatible implementation batch,
collects successful task summaries, ranks implementation reports with
a deterministic review heuristic, and returns the selected summary plus
manual review checklist.  It does not apply patches, edit source files,
or mutate routing settings."
     :read-only t)
    (,(anvil-server-encode-handler
       #'anvil-autoresearch--tool-routing-implementation-promote-memory)
     :id "autoresearch-routing-implementation-promote-memory"
     :intent (research memory)
     :layer workflow
     :description
     "Doc 43 Phase 3f memory promotion for accepted Q1-B routing
implementation summaries.  Given JSON from
`autoresearch-routing-implementation-harvest', or one selected
candidate object with a summary field, writes one DB-direct
anvil-memory entry via `anvil-memory-add'.  Optional name, type,
memory_description, and tags_json control the stored memory row.  This
tool writes memory state but does not edit source files or mutate
routing settings."
     :read-only nil)
    (,(anvil-server-encode-handler
       #'anvil-autoresearch--tool-nelisp-codegen-plan)
     :id "autoresearch-nelisp-codegen-plan"
     :intent (research discovery orchestration)
     :layer workflow
     :description
     "Read-only Doc 43 Phase 5 planner for applying autoresearch to
NeLisp Phase 47 codegen.  Resolves the NeLisp repository root, reports
target compiler / test / design files, and returns byte-compile plus
ERT holdout commands and the implementation prompt used by
`autoresearch-nelisp-codegen-implement'.  Optional focus narrows the
experiment.  No source files are edited and no orchestrator task is
submitted."
     :read-only t)
    (,(anvil-server-encode-handler
       #'anvil-autoresearch--tool-nelisp-codegen-implement)
     :id "autoresearch-nelisp-codegen-implement"
     :intent (research orchestration)
     :layer workflow
     :description
     "Doc 43 Phase 5 implementation entrance for NeLisp Phase 47 codegen
autoresearch.  Builds a focused codegen plan and submits one bounded
orchestrator task with provider/model/budget/timeout/heartbeat/cwd/
sandbox/worktree metadata.  The task prompt requires Phase 47
byte-identity / byte-length invariant preservation and focused tests.
This tool starts an async implementation task but does not edit NeLisp
source files itself."
     :read-only nil)
    (,(anvil-server-encode-handler
       #'anvil-autoresearch--tool-nelisp-codegen-harvest)
     :id "autoresearch-nelisp-codegen-harvest"
     :intent (research discovery orchestration)
     :layer workflow
     :description
     "Read-only Doc 43 Phase 5 harvest/review helper for NeLisp Phase 47
codegen autoresearch.  Given a batch id from
`autoresearch-nelisp-codegen-implement', collects successful task
summaries, ranks implementation reports with a deterministic codegen
heuristic, and returns the selected summary plus manual review
checklist.  It does not apply patches or mutate NeLisp."
     :read-only t))
  "Spec list consumed by `anvil-server-register-tools'.")

(defun anvil-autoresearch--register-tools ()
  "Register autoresearch MCP tools."
  (anvil-server-register-tools anvil-autoresearch--server-id
                               anvil-autoresearch--tool-specs))

(defun anvil-autoresearch--unregister-tools ()
  "Unregister autoresearch MCP tools."
  (anvil-server-unregister-tools anvil-autoresearch--server-id
                                 anvil-autoresearch--tool-specs))

;;;###autoload
(defun anvil-autoresearch-enable ()
  "Register autoresearch MCP tools."
  (interactive)
  (anvil-autoresearch--register-tools))

;;;###autoload
(defun anvil-autoresearch-disable ()
  "Unregister autoresearch MCP tools."
  (interactive)
  (anvil-autoresearch--unregister-tools))

(provide 'anvil-autoresearch)
;;; anvil-autoresearch.el ends here
