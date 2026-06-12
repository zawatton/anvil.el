;;; anvil-eval.el --- Elisp evaluation tools for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides sync and async Elisp evaluation as MCP tools.
;; - `emacs-eval' — synchronous evaluation (< timeout)
;; - `emacs-eval-async' — fire-and-forget, returns job ID
;; - `emacs-eval-result' — poll async job result
;; - `emacs-eval-jobs' — list all async jobs
;;
;; Also integrates tool-call timeout, org-mode fast-mode, and
;; request serialization mutex into the server core.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)

;;; Customization

(defgroup anvil-eval nil
  "Anvil Elisp evaluation tools."
  :group 'anvil
  :prefix "anvil-eval-")

(defcustom anvil-eval-timeout 30
  "Hard timeout in seconds for synchronous MCP tool calls.
Tools exceeding this are killed.  Use async variant for longer ops."
  :type 'integer
  :group 'anvil-eval)

(defcustom anvil-eval-org-fast-mode t
  "When non-nil, minimize `org-mode' setup during MCP tool calls.
Skips mode hooks, disables font-lock and org-element-cache.
Improves performance ~4x on large org files."
  :type 'boolean
  :group 'anvil-eval)

(defcustom anvil-eval-nelisp-timeout 30
  "Hard timeout in seconds for synchronous `nelisp-eval' tool calls."
  :type 'integer
  :group 'anvil-eval)

(defcustom anvil-eval-nelisp-source-directory nil
  "Directory containing pure Elisp NeLisp sources, or nil for auto-detect.
The directory must contain `nelisp-read.el', `nelisp-eval.el', and
`nelisp-macro.el'."
  :type '(choice (const :tag "Auto-detect" nil) directory)
  :group 'anvil-eval)

(defcustom anvil-eval-nelisp-reset-before-eval nil
  "When non-nil, reset pure Elisp NeLisp state before each `nelisp-eval' call.
The default nil keeps MCP calls REPL-like: definitions from previous
calls remain available in NeLisp's own global/function tables."
  :type 'boolean
  :group 'anvil-eval)

;;; Tool timeout

(defun anvil-eval--timeout-advice (orig-fn &rest args)
  "Wrap MCP tool call dispatch with `with-timeout' as a safety net."
  (with-timeout
      (anvil-eval-timeout
       (anvil-server-tool-throw
        (format "MCP tool call exceeded %ds timeout — use async variant"
                anvil-eval-timeout)))
    (apply orig-fn args)))

;;; Org-mode fast mode

(defvar anvil-eval--in-org-fast-mode nil
  "Non-nil when MCP tool is running with fast org-mode.
Set automatically for the duration of each MCP tool call.")

(defun anvil-eval--org-mode-fast-advice (orig-fn &rest args)
  "Make `org-mode' setup minimal when called inside an MCP tool."
  (if (and anvil-eval-org-fast-mode anvil-eval--in-org-fast-mode
           ;; Skip the fast path for user agenda files: those buffers
           ;; outlive the MCP tool call, and `(font-lock-mode -1)' on
           ;; them sets `font-lock-mode-set-explicitly' so
           ;; `global-font-lock-mode' never re-enables fontification —
           ;; the buffer stays uncoloured for the rest of the session.
           (not (and buffer-file-name
                     (member (expand-file-name buffer-file-name)
                             (mapcar #'expand-file-name
                                     (org-agenda-files t t))))))
      (progn
        ;; setq-local, not let: Emacs 30's define-derived-mode calls
        ;; (make-local-variable 'delay-mode-hooks) inside org-mode, which
        ;; errors if the variable is already let-bound.  Making it
        ;; buffer-local first turns that call into a harmless no-op.
        (setq-local delay-mode-hooks t)
        (apply orig-fn args)
        (when (fboundp 'font-lock-mode)
          (font-lock-mode -1))
        (when (boundp 'org-element-use-cache)
          (setq-local org-element-use-cache nil)))
    (apply orig-fn args)))

(defun anvil-eval--org-fast-mode-wrapper (orig-fn &rest args)
  "Enable fast org-mode for the duration of any MCP tool call."
  (let ((anvil-eval--in-org-fast-mode t))
    (apply orig-fn args)))

;;; Request serialization mutex

(defvar anvil-eval--request-mutex
  (when (fboundp 'make-mutex)
    (make-mutex "anvil-request"))
  "Mutex serializing MCP request dispatch.
Prevents state corruption from re-entrant calls via yield points.
Only available in Emacs with thread support.")

(defun anvil-eval--request-mutex-advice (orig-fn &rest args)
  "Serialize MCP request dispatch via a mutex."
  (if anvil-eval--request-mutex
      (with-mutex anvil-eval--request-mutex
        (apply orig-fn args))
    (apply orig-fn args)))

;;; Sync eval tool

(defun anvil-eval--sync (expression)
  "Evaluate EXPRESSION as Emacs Lisp and return the result as string.

  Synchronous evaluation with timeout protection.
  For long-running operations, use emacs-eval-async instead.

  MCP Parameters:
    expression - Emacs Lisp expression to evaluate (string, required)
                 Example: \"(elfeed-update)\"
                 Example: \"(buffer-list)\""
  (anvil-server-with-error-handling
   (let* ((form (car (read-from-string expression)))
          (result (eval form t)))
     (format "%S" result))))

;;; NeLisp eval tool (pure Elisp REPL)

(defvar anvil-eval--nelisp-source-cache 'unset
  "Cached result of `anvil-eval--locate-nelisp-source-directory'.")

(defvar anvil-eval--nelisp-source-cache-key nil
  "Candidate directory list used for `anvil-eval--nelisp-source-cache'.")

(defvar anvil-eval--nelisp-loaded nil
  "Non-nil once the pure Elisp NeLisp evaluator has been loaded.")

(declare-function nelisp--reset "nelisp-eval" ())
(declare-function nelisp-eval-string "nelisp-eval" (str))

(defun anvil-eval--nelisp-source-candidates ()
  "Return candidate pure Elisp NeLisp source directories."
  (list anvil-eval-nelisp-source-directory
        (getenv "NELISP_ELISP_DIR")
        (expand-file-name "Cowork/Notes/dev/nelisp/src"
                          (or (getenv "HOME") "~"))
        (expand-file-name "Notes/dev/nelisp/src"
                          (or (getenv "HOME") "~"))))

(defun anvil-eval--nelisp-source-directory-p (dir)
  "Return non-nil when DIR contains the pure Elisp NeLisp evaluator."
  (let ((expanded (and dir (expand-file-name dir))))
    (and expanded
         (file-readable-p (expand-file-name "nelisp-read.el" expanded))
         (file-readable-p (expand-file-name "nelisp-eval.el" expanded))
         (file-readable-p (expand-file-name "nelisp-macro.el" expanded))
         expanded)))

(defun anvil-eval-nelisp-clear-source-cache ()
  "Clear cached pure Elisp NeLisp source discovery state."
  (interactive)
  (setq anvil-eval--nelisp-source-cache 'unset)
  (setq anvil-eval--nelisp-source-cache-key nil))

(defun anvil-eval--locate-nelisp-source-directory ()
  "Return directory containing pure Elisp NeLisp sources, or nil."
  (let ((candidates (anvil-eval--nelisp-source-candidates)))
    (if (and (not (eq anvil-eval--nelisp-source-cache 'unset))
             (equal candidates anvil-eval--nelisp-source-cache-key))
        anvil-eval--nelisp-source-cache
      (let ((found (cl-some #'anvil-eval--nelisp-source-directory-p
                            candidates)))
        (setq anvil-eval--nelisp-source-cache-key candidates)
        (setq anvil-eval--nelisp-source-cache found)))))

(defun anvil-eval--ensure-nelisp ()
  "Load the pure Elisp NeLisp evaluator and initialize REPL state."
  (let ((src-dir (anvil-eval--locate-nelisp-source-directory)))
    (unless src-dir
      (error "pure Elisp nelisp sources not found — set anvil-eval-nelisp-source-directory or NELISP_ELISP_DIR"))
    (let ((load-path (cons src-dir load-path)))
      (require 'nelisp-eval)
      (require 'nelisp-macro))
    (unless anvil-eval--nelisp-loaded
      (nelisp--reset)
      (setq anvil-eval--nelisp-loaded t))))

(defun anvil-eval--nelisp-host (expression)
  "Evaluate EXPRESSION through the pure Elisp NeLisp evaluator."
  (anvil-eval--ensure-nelisp)
  (when (and anvil-eval-nelisp-reset-before-eval
             (fboundp 'nelisp--reset))
    (nelisp--reset))
  (prin1-to-string (nelisp-eval-string expression)))

(defun anvil-eval--reset-nelisp-host ()
  "Reset the pure Elisp NeLisp evaluator and keep it loaded."
  (anvil-eval--ensure-nelisp)
  (nelisp--reset)
  (setq anvil-eval--nelisp-loaded t)
  "nelisp-eval reset")

(defun anvil-eval--nelisp (expression)
  "Evaluate EXPRESSION with the pure Elisp NeLisp evaluator.

This runs inside the host Emacs daemon, but through NeLisp's own
reader/evaluator and NeLisp-owned global/function tables.  Calls are
stateful by default so the MCP tool behaves like a practical REPL.

MCP Parameters:
  expression - NeLisp / Elisp expression to evaluate (string, required)
               Example: \"(+ 1 2 3)\"
               Example: \"(length \\\"hello\\\")\""
  (anvil-server-with-error-handling
   (with-timeout (anvil-eval-nelisp-timeout
                  (error "nelisp-eval timed out after %ds"
                         anvil-eval-nelisp-timeout))
     (anvil-eval--nelisp-host expression))))

(defun anvil-eval--nelisp-reset ()
  "Reset the stateful pure Elisp NeLisp REPL.

Use this when a previous `nelisp-eval' call intentionally changed
globals/functions or when recovering from a bad exploratory session."
  (anvil-server-with-error-handling
   (with-timeout (anvil-eval-nelisp-timeout
                  (error "nelisp-eval reset timed out after %ds"
                         anvil-eval-nelisp-timeout))
     (anvil-eval--reset-nelisp-host))))

;;; Async eval tools

(defvar anvil-eval--async-jobs (make-hash-table :test 'equal)
  "Hash table mapping job-id to async job plists.
Each job records :status, :result, :expression and :start-time.
Once the timer starts evaluating the form, :run-start-time is set;
completed jobs also carry :finish-time, :queue-wait-sec and
:runtime-sec.")

(defvar anvil-eval--async-counter 0
  "Counter for generating unique job IDs.")

(defun anvil-eval--async (expression)
  "Evaluate EXPRESSION asynchronously via run-with-timer.
Returns a job ID immediately.  The expression runs in the next
event loop iteration, so Emacs remains responsive.
Use emacs-eval-result to retrieve the result.

MCP Parameters:
  expression - Emacs Lisp expression to evaluate asynchronously
               (string, required)
               Example: \"(byte-compile-file \\\"~/.emacs.d/init.el\\\")\"
               Use for long-running operations that would timeout with
               emacs-eval."
  (anvil-server-with-error-handling
    (let* ((job-id (format "job-%d-%d"
                           (setq anvil-eval--async-counter
                                 (1+ anvil-eval--async-counter))
                           (truncate (float-time))))
           (form (car (read-from-string expression))))
      (puthash job-id
               (list :status 'running
                     :result nil
                     :expression expression
                     :start-time (current-time))
               anvil-eval--async-jobs)
      (run-with-timer 0 nil
        (lambda ()
          (let ((job (gethash job-id anvil-eval--async-jobs)))
            (when job
              (let ((run-start (current-time))
                    status result)
                (setq job (plist-put job :run-start-time run-start))
                (condition-case err
                    (setq result (format "%S" (eval form t))
                          status 'done)
                  (error
                   (setq result (format "Error: %S" err)
                         status 'error)))
                (let ((finish (current-time)))
                  (setq job (plist-put job :status status))
                  (setq job (plist-put job :result result))
                  (setq job (plist-put job :finish-time finish))
                  (setq
                   job
                   (plist-put
                    job :queue-wait-sec
                    (float-time
                     (time-subtract run-start
                                    (plist-get job :start-time)))))
                  (setq
                   job
                   (plist-put
                    job :runtime-sec
                    (float-time
                     (time-subtract finish run-start))))
                  (puthash job-id job anvil-eval--async-jobs)))))))
      (format "Job started: %s" job-id))))

(defun anvil-eval--format-seconds (seconds)
  "Format SECONDS as a compact duration string, or return \"N/A\"."
  (if (numberp seconds)
      (format "%.1fs" seconds)
    "N/A"))

(defun anvil-eval--result (job-id)
  "Get the result of an async job.
Returns status, age, queue wait, runtime and result.  Completed
results auto-clean after 10 minutes.

MCP Parameters:
  job-id - The job ID returned by emacs-eval-async (string, required)
           Example: \"job-1-1711843200\""
  (anvil-server-with-error-handling
    (let ((job (gethash job-id anvil-eval--async-jobs)))
      (if (not job)
          (format "Job not found: %s (may have been cleaned up)" job-id)
        (let ((status (plist-get job :status))
              (result (plist-get job :result))
              (now (current-time)))
          (let* ((start-time (plist-get job :start-time))
                 (run-start (plist-get job :run-start-time))
                 (finish (plist-get job :finish-time))
                 (elapsed (float-time
                           (time-subtract now start-time)))
                 (queue-wait
                  (or (plist-get job :queue-wait-sec)
                      (and run-start
                           (float-time
                            (time-subtract run-start start-time)))
                      elapsed))
                 (runtime
                  (or (plist-get job :runtime-sec)
                      (and run-start
                           (float-time
                            (time-subtract (or finish now)
                                           run-start))))))
          (when (and (not (eq status 'running))
                     (> elapsed 600))
            (remhash job-id anvil-eval--async-jobs))
          (format
           (concat "status: %s\n"
                   "elapsed: %.1fs\n"
                   "age: %.1fs\n"
                   "queue-wait: %s\n"
                   "runtime: %s\n"
                   "result: %s")
           status elapsed elapsed
           (anvil-eval--format-seconds queue-wait)
           (anvil-eval--format-seconds runtime)
           (or result "N/A"))))))))

(defun anvil-eval--jobs ()
  "List all async jobs and their statuses.

MCP Parameters: (none)"
  (anvil-server-with-error-handling
    (let ((jobs '()))
      (maphash
       (lambda (id job)
         (push (format "%s: %s (%.1fs) - %s"
                       id
                       (plist-get job :status)
                       (float-time
                        (time-subtract (current-time)
                                       (plist-get job :start-time)))
                       (truncate-string-to-width
                        (or (plist-get job :expression) "") 50))
               jobs))
       anvil-eval--async-jobs)
      (if jobs
          (mapconcat #'identity (nreverse jobs) "\n")
        "No async jobs."))))

;;; Module enable/disable

(defvar anvil-eval--server-id "anvil"
  "Server ID used when registering eval tools.")

(defun anvil-eval-enable ()
  "Register eval tools and install advice."
  (setq anvil-eval--server-id
        (or (and (boundp 'anvil-server-id) anvil-server-id) "anvil"))
  ;; Install advice
  (advice-add 'anvil-server--handle-tools-call
              :around #'anvil-eval--timeout-advice)
  (advice-add 'anvil-server--handle-tools-call
              :around #'anvil-eval--org-fast-mode-wrapper)
  (advice-add 'org-mode :around #'anvil-eval--org-mode-fast-advice)
  (advice-add 'anvil-server-process-jsonrpc
              :around #'anvil-eval--request-mutex-advice)
  ;; Register tools
  (anvil-server-register-tool
   #'anvil-eval--sync
   :id "emacs-eval"
   :intent '(eval)
   :layer 'dev
   :description
   "Evaluate Emacs Lisp expression synchronously and return the result.
Use for quick operations (< 30s): querying state, small edits,
reading data. For heavy operations use emacs-eval-async instead."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--async
   :id "emacs-eval-async"
   :intent '(eval)
   :layer 'dev
   :description
   "Evaluate Emacs Lisp expression asynchronously.
Returns a job ID immediately. Emacs remains responsive during execution.
Use for long-running operations: git clone, byte-compile, package install.
Retrieve result with emacs-eval-result tool using the returned job ID."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--result
   :id "emacs-eval-result"
   :intent '(eval admin)
   :layer 'dev
   :description
   "Get the result of an async job started by emacs-eval-async.
Returns status (running/done/error), age, queue wait, runtime, and result.
Poll this until status is 'done' or 'error'."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--jobs
   :id "emacs-eval-jobs"
   :intent '(eval admin)
   :layer 'dev
   :description
   "List all async jobs and their statuses.
Useful for checking what's running or debugging stuck jobs."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--nelisp
   :id "nelisp-eval"
   :intent '(eval)
   :layer 'dev
   :description
   "Evaluate expression with the pure Elisp NeLisp evaluator and
return the printed result.  Uses NeLisp's reader/evaluator and keeps
NeLisp globals across calls, so it behaves as a practical MCP REPL."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--nelisp-reset
   :id "nelisp-eval-reset"
   :intent '(eval admin)
   :layer 'dev
   :description
   "Reset the stateful pure Elisp NeLisp REPL used by nelisp-eval."
   :server-id anvil-eval--server-id))

(defun anvil-eval-disable ()
  "Unregister eval tools and remove advice."
  (anvil-server-unregister-tool "emacs-eval" anvil-eval--server-id)
  (anvil-server-unregister-tool "emacs-eval-async" anvil-eval--server-id)
  (anvil-server-unregister-tool "emacs-eval-result" anvil-eval--server-id)
  (anvil-server-unregister-tool "emacs-eval-jobs" anvil-eval--server-id)
  (anvil-server-unregister-tool "nelisp-eval" anvil-eval--server-id)
  (anvil-server-unregister-tool "nelisp-eval-reset" anvil-eval--server-id)
  (advice-remove 'anvil-server--handle-tools-call
                 #'anvil-eval--timeout-advice)
  (advice-remove 'anvil-server--handle-tools-call
                 #'anvil-eval--org-fast-mode-wrapper)
  (advice-remove 'org-mode #'anvil-eval--org-mode-fast-advice)
  (advice-remove 'anvil-server-process-jsonrpc
                 #'anvil-eval--request-mutex-advice))

(provide 'anvil-eval)
;;; anvil-eval.el ends here
