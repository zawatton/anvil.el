;;; anvil-orchestrator-openrouter.el --- OpenRouter provider for anvil-orchestrator -*- lexical-binding: t; -*-

(require 'anvil-orchestrator)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup anvil-orchestrator-openrouter nil
  "OpenRouter provider for anvil-orchestrator."
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-openrouter-default-model "nex-agi/nex-n2.5-pro:free"
  "Default model identifier for OpenRouter provider.
Picked on measurement, 2026-09-23.  The same bounded task was sent to
three free tool-capable models: `nvidia/nemotron-3-ultra-550b-a55b:free'
answered \"Service temporarily overloaded\" on every 2026-09-22 attempt,
`qwen/qwen3.8-27b:free' hit HTTP 429 twice mid-conversation, and this
one completed it in 72 s with the correct answer.  Context is 262144
here against nemotron's 1000000, so a task needing more must name its
own model.  Re-measure before trusting this: free pools move."
  :type 'string
  :group 'anvil-orchestrator-openrouter)

(defcustom anvil-orchestrator-openrouter-context-only t
  "Whether OpenRouter workers run without `terminal' and `file' tools.
Non-nil leaves a worker with the read-only Anvil code-context MCP tools
alone, which is the safe default: it cannot edit the tree or run
commands.  It also cannot implement anything, and a model asked to do
so anyway tends to invent an answer rather than refuse (measured
2026-09-23), so a task that needs to write must say so.

A task overrides this per submission with a `:context-only' key
\(`context_only' over the MCP surface); the value there wins over this
default, including an explicit nil."
  :type 'boolean
  :group 'anvil-orchestrator-openrouter)

(defcustom anvil-orchestrator-openrouter-mcp-config
  (expand-file-name "anvil-mcp.json" "~/.hermes/")
  "Path to the Anvil MCP configuration JSON file passed to the helper script."
  :type 'file
  :group 'anvil-orchestrator-openrouter)

(defconst anvil-orchestrator--openrouter-helper
  (expand-file-name "tools/anvil-openrouter-worker.el"
                    (file-name-directory
                     (or (and (boundp 'load-file-name) load-file-name)
                         buffer-file-name))))

(defun anvil-orchestrator-openrouter--check ()
  "Verify NeLisp, Hermes, worker script, and MCP config are available."
  (unless (and (file-executable-p (anvil-orchestrator-openrouter--nelisp))
               (executable-find "hermes")
               (file-readable-p anvil-orchestrator--openrouter-helper)
               (file-readable-p anvil-orchestrator-openrouter-mcp-config))
    (user-error "Missing dependency: NeLisp, Hermes, worker script, or MCP config file (%s)"
                anvil-orchestrator-openrouter-mcp-config))
  t)

(defun anvil-orchestrator-openrouter--nelisp ()
  "Return the standalone NeLisp executable used by the worker."
  (or (getenv "NELISP_BIN")
      (executable-find "nelisp")
      (and (getenv "ANVIL_NOTES_DIR")
           (expand-file-name "dev/nelisp/target/nelisp" (getenv "ANVIL_NOTES_DIR")))
      (expand-file-name "../nelisp/target/nelisp"
                        (file-name-directory
                         (directory-file-name
                          (file-name-directory anvil-orchestrator--openrouter-helper))))))

(defun anvil-orchestrator-openrouter--nelisp-root ()
  "Return the repository root of the selected NeLisp executable."
  (file-name-directory
   (directory-file-name
    (file-name-directory (file-truename (anvil-orchestrator-openrouter--nelisp))))))

(defun anvil-orchestrator-openrouter--context-only-p (task)
  "Return non-nil when TASK must run without `terminal' / `file' tools.
An explicit key on TASK wins over
`anvil-orchestrator-openrouter-context-only', so a task can ask for
write-capable tools with `:context-only' nil.  Both the elisp spelling
and the `context_only' one that arrives over MCP are accepted."
  (cond ((plist-member task :context-only) (plist-get task :context-only))
        ((plist-member task :context_only) (plist-get task :context_only))
        (t anvil-orchestrator-openrouter-context-only)))

(defun anvil-orchestrator-openrouter--build-cmd (task)
  "Build command list for OpenRouter provider."
  (anvil-orchestrator-openrouter--check)
  (let* ((model (plist-get task :model))
         (prompt (plist-get task :prompt))
         (task-timeout (or (plist-get task :timeout-sec)
                           anvil-orchestrator-timeout-sec-default))
         ;; Leave the orchestrator time to collect and persist the worker's
         ;; own terminal record before its outer watchdog fires.
         (worker-timeout (max 1 (min 300 (- task-timeout 10))))
         (workdir anvil-orchestrator-work-dir))
    (cond
     ((null model)
      (setq model anvil-orchestrator-openrouter-default-model))
     ((and (stringp model) (string-match-p "\\`[^[:space:]]+:free\\'" model))
      ;; valid explicitly provided model
      )
     (t
      (user-error "Invalid model specified: '%s'. Must match pattern \\`[^[:space:]]+:free\\'" model)))
    (unless (and prompt
                 (stringp prompt)
                 (not (string-blank-p prompt)))
      (user-error "Task requires non-empty :prompt"))
    (make-directory workdir t)
    (let ((prompt-file (make-temp-file (expand-file-name "anvil-prompt-" workdir))))
      (with-temp-file prompt-file
        (let ((coding-system-for-write 'utf-8-unix))
          (insert prompt)))
      (append
       (list "env" (concat "NELISP_ROOT=" (anvil-orchestrator-openrouter--nelisp-root))
             (anvil-orchestrator-openrouter--nelisp)
             "--load" anvil-orchestrator--openrouter-helper "--" "run"
             "--model" model
             "--prompt-file" prompt-file
             "--work-dir" workdir
             "--timeout" (number-to-string worker-timeout))
       (when (anvil-orchestrator-openrouter--context-only-p task)
         (list "--context-only"))
       (list "--anvil-config" anvil-orchestrator-openrouter-mcp-config)))))

;;;###autoload
(defun anvil-orchestrator-openrouter-models (&optional limit)
  "Fetch available OpenRouter models via helper script.
LIMIT defaults to 12. Returns parsed JSON plist."
  (interactive)
  (unless (and (integerp limit)
               (<= 1 limit)
               (<= limit 100))
    (setq limit 12))
  (let ((buf (generate-new-buffer " *anvil-openrouter-models*")))
    (unwind-protect
        (with-current-buffer buf
          (let* ((process-environment
                  (cons (concat "NELISP_ROOT=" (anvil-orchestrator-openrouter--nelisp-root))
                        process-environment))
                 (exit (call-process
                       (anvil-orchestrator-openrouter--nelisp) nil buf nil
                       "--load" anvil-orchestrator--openrouter-helper "--"
                       "models" "--limit" (number-to-string limit))))
            (unless (eq exit 0)
              (user-error "Failed to fetch models: %s" (buffer-string)))
            (goto-char (point-min))
            (prog1
                (json-parse-buffer :object-type 'plist :array-type 'list)
              (when (called-interactively-p 'interactive)
                (message "OpenRouter models fetched")))))
      (kill-buffer buf))))

;;;###autoload
(defun anvil-orchestrator-openrouter-quota ()
  "Return the remote account status and conservative local request counters."
  (interactive)
  (let ((buf (generate-new-buffer " *anvil-openrouter-quota*")))
    (unwind-protect
        (with-current-buffer buf
          (let* ((process-environment
                  (cons (concat "NELISP_ROOT=" (anvil-orchestrator-openrouter--nelisp-root))
                        process-environment))
                 (exit (call-process
                       (anvil-orchestrator-openrouter--nelisp) nil buf nil
                       "--load" anvil-orchestrator--openrouter-helper "--" "quota")))
            (unless (eq exit 0)
              (user-error "Failed to fetch OpenRouter quota: %s" (buffer-string)))
            (goto-char (point-min))
            (prog1 (json-parse-buffer :object-type 'plist :array-type 'list)
              (when (called-interactively-p 'interactive)
                (message "OpenRouter quota checked")))))
      (kill-buffer buf))))

(defun anvil-orchestrator-openrouter--launch-capacity ()
  "Return an OpenRouter launch-admission plist from the worker quota API.
Daily and credit exhaustion are terminal for queued tasks, as is a 429
whose cooldown window has not yet elapsed.  Minute-window exhaustion
receives a bounded retry instead of busy polling."
  (let* ((quota (anvil-orchestrator-openrouter-quota))
         (remote (plist-get quota :remote))
         (local (plist-get quota :local))
         (remaining-daily (plist-get local :remaining_daily))
         (remaining-minute (plist-get local :remaining_minute))
         (blocked-seconds (plist-get local :blocked_seconds))
         (last-status (plist-get local :last_status))
         (credit (plist-get remote :limit_remaining)))
    (cond
     ((or (not (integerp remaining-daily))
          (not (integerp remaining-minute)))
      (list :capacity 0 :retry-after-sec 5))
     ;; A 429 is terminal only while its cooldown window is still open.
     ;; `last_status' keeps saying "rate_limited" until some worker run
     ;; rewrites it, and this preflight is what stops that run happening, so
     ;; keying on the status alone wedges the provider shut for good once one
     ;; 429 lands (measured 2026-09-23: blocked_seconds had been 0 for
     ;; minutes and every task still died before launch).  When the quota
     ;; payload carries no usable `blocked_seconds', fall back to the status
     ;; strings rather than assume the window has passed.
     ((if (numberp blocked-seconds)
          (> blocked-seconds 0)
        (or (equal last-status "rate_limited")
            (and (stringp last-status)
                 (string-match-p "rate_limit_block" last-status))))
      (list :capacity 0
            :terminal-reason "OpenRouter 429/cooldown observed; retry disabled"))
     ((or (<= remaining-daily 0)
          (equal last-status "preflight_rejected:daily_limit"))
      (list :capacity 0
            :terminal-reason "OpenRouter daily request allowance exhausted"))
     ((or (and (numberp credit) (<= credit 0))
          (equal last-status "preflight_rejected:credit_limit"))
      (list :capacity 0
            :terminal-reason "OpenRouter credit exhausted"))
     ((<= remaining-minute 0)
      (list :capacity 0 :retry-after-sec 60))
     (t (list :capacity (min remaining-daily remaining-minute))))))

(anvil-orchestrator-register-provider
 'openrouter
 :cli "hermes"
 :version-check #'anvil-orchestrator-openrouter--check
 :build-cmd #'anvil-orchestrator-openrouter--build-cmd
 :parse-output #'anvil-orchestrator--claude-parse-output
 :supports-tool-use t
 :supports-worktree nil
 :supports-budget nil
 :supports-system-prompt-append nil
 :default-model anvil-orchestrator-openrouter-default-model
 :launch-capacity #'anvil-orchestrator-openrouter--launch-capacity
 :disable-auto-retry t
 :cost-estimator (lambda (_task) 0.0))

(provide 'anvil-orchestrator-openrouter)
;;; anvil-orchestrator-openrouter.el ends here
