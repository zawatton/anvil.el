;;; anvil-openrouter-worker.el --- NeLisp OpenRouter worker -*- lexical-binding: t; -*-

(unless (fboundp 'nelisp-json-parse-string)
  (let* ((binary (file-truename (expand-file-name invocation-name invocation-directory)))
         (root (or (getenv "NELISP_ROOT")
                   (file-name-directory (directory-file-name (file-name-directory binary))))))
    (load (expand-file-name "packages/nelisp-json/src/nelisp-json.el" root))))

(defconst anvil-orw-daily-paid-limit 1000)
(defconst anvil-orw-daily-unpaid-limit 50)
(defconst anvil-orw-minute-limit 20)
(defconst anvil-orw-block-seconds 60)

(defun anvil-orw-read-file (path &optional max-bytes)
  (with-temp-buffer
    (insert-file-contents path nil 0 max-bytes)
    (buffer-string)))

(defun anvil-orw-write-file (path text)
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert text)))

(defun anvil-orw-json-read (text)
  (nelisp-json-parse-string text :object-type 'alist :array-type 'list
                            :null-object nil :false-object :json-false))

(defun anvil-orw-json-read-vectors (text)
  (nelisp-json-parse-string text :object-type 'alist :array-type 'vector
                            :null-object nil :false-object :json-false))

(defun anvil-orw-json-write (value)
  (nelisp-json-serialize value :null-object nil :false-object :json-false))

(defun anvil-orw-alist-get (key value)
  (cdr (assoc key value)))

(defun anvil-orw-api-key ()
  (or (let ((key (getenv "OPENROUTER_API_KEY")))
        (and key (not (string-empty-p (string-trim key))) (string-trim key)))
      (let ((path (expand-file-name ".hermes/.env" (or (getenv "HOME") "~"))))
        (when (file-readable-p path)
          (let ((lines (split-string (anvil-orw-read-file path) "\n")) found)
            (while (and lines (not found))
              (when (string-prefix-p "OPENROUTER_API_KEY=" (car lines))
                (setq found (string-trim (substring (car lines) 19) "[\"']" "[\"']")))
              (setq lines (cdr lines)))
            found)))))

(defun anvil-orw-state-path ()
  (expand-file-name "anvil/openrouter-quota.json"
                    (or (getenv "XDG_CACHE_HOME")
                        (expand-file-name ".cache" (or (getenv "HOME") "~")))))

(defun anvil-orw-default-state ()
  `((utc_date . ,(format-time-string "%Y-%m-%d" nil t))
    (launched_calls . 0) (successful_results . 0) (failed_calls . 0)
    (rate_limited_calls . 0) (recent_timestamps . ())
    (blocked_until . 0) (last_status . "new_day")))

(defun anvil-orw-wait (seconds)
  "Wait SECONDS while allowing the standalone process loop to make progress."
  (unless (fboundp 'accept-process-output)
    (error "accept-process-output is unavailable"))
  (accept-process-output nil seconds))

(defun anvil-orw-with-state (function)
  (let* ((path (anvil-orw-state-path)) (lock (concat path ".lock"))
         (tries 0) acquired state result)
    (make-directory (file-name-directory path) t)
    (while (and (not acquired) (< tries 100))
      (condition-case nil (progn (make-directory lock) (setq acquired t))
        (file-already-exists (anvil-orw-wait 0.05)))
      (setq tries (1+ tries)))
    (unless acquired (error "quota state lock timeout"))
    (unwind-protect
        (progn
          (setq state (condition-case nil
                          (anvil-orw-json-read (anvil-orw-read-file path))
                        (error (anvil-orw-default-state))))
          (unless (equal (anvil-orw-alist-get 'utc_date state)
                         (format-time-string "%Y-%m-%d" nil t))
            (setq state (anvil-orw-default-state)))
          (setq result (funcall function state))
          (let ((tmp (concat path ".tmp")))
            (anvil-orw-write-file tmp (anvil-orw-json-write state))
            (set-file-modes tmp #o600)
            (rename-file tmp path t))
          result)
      (ignore-errors (delete-directory lock)))))

(defun anvil-orw-call (program destination &rest args)
  (apply #'call-process program nil destination nil args))

(defun anvil-orw-curl-json (url &optional key)
  (let ((out (make-temp-file "anvil-openrouter-http-")))
    (unwind-protect
        (let ((args (append '("--fail-with-body" "--silent" "--show-error" "--max-time" "15")
                            (and key (list "-H" (concat "Authorization: Bearer " key)))
                            (list "-H" "Accept: application/json" url))))
          (unless (= 0 (apply #'anvil-orw-call "curl" out args))
            (error "OpenRouter request failed"))
          (anvil-orw-json-read (anvil-orw-read-file out)))
      (ignore-errors (delete-file out)))))

(defun anvil-orw-key-status (key)
  (let ((data (anvil-orw-alist-get 'data
                 (anvil-orw-curl-json "https://openrouter.ai/api/v1/key" key))))
    (mapcar (lambda (name) (cons name (anvil-orw-alist-get name data)))
            '(is_free_tier limit limit_remaining usage_daily rate_limit))))

(defun anvil-orw-daily-limit (remote)
  (if (eq (anvil-orw-alist-get 'is_free_tier remote) t)
      anvil-orw-daily-unpaid-limit anvil-orw-daily-paid-limit))

(defun anvil-orw-prune-recent (state now)
  (setcdr (assoc 'recent_timestamps state)
          (seq-filter (lambda (value) (and (numberp value) (> value (- now 60))))
                      (anvil-orw-alist-get 'recent_timestamps state))))

(defun anvil-orw-reserve (key)
  (let* ((remote (anvil-orw-key-status key))
         (limit (anvil-orw-daily-limit remote)) (now (float-time)))
    (anvil-orw-with-state
     (lambda (state)
       (anvil-orw-prune-recent state now)
       (let ((reason
              (cond ((> (anvil-orw-alist-get 'blocked_until state) now) "rate_limit_block")
                    ((>= (anvil-orw-alist-get 'launched_calls state) limit) "daily_limit")
                    ((>= (length (anvil-orw-alist-get 'recent_timestamps state))
                         anvil-orw-minute-limit) "minute_limit")
                    ((let ((remaining (anvil-orw-alist-get 'limit_remaining remote)))
                       (and (numberp remaining) (<= remaining 0))) "credit_limit"))))
         (if reason
             (progn (setcdr (assoc 'last_status state)
                            (concat "preflight_rejected:" reason))
                    `((allowed . :json-false) (error . ,reason)
                      (model_switch_allowed . :json-false)))
           (setcdr (assoc 'launched_calls state)
                   (1+ (anvil-orw-alist-get 'launched_calls state)))
           (setcdr (assoc 'recent_timestamps state)
                   (cons now (anvil-orw-alist-get 'recent_timestamps state)))
           (setcdr (assoc 'last_status state) "launched")
           `((allowed . t)
             (remaining_daily . ,(- limit (anvil-orw-alist-get 'launched_calls state)))
             (model_switch_allowed . :json-false))))))))

(defun anvil-orw-record (status)
  (anvil-orw-with-state
   (lambda (state)
     (let ((field (if (equal status "success") 'successful_results 'failed_calls)))
       (setcdr (assoc field state) (1+ (anvil-orw-alist-get field state))))
     (when (equal status "rate_limited")
       (setcdr (assoc 'rate_limited_calls state)
               (1+ (anvil-orw-alist-get 'rate_limited_calls state)))
       (setcdr (assoc 'blocked_until state) (+ (float-time) anvil-orw-block-seconds)))
     (setcdr (assoc 'last_status state) status))))

(defun anvil-orw-quota (key &optional set-launched)
  (when set-launched
    (unless (and (integerp set-launched) (<= 0 set-launched 1000))
      (error "set-launched must be 0..1000"))
    (anvil-orw-with-state
     (lambda (state)
       (setcdr (assoc 'launched_calls state) set-launched)
       (setcdr (assoc 'last_status state) "counter_initialized"))))
  (let* ((remote (anvil-orw-key-status key)) (limit (anvil-orw-daily-limit remote))
         (now (float-time)))
    (anvil-orw-with-state
     (lambda (state)
       (anvil-orw-prune-recent state now)
       `((remote . ,remote)
         (local . ((utc_date . ,(anvil-orw-alist-get 'utc_date state))
                   (launched_calls . ,(anvil-orw-alist-get 'launched_calls state))
                   (successful_results . ,(anvil-orw-alist-get 'successful_results state))
                   (failed_calls . ,(anvil-orw-alist-get 'failed_calls state))
                   (rate_limited_calls . ,(anvil-orw-alist-get 'rate_limited_calls state))
                   (last_status . ,(anvil-orw-alist-get 'last_status state))
                   (remaining_daily . ,(max 0 (- limit (anvil-orw-alist-get 'launched_calls state))))
                   (remaining_minute . ,(max 0 (- anvil-orw-minute-limit
                                                    (length (anvil-orw-alist-get 'recent_timestamps state)))))
                   (blocked_seconds . ,(max 0 (ceiling (- (anvil-orw-alist-get 'blocked_until state) now))))))
         (daily_free_limit . ,limit) (model_switch_allowed . :json-false))))))

(defun anvil-orw-option (args name &optional default)
  (let ((tail (member name args))) (if (and tail (cdr tail)) (cadr tail) default)))

(defun anvil-orw-config (model turns mcp context-only)
  `((model . ((provider . "openrouter") (default . ,model)))
    (fallback_providers . [])
    (agent . ((max_turns . ,turns) (reasoning_effort . "none")))
    (platform_toolsets . ((cli . ,(vconcat (append (unless context-only '("terminal" "file"))
                                                   (mapcar (lambda (x) (symbol-name (car x))) mcp))))))
    (compression . ((enabled . :json-false)))
    (memory . ((memory_enabled . :json-false) (user_profile_enabled . :json-false) (nudge_interval . 0)))
    (auxiliary . ((compression . ((provider . "openrouter") (model . ,model) (fallback_chain . [])))
                  (title_generation . ((enabled . :json-false) (model_upgrade_enabled . :json-false)
                                       (provider . "openrouter") (model . ,model) (fallback_chain . [])))
                  (background_review . ((enabled . :json-false) (provider . "openrouter")
                                        (model . ,model) (fallback_chain . [])))))
    (mcp_servers . ,mcp)))

(defun anvil-orw-contains-429 (home)
  (catch 'found
    (dolist (rel '("hermes_stdout.log" "hermes_stderr.log" "logs/errors.log" "logs/agent.log"))
      (let ((path (expand-file-name rel home)))
        (when (file-readable-p path)
          (let ((text (downcase (anvil-orw-read-file path 1048576))))
            (when (or (string-match-p "429" text) (string-match-p "too many requests" text)
                      (string-match-p "rate limit" text))
              (throw 'found t)))))) nil))

(defun anvil-orw-run (args key)
  (let* ((model (anvil-orw-option args "--model"))
         (prompt (anvil-orw-option args "--prompt-file"))
         (work (anvil-orw-option args "--work-dir"))
         (timeout (string-to-number (anvil-orw-option args "--timeout" "300")))
         (turns (string-to-number (anvil-orw-option args "--max-turns" "8")))
         (mcp-path (anvil-orw-option args "--anvil-config"))
         (context-only (member "--context-only" args)))
    (unless (and model (string-suffix-p ":free" model)
                 (not (string-match-p "[[:space:]]" model))) (error "invalid free model"))
    (unless (and prompt (file-readable-p prompt) work) (error "missing prompt or work directory"))
    (let ((reservation (anvil-orw-reserve key)))
      (unless (eq (anvil-orw-alist-get 'allowed reservation) t)
        (princ (concat (anvil-orw-json-write reservation) "\n"))
        (nelisp--exit-process 1)))
    (make-directory work t)
    (let* ((home (make-temp-file (expand-file-name "anvil-hermes-" work) t))
           (stdout (expand-file-name "hermes_stdout.log" home))
           (stderr (expand-file-name "hermes_stderr.log" home))
           (prompt-copy (expand-file-name "prompt.txt" home))
           (mcp (if mcp-path
                    (or (anvil-orw-alist-get 'mcp_servers
                          (anvil-orw-json-read-vectors (anvil-orw-read-file mcp-path)))
                        (error "anvil config missing mcp_servers")) '()))
           (tools (append (unless context-only '("terminal" "file"))
                          (mapcar (lambda (x) (symbol-name (car x))) mcp)))
           (cred (expand-file-name ".hermes/.env" (or (getenv "HOME") "~"))))
      (set-file-modes home #o700)
      (when (and (not (getenv "OPENROUTER_API_KEY")) (file-readable-p cred))
        (make-symbolic-link cred (expand-file-name ".env" home) t))
      (anvil-orw-write-file (expand-file-name "config.yaml" home)
                            (anvil-orw-json-write (anvil-orw-config model turns mcp context-only)))
      (anvil-orw-write-file prompt-copy (anvil-orw-read-file prompt))
      (let* ((argv (append (list (format "%ds" timeout) "env" (concat "HERMES_HOME=" home)
                                  "hermes" "chat" "--provider" "openrouter" "--model" model
                                  "--query-file" prompt-copy "--oneshot" "--format" "stream-json"
                                  "--toolsets" (string-join tools ",") "--reasoning" "none"
                                  "--max-turns" (number-to-string turns) "--run-budget"
                                  (number-to-string timeout))))
             status)
        (unwind-protect
            (progn
              (setq status (apply #'call-process "timeout" nil stdout nil argv))
              (if (not (= status 0))
                  (progn
                    (anvil-orw-record (if (anvil-orw-contains-429 home) "rate_limited" "hermes_error"))
                    (princ (concat (anvil-orw-json-write
                                    `((error . ,(if (anvil-orw-contains-429 home) "openrouter_429" "hermes_error"))
                                      (model_switch_allowed . :json-false) (artifacts . ,home))) "\n"))
                    (nelisp--exit-process 1))
                (let (result tool-use tool-result)
                  (dolist (line (split-string (anvil-orw-read-file stdout) "\n" t))
                    (when (string-prefix-p "{" (string-trim-left line))
                      (let* ((obj (anvil-orw-json-read (string-trim-left line)))
                             (type (anvil-orw-alist-get 'type obj)))
                        (cond ((equal type "tool_use") (setq tool-use (1+ (or tool-use 0))))
                              ((equal type "tool_result") (setq tool-result (1+ (or tool-result 0))))
                              ((equal type "result") (setq result obj))))))
                  (unless (and result (= (or (anvil-orw-alist-get 'exit_code result) 1) 0)
                               (stringp (anvil-orw-alist-get 'text result)))
                    (anvil-orw-record "result_error") (error "invalid Hermes result"))
                  (anvil-orw-record "success")
                  (princ (concat (anvil-orw-json-write
                                  `((type . "result") (result . ,(anvil-orw-alist-get 'text result))
                                    (usage . ,(or (anvil-orw-alist-get 'tokens result) '()))
                                    (session_id . ,(or (anvil-orw-alist-get 'session_id result) ""))
                                    (tool_use_count . ,(or tool-use 0))
                                    (tool_result_count . ,(or tool-result 0))
                                    (artifacts . ,home))) "\n"))))
          nil))))))

(defun anvil-orw-models (args)
  (let* ((limit (string-to-number (anvil-orw-option args "--limit" "12")))
         (data (anvil-orw-alist-get 'data (anvil-orw-curl-json "https://openrouter.ai/api/v1/models"))) out)
    (dolist (model data)
      (when (and (< (length out) limit)
                 (string-suffix-p ":free" (or (anvil-orw-alist-get 'id model) ""))
                 (member "tools" (anvil-orw-alist-get 'supported_parameters model)))
        (setq out (append out (list `((id . ,(anvil-orw-alist-get 'id model))
                                      (context_length . ,(anvil-orw-alist-get 'context_length model))))))))
    (princ (concat (anvil-orw-json-write
                    `((source . "https://openrouter.ai/api/v1/models") (models . ,(vconcat out)))) "\n"))))

(defun anvil-orw-main ()
  (let* ((raw (or (and (boundp 'nelisp-standalone-argv) nelisp-standalone-argv)
                  command-line-args-left))
         (args (cdr (member "--" raw))) (command (car args)) (rest (cdr args)))
    (condition-case err
        (cond ((equal command "models") (anvil-orw-models rest))
              ((equal command "quota")
               (let ((key (or (anvil-orw-api-key) (error "credentials missing")))
                     (set (anvil-orw-option rest "--set-launched")))
                 (princ (concat (anvil-orw-json-write
                                 (anvil-orw-quota key (and set (string-to-number set)))) "\n"))))
              ((equal command "run")
               (anvil-orw-run rest (or (anvil-orw-api-key) (error "credentials missing"))))
              (t (error "expected models, quota, or run")))
      (error (princ (concat (anvil-orw-json-write
                             `((error . ,(error-message-string err))
                               (model_switch_allowed . :json-false))) "\n"))
             (nelisp--exit-process 1)))))

(anvil-orw-main)
(nelisp--exit-process 0)
;;; anvil-openrouter-worker.el ends here
