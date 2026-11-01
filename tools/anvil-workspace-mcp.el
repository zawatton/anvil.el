;;; anvil-workspace-mcp.el --- Public standalone workspace adapter -*- lexical-binding: t; -*-

(require 'json)

(defcustom anvil-workspace-mcp-timeout-sec 40
  "Maximum runtime for one public workspace worker request."
  :type 'integer
  :group 'anvil)

(defcustom anvil-workspace-mcp-output-limit-bytes (* 4 1024 1024)
  "Maximum JSON response size accepted from the workspace worker."
  :type 'integer
  :group 'anvil)

(defconst anvil-workspace-mcp--module-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst anvil-workspace-mcp--public-actions
  '("configure" "sync" "status" "capabilities" "get" "search"
    "aggregate" "edit" "create" "operation" "reproject"))

(defun anvil-workspace-mcp--worker-path ()
  "Return the configured worker path or the default sibling worker."
  (or (getenv "ANVIL_WORKSPACE_WORKER")
      (expand-file-name "anvil-workspace.py" anvil-workspace-mcp--module-dir)))

(defun anvil-workspace-mcp--tool (action options-json)
  "Call the public workspace ACTION with JSON object OPTIONS-JSON.

MCP Parameters:
  action - One of configure, sync, status, capabilities, get, search,
           aggregate, edit, create, operation, or reproject.
  options-json - Action options encoded as a JSON object string.

Reproject reads current database snapshots, including dirty documents. Its
optional wait_ms value (0..30000) bounds how long this request drains Org
semantic projections; it never changes source files or document revisions.

Edit and create requests are queued for the Emacs owner. A queued operation
does not mean that the buffer or file has been saved."
  (unless (and (member action anvil-workspace-mcp--public-actions)
               (stringp options-json)
               (<= (string-bytes options-json) (* 1024 1024))
               (string-match-p "\\`[ \t\r\n]*{" options-json)
               (integerp anvil-workspace-mcp-timeout-sec)
               (<= 1 anvil-workspace-mcp-timeout-sec 45)
               (integerp anvil-workspace-mcp-output-limit-bytes)
               (<= 1024 anvil-workspace-mcp-output-limit-bytes (* 8 1024 1024)))
    (error "Malformed or unsupported workspace request"))
  (let* ((options (condition-case nil
                      (json-parse-string options-json :object-type 'alist
                                         :array-type 'list)
                    (error :invalid-json)))
         (request (and (not (eq options :invalid-json))
                       (listp options)
                        ;; ACTION is a validated member of a fixed ASCII allowlist.
                        (format "{\"action\":\"%s\",\"options\":%s}"
                                action options-json))))
    (unless request (error "Workspace options must be a JSON object"))
    (let* ((worker (anvil-workspace-mcp--worker-path))
           (python (or (executable-find "python3")
                       (error "Cannot find python3")))
           (timeout-bin (or (executable-find "timeout")
                            (error "Cannot find GNU timeout")))
           (input (make-temp-file "anvil-workspace-in-"))
           (stdout (make-temp-file "anvil-workspace-out-"))
           (stderr (make-temp-file "anvil-workspace-err-"))
           (status nil)
           (response nil)
           (response-json nil)
           (errors nil))
      (unwind-protect
          (progn
            (with-temp-file input (insert request))
            ;; Keep shell syntax fixed; paths and executable names are argv values.
            (setq status
                  (call-process timeout-bin nil nil nil
                                "-k" "2s" (format "%ds" anvil-workspace-mcp-timeout-sec)
                                "sh" "-c"
                                "in=$1; out=$2; err=$3; shift 3; exec \"$@\" <\"$in\" >\"$out\" 2>\"$err\""
                                "anvil-workspace-worker" input stdout stderr python worker))
            (setq errors (with-temp-buffer
                           (insert-file-contents-literally stderr)
                           (buffer-string)))
            (unless (and (integerp status) (zerop status) (equal errors ""))
              (error "Workspace worker %s%s"
                     (if (memq status '(124 137)) "timed out" (format "exited %S" status))
                     (if (string-empty-p errors) ""
                       (concat ": " (substring (string-trim errors) 0
                                               (min 1000 (length (string-trim errors))))))))
            (when (> (file-attribute-size (file-attributes stdout))
                     anvil-workspace-mcp-output-limit-bytes)
              (error "Workspace worker exceeded the response size limit"))
            (with-temp-buffer
              (insert-file-contents-literally stdout)
              (let* ((bytes (buffer-string))
                     (decoded (decode-coding-string bytes 'utf-8-unix)))
                (unless (string-equal (string-as-unibyte bytes)
                                      (string-as-unibyte
                                       (encode-coding-string decoded 'utf-8-unix)))
                  (error "Workspace worker returned invalid UTF-8"))
                (setq response-json decoded
                      response (json-parse-string decoded :object-type 'alist
                                                  :array-type 'list))))
            (unless (and (listp response) (assq 'ok response)
                         (memq (alist-get 'ok response) '(t :false)))
              (error "Workspace worker returned an invalid response"))
            (when (eq (alist-get 'ok response) :false)
              (let ((failure (alist-get 'error response)))
                (unless (and (listp failure)
                             (stringp (alist-get 'code failure))
                             (stringp (alist-get 'message failure)))
                  (error "Workspace worker returned an invalid error object"))))
            response-json)
        (dolist (path (list input stdout stderr))
          (when (file-exists-p path) (delete-file path)))))))

(provide 'anvil-workspace-mcp)
;;; anvil-workspace-mcp.el ends here
