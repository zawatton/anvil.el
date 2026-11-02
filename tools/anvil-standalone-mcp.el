;;; anvil-standalone-mcp.el --- Minimal Anvil MCP profile for NeLisp -*- lexical-binding: t; -*-

(let ((root (or (getenv "ANVIL_EL_DIR")
                (file-name-directory
                 (directory-file-name
                  (file-name-directory (or load-file-name buffer-file-name)))))))
  (add-to-list 'load-path root))

(defun anvil-standalone--load-json-source ()
  "Load the JSON provider source before MCP code can require its stale artifact."
  (let* ((override (getenv "ANVIL_STANDALONE_JSON_SOURCE"))
         (located (locate-library "json"))
         (source (or override
                     (and located
                          (if (string-match-p "\\.el\\'" located)
                              located
                            (concat (file-name-sans-extension located) ".el"))))))
    (unless (and source (file-readable-p source))
      (error "Cannot locate readable JSON provider source: %s" (or source located "json")))
    ;; Load the exact .el path with suffix search disabled, even if a newer
    ;; or stale .elc is also present beside it.
    (load source nil t t t)
    (unless (and (featurep 'json) (fboundp 'json-read-from-string))
      (error "JSON provider source did not define the json feature"))
    source))

(defvar anvil-standalone--json-source nil
  "Exact JSON provider source loaded by this profile.")
(setq anvil-standalone--json-source (anvil-standalone--load-json-source))

(defvar anvil-standalone--stdio-source nil
  "Exact stdio provider source loaded by this profile.")

(defun anvil-standalone--load-stdio-source ()
  "Load the exact stdio reader source, bypassing stale compiled files."
  (let* ((override (getenv "ANVIL_STANDALONE_STDIO_SOURCE"))
         (nelisp-dir (getenv "NELISP_EMACS_DIR"))
         (compat-source (and nelisp-dir
                             (expand-file-name "src/emacs-stdio.el" nelisp-dir)))
         (located (locate-library "emacs-stdio"))
         (source (or override
                     (and compat-source (file-readable-p compat-source)
                          compat-source)
                     (and located
                          (if (string-match-p "\\.el\\'" located)
                              located
                            (concat (file-name-sans-extension located) ".el"))))))
    (unless (and source (file-readable-p source))
      (error "Cannot locate readable stdio provider source: %s"
             (or source located "emacs-stdio")))
    (load source nil t t t)
    (unless (and (featurep 'emacs-stdio)
                 (fboundp 'emacs-stdio-read-line)
                 (fboundp 'emacs-stdio-read-bytes))
      (error "Stdio provider source did not define the reader API"))
    (setq anvil-standalone--stdio-source source)))

(defun anvil-standalone--call-with-stdio-reader (function)
  "Call FUNCTION with the chunked reader only on NeLisp standalone."
  (if (fboundp 'read-stdin-bytes)
      (progn
        (anvil-standalone--load-stdio-source)
        (let ((original (symbol-function 'read-from-minibuffer)))
          (unwind-protect
              (progn
                (fset 'read-from-minibuffer
                      (lambda (&rest _args)
                        (let ((line (emacs-stdio-read-line)))
                          (if (fboundp 'anvil-runtime-shell--multibyte)
                              (anvil-runtime-shell--multibyte line)
                            line))))
                (funcall function))
            (fset 'read-from-minibuffer original))))
    (funcall function)))

(load (expand-file-name
       "anvil-standalone-file-attributes.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil)

;; Load source explicitly so the public workspace adapter is never stale .elc.
(load (expand-file-name
       "anvil-workspace-mcp.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil)

(defvar anvil-standalone--profile-path load-file-name)
(defcustom anvil-standalone-context-timeout-sec 20
  "Maximum time for a standalone context worker, in seconds."
  :type 'integer
  :group 'anvil)

(defun anvil-standalone--worker-bin ()
  "Return the current standalone executable path."
  (or (getenv "NELISP_BIN")
      (executable-find "nelisp")
      (error "Cannot find the standalone nelisp executable")))

(defun anvil-standalone--run-context-worker (handler args)
  "Run HANDLER with ARGS in a bounded standalone child process."
  (unless (and (memq handler
                     '(anvil-standalone-context-handler
                       anvil-standalone-context-batch-handler
                       anvil-standalone-context-dependencies-handler
                       anvil-standalone-context-project-handler
                       anvil-standalone-defs-index-handler
                       anvil-standalone-defs-index-update-handler))
               (listp args)
               (<= (length args) 8)
               (cl-every (lambda (arg) (or (null arg) (stringp arg))) args)
               (integerp anvil-standalone-context-timeout-sec)
               (<= 1 anvil-standalone-context-timeout-sec 45))
    (error "Malformed standalone context worker request"))
  (let* ((timeout-bin (or (executable-find "timeout")
                          (error "Cannot find GNU timeout")))
         (env-bin (or (executable-find "env")
                      (error "Cannot find env")))
         (nelisp-bin (anvil-standalone--worker-bin))
         (stdout (make-temp-file "anvil-context-worker-out-"))
         (stderr (make-temp-file "anvil-context-worker-err-"))
         (seconds (number-to-string anvil-standalone-context-timeout-sec))
         (expr (format
                "(let ((result (apply #'%s '%S))) (princ %S) (prin1 result) (princ %S))"
                (symbol-name handler) args "\nANVIL-WORKER-RESULT:" "\n"))
         (exit-code nil)
         (output nil)
         (errors nil)
         (marker nil)
         (encoded nil)
         (prefix nil))
    (unwind-protect
        (progn
         (setq exit-code
                (call-process
                 timeout-bin nil nil nil "-k" "2s" seconds
                 "sh" "-c"
                 "out=$1; err=$2; shift 2; exec \"$@\" >\"$out\" 2>\"$err\""
                 "anvil-context-worker" stdout stderr env-bin
                 (concat "ANVIL_STANDALONE_CONTEXT_WORKER="
                         (symbol-name handler))
                 nelisp-bin "--load" anvil-standalone--profile-path
                 "--eval" expr))
          (setq output (with-temp-buffer
                         (insert-file-contents-literally stdout)
                         (buffer-string))
                errors (with-temp-buffer
                         (insert-file-contents-literally stderr)
                         (buffer-string)))
          (unless (and (integerp exit-code) (zerop exit-code))
            (error "Standalone context worker %s%s"
                   (if (memq exit-code '(124 137))
                       (format "timed out after %s seconds" seconds)
                     (format "exited with status %S" exit-code))
                   (if (equal errors "") ""
                     (concat ": " (substring (string-trim errors) 0
                                             (min 1024 (length (string-trim errors))))))))
          (setq marker (string-match (regexp-quote "ANVIL-WORKER-RESULT:") output)
                encoded (and marker
                             (substring output
                                        (+ marker (length "ANVIL-WORKER-RESULT:"))))
                prefix (and marker (substring output 0 marker)))
          (unless (and (equal errors "") marker
                       (string-match-p "\\`[ \t\n\r]*\\'" prefix))
            (error "Standalone context worker returned malformed output (marker=%S stderr=%S prefix=%S)"
                   marker (substring errors 0 (min 256 (length errors)))
                   (and prefix (substring prefix 0 (min 256 (length prefix))))))
          (let* ((parsed (condition-case nil
                             (read-from-string encoded)
                           (error nil))))
            (unless (and (consp parsed)
                         (string-match-p "\\`[ \t\n\r]*\\'"
                                         (substring encoded (cdr parsed))))
              (error "Standalone context worker returned a malformed result"))
            (car parsed)))
      (when (file-exists-p stdout) (delete-file stdout))
      (when (file-exists-p stderr) (delete-file stderr)))))

(defun anvil-standalone--context-worker-tool (handler args)
  "Apply HANDLER to ARGS using a bounded fresh runtime."
  (anvil-standalone--run-context-worker handler args))

;; These compatibility definitions are needed by the current standalone
;; runtime.  Metrics are outside this read-only profile, so delayed metrics
;; registration is intentionally omitted.
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory
    (expand-file-name ".emacs.d/" (or (getenv "HOME") "~"))))
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (_file &rest _body) nil))
(unless (fboundp 'called-interactively-p)
  (defun called-interactively-p (&optional _kind) nil))
(unless (fboundp 'system-name)
  (defun system-name ()
    "Return the host name from the runtime environment."
    (or (getenv "HOSTNAME")
        (getenv "COMPUTERNAME")
        (getenv "HOST")
        "localhost")))
(unless (fboundp 'documentation)
  (defun documentation (function &optional _raw)
    (let ((symbol (if (and (symbolp function) (fboundp function)) function)))
      (and symbol (get symbol 'function-documentation)))))
(unless (fboundp 'find-buffer-visiting)
  (defun find-buffer-visiting (_file) nil))
(unless (fboundp 'locate-dominating-file)
  (defun locate-dominating-file (file name)
    (let* ((expanded (expand-file-name file))
           (directory (file-name-as-directory
                       (if (file-directory-p expanded)
                           expanded
                         (file-name-directory expanded))))
           found parent)
      (while (and directory (not found))
        (when (if (functionp name)
                  (funcall name directory)
                (file-exists-p (expand-file-name name directory)))
          (setq found directory))
        (unless found
          (setq parent
                (file-name-directory (directory-file-name directory)))
          (setq directory
                (unless (or (null parent) (equal parent directory)) parent))))
      found)))
(unless (fboundp 'help-function-arglist)
  (defun help-function-arglist (function &optional _preserve-names)
    (let ((definition (if (symbolp function)
                          (symbol-function function)
                        function)))
      (or (condition-case nil (aref definition 0) (error nil))
          (and (consp definition) (eq (car definition) 'lambda)
               (cadr definition))
          (and (consp definition) (eq (car definition) 'closure)
               (caddr definition))))))

(unless (fboundp 'advice-add)
  (let* ((notes (getenv "ANVIL_NOTES_DIR"))
         (nelisp-root (and notes (expand-file-name "dev/nelisp" notes))))
    (when nelisp-root
      (add-to-list 'load-path (expand-file-name "src" nelisp-root))
      (add-to-list 'load-path
                   (expand-file-name "packages/nelisp-control-flow/src"
                                     nelisp-root))
      (require 'nelisp-control-flow)
      (defalias 'advice-add 'nelisp-advice-add)
      (defalias 'advice-remove 'nelisp-advice-remove)
      (defalias 'advice-member-p 'nelisp-advice-member-p))))

;; Timer fallbacks.  Older standalone runtimes provide `run-at-time',
;; `cancel-timer' and `timerp' (nelisp-async-core) but not the
;; `run-with-*' wrappers that several anvil modules call when enabled.
;; Newer NeLisp defines both natively, so these guards step aside there.
(unless (fboundp 'run-with-timer)
  (defun run-with-timer (secs repeat function &rest args)
    "Run FUNCTION with ARGS after SECS seconds, repeating every REPEAT.
Fallback for runtimes that lack a native `run-with-timer'."
    (apply #'run-at-time secs repeat function args)))

(unless (fboundp 'run-with-idle-timer)
  (defun run-with-idle-timer (secs repeat function &rest args)
    "Run FUNCTION with ARGS once idle for SECS seconds.
The standalone server has no user input and is therefore always idle, so
this approximates idleness with a plain timer.  When REPEAT is non-nil the
function runs every SECS seconds, with a 1.0 second floor so a missing or
non-positive SECS cannot spin."
    (let ((interval (and repeat
                         (if (and (numberp secs) (> secs 0)) secs 1.0))))
      (apply #'run-at-time secs interval function args))))

;; Load the current provider before Anvil can select a stale .elc.  NeLisp's
;; library lookup prefers bytecode independently of load-suffixes.
(let ((compat-dir (or (getenv "NELISP_EMACS_LIB_DIR")
                      (and (getenv "ANVIL_NOTES_DIR")
                           (expand-file-name "dev/nelisp-emacs-lib/src"
                                             (getenv "ANVIL_NOTES_DIR"))))))
  (when compat-dir
    (load (expand-file-name "emacs-char-table.el" compat-dir))))

(require 'anvil)
(require 'anvil-server-commands)
(require 'anvil-context)
;; The batch, dependency and project context modules are optional: a checkout
;; without them still serves every other tool.  Their tools register below
;; only when the feature loaded.
(require 'anvil-context-batch nil t)
(require 'anvil-context-dependencies nil t)

(let ((compat-dir (or (getenv "NELISP_EMACS_LIB_DIR")
                      (and (getenv "ANVIL_NOTES_DIR")
                           (expand-file-name "dev/nelisp-emacs-lib/src"
                                             (getenv "ANVIL_NOTES_DIR"))))))
  (when compat-dir
    (add-to-list 'load-path compat-dir)
    (load (expand-file-name "emacs-sqlite-ffi.el" compat-dir))
    (load (expand-file-name "emacs-textmodes-stub.el" compat-dir))))

;; SQLite defaults to no lock wait in the standalone FFI.  A short busy
;; timeout lets separate MCP processes initialize or read the same DBs.
(when (and (fboundp 'sqlite-open) (fboundp 'sqlite-execute)
           (not (fboundp 'anvil-standalone--sqlite-open)))
  (defalias 'anvil-standalone--sqlite-open (symbol-function 'sqlite-open))
  (defun sqlite-open (&optional file readonly disable-uri)
    (let ((db (anvil-standalone--sqlite-open file readonly disable-uri)))
      (sqlite-execute db "PRAGMA busy_timeout = 5000")
      db)))

(require 'anvil-defs)
(require 'anvil-context-project nil t)
(require 'anvil-disclosure)
(require 'anvil-memory)
(require 'anvil-worklog)

(let ((nso-dir (or (getenv "NSO_LISP_DIR")
                   (and (getenv "ANVIL_NOTES_DIR")
                        (expand-file-name "dev/nelisp-system-one/lisp"
                                          (getenv "ANVIL_NOTES_DIR"))))))
  (when nso-dir
    (add-to-list 'load-path nso-dir)
    (load (expand-file-name "nso-context.el" nso-dir))))

(load (expand-file-name "anvil-standalone-fast.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(when (getenv "ANVIL_STANDALONE_DEBUG")
  (setq anvil-server--debug-trace t))

;; The profile has a fixed tool set.  Avoid the standalone serializer's
;; cumulative schema-cache rewrite, which is unnecessary at process startup.
(setq anvil-server--schema-cache-loaded t
      anvil-server--schema-cache nil)
(fset 'anvil-server--schema-cache-put
      '(lambda (_id _signature _schema _fragment) nil))

;; The current standalone cl-loop does not accept the destructuring/by
;; combination used by anvil-server-register-tools.  Keep its public
;; registration semantics with a portable plist walk for optional modules.
(defun anvil-server-register-tools (server-id specs)
  "Register every HANDLER/PROPS SPEC under SERVER-ID."
  (let (ids)
    (dolist (spec specs)
      (unless (and (consp spec) (functionp (car spec)))
        (error "anvil-server-register-tools: malformed spec %S" spec))
      (let ((tail (cdr spec)) props)
        (while tail
          (unless (cdr tail)
            (error "anvil-server-register-tools: malformed spec %S" spec))
          (unless (eq (car tail) :server-id)
            (setq props (append props (list (car tail) (cadr tail)))))
          (setq tail (cddr tail)))
        (let* ((final (append (list :server-id server-id) props))
               (id (plist-get final :id)))
          (apply #'anvil-server-register-tool (car spec) final)
          (push id ids))))
    (nreverse ids)))

(fset 'anvil-standalone-context-handler
      '(lambda (path query &optional budget-chars diagnostic-limit)
         (anvil-context--tool path query budget-chars diagnostic-limit)))
(fset 'anvil-standalone-context-batch-handler
      '(lambda (requests &optional encoding)
         (anvil-context-batch--tool requests encoding)))
(fset 'anvil-standalone-context-dependencies-handler
      '(lambda (path symbol &optional budget-chars depth additional-paths direction)
         (anvil-context-dependencies--tool
          path symbol budget-chars depth additional-paths direction)))
(fset 'anvil-standalone-context-project-handler
      '(lambda (path symbol root &optional budget-chars depth direction diagnostic-limit discovery-mode)
         (anvil-context-project--tool
          path symbol root budget-chars depth direction diagnostic-limit discovery-mode)))
(fset 'anvil-standalone-defs-index-handler
      '(lambda (query &optional limit)
         (anvil-server-with-error-handling
          (format "%S" (anvil-disclosure-defs-index query limit)))))
(fset 'anvil-standalone-defs-index-update-handler
      '(lambda (paths &optional mode)
         (anvil-defs--tool-index-update paths mode)))

;; Slow source scans run in a fresh child so timeout kills the entire worker
;; process group while this MCP server remains able to accept later requests.
(unless (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
  (fset 'anvil-standalone-context-handler
        '(lambda (path query &optional budget-chars diagnostic-limit)
           (anvil-standalone--context-worker-tool
            'anvil-standalone-context-handler
            (list path query budget-chars diagnostic-limit))))
  (fset 'anvil-standalone-context-batch-handler
        '(lambda (requests &optional encoding)
           (anvil-standalone--context-worker-tool
            'anvil-standalone-context-batch-handler (list requests encoding))))
  (fset 'anvil-standalone-context-dependencies-handler
        '(lambda (path symbol &optional budget-chars depth additional-paths direction)
           (anvil-standalone--context-worker-tool
            'anvil-standalone-context-dependencies-handler
            (list path symbol budget-chars depth additional-paths direction))))
  (fset 'anvil-standalone-context-project-handler
        '(lambda (path symbol root &optional budget-chars depth direction diagnostic-limit discovery-mode)
           (anvil-standalone--context-worker-tool
            'anvil-standalone-context-project-handler
            (list path symbol root budget-chars depth direction diagnostic-limit discovery-mode))))
  (fset 'anvil-standalone-defs-index-handler
        '(lambda (query &optional limit)
           (anvil-standalone--context-worker-tool
            'anvil-standalone-defs-index-handler (list query limit))))
  (fset 'anvil-standalone-defs-index-update-handler
        '(lambda (paths &optional mode)
           (anvil-standalone--context-worker-tool
            'anvil-standalone-defs-index-update-handler (list paths mode)))))

(anvil-server-register-tool
 #'anvil-standalone-context-handler :id "code-context" :server-id "emacs-eval"
 :intent '(file-read) :layer 'core :read-only t
 :schema '((type . "object")
           (properties . (("path" . ((description . "Local source file.") (type . "string")))
                          ("query" . ((description . "Literal search terms separated by spaces.") (type . "string")))
                          ("budget-chars" . ((description . "Optional excerpt character budget, default 6000, max 32000.") (type . "string")))
                          ("diagnostic-limit" . ((description . "Optional omitted-range detail limit, 0..128, default 20.") (type . "string")))))
           (required . ["path" "query"]))
 :description "Retrieve matching code excerpts within a character budget. Local lexical baseline through nelisp-system-one; reports omitted counts and ranges with optional diagnostic-limit. Use for targeted source inspection, not complete review or instructions.")
(when (featurep 'anvil-context-batch)
  (anvil-server-register-tool
   #'anvil-standalone-context-batch-handler :id "code-context-batch" :server-id "emacs-eval"
   :intent '(file-read) :layer 'core :read-only t
   :schema '((type . "object")
             (properties . (("requests" . ((description . "JSON array string of 1..16 bounded context requests.") (type . "string")))
                            ("encoding" . ((description . "Optional standard or shared-excerpts encoding.") (type . "string")))))
             (required . ["requests"]))
   :description "Retrieve bounded code contexts in one call; identical requests share results through NSO. Optional shared-excerpts encoding pools exact overlapping excerpts. Little overlap can increase size. Local lexical scorer only; file-attribute checks are not a snapshot."))
(when (featurep 'anvil-context-dependencies)
  (anvil-server-register-tool
   #'anvil-standalone-context-dependencies-handler :id "code-context-dependencies"
   :server-id "emacs-eval" :intent '(file-read) :layer 'core :read-only t
   :schema '((type . "object")
             (properties . (("path" . ((description . "Local Elisp source file.") (type . "string")))
                            ("symbol" . ((description . "Exact function name.") (type . "string")))
                            ("budget-chars" . ((description . "Optional excerpt budget, default 6000, maximum 32000.") (type . "string")))
                            ("depth" . ((description . "Optional dependency depth, default 1, maximum 3.") (type . "string")))
                            ("additional-paths" . ((description . "Optional JSON array string of up to seven local Elisp paths.") (type . "string")))
                            ("direction" . ((description . "Optional callees or callers direction.") (type . "string")))))
             (required . ["path" "symbol"]))
   :description "Retrieve an exact Elisp function and bounded callees or callers from up to eight explicit files, each at most 64 MiB. Large files use bounded top-level definition scanning. All reached definitions must fit. No implicit directory search or dynamic call resolution."))
(when (featurep 'anvil-context-project)
  (anvil-server-register-tool
   #'anvil-standalone-context-project-handler :id "code-context-project"
   :server-id "emacs-eval" :intent '(file-read) :layer 'core :read-only t
   :schema '((type . "object")
             (properties . (("path" . ((description . "Local primary Elisp file defining the symbol.") (type . "string")))
                            ("symbol" . ((description . "Exact function name.") (type . "string")))
                            ("root" . ((description . "Explicit local project directory restricting discovered files.") (type . "string")))
                            ("budget-chars" . ((description . "Optional excerpt budget, default 6000, maximum 32000.") (type . "string")))
                            ("depth" . ((description . "Optional graph depth, default 1, maximum 3.") (type . "string")))
                            ("direction" . ((description . "Optional callees or callers direction.") (type . "string")))
                            ("diagnostic-limit" . ((description . "Optional maximum detailed discovery issues, 0..128.") (type . "string")))
                            ("discovery-mode" . ((description . "Optional indexed or source discovery mode.") (type . "string")))))
             (required . ["path" "symbol" "root"]))
   :description "Retrieve dependencies through NSO within an explicit root. Default indexed discovery uses indexed locations and reverse edges; optional source mode scans at most eight Elisp files without the DB to find new callers and unindexed files. Bounded, partial, no index rebuild."))
(anvil-server-register-tool
 #'anvil-standalone-defs-index-handler :id "defs-index"
 :server-id "emacs-eval" :intent '(elisp-read structure) :layer 'core :read-only t
 :schema '((type . "object")
           (properties . (("query" . ((description . "Symbol-name substring to match.") (type . "string")))
                          ("limit" . ((description . "Optional result limit, default 50, maximum 200.") (type . "string")))))
           (required . ["query"]))
 :description "Return slim indexed Elisp definition pointers. An empty result does not prove absence.")
(anvil-server-register-tool
 #'anvil-standalone-defs-index-update-handler :id "defs-index-update"
 :server-id "emacs-eval" :intent '(elisp-read admin) :layer 'workflow
 :schema '((type . "object")
           (properties . (("paths" . ((description . "JSON array string of one to eight explicit local paths.") (type . "string")))
                          ("mode" . ((description . "Optional full or functions scan mode.") (type . "string")))))
           (required . ["paths"]))
 :description "Add or rescan explicit Elisp paths in the shared index while preserving unrelated files.")

(anvil-server-register-tool
 #'anvil-workspace-mcp--tool :id "workspace" :server-id "emacs-eval"
 :intent '(file-read file-write sqlite-write) :layer 'workflow :read-only nil
 :schema '((type . "object")
           (properties . (("action" . ((description . "Public action: configure, sync, status, capabilities, get, search, aggregate, edit, create, or operation.") (type . "string")))
                          ("options-json" . ((description . "Action options as a JSON object string.") (type . "string")))))
           (required . ["action" "options-json"]))
 :description "Query or configure the local workspace projection and queue revision-checked edits for the Emacs owner. Edit/create responses report queue state; they do not claim file persistence until the operation reports saved.")

;; Only Lisp state belongs in the image. Native DB handles are process-local.
(unless (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
  (require 'nl-ffi-memory)
  (anvil-memory--register-tools)
  (anvil-worklog--register-tools)
  ;; Lint and cron tools hold no native handles, so they live in the image.
  (require 'anvil-lint)
  (require 'anvil-cron)
  (anvil-lint-enable)
  (anvil-cron-enable)
  ;; File tools; `anvil-file-enable' also re-enables the context tools, which
  ;; are already registered above with worker-split handlers, so stub those out.
  (defvar anvil-standalone--file-tools-error nil
    "Error that kept the file tools from loading, or nil.")
  ;; anvil-file needs Org, which loads only with the nelisp-emacs-lib
  ;; provider (ANVIL_NOTES_DIR / NELISP_EMACS_LIB_DIR).  Without it, skip the
  ;; file tools: a failed attempt half-installs the runtime's own buffer
  ;; layer and breaks other tools (anvil-shell), so do not even try.
  (condition-case err
      (when (or (getenv "NELISP_EMACS_LIB_DIR") (getenv "ANVIL_NOTES_DIR"))
        (require 'anvil-file)
        (let ((saved (mapcar (lambda (f) (cons f (symbol-function f)))
                             '(anvil-context-enable anvil-context-batch-enable
                               anvil-context-dependencies-enable
                               anvil-context-project-enable))))
          (dolist (c saved) (fset (car c) (lambda () nil)))
          (unwind-protect (anvil-file-enable)
            (dolist (c saved) (fset (car c) (cdr c)))))
        ;; Offload children do not load anvil-file here; run these
        ;; tools in-process.
        (let ((tools (gethash "emacs-eval" anvil-server--tools)))
          (dolist (id '("file-outline" "code-extract-pattern"
                        "code-add-field-by-map"))
            (puthash id (plist-put (gethash id tools) :offload nil) tools))))
    ;; No `message': on NeLisp it writes to stdout, the MCP channel.
    (error (setq anvil-standalone--file-tools-error err))))

;; Modules whose enable step needs the anvil-state KV store.  The enable
;; functions open the DB handle and install a daily idle vacuum timer; a
;; native DB handle cannot live in the image and the daemon should not run
;; a vacuum, so stub `anvil-state-enable' while registering.  The state DB
;; then opens lazily on the first tool call (see `anvil-standalone-start').
(defvar anvil-standalone--state-tools-errors nil
  "Alist of (MODULE . ERROR) for state-backed modules that failed to load.")
(unless (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
  (condition-case err
      (require 'anvil-state)
    (error (push (cons 'anvil-state err) anvil-standalone--state-tools-errors)))
  (when (fboundp 'anvil-state-enable)
    (let ((saved (symbol-function 'anvil-state-enable)))
      (fset 'anvil-state-enable (lambda () nil))
      (unwind-protect
          (dolist (module '(anvil-compact anvil-session anvil-shell-filter))
            (condition-case err
                (progn (require module)
                       (funcall (intern (format "%s-enable" module))))
              ;; No `message': on NeLisp it writes to stdout, the MCP channel.
              (error (push (cons module err)
                           anvil-standalone--state-tools-errors))))
        (fset 'anvil-state-enable saved)))))

;; Port 6: agent, data, dev, discovery, harness-telemetry, session-store.
;; Registered under the same "emacs-eval" server id the loader serves.  The
;; telemetry enable step opens its SQLite handle eagerly; a native handle
;; cannot live in the image, so stub the opener while registering (it opens
;; lazily on the first tool call, see `anvil-standalone-start').
(defvar anvil-standalone--port6-errors nil
  "Alist of (MODULE . ERROR) for port-6 modules that failed to load.")
(unless (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
  ;; anvil-data needs the nelisp-emacs-lib checkout, like anvil-file: with the
  ;; runtime's bundled buffer layer its require half-loads and breaks
  ;; anvil-shell (`before-change-functions' void).
  (dolist (module (append '(anvil-agent anvil-dev anvil-discovery
                            anvil-harness-telemetry anvil-session-store)
                          (and (or (getenv "NELISP_EMACS_LIB_DIR")
                                   (getenv "ANVIL_NOTES_DIR"))
                               '(anvil-data))))
    (condition-case err
        (progn
          (require module)
          (if (eq module 'anvil-harness-telemetry)
              (let ((saved (symbol-function 'anvil-harness-telemetry--open)))
                (fset 'anvil-harness-telemetry--open (lambda () nil))
                (unwind-protect (anvil-harness-telemetry-enable)
                  (fset 'anvil-harness-telemetry--open saved)))
            (funcall (intern (format "%s-enable" module)))))
      ;; No `message': on NeLisp it writes to stdout, the MCP channel.
      (error (push (cons module err) anvil-standalone--port6-errors)))))

;; Port 7: bisect and cad.  Each module loads in its own condition-case so a
;; failure in one cannot hide the other.  Both register on "emacs-eval".
(defvar anvil-standalone--port7-errors nil
  "Alist of (MODULE . ERROR) for port-7 modules that failed to load.")
(unless (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
  (dolist (module '(anvil-bisect anvil-cad))
    (condition-case err
        (progn
          (require module)
          (funcall (intern (format "%s-enable" module))))
      ;; No `message': on NeLisp it writes to stdout, the MCP channel.
      (error (push (cons module err) anvil-standalone--port7-errors)))))

(defun anvil-standalone-start ()
  "Serve the prepared registry, opening databases on first use."
  (setq default-directory
        (file-name-as-directory (or (getenv "ANVIL_STANDALONE_CWD")
                                    default-directory)))
  (setq temporary-file-directory
        (file-name-as-directory (or (getenv "TMPDIR") temporary-file-directory)))
  ;; `user-emacs-directory' is the host Emacs directory, whose
  ;; anvil-state.db the host daemon owns.  Keep the standalone KV store apart.
  (setq anvil-state-db-path
        (or (getenv "ANVIL_STATE_DB")
            (expand-file-name
             "anvil-standalone/anvil-state.db"
             (or (getenv "XDG_CACHE_HOME")
                 (expand-file-name ".cache" (or (getenv "HOME") "~"))))))
  ;; Same isolation for the databases the port-6 modules write by default
  ;; (session-store under `user-emacs-directory'; telemetry in the worklog DB).
  (let ((cache (expand-file-name
                "anvil-standalone"
                (or (getenv "XDG_CACHE_HOME")
                    (expand-file-name ".cache" (or (getenv "HOME") "~"))))))
    (setq anvil-session-store-db-path
          (or (getenv "ANVIL_SESSION_STORE_DB")
              (expand-file-name "anvil-session-events.db" cache)))
    (setq anvil-harness-telemetry-db-path
          (or (getenv "ANVIL_HARNESS_TELEMETRY_DB")
              (expand-file-name "anvil-harness-telemetry.db" cache))))
  (setq anvil-server--debug-trace (and (getenv "ANVIL_STANDALONE_DEBUG") t))
  (anvil-standalone--call-with-stdio-reader
   (lambda () (anvil-server-run-batch-stdio "emacs-eval")))
  (nelisp--exit-process 0))

(unless (or (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
            (getenv "ANVIL_STANDALONE_TEST_NO_SERVER")
            (getenv "ANVIL_STANDALONE_BUILD_IMAGE"))
  (anvil-standalone-start))

;;; anvil-standalone-mcp.el ends here
