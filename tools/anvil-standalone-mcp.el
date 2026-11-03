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

;; A runtime built from a source tree that was later deleted keeps stale
;; entries in `load-path'.  Remap them onto a live NeLisp source root, silently.
(defun anvil-standalone--source-roots ()
  "Return candidate NeLisp source roots, most specific first."
  (let* ((env (getenv "ANVIL_NELISP_SOURCE_DIR"))
         (notes (getenv "ANVIL_NOTES_DIR"))
         (bin (and (boundp 'invocation-directory) (boundp 'invocation-name)
                   (stringp invocation-directory) (stringp invocation-name)
                   (expand-file-name invocation-name invocation-directory)))
         (bin (and bin (file-exists-p bin) (file-truename bin)))
         (checkout (and bin
                        (equal (file-name-nondirectory
                                (directory-file-name (file-name-directory bin)))
                               "target")
                        (file-name-directory
                         (directory-file-name (file-name-directory bin)))))
         (roots nil))
    (dolist (r (list env checkout
                     (and notes (expand-file-name
                                 "dev/nelisp-anvil-runtime/a6e710746" notes))))
      (when (and r (file-directory-p r))
        (push (file-name-as-directory (expand-file-name r)) roots)))
    (nreverse roots)))

(defun anvil-standalone--recover-load-path ()
  "Remap missing `load-path' entries onto a live source root."
  (unless (or (getenv "ANVIL_STANDALONE_NO_SOURCE_REMAP")
              (locate-library "json"))
    (let ((roots (anvil-standalone--source-roots)) (new nil))
      (dolist (p load-path)
        (if (and (stringp p) (file-directory-p p))
            (push p new)
          (when (stringp p)
            (let ((rel (replace-regexp-in-string
                        "\\`.*/src-[0-9a-f]+/?" "" p))
                  (hit nil))
              (dolist (r roots)
                (let ((cand (expand-file-name rel r)))
                  (when (and (not hit) (file-directory-p cand))
                    (setq hit cand))))
              (when hit (push hit new))))))
      (when new (setq load-path (nreverse new))))))
(anvil-standalone--recover-load-path)

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
                       anvil-standalone-defs-index-update-handler
                       anvil-standalone-defs-index-rebuild-handler
                       anvil-standalone-defs-clusters-handler
                       anvil-standalone-defs-architecture-handler
                       anvil-standalone-org-index-rebuild-handler))
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

;; `with-timeout' and `directory-files-recursively' are missing on standalone.
;; Both are ports of the GNU Emacs definitions (timer.el, files.el).  As in
;; GNU, the timeout is only noticed while the program waits (sit-for,
;; accept-process-output, ...); there is no preemption.
(defvar with-timeout-timers nil
  "List of all timers used by currently pending `with-timeout' calls.")

(unless (fboundp 'with-timeout)
  (defmacro with-timeout (list &rest body)
    "Run BODY, but if it doesn't finish in SECONDS seconds, give up.
If we give up, we run the TIMEOUT-FORMS and return the value of the last one.
The timeout is checked only while waiting for external events.
\n(fn (SECONDS TIMEOUT-FORMS...) BODY)"
    (declare (indent 1) (debug ((form body) body)))
    (let ((seconds (car list))
          (timeout-forms (cdr list))
          (timeout (make-symbol "timeout")))
      `(let ((-with-timeout-value-
              (catch ',timeout
                (let* ((-with-timeout-timer-
                        (run-with-timer ,seconds nil
                                        (lambda () (throw ',timeout ',timeout))))
                       (with-timeout-timers
                           (cons -with-timeout-timer- with-timeout-timers)))
                  (unwind-protect
                      (progn ,@body)
                    (cancel-timer -with-timeout-timer-))))))
         (if (eq -with-timeout-value- ',timeout)
             (progn ,@timeout-forms)
           -with-timeout-value-)))))

(unless (fboundp 'directory-files-recursively)
  (defun directory-files-recursively (dir regexp
                                          &optional include-directories predicate
                                          follow-symlinks)
    "Return list of all files under directory DIR whose names match REGEXP.
Files are returned depth first, sorted alphabetically within each directory,
as absolute names.  Directories are included when INCLUDE-DIRECTORIES is
non-nil.  PREDICATE is nil (descend everywhere), t (ignore unreadable
subdirectories) or a function called with each subdirectory name.  Symbolic
links to directories are followed only when FOLLOW-SYMLINKS is non-nil.
Port of the GNU definition; `file-name-all-completions' is not available on
standalone, so the directory listing is rebuilt from `directory-files'."
    (let* ((result nil)
           (files nil)
           (dir (directory-file-name dir))
           ;; Errors for a missing or unreadable DIR propagate, as in GNU;
           ;; the standalone `directory-files' returns nil for a missing DIR.
           (_ (unless (file-directory-p dir)
                (signal 'file-missing
                        (list "Opening directory"
                              "No such file or directory" dir))))
           (names (sort (mapcar
                         (lambda (f)
                           (if (file-directory-p (concat dir "/" f))
                               (concat f "/")
                             f))
                         (directory-files dir nil nil t))
                        #'string<)))
      (dolist (file names)
        (unless (member file '("./" "../" "." ".."))
          (if (eq (aref file (1- (length file))) ?/)
              (let* ((leaf (substring file 0 (1- (length file))))
                     (full-file (concat dir "/" leaf)))
                (when (and (or (not (file-symlink-p full-file))
                               follow-symlinks)
                           (or (eq predicate nil)
                               (eq predicate t)
                               (funcall predicate full-file)))
                  (let ((sub-files
                         (if (eq predicate t)
                             (condition-case nil
                                 (directory-files-recursively
                                  full-file regexp include-directories
                                  predicate follow-symlinks)
                               (file-error nil))
                           (directory-files-recursively
                            full-file regexp include-directories
                            predicate follow-symlinks))))
                    (setq result (nconc result sub-files))))
                (when (and include-directories
                           (string-match regexp leaf))
                  (setq result (nconc result (list full-file)))))
            (when (string-match regexp file)
              (push (concat dir "/" file) files)))))
      (nconc result (nreverse files)))))

;; Small helpers that anvil-eval and anvil-semantic call.
(unless (fboundp 'file-name-base)
  (defun file-name-base (&optional filename)
    "Return the base name of FILENAME: no directory, no extension."
    (file-name-sans-extension
     (file-name-nondirectory (or filename buffer-file-name)))))

(unless (fboundp 'time-subtract)
  (defun time-subtract (a b)
    "Return A minus B in seconds as a float, which `float-time' accepts.
Fallback for runtimes without a native `time-subtract'; nil means now."
    (- (float-time a) (float-time b))))

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
;; The defs index never uses the host DB (`anvil-defs-index-db-path' defaults
;; to the host daemon's file under `user-emacs-directory', and a rebuild
;; deletes every row).  ANVIL_DEFS_INDEX_DB overrides the isolated default.
(defun anvil-standalone--defs-db-setup ()
  "Point the defs index at the isolated standalone DB."
  ;; The worker skips port 10, but anvil-defs `require's anvil-sexp.
  (when (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
    (anvil-standalone--load-anvil-sexp))
  (setq anvil-defs-index-db-path
        (or (getenv "ANVIL_DEFS_INDEX_DB")
            (expand-file-name
             "anvil-standalone/anvil-defs-index.db"
             (or (getenv "XDG_CACHE_HOME")
                 (expand-file-name ".cache" (or (getenv "HOME") "~")))))))

(fset 'anvil-standalone-defs-index-handler
      '(lambda (query &optional limit)
         (anvil-standalone--defs-db-setup)
         (anvil-server-with-error-handling
          (format "%S" (anvil-disclosure-defs-index query limit)))))
(fset 'anvil-standalone-defs-index-update-handler
      '(lambda (paths &optional mode)
         (anvil-standalone--defs-db-setup)
         (anvil-defs--tool-index-update paths mode)))
(fset 'anvil-standalone-defs-index-rebuild-handler
      '(lambda (&optional paths)
         (anvil-standalone--defs-db-setup)
         (anvil-defs--tool-index-rebuild paths)))
(fset 'anvil-standalone-defs-clusters-handler
      '(lambda (&optional pattern min_size limit)
         (anvil-standalone--defs-db-setup)
         (anvil-defs--tool-clusters pattern min_size limit)))
(fset 'anvil-standalone-defs-architecture-handler
      '(lambda (&optional pattern hotspots clusters)
         (anvil-standalone--defs-db-setup)
         (anvil-defs--tool-architecture pattern hotspots clusters)))

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
            'anvil-standalone-defs-index-update-handler (list paths mode))))
  (fset 'anvil-standalone-defs-index-rebuild-handler
        '(lambda (&optional paths)
           (anvil-standalone--context-worker-tool
            'anvil-standalone-defs-index-rebuild-handler (list paths))))
  (fset 'anvil-standalone-defs-clusters-handler
        '(lambda (&optional pattern min_size limit)
           (anvil-standalone--context-worker-tool
            'anvil-standalone-defs-clusters-handler (list pattern min_size limit))))
  (fset 'anvil-standalone-defs-architecture-handler
        '(lambda (&optional pattern hotspots clusters)
           (anvil-standalone--context-worker-tool
            'anvil-standalone-defs-architecture-handler (list pattern hotspots clusters)))))

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

;; S12 defs group.  Fast index queries run in-process (the server DB is the
;; isolated one set in `anvil-standalone-start'); the tools that rebuild the
;; index, call git or walk the whole call graph go through the bounded child.
(dolist (spec '(("defs-search" anvil-defs--tool-search
                 "Search the elisp symbol index for defining forms by exact or fuzzy name (Layer 2).")
                ("defs-references" anvil-defs--tool-references
                 "List indexed references (calls, quotes, ...) to a symbol.")
                ("defs-signature" anvil-defs--tool-signature
                 "Return the indexed signature of a symbol.")
                ("defs-who-requires" anvil-defs--tool-who-requires
                 "List the files that require a feature.")
                ("defs-trace-path" anvil-defs--tool-trace-path
                 "Trace callers or callees of a symbol through the indexed call graph.")
                ("defs-dead-code" anvil-defs--tool-dead-code
                 "List unreferenced definitions (candidates only; dynamic use is invisible).")
                ("defs-callers-rank" anvil-defs--tool-callers-rank
                 "Rank definitions by caller count.")
                ("defs-index-status" anvil-defs--tool-index-status
                 "Return index schema version, DB path, and file, def, ref and feature counts.")))
  (anvil-server-register-tool
   (nth 1 spec) :id (car spec) :server-id "emacs-eval"
   :intent '(elisp-read) :layer 'core :read-only t :description (nth 2 spec)))
(anvil-server-register-tool
 #'anvil-standalone-defs-index-rebuild-handler :id "defs-index-rebuild"
 :server-id "emacs-eval" :intent '(elisp-read admin) :layer 'workflow
 :schema '((type . "object")
           (properties . (("paths" . ((description . "Optional comma-separated roots. Default: anvil-defs-paths or the containing git project.") (type . "string")))))
           (required . []))
 :description "Rebuild the defs index from scratch in the isolated standalone DB (never the host DB).")
;; defs-detect-changes is registered by port 16 (S13.3).
(anvil-server-register-tool
 #'anvil-standalone-defs-clusters-handler :id "defs-clusters"
 :server-id "emacs-eval" :intent '(elisp-read) :layer 'core :read-only t
 :schema '((type . "object")
           (properties . (("pattern" . ((description . "Optional SQL LIKE on file path.") (type . "string")))
                          ("min_size" . ((description . "Drop clusters smaller than this, default 2.") (type . "string")))
                          ("limit" . ((description . "Maximum clusters, default 50.") (type . "string")))))
           (required . []))
 :description "Group files that call into each other densely into communities (approximate module map).")
(anvil-server-register-tool
 #'anvil-standalone-defs-architecture-handler :id "defs-architecture"
 :server-id "emacs-eval" :intent '(elisp-read) :layer 'core :read-only t
 :schema '((type . "object")
           (properties . (("pattern" . ((description . "Optional SQL LIKE on file path to scope clusters.") (type . "string")))
                          ("hotspots" . ((description . "Top hotspots to include, default 10.") (type . "string")))
                          ("clusters" . ((description . "Top clusters to detail, default 10.") (type . "string")))))
           (required . []))
 :description "One-call architecture overview of the indexed code: totals, hotspots, clusters.")

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

;; tabulated-list is not on the runtime load-path.  Load only the vendored
;; nelisp-emacs-lib file, here rather than beside the other compat files: its
;; macros need `gv-letplace', which exists only after the modules above loaded.
;; Adding the whole vendor directory to `load-path' would shadow other
;; libraries.  No `message': on NeLisp it writes to stdout, the MCP channel.
(defvar anvil-standalone--tablist-error nil
  "Error from loading the vendored tabulated-list, or nil.")
(let ((compat-dir (or (getenv "NELISP_EMACS_LIB_DIR")
                      (and (getenv "ANVIL_NOTES_DIR")
                           (expand-file-name "dev/nelisp-emacs-lib/src"
                                             (getenv "ANVIL_NOTES_DIR"))))))
  (when (and compat-dir (not (featurep 'tabulated-list)))
    (condition-case err
        (load (expand-file-name
               "../vendor/emacs-lisp/emacs-lisp/tabulated-list.el" compat-dir)
              nil t)
      (error (setq anvil-standalone--tablist-error err)))))

;; Port 8: modules that need tabulated-list, so they are gated on the feature.  Each loads in its
;; own condition-case.  anvil-fusion-mcp and anvil-fusion-longrun-mcp load but
;; are deliberately not enabled: their only safe tools are job pollers, and a
;; standalone server has no safe way to start a job (fusion-ask, longrun-start
;; and longrun-resume call LLM providers and spawn workers).
(defvar anvil-standalone--port8-errors nil
  "Alist of (MODULE . ERROR) for port-8 modules that failed to load.")
(unless (or (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
            (not (featurep 'tabulated-list)))
  (condition-case err
      (progn
        (require 'anvil-orchestrator-routing)
        (anvil-orchestrator-routing-enable))
    ;; No `message': on NeLisp it writes to stdout, the MCP channel.
    (error (push (cons 'anvil-orchestrator-routing err)
                 anvil-standalone--port8-errors)))
  ;; anvil-manifest registers on the server id "default", which this loader
  ;; does not serve, and its enable function installs a global tools/list
  ;; filter.  Skip the enable function: alias "default" to the served
  ;; "emacs-eval" (the summary index resolves aliases) and register only the
  ;; four read-only tool specs there.
  (condition-case err
      (progn
        (require 'anvil-manifest)
        (cl-pushnew '("default" . "emacs-eval") anvil-server-id-aliases
                    :test #'equal)
        (anvil-server-register-tools "emacs-eval" anvil-manifest--tool-specs))
    (error (push (cons 'anvil-manifest err) anvil-standalone--port8-errors))))

;; Port 9: anvil-eval and anvil-semantic (need the `with-timeout' and
;; `directory-files-recursively' shims above).  Each loads in its own
;; condition-case.
;;  * anvil-eval registers on the server id "anvil", which this loader does not
;;    serve, and its enable function installs global advice (tool-call timeout,
;;    org-mode, request mutex, server-execute).  Skip the enable function and
;;    register four tools on "emacs-eval".  `emacs-eval' evaluates in the
;;    server process itself, with the same trust as the host tool.
;;    `nelisp-eval' and `nelisp-eval-reset' are registered later, by port 13.
;;  * anvil-semantic keeps its index under the default
;;    `user-emacs-directory'.  Point it at the isolated standalone cache before
;;    registering, and register the tools directly instead of calling
;;    `anvil-semantic-enable', which would open (and create) the database at
;;    load time.
(defvar anvil-standalone--port9-errors nil
  "Alist of (MODULE . ERROR) for port-9 modules that failed to load.")
(unless (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
  (condition-case err
      (progn
        (require 'anvil-eval)
        (dolist (spec '((anvil-eval--sync "emacs-eval" (eval)
                         "Evaluate an Emacs Lisp expression in the standalone server process and return the printed result.")
                        (anvil-eval--async "emacs-eval-async" (eval)
                         "Evaluate an expression asynchronously; returns a job id. The job runs when the server next waits.")
                        (anvil-eval--result "emacs-eval-result" (eval admin)
                         "Get the status and result of a job started by emacs-eval-async.")
                        (anvil-eval--jobs "emacs-eval-jobs" (eval admin)
                         "List all async eval jobs and their statuses.")))
          (anvil-server-register-tool
           (nth 0 spec) :id (nth 1 spec) :intent (nth 2 spec) :layer 'dev
           :description (nth 3 spec) :server-id "emacs-eval")))
    (error (push (cons 'anvil-eval err) anvil-standalone--port9-errors)))
  (condition-case err
      (progn
        (require 'anvil-semantic)
        (setq anvil-semantic-db-path
              (or (getenv "ANVIL_SEMANTIC_DB")
                  (expand-file-name
                   "anvil-standalone/anvil-semantic-index.db"
                   (or (getenv "XDG_CACHE_HOME")
                       (expand-file-name ".cache" (or (getenv "HOME") "~"))))))
        (anvil-semantic--register-tools)
        ;; Not served: it calls an embedding provider over HTTP.
        (anvil-server-unregister-tool "semantic-embed-index" "emacs-eval"))
    (error (push (cons 'anvil-semantic err) anvil-standalone--port9-errors))))

;; Port 10: anvil-elisp and anvil-sexp.  Both `require' `bytecomp', whose GNU
;; source cannot load on standalone: it calls `define-compilation-mode' (absent
;; from the reduced compile engine) and then fails on a pcase pattern.  Only
;; their byte-compile paths use it, and standalone cannot execute GNU byte-code
;; anyway.  So `define-compilation-mode' gets a faithful macro fallback and the
;; `bytecomp' feature is declared only while anvil-sexp loads (then withdrawn,
;; so later requires still fail honestly); `byte-compile-file' stays void.
;;  * anvil-elisp: not loaded (no tool answers correctly).  Screened on standalone:
;;    elisp-describe-function answers without docstrings (wrong), -variable and
;;    -get-function-definition hit void functions (documentation-property,
;;    find-lisp-object-file-name), -read-source-file has no system lisp dir,
;;    elisp-ert-run reports 0 tests for a passing test, ert-run-distilled needs
;;    shell-quote-argument, and elisp-byte-compile-file cannot work.
;;  * anvil-sexp: only the tools whose answers match host Emacs are registered.
;;    Left out: sexp-macroexpand (empty expansion), sexp-wrap-form,
;;    sexp-rename-symbol and sexp-replace-call (need syntax-ppss and
;;    bounds-of-thing-at-point, which standalone lacks) and sexp-verify (its
;;    byte-compile and checkdoc checks are unavailable).
(defun anvil-standalone--load-anvil-sexp ()
  "Load anvil-sexp with the shims described above.
Also called lazily by the context worker, because some anvil-defs paths
`require' anvil-sexp."
  (unless (fboundp 'define-compilation-mode)
    (defmacro define-compilation-mode (mode name doc &rest body)
      "Fallback: define MODE as a derived mode (see GNU compile.el)."
      `(define-derived-mode ,mode ,(if (fboundp 'compilation-mode)
                                       'compilation-mode
                                     'special-mode)
         ,name ,doc ,@body)))
  (unless (fboundp 'looking-at-p)
    (defun looking-at-p (regexp)
      "Like `looking-at' but without changing the match data."
      (save-match-data (looking-at regexp))))
  (let ((fake-bytecomp (not (featurep 'bytecomp))))
    (when fake-bytecomp (provide 'bytecomp))
    (unwind-protect (require 'anvil-sexp)
      (when fake-bytecomp (setq features (delq 'bytecomp features))))))
(defvar anvil-standalone--port10-errors nil
  "Alist of (MODULE . ERROR) for port-10 modules that failed to load.")
;; Gated on the nelisp-emacs-lib compat dir being locatable: without it the
;; `ert'/`help-fns' requires pull in a buffer layer that breaks
;; `insert-file-contents' (void `before-change-functions'), which the
;; shell-drain check (S3.3) exposes when ANVIL_NOTES_DIR is unset.
(unless (or (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
            (not (or (getenv "NELISP_EMACS_LIB_DIR")
                     (getenv "ANVIL_NOTES_DIR"))))
  (condition-case err
      (progn
        (anvil-standalone--load-anvil-sexp)
        (anvil-sexp-enable)
        (dolist (id '("sexp-verify" "sexp-macroexpand" "sexp-wrap-form"
                      "sexp-rename-symbol" "sexp-replace-call"))
          (anvil-server-unregister-tool id "emacs-eval")))
    (error (push (cons 'anvil-sexp err) anvil-standalone--port10-errors))))

(defvar anvil-standalone--port11-unregister
  '("org-read-outline" "org-read-headline" "org-read-by-id" "org-edit-body"
    "org-rename-headline" "org-add-todo" "org-update-todo-state"
    "org-capture-string" "org-habit-summary" "org-agenda-view")
  "Ids of anvil-org tools that answered differently from host Emacs.
Standalone has only the lightweight Org subset: `org-current-level',
`org-outline-level', `org-back-to-heading' and friends, the capture and
agenda machinery, and `user-init-file' are missing, so these tools signal.")
;; Port 11: anvil-org.  Its requires need several GNU facilities that standalone
;; lacks; each shim below is fboundp/boundp guarded and copies GNU source.
;;  * `version<' and friends: copied from GNU subr.el (the whole vendor subr.el
;;    crashes on load).
;;  * `org-release' needs the `org-version' feature; `org-org-menu' is void
;;    because standalone drops `easy-menu-define' (a placeholder keymap does).
;;  * `subr-arity' is aliased to `func-arity'; `user-mail-address' and
;;    `user-full-name' are defvar'd; `rx' loads from the vendored directory.
;;  * anvil-org uses only `url-unhex-string' from url-util, whose real file
;;    cannot load (url-vars needs `emacs-lisp-mode-syntax-table').  The two
;;    decoding functions are copied from GNU url-util.el and the `url-util'
;;    feature is declared ONLY while anvil-org loads, then withdrawn.
;;  * Tools screened by real call against host Emacs 31.1 (see S11 in
;;    tools/ai/anvil-standalone-coverage-progress.org).  Registered (same
;;    answer as host on the fixture): org-read-file, org-get-allowed-files,
;;    org-get-todo-config, org-get-tag-config.  The other ten are unregistered
;;    below.
;;  * File access: `anvil-org-allowed-files' defaults to nil with
;;    `anvil-org-allowed-files-enabled' t, so the tools see no file unless the
;;    caller configures it; the agenda/habit tools take their files from that
;;    list, never from the user's real `org-agenda-files'.
(defvar anvil-standalone--port11-errors nil
  "Alist of (MODULE . ERROR) for port-11 modules that failed to load.")
(unless (or (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
            (not (or (getenv "NELISP_EMACS_LIB_DIR")
                     (getenv "ANVIL_NOTES_DIR"))))
  (let* ((compat-dir (or (getenv "NELISP_EMACS_LIB_DIR")
                         (expand-file-name "dev/nelisp-emacs-lib/src"
                                           (getenv "ANVIL_NOTES_DIR"))))
         (vendor (expand-file-name "../vendor/emacs-lisp" compat-dir))
         (fake-url-util (not (featurep 'url-util))))
    (unless (fboundp 'version<)
  (defconst version-separator "."
    "Specify the string used to separate the version elements.
  Usually the separator is \".\", but it can be any other string.")
  (defconst version-regexp-alist
    '(("^[-._+ ]?snapshot$"                                 . -4)
      ("^[-._+]$"                                           . -4)
      ("^[-._+ ]?\\(cvs\\|git\\|bzr\\|svn\\|hg\\|darcs\\)$" . -4)
      ("^[-._+ ]?unknown$"                                  . -4)
      ("^[-._+ ]?alpha$"                                    . -3)
      ("^[-._+ ]?beta$"                                     . -2)
      ("^[-._+ ]?\\(pre\\|rc\\)$"                           . -1))
    "Specify association between non-numeric version and its priority.
  This association is used to handle version string like \"1.0pre2\",
  \"0.9alpha1\", etc.  It's used by `version-to-list' (which see) to convert the
  non-numeric part of a version string to an integer.  For example:
     String Version    Integer List Version
     \"0.9snapshot\"     (0  9 -4)
     \"1.0-git\"         (1  0 -4)
     \"1.0.cvs\"         (1  0 -4)
     \"1.0pre2\"         (1  0 -1 2)
     \"1.0PRE2\"         (1  0 -1 2)
     \"22.8beta3\"       (22 8 -2 3)
     \"22.8 Beta3\"      (22 8 -2 3)
     \"0.9alpha1\"       (0  9 -3 1)
     \"0.9AlphA1\"       (0  9 -3 1)
     \"0.9 alpha\"       (0  9 -3)
  Each element has the following form:
     (REGEXP . PRIORITY)
  Where:
  REGEXP		regexp used to match non-numeric part of a version string.
  		It should begin with the `^' anchor and end with a `$' to
  		prevent false hits.  Letter-case is ignored while matching
  		REGEXP.
  PRIORITY	a negative integer specifying non-numeric priority of REGEXP.")
  (defun version-to-list (ver)
    "Convert version string VER into a list of integers.
  The version syntax is given by the following EBNF:
     VERSION ::= NUMBER ( SEPARATOR NUMBER )*.
     NUMBER ::= (0|1|2|3|4|5|6|7|8|9)+.
     SEPARATOR ::= `version-separator' (which see)
  	       | `version-regexp-alist' (which see).
  The NUMBER part is optional if SEPARATOR is a match for an element
  in `version-regexp-alist'.
  Examples of valid version syntax:
     1.0pre2   1.0.7.5   22.8beta3   0.9alpha1   6.9.30Beta   2.4.snapshot   .5
  Examples of invalid version syntax:
     1.0prepre2   1.0..7.5   22.8X3   alpha3.2
  Examples of version conversion:
     Version String    Version as a List of Integers
     \".5\"              (0 5)
     \"0.9 alpha\"       (0  9 -3)
     \"0.9AlphA1\"       (0  9 -3 1)
     \"0.9snapshot\"     (0  9 -4)
     \"1.0-git\"         (1  0 -4)
     \"1.0.7.5\"         (1  0  7 5)
     \"1.0.cvs\"         (1  0 -4)
     \"1.0PRE2\"         (1  0 -1 2)
     \"1.0pre2\"         (1  0 -1 2)
     \"22.8 Beta3\"      (22 8 -2 3)
     \"22.8beta3\"       (22 8 -2 3)
  See documentation for `version-separator' and `version-regexp-alist'."
    (declare (side-effect-free t))
    (unless (stringp ver)
      (error "Version must be a string"))
    (if (and (>= (length ver) (length version-separator))
  	   (string-equal (substring ver 0 (length version-separator))
  			 version-separator))
        (setq ver (concat "0" ver)))
    (unless (string-match-p "^[0-9]" ver)
      (error "Invalid version syntax: `%s' (must start with a number)" ver))
    (save-match-data
      (let ((i 0)
  	  (case-fold-search t)		; ignore case in matching
  	  lst s al)
        (while (and (setq s (string-match "[0-9]+" ver i))
  		  (= s i))
  	(setq lst (cons (string-to-number (substring ver i (match-end 0)))
  			lst)
  	      i   (match-end 0))
  	(when (and (setq s (string-match "[^0-9]+" ver i))
  		   (= s i))
  	  (setq s (substring ver i (match-end 0))
  		i (match-end 0))
  	  (unless (string= s version-separator)
  	    (setq al version-regexp-alist)
  	    (while (and al (not (string-match (caar al) s)))
  	      (setq al (cdr al)))
  	    (cond (al
  		   (push (cdar al) lst))
          ((and (string-match "^[-._+ ]?\\([a-zA-Z]\\)$" s)
             (= i (length ver)))
  		   (push (- (aref (downcase (match-string 1 s)) 0) ?a -1)
  			 lst))
  		  (t (error "Invalid version syntax: `%s'" ver))))))
      (nreverse lst))))
  (defun version-list-< (l1 l2)
    "Return t if L1, a list specification of a version, is lower than L2.
  Note that a version specified by the list (1) is equal to (1 0),
  \(1 0 0), (1 0 0 0), etc.  That is, the trailing zeros are insignificant.
  Also, a version given by the list (1) is higher than (1 -1), which in
  turn is higher than (1 -2), which is higher than (1 -3)."
    (declare (pure t) (side-effect-free t))
    (while (and l1 l2 (= (car l1) (car l2)))
      (setq l1 (cdr l1)
  	  l2 (cdr l2)))
    (cond
     ((and l1 l2) (< (car l1) (car l2)))
     ((and (null l1) (null l2)) nil)
     (l1 (< (version-list-not-zero l1) 0))
     (t  (< 0 (version-list-not-zero l2)))))
  (defun version-list-= (l1 l2)
    "Return t if L1, a list specification of a version, is equal to L2.
  Note that a version specified by the list (1) is equal to (1 0),
  \(1 0 0), (1 0 0 0), etc.  That is, the trailing zeros are insignificant.
  Also, a version given by the list (1) is higher than (1 -1), which in
  turn is higher than (1 -2), which is higher than (1 -3)."
    (declare (pure t) (side-effect-free t))
    (while (and l1 l2 (= (car l1) (car l2)))
      (setq l1 (cdr l1)
  	  l2 (cdr l2)))
    (cond
     ((and l1 l2) nil)
     ((and (null l1) (null l2)))
     (l1 (zerop (version-list-not-zero l1)))
     (t  (zerop (version-list-not-zero l2)))))
  (defun version-list-<= (l1 l2)
    "Return t if L1, a list specification of a version, is lower or equal to L2.
  Note that integer list (1) is equal to (1 0), (1 0 0), (1 0 0 0),
  etc.  That is, the trailing zeroes are insignificant.  Also, integer
  list (1) is greater than (1 -1) which is greater than (1 -2)
  which is greater than (1 -3)."
    (declare (pure t) (side-effect-free t))
    (while (and l1 l2 (= (car l1) (car l2)))
      (setq l1 (cdr l1)
  	  l2 (cdr l2)))
    (cond
     ((and l1 l2) (< (car l1) (car l2)))
     ((and (null l1) (null l2)))
     (l1 (<= (version-list-not-zero l1) 0))
     (t  (<= 0 (version-list-not-zero l2)))))
  (defun version-list-not-zero (lst)
    "Return the first non-zero element of LST, which is a list of integers.
  If all LST elements are zeros or LST is nil, return zero."
    (declare (pure t) (side-effect-free t))
    (while (and lst (zerop (car lst)))
      (setq lst (cdr lst)))
    (if lst
        (car lst)
      0))
  (defun version< (v1 v2)
    "Return t if version V1 is lower (older) than V2.
  Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
  etc.  That is, the trailing \".0\"s are insignificant.  Also, version
  string \"1\" is higher (newer) than \"1pre\", which is higher than \"1beta\",
  which is higher than \"1alpha\", which is higher than \"1snapshot\".
  Also, \"-GIT\", \"-CVS\" and \"-NNN\" are treated as snapshot versions."
    (declare (side-effect-free t))
    (version-list-< (version-to-list v1) (version-to-list v2)))
  (defun version<= (v1 v2)
    "Return t if version V1 is lower (older) than or equal to V2.
  Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
  etc.  That is, the trailing \".0\"s are insignificant.  Also, version
  string \"1\" is higher (newer) than \"1pre\", which is higher than \"1beta\",
  which is higher than \"1alpha\", which is higher than \"1snapshot\".
  Also, \"-GIT\", \"-CVS\" and \"-NNN\" are treated as snapshot versions."
    (declare (side-effect-free t))
    (version-list-<= (version-to-list v1) (version-to-list v2)))
  (defun version= (v1 v2)
    "Return t if version V1 is equal to V2.
  Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
  etc.  That is, the trailing \".0\"s are insignificant.  Also, version
  string \"1\" is higher (newer) than \"1pre\", which is higher than \"1beta\",
  which is higher than \"1alpha\", which is higher than \"1snapshot\".
  Also, \"-GIT\", \"-CVS\" and \"-NNN\" are treated as snapshot versions."
    (declare (side-effect-free t))
    (version-list-= (version-to-list v1) (version-to-list v2)))
    )
    (unless (fboundp 'subr-arity)
      (defun subr-arity (subr) (func-arity subr)))
    (unless (boundp 'user-mail-address) (defvar user-mail-address ""))
    (unless (boundp 'user-full-name) (defvar user-full-name ""))
    (unless (boundp 'org-org-menu) (defvar org-org-menu (make-sparse-keymap)))
    ;; GNU org.el: used by `org-get-todo-config'.
    (unless (fboundp 'org-remove-keyword-keys)
      (defun org-remove-keyword-keys (list)
        "Remove a pair of parenthesis at the end of each string in LIST."
        (mapcar (lambda (x)
                  (if (string-match "(.*)$" x)
                      (substring x 0 (match-beginning 0))
                    x))
                list)))
    ;; GNU defaults of the tag defcustoms read by `org-get-tag-config'.
    (unless (boundp 'org-use-tag-inheritance) (defvar org-use-tag-inheritance t))
    (unless (boundp 'org-tags-exclude-from-inheritance)
      (defvar org-tags-exclude-from-inheritance nil))
    (unless (boundp 'org-tag-alist) (defvar org-tag-alist nil))
    (unless (boundp 'org-tag-persistent-alist)
      (defvar org-tag-persistent-alist nil))
    (unless (fboundp 'byte-to-string)
      (defun byte-to-string (byte) (unibyte-string byte)))
    (unless (fboundp 'url-unhex)
      (defun url-unhex (x)
        (if (> x ?9)
            (if (>= x ?a) (+ 10 (- x ?a)) (+ 10 (- x ?A)))
          (- x ?0))))
    (unless (fboundp 'url-unhex-string)
      (defun url-unhex-string (str &optional allow-newlines)
        "Decode %XX sequences in a percent-encoded URL (GNU url-util.el)."
        (setq str (or str ""))
        (let ((tmp "") (case-fold-search t))
          (while (string-match "%[0-9a-f][0-9a-f]" str)
            (let* ((start (match-beginning 0))
                   (ch1 (url-unhex (elt str (+ start 1))))
                   (code (+ (* 16 ch1) (url-unhex (elt str (+ start 2))))))
              (setq tmp (concat tmp (substring str 0 start)
                                (cond (allow-newlines (byte-to-string code))
                                      ((or (= code ?\n) (= code ?\r)) " ")
                                      (t (byte-to-string code))))
                    str (substring str (match-end 0)))))
          (concat tmp str))))
    (add-to-list 'load-path (expand-file-name "org" vendor) t)
    (add-to-list 'load-path (expand-file-name "emacs-lisp" vendor) t)
    (when fake-url-util (provide 'url-util))
    (unwind-protect
        (condition-case err
            (progn
              (require 'org-version)
              (require 'rx)
              (require 'anvil-org)
              (anvil-org-enable)
              (dolist (id anvil-standalone--port11-unregister)
                (anvil-server-unregister-tool id "emacs-eval")))
          (error (push (cons 'anvil-org err) anvil-standalone--port11-errors)))
      (when fake-url-util (setq features (delq 'url-util features))))))


;; Port 12: anvil-orchestrator tools (S12.4).  anvil-orchestrator is already loaded
;; by port 8 (routing requires it) but only the routing tools are registered.
;; The enable function is NOT called: it rehydrates tasks from the state DB and
;; starts pump/watchdog timers.  `anvil-orchestrator--register-tools' registers
;; the tool specs only.  Orchestrator state never touches the host: the state DB
;; is `anvil-state-db-path' (isolated in `anvil-standalone-start') and job
;; stdout/stderr files live under `anvil-orchestrator-work-dir', which
;; `anvil-standalone--orchestrator-setup' moves to the isolated cache
;; (ANVIL_ORCHESTRATOR_WORK_DIR overrides).
;;  * Tools are screened by real call against host Emacs 31.1 with a fake
;;    provider whose command is `sh -c printf' (see S12.4 in
;;    tools/ai/anvil-standalone-coverage-progress.org).  Tools whose answer
;;    differs from the host are listed in `anvil-standalone--port12-unregister'
;;    with the first blocker beside each.
(defvar anvil-standalone--port12-errors nil
  "Alist of (MODULE . ERROR) for port-12 modules that failed to load.")
(defvar anvil-standalone--port12-unregister
  '("orchestrator-cancel" "orchestrator-stop")
  "Orchestrator tool ids that are registered by the module but not served.
Both kill a running job with a signal.  On standalone the killed task then
reports exit-code 0 where the host reports 15 (`process-exit-status' of a
signalled process), so orchestrator-collect/status after a cancel differ.")
(defun anvil-standalone--orchestrator-setup ()
  "Point the orchestrator job directory at the isolated standalone cache."
  (setq anvil-orchestrator-work-dir
        (or (getenv "ANVIL_ORCHESTRATOR_WORK_DIR")
            (expand-file-name
             "anvil-standalone/anvil-orchestrator"
             (or (getenv "XDG_CACHE_HOME")
                 (expand-file-name ".cache" (or (getenv "HOME") "~")))))))
;; Shims (fboundp/boundp guarded): `timer-list' is read by the pump-timer
;; check and `process-list' by the collect wait loop.  This `process-list'
;; always returns nil (standalone keeps no process registry yet), so
;; `get-buffer-process' style lookups find nothing; replace it with a real
;; registry before porting tools that depend on process lookup.
(unless (boundp 'timer-list) (defvar timer-list nil))
(unless (fboundp 'process-list) (defun process-list () nil))
(unless (or (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
            (not (featurep 'tabulated-list)))
  (condition-case err
      (progn
        (require 'anvil-orchestrator)
        (anvil-standalone--orchestrator-setup)
        (anvil-orchestrator--register-tools)
        (dolist (id anvil-standalone--port12-unregister)
          (anvil-server-unregister-tool id "emacs-eval")))
    (error (push (cons 'anvil-orchestrator err)
                 anvil-standalone--port12-errors))))

;; Port 13: remaining wave-1 groups (S12.5).  Every tool is screened by real call
;; against host `emacs --batch -Q' on a temp fixture (see S12.5 in
;; tools/ai/anvil-standalone-coverage-progress.org).  Databases live under the
;; isolated `anvil-standalone' cache; no LLM, CLI agent or network is touched.
;;  * anvil-sqlite: `sqlite-select' with RETURN-TYPE `set' (a statement object
;;    walked by `sqlite-more-p'/`sqlite-next'/`sqlite-finalize') is absent, so a
;;    fboundp-guarded shim returns a cursor over the selected rows.
;;  * anvil-disclosure: only the tools that read caller-given files or the
;;    isolated org index are registered here.
;;  * anvil-bench: `(require 'profiler)' fails (no profiler.el), so the feature
;;    is declared only while the module loads.  bench-profile-expr stays out.
;;  * anvil-org-index: DB path moved to the isolated cache; `message' is muted
;;    around the handlers because on NeLisp it writes to the MCP channel.
(defvar anvil-standalone--port13-errors nil
  "Alist of (MODULE . ERROR) for port-13 modules that failed to load.")
(defvar anvil-standalone--port13-unregister
  '("bench-profile-expr" "org-index-rebuild")
  "Port-13 tool ids registered by their module but not served.
bench-profile-expr: standalone has no sampling profiler (`profiler-start' is
void), so the :top list is empty where the host reports frames.
org-index-rebuild: an offload tool; the offload worker reports
`void-function anvil-org-index--tool-rebuild' because standalone has no worker
Emacs that can load the module.")
;; Not registered, with the first blocker (checked 2026-11-03):
;;  * ert-run-distilled: anvil-elisp fails to load (eager macro-expansion failure,
;;    pcase pattern with branch-local bindings) and shell-quote-argument is void.
;;  * semantic-embed-index: calls an embedding provider over HTTP.
;;  * fusion-ask fusion-result longrun-start longrun-resume longrun-status: the
;;    panels resolve to real LLM providers; the loader has no provider stub.
;;  * journal-memo-append: defined in ~/.emacs.d/custom-lisp, not in this repo.
;;  * ldb-*: defined in dev/lisp-dialect-bridge, not in this repo.
(unless (boundp 'gc-cons-threshold) (defvar gc-cons-threshold 800000))
(unless (fboundp 'sqlite-more-p)
  (when (fboundp 'sqlite-select)
    (defalias 'anvil-standalone--sqlite-select (symbol-function 'sqlite-select))
    (defun sqlite-select (db query &optional values return-type)
      "Like GNU `sqlite-select'; RETURN-TYPE `set' returns a row cursor."
      (if (eq return-type 'set)
          (list 'sqlite-set
                (anvil-standalone--sqlite-select db query values))
        (anvil-standalone--sqlite-select db query values return-type)))
    (defun sqlite-more-p (set) (and (consp set) (cadr set) t))
    (defun sqlite-next (set) (and (consp set) (pop (cadr set))))
    (defun sqlite-finalize (set) (and (consp set) (setcar (cdr set) nil)) t)))
(defun anvil-standalone--mute-message (fn)
  "Redefine FN with its own arglist and docstring, running with `message' muted.
The loader never lets handlers emit `message' output, and the server derives
the tool parameters from the handler arglist, so a `&rest' wrapper will not do."
  (let* ((orig (symbol-function fn))
         (arglist (help-function-arglist orig t))
         (vars (seq-remove (lambda (a) (memq a '(&optional &rest))) arglist)))
    (eval `(defun ,fn ,arglist ,(documentation orig t)
             (let ((old (symbol-function 'message)))
               (fset 'message #'ignore)
               (unwind-protect (funcall ',orig ,@vars)
                 (fset 'message old))))
          t)))
(defun anvil-standalone--port13-setup ()
  "Open the isolated org-index DB (called from `anvil-standalone-start')."
  (when (and (featurep 'anvil-org-index) (not anvil-org-index--db))
    (setq anvil-org-index-db-path
          (or (getenv "ANVIL_ORG_INDEX_DB")
              (expand-file-name
               "anvil-standalone/anvil-org-index.db"
               (or (getenv "XDG_CACHE_HOME")
                   (expand-file-name ".cache" (or (getenv "HOME") "~"))))))
    (make-directory (file-name-directory anvil-org-index-db-path) t)
    (setq anvil-org-index--backend (anvil-org-index--detect-backend))
    (setq anvil-org-index--db (anvil-org-index--open anvil-org-index-db-path))
    (anvil-org-index--apply-ddl anvil-org-index--db)))
(unless (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
  (condition-case err
      (progn
        (require 'anvil-sqlite)
        (anvil-sqlite-enable))
    (error (push (cons 'anvil-sqlite err) anvil-standalone--port13-errors)))
  (condition-case err
      (anvil-server-register-tools
       "emacs-eval"
       (seq-filter (lambda (spec)
                     (member (plist-get (cdr spec) :id)
                             '("file-read-snippet" "journal-index"
                               "disclosure-fetch" "disclosure-help"
                               "org-index-index")))
                   anvil-disclosure--tool-specs))
    (error (push (cons 'anvil-disclosure err) anvil-standalone--port13-errors)))
  (condition-case err
      (let ((fake-profiler (not (featurep 'profiler))))
        (when fake-profiler (provide 'profiler))
        (unwind-protect (require 'anvil-bench)
          (when fake-profiler (setq features (delq 'profiler features))))
        (anvil-bench--register-tools)
        (dolist (id anvil-standalone--port13-unregister)
          (anvil-server-unregister-tool id "emacs-eval")))
    (error (push (cons 'anvil-bench err) anvil-standalone--port13-errors)))
  (condition-case err
      (progn
        (require 'anvil-eval)
        (dolist (spec '((anvil-eval--nelisp "nelisp-eval"
                         "Evaluate expression with the pure Elisp NeLisp evaluator and return the printed result.")
                        (anvil-eval--nelisp-reset "nelisp-eval-reset"
                         "Reset the stateful pure Elisp NeLisp REPL used by nelisp-eval.")))
          (anvil-server-register-tool
           (nth 0 spec) :id (nth 1 spec) :intent '(eval) :layer 'dev
           :description (nth 2 spec)
           :server-id "emacs-eval")))
    (error (push (cons 'anvil-eval err) anvil-standalone--port13-errors)))
  (condition-case err
      (progn
        (require 'anvil-org-index)
        (anvil-standalone--mute-message 'anvil-org-index--tool-search)
        (anvil-standalone--mute-message 'anvil-org-index--tool-rebuild)
        (anvil-org-index--register-tools)
        (dolist (id anvil-standalone--port13-unregister)
          (anvil-server-unregister-tool id "emacs-eval")))
    (error (push (cons 'anvil-org-index err) anvil-standalone--port13-errors))))

;; Port 14: fusion, longrun, journal-memo, ldb and org-index-rebuild (S12.6).
;; Every tool is screened by real call against host `emacs --batch -Q' on a temp
;; fixture (see S12.6 in tools/ai/anvil-standalone-coverage-progress.org).
;;  * fusion-ask/fusion-result/longrun-*: registered as-is.  In production they
;;    use the user's real provider config exactly like host; only the screening
;;    fixture swaps in fake providers whose command is `sh -c printf'.  The
;;    quest store DB moves to the isolated cache (ANVIL_FUSION_LONGRUN_DB).
;;  * journal-memo-append (~/.emacs.d/custom-lisp/my-journal-memo.el) and
;;    ldb-* (dev/lisp-dialect-bridge) live outside this repo.  Their
;;    directories come from ANVIL_CUSTOM_LISP_DIR / ANVIL_LDB_DIR, defaulting
;;    to HOME/.emacs.d/custom-lisp and ANVIL_NOTES_DIR/dev/lisp-dialect-bridge.
;;    An absent directory or library is skipped silently.
;;  * org-index-rebuild is an offload tool: host spawns `emacs --batch' with
;;    the handler's module required.  Standalone has no symbol-file, so that
;;    worker reports `void-function'.  The tool is re-registered on a handler
;;    that runs in a bounded standalone child (the context-worker path), which
;;    loads anvil-org-index and the isolated DB itself.
(defvar anvil-standalone--port14-errors nil
  "Alist of (MODULE . ERROR) for port-14 modules that failed to load.")
(defvar anvil-standalone--port14-unregister nil
  "Port-14 tool ids that stay unregistered, with the first blocker.
bench-profile-expr: standalone has no sampling profiler (`profiler-start' is
void), so the :top list would be empty where the host reports sampled frames.
Registering it would hand back a faked answer, so it stays out until the
runtime grows a profiler.")
(defun anvil-standalone--port14-cache-dir ()
  "Return the isolated standalone cache directory."
  (expand-file-name
   "anvil-standalone"
   (or (getenv "XDG_CACHE_HOME")
       (expand-file-name ".cache" (or (getenv "HOME") "~")))))
(defun anvil-standalone--port14-optional-dir (env default)
  "Return the existing directory named by ENV or DEFAULT, else nil."
  (let ((dir (or (getenv env) default)))
    (and dir (file-directory-p dir) dir)))
;; Shims (fboundp guarded) for the file-visiting calls my-journal-memo makes.
;; `date-to-time' only handles the "YYYY-MM-DD HH:MM:SS" form the module passes.
(unless (fboundp 'date-to-time)
  (defun date-to-time (date)
    (if (string-match "\\`\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" date)
        (encode-time (list (string-to-number (match-string 6 date))
                           (string-to-number (match-string 5 date))
                           (string-to-number (match-string 4 date))
                           (string-to-number (match-string 3 date))
                           (string-to-number (match-string 2 date))
                           (string-to-number (match-string 1 date))
                           nil -1 nil))
      (error "date-to-time: unsupported date %S" date))))
(unless (fboundp 'find-file-noselect)
  (defun find-file-noselect (file &rest _)
    (let ((buf (generate-new-buffer (file-name-nondirectory file))))
      (with-current-buffer buf
        (when (file-exists-p file) (insert-file-contents file))
        (setq buffer-file-name (expand-file-name file)))
      buf)))
(unless (fboundp 'save-buffer)
  (defun save-buffer (&rest _)
    (write-region (point-min) (point-max) buffer-file-name nil 'silent)))
(defun anvil-standalone--org-index-rebuild-here (paths)
  "Rebuild the isolated org-index for PATHS inside a context worker."
  (require 'anvil-org-index)
  (let ((old (symbol-function 'message)))
    (fset 'message #'ignore)
    (unwind-protect
        (progn
          (setq anvil-org-index-db-path
                (or (getenv "ANVIL_ORG_INDEX_DB")
                    (expand-file-name "anvil-org-index.db"
                                      (anvil-standalone--port14-cache-dir))))
          (make-directory (file-name-directory anvil-org-index-db-path) t)
          (anvil-org-index--tool-rebuild paths))
      (fset 'message old))))
(defun anvil-standalone-org-index-rebuild-handler (&optional paths)
  "Rebuild the entire org-index from disk in a bounded standalone child.

MCP Parameters:
  paths - Optional comma-separated list of directory paths to
          rebuild.  Empty or omitted means every directory in
          the configured `anvil-org-index-paths'."
  (if (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
      (anvil-standalone--org-index-rebuild-here paths)
    (anvil-standalone--context-worker-tool
     'anvil-standalone-org-index-rebuild-handler (list paths))))
(unless (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
  (condition-case err
      (progn
        (require 'anvil-fusion-mcp)
        (require 'anvil-fusion-longrun-mcp)
        (require 'anvil-fusion-longrun-store)
        (setq anvil-fusion-longrun-store-db-path
              (or (getenv "ANVIL_FUSION_LONGRUN_DB")
                  (expand-file-name "anvil-fusion-longrun.db"
                                    (anvil-standalone--port14-cache-dir))))
        (make-directory (file-name-directory anvil-fusion-longrun-store-db-path) t)
        (dolist (fn '(anvil-fusion--tool-ask anvil-fusion--tool-result
                      anvil-fusion-longrun--tool-start
                      anvil-fusion-longrun--tool-resume
                      anvil-fusion-longrun--tool-status))
          (anvil-standalone--mute-message fn))
        (anvil-fusion--register-tools)
        (anvil-fusion-longrun--register-tools))
    (error (push (cons 'anvil-fusion err) anvil-standalone--port14-errors)))
  (condition-case err
      (let ((dir (anvil-standalone--port14-optional-dir
                  "ANVIL_CUSTOM_LISP_DIR"
                  (expand-file-name ".emacs.d/custom-lisp"
                                    (or (getenv "HOME") "~")))))
        (when (and dir (file-exists-p (expand-file-name "my-journal-memo.el" dir)))
          (add-to-list 'load-path dir)
          (require 'my-journal-memo)
          (anvil-standalone--mute-message 'my-journal-memo--tool-append)
          (my-journal-memo-enable)))
    (error (push (cons 'my-journal-memo err) anvil-standalone--port14-errors)))
  (condition-case err
      (let ((dir (anvil-standalone--port14-optional-dir
                  "ANVIL_LDB_DIR"
                  (expand-file-name
                   "dev/lisp-dialect-bridge"
                   (or (getenv "ANVIL_NOTES_DIR")
                       (expand-file-name "Cowork/Notes"
                                         (or (getenv "HOME") "~")))))))
        (when (and dir (file-exists-p (expand-file-name "ldb-mcp.el" dir)))
          (add-to-list 'load-path dir)
          (require 'lisp-dialect-bridge)
          (load (expand-file-name "ldb-mcp.el" dir) nil t)
          (dolist (fn '(ldb-mcp--tool-translate ldb-mcp--tool-translate-to-file
                        ldb-mcp--tool-capabilities))
            (anvil-standalone--mute-message fn))
          (ldb-mcp-enable)))
    (error (push (cons 'ldb-mcp err) anvil-standalone--port14-errors)))
  (condition-case err
      (when (featurep 'anvil-org-index)
        (anvil-server-unregister-tool "org-index-rebuild" "emacs-eval")
        (anvil-server-register-tool
         #'anvil-standalone-org-index-rebuild-handler
         :id "org-index-rebuild" :intent '(org-read admin) :layer 'workflow
         :server-id "emacs-eval"
         :description
         "Rebuild the entire org-index from disk.  Runs in a bounded standalone
child process so the main server stays responsive; commits in chunks and
persists them in the DB, so a follow-up call can re-run safely."))
    (error (push (cons 'org-index-rebuild err) anvil-standalone--port14-errors))))

;; Port 15: anvil-dashboard (S12.7).  Tools are screened by real call against
;; host `emacs --batch -Q' on a fixture HOME / notes root (see S12.7 in
;; tools/ai/anvil-standalone-coverage-progress.org).
;;  * The module is optional (`require' with NOERROR plus a `featurep' guard),
;;    so a checkout without anvil-dashboard.el still starts.
;;  * The HTTP filter answers with `process-send-string', whose
;;    `nelisp-process-buffer-p' is an unresolved autoload on this runtime
;;    (invalid-function); requiring the feature up front resolves it.
;;  * `process-contact :service' does not report an OS-assigned port, so an
;;    ephemeral port (0 or t) would not give the host answer: callers pass an
;;    explicit port.
(defvar anvil-standalone--port15-errors nil
  "Alist of (MODULE . ERROR) for port-15 modules that failed to load.")
(defvar anvil-standalone--port15-unregister nil
  "Port-15 tool ids that stay unregistered, with the first blocker.
None: dashboard-snapshot, dashboard-serve-start and dashboard-serve-stop give
the host answers on the fixture.")
(unless (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
  (condition-case err
      (progn
        (require 'nelisp-process-buffer nil t)
        (when (require 'anvil-dashboard nil t)
          (anvil-dashboard--register-tools)
          (dolist (id anvil-standalone--port15-unregister)
            (anvil-server-unregister-tool id "emacs-eval"))))
    (error (push (cons 'anvil-dashboard err) anvil-standalone--port15-errors))))

;; Port 16: process coding group (S13.3): defs-detect-changes and browser-*.
;; Tools are screened by real call against host `emacs --batch -Q' on a temp
;; fixture (see S13.3 in tools/ai/anvil-standalone-coverage-progress.org).
;;  * `process-coding-system-alist' comes from nelisp-emacs-lib
;;    (emacs-cc-coding-2.el, the real provider); anvil-host and anvil-browser
;;    let-bind it around `make-process'.
;;  * `shell-quote-argument' is the GNU subr.el POSIX branch, copied under an
;;    fboundp guard.
;;  * defs-detect-changes runs `git diff' through `anvil-shell' with :cwd, which
;;    host serves by binding `default-directory'; NeLisp `make-process' ignores
;;    it (wave-3 gap).  While this handler runs, `anvil-host--run' prefixes the
;;    shell command with `cd DIR &&', so the child sees the same cwd and the
;;    result plist (:command is the unprefixed text) is unchanged.
;;  * browser-*: anvil-browser drives the agent-browser CLI over a pipe
;;    (`make-process' :stderr, JSON on stdin); no cwd is involved.
(defvar anvil-standalone--port16-errors nil
  "Alist of (MODULE . ERROR) for port-16 modules that failed to load.")
(defvar anvil-standalone--port16-unregister nil
  "Port-16 tool ids that stay unregistered, with the first blocker.
None: defs-detect-changes and the five browser-* tools give the host answers
on the fixture.")
(let ((compat-dir (or (getenv "NELISP_EMACS_LIB_DIR")
                      (and (getenv "ANVIL_NOTES_DIR")
                           (expand-file-name "dev/nelisp-emacs-lib/src"
                                             (getenv "ANVIL_NOTES_DIR"))))))
  (when (and compat-dir (not (boundp 'process-coding-system-alist)))
    (load (expand-file-name "emacs-cc-coding-2.el" compat-dir) t t)))
(unless (fboundp 'shell-quote-argument)
  (defun shell-quote-argument (argument &optional posix)
    "Quote ARGUMENT for passing as argument to an inferior shell (POSIX branch)."
    (ignore posix)
    (if (equal argument "")
        "''"
      ;; Quote everything except POSIX filename characters.
      ;; This should be safe enough even for really weird shells.
      (replace-regexp-in-string
       "\n" "'\n'"
       (replace-regexp-in-string "[^-0-9a-zA-Z_./\n]" "\\\\\\&" argument)))))
(defvar anvil-standalone--shell-cd nil
  "Non-nil while `anvil-host--run' must emulate `default-directory' with cd.")
(when (and (fboundp 'anvil-host--run)
           (not (fboundp 'anvil-standalone--host-run-orig)))
  (defalias 'anvil-standalone--host-run-orig (symbol-function 'anvil-host--run))
  (defun anvil-host--run (command coding cwd timeout)
    "Like `anvil-host--run'; under `anvil-standalone--shell-cd' cd into CWD."
    (anvil-standalone--host-run-orig
     (if (and anvil-standalone--shell-cd cwd)
         (concat "cd " (shell-quote-argument (expand-file-name cwd)) " && " command)
       command)
     coding cwd timeout)))
(fset 'anvil-standalone-defs-detect-changes-handler
      '(lambda (&optional repo rev depth limit)
         "Map a git diff to the indexed symbols it touches + their callers.

MCP Parameters:
  repo   - Repo directory (string).  Default: the git project root.
  rev    - Revision diffed against the working tree (default \"HEAD\" =
           uncommitted changes).  E.g. \"HEAD~3\", a branch, a SHA.
  depth  - Blast-radius caller depth 1..5 (string or integer); default 1.
  limit  - Max changed symbols (string or integer); default 100."
         (anvil-standalone--defs-db-setup)
         (let ((anvil-standalone--shell-cd t))
           (anvil-defs--tool-detect-changes repo rev depth limit))))
(anvil-server-register-tool
 #'anvil-standalone-defs-detect-changes-handler :id "defs-detect-changes"
 :server-id "emacs-eval" :intent '(elisp-read) :layer 'core :read-only t
 :description "Map a git diff to the indexed definitions it touches and their transitive callers (blast-radius).  rev (default HEAD = uncommitted changes) is diffed against the working tree; each changed symbol comes back with direct-caller count, test-coverage flag and a coarse high/medium/low risk.")
(unless (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
  (condition-case err
      (when (require 'anvil-browser nil t)
        (anvil-browser--register-tools)
        (dolist (id anvil-standalone--port16-unregister)
          (anvil-server-unregister-tool id "emacs-eval")))
    (error (push (cons 'anvil-browser err) anvil-standalone--port16-errors))))

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
  (anvil-standalone--defs-db-setup)
  (when (fboundp 'anvil-standalone--orchestrator-setup)
    (anvil-standalone--orchestrator-setup))
  (when (fboundp 'anvil-standalone--port13-setup)
    (condition-case err (anvil-standalone--port13-setup)
      (error (push (cons 'org-index-db err) anvil-standalone--port13-errors))))
  (setq anvil-server--debug-trace (and (getenv "ANVIL_STANDALONE_DEBUG") t))
  (anvil-standalone--call-with-stdio-reader
   (lambda () (anvil-server-run-batch-stdio "emacs-eval")))
  (nelisp--exit-process 0))

(unless (or (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
            (getenv "ANVIL_STANDALONE_TEST_NO_SERVER")
            (getenv "ANVIL_STANDALONE_BUILD_IMAGE"))
  (anvil-standalone-start))

;;; anvil-standalone-mcp.el ends here
