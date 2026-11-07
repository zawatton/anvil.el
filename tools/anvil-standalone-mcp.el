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

;; emacs-lib resolver.  Every site that needs the Emacs compatibility library
;; goes through these helpers.  NELISP_ROOT_DIR (a nelisp checkout root) selects
;; the packaged layout: packages/emacs-lib-*/src plus vendor/emacs-lisp.  When it
;; is unset the legacy flat directory applies unchanged: NELISP_EMACS_LIB_DIR,
;; else $ANVIL_NOTES_DIR/dev/nelisp-emacs-lib/src.
(defun anvil-standalone--elib-root ()
  "Return the nelisp checkout root from NELISP_ROOT_DIR, or nil when unset."
  (let ((root (getenv "NELISP_ROOT_DIR")))
    (and root (> (length root) 0) root)))

(defun anvil-standalone--elib-legacy-dir ()
  "Return the legacy flat emacs-lib directory, or nil when not configured."
  (or (getenv "NELISP_EMACS_LIB_DIR")
      (and (getenv "ANVIL_NOTES_DIR")
           (expand-file-name "dev/nelisp-emacs-lib/src"
                             (getenv "ANVIL_NOTES_DIR")))))

(defun anvil-standalone--elib-available-p ()
  "Return non-nil when an emacs-lib source tree is configured."
  (and (or (anvil-standalone--elib-root)
           (anvil-standalone--elib-legacy-dir))
       t))

(defun anvil-standalone--elib-src-dirs ()
  "Return the emacs-lib source directories in search order."
  (let ((root (anvil-standalone--elib-root)))
    (if root
        (let (dirs)
          (dolist (pkg '("emacs-lib-core" "emacs-lib-facade"
                         "emacs-lib-text-core" "emacs-lib-bridges"))
            (let ((dir (expand-file-name (concat "packages/" pkg "/src") root)))
              (when (file-directory-p dir) (push dir dirs))))
          (nreverse dirs))
      (let ((dir (anvil-standalone--elib-legacy-dir)))
        (and dir (list dir))))))

(defun anvil-standalone--elib-file (name)
  "Return the absolute path of emacs-lib file NAME, or nil.
Legacy mode returns the path inside the flat directory without checking that
it exists, as the former per-site code did."
  (if (anvil-standalone--elib-root)
      (let (found)
        (dolist (dir (anvil-standalone--elib-src-dirs))
          (let ((path (expand-file-name name dir)))
            (when (and (not found) (file-exists-p path))
              (setq found path))))
        found)
    (let ((dir (anvil-standalone--elib-legacy-dir)))
      (and dir (expand-file-name name dir)))))

(defun anvil-standalone--elib-load (name &optional noerror)
  "Load emacs-lib file NAME.  Return nil when NOERROR and it cannot be found.
NOERROR also silences load errors, like `(load FILE t t)'."
  (let ((path (anvil-standalone--elib-file name)))
    (cond (path (if noerror (load path t t) (load path)))
          (noerror nil)
          (t (error "emacs-lib file not found: %s" name)))))

(defun anvil-standalone--elib-vendor-dir ()
  "Return the vendored GNU Lisp directory (vendor/emacs-lisp), or nil."
  (let ((root (anvil-standalone--elib-root)))
    (if root
        (expand-file-name "vendor/emacs-lisp" root)
      (let ((dir (anvil-standalone--elib-legacy-dir)))
        (and dir (expand-file-name "../vendor/emacs-lisp" dir))))))

(defun anvil-standalone--elib-add-load-path ()
  "Put the emacs-lib source directories on `load-path'.
In NELISP_ROOT_DIR mode the vendored vendor/emacs-lisp, its org and
emacs-lisp subdirectories follow at the end, so Org and the GNU libraries
resolve while the runtime's own libraries keep priority."
  (dolist (dir (reverse (anvil-standalone--elib-src-dirs)))
    (add-to-list 'load-path dir))
  (when (anvil-standalone--elib-root)
    (let ((vendor (anvil-standalone--elib-vendor-dir)))
      (dolist (sub '("" "org" "emacs-lisp"))
        (let ((dir (directory-file-name (expand-file-name sub vendor))))
          (when (file-directory-p dir) (add-to-list 'load-path dir t)))))))

;; Load the current provider before Anvil can select a stale .elc.  NeLisp's
;; library lookup prefers bytecode independently of load-suffixes.
(when (anvil-standalone--elib-available-p)
  (anvil-standalone--elib-load "emacs-char-table.el"))

(require 'anvil)
(require 'anvil-server-commands)
(require 'anvil-context)
;; The batch, dependency and project context modules are optional: a checkout
;; without them still serves every other tool.  Their tools register below
;; only when the feature loaded.
(require 'anvil-context-batch nil t)
(require 'anvil-context-dependencies nil t)

(when (anvil-standalone--elib-available-p)
  (anvil-standalone--elib-add-load-path)
  (anvil-standalone--elib-load "emacs-sqlite-ffi.el")
  (anvil-standalone--elib-load "emacs-textmodes-stub.el"))

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

;; `version<' and friends, copied from GNU subr.el (the whole vendor subr.el
;; crashes on load).  Port 6 (anvil-data) and Port 11 (anvil-org) need them,
;; and the emacs-lib tree does not preload them.
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
(version-list-= (version-to-list v1) (version-to-list v2))))

;; GNU Org facilities needed by every module that requires Org (anvil-file,
;; anvil-data, anvil-org), so they are set up before the first of them loads.
;; `org-release' lives in org-version.el, which standalone does not autoload;
;; `org-org-menu' is void because standalone drops `easy-menu-define'.
(when (anvil-standalone--elib-available-p)
  (let ((vendor (anvil-standalone--elib-vendor-dir)))
    (when (and vendor (not (featurep 'org-version)))
      (add-to-list 'load-path (expand-file-name "org" vendor) t)
      (condition-case nil (require 'org-version) (error nil)))
    ;; GNU Emacs autoloads Org entry points (for example
    ;; `org-element-cache-reset', called by `org-mode') from org-loaddefs.el.
    (when (and vendor (not (fboundp 'org-element-cache-reset)))
      (condition-case nil
          (load (expand-file-name "org/org-loaddefs.el" vendor) nil t)
        (error nil))))
  (unless (boundp 'org-org-menu) (defvar org-org-menu (make-sparse-keymap))))

;; `display-color-cells' is GNU frame.el, which standalone does not load.  For a
;; batch (non-graphical) frame it reaches `tty-display-color-cells', whose C
;; code (term.c) returns 0 on the initial terminal; host Emacs 31.1 --batch
;; also answers 0.  Org's face defcustoms call it while loading.
(unless (fboundp 'display-color-cells)
  (defun display-color-cells (&optional _display)
    "Return the number of colors DISPLAY supports; 0 in batch (GNU term.c)."
    0))

;; The runtime provides `seq' without `seq-do-indexed', which GNU
;; org-element-ast.el calls while loading.  The emacs-lib seq.el defines it
;; behind an fboundp guard, so loading that file only fills the gap.
(when (and (anvil-standalone--elib-available-p)
           (not (fboundp 'seq-do-indexed)))
  (anvil-standalone--elib-load "seq.el" t))

;; Variables GNU defines outside the Org tree.  `user-mail-address' and
;; `user-full-name' are set at startup (startup.el / editfns.c); batch Emacs
;; without an identity leaves them empty.  `customize-package-emacs-version-alist'
;; is a plain `defvar' nil in GNU cus-edit.el (a GUI library not loaded here);
;; org.el pushes onto it while loading.
(unless (boundp 'user-mail-address) (defvar user-mail-address ""))
(unless (boundp 'user-full-name) (defvar user-full-name ""))
(unless (boundp 'customize-package-emacs-version-alist)
  (defvar customize-package-emacs-version-alist nil))

;; `custom-face-attributes' is a GNU cus-face.el defconst; org-faces.el reads it
;; while loading.  Load the staged GNU cus-face.el (root layout only) when no
;; provider defined it.
(when (and (anvil-standalone--elib-root)
           (not (boundp 'custom-face-attributes)))
  (condition-case nil
      (load (expand-file-name "vendor/staged-emacs-lisp/cus-face.el"
                              (anvil-standalone--elib-root))
            nil t)
    (error nil)))

;; org-src.el requires `sh-script' only to read `sh-ancestor-alist' (the shell
;; names for `org-src-lang-modes').  GNU sh-script.el needs treesit and smie,
;; which standalone lacks, so copy the defcustom default (GNU sh-script.el
;; 31.1) and provide the feature only while the Org modules load (withdrawn
;; after Port 11 together with `gnus-sum').
(defvar anvil-standalone--fake-sh-script
  (and (anvil-standalone--elib-root) (not (featurep 'sh-script)))
  "Non-nil when `sh-script' is provided only while the Org modules load.")
(when anvil-standalone--fake-sh-script
  (unless (boundp 'sh-ancestor-alist)
    (defvar sh-ancestor-alist
      '((ash . sh) (bash . jsh) (bash2 . jsh) (dash . ash) (dtksh . ksh)
        (es . rc) (itcsh . tcsh) (jcsh . csh) (jsh . sh) (ksh . ksh88)
        (ksh88 . jsh) (oash . sh) (pdksh . ksh88) (mksh . pdksh) (posix . sh)
        (tcsh . csh) (wksh . ksh88) (wsh . sh) (zsh . ksh88) (rpm . sh))))
  (provide 'sh-script))

;; org.el derives `org-mode-transpose-word-syntax-table' from GNU text-mode.el's
;; `text-mode-syntax-table'; load the vendored file when no provider did.
(when (and (anvil-standalone--elib-root)
           (not (boundp 'text-mode-syntax-table)))
  (condition-case nil
      (load (expand-file-name "vendor/emacs-lisp/textmodes/text-mode.el"
                              (anvil-standalone--elib-root))
            nil t)
    (error nil)))

;; `mode-require-final-newline' is a GNU files.el defcustom (default t) that
;; `org-mode' reads when it initialises a buffer.
(unless (boundp 'mode-require-final-newline)
  (defvar mode-require-final-newline t))

;; `with-silent-modifications' is a GNU subr.el macro (copied from 31.1; the
;; whole subr.el cannot load) used by org-element and other Org libraries.
(unless (fboundp 'with-silent-modifications)
  (defmacro with-silent-modifications (&rest body)
    "Execute BODY, pretending it does not modify the buffer."
    (declare (debug t) (indent 0))
    (let ((modified (make-symbol "modified")))
      `(let* ((,modified (buffer-modified-p))
              (buffer-undo-list t)
              (inhibit-read-only t)
              (inhibit-modification-hooks t))
         (unwind-protect
             (progn
               ,@body)
           (when (memq ,modified '(nil autosaved))
             (restore-buffer-modified-p ,modified)))))))

;; `org-mode' extends GNU dnd.el's `dnd-protocol-alist'; load the vendored file.
(when (and (anvil-standalone--elib-root) (not (boundp 'dnd-protocol-alist)))
  (condition-case nil
      (load (expand-file-name "vendor/emacs-lisp/dnd.el"
                              (anvil-standalone--elib-root))
            nil t)
    (error nil)))

;; GNU C primitives called by Org file visiting and capture.
;; `get-file-buffer' (buffer.c): the live buffer visiting FILENAME, else nil.
;; `current-idle-time' (keyboard.c): nil unless Emacs is idle; a batch run
;; never is, so nil matches GNU there.
(unless (fboundp 'get-file-buffer)
  (defun get-file-buffer (filename)
    "Return the buffer visiting FILENAME (a string), or nil (GNU buffer.c)."
    (let ((name (expand-file-name filename)) found)
      (dolist (b (buffer-list))
        (let ((f (buffer-local-value 'buffer-file-name b)))
          (when (and (not found) f (string= (expand-file-name f) name))
            (setq found b))))
      found)))
(unless (fboundp 'current-idle-time)
  (defun current-idle-time () nil))

;; More GNU pieces the Org tools reach at run time.  `epa-file-name-regexp' is
;; the GNU epa-hook.el default (epa-file.el is not loaded).  `indent-to' (C,
;; indent.c) is defined below, `untabify' from the
;; vendored GNU tabify.el.
(unless (boundp 'epa-file-name-regexp)
  (defvar epa-file-name-regexp "\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'"))
(when (anvil-standalone--elib-root)
  ;; `indent-line-to' is GNU indent.el.
  (unless (fboundp 'indent-line-to)
    (condition-case nil
        (load (expand-file-name "vendor/emacs-lisp/indent.el"
                                (anvil-standalone--elib-root))
              nil t)
      (error nil)))
  (unless (fboundp 'untabify)
    (condition-case nil
        (load (expand-file-name "vendor/emacs-lisp/tabify.el"
                                (anvil-standalone--elib-root))
              nil t)
      (error nil))))

;; `indent-to' (indent.c): insert whitespace from point to COLUMN, at least
;; MINIMUM columns, using tabs when `indent-tabs-mode' is set.  (Loading the
;; whole emacs-lib files-standalone-buffer.el for it breaks the http tools.)
;; `font-lock-flush' (font-lock.el) only acts when Font Lock is on, never so in
;; this runtime.  `store-match-data' is GNU's old name of `set-match-data'.
(unless (fboundp 'indent-to)
  (defun indent-to (column &optional minimum)
    "Indent from point to COLUMN, at least MINIMUM columns; return the column."
    (let* ((from (current-column))
           (mincol (+ from (max 0 (or minimum 0))))
           (target (max column mincol)))
      (when (> target from)
        (if indent-tabs-mode
            (let ((tw (max 1 tab-width)))
              (while (<= (* (1+ (/ from tw)) tw) target)
                (insert ?\t)
                (setq from (* (1+ (/ from tw)) tw)))
              (insert (make-string (- target from) ?\s)))
          (insert (make-string (- target from) ?\s))))
      target)))
;; `current-indentation' (indent.c): horizontal position of the first
;; non-blank character of the current line.  `just-one-space' is the GNU
;; simple.el definition (simple.el itself is not loaded).
(unless (fboundp 'current-indentation)
  (defun current-indentation ()
    "Return the indentation of the current line (indent.c semantics)."
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (current-column))))
(unless (fboundp 'just-one-space)
  (defun just-one-space (&optional n)
    "Delete all spaces and tabs around point, leaving one space (or N spaces).
If N is negative, delete newlines as well, leaving -N spaces."
    (interactive "*p")
    (let ((orig-pos (point))
          (skip-characters (if (and n (< n 0)) " \t\n\r" " \t"))
          (num (abs (or n 1))))
      (skip-chars-backward skip-characters)
      (when (fboundp 'constrain-to-field) (constrain-to-field nil orig-pos))
      (let* ((num (- num (skip-chars-forward " " (+ num (point)))))
             (mid (point))
             (end (progn
                    (skip-chars-forward skip-characters)
                    (if (fboundp 'constrain-to-field)
                        (constrain-to-field nil orig-pos t)
                      (point)))))
        (delete-region mid end)
        (insert (make-string num ?\s))))))
;; TEMPORARY, remove when the core fix lands: on NeLisp `display-warning'
;; leaves the caller's current buffer read-only.  Repro:
;;   (with-temp-buffer (display-warning '(a b) "hello") buffer-read-only)
;; => t on NeLisp, nil on GNU 31.1.  Restore the caller's value afterwards.
(when (and (anvil-standalone--elib-root) (fboundp 'display-warning)
           (fboundp 'advice-add)
           (not (get 'display-warning 'anvil-standalone-ro-guard)))
  (advice-add 'display-warning :around
              (lambda (orig &rest args)
                (let ((buf (current-buffer))
                      (ro buffer-read-only))
                  (unwind-protect (apply orig args)
                    (when (buffer-live-p buf)
                      (with-current-buffer buf
                        (setq buffer-read-only ro)))))))
  (put 'display-warning 'anvil-standalone-ro-guard t))
(unless (fboundp 'font-lock-flush)
  (defun font-lock-flush (&optional _beg _end) nil))
(unless (fboundp 'store-match-data)
  (defalias 'store-match-data #'set-match-data))

;; `pos-visible-in-window-p' (window.c): a batch run has no displayed window,
;; so no position is visible.
(unless (fboundp 'pos-visible-in-window-p)
  (defun pos-visible-in-window-p (&optional _pos _window _partially) nil))

;; org.el has (eval-when-compile (require 'gnus-sum)), which evaluates when
;; the source is loaded uncompiled.  Gnus is not vendored, and Org uses gnus-sum
;; only for compile-time macros, so declare the feature while the Org-using
;; modules load (anvil-file, Port 6, Port 11) and withdraw it after Port 11.
(defvar anvil-standalone--fake-gnus-sum
  (and (anvil-standalone--elib-available-p) (not (featurep 'gnus-sum)))
  "Non-nil when `gnus-sum' is provided only while the Org modules load.")
(when anvil-standalone--fake-gnus-sum (provide 'gnus-sum))

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
      (when (anvil-standalone--elib-available-p)
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
                          (and (anvil-standalone--elib-available-p)
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
(let ((vendor (anvil-standalone--elib-vendor-dir)))
  (when (and vendor (not (featurep 'tabulated-list)))
    (condition-case err
        (load (expand-file-name "emacs-lisp/tabulated-list.el" vendor) nil t)
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
;;  * anvil-elisp: loaded by Port 20 (S14.4), which supplies the missing GNU
;;    functions; see there for the one tool left out.
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
            (not (anvil-standalone--elib-available-p)))
  (condition-case err
      (progn
        (anvil-standalone--load-anvil-sexp)
        (anvil-sexp-enable)
        (dolist (id '("sexp-verify" "sexp-macroexpand" "sexp-wrap-form"
                      "sexp-rename-symbol" "sexp-replace-call"))
          (anvil-server-unregister-tool id "emacs-eval")))
    (error (push (cons 'anvil-sexp err) anvil-standalone--port10-errors))))

(defvar anvil-standalone--port11-unregister
  '("org-agenda-view")
  "Ids of anvil-org tools that still answer differently from host Emacs.
org-read-outline/headline/by-id, org-edit-body, org-rename-headline,
org-add-todo, org-update-todo-state, org-capture-string and org-habit-summary
match the host since `anvil-standalone-org.el' supplies the missing GNU
functions (the genuine org-capture.el and org-habit.el are loaded on the first
call of their tool, with the base buffer standing in for the dead
`make-indirect-buffer').  Still blocked: org-agenda-view.  The genuine
org-agenda.el needs org-element (`org-element-deferred-create' is void after
`seq-difference' is shimmed) plus org-agenda-list and the calendar/diary
machinery, none of which exist in this runtime.")
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
;;    org-get-todo-config, org-get-tag-config, plus (wave 2, shims in
;;    tools/anvil-standalone-org.el) org-read-outline, org-read-headline and
;;    org-read-by-id, org-edit-body, org-rename-headline, org-add-todo and
;;    org-update-todo-state, org-capture-string and org-habit-summary.  Only
;;    org-agenda-view is unregistered in `anvil-standalone--port11-unregister'.
;;  * tools/anvil-standalone-org.el is a separate file (loaded relative to the
;;    loader) so the many GNU Org shims stay out of this long loader; all are
;;    fboundp/boundp guarded.
;;  * File access: `anvil-org-allowed-files' defaults to nil with
;;    `anvil-org-allowed-files-enabled' t, so the tools see no file unless the
;;    caller configures it; the agenda/habit tools take their files from that
;;    list, never from the user's real `org-agenda-files'.
(defvar anvil-standalone--port11-errors nil
  "Alist of (MODULE . ERROR) for port-11 modules that failed to load.")
(unless (or (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
            (not (anvil-standalone--elib-available-p)))
  (let* ((vendor (anvil-standalone--elib-vendor-dir))
         (fake-url-util (not (featurep 'url-util))))
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
    (load (expand-file-name "anvil-standalone-org.el"
                            (file-name-directory (or load-file-name buffer-file-name)))
          nil t)
    (when fake-url-util (provide 'url-util))
    (unwind-protect
        (condition-case err
            (progn
              (require 'org-version)
              (require 'rx)
              (require 'anvil-org)
              (anvil-org-enable)
              (anvil-standalone-org-install-lazy-wrappers)
              (dolist (id anvil-standalone--port11-unregister)
                (anvil-server-unregister-tool id "emacs-eval")))
          (error (push (cons 'anvil-org err) anvil-standalone--port11-errors)))
      (when fake-url-util (setq features (delq 'url-util features)))
      (when anvil-standalone--fake-sh-script
        (setq features (delq 'sh-script features)))
      (when anvil-standalone--fake-gnus-sum
        (setq features (delq 'gnus-sum features))))))


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
(defvar anvil-standalone--port12-unregister nil
  "Orchestrator tool ids that are registered by the module but not served.
None: orchestrator-cancel and orchestrator-stop give the host answers once
Port 21 reports the signal a process died of.")
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
;;    is declared only while the module loads.  bench-profile-expr runs with
;;    no sampler (empty :top, see `anvil-standalone--port13-unregister').
;;  * anvil-org-index: DB path moved to the isolated cache; `message' is muted
;;    around the handlers because on NeLisp it writes to the MCP channel.
(defvar anvil-standalone--port13-errors nil
  "Alist of (MODULE . ERROR) for port-13 modules that failed to load.")
(defvar anvil-standalone--port13-unregister
  '("org-index-rebuild")
  "Port-13 tool ids registered by their module but not served.
org-index-rebuild: an offload tool; the offload worker reports
`void-function anvil-org-index--tool-rebuild' because standalone has no worker
Emacs that can load the module.
bench-profile-expr is served (S14.6): the module wraps `profiler-start' in
`ignore-errors', so without a sampling profiler it reports the timed loop
with an empty :top and :total-samples 0, the host answer for an expression
too short to be sampled.  Sampled frames are timing-dependent on the host and
are not produced here.")
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
None: bench-profile-expr moved to the served set (see Port 13).")
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
(when (and (anvil-standalone--elib-available-p)
           (not (boundp 'process-coding-system-alist)))
  (anvil-standalone--elib-load "emacs-cc-coding-2.el" t))
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

;; Port 17: url stack (S13.4): anvil-http, anvil-web, anvil-uri, anvil-semantic.
;; Gated like Port 10/11: without a nelisp-emacs-lib checkout nothing is loaded.
;; The vendored GNU url/net/mail/gnus/calendar/international/emacs-lisp
;; directories join `load-path' (resolved from the compat dir, as in Port 11), so
;; the real url.el, url-http.el and url-util.el serve `url-retrieve-synchronously'
;; over `make-network-process' (verified against a local server).  Shims below are
;; fboundp/boundp guarded and copy GNU source or nelisp-emacs-lib providers:
;;  * `copy-syntax-table' needs emacs-syntax-table.el (native `copy-sequence'
;;    cannot copy a syntax char-table); coding-system-list and friends come from
;;    emacs-cc-coding-1/2.el and emacs-parity-fns2.el.
;;  * `custom-declare-face' and `locate-user-emacs-file' are the
;;    nelisp-emacs-lib providers (emacs-stub.el, emacs-fileio.el) copied here.
;;  * `file-expand-wildcards' and `wildcard-to-regexp' are copied from GNU
;;    files.el; `file-local-name' and `file-accessible-directory-p' from GNU
;;    files.el as well.
;; Real url-util is loaded afterwards; Port 11's url-util fake only applies when
;; the feature is absent, and S11.1/S11.3 are rerun to confirm it still works.
;; `call-process' with DESTINATION nil must discard the child's output (GNU
;; process.c).  NeLisp lets the child inherit stdout, and smime.el's
;; `smime-openssl-program' defcustom runs `openssl version' that way at load
;; time, writing a banner into the MCP JSON channel.  Route a nil destination
;; to /dev/null (the file redirect the runtime does honour); every other destination is passed through.
(unless (fboundp 'anvil-standalone--call-process-orig)
  (defalias 'anvil-standalone--call-process-orig (symbol-function 'call-process))
  (defun call-process (program &optional infile destination display &rest args)
    "Like `call-process', but a nil DESTINATION discards the output."
    (if destination
        (apply #'anvil-standalone--call-process-orig
               program infile destination display args)
      (apply #'anvil-standalone--call-process-orig
             program infile "/dev/null" display args))))
(defvar anvil-standalone--syntax-fns
  '(char-syntax standard-syntax-table make-syntax-table copy-syntax-table
    string-to-syntax modify-syntax-entry syntax-table set-syntax-table
    parse-partial-sexp)
  "GNU syntax-table names that emacs-syntax-table.el defines.")
(defvar anvil-standalone--provider-syntax-fns nil
  "Alist of (NAME . provider definition) from emacs-syntax-table.el.")
(defvar anvil-standalone--port17-errors nil
  "Alist of (MODULE . ERROR) for port-17 modules that failed to load.")
(defvar anvil-standalone--port17-unregister
  nil
  "Port-17 tool ids registered by their module but not served, with blockers.
None: http-fetch-batch, http-cache-clear, web-reddit-thread and
semantic-embed-index give the host answers on the S13.7 fixture.")
(unless (or (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
            (not (anvil-standalone--elib-available-p)))
  (let* ((vendor (anvil-standalone--elib-vendor-dir)))
    (condition-case err
        (progn
          ;; The provider's syntax-table functions (char-table backed) must
          ;; not be mixed with the runtime's: provider modify-syntax-entry on
          ;; a runtime table breaks sexp-replace-defun (arrayp (1)).  Keep the
          ;; runtime set globally and use the provider set only while the url
          ;; stack loads (rfc2047 calls set-char-table-range on a fresh table).
          (let ((saved (mapcar (lambda (f)
                                 (cons f (and (fboundp f) (symbol-function f))))
                               anvil-standalone--syntax-fns)))
            (anvil-standalone--elib-load "emacs-syntax-table.el" t)
            (setq anvil-standalone--provider-syntax-fns
                  (mapcar (lambda (f)
                            (cons f (and (fboundp f) (symbol-function f))))
                          anvil-standalone--syntax-fns))
            (dolist (s saved)
              (when (cdr s) (fset (car s) (cdr s)))))
          ;; emacs-parity-fns2.el may fail late on a vc-hooks require after
          ;; defining coding-system-list; that tail error is ignored.
          (dolist (f '("emacs-cc-coding-1.el" "emacs-cc-coding-2.el"
                       "emacs-parity-fns2.el" "emacs-subr-extras.el"
                       "emacs-easy-mmode.el"))
            (condition-case nil (anvil-standalone--elib-load f t)
              (error nil)))
          (add-to-list 'load-path vendor t)
          (dolist (d '("net" "url" "mail" "gnus" "calendar" "international"
                       "emacs-lisp"))
            (add-to-list 'load-path (expand-file-name d vendor) t)))
      (error (push (cons 'url-stack-prep err) anvil-standalone--port17-errors)))))
(unless (boundp 'current-language-environment)
  (defvar current-language-environment "UTF-8"))
(dolist (v '(minibuffer-local-completion-map minibuffer-local-must-match-map
             minibuffer-local-map))
  (unless (boundp v) (set v (make-sparse-keymap))))
;; Genuine minibuffer.el forms (GNU Emacs), copied verbatim: gnus-util.el and url
;; read these at load time.
(unless (boundp 'completion-styles-alist)
  (defvar completion-styles-alist
    '((emacs21
       completion-emacs21-try-completion completion-emacs21-all-completions
       "Simple prefix-based completion.
  I.e. when completing \"foo_bar\" (where _ is the position of point),
  it will consider all completions candidates matching the glob
  pattern \"foobar*\".")
      (emacs22
       completion-emacs22-try-completion completion-emacs22-all-completions
       "Prefix completion that only operates on the text before point.
  I.e. when completing \"foo_bar\" (where _ is the position of point),
  it will consider all completions candidates matching the glob
  pattern \"foo*\" and will add back \"bar\" to the end of it.")
      (basic
       completion-basic-try-completion completion-basic-all-completions
       "Completion of the prefix before point and the suffix after point.
  I.e. when completing \"foo_bar\" (where _ is the position of point),
  it will consider all completions candidates matching the glob
  pattern \"foo*bar*\".")
      (partial-completion
       completion-pcm-try-completion completion-pcm-all-completions
       "Completion of multiple words, each one taken as a prefix.
  I.e. when completing \"l-co_h\" (where _ is the position of point),
  it will consider all completions candidates matching the glob
  pattern \"l*-co*h*\".
  Furthermore, for completions that are done step by step in subfields,
  the method is applied to all the preceding fields that do not yet match.
  E.g. C-x C-f /u/mo/s TAB could complete to /usr/monnier/src.
  Additionally the user can use the char \"*\" as a glob pattern.")
      (substring
       completion-substring-try-completion completion-substring-all-completions
       "Completion of the string taken as a substring.
  I.e. when completing \"foo_bar\" (where _ is the position of point),
  it will consider all completions candidates matching the glob
  pattern \"*foo*bar*\".")
      (flex
       completion-flex-try-completion completion-flex-all-completions
       "Completion of an in-order subset of characters.
  When completing \"foo\" the glob \"*f*o*o*\" is used, so that
  \"foo\" can complete to \"frodo\".")
      (initials
       completion-initials-try-completion completion-initials-all-completions
       "Completion of acronyms and initialisms.
  E.g. can complete M-x lch to list-command-history
  and C-x C-f ~/sew to ~/src/emacs/work.")
      (shorthand
       completion-shorthand-try-completion completion-shorthand-all-completions
       "Completion of symbol shorthands setup in `read-symbol-shorthands'.
  E.g. can complete \"x-foo\" to \"xavier-foo\" if the shorthand
  ((\"x-\" . \"xavier-\")) is set up in the buffer of origin."))
    "List of available completion styles.
  Each element has the form (NAME TRY-COMPLETION ALL-COMPLETIONS DOC):
  where NAME is the name that should be used in `completion-styles',
  TRY-COMPLETION is the function that does the completion (it should
  follow the same calling convention as `completion-try-completion'),
  ALL-COMPLETIONS is the function that lists the completions (it should
  follow the calling convention of `completion-all-completions'),
  and DOC describes the way this style of completion works."))
(unless (boundp 'completion-styles)
  (defcustom completion-styles
    ;; First, use `basic' because prefix completion has been the standard
    ;; for "ever" and works well in most cases, so using it first
    ;; ensures that we obey previous behavior in most cases.
    '(basic
      ;; Then use `partial-completion' because it has proven to
      ;; be a very convenient extension.
      partial-completion
      ;; Finally use `emacs22' so as to maintain (in many/most cases)
      ;; the previous behavior that when completing "foobar" with point
      ;; between "foo" and "bar" the completion try to complete "foo"
      ;; and simply add "bar" to the end of the result.
      emacs22)
    "List of completion styles to use.
  The available styles are listed in `completion-styles-alist'.
  
  Note that `completion-category-overrides' may override these
  styles for specific categories, such as files, buffers, etc."
    :type completion--styles-type
    :version "23.1"))
(unless (boundp 'completion-category-defaults)
  (defvar completion-category-defaults
    '((buffer (styles . (basic substring)))
      (unicode-name (styles . (basic substring)))
      ;; A new style that combines substring and pcm might be better,
      ;; e.g. one that does not anchor to bos.
      (project-file (styles . (substring)))
      (xref-location (styles . (substring)))
      (info-menu (styles . (basic substring)))
      (symbol-help (styles . (basic shorthand substring)))
      (calendar-month (display-sort-function . identity)))
    "Default settings for specific completion categories.
  
  Each entry has the shape (CATEGORY . ALIST) where ALIST is
  an association list that can specify properties such as:
  - `styles': the list of `completion-styles' to use for that category.
  - `cycle': the `completion-cycle-threshold' to use for that category.
  - `cycle-sort-function': function to sort entries when cycling.
  - `display-sort-function': function to sort entries in *Completions*.
  - `group-function': function for grouping the completion candidates.
  - `annotation-function': function to add annotations in *Completions*.
  - `affixation-function': function to prepend/append a prefix/suffix.
  
  Categories are symbols such as `buffer' and `file', used when
  completing buffer and file names, respectively.
  
  Also see `completion-category-overrides'."))
(unless (boundp 'completion-category-overrides)
  (defcustom completion-category-overrides nil
    "List of category-specific user overrides for completion metadata.
  
  Each override has the shape (CATEGORY . ALIST) where ALIST is
  an association list that can specify properties such as:
  - `styles': the list of `completion-styles' to use for that category.
  - `cycle': the `completion-cycle-threshold' to use for that category.
  - `cycle-sort-function': function to sort entries when cycling.
  - `display-sort-function': nil means to use either the sorting
  function from metadata, or if that is nil, fall back to `completions-sort';
  `identity' disables sorting and keeps the original order; and other
  possible values are the same as in `completions-sort'.
  - `group-function': function for grouping the completion candidates.
  - `annotation-function': function to add annotations in *Completions*.
  - `affixation-function': function to prepend/append a prefix/suffix.
  See more description of metadata in `completion-metadata'.
  
  Categories are symbols such as `buffer' and `file', used when
  completing buffer and file names, respectively.
  
  If a property in a category is specified by this variable, it
  overrides the default specified in `completion-category-defaults'."
    :version "25.1"
    :type `(alist :key-type (choice :tag "Category"
  				  (const buffer)
                                    (const file)
                                    (const unicode-name)
  				  (const bookmark)
                                    symbol)
            :value-type
            (set :tag "Properties to override"
  	   (cons :tag "Completion Styles"
  		 (const :tag "Select a style from the menu;" styles)
  		 ,completion--styles-type)
             (cons :tag "Completion Cycling"
  		 (const :tag "Select one value from the menu." cycle)
                   ,completion--cycling-threshold-type)
             (cons :tag "Cycle Sorting"
                   (const :tag "Select one value from the menu."
                          cycle-sort-function)
                   (choice (function :tag "Custom function")))
             (cons :tag "Completion Sorting"
                   (const :tag "Select one value from the menu."
                          display-sort-function)
                   (choice (const :tag "Use default" nil)
                           (const :tag "No sorting" identity)
                           (const :tag "Alphabetical sorting"
                                  minibuffer-sort-alphabetically)
                           (const :tag "Historical sorting"
                                  minibuffer-sort-by-history)
                           (function :tag "Custom function")))
             (cons :tag "Completion Groups"
                   (const :tag "Select one value from the menu."
                          group-function)
                   (choice (function :tag "Custom function")))
             (cons :tag "Completion Annotation"
                   (const :tag "Select one value from the menu."
                          annotation-function)
                   (choice (function :tag "Custom function")))
             (cons :tag "Completion Affixation"
                   (const :tag "Select one value from the menu."
                          affixation-function)
                   (choice (function :tag "Custom function")))))))
(unless (fboundp 'coding-system-p)
  (defun coding-system-p (object)
    (and (or (null object) (symbolp object))
         (or (null object) (and (get object 'coding-system) t)
             (and (memq object '(no-conversion undecided prefer-utf-8 raw-text
                                 ascii us-ascii utf-8 utf-8-unix utf-8-dos
                                 utf-8-mac utf-8-emacs utf-16 latin-1
                                 iso-8859-1 binary emacs-mule escape-quoted))
                  t))
         t)))
(unless (boundp 'sort-coding-systems-predicate)
  (defvar sort-coding-systems-predicate nil))
(unless (fboundp 'sort-coding-systems)
  (defun sort-coding-systems (codings)
    (if sort-coding-systems-predicate
        (sort codings sort-coding-systems-predicate)
      codings)))
(unless (fboundp 'custom-declare-face)
  (defun custom-declare-face (face spec doc &rest args)
    "Standalone load-time fallback for evaluated Custom faces."
    (when (fboundp 'emacs-faces-make-face)
      (emacs-faces-make-face face)
      (let ((attrs (and (fboundp 'emacs-faces-default-attrs-from-spec)
                        (emacs-faces-default-attrs-from-spec spec))))
        (when (and attrs (fboundp 'emacs-faces-set-attribute))
          (apply #'emacs-faces-set-attribute face nil attrs))))
    (put face 'face-defface-spec spec)
    (put face 'face-documentation doc)
    (put face 'custom-args args)
    face))
(unless (fboundp 'locate-user-emacs-file)
  (defun locate-user-emacs-file (new-name &optional old-name)
    "Return an absolute per-user Emacs-specific file name for NEW-NAME."
    (let ((bestname (expand-file-name new-name user-emacs-directory)))
      (if (and old-name (not (file-readable-p bestname)))
          (let ((oldname (expand-file-name old-name "~")))
            (if (file-readable-p oldname) oldname bestname))
        bestname))))
(unless (fboundp 'file-local-name)
  (defun file-local-name (file)
    (or (and (fboundp 'file-remote-p) (file-remote-p file 'localname)) file)))
(unless (fboundp 'file-accessible-directory-p)
  (defun file-accessible-directory-p (filename)
    (and (file-directory-p filename) (file-executable-p filename))))
(unless (fboundp 'file-expand-wildcards)
  (defun file-expand-wildcards (pattern &optional full regexp)
    "Expand (a.k.a. \"glob\") file-name wildcard pattern PATTERN.
  This returns a list of file names that match PATTERN.
  The returned list of file names is sorted in the `string<' order.

  PATTERN is, by default, a \"glob\"/wildcard string, e.g.,
  \"/tmp/*.png\" or \"/*/*/foo.png\", but can also be a regular
  expression if the optional REGEXP parameter is non-nil.  In any
  case, the matches are applied per sub-directory, so a match can't
  span a parent/sub directory, which means that a regexp bit can't
  contain the \"/\" character.

  The returned list of file names is sorted in the `string<' order.

  If PATTERN is written as an absolute file name, the expansions in
  the returned list are also absolute.

  If PATTERN is written as a relative file name, it is interpreted
  relative to the current `default-directory'.
  The file names returned are normally also relative to the current
  default directory.  However, if FULL is non-nil, they are absolute."
    (save-match-data
      (let* ((nondir (file-name-nondirectory pattern))
  	   (dirpart (file-name-directory pattern))
  	   ;; A list of all dirs that DIRPART specifies.
  	   ;; This can be more than one dir
  	   ;; if DIRPART contains wildcards.
  	   (dirs (if (and dirpart
  			  (string-match "[[*?]" (file-local-name dirpart)))
  		     (mapcar #'file-name-as-directory
  			     (file-expand-wildcards
                                (directory-file-name dirpart) nil regexp))
  		   (list dirpart)))
  	   contents)
        (dolist (dir (nreverse dirs))
  	(when (or (null dir)	; Possible if DIRPART is not wild.
  		  (file-accessible-directory-p dir))
            (if (equal "" nondir)
                ;; `nondir' is "" when the pattern ends in "/".  Basically ""
                ;; refers to the directory itself, like ".", but it's not
                ;; among the names returned by `directory-files', so we have
                ;; to special-case it.
                (push (or dir nondir) contents)
  	    (let ((this-dir-contents
  		   ;; Filter out "." and ".."
  		   (delq nil
                           (mapcar (lambda (name)
                                     (unless (string-match "\\`\\.\\.?\\'"
                                                           (file-name-nondirectory
                                                            name))
                                       name))
  			         (directory-files
                                    (or dir ".") full
                                    (if regexp
                                        ;; We're matching each file name
                                        ;; element separately.
                                        (concat "\\`" nondir "\\'")
  				   (wildcard-to-regexp nondir)))))))
  	      (setq contents
  		    (nconc
  		     (if (and dir (not full))
  			 (mapcar (lambda (name) (concat dir name))
  			         this-dir-contents)
  		       this-dir-contents)
  		     contents))))))
        contents))))
(unless (fboundp 'wildcard-to-regexp)
  (defun wildcard-to-regexp (wildcard)
    "Given a shell file name pattern WILDCARD, return an equivalent regexp.
  The generated regexp will match a file name only if the file name
  matches that wildcard according to shell rules.  Only wildcards known
  by `sh' are supported."
    (let* ((i (string-match "[[.*+\\^$?]" wildcard))
  	 ;; Copy the initial run of non-special characters.
  	 (result (substring wildcard 0 i))
  	 (len (length wildcard)))
      ;; If no special characters, we're almost done.
      (if i
  	(while (< i len)
  	  (let ((ch (aref wildcard i))
  		j)
  	    (setq
  	     result
  	     (concat result
  		     (cond
  		      ((and (eq ch ?\[)
  			    (< (1+ i) len)
  			    (eq (aref wildcard (1+ i)) ?\]))
  		       "\\[")
  		      ((eq ch ?\[)	; [...] maps to regexp char class
  		       (progn
  			 (setq i (1+ i))
  			 (concat
  			  (cond
  			   ((eq (aref wildcard i) ?!) ; [!...] -> [^...]
  			    (progn
  			      (setq i (1+ i))
  			      (if (eq (aref wildcard i) ?\])
  				  (progn
  				    (setq i (1+ i))
  				    "[^]")
  				"[^")))
  			   ((eq (aref wildcard i) ?^)
  			    ;; Found "[^".  Insert a `\0' character
  			    ;; (which cannot happen in a filename)
  			    ;; into the character class, so that `^'
  			    ;; is not the first character after `[',
  			    ;; and thus non-special in a regexp.
  			    (progn
  			      (setq i (1+ i))
  			      "[\000^"))
  			   ((eq (aref wildcard i) ?\])
  			    ;; I don't think `]' can appear in a
  			    ;; character class in a wildcard, but
  			    ;; let's be general here.
  			    (progn
  			      (setq i (1+ i))
  			      "[]"))
  			   (t "["))
  			  (prog1	; copy everything up to next `]'.
  			      (substring wildcard
  					 i
  					 (setq j (string-search
  						  "]" wildcard i)))
  			    (setq i (if j (1- j) (1- len)))))))
  		      ((eq ch ?.)  "\\.")
  		      ((eq ch ?*)  "[^\000]*")
  		      ((eq ch ?+)  "\\+")
  		      ((eq ch ?^)  "\\^")
  		      ((eq ch ?$)  "\\$")
  		      ((eq ch ?\\) "\\\\") ; probably cannot happen...
  		      ((eq ch ??)  "[^\000]")
  		      (t (char-to-string ch)))))
  	    (setq i (1+ i)))))
      ;; Shell wildcards should match the entire filename,
      ;; not its part.  Make the regexp say so.
      (concat "\\`" result "\\'")))
  )
;; GNU subr.el: docstring filler used by `substitute-command-keys'-era macros.
(unless (fboundp 'internal--fill-string-single-line)
  (defun internal--fill-string-single-line (str)
    "Fill string STR to `fill-column'."
    (if (< (length str) fill-column)
        str
      (let* ((limit (min fill-column (length str)))
             (fst (substring str 0 limit))
             (lst (substring str limit)))
        (cond ((string-match "\\( \\)$" fst)
               (setq fst (replace-match "\n" nil nil fst 1)))
              ((string-match "^ \\(.*\\)" lst)
               (setq fst (concat fst "\n"))
               (setq lst (match-string 1 lst)))
              ((string-match ".*\\( \\(.+\\)\\)$" fst)
               (setq lst (concat (match-string 2 fst) lst))
               (setq fst (replace-match "\n" nil nil fst 1))))
        (concat fst (internal--fill-string-single-line lst))))))
(unless (fboundp 'internal--format-docstring-line)
  (defun internal--format-docstring-line (string &rest objects)
    "Format a single line from a documentation string out of STRING and OBJECTS."
    (when (string-match "\n" string)
      (error "Unable to fill string containing newline: %S" string))
    (internal--fill-string-single-line (apply #'format string objects))))
;; Abbrev tables: nelisp-emacs-lib provider (obarray-backed define-abbrev-table).
(when (and (anvil-standalone--elib-available-p)
           (not (fboundp 'make-abbrev-table)))
  (dolist (f '("emacs-symbol.el" "emacs-parity-abbrev.el"))
    (condition-case nil (anvil-standalone--elib-load f t)
      (error nil))))
;; C variables that are nil in a GNU batch session.
(dolist (v '(window-system initial-window-system))
  (unless (boundp v) (eval (list (quote defvar) v nil) t)))
;; GNU byte-run.el
(unless (fboundp 'function-put)
  (defun function-put (func prop value)
    "Set FUNCTION's property PROP to VALUE."
    (put func prop value)))
;; GNU gv.el: (gv-define-simple-setter symbol-value set).  mailcap.el does
;; (push X (symbol-value storage)).
(unless (get 'symbol-value 'cl-simple-setter)
  (put 'symbol-value 'cl-simple-setter 'set))
;; Case conversion of a region (a C primitive in GNU casefiddle.c): rewrite the
;; text in place, keeping point.  `region-noncontiguous-p' is not honoured.
(unless (fboundp 'downcase-region)
  (defun downcase-region (beg end &optional _region-noncontiguous-p)
    "Convert the region to lower case."
    (let ((b (min beg end)) (e (max beg end)))
      (let ((new (downcase (buffer-substring b e))))
        (unless (equal new (buffer-substring b e))
          (save-excursion (goto-char b) (delete-region b e) (insert new)))))
    nil))
(unless (fboundp 'upcase-region)
  (defun upcase-region (beg end &optional _region-noncontiguous-p)
    "Convert the region to upper case."
    (let ((b (min beg end)) (e (max beg end)))
      (let ((new (upcase (buffer-substring b e))))
        (unless (equal new (buffer-substring b e))
          (save-excursion (goto-char b) (delete-region b e) (insert new)))))
    nil))
;; Time arithmetic (C primitives in GNU timefns.c).  The runtime's
;; `time-subtract' already returns a float number of seconds, so these follow
;; it: results are float seconds, which `float-time' and the comparisons accept.
(unless (fboundp 'time-add)
  (defun time-add (a b)
    "Return the sum of time values A and B as float seconds."
    (+ (float-time a) (float-time b))))
(unless (fboundp 'time-less-p)
  (defun time-less-p (a b)
    "Return non-nil if time value A is earlier than B."
    (< (float-time a) (float-time b))))
(unless (fboundp 'time-equal-p)
  (defun time-equal-p (a b)
    "Return non-nil if time values A and B are equal."
    (= (float-time a) (float-time b))))
;; `standalone-compat/url-parse.el' (a minimal stub for the nelisp-agent
;; examples) shadows GNU url-parse.el and lacks these helpers; the real file
;; cannot load (it requires auth-source -> icons).  Bodies are GNU url-parse.el.
(unless (fboundp 'url-path-and-query)
  (defun url-path-and-query (urlobj)
    "Return the path and query components of URLOBJ as (PATH . QUERY)."
    (let ((name (url-filename urlobj))
          path query)
      (when name
        (if (string-match "\\?" name)
            (setq path  (substring name 0 (match-beginning 0))
                  query (substring name (match-end 0)))
          (setq path name)))
      (cons path query))))
(unless (fboundp 'url-port-if-non-default)
  (defun url-port-if-non-default (urlobj)
    "Return the port number of URLOBJ if it is not the scheme default."
    (let ((port (url-portspec urlobj))
          type)
      (and port
           (or (null (setq type (url-type urlobj)))
               (not (equal port (url-scheme-get-property type 'default-port))))
           port))))
(unless (fboundp 'url-recreate-url)
  (defun url-recreate-url (urlobj)
    "Recreate a URL string from the parsed URLOBJ."
    (let* ((type (url-type urlobj))
           (user (url-user urlobj))
           (pass (url-password urlobj))
           (host (url-host urlobj))
           (port (url-port-if-non-default urlobj))
           (file (url-filename urlobj))
           (frag (url-target urlobj)))
      (concat (if type (concat type ":"))
              (if (url-fullness urlobj) "//")
              (if (or user pass)
                  (concat user (if pass (concat ":" pass)) "@"))
              host
              (if port (format ":%d" (url-port urlobj)))
              (or file "/")
              (if frag (concat "#" frag))))))
;; GNU subr.el macro.
(unless (fboundp 'bound-and-true-p)
  (defmacro bound-and-true-p (var)
    "Return the value of symbol VAR if it is bound, else nil."
    (list 'and (list 'boundp (list 'quote var)) var)))
;; Process-buffer bookkeeping.  `make-network-process' ignores :buffer
;; (`process-buffer' stays nil until `set-process-buffer'), and the runtime has
;; no `get-buffer-process' (`process-list' is empty even with live processes).
;; url-http.el depends on both: its filter and sentinel run in
;; (process-buffer proc), and `url-retrieve-synchronously' polls
;; (get-buffer-process proc-buffer).  Apply :buffer after creation and record
;; the pairing so `get-buffer-process' can answer.
(defvar anvil-standalone--buffer-processes nil
  "Alist of (BUFFER . PROCESS) for processes given a buffer by the url stack.")
(unless (fboundp 'anvil-standalone--make-network-process-orig)
  (defalias 'anvil-standalone--make-network-process-orig
    (symbol-function 'make-network-process))
  (defun make-network-process (&rest args)
    "Like `make-network-process', but honour the :buffer argument."
    (let ((proc (apply #'anvil-standalone--make-network-process-orig args))
          (buf (plist-get args :buffer)))
      (when buf
        (when (stringp buf) (setq buf (get-buffer-create buf)))
        (set-process-buffer proc buf)
        (push (cons buf proc) anvil-standalone--buffer-processes))
      proc)))
(unless (fboundp 'get-buffer-process)
  (defun get-buffer-process (buffer)
    "Return the live process whose buffer is BUFFER, or nil."
    (when (stringp buffer) (setq buffer (get-buffer buffer)))
    (let (found)
      (dolist (pair anvil-standalone--buffer-processes)
        (when (and (not found) (eq (car pair) buffer)
                   (memq (process-status (cdr pair))
                         '(run open listen connect stop)))
          (setq found (cdr pair))))
      found)))
;; GNU timer.el (pulled in by mm-util) declares timers with
;; `(cl-defstruct (timer (:type vector)))'.  This runtime's `cl-defstruct' ignores
;; `:type vector' and builds a record, so timer.el's vector-based `timerp' rejects
;; its own (setf (timer--time ...))' is unsupported.  The runtime already has a
;; native timer implementation (cancel-timer', sleep-for' (C, keyboard.c): wait SECONDS while processing process output.
(unless (fboundp 'sleep-for)
  (defun sleep-for (seconds &optional millisec)
    "Wait SECONDS (plus MILLISEC), handling process output; return nil."
    (accept-process-output nil (+ seconds (if millisec (/ millisec 1000.0) 0)))
    nil))
;; GNU timer.el (pulled in by mm-util) declares timers with
;; (cl-defstruct (timer (:type vector))).  This runtime's `cl-defstruct' ignores
;; :type vector and builds a record, so timer.el's vector-based `timerp' rejects
;; its own timer-create result (wrong-type-argument timerp #s(timer ...)) and
;; (setf (timer--time ...)) is unsupported.  The runtime already has native
;; timers (`run-at-time', `cancel-timer', `timerp'), so declare the feature and
;; leave timer.el unloaded.
(when (and (not (featurep 'timer))
           (fboundp 'run-at-time) (fboundp 'cancel-timer) (fboundp 'timerp))
  (provide 'timer))
;; GNU `sleep-for' (C, keyboard.c): wait SECONDS while processing process output.
(unless (fboundp 'sleep-for)
  (defun sleep-for (seconds &optional millisec)
    "Wait SECONDS (plus MILLISEC), handling process output; return nil."
    (accept-process-output nil (+ seconds (if millisec (/ millisec 1000.0) 0)))
    nil))
;; The prelude `cl-loop' rejects `for F in L if F do ... append ...' (gnutls.el);
;; nelisp-emacs-lib's emacs-cl-macros.el expands it.  It replaces `cl-loop' only
;; while the url stack loads, then the prelude macro returns.
(unless (or (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
            (not (anvil-standalone--elib-available-p)))
  (let ((cl-macros (anvil-standalone--elib-file "emacs-cl-macros.el"))
        (old-loop (symbol-function 'cl-loop))
        (old-syntax (mapcar (lambda (f)
                              (cons f (and (fboundp f) (symbol-function f))))
                            anvil-standalone--syntax-fns))
        ;; nelisp-http requires nelisp-process, whose cl-defstruct accessor
        ;; `nelisp-process-status' and `nelisp-make-process' replace the
        ;; runtime natives that `process-status' and `make-process' use;
        ;; orchestrator-collect then never sees its task exit.  Keep them.
        (natives (delq nil (mapcar (lambda (f)
                                     (and (fboundp f)
                                          (cons f (symbol-function f))))
                                   '(nelisp-process-status
                                     nelisp-make-process))))
        ;; url pulls in GNU time-date.el, whose `date-to-time' fails on this
        ;; runtime for the form the journal shim (Port 15) handles.
        (old-d2t (and (fboundp 'date-to-time)
                      (symbol-function 'date-to-time))))
    (unwind-protect
        (progn
          ;; The packaged layout has no emacs-cl-macros.el (cl-lib comes from the
          ;; vendored GNU sources), so the runtime `cl-loop' then stays.
          (when cl-macros (fmakunbound 'cl-loop))
          (dolist (p anvil-standalone--provider-syntax-fns)
            (when (cdr p) (fset (car p) (cdr p))))
          (when cl-macros (load cl-macros t t))
          (dolist (f '(mail-utils url-cache url-handlers url url-http anvil-http anvil-web
                       anvil-uri anvil-semantic))
            (condition-case err (require f)
              (error (push (cons f err) anvil-standalone--port17-errors)))))
      (fset 'cl-loop old-loop)
      (dolist (o old-syntax)
        (if (cdr o) (fset (car o) (cdr o)) (fmakunbound (car o))))
      (dolist (n natives) (fset (car n) (cdr n)))
      (when (and old-d2t (not (eq old-d2t (symbol-function 'date-to-time))))
        (let ((gnu (symbol-function 'date-to-time)))
          (fset 'date-to-time
                (lambda (date)
                  (condition-case nil (funcall old-d2t date)
                    (error (funcall gnu date))))))))))
;; `defvar-local' (and `make-variable-buffer-local') is a no-op on this runtime:
;; `(setq V X)' in another buffer writes the global value instead of creating a
;; buffer-local one (probe: `(defvar-local zz nil) (with-temp-buffer (setq zz 5)
;; (local-variable-p (quote zz)))' gives nil; `make-local-variable' works).
;; url-http.el does `(let ((url-current-object url)) ... (with-current-buffer
;; buffer (setq url-current-object url)))' and reads it from the filter in the
;; retrieval buffer, so the setq is lost when the let exits.  Give every buffer
;; created by `generate-new-buffer' a real local binding of url-vars.el's three
;; `defvar-local' variables, which is what the automatic-local setq would do.
(defvar anvil-standalone--auto-local-vars
  '(url-current-object url-current-mime-headers url-current-lastloc)
  "Variables declared with `defvar-local' that need a local value per buffer.")
(unless (fboundp 'anvil-standalone--generate-new-buffer-orig)
  (defalias 'anvil-standalone--generate-new-buffer-orig
    (symbol-function 'generate-new-buffer))
  (defun generate-new-buffer (name &optional inhibit-buffer-hooks)
    "Like `generate-new-buffer', but give the buffer the auto-local variables."
    (let ((buf (anvil-standalone--generate-new-buffer-orig
                name inhibit-buffer-hooks)))
      (with-current-buffer buf
        (dolist (v anvil-standalone--auto-local-vars)
          (when (boundp v)
            (set (nelisp-buffer-local-make v) (symbol-value v)))))
      buf)))
;; `url-do-setup' parses /etc/mailcap and /etc/mime.types (50-80 KB).  The
;; runtime's buffer access costs O(position) per char-addressed operation
;; (about 3-30 ms per `goto-char'/`skip-chars-forward' at offset 20000), so that
;; parse takes minutes and `url-retrieve-synchronously' looks hung.  HTTP
;; fetching does not use viewer or extension tables; pre-mark both parsed
;; from nonexistent files so only the built-in `mailcap-mime-data' defaults
;; remain.
(when (and (featurep 'mailcap) (fboundp 'mailcap-parse-mailcaps))
  (condition-case err
      (progn
        (mailcap-parse-mailcaps '(("/nonexistent/mailcap" system)) t)
        (mailcap-parse-mimetypes '("/nonexistent/mime.types") t))
    (error (push (cons 'mailcap-presetup err) anvil-standalone--port17-errors))))
;; `url-cookie-setup-save-timer' arms a periodic `run-at-time'; the vendored
;; timer.el then runs `(setf (timer--time timer) ...)' on a cl-defstruct
;; accessor, which this runtime reports as "setf: unsupported place".  A
;; headless MCP fetcher must not write the cookie jar to disk anyway.
(when (boundp 'url-cookie-save-interval)
  (setq url-cookie-save-interval nil))
;; Headless: no progress messages (url-display-message reads the C-level
;; `byte-count-to-string-function', which standalone lacks).
(when (boundp 'url-show-status)
  (setq url-show-status nil))
;; GNU files.el defvar; url-http.el evaluates it as an argument before
;; `url-display-message' can decline to print.
(unless (boundp 'byte-count-to-string-function)
  (defvar byte-count-to-string-function
    (if (fboundp 'file-size-human-readable)
        #'file-size-human-readable
      (lambda (n) (format "%s" n)))
    "Function that turns a byte count into a string."))
;; url-http calls (gnutls-available-p) while picking a connection type; this
;; runtime has no gnutls, and GNU answers nil without TLS support.
(unless (fboundp 'gnutls-available-p)
  (defun gnutls-available-p () nil))
;; Likewise no libxml: GNU built without it answers nil (dom/shr callers and
;; anvil-claude-watchdog probe it once the url stack is loaded).
(unless (fboundp 'libxml-available-p)
  (defun libxml-available-p () nil))
;; Second runtime gap: with a dynamic `let' binding of V active, a `setq' of V
;; made inside `with-current-buffer' on a buffer holding a local V writes the
;; let-bound value, not the local one (probe in u5.el: local-variable-p t, but
;; after the let the buffer's V is still nil).  `url-http' does exactly that
;; for `url-current-object', so set the local once `url-http' has returned,
;; outside the let; the filter runs later and reads the local.
(when (and (featurep 'url-http) (not (fboundp 'anvil-standalone--url-http-orig)))
  (defalias 'anvil-standalone--url-http-orig (symbol-function 'url-http))
  (defun url-http (url callback cbargs &optional retry-buffer gateway-method)
    "Like `url-http', but make `url-current-object' local in the result."
    (let ((buf (anvil-standalone--url-http-orig
                url callback cbargs retry-buffer gateway-method)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (set (nelisp-buffer-local-make 'url-current-object) url)))
      buf)))
;; Third runtime gap: url-queue's concurrent url-http jobs share the url-http
;; state (`url-http-target-url' and the response buffer are not per-job), so two
;; queued fetches both answered with the last job's final URL and the first
;; callback's slot stayed empty.  `anvil-http--request-async' is the documented
;; seam (its docstring: tests stub it to call CALLBACK synchronously); serve it
;; with one synchronous fetch per URL, which `http-fetch' already proves works.
(when (and (featurep 'anvil-http) (fboundp 'anvil-http--request))
  (defun anvil-http--request-async (url extra-headers timeout callback)
    "Fetch URL synchronously and call CALLBACK with the response plist.
CALLBACK gets (:status :headers :body :final-url) or (:error STRING)."
    (funcall callback
             (condition-case err
                 (anvil-http--request "GET" url extra-headers
                                      (or timeout anvil-http-timeout-sec))
               (error (list :error (format "%S" (cdr err))))))))
(unless (or (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
            (not (featurep 'anvil-http)))
  (condition-case err
      (progn
        (anvil-http--register-tools)
        (when (featurep 'anvil-web) (anvil-web--register-tools))
        ;; Port 9 loaded anvil-semantic and unregistered the embedding tool.
        (when (featurep 'anvil-semantic) (anvil-semantic--register-tools))
        ;; The three URI/HTTP-cache disclosure tools.
        (anvil-server-register-tools
         "emacs-eval"
         (seq-filter (lambda (spec)
                       (member (plist-get (cdr spec) :id)
                               '("anvil-uri-fetch" "http-cache-index"
                                 "http-cache-get")))
                     anvil-disclosure--tool-specs))
        (dolist (id anvil-standalone--port17-unregister)
          (anvil-server-unregister-tool id "emacs-eval")))
    (error (push (cons 'register err) anvil-standalone--port17-errors))))

;; Port 18: anvil-buffer (S13.5): buffer-read, buffer-save, buffer-list-modified.
;; Tools are screened by real call against host `emacs --batch -Q' on a temp
;; fixture file (see S13.5 in tools/ai/anvil-standalone-coverage-progress.org).
;; The runtime has no visited-file modtime primitives, which anvil-disk's
;; divergence classifier and `anvil-buffer-save' use.  The shims below are
;; fboundp-guarded and keep the modtime per buffer in a side table:
;;  * `set-visited-file-modtime' records the file's current mtime (or TIME);
;;    `visited-file-modtime' returns it, or 0 when none is recorded (GNU);
;;    `verify-visited-file-modtime' compares it with the file's mtime.
;;  * `position-bytes' is the GNU 1-based byte position of a character
;;    position (the module only uses the difference of two).
;;  * The Port 15 `find-file-noselect' shim sets `buffer-file-name' globally
;;    (no buffer-local binding) and records no modtime; it is replaced once by a
;;    version that localises the variable, records the modtime, leaves the
;;    buffer unmodified and reuses a buffer already visiting the file.
(defvar anvil-standalone--port18-errors nil
  "Alist of (MODULE . ERROR) for port-18 modules that failed to load.")
(defvar anvil-standalone--port18-unregister nil
  "Port-18 tool ids that stay unregistered, with the first blocker.
None: buffer-read, buffer-save and buffer-list-modified give the host answers
on the fixture.")
(defvar anvil-standalone--visited-modtimes (make-hash-table :test 'eq)
  "Buffer -> recorded visited-file modification time.")
;;  * `buffer-file-name' is a stub here (the variable is set, the function always
;;    returns nil and rejects a BUFFER argument) and `find-buffer-visiting' is a
;;    nil stub; both are replaced once by definitions that read the variable.
(unless (boundp 'anvil-standalone--port18-visiting)
  (defvar anvil-standalone--port18-visiting t)
  (defun buffer-file-name (&optional buffer)
    "Return the file visited by BUFFER (default: current buffer), or nil."
    (condition-case nil
        (with-current-buffer (or buffer (current-buffer)) buffer-file-name)
      (error nil)))
  (defun find-buffer-visiting (file &optional _predicate)
    "Return a live buffer visiting FILE, or nil."
    (let ((abs (expand-file-name file)) found)
      (dolist (b (buffer-list))
        (when (and (not found) (buffer-live-p b)
                   (equal (buffer-file-name b) abs))
          (setq found b)))
      found)))
(unless (fboundp 'set-visited-file-modtime)
  (defun set-visited-file-modtime (&optional time)
    "Record TIME, or the mtime of the visited file, for the current buffer."
    (puthash (current-buffer)
             (or time
                 (and buffer-file-name
                      (file-attribute-modification-time
                       (file-attributes buffer-file-name)))
                 0)
             anvil-standalone--visited-modtimes)
    nil))
(unless (fboundp 'visited-file-modtime)
  (defun visited-file-modtime ()
    "Return the recorded visited-file mtime of the current buffer, or 0."
    (gethash (current-buffer) anvil-standalone--visited-modtimes 0)))
(unless (fboundp 'verify-visited-file-modtime)
  (defun verify-visited-file-modtime (&optional buf)
    "Return non-nil when the visited file's mtime equals the recorded one."
    (with-current-buffer (or buf (current-buffer))
      (let ((rec (gethash (current-buffer) anvil-standalone--visited-modtimes 0))
            (attrs (and buffer-file-name (file-attributes buffer-file-name))))
        (or (null buffer-file-name)
            (and (numberp rec) (zerop rec))
            (and attrs
                 (equal rec (file-attribute-modification-time attrs))))))))
(unless (fboundp 'position-bytes)
  (defun position-bytes (pos)
    "Return the 1-based byte position of character position POS."
    (1+ (string-bytes (buffer-substring-no-properties (point-min) pos)))))
(unless (get 'find-file-noselect 'anvil-standalone-port18)
  (put 'find-file-noselect 'anvil-standalone-port18 t)
  (defun find-file-noselect (file &rest _)
    "Return a buffer visiting FILE, reusing one that already visits it."
    (let ((abs (expand-file-name file)))
      (or (find-buffer-visiting abs)
          (let ((buf (generate-new-buffer (file-name-nondirectory abs))))
            (with-current-buffer buf
              (when (file-exists-p abs) (insert-file-contents abs))
              ;; setq on an unlocalised variable writes the global value here.
              (if (fboundp 'nelisp-buffer-local-make)
                  (set (nelisp-buffer-local-make 'buffer-file-name) abs)
                (make-local-variable 'buffer-file-name)
                (setq buffer-file-name abs))
              (set-buffer-modified-p nil)
              (set-visited-file-modtime))
            buf)))))
(unless (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
  (condition-case err
      (when (require 'anvil-buffer nil t)
        (anvil-buffer-enable)
        (dolist (id anvil-standalone--port18-unregister)
          (anvil-server-unregister-tool id "emacs-eval")))
    (error (push (cons 'anvil-buffer err) anvil-standalone--port18-errors))))

;; Port 19: anvil-git (S14.3): git-repo-root, git-head-sha, git-branch-current,
;; git-log, git-diff-names, git-diff-stats, git-status, git-worktree-list
;; (anvil-git.el) and git-commit-message, git-pr-body (anvil-git-msg.el).
;; Every git call goes through `anvil-shell' -> `make-process' with
;; `default-directory' bound to the repo.  NeLisp `make-process' and
;; `call-process' ignore `default-directory' (S3.1 root cause), so the child
;; inherits the server cwd.  The runtime is not changed; instead both
;; primitives are wrapped here.  When a process is started under a
;; `default-directory' that differs from the child's inherited cwd, the command
;; is rewritten to
;;   /bin/sh -c 'cd -- "$1" && shift && exec "$@"' sh DIR PROG ARGS...
;; so no argument is ever spliced into shell text.  A load-time probe runs
;; `pwd' under a bound `default-directory'; if the runtime honours it the
;; wrappers are inert.  The `call-process' wrapper composes with Port 17's
;; (nil DESTINATION -> /dev/null) by capturing whatever is defined now.
(defvar anvil-standalone--port19-errors nil
  "Alist of (MODULE . ERROR) for port-19 modules that failed to load.")
(defvar anvil-standalone--port19-unregister nil
  "Port-19 tool ids that stay unregistered, with the first blocker.
None: the ten git tools give the host answers on the fixture.")
(defconst anvil-standalone--cd-script "cd -- \"$1\" && shift && exec \"$@\""
  "Shell text that enters directory $1 and execs the remaining arguments.")
(defvar anvil-standalone--process-cwd-ignored 'unknown
  "Non-nil when this runtime ignores `default-directory' for child processes.")
(defun anvil-standalone--cwd-ignored-p ()
  "Probe once whether `call-process' ignores a bound `default-directory'."
  (when (eq anvil-standalone--process-cwd-ignored 'unknown)
    (setq anvil-standalone--process-cwd-ignored
          (condition-case nil
              (let ((default-directory "/"))
                (with-temp-buffer
                  (apply (if (fboundp 'anvil-standalone--call-process-orig)
                             #'anvil-standalone--call-process-orig
                           #'call-process)
                         "/bin/sh" nil t nil '("-c" "pwd"))
                  (not (equal (string-trim (buffer-string)) "/"))))
            (error nil))))
  anvil-standalone--process-cwd-ignored)
(defun anvil-standalone--cd-dir ()
  "Return `default-directory' as a plain directory to cd into, or nil."
  (and (stringp default-directory)
       (not (file-remote-p default-directory))
       (file-directory-p default-directory)
       (anvil-standalone--cwd-ignored-p)
       (directory-file-name (expand-file-name default-directory))))
(unless (get 'call-process 'anvil-standalone-port19)
  (let ((prev (symbol-function 'call-process)))
    (defalias 'anvil-standalone--call-process-port17 prev)
    (defun call-process (program &optional infile destination display &rest args)
      "Like `call-process', but honours `default-directory' by cd-ing first."
      (let ((dir (anvil-standalone--cd-dir)))
        (if dir
            (apply #'anvil-standalone--call-process-port17
                   "/bin/sh" infile destination display
                   "-c" anvil-standalone--cd-script "sh" dir program args)
          (apply #'anvil-standalone--call-process-port17
                 program infile destination display args))))
    (put 'call-process 'anvil-standalone-port19 t)))
(unless (get 'make-process 'anvil-standalone-port19)
  (defalias 'anvil-standalone--make-process-orig (symbol-function 'make-process))
  (defun make-process (&rest args)
    "Like `make-process', but honours `default-directory' by cd-ing first."
    (let ((dir (anvil-standalone--cd-dir))
          (command (plist-get args :command)))
      (when (and dir (consp command))
        (setq args (plist-put (copy-sequence args) :command
                              (append (list "/bin/sh" "-c"
                                            anvil-standalone--cd-script
                                            "sh" dir)
                                      command))))
      (apply #'anvil-standalone--make-process-orig args)))
  (put 'make-process 'anvil-standalone-port19 t))
(unless (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
  (condition-case err
      (when (require 'anvil-git nil t)
        (anvil-git-enable)
        (when (require 'anvil-git-msg nil t)
          (anvil-git-msg-enable))
        (dolist (id anvil-standalone--port19-unregister)
          (anvil-server-unregister-tool id "emacs-eval")))
    (error (push (cons 'anvil-git err) anvil-standalone--port19-errors))))

;; Port 20: anvil-elisp (S14.4): elisp-describe-function, -describe-variable,
;; -get-function-definition, -read-source-file, -ert-run and ert-run-distilled.
;; The runtime keeps a closure's docstring in slot 4 but `documentation' does
;; not return it, drops the docstring of `defvar', records no per-symbol
;; entries in `load-history', and has no `documentation-property' or
;; `find-lisp-object-file-name'.  Guarded shims (defined only when the
;; function is missing or demonstrably wrong) restore them:
;;  * `documentation' falls back to the closure's slot-4 docstring.
;;  * `find-lisp-object-file-name' scans the `load-history' files, newest
;;    first, for a top-level definition form of the symbol (what GNU's
;;    per-symbol load-history entries record).
;;  * `documentation-property' returns the property, or the docstring read
;;    from that source form for `variable-documentation'.
;; S14.6: elisp-ert-run runs the genuine vendored GNU ert.el in-process (loaded
;; on first use, see `anvil-standalone--ensure-gnu-ert'); bench-profile-expr
;; is served by Port 13.  elisp-byte-compile-file and ert-run-distilled run a
;; fresh host GNU Emacs batch process.
(defvar anvil-standalone--port20-errors nil
  "Alist of (MODULE . ERROR) for port-20 modules that failed to load.")
(defvar anvil-standalone--port20-unregister nil
  "Port-20 tool ids that stay unregistered.  None: every anvil-elisp tool is served.")
(defconst anvil-standalone--source-defun-heads
  '("defun" "defmacro" "defsubst" "cl-defun" "cl-defmacro" "cl-defsubst"
    "define-minor-mode" "define-derived-mode" "defalias" "ert-deftest")
  "Heads of top-level forms that define a function.")
(defconst anvil-standalone--source-defvar-heads
  '("defvar" "defconst" "defcustom" "defvar-local" "defface")
  "Heads of top-level forms that define a variable.")
(defun anvil-standalone--source-form-in-file (file sym heads)
  "Return (POSITION . FORM) of the definition of SYM among HEADS in FILE."
  (when (and (stringp file) (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((re (concat "^(\\(?:" (mapconcat #'regexp-quote heads "\\|")
                        "\\)[ \t\n]+'?" (regexp-quote (symbol-name sym))
                        "[ \t\n)]"))
            (found nil))
        (while (and (not found) (re-search-forward re nil t))
          (let ((start (match-beginning 0)))
            (goto-char start)
            (condition-case nil
                (setq found (cons start (read (current-buffer))))
              (error (goto-char (1+ start))))))
        found))))
(defun anvil-standalone--source-definition (sym heads)
  "Return (FILE . FORM) for the newest loaded file defining SYM among HEADS."
  (let ((result nil))
    (dolist (entry load-history)
      (let ((file (and (consp entry) (stringp (car entry)) (car entry))))
        (when (and file (not result))
          (when (string-suffix-p ".elc" file)
            (setq file (substring file 0 -1)))
          (let ((hit (anvil-standalone--source-form-in-file file sym heads)))
            (when hit (setq result (cons file (cdr hit))))))))
    result))
(unless (get 'documentation 'anvil-standalone-port20)
  (when (fboundp 'documentation)
    (defalias 'anvil-standalone--documentation-orig
      (symbol-function 'documentation))
    (defun documentation (function &optional raw)
      "Like `documentation', but also reads a closure's slot-4 docstring."
      (or (anvil-standalone--documentation-orig function raw)
          (let* ((f (condition-case nil
                        (if (symbolp function)
                            (and (fboundp function) (indirect-function function))
                          function)
                      (error nil)))
                 (f (if (eq (car-safe f) 'macro) (cdr f) f))
                 (doc (and f (not (symbolp f))
                           (condition-case nil
                               (and (> (length f) 4) (aref f 4))
                             (error nil)))))
            (when (stringp doc)
              (if (and (not raw) (fboundp 'substitute-command-keys))
                  (substitute-command-keys doc)
                doc)))))
    (put 'documentation 'anvil-standalone-port20 t)))
(unless (fboundp 'find-lisp-object-file-name)
  (defun find-lisp-object-file-name (object type)
    "Return the loaded source file that defines OBJECT as TYPE, or nil."
    (and (symbolp object)
         (car (anvil-standalone--source-definition
               object (if (eq type 'defvar)
                          anvil-standalone--source-defvar-heads
                        anvil-standalone--source-defun-heads))))))
(unless (fboundp 'documentation-property)
  (defun documentation-property (symbol prop &optional raw)
    "Return the PROP documentation of SYMBOL, reading the source if needed."
    (let ((val (get symbol prop)))
      (when (and (null val) (eq prop 'variable-documentation))
        (let ((form (cdr (anvil-standalone--source-definition
                          symbol anvil-standalone--source-defvar-heads))))
          (when (stringp (nth 3 form)) (setq val (nth 3 form)))))
      (if (and (stringp val) (not raw) (fboundp 'substitute-command-keys))
          (substitute-command-keys val)
        val))))
;; find-func.el is not part of the vendored GNU lisp; anvil-elisp needs only the
;; (BUFFER . POINT) of the definition inside the file it already inserted.
(unless (fboundp 'find-function-search-for-symbol)
  (defun find-function-search-for-symbol (symbol type file)
    "Return (BUFFER . POINT) of SYMBOL's definition of TYPE in FILE, or nil."
    (let ((hit (anvil-standalone--source-form-in-file
                file symbol (if (eq type 'defvar)
                                anvil-standalone--source-defvar-heads
                              anvil-standalone--source-defun-heads))))
      (and hit (cons (current-buffer) (car hit))))))
;; ert-run-distilled starts `invocation-name --batch -Q', which is the NeLisp
;; binary here and cannot run GNU ERT.  The digest is defined over a fresh GNU
;; Emacs batch process, so the command builder substitutes the host `emacs'
;; for the first word (the tool handler itself keeps its MCP arglist).
(defun anvil-standalone--wrap-ert-distilled ()
  "Wrap `anvil-elisp--ert-distilled-command' once to run the host Emacs."
  (unless (get 'anvil-elisp--ert-distilled-command 'anvil-standalone-port20)
    (let ((orig (symbol-function 'anvil-elisp--ert-distilled-command)))
      (fset 'anvil-elisp--ert-distilled-command
            (lambda (files load-path-dirs selector)
              (let ((command (funcall orig files load-path-dirs selector))
                    (host (executable-find "emacs")))
                (if host (cons host (cdr command)) command)))))
    (put 'anvil-elisp--ert-distilled-command 'anvil-standalone-port20 t)))
;; elisp-byte-compile-file: standalone has no byte compiler, so the host
;; handler runs unchanged in a fresh GNU Emacs batch process (the same way
;; ert-run-distilled uses one) and its printed plist is the answer.  The .elc
;; is written next to the source, exactly as on the host.  The tool is
;; re-registered with the original description because the original handler is
;; an offload tool whose worker does not load anvil-elisp.
(defun anvil-standalone--elisp-byte-compile-file (file)
  "Byte-compile FILE in a host Emacs batch process and return its plist.

MCP Parameters:
  file - Path to an .el file to byte-compile (string)."
  (let* ((host (or (executable-find "emacs")
                   (error "No host emacs on PATH")))
         (dir (file-name-directory (locate-library "anvil-elisp")))
         (form (format "(progn (require 'anvil-elisp) (princ (anvil-elisp--byte-compile-file %S)))"
                       (expand-file-name file))))
    (with-temp-buffer
      (let ((rc (call-process "/bin/sh" nil t nil "-c"
                              "exec \"$@\" 2>/dev/null" "sh"
                              host "--batch" "-Q" "-L" dir "--eval" form)))
        (unless (eq rc 0)
          (error "Host byte-compile process failed (exit %S)" rc))
        (string-trim (buffer-string))))))
(defun anvil-standalone--reregister-byte-compile ()
  "Replace the offload `elisp-byte-compile-file' tool with the host delegate."
  (let* ((tools (gethash "emacs-eval" anvil-server--tools))
         (old (gethash "elisp-byte-compile-file" tools)))
    (when old
      (anvil-server-unregister-tool "elisp-byte-compile-file" "emacs-eval")
      (anvil-server-register-tool
       #'anvil-standalone--elisp-byte-compile-file
       :id "elisp-byte-compile-file"
       :intent '(elisp-build)
       :layer 'dev
       :server-id "emacs-eval"
       :description (plist-get old :description)
       :read-only nil))))
;; elisp-ert-run (S14.6): the runtime ships the reduced nelisp-emacs-lib ert
;; (no `ert-run-test'), so the tool would report 0 tests.  The genuine vendored
;; GNU ert.el is loaded on first use instead (never at loader start: a vendor
;; read at load time must stay out of the startup path).  It loads once these
;; small stand-ins exist: `source-directory' (debug.el, find-func.el), an empty
;; `prog-mode-map', the
;; font-lock keyword variables debug.el/backtrace.el only mention, and
;; `messages-buffer' and `message-log-max'.  The runtime's `buffer-substring' rejects a marker
;; argument, which ert.el uses to cut the messages log, so it is wrapped to
;; accept markers.  The tests then run in this process like on the host.
(defun anvil-standalone--ensure-gnu-ert ()
  "Load the vendored GNU ert.el once, replacing the reduced runtime ert."
  (unless (fboundp 'ert-run-test)
    (unless (boundp 'source-directory)
      (defvar source-directory "/usr/share/emacs/"))
    (dolist (var '(lisp-el-font-lock-keywords-for-backtraces
                   lisp-el-font-lock-keywords-for-backtraces-1
                   lisp-el-font-lock-keywords-for-backtraces-2))
      (unless (boundp var) (set var nil)))
    (unless (boundp 'message-log-max)
      (defvar message-log-max 1000))
    (unless (fboundp 'messages-buffer)
      (defun messages-buffer ()
        "Return the *Messages* buffer, creating it if needed."
        (get-buffer-create "*Messages*")))
    (unless (get 'buffer-substring 'anvil-standalone-port20)
      (let ((probe (condition-case nil
                       (with-temp-buffer
                         (insert "ab")
                         (buffer-substring (copy-marker 1) (point-max))
                         t)
                     (error nil))))
        (unless probe
          (defalias 'anvil-standalone--buffer-substring-orig
            (symbol-function 'buffer-substring))
          (defun buffer-substring (start end)
            "Like `buffer-substring', but START and END may be markers."
            (anvil-standalone--buffer-substring-orig
             (if (markerp start) (marker-position start) start)
             (if (markerp end) (marker-position end) end)))))
      (put 'buffer-substring 'anvil-standalone-port20 t))
    (let ((dir (let ((bt (locate-library "backtrace")))
                 (and bt (file-name-directory bt)))))
      (unless (and dir (file-readable-p (expand-file-name "ert.el" dir)))
        (error "Vendored GNU ert.el not found"))
      (setq features (delq 'ert features))
      ;; DIR goes last so the runtime's own lisp-mode etc. win over the
      ;; vendored ones that backtrace.el requires.
      (let ((load-path (append load-path (list dir))))
        (unless (boundp 'prog-mode-map)
          (defvar prog-mode-map (make-sparse-keymap)))
        (dolist (lib '("debug" "backtrace" "find-func"))
          (load (expand-file-name (concat lib ".el") dir) nil t))
        (load (expand-file-name "ert.el" dir) nil t)))
    (unless (fboundp 'ert-run-test)
      (error "GNU ert.el did not load"))))
(defun anvil-standalone--wrap-ert-run ()
  "Make `anvil-elisp--ert-run' load GNU ert first, keeping its MCP signature."
  (unless (get 'anvil-elisp--ert-run 'anvil-standalone-port20)
    (let ((orig (symbol-function 'anvil-elisp--ert-run))
          (doc (documentation 'anvil-elisp--ert-run t)))
      (defalias 'anvil-standalone--ert-run-orig orig)
      (eval `(defun anvil-elisp--ert-run (file &optional selector fresh)
               ,doc
               (anvil-standalone--ensure-gnu-ert)
               (anvil-standalone--ert-run-orig file selector fresh))
            t))
    (put 'anvil-elisp--ert-run 'anvil-standalone-port20 t)))
;; GNU facilities anvil-elisp reads: the custom-variable test, the package
;; directory list and the (already active, in batch) auto-compression mode.
(unless (fboundp 'custom-variable-p)
  (defun custom-variable-p (variable)
    "Return non-nil if VARIABLE is a customizable variable (GNU wording)."
    (and (symbolp variable)
         (or (get variable 'standard-value)
             (get variable 'custom-autoload)))))
(unless (boundp 'package-directory-list)
  (defvar package-directory-list nil
    "Directories of system-wide packages; none on standalone."))
(unless (boundp 'auto-compression-mode)
  (defvar auto-compression-mode t
    "Non-nil: compressed files are read transparently, as in `emacs --batch'.")
  (unless (fboundp 'auto-compression-mode)
    (defun auto-compression-mode (&optional _arg)
      "Stand-in: compressed-file reading is always treated as active." nil)))
(unless (or (getenv "ANVIL_STANDALONE_CONTEXT_WORKER")
            (not (anvil-standalone--elib-available-p)))
  (condition-case err
      (when (require 'anvil-elisp nil t)
        (unless anvil-elisp--system-lisp-dir
          (let ((subr (locate-library "subr")))
            (when subr
              (setq anvil-elisp--system-lisp-dir (file-name-directory subr)))))
        (anvil-standalone--wrap-ert-distilled)
        (anvil-standalone--wrap-ert-run)
        (anvil-elisp-enable)
        (anvil-standalone--reregister-byte-compile)
        (dolist (id anvil-standalone--port20-unregister)
          (anvil-server-unregister-tool id "emacs-eval")))
    (error (push (cons 'anvil-elisp err) anvil-standalone--port20-errors))))

;; Port 21: signalled processes (S14.5): orchestrator-cancel and
;; orchestrator-stop.  The orchestrator kills a running job with
;; `signal-process' (SIGTERM, then SIGKILL) and records `process-exit-status'
;; as the task exit-code.  This runtime has no `signal-process' (the call is
;; wrapped in `ignore-errors' by the module, so nothing was signalled and the
;; task stayed running), no `interrupt-process', and `kill-process' makes the
;; child die with a shell-style 128+N exit: `process-status' says `exit' and
;; `process-exit-status' 143 where GNU says `signal' and 15 (9 for
;; `kill-process' / `delete-process', 2 for `interrupt-process').  The runtime
;; is not changed.  A load-time probe signals a real child with the native
;; `signal-process'; when the runtime already answers `signal' and the signal
;; number, nothing below is installed.  Otherwise:
;;  * `signal-process' sends the signal with the kill(1) command and records
;;    the signal number on the process (`anvil-sent-signal' property);
;;    `kill-process', `delete-process' and `interrupt-process' record 9, 9 and 2
;;    the same way (GNU sends SIGKILL, SIGKILL and SIGINT for them here);
;;  * `process-status' returns `signal' and `process-exit-status' the signal
;;    number when the recorded signal matches the runtime's exit report
;;    (128+N, or N).  A child that handled the signal and exited with another
;;    code keeps the runtime's answer.
;; Limitation: the sentinel event string stays the runtime's ("terminated\n"
;; for every signal), GNU reports "killed\n" for 9, "interrupt\n" for 2 etc.
;; The `sh -c ... exec' prefix of Port 19 keeps the child's PID, so the signal
;; reaches the real program.
(defvar anvil-standalone--signal-numbers
  '((SIGHUP . 1) (SIGINT . 2) (SIGQUIT . 3) (SIGKILL . 9) (SIGUSR1 . 10)
    (SIGUSR2 . 12) (SIGTERM . 15) (SIGCONT . 18) (SIGSTOP . 19))
  "Signal names `signal-process' accepts here, with their Linux numbers.")
(defvar anvil-standalone--signals-native 'unknown
  "Non-nil when the runtime reports signalled processes as GNU does.")
(defun anvil-standalone--signal-number (sig)
  "Return the signal number for SIG (integer, symbol or string), or nil."
  (cond ((integerp sig) sig)
        (t (let ((name (upcase (format "%s" sig))))
             (unless (string-prefix-p "SIG" name)
               (setq name (concat "SIG" name)))
             (cdr (assq (intern name) anvil-standalone--signal-numbers))))))
(defun anvil-standalone--process-pid (process)
  "Return the OS pid of PROCESS, or nil."
  (or (and (integerp process) process)
      (let ((pid (ignore-errors (process-id process))))
        (and (integerp pid) pid))
      (and (vectorp process) (> (length process) 1)
           (integerp (aref process 1)) (aref process 1))))
(defun anvil-standalone--signals-native-p ()
  "Probe once whether the runtime reports a signalled child as GNU does."
  (when (eq anvil-standalone--signals-native 'unknown)
    (setq anvil-standalone--signals-native
          (condition-case nil
              (and (fboundp 'signal-process)
                   (let ((p (make-process :name "anvil-sig-probe"
                                          :command '("sleep" "30")
                                          :connection-type 'pipe))
                         (n 0))
                     (signal-process p 'SIGTERM)
                     (while (and (< n 30) (process-live-p p))
                       (accept-process-output nil 0.1)
                       (setq n (1+ n)))
                     (prog1 (and (eq (process-status p) 'signal)
                                 (eql (process-exit-status p) 15))
                       (ignore-errors (delete-process p)))))
            (error nil))))
  anvil-standalone--signals-native)
(unless (or (get 'process-status 'anvil-standalone-port21)
            (anvil-standalone--signals-native-p))
  (defalias 'anvil-standalone--process-status-orig
    (symbol-function 'process-status))
  (defalias 'anvil-standalone--process-exit-status-orig
    (symbol-function 'process-exit-status))
  (defalias 'anvil-standalone--delete-process-orig
    (symbol-function 'delete-process))
  (defun anvil-standalone--send-signal (process sig)
    "Send SIG to PROCESS with kill(1) and record it.  Return 0 or -1."
    (let ((num (anvil-standalone--signal-number sig))
          (pid (anvil-standalone--process-pid process)))
      (cond
       ((not (and num pid)) -1)
       (t (when (and (not (integerp process)) (memq num '(1 2 3 9 15)))
            (ignore-errors (process-put process 'anvil-sent-signal num)))
          (if (eql 0 (call-process "kill" nil nil nil
                                   (format "-%d" num) (number-to-string pid)))
              0
            -1)))))
  (defun signal-process (process sig &optional _remote)
    "Send signal SIG to PROCESS (a process or pid) through kill(1)."
    (anvil-standalone--send-signal process sig))
  (defun interrupt-process (&optional process _current-group)
    "Send SIGINT to PROCESS."
    (when (processp process) (anvil-standalone--send-signal process 'SIGINT))
    process)
  (defun kill-process (&optional process _current-group)
    "Send SIGKILL to PROCESS."
    (when (and process (not (integerp process)) (process-live-p process))
      (anvil-standalone--send-signal process 'SIGKILL))
    process)
  (defun delete-process (&optional process)
    "Send SIGKILL to a live PROCESS, then delete it."
    (when (and process (processp process) (process-live-p process))
      (anvil-standalone--send-signal process 'SIGKILL))
    (anvil-standalone--delete-process-orig process))
  (defun anvil-standalone--signalled-p (process)
    "Return the recorded signal of PROCESS when the runtime reported it as exit."
    (let ((sig (and (processp process) (ignore-errors (process-get process 'anvil-sent-signal))))
          (code (and (processp process)
                     (eq (anvil-standalone--process-status-orig process) 'exit)
                     (anvil-standalone--process-exit-status-orig process))))
      (and sig (memql code (list sig (+ 128 sig))) sig)))
  (defun process-status (process)
    "Like `process-status', reporting `signal' for a process killed by a signal."
    (if (anvil-standalone--signalled-p process)
        'signal
      (anvil-standalone--process-status-orig process)))
  (defun process-exit-status (process)
    "Like `process-exit-status', returning the signal number for `signal'."
    (or (anvil-standalone--signalled-p process)
        (anvil-standalone--process-exit-status-orig process)))
  (put 'process-status 'anvil-standalone-port21 t))
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
