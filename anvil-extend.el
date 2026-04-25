;;; anvil-extend.el --- Claude self-extension SDK scaffold (Phase A) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton
;; Keywords: comm, tools, ai, mcp, codegen
;; Package-Requires: ((emacs "28.2"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; anvil-extend implements Phase A of the Claude self-extension SDK
;; sketched in `docs/design/35-claude-self-extension-sdk.org' (DRAFT
;; v0).  Phase A is intentionally tiny: it only ships a scaffold
;; generator that lets a Claude Code session emit a new ad-hoc tool
;; module — elisp body + ERT skeleton + MCP register stub — into a
;; storage directory that is completely isolated from the host anvil
;; package.
;;
;; Public API (all `anvil-extend-*' prefixed for safe namespace
;; reservation while Doc 35 is still DRAFT v0; codex review may rename
;; the eventual stable surface):
;;
;;   (anvil-extend-scaffold NAME &key params body docstring)
;;     Emit `<NAME>.el', `<NAME>-test.el', `<NAME>-register.el' under
;;     `anvil-extend-storage-dir' and return a plist describing the
;;     three written paths.
;;
;;   (anvil-extend-load NAME)
;;     Byte-compile and load `<NAME>.el'.  Returns a plist with
;;     `:status :loaded' on success, `:status :failed :reason ...' on
;;     failure (no error is raised so MCP callers can introspect).
;;
;;   (anvil-extend-test NAME)
;;     Run `<NAME>-test.el' under ERT-batch and report pass/fail
;;     counts as a plist.
;;
;;   (anvil-extend-list)
;;     Enumerate every scaffolded extension currently visible under
;;     `anvil-extend-storage-dir'.
;;
;;   (anvil-extend-remove NAME)
;;     Delete the three files belonging to NAME and `unload-feature'
;;     the loaded extension if applicable.
;;
;; One MCP tool, `anvil-extend-scaffold', is registered against the
;; `emacs-eval' server so a Claude Code session can author new tools
;; from inside the conversation.
;;
;; Phase A explicitly excludes hot-reload after edit (Phase B), sandbox
;; eval (Phase C), rationale auto-record (Phase D), the NeLisp execute
;; path (Phase E), and ephemeral/permanent promotion gates (Phase F).
;; Those land in subsequent design rounds once Doc 35 reaches LOCKED.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ert)

;;;; --- customization ------------------------------------------------------

(defgroup anvil-extend nil
  "Claude self-extension SDK scaffold generator (Doc 35 Phase A)."
  :group 'anvil
  :prefix "anvil-extend-")

(defcustom anvil-extend-storage-dir
  (expand-file-name "~/.anvil-extend/")
  "Directory where scaffolded extension files are emitted.

Each successful `anvil-extend-scaffold' call writes three files to
this directory:

  - `NAME.el'          — extension body (autoloaded `defun NAME')
  - `NAME-test.el'     — ERT skeleton with one passing smoke test
  - `NAME-register.el' — call to `anvil-server-register-tool' that
                         wires the extension into the MCP surface.

The directory is created on demand.  Tests rebind this via
`let' to keep CI hermetic."
  :type 'directory
  :group 'anvil-extend)

(defcustom anvil-extend-server-id "emacs-eval"
  "MCP server-id used when registering anvil-extend's own tools."
  :type 'string
  :group 'anvil-extend)

(defcustom anvil-extend-default-pool 'ephemeral
  "Default `:pool' tag stored in the register stub.

Phase A only records this metadata; promotion between
`ephemeral' and `permanent' is a Phase F concern."
  :type '(choice (const ephemeral) (const permanent))
  :group 'anvil-extend)

;;;; --- internal helpers ---------------------------------------------------

(defconst anvil-extend--name-regexp
  "\\`[a-z][a-z0-9-]*\\'"
  "Allowed shape for an extension NAME.

Lowercase ASCII letters/digits/hyphens, must start with a
letter.  This is intentionally narrower than what Elisp accepts
because the NAME is interpolated unquoted into generated source
files and into a path on disk.")

(defun anvil-extend--check-name (name)
  "Signal `user-error' unless NAME matches `anvil-extend--name-regexp'.
NAME must be a symbol."
  (unless (symbolp name)
    (user-error "anvil-extend: NAME must be a symbol, got %S" name))
  (let ((case-fold-search nil)
        (s (symbol-name name)))
    (unless (string-match-p anvil-extend--name-regexp s)
      (user-error
       "anvil-extend: NAME %S must match %s (lowercase, hyphenated)"
       name anvil-extend--name-regexp))))

(defun anvil-extend--ensure-storage ()
  "Create `anvil-extend-storage-dir' if absent and return its expanded path."
  (let ((dir (file-name-as-directory
              (expand-file-name anvil-extend-storage-dir))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun anvil-extend--paths (name)
  "Return a plist of (`:elisp-file' `:test-file' `:register-stub') for NAME.

Files are not required to exist; this is a pure path computation."
  (let* ((dir (file-name-as-directory
               (expand-file-name anvil-extend-storage-dir)))
         (base (symbol-name name)))
    (list :elisp-file    (expand-file-name (concat base ".el")          dir)
          :test-file     (expand-file-name (concat base "-test.el")     dir)
          :register-stub (expand-file-name (concat base "-register.el") dir))))

(defun anvil-extend--write (path content)
  "Write CONTENT to PATH using utf-8-unix, no backup, silent."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert content))))

(defun anvil-extend--render-elisp (name params body docstring)
  "Build the `<NAME>.el' source string.

PARAMS is a list of symbols (the function arglist).  BODY is a
list of forms (the function body).  DOCSTRING is a string."
  (let* ((sym  name)
         (file (concat (symbol-name sym) ".el"))
         (feat (symbol-name sym))
         (header
          (format ";;; %s --- anvil-extend scaffold (Phase A) -*- lexical-binding: t; -*-

;; Generated by `anvil-extend-scaffold' on %s.
;; Pool: %s
;; Status: scaffold (Phase A) — no rationale recorded yet (Phase D).

;;; Commentary:

;; Auto-emitted extension stub.  Hand-edit at will, then
;; `anvil-extend-load' to byte-compile + reload.

;;; Code:

"
                  file
                  (format-time-string "%Y-%m-%d %H:%M:%S")
                  anvil-extend-default-pool))
         (defun-form
          (pp-to-string
           `(defun ,sym ,params
              ,docstring
              ,@body)))
         (provide-form
          (format "\n(provide '%s)\n;;; %s ends here\n" feat file)))
    (concat header defun-form provide-form)))

(defun anvil-extend--render-test (name params)
  "Build the `<NAME>-test.el' source string.

PARAMS lets the smoke test pick a sane invocation: zero-arg
extensions are called bare; otherwise the test only asserts
`fboundp' so generated code stays compilable without guessing
argument values."
  (let* ((sym  name)
         (feat (symbol-name sym))
         (file (concat feat "-test.el"))
         (smoke-call
          (if params
              ;; Don't try to invent inputs; just assert the
              ;; symbol is callable so the scaffold compiles.
              `(should (fboundp ',sym))
            `(should (or (null (,sym))
                         t))))
         (smoke-form
          (pp-to-string
           `(ert-deftest ,(intern (concat feat "-smoke")) ()
              ,(format "Smoke test generated by anvil-extend-scaffold for `%s'." sym)
              ,smoke-call))))
    (concat
     (format ";;; %s --- ERT for anvil-extend scaffold -*- lexical-binding: t; -*-

;;; Commentary:

;; Auto-emitted ERT skeleton.  Replace the smoke test with real
;; assertions before promoting to `permanent'.

;;; Code:

(require 'ert)
(require '%s)

" file feat)
     smoke-form
     (format "\n(provide '%s-test)\n;;; %s ends here\n" feat file))))

(defun anvil-extend--render-register (name docstring)
  "Build the `<NAME>-register.el' MCP wiring stub.

The stub is a separate file so the user (or a Phase F promote
flow) can decide whether to wire the extension into the live MCP
surface or keep it dormant."
  (let* ((sym  name)
         (feat (symbol-name sym))
         (file (concat feat "-register.el"))
         (tool-id (symbol-name sym))
         (form
          (pp-to-string
           `(progn
              (require 'anvil-server)
              (require ',sym)
              (anvil-server-register-tool
               #',sym
               :id ,tool-id
               :description ,(or docstring
                                 (format "Scaffolded by anvil-extend on %s."
                                         (format-time-string "%Y-%m-%d")))
               :server-id anvil-extend-server-id
               :intent '(extend scaffold)
               :layer 'experimental
               :stability 'experimental)))))
    (concat
     (format ";;; %s --- MCP register stub for anvil-extend scaffold -*- lexical-binding: t; -*-

;;; Commentary:

;; Loading this file registers `%s' as an MCP tool against the
;; configured anvil server-id.  Phase A does NOT auto-load this
;; stub; promotion to live MCP surface is opt-in.

;;; Code:

(require 'anvil-extend)

" file feat)
     form
     (format "\n(provide '%s-register)\n;;; %s ends here\n" feat file))))

(defun anvil-extend--feature-loaded-p (name)
  "Non-nil when feature corresponding to NAME is currently loaded."
  (featurep name))

;;;; --- public: scaffold ---------------------------------------------------

;;;###autoload
(cl-defun anvil-extend-scaffold (name &key params body docstring)
  "Generate the three scaffold files for extension NAME.

NAME is a symbol matching `anvil-extend--name-regexp'.  PARAMS is
a list of symbols forming the extension function's arglist (may
be nil for zero-arg extensions).  BODY is a list of forms making
up the function body; if nil, a placeholder `(progn nil)' body is
emitted so the generated file still compiles.  DOCSTRING is a
string used both for the `defun' and as the MCP tool description.

Returns a plist:

  (:elisp-file PATH
   :test-file  PATH
   :register-stub PATH
   :status     :created)"
  (anvil-extend--check-name name)
  (anvil-extend--ensure-storage)
  (let* ((paths   (anvil-extend--paths name))
         (params* (or params nil))
         (body*   (or body '(nil)))
         (doc*    (or docstring
                      (format "anvil-extend scaffold for `%s'." name))))
    (anvil-extend--write
     (plist-get paths :elisp-file)
     (anvil-extend--render-elisp name params* body* doc*))
    (anvil-extend--write
     (plist-get paths :test-file)
     (anvil-extend--render-test name params*))
    (anvil-extend--write
     (plist-get paths :register-stub)
     (anvil-extend--render-register name doc*))
    (append paths (list :status :created))))

;;;; --- public: load -------------------------------------------------------

;;;###autoload
(defun anvil-extend-load (name)
  "Byte-compile and load `<NAME>.el' from `anvil-extend-storage-dir'.

Returns a plist `(:status :loaded :name NAME)' on success.  On
failure the plist is `(:status :failed :name NAME :reason MSG)'
— failure is reported as data so MCP callers can introspect
without raising."
  (anvil-extend--check-name name)
  (let* ((paths (anvil-extend--paths name))
         (file  (plist-get paths :elisp-file)))
    (cond
     ((not (file-exists-p file))
      (list :status :failed
            :name name
            :reason (format "no scaffold file at %s" file)))
     (t
      (condition-case err
          (let* ((dir (file-name-directory file))
                 (load-path (cons dir load-path))
                 (byte-compile-warnings nil))
            (when (featurep name)
              (ignore-errors (unload-feature name t)))
            ;; Compile, but keep going if compile fails — the .el
            ;; load below is what actually gates success.
            (ignore-errors (byte-compile-file file))
            (load file nil 'nomessage)
            (list :status :loaded :name name))
        (error
         (list :status :failed
               :name name
               :reason (error-message-string err))))))))

;;;; --- public: test -------------------------------------------------------

;;;###autoload
(defun anvil-extend-test (name)
  "Run `<NAME>-test.el' from `anvil-extend-storage-dir' under ERT.

Returns a plist:

  (:status :passed | :failed | :missing
   :name NAME
   :pass-count N
   :fail-count M
   :details PLIST-OR-STRING)

When the test file is absent, returns `:status :missing' with the
expected path under `:reason'."
  (anvil-extend--check-name name)
  (let* ((paths (anvil-extend--paths name))
         (test-file (plist-get paths :test-file))
         (selector (intern (format "\\`%s-" (symbol-name name)))))
    (cond
     ((not (file-exists-p test-file))
      (list :status :missing
            :name name
            :reason (format "no test file at %s" test-file)))
     (t
      (condition-case err
          (let* ((dir (file-name-directory test-file))
                 (load-path (cons dir load-path)))
            ;; Make sure the extension under test is loaded; ignore
            ;; load failures so we can report them via the test result.
            (ignore-errors (anvil-extend-load name))
            (load test-file nil 'nomessage)
            ;; `ert-run-tests-batch' echoes a "Ran N tests..." summary
            ;; through `message', which corrupts an outer ERT batch
            ;; harness if anvil-extend-test is itself called from a
            ;; test (the outer ert-run-tests-batch-and-exit parses the
            ;; first matching summary it sees).  Silence the inner run.
            (let* ((inhibit-message t)
                   (message-log-max nil)
                   (stats (ert-run-tests-batch
                           (format "\\`%s-" (symbol-name name))))
                   (passed (ert-stats-completed-expected stats))
                   (failed (ert-stats-completed-unexpected stats))
                   (status (if (zerop failed) :passed :failed)))
              (list :status status
                    :name name
                    :pass-count passed
                    :fail-count failed
                    :details (list :selector selector
                                   :total (ert-stats-total stats)))))
        (error
         (list :status :failed
               :name name
               :pass-count 0
               :fail-count 1
               :details (error-message-string err))))))))

;;;; --- public: list -------------------------------------------------------

;;;###autoload
(defun anvil-extend-list ()
  "Enumerate every scaffolded extension visible under storage dir.

Returns a list of plists:

  (:name NAME
   :loaded BOOL
   :tested BOOL
   :pool ephemeral|permanent
   :elisp-file PATH
   :test-file PATH
   :register-stub PATH)

`:tested' is t when the corresponding `<NAME>-test.el' exists on
disk; the scaffold always emits one so this is a structural
check, not a result of running the tests."
  (let* ((dir (file-name-as-directory
               (expand-file-name anvil-extend-storage-dir))))
    (if (not (file-directory-p dir))
        nil
      (let* ((files (directory-files dir nil "\\.el\\'" t))
             (names (cl-remove-duplicates
                     (delq nil
                           (mapcar
                            (lambda (f)
                              (let ((b (file-name-sans-extension f)))
                                (cond
                                 ((string-suffix-p "-test" b) nil)
                                 ((string-suffix-p "-register" b) nil)
                                 (t b))))
                            files))
                     :test #'string=)))
        (mapcar
         (lambda (base)
           (let* ((sym (intern base))
                  (paths (anvil-extend--paths sym)))
             (list :name sym
                   :loaded (anvil-extend--feature-loaded-p sym)
                   :tested (file-exists-p (plist-get paths :test-file))
                   :pool anvil-extend-default-pool
                   :elisp-file (plist-get paths :elisp-file)
                   :test-file (plist-get paths :test-file)
                   :register-stub (plist-get paths :register-stub))))
         (sort names #'string<))))))

;;;; --- public: remove -----------------------------------------------------

;;;###autoload
(defun anvil-extend-remove (name)
  "Delete every file belonging to extension NAME and unload it.

Returns a plist `(:status :removed :name NAME :deleted N)'."
  (anvil-extend--check-name name)
  (let* ((paths (anvil-extend--paths name))
         (deleted 0))
    (when (featurep name)
      (ignore-errors (unload-feature name t)))
    (when (featurep (intern (concat (symbol-name name) "-test")))
      (ignore-errors (unload-feature
                      (intern (concat (symbol-name name) "-test")) t)))
    (dolist (k '(:elisp-file :test-file :register-stub))
      (let ((p (plist-get paths k)))
        (when (file-exists-p p)
          (delete-file p)
          (cl-incf deleted))
        ;; Drop the byte-compiled artefact too.
        (let ((elc (concat p "c")))
          (when (file-exists-p elc)
            (delete-file elc)
            (cl-incf deleted)))))
    (list :status :removed :name name :deleted deleted)))

;;;; --- MCP wrappers -------------------------------------------------------

;; Forward declarations to silence the byte-compiler when anvil-server
;; is not on `load-path' at compile time (e.g. running ERT in a fresh
;; Emacs that only requires `anvil-extend').  The macro
;; `anvil-server-with-error-handling' is referenced in the wrapper
;; below; pull anvil-server in at compile time when it is reachable so
;; the macro is fully expanded for byte-compilation, but tolerate its
;; absence at run time on hosts that have not loaded the server.
(declare-function anvil-server-register-tool "anvil-server")
(declare-function anvil-server-unregister-tool "anvil-server")
(declare-function anvil-server-tool-throw "anvil-server")
(eval-when-compile
  (when (locate-library "anvil-server")
    (require 'anvil-server nil t)))

(defun anvil-extend--tool-scaffold (name &optional params body docstring)
  "MCP wrapper for `anvil-extend-scaffold'.

MCP Parameters:
  name - extension name (string, lowercase + hyphens)
  params - optional arglist (list of symbol names, may be nil)
  body - optional body forms (list of S-expressions)
  docstring - optional docstring + MCP description for the new tool

Returns the printed plist returned by `anvil-extend-scaffold'."
  (anvil-server-with-error-handling
   (let* ((sym (cond ((symbolp name) name)
                     ((stringp name) (intern name))
                     (t (user-error
                         "anvil-extend: NAME must be string or symbol, got %S"
                         name))))
          (result (anvil-extend-scaffold
                   sym
                   :params params
                   :body body
                   :docstring docstring)))
     (format "%S" result))))

;;;; --- module lifecycle ---------------------------------------------------

;;;###autoload
(defun anvil-extend-enable ()
  "Register anvil-extend's MCP tools.

Phase A only registers a single tool, `anvil-extend-scaffold',
which emits the three scaffold files.  `load' / `test' / `list' /
`remove' stay function-only for the moment because they are
short-circuit operations the user typically drives interactively
while iterating on the generated source."
  (interactive)
  (require 'anvil-server)
  (anvil-server-register-tool
   #'anvil-extend--tool-scaffold
   :id "anvil-extend-scaffold"
   :intent '(extend scaffold codegen)
   :layer 'experimental
   :stability 'experimental
   :server-id anvil-extend-server-id
   :description
   "Generate three scaffold files for a new ad-hoc anvil extension:
`<NAME>.el' (function body), `<NAME>-test.el' (ERT skeleton),
`<NAME>-register.el' (MCP register stub).  Files land under
`anvil-extend-storage-dir' (default ~/.anvil-extend/).  The new
tool is NOT auto-loaded; call `anvil-extend-load' to byte-compile
+ load it, then `anvil-extend-test' to run its ERT.  Phase A of
Doc 35 (DRAFT v0) — API surface may rename in subsequent phases."))

(defun anvil-extend-disable ()
  "Unregister anvil-extend's MCP tools."
  (interactive)
  (when (featurep 'anvil-server)
    (ignore-errors
      (anvil-server-unregister-tool
       "anvil-extend-scaffold" anvil-extend-server-id))))

(provide 'anvil-extend)
;;; anvil-extend.el ends here
