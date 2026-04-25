;;; anvil-extend.el --- Claude self-extension SDK scaffold + hot-reload + sandbox v0 (Phase A+B+C) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton
;; Keywords: comm, tools, ai, mcp, codegen
;; Package-Requires: ((emacs "28.2"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; anvil-extend implements Phases A, B, and C (sandbox v0) of the
;; Claude self-extension SDK sketched in
;; `docs/design/38-claude-self-extension-sdk.org' (LOCKED v2;
;; renumbered from former Doc 35 per option B path).
;;
;; Phase A ships a scaffold generator that lets a Claude Code session
;; emit a new ad-hoc tool module — elisp body + ERT skeleton + MCP
;; register stub — into a storage directory that is completely isolated
;; from the host anvil package.
;;
;; Phase B layers transactional hot-reload on top: byte-compile the
;; freshly edited `<NAME>.el' into a per-pid staging area, swap the
;; loaded feature in place, and roll back to a backup if compile or
;; load fails.  A `filenotify' watcher lets a session auto-reload after
;; every edit so the inner loop is `scaffold → edit → save → ERT' with
;; no manual reload step.
;;
;; Phase C (sandbox v0) layers an isolated-subprocess + static AST
;; policy-scan baseline on top.  Per Doc 38 §3.C the v0 surface is
;; deliberately the best-effort layer that can be built on shipped
;; code: forms are evaluated through `anvil-offload' (a separate REPL
;; subprocess) so a runaway eval cannot directly damage the host
;; daemon, and a recursive AST walker rejects forms that statically
;; reference high-privilege callables (shell-command, delete-file,
;; make-process, network primitives, ...) before any subprocess hop
;; is made.  This is *not* a syscall-level sandbox — file I/O and
;; network sockets opened from inside the subprocess still hit the
;; OS — and Doc 38 §3.C.v1 / §6.1 will eventually layer real
;; seccomp / RLIMIT / chroot enforcement on top.  Phase v0's scope is
;; strictly: process isolation + AST scan + per-session policy plist.
;;
;; Public API (all `anvil-extend-*' prefixed for safe namespace
;; reservation per Doc 38 §3.A/§3.B spec; the Phase A names are
;; LOCKED, Phase B adds five names that are equally stable):
;;
;;   (anvil-extend-scaffold NAME &key params body docstring)  -- Phase A
;;     Emit `<NAME>.el', `<NAME>-test.el', `<NAME>-register.el' under
;;     `anvil-extend-storage-dir' and return a plist describing the
;;     three written paths.
;;
;;   (anvil-extend-load NAME)                                 -- Phase A
;;     Byte-compile and load `<NAME>.el'.  Returns a plist with
;;     `:status :loaded' on success, `:status :failed :reason ...' on
;;     failure (no error is raised so MCP callers can introspect).
;;
;;   (anvil-extend-test NAME)                                 -- Phase A
;;     Run `<NAME>-test.el' under ERT-batch and report pass/fail
;;     counts as a plist.
;;
;;   (anvil-extend-list)                                      -- Phase A
;;     Enumerate every scaffolded extension currently visible under
;;     `anvil-extend-storage-dir'.
;;
;;   (anvil-extend-remove NAME)                               -- Phase A
;;     Delete the three files belonging to NAME and `unload-feature'
;;     the loaded extension if applicable.
;;
;;   (anvil-extend-reload NAME)                               -- Phase B
;;     Transactional re-byte-compile + unload-feature + load.  Backs
;;     up the live `.elc' before swapping and restores it on failure
;;     so the previous definition stays in memory after a bad edit.
;;     Returns `(:status :reloaded ...)' / `(:status :rolled-back ...)'
;;     / `(:status :failed ...)'.
;;
;;   (anvil-extend-reload-all)                                -- Phase B
;;     Sweep every scaffolded extension and `anvil-extend-reload' it.
;;
;;   (anvil-extend-watch NAME)                                -- Phase B
;;     Add a `filenotify' watch on `<NAME>.el' that auto-reloads
;;     when the file changes (debounced so a single save does not
;;     trigger N reloads).  Returns the watch handle.
;;
;;   (anvil-extend-unwatch NAME-OR-HANDLE)                    -- Phase B
;;     Tear down a previously installed watch.
;;
;;   (anvil-extend-watch-all)                                 -- Phase B
;;     Watch every scaffolded extension currently on disk.
;;
;;   (anvil-extend-eval-sandboxed FORM &key timeout memory-limit)  -- Phase C
;;     Statically AST-scan FORM for blacklisted callables; on clean
;;     scan, ship FORM to an `anvil-offload' subprocess and await the
;;     reply.  Returns a plist `(:status :success :result V)' /
;;     `(:status :violation :forms ...)' / `(:status :timeout ...)' /
;;     `(:status :error :reason ...)' — failures are reported as data
;;     so MCP callers do not have to wrap a `condition-case'.
;;
;;   (anvil-extend-scan-dangerous-forms FORM)                 -- Phase C
;;     Pure recursive AST walker; returns the list of
;;     `(:form FORM :reason REASON)' hits, or nil if FORM only uses
;;     callables outside the blacklist.  Honors any allowances
;;     toggled by `anvil-extend-sandbox-policy'.
;;
;;   (anvil-extend-sandbox-policy &key allow-shell allow-network
;;                                     allow-fileio)           -- Phase C
;;     Update the per-session policy plist that
;;     `anvil-extend-scan-dangerous-forms' consults.  Defaults deny
;;     all three classes; pass `:allow-shell t' (etc.) to permit a
;;     class.  Returns the resulting plist.
;;
;; MCP tools registered against `emacs-eval':
;;
;;   `anvil-extend-scaffold'              (Phase A) — emit scaffold files
;;   `anvil-extend-reload'                (Phase B) — transactional hot-reload
;;   `anvil-extend-watch'                 (Phase B) — install auto-reload watch
;;   `anvil-extend-eval-sandboxed'        (Phase C) — sandbox v0 eval
;;   `anvil-extend-scan-dangerous-forms'  (Phase C) — AST policy scan
;;
;; Phases D-F (rationale auto-record / NeLisp execute path / ephemeral-
;; permanent promotion gates) remain out of scope per Doc 38 §3.D-§3.F.
;; Phase C v1 (real syscall sandbox = seccomp / RLIMIT / chroot /
;; namespace) is the future Phase 9d.A6 task per Doc 38 §3.C.v1 / §6.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ert)
(require 'filenotify)
(require 'bytecomp)

;; Phase C uses anvil-offload's REPL pool + future API for the
;; isolated-subprocess baseline.  We require eagerly because
;; `anvil-extend-eval-sandboxed' is part of the public surface and
;; load failure should surface at module load time, not first call.
(require 'anvil-offload)

;;;; --- customization ------------------------------------------------------

(defgroup anvil-extend nil
  "Claude self-extension SDK scaffold generator (Doc 38 Phase A)."
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

(defcustom anvil-extend-watch-debounce 0.3
  "Debounce seconds applied to `anvil-extend-watch' file events.

A single editor `save-buffer' typically produces several
`filenotify' events (truncate + write + chmod).  Reloading on
every one of them is wasteful and can race with the editor's
own write.  Events that arrive within this many seconds of an
already-scheduled reload coalesce into a single reload."
  :type 'number
  :group 'anvil-extend)

(defcustom anvil-extend-staging-root
  (expand-file-name
   (format "anvil-extend-staging-%d/" (emacs-pid))
   temporary-file-directory)
  "Per-pid staging directory used by `anvil-extend-reload'.

Phase B byte-compiles the freshly edited `<NAME>.el' into this
directory before swapping it into the canonical storage dir, so
a partial / corrupt artifact never overwrites the live one.  See
Doc 38 §3.B step 1 (`staging') and §2.7 A (transactional reload)."
  :type 'directory
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

;;;; --- Phase B: transactional hot-reload ---------------------------------

(defvar anvil-extend--watches nil
  "Alist of (NAME . HANDLE) for live `filenotify' watchers.

NAME is the extension symbol; HANDLE is whatever
`file-notify-add-watch' returned and is what
`file-notify-rm-watch' expects.")

(defvar anvil-extend--reload-pending nil
  "Alist of (NAME . TIMER) for debounced reloads.

`anvil-extend-watch' coalesces a burst of `filenotify' events
into a single reload by re-arming the timer in this alist; only
the latest `run-at-time' actually fires.")

(define-error 'anvil-extend-reload-error
  "anvil-extend hot-reload failed")

(defun anvil-extend--ensure-staging ()
  "Create `anvil-extend-staging-root' if absent and return its expanded path."
  (let ((dir (file-name-as-directory
              (expand-file-name anvil-extend-staging-root))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun anvil-extend--staging-path (name)
  "Return the staging `.elc' path used during reload of NAME."
  (let ((dir (anvil-extend--ensure-staging)))
    (expand-file-name (concat (symbol-name name) ".elc") dir)))

(defun anvil-extend--canonical-elc (name)
  "Return the canonical `.elc' path for NAME under storage dir."
  (concat (plist-get (anvil-extend--paths name) :elisp-file) "c"))

(defun anvil-extend--backup-path (name)
  "Return the rollback backup path used during reload of NAME."
  (let ((dir (anvil-extend--ensure-staging)))
    (expand-file-name (concat (symbol-name name) ".elc.bak") dir)))

(defun anvil-extend--byte-compile-quiet (src dest)
  "Byte-compile SRC writing the artifact to DEST.

Returns t on success, nil on failure.  Warnings and errors are
captured into the ` *anvil-extend-bc*' buffer rather than
spamming the calling session.  The byte-compiler is invoked with
`load-prefer-newer' bound to t so a stale `.elc' next to SRC
cannot mask a fresh `.el' (see
`feedback_canonical_byte_compile_stale_elc.md')."
  (let ((load-prefer-newer t)
        (byte-compile-dest-file-function (lambda (_) dest))
        (byte-compile-verbose nil)
        (byte-compile-warnings nil)
        (inhibit-message t)
        (message-log-max nil))
    (condition-case _err
        (let ((bytecomp-buf (get-buffer-create " *anvil-extend-bc*")))
          (with-current-buffer bytecomp-buf (erase-buffer))
          (and (byte-compile-file src)
               (file-exists-p dest)))
      (error nil))))

(defun anvil-extend--swap-load (src)
  "`unload-feature' + `load' SRC silently, returning success bool.

SRC is the canonical `.el' (or `.elc') path.  The feature symbol
matches the file's basename per the scaffold convention
(`(provide '<NAME>)' at the bottom of `<NAME>.el')."
  (let* ((base (file-name-base src))
         (sym (intern base)))
    (when (featurep sym)
      (ignore-errors (unload-feature sym t)))
    (let ((load-prefer-newer t)
          (inhibit-message t)
          (message-log-max nil))
      (condition-case err
          (progn (load src nil 'nomessage) t)
        (error
         (cons nil (error-message-string err)))))))

;;;###autoload
(defun anvil-extend-reload (name)
  "Transactionally re-byte-compile + reload extension NAME.

Steps (Doc 38 §3.B step 1-3):

  1. Stage: byte-compile `<NAME>.el' into a per-pid staging dir.
  2. Backup: copy the canonical `.elc' (if any) to a `.bak'.
  3. Swap: move staging artifact over the canonical `.elc',
     `unload-feature' NAME, then `load' the freshly written file.

On compile or load failure the canonical `.elc' is restored from
the backup and the previously loaded definition stays in memory
(or is reloaded from the backup if `unload-feature' had already
fired).  Failure is reported as data, not as a signal.

Returns one of:

  (:status :reloaded     :name NAME)
  (:status :rolled-back  :name NAME :stage compile|load
                          :reason MSG)
  (:status :failed       :name NAME :reason MSG)
  (:status :missing      :name NAME :reason MSG)"
  (anvil-extend--check-name name)
  (cl-block anvil-extend-reload
    (let* ((paths (anvil-extend--paths name))
           (src   (plist-get paths :elisp-file)))
      (unless (file-exists-p src)
        (cl-return-from anvil-extend-reload
          (list :status :missing
                :name name
                :reason (format "no scaffold file at %s" src))))
      (let* ((staging   (anvil-extend--staging-path name))
             (canonical (anvil-extend--canonical-elc name))
             (backup    (anvil-extend--backup-path name))
             (had-elc   (file-exists-p canonical)))
        ;; Step 0: capture rollback baseline.
        (when had-elc
          (copy-file canonical backup t t))
        ;; Step 1: stage compile.
        (unless (anvil-extend--byte-compile-quiet src staging)
          (when (file-exists-p staging)
            (ignore-errors (delete-file staging)))
          (cl-return-from anvil-extend-reload
            (list :status :rolled-back
                  :name name
                  :stage 'compile
                  :reason (format "byte-compile failed for %s" src))))
        ;; Step 2: swap staging artifact in.
        (condition-case err
            (rename-file staging canonical t)
          (error
           (when had-elc
             (ignore-errors (copy-file backup canonical t t)))
           (cl-return-from anvil-extend-reload
             (list :status :failed
                   :name name
                   :reason (format "swap failed: %s"
                                   (error-message-string err))))))
        ;; Step 3: load and report.
        (let ((swap-result (anvil-extend--swap-load src)))
          (cond
           ((eq swap-result t)
            (when (file-exists-p backup)
              (ignore-errors (delete-file backup)))
            (list :status :reloaded :name name))
           (t
            (when had-elc
              (ignore-errors (copy-file backup canonical t t))
              (ignore-errors
                (let ((inhibit-message t)
                      (message-log-max nil))
                  (load canonical nil 'nomessage))))
            (list :status :rolled-back
                  :name name
                  :stage 'load
                  :reason (or (cdr swap-result)
                              "load failed")))))))))

;;;###autoload
(defun anvil-extend-reload-all ()
  "Reload every scaffolded extension currently visible on disk.

Returns a list of plists, one per extension, each of which is
the value returned by `anvil-extend-reload'."
  (mapcar
   (lambda (row) (anvil-extend-reload (plist-get row :name)))
   (anvil-extend-list)))

;;;###autoload
(defun anvil-extend-watch (name)
  "Add a `filenotify' watch on extension NAME, auto-reloading on edit.

Returns the watch handle (the value `file-notify-add-watch'
returns).  Subsequent calls for the same NAME tear down the
previous watch first so handles do not leak.

Reloads are debounced by `anvil-extend-watch-debounce' so a
single editor save (which typically emits several `change'
events) coalesces into one reload.

Phase B does not auto-record rationale (= Phase D); this call
exists purely to close the inner edit-save-test loop."
  (anvil-extend--check-name name)
  (let* ((paths (anvil-extend--paths name))
         (src   (plist-get paths :elisp-file)))
    (unless (file-exists-p src)
      (user-error "anvil-extend: cannot watch %s — %s does not exist"
                  name src))
    ;; Drop any existing watch for the same NAME.
    (anvil-extend-unwatch name)
    (let* ((cb
            (lambda (event)
              (let ((action (and (consp event) (nth 1 event))))
                (when (memq action '(changed renamed created attribute-changed))
                  (anvil-extend--schedule-reload name)))))
           (handle
            (file-notify-add-watch src '(change attribute-change) cb)))
      (push (cons name handle) anvil-extend--watches)
      handle)))

(defun anvil-extend--schedule-reload (name)
  "Re-arm the debounce timer for NAME.

If a reload is already pending it is cancelled and replaced so
the burst of `filenotify' events emitted by a single save does
not produce multiple reloads."
  (let ((existing (assq name anvil-extend--reload-pending)))
    (when (and existing (timerp (cdr existing)))
      (cancel-timer (cdr existing)))
    (setq anvil-extend--reload-pending
          (assq-delete-all name anvil-extend--reload-pending)))
  (let ((timer
         (run-at-time
          anvil-extend-watch-debounce nil
          (lambda ()
            (setq anvil-extend--reload-pending
                  (assq-delete-all name anvil-extend--reload-pending))
            (ignore-errors (anvil-extend-reload name))))))
    (push (cons name timer) anvil-extend--reload-pending)
    timer))

;;;###autoload
(defun anvil-extend-unwatch (name-or-handle)
  "Remove a previously installed watch.

NAME-OR-HANDLE may be the extension symbol (in which case the
matching entry is looked up in `anvil-extend--watches') or the
opaque handle returned by `anvil-extend-watch'.  Returns t when
something was removed, nil otherwise."
  (cond
   ((symbolp name-or-handle)
    (let ((cell (assq name-or-handle anvil-extend--watches)))
      (when cell
        (ignore-errors (file-notify-rm-watch (cdr cell)))
        (setq anvil-extend--watches
              (assq-delete-all name-or-handle anvil-extend--watches))
        ;; Also drop any pending debounced reload.
        (let ((p (assq name-or-handle anvil-extend--reload-pending)))
          (when (and p (timerp (cdr p))) (cancel-timer (cdr p))))
        (setq anvil-extend--reload-pending
              (assq-delete-all name-or-handle anvil-extend--reload-pending))
        t)))
   (t
    (let ((cell (rassoc name-or-handle anvil-extend--watches)))
      (when cell
        (ignore-errors (file-notify-rm-watch (cdr cell)))
        (setq anvil-extend--watches
              (delq cell anvil-extend--watches))
        (let ((p (assq (car cell) anvil-extend--reload-pending)))
          (when (and p (timerp (cdr p))) (cancel-timer (cdr p))))
        (setq anvil-extend--reload-pending
              (assq-delete-all (car cell) anvil-extend--reload-pending))
        t)))))

;;;###autoload
(defun anvil-extend-watch-all ()
  "Install a `filenotify' watch on every visible scaffolded extension.

Returns a list of (NAME . HANDLE) pairs."
  (mapcar
   (lambda (row)
     (let ((name (plist-get row :name)))
       (cons name (anvil-extend-watch name))))
   (anvil-extend-list)))

;;;; --- Phase C: sandbox v0 (isolated subprocess + AST scan) --------------

;; Per Doc 38 §3.C.v0 the Phase C scope is *process isolation +
;; static AST policy scan*, NOT a syscall-level sandbox.  See the
;; commentary above and the §3.C.v0 / §6.1 sections of the design
;; doc for the full enumeration of what is and is not enforced.

(defcustom anvil-extend-sandbox-default-timeout 5.0
  "Default seconds awaited for a sandboxed eval before timing out.

The timeout is enforced via `anvil-future-await' on the
`anvil-offload' future.  On expiry the future is left running
in the subprocess (use `anvil-future-kill' from the result
plist's `:future' field if you need to hard-stop it); a
`(:status :timeout ...)' plist is returned to the caller."
  :type 'number
  :group 'anvil-extend)

(defcustom anvil-extend-sandbox-memory-limit nil
  "Soft memory limit hint (bytes) recorded into the result plist.

Phase v0 does not enforce this — `RLIMIT_AS' is a Phase v1
concern (Doc 38 §3.C.v1 / §6).  The value is merely echoed back
in the result plist so a caller can record what it asked for."
  :type '(choice (const :tag "Unbounded" nil) integer)
  :group 'anvil-extend)

(defconst anvil-extend--dangerous-shell-functions
  '(shell-command
    shell-command-to-string
    call-process
    call-process-region
    call-process-shell-command
    process-file
    process-file-shell-command
    start-process
    start-process-shell-command
    start-file-process
    start-file-process-shell-command
    make-process
    make-pipe-process)
  "Callables classified as `:shell' for AST policy purposes.

Hits in this list are rejected unless the active policy plist
has `:allow-shell t'.  The list intentionally covers both the
synchronous `*-process' family and the asynchronous
`make-process' / `start-process' family because both can shell
out under the right `:command' value.")

(defconst anvil-extend--dangerous-fileio-functions
  '(delete-file
    delete-directory
    rename-file
    copy-file
    write-region
    set-file-modes
    set-file-times
    set-file-acl
    set-file-extended-attributes
    add-name-to-file
    make-symbolic-link
    chmod
    chown)
  "Callables classified as `:fileio' for AST policy purposes.

Hits in this list are rejected unless the active policy plist
has `:allow-fileio t'.  Read-only file access (e.g.
`insert-file-contents') is intentionally NOT in this list — the
v0 policy is mutation-oriented because read-only access cannot
damage the host filesystem.")

(defconst anvil-extend--dangerous-network-functions
  '(make-network-process
    open-network-stream
    url-retrieve
    url-retrieve-synchronously
    url-copy-file
    url-http
    network-stream-open
    open-tls-stream)
  "Callables classified as `:network' for AST policy purposes.

Hits in this list are rejected unless the active policy plist
has `:allow-network t'.")

(defun anvil-extend--default-policy ()
  "Return a freshly allocated deny-all policy plist.

Built with `list' (not a quoted literal) so successive
`plist-put' calls — which mutate cells in place when a key
already exists — cannot share structure with the module-level
default and silently flip subsequent callers' policy.  This
mirrors `feedback_elisp_quoted_literal_nreverse_share.md'."
  (list :allow-shell nil :allow-network nil :allow-fileio nil))

(defvar anvil-extend--sandbox-policy
  (anvil-extend--default-policy)
  "Per-session policy plist consulted by the AST scanner.

Default denies all three classes (`:allow-shell',
`:allow-network', `:allow-fileio').  Mutated through
`anvil-extend-sandbox-policy'; per Doc 38 §3.C.v0 the policy
layer is best-effort — flipping `:allow-shell t' lets a form
through the AST gate but does not weaken any other defence
because there is no other defence at v0.")

(defun anvil-extend--policy-class-allowed-p (class &optional policy)
  "Return non-nil if CLASS is currently permitted by POLICY.

CLASS is one of `:shell', `:fileio', `:network'.  POLICY
defaults to the active `anvil-extend--sandbox-policy'."
  (let* ((p (or policy anvil-extend--sandbox-policy))
         (key (pcase class
                (:shell   :allow-shell)
                (:fileio  :allow-fileio)
                (:network :allow-network)
                (_        nil))))
    (and key (plist-get p key))))

(defun anvil-extend--classify-function (sym)
  "Return the policy class for SYM, or nil if not blacklisted.

Returns one of `:shell', `:fileio', `:network', or nil."
  (cond
   ((memq sym anvil-extend--dangerous-shell-functions)   :shell)
   ((memq sym anvil-extend--dangerous-fileio-functions)  :fileio)
   ((memq sym anvil-extend--dangerous-network-functions) :network)
   (t                                                    nil)))

(defun anvil-extend-scan-dangerous-forms (form &optional policy)
  "Recursively scan FORM for blacklisted callables.

POLICY is an optional policy plist (same shape as
`anvil-extend--sandbox-policy'); defaults to the live module
value.  Returns a list of `(:form SUBFORM :reason CLASS)'
plists, or nil if FORM only references callables that are
either outside the blacklist or explicitly permitted by POLICY.

The walker descends into every `cdr' of every cons cell so
nested forms (e.g. `(let (...) (shell-command ...))') are
detected.  Vectors are walked elementwise.  Atoms (numbers,
strings, symbols other than the call head, ...) are pure
data and never produce a hit.  No macroexpansion is performed
so a macro that internally expands to `shell-command' will
*not* trip the gate — Doc 38 §3.C.v0 documents this as an
explicit best-effort limitation."
  (let ((hits nil)
        (active-policy (or policy anvil-extend--sandbox-policy)))
    (cl-labels
        ((walk (node)
           (cond
            ((vectorp node)
             (dotimes (i (length node))
               (walk (aref node i))))
            ((consp node)
             (let ((head (car node)))
               (when (symbolp head)
                 (let ((class (anvil-extend--classify-function head)))
                   (when (and class
                              (not (anvil-extend--policy-class-allowed-p
                                    class active-policy)))
                     (push (list :form node :reason class) hits)))))
             ;; Walk every element so that head-position calls inside
             ;; nested forms (e.g. (let (...) (shell-command ...))) are
             ;; also caught.  `dolist' over a list whose tail is nil
             ;; handles both proper and dotted lists adequately for
             ;; macro-shaped source.
             (let ((cur node))
               (while (consp cur)
                 (walk (car cur))
                 (setq cur (cdr cur)))
               ;; A trailing non-nil atom is just data; ignore.
               )))))
      (walk form))
    (nreverse hits)))

(defun anvil-extend-sandbox-policy (&rest args)
  "Update the per-session sandbox policy plist and return it.

ARGS is a list of keyword/value pairs.  Recognised keys:

  :allow-shell    BOOL  -- permit AST hits in
                           `anvil-extend--dangerous-shell-functions'
  :allow-network  BOOL  -- permit AST hits in
                           `anvil-extend--dangerous-network-functions'
  :allow-fileio   BOOL  -- permit AST hits in
                           `anvil-extend--dangerous-fileio-functions'

Unknown keys are stored verbatim so future Phase v1 additions
(e.g. `:max-memory') compose without code changes here.  Returns
the resulting policy plist."
  (cl-loop for (k v) on args by #'cddr
           do (setq anvil-extend--sandbox-policy
                    (plist-put anvil-extend--sandbox-policy k v)))
  anvil-extend--sandbox-policy)

(defun anvil-extend-sandbox-policy-reset ()
  "Reset the sandbox policy to the deny-all default.

Provided so tests (and humans) can recover deterministically
from a session that flipped a class on with
`anvil-extend-sandbox-policy'.  Returns the reset plist.  Uses
`anvil-extend--default-policy' so successive resets do not
share structure (see commentary on that helper)."
  (setq anvil-extend--sandbox-policy (anvil-extend--default-policy)))

(define-error 'anvil-extend-sandbox-violation
  "anvil-extend sandbox AST policy violation")

;;;###autoload
(cl-defun anvil-extend-eval-sandboxed (form &key timeout memory-limit policy)
  "Evaluate FORM in an isolated subprocess after a static AST policy scan.

Steps (Doc 38 §3.C.v0):

  1. Walk FORM with `anvil-extend-scan-dangerous-forms' under
     POLICY (default = live `anvil-extend--sandbox-policy').
     If any blacklisted callable shows up that the policy does
     not permit, return immediately with `:status :violation'.
  2. Ship the still-quoted FORM to an `anvil-offload' subprocess
     so a runaway eval cannot crash or wedge the host daemon.
  3. `anvil-future-await' the reply with TIMEOUT seconds (defaults
     to `anvil-extend-sandbox-default-timeout').  On timeout the
     future is returned in the result plist so a caller can
     `anvil-future-kill' it; the subprocess is *not* auto-killed
     because Phase v0 is opt-in on hard-kill.

Failure modes are reported as data, not as signals, so MCP
callers can introspect without a `condition-case' wrapper.
Returns one of:

  (:status :success   :result VALUE)
  (:status :violation :forms LIST :reason \"static AST scan rejected\")
  (:status :timeout   :timeout SEC :future FUTURE)
  (:status :error     :reason MSG)

MEMORY-LIMIT is currently a hint only; per Doc 38 §3.C.v0 there
is no `RLIMIT_AS' enforcement at v0 — it is echoed back in the
`:memory-limit' field of the result plist so callers can record
what they asked for."
  (let* ((scan-policy (or policy anvil-extend--sandbox-policy))
         (hits (anvil-extend-scan-dangerous-forms form scan-policy)))
    (cond
     (hits
      (list :status :violation
            :forms hits
            :reason "static AST scan rejected"
            :memory-limit (or memory-limit
                              anvil-extend-sandbox-memory-limit)))
     (t
      (let ((wait (or timeout anvil-extend-sandbox-default-timeout)))
        (condition-case err
            (let* ((future (anvil-offload `(eval ',form lexical-binding)))
                   (settled (anvil-future-await future wait)))
              (cond
               ((not settled)
                (list :status :timeout
                      :timeout wait
                      :future future
                      :memory-limit (or memory-limit
                                        anvil-extend-sandbox-memory-limit)))
               ((eq (anvil-future-status future) 'done)
                (list :status :success
                      :result (anvil-future-value future)
                      :memory-limit (or memory-limit
                                        anvil-extend-sandbox-memory-limit)))
               ((eq (anvil-future-status future) 'error)
                (list :status :error
                      :reason (or (anvil-future-error future)
                                  "subprocess returned error")
                      :memory-limit (or memory-limit
                                        anvil-extend-sandbox-memory-limit)))
               (t
                (list :status :error
                      :reason (format "unexpected future status: %S"
                                      (anvil-future-status future))
                      :memory-limit (or memory-limit
                                        anvil-extend-sandbox-memory-limit)))))
          (error
           (list :status :error
                 :reason (error-message-string err)
                 :memory-limit (or memory-limit
                                   anvil-extend-sandbox-memory-limit)))))))))

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

(defun anvil-extend--coerce-name (name)
  "Coerce NAME (string or symbol) into a valid extension symbol."
  (cond ((symbolp name) name)
        ((stringp name) (intern name))
        (t (user-error
            "anvil-extend: NAME must be string or symbol, got %S" name))))

(defun anvil-extend--tool-scaffold (name &optional params body docstring)
  "MCP wrapper for `anvil-extend-scaffold'.

MCP Parameters:
  name - extension name (string, lowercase + hyphens)
  params - optional arglist (list of symbol names, may be nil)
  body - optional body forms (list of S-expressions)
  docstring - optional docstring + MCP description for the new tool

Returns the printed plist returned by `anvil-extend-scaffold'."
  (anvil-server-with-error-handling
   (let* ((sym (anvil-extend--coerce-name name))
          (result (anvil-extend-scaffold
                   sym
                   :params params
                   :body body
                   :docstring docstring)))
     (format "%S" result))))

(defun anvil-extend--tool-reload (name)
  "MCP wrapper for `anvil-extend-reload'.

MCP Parameters:
  name - extension name (string or symbol; lowercase + hyphens)

Returns the printed plist returned by `anvil-extend-reload'
(`:status :reloaded' / `:rolled-back' / `:failed' / `:missing')."
  (anvil-server-with-error-handling
   (let* ((sym (anvil-extend--coerce-name name))
          (result (anvil-extend-reload sym)))
     (format "%S" result))))

(defun anvil-extend--tool-watch (name)
  "MCP wrapper for `anvil-extend-watch'.

MCP Parameters:
  name - extension name (string or symbol; lowercase + hyphens)

Returns a printed plist `(:status :watching :name NAME)' so the
opaque `filenotify' handle (which is not JSON-serialisable) does
not leak across the MCP boundary.  Use `anvil-extend-unwatch'
with the NAME to tear the watch down."
  (anvil-server-with-error-handling
   (let ((sym (anvil-extend--coerce-name name)))
     (anvil-extend-watch sym)
     (format "%S" (list :status :watching :name sym)))))

(defun anvil-extend--coerce-form (form)
  "Coerce FORM (string or list) into an evaluable S-expression.

When FORM is a string it is `read'-parsed; when it is already a
cons / atom it is returned as-is.  Used by the MCP wrappers so
JSON callers can pass either a printed S-expr or a structured
list."
  (cond
   ((stringp form)
    (condition-case err
        (car (read-from-string form))
      (error
       (user-error
        "anvil-extend: cannot read FORM string: %s"
        (error-message-string err)))))
   (t form)))

(defun anvil-extend--tool-eval-sandboxed (form &optional timeout memory-limit)
  "MCP wrapper for `anvil-extend-eval-sandboxed'.

MCP Parameters:
  form - S-expression to evaluate (printed string or structured list)
  timeout - optional seconds to await before reporting :timeout
  memory-limit - optional byte hint (echoed back; not enforced at v0)

Returns the printed result plist (`:status :success' / `:violation'
/ `:timeout' / `:error').  The opaque `:future' handle from a
`:timeout' result is stripped before printing because futures are
not safely serialisable across the MCP boundary."
  (anvil-server-with-error-handling
   (let* ((parsed (anvil-extend--coerce-form form))
          (result (anvil-extend-eval-sandboxed
                   parsed
                   :timeout      timeout
                   :memory-limit memory-limit))
          ;; Strip the (unprintable / non-serialisable) future
          ;; before crossing the MCP boundary.
          (clean (cl-loop for (k v) on result by #'cddr
                          unless (eq k :future)
                          append (list k v))))
     (format "%S" clean))))

(defun anvil-extend--tool-scan-dangerous-forms (form)
  "MCP wrapper for `anvil-extend-scan-dangerous-forms'.

MCP Parameters:
  form - S-expression to scan (printed string or structured list)

Returns a printed list of `(:form ... :reason ...)' hits, or the
string \"nil\" when FORM passes the active policy."
  (anvil-server-with-error-handling
   (let* ((parsed (anvil-extend--coerce-form form))
          (hits (anvil-extend-scan-dangerous-forms parsed)))
     (format "%S" hits))))

;;;; --- module lifecycle ---------------------------------------------------

;;;###autoload
(defun anvil-extend-enable ()
  "Register anvil-extend's MCP tools.

Phase A registers `anvil-extend-scaffold'.  Phase B adds
`anvil-extend-reload' (transactional hot-reload after edit) and
`anvil-extend-watch' (auto-reload on file change).  `load' /
`test' / `list' / `remove' / `reload-all' / `unwatch' /
`watch-all' stay function-only because they are typically driven
interactively while iterating on the generated source."
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
Doc 38 (LOCKED v2) — Phase A surface is stable; later phases extend.")
  (anvil-server-register-tool
   #'anvil-extend--tool-reload
   :id "anvil-extend-reload"
   :intent '(extend reload hot-reload)
   :layer 'experimental
   :stability 'experimental
   :server-id anvil-extend-server-id
   :description
   "Transactionally re-byte-compile and reload a scaffolded extension.
Stages the new artifact under a per-pid staging dir, swaps it
into the canonical storage dir, and rolls back from a backup
on compile or load failure so the previously loaded definition
remains in memory.  Phase B of Doc 38 §3.B (LOCKED v2).")
  (anvil-server-register-tool
   #'anvil-extend--tool-watch
   :id "anvil-extend-watch"
   :intent '(extend reload watch filenotify)
   :layer 'experimental
   :stability 'experimental
   :server-id anvil-extend-server-id
   :description
   "Install a `filenotify' watch that auto-reloads an extension on edit.
Reloads are debounced (default 0.3s) so a single editor save
coalesces into one transactional reload (Phase B of Doc 38 §3.B
LOCKED v2).  Use `anvil-extend-unwatch' with the same NAME to
remove the watch.")
  (anvil-server-register-tool
   #'anvil-extend--tool-eval-sandboxed
   :id "anvil-extend-eval-sandboxed"
   :intent '(extend sandbox eval isolated)
   :layer 'experimental
   :stability 'experimental
   :server-id anvil-extend-server-id
   :description
   "Evaluate an S-expression in an isolated `anvil-offload' subprocess
after a static AST policy scan rejects high-privilege callables
(shell-command / delete-file / make-process / network primitives).
Returns a plist `(:status :success | :violation | :timeout | :error
...)'.  Phase C v0 of Doc 38 §3.C — process isolation + AST scan
only, NOT a syscall-level sandbox; real seccomp / RLIMIT / chroot
enforcement is the future Phase v1 (Doc 38 §3.C.v1 / §6).")
  (anvil-server-register-tool
   #'anvil-extend--tool-scan-dangerous-forms
   :id "anvil-extend-scan-dangerous-forms"
   :intent '(extend sandbox scan ast)
   :layer 'experimental
   :stability 'experimental
   :server-id anvil-extend-server-id
   :description
   "Statically AST-scan an S-expression for blacklisted callables.
Returns a list of `(:form ... :reason CLASS)' hits (CLASS is one
of `:shell', `:fileio', `:network'), or nil if FORM only uses
callables that the active policy allows.  Pure read-only — no
subprocess hop, no host-side eval.  Phase C v0 of Doc 38 §3.C."))

(defun anvil-extend-disable ()
  "Unregister anvil-extend's MCP tools."
  (interactive)
  (when (featurep 'anvil-server)
    (dolist (id '("anvil-extend-scaffold"
                  "anvil-extend-reload"
                  "anvil-extend-watch"
                  "anvil-extend-eval-sandboxed"
                  "anvil-extend-scan-dangerous-forms"))
      (ignore-errors
        (anvil-server-unregister-tool id anvil-extend-server-id)))))

(provide 'anvil-extend)
;;; anvil-extend.el ends here
