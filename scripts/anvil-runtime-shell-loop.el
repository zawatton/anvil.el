;;; anvil-runtime-shell-loop.el --- nelisp-shell replacement for bin/anvil-runtime  -*- lexical-binding: t; -*-

;; Phase B5 Final B Stage 1 (= 2026-05-09)
;;
;; Doc anvil-runtime pure-elisp roadmap: replace the Rust crate
;; `anvil-runtime' (= 5,667 LOC) with a shell launcher that runs this
;; file under standalone NeLisp.  This file:
;;
;;   1. Loads the L2 Emacs C-primitive shims (`emacs-init', `emacs-stub')
;;      and the json / backquote / cl-defstruct fixes shipped in
;;      Phase B2-B5.
;;   2. Loads the anvil-server.el + anvil-server-commands.el +
;;      anvil-server-metrics.el modules from the user's anvil.el
;;      checkout.
;;   3. Activates the `read-from-minibuffer' shim (= line-buffered
;;      stdin from `read-stdin-bytes' bytes) so anvil-server's MCP
;;      Content-Length frame reader has a working line source.
;;   4. Calls `anvil-server-start' to install the active-server
;;      registry then enters `anvil-server-run-batch-stdio' which
;;      blocks reading frames until EOF.
;;
;; Configuration via env vars (matches Rust binary semantics):
;;   ANVIL_EL_DIR — directory containing anvil-server*.el (required;
;;                  default = $HOME/.emacs.d/external-packages/anvil.el).
;;   ANVIL_SERVER_ID — server-id argument passed to `anvil-server-start'
;;                     and `anvil-server-run-batch-stdio' (default
;;                     = "default").
;;   NELISP_EMACS_DIR — directory containing this file's siblings
;;                      `src/emacs-init.el' / `src/emacs-stub.el'
;;                      (= the nelisp-emacs checkout root).
;;
;; Once tested end-to-end, the Rust crate `anvil-runtime/' can be
;; deleted in Final B Stage 2 along with the bin/anvil-runtime symlink
;; rewire.

;;; Code:

(defun anvil-runtime-shell--env (name default)
  (let ((val (and (fboundp 'getenv) (getenv name))))
    (if (and val (> (length val) 0)) val default)))

;; Path resolution.  Priority order:
;;
;;   1. `anvil-runtime-bootstrap-{anvil-el,nelisp-emacs}-dir' set by
;;      `bin/anvil-runtime' before loading us.  This is the primary
;;      channel — env vars cannot work because standalone NeLisp's
;;      `getenv' is itself a polyfill that only binds once
;;      `emacs-callproc.el' is loaded inside `(load init-el)' below.
;;
;;   2. `getenv' fallback — useful when this file is loaded under host
;;      Emacs for tests / interactive development.
;;
;;   3. `load-file-name'-derived sibling-checkout convention
;;      (`<parent>/anvil.el', `<parent>/nelisp-emacs').
;;
;;   4. Hardcoded dev-mirror fallback.
(let* ((anvil-el-dir
        (or (and (boundp 'anvil-runtime-bootstrap-anvil-el-dir)
                 anvil-runtime-bootstrap-anvil-el-dir)
            (anvil-runtime-shell--env
             "ANVIL_EL_DIR"
             (or (and (boundp 'load-file-name) load-file-name
                      (file-name-directory
                       (directory-file-name
                        (file-name-directory load-file-name))))
                 "/home/madblack-21/Cowork/Notes/dev/anvil.el"))))
       (nelisp-emacs-dir
        (or (and (boundp 'anvil-runtime-bootstrap-nelisp-emacs-dir)
                 anvil-runtime-bootstrap-nelisp-emacs-dir)
            (anvil-runtime-shell--env
             "NELISP_EMACS_DIR"
             (concat (file-name-directory
                      (directory-file-name anvil-el-dir))
                     "nelisp-emacs"))))
       (server-id
        ;; Default `emacs-eval' matches the constant used by every
        ;; GREEN-bucket anvil-* module's `--server-id'.  Tools register
        ;; into that bucket, so the dispatch must look them up there.
        ;; Override via ANVIL_SERVER_ID once getenv reads OS env.
        (anvil-runtime-shell--env "ANVIL_SERVER_ID" "emacs-eval"))
       (init-el (concat nelisp-emacs-dir "/src/emacs-init.el"))
       (stub-el (concat nelisp-emacs-dir "/src/emacs-stub.el"))
       (stdio-el (concat nelisp-emacs-dir "/src/emacs-stdio.el"))
       (metrics-el (concat anvil-el-dir "/anvil-server-metrics.el"))
       (server-el (concat anvil-el-dir "/anvil-server.el"))
       (server-commands-el (concat anvil-el-dir "/anvil-server-commands.el")))

  ;; Bootstrap layer 2 + json / backquote fixes.
  (load init-el nil t)
  (load stub-el nil t)

  ;; --- TEMPORARY alist-get override (= Doc 98 §98.2 workaround) ---
  ;;
  ;; The fixed `alist-get' lives in `nelisp/lisp/nelisp-stdlib-misc.el'
  ;; on the source side, but standalone NeLisp's `.image' baker (= Doc 98
  ;; §98.2) doesn't yet capture elisp-side `defun' / `fset' mutations that
  ;; route through the env-shim, so the alist-get image baked into the
  ;; binary is still the pre-fix version.  Direct-load the .el here so
  ;; the corrected `alist-get' wins over the image-embedded one.
  ;;
  ;; Remove this block once Doc 98 §98.2 (= elisp-complete frozen-heap
  ;; baker) ships and the .image carries the fixed alist-get.
  (let* ((nelisp-lisp-dir
          (or (and (boundp 'anvil-runtime-bootstrap-nelisp-lisp-dir)
                   (> (length anvil-runtime-bootstrap-nelisp-lisp-dir) 0)
                   anvil-runtime-bootstrap-nelisp-lisp-dir)
              (concat (file-name-directory (directory-file-name anvil-el-dir))
                      "nelisp/lisp")))
         (stdlib-misc (concat nelisp-lisp-dir "/nelisp-stdlib-misc.el")))
    (when (file-exists-p stdlib-misc)
      (condition-case err
          (load stdlib-misc nil t)
        (error
         (when (fboundp 'nelisp--write-stderr-line)
           (nelisp--write-stderr-line
            (concat "[shell-loop] nelisp-stdlib-misc override load ERR: "
                    (format "%S" err))))))))

  ;; Put `anvil-el-dir' on `load-path' so any `(require 'anvil-orchestrator-routing)'
  ;; / `(require 'anvil-orchestrator-presets)' / etc. inside tool-module
  ;; files resolve to siblings of `anvil-server.el'.  Without this the
  ;; load/enable of those modules fails with "Cannot open load file".
  (add-to-list 'load-path anvil-el-dir)

  ;; anvil-server module load chain.  Order: metrics → server → commands
  ;; (= matches anvil-server.el's `(require 'anvil-server-metrics)' and
  ;; anvil-server-commands.el's `(require 'anvil-server)').
  (load metrics-el nil t)
  (load server-el nil t)
  (load server-commands-el nil t)

  ;; stdin shim — anvil-server-run-batch-stdio reads frames via
  ;; `read-from-minibuffer'; emacs-stdio.el's installer overrides the
  ;; bulk-stub nil binding with a chunked reader backed by libc.read.
  (load stdio-el nil t)
  (when (fboundp 'emacs-stdio-install-stdin-shim)
    (emacs-stdio-install-stdin-shim))

  ;; Substrate polyfills (cl-* gaps + sqlite cursor protocol +
  ;; benchmark/profiler stubs) for anvil-* GREEN-bucket modules.
  ;; Loaded BEFORE the tool-module chain so registrations and call
  ;; handlers see a healthy substrate.  See
  ;; `<anvil.el>/scripts/anvil-runtime-polyfills.el' for the surface
  ;; audit (= migrated from `nelisp-emacs/scripts/' 2026-05-14).
  (let ((polyfills-el (concat anvil-el-dir
                              "/scripts/anvil-runtime-polyfills.el")))
    (when (fboundp 'nelisp--write-stderr-line)
      (nelisp--write-stderr-line
       (concat "[shell-loop] loading polyfills " polyfills-el)))
    (condition-case err
        (load polyfills-el nil t)
      (error
       (when (fboundp 'nelisp--write-stderr-line)
         (nelisp--write-stderr-line
          (concat "[shell-loop] polyfills load ERR: "
                  (format "%S" err)))))))

  ;; `help-function-arglist' polyfill — emacs-stub-bulk.el ships a
  ;; nil-returning placeholder, but anvil-server.el's tool dispatcher
  ;; uses the arglist to validate provided params (= every supplied
  ;; param becomes "Unexpected" when the polyfill returns nil).
  ;; NeLisp standalone wraps user defuns as `(closure ENV ARGS BODY)',
  ;; so we extract ARGS by case on the head symbol.  Unconditional
  ;; `defun' overrides whatever bulk wired in.
  (defun help-function-arglist (function &optional _preserve-names)
    (let ((fn (if (symbolp function)
                  (symbol-function function)
                function)))
      (cond
       ((and (consp fn) (eq (car fn) 'closure))
        (car (cdr (cdr fn))))
       ((and (consp fn) (eq (car fn) 'lambda))
        (car (cdr fn)))
       ((and (consp fn) (eq (car fn) 'macro))
        (help-function-arglist (cdr fn) _preserve-names))
       ((and (vectorp fn) (>= (length fn) 1))
        (aref fn 0))
       (t nil))))

  ;; Phase B5 Final B Stage 1b — regex-free overrides for the framing
  ;; helpers anvil-server.el uses.  Standalone NeLisp ships only
  ;; `string-match-p' and even that is a hand-rolled lookup table (=
  ;; not a real regex engine), so the original `string-match' /
  ;; `replace-regexp-in-string' patterns return nil unconditionally.
  ;; The overrides below cover the exact callsites in anvil-server's
  ;; MCP framing path with string-search / substring-based logic.

  ;; `(replace-regexp-in-string "\r\\'" "" line)' just strips a single
  ;; trailing CR.  Replace with a simple suffix check.
  (defun anvil-server--strip-trailing-cr (s)
    (if (and (stringp s)
             (> (length s) 0)
             (eq (aref s (1- (length s))) ?\r))
        (substring s 0 (1- (length s)))
      s))

  (defun anvil-server-mcp-parse-content-length-header (header-block)
    "Phase B5 Stage 1b override — regex-free Content-Length parser.
Walks HEADER-BLOCK line-by-line searching for `Content-Length:'
case-insensitively, returns the integer value or nil."
    (when (stringp header-block)
      (let ((lines (split-string header-block "\r\n"))
            (found nil))
        (while (and lines (not found))
          (let* ((line (anvil-server--strip-trailing-cr (car lines)))
                 (line-down (downcase line))
                 (prefix "content-length:"))
            (when (and (>= (length line-down) (length prefix))
                       (string= (substring line-down 0 (length prefix)) prefix))
              (let* ((rest (substring line (length prefix)))
                     ;; Skip leading whitespace.
                     (i 0)
                     (n (length rest)))
                (while (and (< i n)
                            (let ((c (aref rest i)))
                              (or (eq c 32) (eq c ?\t))))
                  (setq i (1+ i)))
                (let* ((num-start i)
                       (num-end i))
                  (while (and (< num-end n)
                              (let ((c (aref rest num-end)))
                                (and (>= c ?0) (<= c ?9))))
                    (setq num-end (1+ num-end)))
                  (when (> num-end num-start)
                    (setq found (string-to-number
                                 (substring rest num-start num-end))))))))
          (setq lines (cdr lines)))
        found)))

  (defun anvil-server-mcp-detect-framing-p (initial)
    "Phase B5 Stage 1b override — case-insensitive prefix check for
`Content-Length:' on INITIAL string (= the first line read from stdin)."
    (when (stringp initial)
      (let* ((stripped (anvil-server--strip-trailing-cr initial))
             (down (downcase stripped))
             (prefix "content-length:"))
        (and (>= (length down) (length prefix))
             (string= (substring down 0 (length prefix)) prefix)))))

  (defun anvil-server-mcp-frame-encode (body)
    "Phase B5 Stage 1b override — emit `Content-Length: N\r\n\r\nBODY'.
N is the UTF-8 byte length of BODY.  Standalone NeLisp strings are
already UTF-8, so `length' is byte-count for ASCII bodies and
`encode-coding-string' is a no-op identity per Phase B5 stub."
    (let* ((bytes (if (fboundp 'encode-coding-string)
                      (encode-coding-string body 'utf-8 t)
                    body))
           (n (length bytes)))
      (concat "Content-Length: " (number-to-string n) "\r\n\r\n" body)))

  ;; Override the framed-with-prefix reader entirely.  The original uses
  ;; `replace-regexp-in-string' for trailing-CR strip, which is a no-op
  ;; stub on standalone NeLisp — that caused the header read loop to
  ;; consume the body line as just another header (= the empty `\r' line
  ;; never matched `string-empty-p'), so by the time body collection
  ;; ran, stdin was at EOF.  This regex-free port restores correctness.
  (defun anvil-server--batch-read-framed-with-prefix (first-header-line)
    "Phase B5 Stage 1b override — regex-free framed reader.

Body bytes are pulled via `emacs-stdio-read-bytes' (Phase B6 fix,
2026-05-10) because the MCP wire format does not insert a newline
between body and the next frame's header — line-based reads would
consume past the body's Content-Length boundary and discard the
head of the next header line."
    (let ((header-lines (list (anvil-server--strip-trailing-cr first-header-line)))
          (seen-blank nil))
      (catch 'done
        (while (not seen-blank)
          (let ((line (ignore-errors (read-from-minibuffer ""))))
            (cond
             ((null line) (throw 'done nil))
             ((string-empty-p (anvil-server--strip-trailing-cr line))
              (setq seen-blank t))
             (t (push (anvil-server--strip-trailing-cr line)
                      header-lines))))))
      (let* ((header-block (mapconcat #'identity
                                      (nreverse header-lines) "\r\n"))
             (n (anvil-server-mcp-parse-content-length-header header-block)))
        (when (and n (>= n 0))
          (let ((body (and (fboundp 'emacs-stdio-read-bytes)
                           (emacs-stdio-read-bytes n))))
            (and body (> (length body) 0) body))))))

  (defun anvil-server--batch-read-framed-message ()
    "Phase B5 Stage 1b override — used on subsequent loop iterations.
Reads the first header line, then delegates to the framed-with-prefix
reader."
    (let ((first (ignore-errors (read-from-minibuffer ""))))
      (cond
       ((null first) nil)
       (t (anvil-server--batch-read-framed-with-prefix first)))))

  (defun anvil-server--batch-skip-blank-lines ()
    "Phase B5 Stage 1b override — drain blanks until non-blank line.
Uses CR-strip to recognise lines that are CRLF artefacts as blank."
    (let ((line ""))
      (while (and (stringp line)
                  (string-empty-p (anvil-server--strip-trailing-cr line)))
        (setq line (ignore-errors (read-from-minibuffer ""))))
      line))

  ;; Optional tool-module load+enable chain.  `ANVIL_TOOL_MODULES' is a
  ;; comma-separated list of anvil-* module basenames (e.g.
  ;; "anvil-discovery,anvil-sqlite,anvil-bench") that the driver should
  ;; load from ANVIL_EL_DIR and call <name>-enable on, so their MCP tool
  ;; registrations land in the active server registry before the stdio
  ;; loop begins.  GREEN-bucket modules per the standalone gap analysis
  ;; (= no Emacs-only deps) are the safe targets for now.
  ;; Default = GREEN-bucket subset of anvil-* modules verified to
  ;; load+enable on the standalone substrate.  `anvil-bench' was
  ;; originally GREEN per the gap analysis but pulls `subr-x' /
  ;; `benchmark' / `profiler' that the standalone doesn't ship — kept
  ;; out of the default until those polyfill stubs land.
  ;; ANVIL_TOOL_MODULES env var is honoured when NeLisp's
  ;; process-environment stops being a stub-empty (Phase 1.6 keeps it
  ;; nil so getenv returns nil regardless of OS env), but for now the
  ;; default carries the actual list.
  (let* ((modules-env (anvil-runtime-shell--env
                       "ANVIL_TOOL_MODULES"
                       (concat
                        "anvil-discovery,anvil-sqlite,anvil-bench,"
                        "anvil-state,anvil-memory,anvil-worklog,"
                        "anvil-org-index"))))
    (when (fboundp 'nelisp--write-stderr-line)
      (nelisp--write-stderr-line
       (concat "[shell-loop] ANVIL_TOOL_MODULES="
               (if (> (length modules-env) 0) modules-env "<empty>"))))
    (when (> (length modules-env) 0)
      (dolist (name (split-string modules-env "," t))
        (let* ((trimmed (if (fboundp 'string-trim) (string-trim name) name))
               (file (concat anvil-el-dir "/" trimmed ".el"))
               (enable-sym (intern (concat trimmed "-enable"))))
          (when (fboundp 'nelisp--write-stderr-line)
            (nelisp--write-stderr-line
             (concat "[shell-loop] loading " file)))
          ;; Wrap each module's load + enable so a single bad module
          ;; (= unmet substrate dep, missing helper) skips instead of
          ;; aborting the whole driver before run-batch-stdio is reached.
          (condition-case err
              (progn
                (load file nil t)
                (when (fboundp 'nelisp--write-stderr-line)
                  (nelisp--write-stderr-line
                   (concat "[shell-loop] " (symbol-name enable-sym)
                           " fboundp=" (if (fboundp enable-sym) "t" "nil"))))
                (when (fboundp enable-sym)
                  (funcall enable-sym)))
            (error
             (when (fboundp 'nelisp--write-stderr-line)
               (nelisp--write-stderr-line
                (concat "[shell-loop] " trimmed
                        " load/enable ERR: " (format "%S" err))))))))))
  ;; Post-load substrate patches (= anvil-sqlite regex compat etc).
  ;; Must run AFTER the tool-module load+enable chain because the
  ;; patches override functions that the modules define.
  (when (fboundp 'anvil-runtime-polyfills-apply-post-load-patches)
    (condition-case err
        (anvil-runtime-polyfills-apply-post-load-patches)
      (error
       (when (fboundp 'nelisp--write-stderr-line)
         (nelisp--write-stderr-line
          (concat "[shell-loop] post-load patches ERR: "
                  (format "%S" err)))))))

  (when (fboundp 'nelisp--write-stderr-line)
    (let ((bucket (and (boundp 'anvil-server--tools)
                       (gethash server-id anvil-server--tools))))
      (nelisp--write-stderr-line
       (concat "[shell-loop] pre-loop registry keys="
               (if bucket
                   (format "%S" (hash-table-keys bucket))
                 "<no-bucket>")))))

  ;; `anvil-server-run-batch-stdio' itself calls `anvil-server-start'
  ;; on entry, so we MUST NOT call it here (= duplicate call signals
  ;; `MCP server is already running').  Just enter the loop.
  (anvil-server-run-batch-stdio server-id))

;;; anvil-runtime-shell-loop.el ends here
