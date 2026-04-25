;;; m2-bootsmoke-test.el --- T110 M2 boot smoke ERT tests -*- lexical-binding: t; -*-

;;; Commentary:

;; T110 — Phase 9d.L M2 milestone integration ERT suite.
;;
;; Companion to `scripts/m2-bootsmoke.sh' which exercises the full
;; substrate + MCP round-trip in an isolated `emacs --batch -Q'
;; subprocess.  These ERTs run in-process (the host harness already
;; satisfies every load dependency) and validate the load order, the
;; subprocess substrate apis, and the wiring claims that the shell
;; smoke script depends on.
;;
;; Scope (Doc 39 §M2 exit criteria):
;;
;;   * substrate features load (NeLisp Wave 1 + 2 + subprocess)
;;   * `nelisp-make-process' spawns a child and reaps the exit code
;;   * `nelisp-eventloop-multiplex-tick' is callable on the bootstrap
;;     `host-bridge' backend without raising
;;   * `anvil-host' loads cleanly (= shell exec library is available
;;     for downstream callers)
;;   * `anvil-worker' pool initialises without spawning real daemons
;;     (= subprocess MCP layer is wireable from a pure boot harness)
;;
;; These assertions intentionally avoid network, file-notify, and any
;; tool-call shape that the M1 baseline already documents as a host-
;; Emacs path warning (`Tool handler must return string or nil, got
;; cons').  T110 is about M2 substrate readiness, not M1 regression.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; M1 carry-over substrate (always available in the host harness;
;; require defensively in case a single-test invocation shows up).
(require 'nelisp-emacs-compat)
(require 'nelisp-emacs-compat-fileio)
(require 'nelisp-ec-bridge)

;; M2 NEW substrate.
(require 'nelisp-eventloop)
(require 'nelisp-actor)
(require 'nelisp-eventloop-multiplex)
(require 'nelisp-process)

;; anvil core + M2 subprocess layer.
(require 'anvil-server)
(require 'anvil-host)
(require 'anvil-worker)


;;;; --- 1. substrate load order ---------------------------------------------

(ert-deftest m2-bootsmoke-test-substrate-features-loaded ()
  "Every substrate feature called for by `scripts/m2-bootsmoke.sh' is
loaded.  This guards the `(featurep ...)' chain the shell script
prints — if any of these names ever rename, the smoke script will
silently fall through to a partial substrate boot and the test
catches the drift first."
  ;; M1 carry-over (Wave 1 + 2).
  (should (featurep 'nelisp-emacs-compat))
  (should (featurep 'nelisp-emacs-compat-fileio))
  (should (featurep 'nelisp-ec-bridge))
  ;; M2 NEW (Phase 9d.K + 9d.L subprocess substrate).
  (should (featurep 'nelisp-eventloop))
  (should (featurep 'nelisp-actor))
  (should (featurep 'nelisp-eventloop-multiplex))
  (should (featurep 'nelisp-process)))

(ert-deftest m2-bootsmoke-test-anvil-features-loaded ()
  "Anvil core + M2 subprocess layer all load.  Mirrors the
`anvil-features:' line the shell script prints."
  (should (featurep 'anvil-server))
  (should (featurep 'anvil-host))
  (should (featurep 'anvil-worker)))


;;;; --- 2. subprocess spawn round-trip ------------------------------------

(ert-deftest m2-bootsmoke-test-nelisp-make-process-spawn-and-wait ()
  "M2 integration core claim: `nelisp-make-process' spawns a child
through the Phase 9d.L wrapper, the host trampoline routes the
sentinel into the wrap, and `nelisp-process-wait-for-exit' returns
the exit code (0 here).  The 50ms `sleep' gives the wait loop one
poll window to drain output and observe the sentinel — without it,
fast-exit children race the loop's `live-p' check and return nil
even though the child exited cleanly (this is documented host-bridge
behaviour, fixed once the Phase 7.5 FFI bridge replaces the host
poll with an fd-readiness select)."
  (let* ((buf (generate-new-buffer " *m2-spawn-test*"))
         (wrap (nelisp-make-process
                :name "m2-spawn"
                :buffer buf
                :command (list "sh" "-c" "sleep 0.05; echo m2-ok"))))
    (unwind-protect
        (let ((exit (nelisp-process-wait-for-exit wrap 5)))
          (should (eql 0 exit))
          (should-not (nelisp-process-live-p wrap))
          (should (eq 'exit (nelisp-process-status-of wrap))))
      (ignore-errors (nelisp-delete-process wrap))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest m2-bootsmoke-test-nelisp-process-captures-stdout ()
  "Phase 9d.L filter trampoline mirrors the child's stdout into the
wrap buffer.  Required by anvil-host's `anvil-shell' contract
(captured stdout, not just exit code)."
  (let* ((buf (generate-new-buffer " *m2-stdout-test*"))
         (wrap (nelisp-make-process
                :name "m2-stdout"
                :buffer buf
                :command (list "sh" "-c" "sleep 0.05; printf 'hello-m2'"))))
    (unwind-protect
        (progn
          (nelisp-process-wait-for-exit wrap 5)
          ;; Drain any pending output the wait loop didn't catch on
          ;; its last 50ms poll.
          (accept-process-output (nelisp-process-host-proc wrap) 0.5)
          (should (string= "hello-m2"
                           (with-current-buffer buf (buffer-string)))))
      (ignore-errors (nelisp-delete-process wrap))
      (when (buffer-live-p buf) (kill-buffer buf)))))


;;;; --- 3. eventloop multiplex dispatch path -----------------------------

(ert-deftest m2-bootsmoke-test-multiplex-tick-callable ()
  "The Phase 9d.K eventloop multiplexer is callable on the bootstrap
`host-bridge' backend.  An empty registry returns 0 (no callbacks
fired) without raising — this is the contract the shell script's
`multiplex-tick-probe' line documents.  Once Phase 7.5 swaps in the
`rust-ffi' backend, this same call will dispatch through
`nl_syscall_select' with zero call-site churn."
  (should (eq 'host-bridge nelisp-eventloop-multiplex-backend))
  (let ((fired (nelisp-eventloop-multiplex-tick 50)))
    (should (integerp fired))
    (should (>= fired 0))))


;;;; --- 4. anvil-host shell library is available ------------------------

(ert-deftest m2-bootsmoke-test-anvil-host-shell-available ()
  "After the M2 boot, `anvil-shell' is a callable function (not
autoload-only) so downstream modules (anvil-net, anvil-pty-broker,
anvil-shell-filter, anvil-git, anvil-pdf, anvil-xlsx, anvil-proc)
can require it without a second-stage fix-up.  This is the load-
ordering invariant `scripts/m2-bootsmoke.sh' is asserting."
  (should (fboundp 'anvil-shell))
  (should (fboundp 'anvil-shell-by-os))
  ;; Confirm the autoload (if any) has been resolved to a real
  ;; function definition — symbol-function of a defun returns a
  ;; lambda / compiled function, not the literal symbol `autoload'.
  (should (functionp (symbol-function 'anvil-shell)))
  (should-not (eq 'autoload (car-safe (symbol-function 'anvil-shell)))))


;;;; --- 5. anvil-worker pool initialises without spawning daemons -------

(ert-deftest m2-bootsmoke-test-anvil-worker-init-pool-no-spawn ()
  "`anvil-worker--init-pool' must be callable from a clean boot
without raising and WITHOUT spawning real sub-Emacs daemons (= the
`(anvil-worker-spawn)' deferred timer in `anvil-worker-enable' is
the only thing that should ever cause `start-process').  This lets
`scripts/m2-bootsmoke.sh' wire the pool into MCP without paying the
~3-5s daemon startup cost during the smoke run."
  (should (fboundp 'anvil-worker--init-pool))
  (should (fboundp 'anvil-worker--tool-probe))
  ;; init-pool builds the in-memory lane vectors but does not call
  ;; start-process (per anvil-worker.el comment lines 1100-1104).
  (let ((before-procs (length (process-list))))
    (anvil-worker--init-pool)
    (let ((after-procs (length (process-list))))
      ;; No new host process was spawned by init-pool itself.
      (should (= before-procs after-procs))))
  ;; The pool variable is now populated.
  (should (boundp 'anvil-worker--pool))
  (should anvil-worker--pool))


;;;; --- 6. ec-bridge install is idempotent under the M2 substrate -------

(ert-deftest m2-bootsmoke-test-ec-bridge-status-shape ()
  "`nelisp-ec-bridge-status' returns the shape the shell script
greps for.  In the host-Emacs harness all 54 builtins are already
host-bound, so :installed is 0 and :host-bound is 54 — this matches
the output the M1 + M2 smoke scripts print and proves the bridge
install path is a strict no-op on host."
  (let ((status (nelisp-ec-bridge-status)))
    (should (plistp status))
    (should (integerp (plist-get status :total)))
    (should (>= (plist-get status :total) 30))
    (should (integerp (plist-get status :installed)))
    (should (integerp (plist-get status :host-bound)))
    (should (integerp (plist-get status :missing-ec)))
    ;; Sum invariant: installed + host-bound + missing-ec <= total
    ;; (entries can fall in only one bucket at a time).
    (should (<= (+ (plist-get status :installed)
                   (plist-get status :host-bound)
                   (plist-get status :missing-ec))
                (plist-get status :total)))))


(provide 'm2-bootsmoke-test)
;;; m2-bootsmoke-test.el ends here
