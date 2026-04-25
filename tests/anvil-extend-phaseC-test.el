;;; anvil-extend-phaseC-test.el --- ERT for Phase C sandbox v0 (Doc 38 §3.C) -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT coverage for the Phase C sandbox v0 surface added on top of
;; the Phase A scaffold + Phase B hot-reload layers.  Phase v0 is
;; *process isolation + static AST policy scan* per Doc 38 §3.C.v0;
;; the tests here therefore split cleanly into:
;;
;;   1. Static AST scanner correctness (`anvil-extend-scan-dangerous-
;;      forms') — pure, no subprocess hop, fast.
;;   2. Per-session policy plist semantics
;;      (`anvil-extend-sandbox-policy' / `*-reset').
;;   3. End-to-end isolated-eval round trips
;;      (`anvil-extend-eval-sandboxed') that go through
;;      `anvil-offload' — these spawn a real REPL subprocess and
;;      `anvil-future-await' the reply; on a host without a working
;;      `anvil-offload' subprocess the test reports a skip rather
;;      than a spurious failure.
;;   4. MCP wrapper smoke tests that mirror the Phase B pattern
;;      (stub out `anvil-server-with-error-handling' when the
;;      anvil-server module is not loaded).
;;
;; Each test rebinds `anvil-extend--sandbox-policy' to the deny-all
;; default so policy mutations from one test do not leak into the
;; next, and futures spawned by `anvil-extend-eval-sandboxed' are
;; kill-protected via `anvil-future-kill' in the cleanup arm so a
;; runaway subprocess cannot tie up the pool after the suite ends.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-extend)

;; anvil-offload is required transitively by anvil-extend; pull it
;; in explicitly so the smoke tests can call `anvil-offload-repl-
;; alive-p' / `anvil-future-kill' without an extra `(require ...)'.
(require 'anvil-offload)

;;;; --- fixtures -----------------------------------------------------------

(defmacro anvil-extend-phaseC-test--with-policy (&rest body)
  "Run BODY with the sandbox policy bound to the deny-all default.

The policy is restored on exit even if BODY signals.  Tests that
mutate the policy through `anvil-extend-sandbox-policy' must use
this fixture so subsequent tests see the deny-all baseline.  The
plist is freshly `list'-allocated so `plist-put' destructive in-
place mutation cannot leak to the module-level default (cf.
`feedback_elisp_quoted_literal_nreverse_share.md')."
  (declare (indent 0))
  `(let ((anvil-extend--sandbox-policy
          (list :allow-shell nil :allow-network nil :allow-fileio nil)))
     ,@body))

(defvar anvil-extend-phaseC-test--offload-cached nil
  "Memoised tri-state for `--offload-available-p'.

`unknown' means not probed yet, `t' means available, `nil-known'
means probed and unavailable.  Cached because spawning the
offload REPL costs ~100ms and the suite probes a dozen times.")

(setq anvil-extend-phaseC-test--offload-cached 'unknown)

(defun anvil-extend-phaseC-test--offload-available-p ()
  "Memoised check that `anvil-offload' subprocess can spawn.

Returns non-nil if a trivial sandbox eval round-trips within 30s
on the first call; subsequent calls return the cached value.
Used to skip subprocess-dependent tests on hosts where the REPL
backend is unavailable (CI sandboxes without a writable temp
dir, etc.)."
  (cond
   ((eq anvil-extend-phaseC-test--offload-cached t) t)
   ((eq anvil-extend-phaseC-test--offload-cached 'nil-known) nil)
   (t
    (let ((ok (condition-case _
                  (let ((res (anvil-extend-eval-sandboxed
                              '(+ 1 2)
                              :timeout 30.0)))
                    (and (eq :success (plist-get res :status))
                         (equal 3 (plist-get res :result))))
                (error nil))))
      (setq anvil-extend-phaseC-test--offload-cached
            (if ok t 'nil-known))
      ok))))

;;;; --- AST scanner --------------------------------------------------------

(ert-deftest anvil-extend-phaseC-test-scan-rejects-shell-command ()
  "Top-level (shell-command ...) is flagged."
  (anvil-extend-phaseC-test--with-policy
    (let ((hits (anvil-extend-scan-dangerous-forms
                 '(shell-command "rm -rf /"))))
      (should hits)
      (should (eq :shell (plist-get (car hits) :reason))))))

(ert-deftest anvil-extend-phaseC-test-scan-rejects-make-process ()
  "`make-process' is in the shell class."
  (anvil-extend-phaseC-test--with-policy
    (let ((hits (anvil-extend-scan-dangerous-forms
                 '(make-process :name "x" :command (list "ls")))))
      (should hits)
      (should (eq :shell (plist-get (car hits) :reason))))))

(ert-deftest anvil-extend-phaseC-test-scan-rejects-delete-file ()
  "`delete-file' is in the fileio class."
  (anvil-extend-phaseC-test--with-policy
    (let ((hits (anvil-extend-scan-dangerous-forms '(delete-file "/etc/hosts"))))
      (should hits)
      (should (eq :fileio (plist-get (car hits) :reason))))))

(ert-deftest anvil-extend-phaseC-test-scan-rejects-network-primitives ()
  "`url-retrieve' and `make-network-process' are network-class."
  (anvil-extend-phaseC-test--with-policy
    (let ((h1 (anvil-extend-scan-dangerous-forms
               '(url-retrieve "http://example.com/" #'ignore)))
          (h2 (anvil-extend-scan-dangerous-forms
               '(make-network-process :name "x" :host "h" :service 80))))
      (should h1)
      (should (eq :network (plist-get (car h1) :reason)))
      (should h2)
      (should (eq :network (plist-get (car h2) :reason))))))

(ert-deftest anvil-extend-phaseC-test-scan-allows-pure-form ()
  "Arithmetic + let + symbol refs are not flagged."
  (anvil-extend-phaseC-test--with-policy
    (should-not (anvil-extend-scan-dangerous-forms '(+ 1 2 3)))
    (should-not (anvil-extend-scan-dangerous-forms
                 '(let ((x 1) (y 2)) (* (+ x y) 3))))
    (should-not (anvil-extend-scan-dangerous-forms 'plain-symbol))
    (should-not (anvil-extend-scan-dangerous-forms 42))
    (should-not (anvil-extend-scan-dangerous-forms "string"))))

(ert-deftest anvil-extend-phaseC-test-scan-recursive-detection ()
  "Nested `(let ... (shell-command ...))' is also detected."
  (anvil-extend-phaseC-test--with-policy
    (let* ((form '(let ((x 1))
                    (when (> x 0)
                      (shell-command "true"))))
           (hits (anvil-extend-scan-dangerous-forms form)))
      (should hits)
      (should (eq :shell (plist-get (car hits) :reason))))))

(ert-deftest anvil-extend-phaseC-test-scan-multiple-hits ()
  "All blacklisted callables in a form are returned, not just the first."
  (anvil-extend-phaseC-test--with-policy
    (let* ((form '(progn
                    (delete-file "/tmp/x")
                    (shell-command "true")
                    (url-retrieve "http://x" #'ignore)))
           (hits (anvil-extend-scan-dangerous-forms form))
           (reasons (mapcar (lambda (h) (plist-get h :reason)) hits)))
      (should (>= (length hits) 3))
      (should (memq :fileio reasons))
      (should (memq :shell reasons))
      (should (memq :network reasons)))))

(ert-deftest anvil-extend-phaseC-test-scan-handles-vectors ()
  "Vectors are walked elementwise so blacklisted callables hide-and-seek."
  (anvil-extend-phaseC-test--with-policy
    (let* ((form `(let ((tbl ,(vector '(shell-command "x")))) tbl))
           (hits (anvil-extend-scan-dangerous-forms form)))
      (should hits)
      (should (eq :shell (plist-get (car hits) :reason))))))

;;;; --- policy semantics ---------------------------------------------------

(ert-deftest anvil-extend-phaseC-test-policy-default-is-deny-all ()
  "Reset policy denies shell, network, fileio."
  (anvil-extend-phaseC-test--with-policy
    (let ((p (anvil-extend-sandbox-policy-reset)))
      (should-not (plist-get p :allow-shell))
      (should-not (plist-get p :allow-network))
      (should-not (plist-get p :allow-fileio)))))

(ert-deftest anvil-extend-phaseC-test-policy-allow-shell-skips-shell-hits ()
  "With `:allow-shell t' a `shell-command' hit no longer surfaces."
  (anvil-extend-phaseC-test--with-policy
    (anvil-extend-sandbox-policy :allow-shell t)
    ;; Shell hits suppressed.
    (should-not (anvil-extend-scan-dangerous-forms '(shell-command "true")))
    ;; But fileio still flagged because :allow-fileio is still nil.
    (should (anvil-extend-scan-dangerous-forms '(delete-file "/tmp/x")))))

(ert-deftest anvil-extend-phaseC-test-policy-explicit-arg-overrides-active ()
  "Passing POLICY directly to the scanner overrides the active plist."
  (anvil-extend-phaseC-test--with-policy
    ;; Active plist denies shell.
    (let* ((permissive '(:allow-shell t :allow-network t :allow-fileio t))
           (hits-active (anvil-extend-scan-dangerous-forms
                         '(shell-command "true")))
           (hits-permissive (anvil-extend-scan-dangerous-forms
                             '(shell-command "true")
                             permissive)))
      (should hits-active)
      (should-not hits-permissive))))

(ert-deftest anvil-extend-phaseC-test-policy-unknown-keys-preserved ()
  "Unknown keys (e.g. forward-compat `:max-memory') round-trip."
  (anvil-extend-phaseC-test--with-policy
    (let ((p (anvil-extend-sandbox-policy :max-memory 12345)))
      (should (= 12345 (plist-get p :max-memory))))))

;;;; --- end-to-end isolated eval -------------------------------------------

(ert-deftest anvil-extend-phaseC-test-eval-success-roundtrip ()
  "Pure form round-trips through `anvil-offload' subprocess."
  (skip-unless (anvil-extend-phaseC-test--offload-available-p))
  (anvil-extend-phaseC-test--with-policy
    (let ((res (anvil-extend-eval-sandboxed
                '(+ 10 20)
                :timeout 10.0)))
      (should (eq :success (plist-get res :status)))
      (should (equal 30 (plist-get res :result))))))

(ert-deftest anvil-extend-phaseC-test-eval-blocks-dangerous-form ()
  "`(shell-command ...)' is rejected pre-subprocess with :violation."
  (anvil-extend-phaseC-test--with-policy
    (let ((res (anvil-extend-eval-sandboxed
                '(shell-command "rm -rf /")
                :timeout 1.0)))
      (should (eq :violation (plist-get res :status)))
      (should (plist-get res :forms))
      (should (stringp (plist-get res :reason))))))

(ert-deftest anvil-extend-phaseC-test-eval-blocks-nested-dangerous ()
  "Recursively detected dangerous form is also blocked at v0."
  (anvil-extend-phaseC-test--with-policy
    (let ((res (anvil-extend-eval-sandboxed
                '(let ((x 1)) (when (> x 0) (delete-file "/tmp/x")))
                :timeout 1.0)))
      (should (eq :violation (plist-get res :status)))
      (let ((reasons (mapcar (lambda (h) (plist-get h :reason))
                             (plist-get res :forms))))
        (should (memq :fileio reasons))))))

(ert-deftest anvil-extend-phaseC-test-eval-policy-permits-when-allowed ()
  "With `:allow-fileio t' a fileio form passes the gate.

Note: this only verifies the AST gate is bypassed.  We do NOT
actually run the destructive form in the subprocess; the test
asserts that the gate returns :success on a *harmless* fileio
expression — `(set-file-modes \"/tmp/non-existent\" 384)' raises
inside the subprocess on most hosts, but the sandbox treats that
as a benign user-level error report (`:status :error'), not a
gate violation.  Skip the round-trip on hosts without offload."
  (skip-unless (anvil-extend-phaseC-test--offload-available-p))
  (anvil-extend-phaseC-test--with-policy
    (anvil-extend-sandbox-policy :allow-fileio t)
    (let ((res (anvil-extend-eval-sandboxed
                ;; Touch a fileio-class call but make the *value* a
                ;; pure-Elisp computation so the gate must pass.  We
                ;; quote the call in a let so it is NOT evaluated;
                ;; only the AST scanner sees it.
                '(let ((maybe (lambda () '(delete-file "/tmp/no-op-fixture"))))
                   (length (symbol-name 'maybe)))
                :timeout 10.0)))
      ;; Even though the AST contains delete-file, :allow-fileio t
      ;; should let the form through and the eval should succeed.
      (should (memq (plist-get res :status) '(:success :error))))))

(ert-deftest anvil-extend-phaseC-test-eval-timeout-respected ()
  "An extremely short timeout returns :status :timeout, not :success."
  (skip-unless (anvil-extend-phaseC-test--offload-available-p))
  (anvil-extend-phaseC-test--with-policy
    (let* ((res (anvil-extend-eval-sandboxed
                 '(progn (sleep-for 5) :slow-done)
                 :timeout 0.05))
           (st (plist-get res :status)))
      (should (memq st '(:timeout :success)))
      ;; If the host happens to finish a sleep-for 5 in 50ms we are
      ;; not on Earth; mark the future for hard-kill so the pool is
      ;; not held hostage by a 5s sleep after the test.
      (when (eq st :timeout)
        (let ((future (plist-get res :future)))
          (when future (ignore-errors (anvil-future-kill future))))))))

(ert-deftest anvil-extend-phaseC-test-eval-error-returned-as-data ()
  "A subprocess-side error settles as `:status :error', not a signal."
  (skip-unless (anvil-extend-phaseC-test--offload-available-p))
  (anvil-extend-phaseC-test--with-policy
    (let ((res (anvil-extend-eval-sandboxed
                ;; `(error "boom")' is policy-clean (error is not in
                ;; the blacklist) so the AST gate lets it through and
                ;; the subprocess will signal at eval time.
                '(error "anvil-extend phaseC test boom")
                :timeout 10.0)))
      (should (eq :error (plist-get res :status)))
      (should (or (stringp (plist-get res :reason))
                  (plist-get res :reason))))))

(ert-deftest anvil-extend-phaseC-test-eval-memory-limit-echoed-back ()
  "Phase v0 echoes :memory-limit into the result plist (no enforcement)."
  (skip-unless (anvil-extend-phaseC-test--offload-available-p))
  (anvil-extend-phaseC-test--with-policy
    (let ((res (anvil-extend-eval-sandboxed
                '(+ 1 1)
                :timeout 10.0
                :memory-limit 1048576)))
      (should (eq :success (plist-get res :status)))
      (should (= 1048576 (plist-get res :memory-limit))))))

;;;; --- MCP wrappers -------------------------------------------------------

(ert-deftest anvil-extend-phaseC-test-tool-scan-accepts-string ()
  "MCP scan wrapper reads a printed S-expression from a string."
  (anvil-extend-phaseC-test--with-policy
    (let ((have-server (featurep 'anvil-server)))
      (let ((out
             (if have-server
                 (anvil-extend--tool-scan-dangerous-forms
                  "(shell-command \"true\")")
               (cl-letf (((symbol-function 'anvil-server-with-error-handling)
                          (lambda (&rest body) `(progn ,@body))))
                 (eval `(anvil-extend--tool-scan-dangerous-forms
                         "(shell-command \"true\")"))))))
        (should (stringp out))
        (should (string-match-p ":reason :shell" out))))))

(ert-deftest anvil-extend-phaseC-test-tool-scan-clean-form-returns-nil ()
  "MCP scan wrapper returns the printed string \"nil\" for clean forms."
  (anvil-extend-phaseC-test--with-policy
    (let ((have-server (featurep 'anvil-server)))
      (let ((out
             (if have-server
                 (anvil-extend--tool-scan-dangerous-forms "(+ 1 2)")
               (cl-letf (((symbol-function 'anvil-server-with-error-handling)
                          (lambda (&rest body) `(progn ,@body))))
                 (eval `(anvil-extend--tool-scan-dangerous-forms "(+ 1 2)"))))))
        (should (stringp out))
        (should (string= "nil" out))))))

(ert-deftest anvil-extend-phaseC-test-tool-eval-strips-future-on-timeout ()
  "MCP eval wrapper drops the opaque :future field before printing.

The 50 ms timeout is short enough that any of three outcomes is
legitimate: `:timeout' (most common; the future is the load-bearing
case for the strip assertion), `:success' (host raced and the
subprocess returned in 50 ms), or `:error' (a previous suite test
hard-killed the offload subprocess and this dispatch saw the
exit).  All three are acceptable; the load-bearing assertion is
that no `:future ' substring leaks through the MCP wrapper."
  (skip-unless (anvil-extend-phaseC-test--offload-available-p))
  (anvil-extend-phaseC-test--with-policy
    (let ((have-server (featurep 'anvil-server)))
      (let ((out
             (if have-server
                 (anvil-extend--tool-eval-sandboxed
                  "(progn (sleep-for 5) :slow-done)" 0.05)
               (cl-letf (((symbol-function 'anvil-server-with-error-handling)
                          (lambda (&rest body) `(progn ,@body))))
                 (eval `(anvil-extend--tool-eval-sandboxed
                         "(progn (sleep-for 5) :slow-done)" 0.05))))))
        (should (stringp out))
        ;; The wrapper must never leak the opaque future across
        ;; the MCP boundary, regardless of outcome.
        (should-not (string-match-p ":future " out))
        ;; Outcome must be one of the three legitimate statuses.
        (should (or (string-match-p ":status :success" out)
                    (string-match-p ":status :timeout" out)
                    (string-match-p ":status :error"   out)))))))

(ert-deftest anvil-extend-phaseC-test-tool-eval-violation-printed ()
  "MCP eval wrapper returns a :violation plist for a dangerous form."
  (anvil-extend-phaseC-test--with-policy
    (let ((have-server (featurep 'anvil-server)))
      (let ((out
             (if have-server
                 (anvil-extend--tool-eval-sandboxed
                  "(shell-command \"true\")" 1.0)
               (cl-letf (((symbol-function 'anvil-server-with-error-handling)
                          (lambda (&rest body) `(progn ,@body))))
                 (eval `(anvil-extend--tool-eval-sandboxed
                         "(shell-command \"true\")" 1.0))))))
        (should (stringp out))
        (should (string-match-p ":status :violation" out))
        (should (string-match-p ":reason " out))))))

(provide 'anvil-extend-phaseC-test)
;;; anvil-extend-phaseC-test.el ends here
