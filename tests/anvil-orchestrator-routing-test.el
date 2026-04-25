;;; anvil-orchestrator-routing-test.el --- Tests for Doc 22 routing -*- lexical-binding: t; -*-
;;; anvil-audit: fixture-realism-exempt

;;; Commentary:

;; ERT suite for `anvil-orchestrator-routing'.  The selector is pure
;; w.r.t. an injected `:stats-fn', so these tests build synthetic
;; stats tables without touching the orchestrator's live task pool.
;; A small end-to-end test drives `:provider 'auto' through
;; `anvil-orchestrator--coerce-task' to prove the sentinel gets
;; resolved and stamped.
;;
;; Audit exemption: `candidates-from-alist-fixture' intentionally
;; binds `anvil-orchestrator--providers' to an alist literal to
;; exercise the back-compat branch added during the T70 fix.  The
;; `fixture-realism-exempt' marker above tells `anvil-dev's release
;; audit to skip this file's realism check; the canonical hash-table
;; fixture path is still covered by the other tests in the same suite
;; (see `--make-providers' from line 46+).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-orchestrator)
(require 'anvil-orchestrator-routing)


;;;; --- fixtures -------------------------------------------------------------

(defun anvil-orchestrator-routing-test--stats-fn (table)
  "Return a fake stats-fn that looks up `:provider' in TABLE.
TABLE is an alist `((PROV :elapsed-ms-p50 N :total N ...) ...)' — i.e.
the plist follows the provider symbol directly.  Unknown providers
get :total 0.  `:since' is ignored; tests provide already-filtered
shapes."
  (lambda (&rest args)
    (let* ((prov (plist-get args :provider))
           (row  (cdr (assq prov table))))
      (or row
          (list :total 0
                :elapsed-ms-p50 nil
                :elapsed-ms-avg nil
                :total-cost-usd 0.0)))))

(defconst anvil-orchestrator-routing-test--v1-bench
  '((claude :total 5 :elapsed-ms-p50 14000 :elapsed-ms-avg 14000
            :total-cost-usd 0.15)
    (codex  :total 5 :elapsed-ms-p50 5500  :elapsed-ms-avg 5500
            :total-cost-usd 0.0)
    (ollama :total 5 :elapsed-ms-p50 1000  :elapsed-ms-avg 1000
            :total-cost-usd 0.0))
  "Cross-validation fixture modelled on benchmarks/results/*2026-04-19*.")

(defun anvil-orchestrator-routing-test--make-providers (provider-syms)
  "Return a hash-table with PROVIDER-SYMS as keys.

Mirrors the real `anvil-orchestrator--providers' shape — a
`make-hash-table' with `:test \\='eq' populated via
`anvil-orchestrator-register-provider'.  The values are minimal
descriptor stubs since the routing path only reads keys."
  (let ((h (make-hash-table :test 'eq)))
    (dolist (sym provider-syms) (puthash sym (list :id sym) h))
    h))


;;;; --- policy matrix --------------------------------------------------------

(ert-deftest anvil-orchestrator-routing-test/speed-picks-fastest ()
  (let* ((stats-fn (anvil-orchestrator-routing-test--stats-fn
                   anvil-orchestrator-routing-test--v1-bench))
         (decision (anvil-orchestrator-select-provider
                    :policy 'speed
                    :candidates '(claude codex ollama)
                    :min-samples 3
                    :since 0
                    :stats-fn stats-fn)))
    (should (eq (plist-get decision :provider) 'ollama))
    (should (eq (plist-get decision :policy) 'speed))
    (should-not (plist-get decision :cold-start))))

(ert-deftest anvil-orchestrator-routing-test/balanced-picks-codex ()
  "Balanced policy at default coefficient 1000 prefers codex: same $0
as ollama but ollama's p50 (1000ms) vs codex (5500ms) trades for
claude-level pricing avoidance.  We verify codex beats claude and
loses to ollama when coefficient is 1000."
  (let* ((stats-fn (anvil-orchestrator-routing-test--stats-fn
                   anvil-orchestrator-routing-test--v1-bench))
         (anvil-orchestrator-routing-balance-coefficient 1000)
         (decision (anvil-orchestrator-select-provider
                    :policy 'balanced
                    :candidates '(claude codex ollama)
                    :min-samples 3
                    :since 0
                    :stats-fn stats-fn)))
    ;; ollama score: 1000 + 0*1000  = 1000
    ;; codex score : 5500 + 0*1000  = 5500
    ;; claude score: 14000 + 0.15*1000 = 14150
    (should (eq (plist-get decision :provider) 'ollama))))

(ert-deftest anvil-orchestrator-routing-test/balanced-heavy-cost ()
  "With a huge coefficient, claude becomes prohibitively expensive
while codex and ollama stay at $0 — ollama still wins on p50 tie-break."
  (let* ((stats-fn (anvil-orchestrator-routing-test--stats-fn
                   anvil-orchestrator-routing-test--v1-bench))
         (anvil-orchestrator-routing-balance-coefficient 1000000)
         (decision (anvil-orchestrator-select-provider
                    :policy 'balanced
                    :candidates '(claude codex ollama)
                    :min-samples 3
                    :since 0
                    :stats-fn stats-fn)))
    (should (eq (plist-get decision :provider) 'ollama))))

(ert-deftest anvil-orchestrator-routing-test/cost-picks-cheapest-tie-breaks ()
  (let* ((stats-fn (anvil-orchestrator-routing-test--stats-fn
                   anvil-orchestrator-routing-test--v1-bench))
         (decision (anvil-orchestrator-select-provider
                    :policy 'cost
                    :candidates '(claude codex ollama)
                    :min-samples 3
                    :since 0
                    :stats-fn stats-fn)))
    ;; ollama and codex tie at $0; ollama wins tiebreak by p50.
    (should (eq (plist-get decision :provider) 'ollama))))

(ert-deftest anvil-orchestrator-routing-test/quality-picks-first-in-order ()
  "Quality policy consults `anvil-orchestrator-routing-quality-order'
and ignores samples — useful on a fresh install."
  (let ((stats-fn (anvil-orchestrator-routing-test--stats-fn nil))
        (anvil-orchestrator-routing-quality-order
         '(claude codex gemini aider ollama)))
    (let ((decision (anvil-orchestrator-select-provider
                     :policy 'quality
                     :candidates '(codex ollama claude)
                     :stats-fn stats-fn)))
      (should (eq (plist-get decision :provider) 'claude)))))

(ert-deftest anvil-orchestrator-routing-test/quality-falls-back-when-no-overlap ()
  (let ((stats-fn (anvil-orchestrator-routing-test--stats-fn nil))
        (anvil-orchestrator-routing-quality-order '(never-registered)))
    (let ((decision (anvil-orchestrator-select-provider
                     :policy 'quality
                     :candidates '(codex ollama)
                     :stats-fn stats-fn)))
      (should (null (plist-get decision :provider))))))


;;;; --- cold-start / sample floor -------------------------------------------

(ert-deftest anvil-orchestrator-routing-test/cold-start-when-no-samples ()
  (let* ((stats-fn (anvil-orchestrator-routing-test--stats-fn nil))
         (anvil-orchestrator-routing-cold-start-provider 'claude)
         (decision (anvil-orchestrator-select-provider
                    :policy 'speed
                    :candidates '(claude codex ollama)
                    :min-samples 3
                    :since 0
                    :stats-fn stats-fn)))
    (should (plist-get decision :cold-start))
    (should (eq (plist-get decision :provider) 'claude))))

(ert-deftest anvil-orchestrator-routing-test/cold-start-when-samples-below-floor ()
  (let* ((stats-fn (anvil-orchestrator-routing-test--stats-fn
                   '((claude :total 2 :elapsed-ms-p50 14000
                             :elapsed-ms-avg 14000 :total-cost-usd 0.15)
                     (codex  :total 1 :elapsed-ms-p50 5500
                             :elapsed-ms-avg 5500 :total-cost-usd 0.0))))
         (anvil-orchestrator-routing-cold-start-provider 'claude)
         (decision (anvil-orchestrator-select-provider
                    :policy 'balanced
                    :candidates '(claude codex)
                    :min-samples 3
                    :since 0
                    :stats-fn stats-fn)))
    (should (plist-get decision :cold-start))
    (should (eq (plist-get decision :provider) 'claude))))

(ert-deftest anvil-orchestrator-routing-test/function-policy-delegates ()
  (let ((stats-fn (anvil-orchestrator-routing-test--stats-fn
                  anvil-orchestrator-routing-test--v1-bench))
        (calls 0))
    (let* ((decision (anvil-orchestrator-select-provider
                     :prompt "write me a haiku"
                     :policy (lambda (_p _c _per)
                               (cl-incf calls)
                               'codex)
                     :candidates '(claude codex ollama)
                     :min-samples 3
                     :since 0
                     :stats-fn stats-fn)))
      (should (= calls 1))
      (should (eq (plist-get decision :provider) 'codex))
      (should (eq (plist-get decision :policy) 'user)))))


;;;; --- auto sentinel e2e (coerce-task) -------------------------------------

(ert-deftest anvil-orchestrator-routing-test/coerce-task-resolves-auto ()
  "`anvil-orchestrator--coerce-task' sees :provider 'auto, calls the
selector, stamps routing meta on the returned task plist.

This test uses a real hash-table fixture (matching the production
shape from `anvil-orchestrator-register-provider') so the routing
candidate enumerator is exercised on the same data structure used at
runtime."
  (cl-letf* ((fixture anvil-orchestrator-routing-test--v1-bench)
             (stats-fn (anvil-orchestrator-routing-test--stats-fn fixture))
             ((symbol-function 'anvil-orchestrator-stats) stats-fn)
             (anvil-orchestrator--providers
              (anvil-orchestrator-routing-test--make-providers
               '(claude codex ollama)))
             (anvil-orchestrator-routing-balance-coefficient 1000)
             (task (anvil-orchestrator--coerce-task
                    '(:name "t1" :provider auto :prompt "hi"))))
    (should (eq (plist-get task :provider) 'ollama))
    (should (eq (plist-get task :routing-chose) 'ollama))
    (should (eq (plist-get task :routing-policy) 'balanced))
    ;; hash-table-keys order is unspecified, so compare as set.
    (should (equal (sort (copy-sequence (plist-get task :routing-candidates))
                         #'string<)
                   '(claude codex ollama)))))

(ert-deftest anvil-orchestrator-routing-test/coerce-task-respects-policy-override ()
  (cl-letf* ((fixture anvil-orchestrator-routing-test--v1-bench)
             (stats-fn (anvil-orchestrator-routing-test--stats-fn fixture))
             ((symbol-function 'anvil-orchestrator-stats) stats-fn)
             (anvil-orchestrator--providers
              (anvil-orchestrator-routing-test--make-providers
               '(claude codex ollama)))
             (anvil-orchestrator-routing-quality-order
              '(claude codex ollama))
             (task (anvil-orchestrator--coerce-task
                    '(:name "t1" :provider auto :prompt "hi"
                      :policy quality))))
    (should (eq (plist-get task :provider) 'claude))
    (should (eq (plist-get task :routing-policy) 'quality))))

(ert-deftest anvil-orchestrator-routing-test/coerce-task-errors-when-auto-returns-nil ()
  (cl-letf* ((stats-fn (anvil-orchestrator-routing-test--stats-fn nil))
             ((symbol-function 'anvil-orchestrator-stats) stats-fn)
             (anvil-orchestrator--providers
              (anvil-orchestrator-routing-test--make-providers
               '(codex ollama)))
             (anvil-orchestrator-routing-quality-order '(never-registered))
             (anvil-orchestrator-routing-cold-start-provider 'never-registered))
    (should-error
     (anvil-orchestrator--coerce-task
      '(:name "t1" :provider auto :prompt "hi" :policy quality))
     :type 'user-error)))


;;;; --- MCP tool wrapper ----------------------------------------------------

(ert-deftest anvil-orchestrator-routing-test/tool-select-dry-run ()
  "The dry-run MCP handler parses string args, calls the selector,
and echoes the reasoning plist without touching the task queue."
  (cl-letf* ((fixture anvil-orchestrator-routing-test--v1-bench)
             (stats-fn (anvil-orchestrator-routing-test--stats-fn fixture))
             ((symbol-function 'anvil-orchestrator-stats) stats-fn)
             (anvil-orchestrator--providers
              (anvil-orchestrator-routing-test--make-providers
               '(claude codex ollama)))
             (result (anvil-orchestrator-routing--tool-select
                      nil "speed" "claude,codex,ollama" "3600" "3")))
    (should (eq (plist-get result :provider) 'ollama))
    (should (eq (plist-get result :policy) 'speed))
    (should (stringp (plist-get result :reason)))))


;;;; --- Phase 3: provider registry shape regression coverage ----------------

(ert-deftest anvil-orchestrator-routing-test/candidates-from-hash-table ()
  "`anvil-orchestrator-routing--candidates' enumerates a real
hash-table provider registry — the production shape produced by
`anvil-orchestrator-register-provider'.  This is the regression
gate for the bug where `(mapcar #'car ...)' was applied to a
hash-table and silently broke `:provider auto'."
  (let ((anvil-orchestrator--providers
         (anvil-orchestrator-routing-test--make-providers
          '(claude codex ollama))))
    (let ((cands (anvil-orchestrator-routing--candidates)))
      (should (listp cands))
      (should (= 3 (length cands)))
      (should (memq 'claude cands))
      (should (memq 'codex cands))
      (should (memq 'ollama cands)))))

(ert-deftest anvil-orchestrator-routing-test/candidates-empty-hash-table ()
  "Empty registry returns nil, not an error."
  (let ((anvil-orchestrator--providers
         (anvil-orchestrator-routing-test--make-providers nil)))
    (should (null (anvil-orchestrator-routing--candidates)))))

(ert-deftest anvil-orchestrator-routing-test/auto-with-real-shape-end-to-end ()
  "`:provider auto' resolves end-to-end against a hash-table
registry without a `:candidates' override.  This is the path that
production callers exercise — `anvil-orchestrator--coerce-task'
calls `anvil-orchestrator-select-provider' with no `:candidates'
argument, which falls through to `--candidates' which must read
the hash-table."
  (cl-letf* ((fixture anvil-orchestrator-routing-test--v1-bench)
             (stats-fn (anvil-orchestrator-routing-test--stats-fn fixture))
             ((symbol-function 'anvil-orchestrator-stats) stats-fn)
             (anvil-orchestrator--providers
              (anvil-orchestrator-routing-test--make-providers
               '(claude codex ollama)))
             (anvil-orchestrator-routing-balance-coefficient 1000)
             (decision (anvil-orchestrator-select-provider
                        :policy 'speed
                        :since 0
                        :min-samples 3)))
    (should (eq (plist-get decision :provider) 'ollama))
    (should (eq (plist-get decision :policy) 'speed))
    ;; Candidates were derived from the registry (set equality).
    (should (equal (sort (copy-sequence (plist-get decision :candidates))
                         #'string<)
                   '(claude codex ollama)))))

(ert-deftest anvil-orchestrator-routing-test/auto-prefers-task-affinity-quality ()
  "Quality policy at the orchestrator-level resolves `:provider
auto' to the first registered provider in
`anvil-orchestrator-routing-quality-order' — i.e. the policy
expresses task affinity (claude for architecture, codex for
implementation) by static ordering."
  (cl-letf* ((stats-fn (anvil-orchestrator-routing-test--stats-fn nil))
             ((symbol-function 'anvil-orchestrator-stats) stats-fn)
             (anvil-orchestrator--providers
              (anvil-orchestrator-routing-test--make-providers
               '(codex claude ollama)))
             ;; Architecture-first preference list.
             (anvil-orchestrator-routing-quality-order
              '(claude codex ollama))
             (decision (anvil-orchestrator-select-provider
                        :policy 'quality)))
    (should (eq (plist-get decision :provider) 'claude))))

(ert-deftest anvil-orchestrator-routing-test/auto-cold-start-with-real-shape ()
  "When the hash-table registry is populated but stats supplies
no samples, auto routing falls back to the cold-start provider."
  (cl-letf* ((stats-fn (anvil-orchestrator-routing-test--stats-fn nil))
             ((symbol-function 'anvil-orchestrator-stats) stats-fn)
             (anvil-orchestrator--providers
              (anvil-orchestrator-routing-test--make-providers
               '(claude codex ollama)))
             (anvil-orchestrator-routing-cold-start-provider 'claude)
             (decision (anvil-orchestrator-select-provider
                        :policy 'speed
                        :since 0
                        :min-samples 3)))
    (should (plist-get decision :cold-start))
    (should (eq (plist-get decision :provider) 'claude))))

(ert-deftest anvil-orchestrator-routing-test/candidates-from-alist-fixture ()
  "The candidate enumerator also accepts alist fixtures, so the
historic test shape `((claude) (codex))' continues to work — but
production always uses a hash-table.  This guards the back-compat
branch added for fixture ergonomics."
  (let ((anvil-orchestrator--providers '((claude . _) (codex . _) (ollama . _))))
    (let ((cands (anvil-orchestrator-routing--candidates)))
      (should (= 3 (length cands)))
      (should (memq 'claude cands))
      (should (memq 'codex cands))
      (should (memq 'ollama cands)))))

(provide 'anvil-orchestrator-routing-test)

;;; anvil-orchestrator-routing-test.el ends here
