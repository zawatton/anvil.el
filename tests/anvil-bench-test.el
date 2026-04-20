;;; anvil-bench-test.el --- Tests for anvil-bench -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 14 Phase 1.  Deterministic CPU-bound fixtures
;; keep the perf tests fast and platform-independent: the slower
;; thunk does ~1000× more integer arithmetic than the faster one, so
;; even noisy machines see a clear ordering.
;;
;; Profiler tests only check the shape of the returned plist — the
;; built-in CPU profiler's sampling is non-deterministic and may
;; return an empty :top list when run inside a very short fixture.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-bench)


;;;; --- fixtures -------------------------------------------------------------

(defun anvil-bench-test--fast-thunk ()
  "Zero-argument thunk doing trivial work."
  (lambda () (+ 1 1)))

(defun anvil-bench-test--slow-thunk ()
  "Zero-argument thunk doing ~1000× more arithmetic than the fast one."
  (lambda ()
    (let ((s 0))
      (dotimes (i 1000)
        (setq s (+ s i)))
      s)))

(defun anvil-bench-test--equal-thunk ()
  "Pair of identical thunks for the tie-detection test.
Non-trivial but small arithmetic keeps the per-call cost above
`float-time' jitter on slower runners."
  (lambda ()
    (let ((s 0))
      (dotimes (i 200)
        (setq s (+ s i)))
      s)))


;;;; --- bench-compare ------------------------------------------------------

(ert-deftest anvil-bench-test/compare-detects-slower-side ()
  "Fast thunk (A) should win over a 1000× heavier slow thunk (B)."
  (let* ((report (anvil-bench-compare
                  (anvil-bench-test--fast-thunk)
                  (anvil-bench-test--slow-thunk)
                  :n 30 :warmup 5 :label-a "fast" :label-b "slow")))
    (should (memq (plist-get report :winner) '(a)))
    (should (> (plist-get report :b-mean-ms)
               (plist-get report :a-mean-ms)))
    (should (> (plist-get report :ratio) 0))
    (should (< (plist-get report :ratio) 1.0))
    (should (equal "fast" (plist-get report :label-a)))
    (should (equal "slow" (plist-get report :label-b)))
    (should (= 30 (plist-get report :n)))
    (should (= 5  (plist-get report :warmup)))
    (should (plist-get report :env))))

(ert-deftest anvil-bench-test/compare-declares-tie-for-identical-thunks ()
  "Two identical thunks should be within the tie threshold.
Threshold is loosened to 1.0 (allows up to 2× noise) because
`float-time' resolution + OS scheduling jitter + GC on CI runners
can easily produce ratios in the 1.3–1.7 range for microsecond
calls, even when the thunks are byte-for-byte identical."
  (let* ((anvil-bench-tie-threshold 1.0)
         (report (anvil-bench-compare
                  (anvil-bench-test--equal-thunk)
                  (anvil-bench-test--equal-thunk)
                  :n 30 :warmup 5)))
    (should (eq (plist-get report :winner) 'tie))
    (should (stringp (plist-get report :speedup)))))

(ert-deftest anvil-bench-test/compare-env-captures-byte-compile-state ()
  "Report :env should include a-byte-compiled / b-byte-compiled flags."
  (let* ((anon (lambda () 1))
         (report (anvil-bench-compare anon anon :n 5 :warmup 1))
         (env (plist-get report :env)))
    (should (plist-member env :a-byte-compiled))
    (should (plist-member env :b-byte-compiled))
    (should (equal emacs-version (plist-get env :emacs-version)))))

(ert-deftest anvil-bench-test/compare-errors-on-non-function ()
  (should-error (anvil-bench-compare "not a function" (lambda () 1))
                :type 'user-error)
  (should-error (anvil-bench-compare (lambda () 1) nil)
                :type 'user-error))


;;;; --- bench-profile ------------------------------------------------------

(ert-deftest anvil-bench-test/profile-returns-shape ()
  "Profile returns a well-shaped plist even when sampling is sparse."
  (let* ((report (anvil-bench-profile
                  (anvil-bench-test--slow-thunk)
                  :duration-sec 0.1 :top 5)))
    (should (plist-member report :top))
    (should (listp (plist-get report :top)))
    (should (<= (length (plist-get report :top)) 5))
    (should (integerp (plist-get report :total-samples)))
    (should (integerp (plist-get report :iterations)))
    (should (> (plist-get report :iterations) 0))
    (should (plist-get report :env))))

(ert-deftest anvil-bench-test/profile-entries-have-expected-keys ()
  (let* ((report (anvil-bench-profile
                  (anvil-bench-test--slow-thunk)
                  :duration-sec 0.1 :top 3))
         (top (plist-get report :top)))
    (dolist (entry top)
      (should (plist-member entry :function))
      (should (plist-member entry :samples))
      (should (plist-member entry :pct))
      (should (stringp (plist-get entry :function)))
      (should (integerp (plist-get entry :samples)))
      (should (>= (plist-get entry :pct) 0.0))
      (should (<= (plist-get entry :pct) 100.0)))))

(ert-deftest anvil-bench-test/profile-errors-on-non-function ()
  (should-error (anvil-bench-profile "nope") :type 'user-error))


;;;; --- last-report --------------------------------------------------------

(ert-deftest anvil-bench-test/last-report-tracks-most-recent ()
  (let ((anvil-bench--last-report nil))
    (should-not (anvil-bench-last-report))
    (anvil-bench-compare
     (anvil-bench-test--fast-thunk)
     (anvil-bench-test--slow-thunk)
     :n 5 :warmup 1)
    (let ((report (anvil-bench-last-report)))
      (should report)
      (should (plist-member report :winner)))
    (anvil-bench-profile (anvil-bench-test--fast-thunk)
                         :duration-sec 0.05 :top 3)
    (let ((report (anvil-bench-last-report)))
      (should report)
      ;; profile plist does NOT carry :winner
      (should-not (plist-member report :winner))
      (should (plist-member report :top)))))


;;;; --- MCP wrappers -------------------------------------------------------

(ert-deftest anvil-bench-test/tool-compare-reads-string-exprs ()
  "The MCP wrapper must `read' the string expressions, wrap them in
thunks, and return a well-shaped report plist."
  (let* ((report (anvil-bench--tool-compare
                  "(+ 1 1)"
                  "(let ((s 0)) (dotimes (i 1000) (setq s (+ s i))) s)"
                  "30" "5" "fast" "slow")))
    (should (memq (plist-get report :winner) '(a b tie)))
    (should (equal "fast" (plist-get report :label-a)))
    (should (equal "slow" (plist-get report :label-b)))
    (should (= 30 (plist-get report :n)))
    (should (= 5  (plist-get report :warmup)))))

(ert-deftest anvil-bench-test/tool-compare-uses-defaults-for-empty-args ()
  (let* ((report (anvil-bench--tool-compare "(+ 1 1)" "(+ 1 1)" "" "" "" "")))
    (should (= anvil-bench-default-n      (plist-get report :n)))
    (should (= anvil-bench-default-warmup (plist-get report :warmup)))
    (should (equal "a" (plist-get report :label-a)))
    (should (equal "b" (plist-get report :label-b)))))

(ert-deftest anvil-bench-test/tool-profile-expr-parses-string ()
  (let* ((report (anvil-bench--tool-profile-expr "(+ 1 1)" "0.05" "3")))
    (should (plist-member report :top))
    (should (integerp (plist-get report :iterations)))
    (should (<= (length (plist-get report :top)) 3))))

(ert-deftest anvil-bench-test/tool-compare-errors-on-empty-expr ()
  (should-error (anvil-bench--tool-compare "" "(+ 1 1)")
                :type 'anvil-server-tool-error))

(ert-deftest anvil-bench-test/tool-compare-errors-on-unreadable-expr ()
  (should-error (anvil-bench--tool-compare "(unclosed" "(+ 1 1)")
                :type 'anvil-server-tool-error))

(ert-deftest anvil-bench-test/tool-last-returns-most-recent ()
  (let ((anvil-bench--last-report nil))
    (should-not (anvil-bench--tool-last))
    (anvil-bench--tool-compare "(+ 1 1)" "(+ 1 1)" "5" "1")
    (let ((r (anvil-bench--tool-last)))
      (should r)
      (should (plist-member r :winner)))))

(provide 'anvil-bench-test)

;;; anvil-bench-test.el ends here
