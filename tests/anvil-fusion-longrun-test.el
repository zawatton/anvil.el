;;; anvil-fusion-longrun-test.el --- tests for anvil-fusion-longrun -*- lexical-binding: t; -*-

;;; Commentary:
;; Phase 1 spike tests.  The loop is exercised with injected step-fn /
;; distill-fn so no orchestrator (and no real model) is touched: the
;; tests are fast, deterministic, and focus on the bounded-context
;; handoff invariant and context isolation.

;;; Code:

(require 'ert)
(require 'anvil-fusion-longrun)

;;;; --- pure prompt builders ------------------------------------------------

(ert-deftest anvil-fusion-longrun-test-step-prompt-includes-goal-and-digest ()
  (let ((p (anvil-fusion-longrun-build-step-prompt "GOAL-X" "DIGEST-Y" 2 5)))
    (should (string-match-p "GOAL-X" p))
    (should (string-match-p "DIGEST-Y" p))
    (should (string-match-p "2 / 最大 5" p))))

(ert-deftest anvil-fusion-longrun-test-step-prompt-nil-digest ()
  (let ((p (anvil-fusion-longrun-build-step-prompt "G" nil 1 3)))
    (should (string-match-p "(まだ無し)" p))))

(ert-deftest anvil-fusion-longrun-test-distill-prompt-includes-all ()
  (let ((p (anvil-fusion-longrun-build-distill-prompt "GG" "PREV" "OUT" 1234)))
    (should (string-match-p "GG" p))
    (should (string-match-p "PREV" p))
    (should (string-match-p "OUT" p))
    (should (string-match-p "1234" p))))

;;;; --- distill parsing -----------------------------------------------------

(ert-deftest anvil-fusion-longrun-test-parse-done ()
  (let ((r (anvil-fusion-longrun--parse-distill "state body\nSTATUS: DONE" 100)))
    (should (cdr r))
    (should (equal (car r) "state body"))))

(ert-deftest anvil-fusion-longrun-test-parse-continue ()
  (let ((r (anvil-fusion-longrun--parse-distill "state body\nSTATUS: CONTINUE" 100)))
    (should-not (cdr r))
    (should (equal (car r) "state body"))))

(ert-deftest anvil-fusion-longrun-test-parse-no-status ()
  (let ((r (anvil-fusion-longrun--parse-distill "  just state  " 100)))
    (should-not (cdr r))
    (should (equal (car r) "just state"))))

(ert-deftest anvil-fusion-longrun-test-parse-truncates ()
  (let* ((big (concat (make-string 200 ?x) "\nSTATUS: CONTINUE"))
         (r   (anvil-fusion-longrun--parse-distill big 50)))
    (should (= (length (car r)) 50))))

(ert-deftest anvil-fusion-longrun-test-parse-done-only-trailing ()
  "Only a trailing STATUS: DONE terminates; a DONE inside the body does not.
Regression: cataloguing a doc that itself documents the STATUS
protocol put the literal \"STATUS: DONE\" into the digest body and
falsely stopped the quest at step 1."
  ;; mid-body DONE + trailing CONTINUE -> not done, body kept verbatim
  (let ((r (anvil-fusion-longrun--parse-distill
            "課題: 早期 STATUS: DONE 判定\n本文\nSTATUS: CONTINUE" 200)))
    (should-not (cdr r))
    (should (string-match-p "STATUS: DONE" (car r))))
  ;; mid-body DONE, no trailing marker -> not done
  (let ((r (anvil-fusion-longrun--parse-distill
            "本文に STATUS: DONE という語が出るだけ" 200)))
    (should-not (cdr r)))
  ;; trailing DONE still terminates
  (let ((r (anvil-fusion-longrun--parse-distill "本文\nSTATUS: DONE" 200)))
    (should (cdr r))))

;;;; --- loop: termination ---------------------------------------------------

(defun anvil-fusion-longrun-test--const-distill (body done-step)
  "Return a distill-fn that emits BODY-<n> and DONE at DONE-STEP."
  (lambda (_prompt step-n)
    (format "%s-%d\nSTATUS: %s"
            body step-n
            (if (and done-step (>= step-n done-step)) "DONE" "CONTINUE"))))

(ert-deftest anvil-fusion-longrun-test-run-budget-stop ()
  (let ((r (anvil-fusion-longrun-run
            "g"
            :max-steps 3
            :step-fn (lambda (_p _n) "out")
            :distill-fn (anvil-fusion-longrun-test--const-distill "DIGEST" nil))))
    (should (= (plist-get r :steps) 3))
    (should (eq (plist-get r :stopped) 'budget))
    (should (equal (plist-get r :answer) "DIGEST-3"))))

(ert-deftest anvil-fusion-longrun-test-run-early-done ()
  (let ((r (anvil-fusion-longrun-run
            "g"
            :max-steps 9
            :step-fn (lambda (_p _n) "out")
            :distill-fn (anvil-fusion-longrun-test--const-distill "DIGEST" 2))))
    (should (= (plist-get r :steps) 2))
    (should (eq (plist-get r :stopped) 'done))))

;;;; --- loop: bounded-context HANDOFF invariant (the core property) ---------

(ert-deftest anvil-fusion-longrun-test-run-handoff-carries-digest ()
  "Step N must receive step N-1's digest, not the full history."
  (let* ((seen-prompts nil)
         (r (anvil-fusion-longrun-run
             "g"
             :max-steps 3
             :step-fn (lambda (prompt _n) (push prompt seen-prompts) "out")
             :distill-fn (anvil-fusion-longrun-test--const-distill "DIGEST" nil))))
    (setq seen-prompts (nreverse seen-prompts))
    ;; step 1 sees no prior digest
    (should (string-match-p "(まだ無し)" (nth 0 seen-prompts)))
    ;; step 2 sees digest produced at step 1
    (should (string-match-p "DIGEST-1" (nth 1 seen-prompts)))
    ;; step 3 sees digest produced at step 2 (and NOT step 1's, i.e. only the
    ;; latest distilled state is carried -- O(1) context, not accumulated)
    (should (string-match-p "DIGEST-2" (nth 2 seen-prompts)))
    (should-not (string-match-p "DIGEST-1" (nth 2 seen-prompts)))
    (ignore r)))

(ert-deftest anvil-fusion-longrun-test-run-step-prompt-bounded ()
  "Per-step prompt size must not grow with step count (O(1) context)."
  (let* ((anvil-fusion-longrun-converge-patience 0) ; identical digests here; isolate budget
         (sizes nil)
         (_ (anvil-fusion-longrun-run
             "g"
             :max-steps 5
             :digest-max-chars 60
             :step-fn (lambda (prompt _n) (push (length prompt) sizes) "out")
             ;; distiller returns a long body that gets clamped to 60 chars
             :distill-fn (lambda (_p n)
                           (format "%s-%d\nSTATUS: CONTINUE"
                                   (make-string 500 ?z) n)))))
    (setq sizes (nreverse sizes))
    ;; later step prompts are not dramatically larger than the first:
    ;; difference is bounded by the clamped digest size, not by accumulation.
    (let ((first (nth 0 sizes))
          (last  (car (last sizes))))
      (should (< (- last first) 200)))))

;;;; --- loop: context isolation (raw outputs do not leak) -------------------

(ert-deftest anvil-fusion-longrun-test-run-isolates-raw-output ()
  "Raw step OUTPUT is consumed into the digest, never returned verbatim."
  (let ((r (anvil-fusion-longrun-run
            "g"
            :max-steps 2
            :step-fn (lambda (_p _n) "SECRET-RAW-OUTPUT-XYZ")
            :distill-fn (anvil-fusion-longrun-test--const-distill "DIGEST" nil))))
    ;; the distiller (stub) kept only "DIGEST-n"; the secret never appears.
    (should-not (string-match-p "SECRET-RAW-OUTPUT-XYZ"
                                (prin1-to-string r)))
    ;; trace entries carry metadata only (no :output key with the text)
    (dolist (e (plist-get r :trace))
      (should (plist-member e :output-chars))
      (should-not (plist-member e :output)))))

;;;; --- convergence (Phase 4) -----------------------------------------------

(ert-deftest anvil-fusion-longrun-test-streak-helper ()
  (should (= (anvil-fusion-longrun--streak 0 "abc def ghi" "abc def ghi" 0.9 2) 1))
  (should (= (anvil-fusion-longrun--streak 1 "abc def ghi" "abc def ghi" 0.9 2) 2))
  (should (= (anvil-fusion-longrun--streak 1 "abc def ghi" "totally other words" 0.9 2) 0))
  (should (= (anvil-fusion-longrun--streak 5 "x" "y" 0.9 0) 0))    ; patience 0 disables
  (should (= (anvil-fusion-longrun--streak 0 nil "x" 0.9 2) 0)))   ; nil prev -> reset

(ert-deftest anvil-fusion-longrun-test-converged-p-helper ()
  (should (anvil-fusion-longrun--converged-p 2 2))
  (should (anvil-fusion-longrun--converged-p 3 2))
  (should-not (anvil-fusion-longrun--converged-p 1 2))
  (should-not (anvil-fusion-longrun--converged-p 5 0)))

(ert-deftest anvil-fusion-longrun-test-run-converges-on-stagnation ()
  "A digest that stops changing ends the quest early (converged)."
  (let ((r (anvil-fusion-longrun-run
            "g"
            :max-steps 9
            :step-fn (lambda (_p _n) "out")
            :distill-fn (lambda (_p _n) "STABLE STATE\nSTATUS: CONTINUE"))))
    (should (eq (plist-get r :stopped) 'converged))
    ;; patience 2: stagnant streak reaches 2 at step 3
    (should (= (plist-get r :steps) 3))))

(ert-deftest anvil-fusion-longrun-test-converge-patience-zero-disables ()
  (let ((anvil-fusion-longrun-converge-patience 0))
    (let ((r (anvil-fusion-longrun-run
              "g"
              :max-steps 3
              :step-fn (lambda (_p _n) "out")
              :distill-fn (lambda (_p _n) "STABLE STATE\nSTATUS: CONTINUE"))))
      (should (eq (plist-get r :stopped) 'budget))
      (should (= (plist-get r :steps) 3)))))

;;;; --- hermetic step (Phase 4b) --------------------------------------------

(ert-deftest anvil-fusion-longrun-test-disclosure-tools-string ()
  (let ((s (anvil-fusion-longrun-disclosure-tools-string)))
    (should (string-match-p "mcp__emacs-eval__file-outline" s))
    (should (string-match-p "mcp__emacs-eval__file-read-snippet" s))
    (should (string-match-p "," s))))

(ert-deftest anvil-fusion-longrun-test-allowed-tools-plist ()
  (should (equal (anvil-fusion-longrun--allowed-tools-plist '("a" "b"))
                 '(:allowed-tools "a,b")))
  (should (equal (anvil-fusion-longrun--allowed-tools-plist "x,y")
                 '(:allowed-tools "x,y")))
  (should-not (anvil-fusion-longrun--allowed-tools-plist nil))
  (should-not (anvil-fusion-longrun--allowed-tools-plist "")))

(ert-deftest anvil-fusion-longrun-test-apply-suffix ()
  (should (equal (anvil-fusion-longrun--apply-suffix "p" "s") "p\n\ns"))
  (should (equal (anvil-fusion-longrun--apply-suffix "p" "") "p"))
  (should (equal (anvil-fusion-longrun--apply-suffix "p" nil) "p")))

(ert-deftest anvil-fusion-longrun-test-hermetic-threads-allowed-tools ()
  "With :hermetic, the default step-fn restricts the step to disclosure
tools and appends the hermetic instruction to the step prompt."
  (let (captured)
    (cl-letf (((symbol-function 'anvil-fusion-longrun--run-one)
               (lambda (_provider prompt _name &optional _model _cwd _ts _mw
                                  allowed-tools manifest-profile)
                 (push (list :prompt prompt :allowed allowed-tools
                             :mp manifest-profile)
                       captured)
                 "out")))
      (anvil-fusion-longrun-run
       "g" :hermetic t :max-steps 1
       :distill-fn (lambda (_p _n) "D\nSTATUS: DONE"))
      (let ((c (car captured)))
        (should (equal (plist-get c :allowed)
                       (anvil-fusion-longrun--resolve-allowed-tools
                        nil t anvil-fusion-longrun-hermetic-manifest-profile)))
        (should (string-match-p "mcp__emacs-eval-ultra__file-outline"
                                (plist-get c :allowed)))
        (should (eq (plist-get c :mp)
                    anvil-fusion-longrun-hermetic-manifest-profile))
        (should (string-match-p "読み取り専用" (plist-get c :prompt)))
        (should (string-match-p "file-outline" (plist-get c :prompt)))))))

(provide 'anvil-fusion-longrun-test)
;;; anvil-fusion-longrun-test.el ends here
