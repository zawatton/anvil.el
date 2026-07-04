;;; anvil-fusion-async-test.el --- ERT for anvil-fusion async ask -*- lexical-binding: t; -*-

;;; Commentary:
;; State-machine tests for the non-blocking ask.  orchestrator is faked:
;; submit returns sequential batch ids, status reports running/terminal
;; counts (+ :tasks), extract-result returns the fused answer.
;;
;; Doc 61 Phase 6d' (docs/design/61-fusion-verify.org §2 6d') added two
;; more sections: the critique-loop tests (mirroring
;; tests/anvil-fusion-loop-test.el's per-round candidate stub, adapted to
;; poll-driven submission) and the :verify tests (mirroring
;; tests/anvil-fusion-ask-test.el's Phase 6d section, adapted to the
;; extracting/verifying poll stages).  The pre-existing single-round
;; tests are pinned with :max-rounds 0, same discipline
;; tests/anvil-fusion-ask-test.el already uses for the blocking path --
;; without it, the fixed two-candidate stub below is divergent enough
;; (Jaccard < the default 0.5 threshold) to trigger the critique loop
;; now that this module honors `anvil-fusion-max-rounds' by default.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-fusion)
(require 'anvil-fusion-panels)
(or (require 'anvil-orchestrator nil t) ;; load the real module when present (anvil.el)
    (provide 'anvil-orchestrator))  ;; else fake it (fusion standalone repo)
(require 'anvil-fusion-async)

;; Terminal flags toggled per batch id so a poll can move running->done.
(defvar anvil-fusion-async-test--terminal nil
  "Alist BATCH-ID -> terminal-p used by the fake orchestrator-status.")

(defmacro anvil-fusion-async-test--with (&rest body)
  "Run BODY with a faked orchestrator + a clean job table.
Binds `submitted' (newest first) and `calls' (submit count)."
  (declare (indent 0) (debug t))
  `(let ((calls 0)
         (submitted '())
         (anvil-fusion--jobs (make-hash-table :test 'equal))
         (anvil-fusion-async-test--terminal nil))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                (lambda (tasks)
                  (push tasks submitted)
                  (setq calls (1+ calls))
                  (format "b%d" calls)))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (id)
                  (let ((term (cdr (assoc id anvil-fusion-async-test--terminal))))
                    (list :running (if term 0 1) :queued 0
                          :tasks '((:id "m1" :provider ollama :status done
                                        :summary "候補A")
                                   (:id "m2" :provider ollama :status done
                                        :summary "候補B"))))))
               ((symbol-function 'anvil-orchestrator-extract-result)
                (lambda (_id _full) (list :summary "FUSED-ASYNC"))))
       ,@body)))

(defun anvil-fusion-async-test--mark-terminal (batch-id)
  (push (cons batch-id t) anvil-fusion-async-test--terminal))

(ert-deftest anvil-fusion-async-test-submit-returns-job ()
  "ask-async returns a job id immediately and registers the job."
  (anvil-fusion-async-test--with
    (let ((j (anvil-fusion-ask-async "Q" :panel 'sovereign)))
      (should (string-prefix-p "fusion-" (plist-get j :job-id)))
      (should (eq 'members (plist-get j :stage)))
      (should (eq 'local-only (plist-get j :egress)))
      (should (gethash (plist-get j :job-id) anvil-fusion--jobs)))))

(ert-deftest anvil-fusion-async-test-members-running ()
  "While members run, result reports running/members and submits no judge."
  (anvil-fusion-async-test--with
    (let* ((j (anvil-fusion-ask-async "Q" :panel 'sovereign))
           (id (plist-get j :job-id))
           (r (anvil-fusion-result id)))
      (should (eq 'running (plist-get r :status)))
      (should (eq 'members (plist-get r :stage)))
      (should (= 1 calls)))))            ; only the member submit so far

(ert-deftest anvil-fusion-async-test-full-lifecycle ()
  "members terminal -> judging -> done with the fused answer.
Pinned with :max-rounds 0 -- the fixed 候補A/候補B stub is divergent
enough to loop otherwise (see this file's Commentary); the critique
loop itself is covered below."
  (anvil-fusion-async-test--with
    (let* ((j (anvil-fusion-ask-async "Q" :panel 'sovereign :max-rounds 0))
           (id (plist-get j :job-id)))
      ;; members terminal -> next poll submits the judge
      (anvil-fusion-async-test--mark-terminal "b1")
      (let ((r1 (anvil-fusion-result id)))
        (should (eq 'judging (plist-get r1 :stage)))
        (should (= 2 calls)))            ; judge submitted
      ;; judge still running
      (should (eq 'running (plist-get (anvil-fusion-result id) :status)))
      ;; judge terminal -> done
      (anvil-fusion-async-test--mark-terminal "b2")
      (let ((r2 (anvil-fusion-result id)))
        (should (eq 'done (plist-get r2 :status)))
        (should (equal "FUSED-ASYNC" (plist-get r2 :answer)))
        (should (= 2 (plist-get r2 :n-candidates)))
        (should (= 0 (plist-get r2 :rounds)))
        (should-not (plist-get r2 :looped))
        (should (null (plist-get r2 :claims))))
      ;; done is cached / idempotent
      (should (eq 'done (plist-get (anvil-fusion-result id) :status))))))

(ert-deftest anvil-fusion-async-test-unknown-job ()
  (anvil-fusion-async-test--with
    (should-error (anvil-fusion-result "fusion-nope") :type 'user-error)))

(ert-deftest anvil-fusion-async-test-sovereignty-guard ()
  "A non-local judge override on a local-only panel is refused (no submit)."
  (anvil-fusion-async-test--with
    (should-error (anvil-fusion-ask-async "Q" :panel 'sovereign :judge 'claude)
                  :type 'user-error)
    (should (= 0 calls))))

;;;; ============================================================
;;;; Doc 61 Phase 6d' item 1 -- critique loop in the async state machine
;;;; ============================================================

(defmacro anvil-fusion-async-test--with-rounds (rounds-cands &rest body)
  "Like `anvil-fusion-async-test--with' but serves ROUNDS-CANDS (a
list, one member-candidate set per critique round) instead of a
single fixed set, so a critique-loop test can control convergence
per round.  Odd submit calls are member batches (round = (k-1)/2 of
the 1-based submit count K), even submit calls are judge batches --
the same numbering `anvil-fusion-loop-test--with' uses for the
blocking path.  Binds `submitted' (newest first) and `calls'."
  (declare (indent 0) (debug t))
  `(let ((calls 0)
         (submitted '())
         (anvil-fusion--jobs (make-hash-table :test 'equal))
         (anvil-fusion-async-test--terminal nil)
         (rc ,rounds-cands))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                (lambda (tasks)
                  (push tasks submitted)
                  (setq calls (1+ calls))
                  (format "b%d" calls)))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (id)
                  (let* ((term (cdr (assoc id anvil-fusion-async-test--terminal)))
                         (k (string-to-number (substring id 1)))
                         (tasks (if (cl-oddp k)
                                    (or (nth (/ (1- k) 2) rc) (car (last rc)))
                                  (list (list :id (format "j%d" k)
                                             :provider 'ollama :status 'done)))))
                    (list :running (if term 0 1) :queued 0 :tasks tasks))))
               ((symbol-function 'anvil-orchestrator-extract-result)
                (lambda (id _full) (list :summary (concat "FUSED-" id)))))
       ,@body)))

(defconst anvil-fusion-async-test--div
  '((:id "a" :provider ollama :status done :summary "aaaa")
    (:id "b" :provider ollama :status done :summary "wxyz")
    (:id "c" :provider ollama :status done :summary "1234"))
  "Pairwise-disjoint answers -> divergent -> should loop.")

(defconst anvil-fusion-async-test--conv
  '((:id "a" :provider ollama :status done :summary "samesame")
    (:id "b" :provider ollama :status done :summary "samesame")
    (:id "c" :provider ollama :status done :summary "samesame"))
  "Identical answers -> convergent -> should stop.")

(ert-deftest anvil-fusion-async-loop-test-diverges-then-converges ()
  "Divergent round 0 triggers one critique round (poll-driven: members
-> judging -> [loop] -> members -> judging -> done); convergent round
1 stops."
  (anvil-fusion-async-test--with-rounds
      (list anvil-fusion-async-test--div anvil-fusion-async-test--conv)
    (let* ((j (anvil-fusion-ask-async "Q" :panel 'sovereign :max-rounds 2))
           (id (plist-get j :job-id)))
      ;; round 0: members (b1) -> judge (b2)
      (anvil-fusion-async-test--mark-terminal "b1")
      (should (eq 'judging (plist-get (anvil-fusion-result id) :stage)))
      (should (= 2 calls))
      ;; judge terminal + divergent round0 candidates -> loops into round1
      (anvil-fusion-async-test--mark-terminal "b2")
      (let ((r (anvil-fusion-result id)))
        (should (eq 'running (plist-get r :status)))
        (should (eq 'members (plist-get r :stage))))
      (should (= 3 calls))                            ; round1 members (b3)
      ;; round 1: members (b3) -> judge (b4)
      (anvil-fusion-async-test--mark-terminal "b3")
      (should (eq 'judging (plist-get (anvil-fusion-result id) :stage)))
      (should (= 4 calls))
      ;; judge terminal + convergent round1 candidates -> done
      (anvil-fusion-async-test--mark-terminal "b4")
      (let ((r (anvil-fusion-result id)))
        (should (eq 'done (plist-get r :status)))
        (should (= 1 (plist-get r :rounds)))
        (should (plist-get r :looped))
        (should (equal "FUSED-j4" (plist-get r :answer)))))))

(ert-deftest anvil-fusion-async-loop-test-max-rounds-zero ()
  "max-rounds 0 never loops even when divergent."
  (anvil-fusion-async-test--with-rounds (list anvil-fusion-async-test--div)
    (let* ((j (anvil-fusion-ask-async "Q" :panel 'sovereign :max-rounds 0))
           (id (plist-get j :job-id)))
      (anvil-fusion-async-test--mark-terminal "b1")
      (anvil-fusion-result id)                         ; members -> judging
      (anvil-fusion-async-test--mark-terminal "b2")
      (let ((r (anvil-fusion-result id)))
        (should (eq 'done (plist-get r :status)))
        (should (= 0 (plist-get r :rounds)))
        (should-not (plist-get r :looped))
        (should (= 2 calls))))))

(ert-deftest anvil-fusion-async-loop-test-converged-no-loop ()
  "Convergent round 0 stops immediately under the default cap."
  (anvil-fusion-async-test--with-rounds (list anvil-fusion-async-test--conv)
    (let* ((j (anvil-fusion-ask-async "Q" :panel 'sovereign :max-rounds 1))
           (id (plist-get j :job-id)))
      (anvil-fusion-async-test--mark-terminal "b1")
      (anvil-fusion-result id)
      (anvil-fusion-async-test--mark-terminal "b2")
      (should (= 0 (plist-get (anvil-fusion-result id) :rounds))))))

(ert-deftest anvil-fusion-async-loop-test-cap-is-hard ()
  "Persistent divergence stops at the round cap, not at convergence."
  (anvil-fusion-async-test--with-rounds (list anvil-fusion-async-test--div)
    (let* ((j (anvil-fusion-ask-async "Q" :panel 'sovereign :max-rounds 2))
           (id (plist-get j :job-id)))
      (anvil-fusion-async-test--mark-terminal "b1")
      (anvil-fusion-result id)                         ; -> judging (b2)
      (anvil-fusion-async-test--mark-terminal "b2")
      (anvil-fusion-result id)                         ; loops -> members (b3)
      (anvil-fusion-async-test--mark-terminal "b3")
      (anvil-fusion-result id)                         ; -> judging (b4)
      (anvil-fusion-async-test--mark-terminal "b4")
      (anvil-fusion-result id)                         ; loops -> members (b5)
      (anvil-fusion-async-test--mark-terminal "b5")
      (anvil-fusion-result id)                         ; -> judging (b6)
      (anvil-fusion-async-test--mark-terminal "b6")
      (let ((r (anvil-fusion-result id)))
        (should (eq 'done (plist-get r :status)))
        (should (= 2 (plist-get r :rounds)))
        (should (plist-get r :looped))))))

(ert-deftest anvil-fusion-async-loop-test-critique-prompt-in-round1 ()
  "Round 1 members receive the critique prompt; round 0 members the original."
  (anvil-fusion-async-test--with-rounds
      (list anvil-fusion-async-test--div anvil-fusion-async-test--conv)
    (let* ((j (anvil-fusion-ask-async "Q" :panel 'sovereign :max-rounds 2))
           (id (plist-get j :job-id)))
      (anvil-fusion-async-test--mark-terminal "b1")
      (anvil-fusion-result id)
      (anvil-fusion-async-test--mark-terminal "b2")
      (anvil-fusion-result id)                         ; loops -> submits b3
      ;; submitted newest-first: (mem1 judge0 mem0)
      (let ((mem1 (car submitted))
            (mem0 (car (last submitted))))
        (should (string-match-p "草案" (plist-get (car mem1) :prompt)))
        (should (string-match-p "Q" (plist-get (car mem1) :prompt)))
        (should (equal "Q" (plist-get (car mem0) :prompt)))))))

;;;; ============================================================
;;;; Doc 61 Phase 6d' item 2 -- :verify in the async state machine
;;;; ============================================================

(defconst anvil-fusion-async-test--claims-raw
  (list (list :claim "DGR を使う" :kind 'fact :candidates '("A" "B"))
        (list :claim "OCGR でも良い" :kind 'fact :candidates '("C")))
  "Two raw (Phase 6a) claims used as the extraction stub's return value.")

(defconst anvil-fusion-async-test--claims-annotated
  (list (list :claim "DGR を使う" :kind 'fact :candidates '("A" "B")
              :verdict 'confirmed :evidence "a.org:1")
        (list :claim "OCGR でも良い" :kind 'fact :candidates '("C")
              :verdict 'refuted :evidence "反証: OCGR は不可"))
  "Annotated (Phase 6b) claims, one confirmed one refuted.")

(ert-deftest anvil-fusion-async-verify-test-happy-path ()
  "verify t walks members -> extracting -> verifying -> judging, uses
the verified judge template, and the done result carries :claims."
  (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
             (lambda (question _candidates &rest _)
               (should (equal "Q" question))
               anvil-fusion-async-test--claims-raw))
            ((symbol-function 'anvil-fusion-verify-claims)
             (lambda (claims &rest kwargs)
               (should (equal anvil-fusion-async-test--claims-raw claims))
               (should (equal "Q" (plist-get kwargs :question)))
               anvil-fusion-async-test--claims-annotated)))
    (anvil-fusion-async-test--with
      (let* ((j (anvil-fusion-ask-async "Q" :panel 'sovereign :verify t :max-rounds 0))
             (id (plist-get j :job-id)))
        (anvil-fusion-async-test--mark-terminal "b1")
        (let ((r1 (anvil-fusion-result id)))       ; members terminal -> extracting
          (should (eq 'running (plist-get r1 :status)))
          (should (eq 'extracting (plist-get r1 :stage)))
          (should (= 1 calls)))                    ; no judge submit yet
        (let ((r2 (anvil-fusion-result id)))       ; extracting ran -> verifying
          (should (eq 'verifying (plist-get r2 :stage)))
          (should (= 1 calls)))                    ; still no judge submit
        (let ((r3 (anvil-fusion-result id)))       ; verifying ran -> judge submitted
          (should (eq 'judging (plist-get r3 :stage)))
          (should (= 2 calls)))
        (let ((jtask (car (car submitted))))
          (should (string-match-p "検証済み主張表" (plist-get jtask :prompt)))
          (should (string-match-p "OCGR でも良い" (plist-get jtask :prompt)))
          (should (string-match-p "refuted" (plist-get jtask :prompt))))
        (anvil-fusion-async-test--mark-terminal "b2")
        (let ((r4 (anvil-fusion-result id)))
          (should (eq 'done (plist-get r4 :status)))
          (should (equal anvil-fusion-async-test--claims-annotated
                        (plist-get r4 :claims))))))))

(ert-deftest anvil-fusion-async-verify-test-extraction-nil-fallback ()
  "Extraction finds nothing -> straight to judging with the normal
template; verify-claims is never called; :claims stays nil."
  (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims) (lambda (&rest _) nil))
            ((symbol-function 'anvil-fusion-verify-claims)
             (lambda (&rest _) (error "verify-claims should not be called"))))
    (anvil-fusion-async-test--with
      (let* ((j (anvil-fusion-ask-async "Q" :panel 'sovereign :verify t :max-rounds 0))
             (id (plist-get j :job-id)))
        (anvil-fusion-async-test--mark-terminal "b1")
        (anvil-fusion-result id)                    ; members -> extracting
        (let ((r2 (anvil-fusion-result id)))        ; extracting: nil -> judging directly
          (should (eq 'judging (plist-get r2 :stage)))
          (should (= 2 calls)))
        (let ((jtask (car (car submitted))))
          (should-not (string-match-p "検証済み主張表" (plist-get jtask :prompt))))
        (anvil-fusion-async-test--mark-terminal "b2")
        (let ((r3 (anvil-fusion-result id)))
          (should (eq 'done (plist-get r3 :status)))
          (should (equal "FUSED-ASYNC" (plist-get r3 :answer)))
          (should (null (plist-get r3 :claims))))))))

(ert-deftest anvil-fusion-async-verify-test-sovereignty-refuses-before-submit ()
  "A non-local :verify-args :provider override on a local-only panel is
refused before ANY batch is submitted (stricter timing than the
blocking `anvil-fusion-ask', which only refuses after its member
fan-out already ran -- see `anvil-fusion-ask-async''s docstring)."
  (anvil-fusion-async-test--with
    (should-error
     (anvil-fusion-ask-async "Q" :panel 'sovereign :verify t
                             :verify-args (list :provider 'claude))
     :type 'user-error)
    (should (= 0 calls))))

(ert-deftest anvil-fusion-async-verify-test-explicit-template-wins ()
  "An explicit :template overrides the verified-judge template even
when extraction/verification produce claims; :claims is still
returned."
  (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
             (lambda (&rest _) anvil-fusion-async-test--claims-raw))
            ((symbol-function 'anvil-fusion-verify-claims)
             (lambda (&rest _) anvil-fusion-async-test--claims-annotated)))
    (anvil-fusion-async-test--with
      (let* ((j (anvil-fusion-ask-async "Q" :panel 'sovereign :verify t :max-rounds 0
                                        :template "CUSTOM %s // %s"))
             (id (plist-get j :job-id)))
        (anvil-fusion-async-test--mark-terminal "b1")
        (anvil-fusion-result id)                    ; -> extracting
        (anvil-fusion-result id)                    ; -> verifying
        (let ((r3 (anvil-fusion-result id)))        ; -> judging (explicit template)
          (should (eq 'judging (plist-get r3 :stage))))
        (let ((jtask (car (car submitted))))
          (should (string-prefix-p "CUSTOM " (plist-get jtask :prompt)))
          (should-not (string-match-p "検証済み主張表" (plist-get jtask :prompt))))
        (anvil-fusion-async-test--mark-terminal "b2")
        (let ((r4 (anvil-fusion-result id)))
          (should (eq 'done (plist-get r4 :status)))
          (should (equal anvil-fusion-async-test--claims-annotated
                        (plist-get r4 :claims))))))))

(provide 'anvil-fusion-async-test)
;;; anvil-fusion-async-test.el ends here
