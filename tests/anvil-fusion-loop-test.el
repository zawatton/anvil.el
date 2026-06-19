;;; anvil-fusion-loop-test.el --- ERT for anvil-fusion convergence + loop -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure convergence-measure tests plus critique-loop orchestration tests
;; (orchestrator faked with a per-round candidate stub).

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
(require 'anvil-fusion-ask)

;;;; --- pure convergence measure -------------------------------------------

(ert-deftest anvil-fusion-loop-test-jaccard-identical ()
  (should (= 1.0 (anvil-fusion--jaccard "abcd" "abcd"))))

(ert-deftest anvil-fusion-loop-test-jaccard-disjoint ()
  (should (= 0.0 (anvil-fusion--jaccard "abcd" "wxyz"))))

(ert-deftest anvil-fusion-loop-test-jaccard-partial ()
  ;; "abc"->{ab,bc}  "abx"->{ab,bx}  inter=1 union=3
  (should (< 0.3 (anvil-fusion--jaccard "abc" "abx") 0.34)))

(ert-deftest anvil-fusion-loop-test-min-pairwise-single ()
  (should (= 1.0 (anvil-fusion-min-pairwise-similarity '("only")))))

(ert-deftest anvil-fusion-loop-test-min-pairwise-disjoint ()
  (should (= 0.0 (anvil-fusion-min-pairwise-similarity '("abcd" "wxyz" "1234")))))

(ert-deftest anvil-fusion-loop-test-should-loop-divergent ()
  (should (anvil-fusion-should-loop-p
           '((:status done :summary "abcd")
             (:status done :summary "wxyz")))))

(ert-deftest anvil-fusion-loop-test-should-loop-convergent ()
  (should-not (anvil-fusion-should-loop-p
               '((:status done :summary "samesame")
                 (:status done :summary "samesame")))))

(ert-deftest anvil-fusion-loop-test-should-loop-needs-two ()
  "One usable answer (others empty) never loops."
  (should-not (anvil-fusion-should-loop-p
               '((:status done :summary "abcd")
                 (:status failed :summary "")
                 (:status failed :error "x")))))

(ert-deftest anvil-fusion-loop-test-critique-prompt ()
  (let ((p (anvil-fusion-build-critique-prompt "ORIG-Q" "DRAFT-A")))
    (should (string-match-p "ORIG-Q" p))
    (should (string-match-p "DRAFT-A" p))
    (should (string-match-p "草案" p))))

;;;; --- loop orchestration (faked orchestrator, per-round candidates) ------

(defconst anvil-fusion-loop-test--div
  '((:id "a" :provider ollama :status done :summary "aaaa")
    (:id "b" :provider ollama :status done :summary "wxyz")
    (:id "c" :provider ollama :status done :summary "1234"))
  "Pairwise-disjoint answers -> divergent -> should loop.")

(defconst anvil-fusion-loop-test--conv
  '((:id "a" :provider ollama :status done :summary "samesame")
    (:id "b" :provider ollama :status done :summary "samesame")
    (:id "c" :provider ollama :status done :summary "samesame"))
  "Identical answers -> convergent -> should stop.")

(defmacro anvil-fusion-loop-test--with (rounds-cands &rest body)
  "Fake orchestrator serving ROUNDS-CANDS (a list, one per round).
Odd submit calls are member batches (round = (k-1)/2), even calls
are judge batches.  Binds `submitted' (newest first)."
  (declare (indent 1) (debug t))
  `(let ((submitted '()) (calls 0) (rc ,rounds-cands))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                (lambda (tasks)
                  (push tasks submitted)
                  (setq calls (1+ calls))
                  (format "b%d" calls)))
               ((symbol-function 'anvil-orchestrator-collect)
                (lambda (&rest _) t))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (id)
                  (let ((k (string-to-number (substring id 1))))
                    (if (cl-oddp k)
                        (list :tasks (or (nth (/ (1- k) 2) rc) (car (last rc))))
                      (list :tasks (list (list :id (format "j%d" k)
                                               :provider 'ollama :status 'done)))))))
               ((symbol-function 'anvil-orchestrator-extract-result)
                (lambda (id _full) (list :summary (concat "FUSED-" id)))))
       ,@body)))

(ert-deftest anvil-fusion-loop-test-loops-then-converges ()
  "Divergent round 0 triggers one critique round; convergent round 1 stops."
  (anvil-fusion-loop-test--with
      (list anvil-fusion-loop-test--div anvil-fusion-loop-test--conv)
    (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 2)))
      (should (= 1 (plist-get res :rounds)))
      (should (plist-get res :looped))
      ;; final judge was round 1's (4th submit -> j4)
      (should (equal "FUSED-j4" (plist-get res :answer))))))

(ert-deftest anvil-fusion-loop-test-max-rounds-zero ()
  "max-rounds 0 never loops even when divergent."
  (anvil-fusion-loop-test--with (list anvil-fusion-loop-test--div)
    (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 0)))
      (should (= 0 (plist-get res :rounds)))
      (should-not (plist-get res :looped))
      ;; exactly two submits: members + judge
      (should (= 2 (length submitted))))))

(ert-deftest anvil-fusion-loop-test-converged-no-loop ()
  "Convergent round 0 stops immediately under the default cap."
  (anvil-fusion-loop-test--with (list anvil-fusion-loop-test--conv)
    (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 1)))
      (should (= 0 (plist-get res :rounds))))))

(ert-deftest anvil-fusion-loop-test-cap-is-hard ()
  "Persistent divergence stops at the round cap."
  (anvil-fusion-loop-test--with (list anvil-fusion-loop-test--div)
    (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 2)))
      (should (= 2 (plist-get res :rounds))))))

(ert-deftest anvil-fusion-loop-test-critique-prompt-in-round1 ()
  "Round 1 members receive the critique prompt; round 0 members the original."
  (anvil-fusion-loop-test--with
      (list anvil-fusion-loop-test--div anvil-fusion-loop-test--conv)
    (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 2)
    ;; submitted newest-first: (judge1 mem1 judge0 mem0)
    (let ((mem1 (nth 1 submitted))
          (mem0 (nth 3 submitted)))
      (should (string-match-p "草案" (plist-get (car mem1) :prompt)))
      (should (string-match-p "Q" (plist-get (car mem1) :prompt)))
      (should (equal "Q" (plist-get (car mem0) :prompt))))))

(provide 'anvil-fusion-loop-test)
;;; anvil-fusion-loop-test.el ends here
