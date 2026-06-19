;;; anvil-fusion-test.el --- ERT for anvil-fusion Phase 1 -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure-layer tests for the Fusion judge prompt builder.  These do NOT
;; load anvil-orchestrator: the prompt layer is self-contained, and the
;; one full-fidelity path that calls into orchestrator is exercised with
;; a cl-letf stub.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-fusion)

(defconst anvil-fusion-test--candidates
  '((:id "t1" :provider claude :status done
         :summary "地絡継電器は OCGR ではなく DGR を使う。")
    (:id "t2" :provider gemini :status done
         :summary "方向性を持たせるため DGR(地絡方向継電器) が適切。"))
  "Two healthy candidate slim plists.")

;;;; --- candidate text ------------------------------------------------------

(ert-deftest anvil-fusion-test-candidate-text-summary ()
  "Summary fidelity returns the :summary verbatim."
  (should (equal "地絡継電器は OCGR ではなく DGR を使う。"
                 (anvil-fusion--candidate-text
                  (car anvil-fusion-test--candidates) 'summary))))

(ert-deftest anvil-fusion-test-candidate-text-error-fallback ()
  "Empty summary falls back to :error."
  (should (equal "boom"
                 (anvil-fusion--candidate-text
                  '(:id "x" :provider claude :status failed
                        :summary "" :error "boom")
                  'summary))))

(ert-deftest anvil-fusion-test-candidate-text-noinput-fallback ()
  "No summary and no error yields the sentinel."
  (should (equal "(no output)"
                 (anvil-fusion--candidate-text
                  '(:id "x" :provider claude :status failed) 'summary))))

(ert-deftest anvil-fusion-test-candidate-text-full-dispatch ()
  "Full fidelity calls extract-result and uses its untruncated answer."
  (cl-letf (((symbol-function 'anvil-orchestrator-extract-result)
             (lambda (id full)
               (should (equal id "t1"))
               (should full)
               (list :summary "FULL-UNTRUNCATED-ANSWER"))))
    (should (equal "FULL-UNTRUNCATED-ANSWER"
                   (anvil-fusion--candidate-text
                    (car anvil-fusion-test--candidates) 'full)))))

(ert-deftest anvil-fusion-test-candidate-text-full-degrades ()
  "Full fidelity degrades to :summary when extract-result errors."
  (cl-letf (((symbol-function 'anvil-orchestrator-extract-result)
             (lambda (_id _full) (error "no stdout"))))
    (should (equal "地絡継電器は OCGR ではなく DGR を使う。"
                   (anvil-fusion--candidate-text
                    (car anvil-fusion-test--candidates) 'full)))))

;;;; --- candidate formatting ------------------------------------------------

(ert-deftest anvil-fusion-test-format-candidates-numbered ()
  "Candidates are numbered and carry provider + status."
  (let ((block (anvil-fusion--format-candidates
                anvil-fusion-test--candidates 'summary)))
    (should (string-match-p "1\\. \\[provider: claude, status: done\\]" block))
    (should (string-match-p "2\\. \\[provider: gemini, status: done\\]" block))
    (should (string-match-p "DGR" block))))

(ert-deftest anvil-fusion-test-format-candidates-empty ()
  "Empty candidate list does not crash."
  (should (equal "(no candidates)"
                 (anvil-fusion--format-candidates nil 'summary))))

;;;; --- judge prompt --------------------------------------------------------

(ert-deftest anvil-fusion-test-build-prompt-structure ()
  "Prompt embeds the question and all five Fusion analysis axes."
  (let ((p (anvil-fusion-build-judge-prompt
            "地絡保護に使う継電器は？" anvil-fusion-test--candidates
            :fidelity 'summary)))
    (should (string-match-p "地絡保護に使う継電器は？" p))
    (dolist (axis '("合意点" "矛盾点" "部分カバー" "独自洞察" "見落とし" "最終回答"))
      (should (string-match-p (regexp-quote axis) p)))
    ;; both candidate answers present
    (should (string-match-p "OCGR ではなく DGR" p))
    (should (string-match-p "地絡方向継電器" p))))

(ert-deftest anvil-fusion-test-build-prompt-extra ()
  "Extra instruction is appended after the rendered prompt."
  (let ((p (anvil-fusion-build-judge-prompt
            "Q" anvil-fusion-test--candidates
            :extra "200字以内で答えること。")))
    (should (string-suffix-p "200字以内で答えること。" p))))

(ert-deftest anvil-fusion-test-build-prompt-extra-empty-ignored ()
  "Empty :extra is ignored (no trailing blank block)."
  (let ((p (anvil-fusion-build-judge-prompt
            "Q" anvil-fusion-test--candidates :extra "")))
    (should-not (string-suffix-p "\n\n" p))))

(ert-deftest anvil-fusion-test-build-prompt-template-override ()
  "A custom two-%s template fully overrides the default."
  (let ((p (anvil-fusion-build-judge-prompt
            "QQ" anvil-fusion-test--candidates
            :template "Q=%s | C=%s")))
    (should (string-prefix-p "Q=QQ | C=" p))
    (should-not (string-match-p "合意点" p))))

(ert-deftest anvil-fusion-test-build-prompt-nil-original ()
  "Nil original prompt renders as empty, no crash."
  (let ((p (anvil-fusion-build-judge-prompt nil nil)))
    (should (stringp p))
    (should (string-match-p "(no candidates)" p))))

(provide 'anvil-fusion-test)
;;; anvil-fusion-test.el ends here
