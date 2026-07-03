;;; anvil-fusion-verify-test.el --- ERT for anvil-fusion-verify Phase 6a -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure-layer tests for the claim-extraction prompt builder and parser
;; (do NOT load anvil-orchestrator for those).  The single orchestrator
;; call in `anvil-fusion-verify-extract-claims' is exercised with a
;; cl-letf stub, mirroring `tests/anvil-fusion-ask-test.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-fusion)
(or (require 'anvil-orchestrator nil t) ;; load the real module when present (anvil.el)
    (provide 'anvil-orchestrator))  ;; else fake it (fusion standalone repo)
(require 'anvil-fusion-verify)

(defconst anvil-fusion-verify-test--candidates
  '((:id "c1" :name "fusion-member-0-claude" :provider claude :status done
         :summary "絶縁抵抗は 0.1 MOhm と測定された。")
    (:id "c2" :name "fusion-member-1-gemini" :provider gemini :status done
         :summary "絶縁抵抗は 100 MOhm と測定された。"))
  "Two disagreeing candidate slim plists (mirrors the real orchestrator shape).")

;;;; --- candidate label / formatting -----------------------------------------

(ert-deftest anvil-fusion-verify-test-candidate-label-uses-name ()
  "A candidate carrying :name uses it verbatim."
  (should (equal "fusion-member-0-claude"
                 (anvil-fusion-verify--candidate-label
                  (car anvil-fusion-verify-test--candidates) 0))))

(ert-deftest anvil-fusion-verify-test-candidate-label-fallback ()
  "A candidate without :name falls back to an index + provider label."
  (should (equal "candidate-2-ollama"
                 (anvil-fusion-verify--candidate-label
                  '(:id "x" :provider ollama :status done) 1))))

(ert-deftest anvil-fusion-verify-test-format-candidates-empty ()
  "Empty candidate list does not crash."
  (should (equal "(no candidates)"
                 (anvil-fusion-verify--format-candidates nil))))

;;;; --- extraction prompt builder ---------------------------------------------

(ert-deftest anvil-fusion-verify-test-extract-prompt-structure ()
  "Extraction prompt embeds the question, candidate names + bodies, the
max-claims number, and the CLAIM format line."
  (let* ((anvil-fusion-verify-max-claims 5)
         (p (anvil-fusion-verify--extract-prompt
             "絶縁抵抗の測定値は？" anvil-fusion-verify-test--candidates)))
    (should (string-match-p "絶縁抵抗の測定値は？" p))
    (should (string-match-p "fusion-member-0-claude" p))
    (should (string-match-p "fusion-member-1-gemini" p))
    (should (string-match-p "0\\.1 MOhm" p))
    (should (string-match-p "100 MOhm" p))
    (should (string-match-p "\\<5\\>" p))
    (should (string-match-p
             "CLAIM:.*|[ \t]*KIND:.*|[ \t]*CANDIDATES:" p))))

(ert-deftest anvil-fusion-verify-test-extract-prompt-max-claims-changes ()
  "Changing `anvil-fusion-verify-max-claims' is reflected in the prompt."
  (let* ((anvil-fusion-verify-max-claims 3)
         (p (anvil-fusion-verify--extract-prompt "Q" anvil-fusion-verify-test--candidates)))
    (should (string-match-p "\\<3\\>" p))))

(ert-deftest anvil-fusion-verify-test-extract-prompt-nil-question-no-crash ()
  "Nil question renders as empty, no crash."
  (let ((p (anvil-fusion-verify--extract-prompt nil nil)))
    (should (stringp p))
    (should (string-match-p "(no candidates)" p))))

;;;; --- parser: well-formed input ---------------------------------------------

(ert-deftest anvil-fusion-verify-test-parse-well-formed ()
  "Well-formed multi-line input parses into ordered claim plists."
  (let* ((anvil-fusion-verify-max-claims 5)
         (text (concat
                "CLAIM: 絶縁抵抗は 0.1 MOhm である | KIND: number | CANDIDATES: A,B\n"
                "CLAIM: 接地抵抗の測定は不要である | KIND: fact | CANDIDATES: A"))
         (claims (anvil-fusion-verify--parse-claims text)))
    (should (= 2 (length claims)))
    (should (equal "絶縁抵抗は 0.1 MOhm である" (plist-get (nth 0 claims) :claim)))
    (should (eq 'number (plist-get (nth 0 claims) :kind)))
    (should (equal '("A" "B") (plist-get (nth 0 claims) :candidates)))
    (should (equal "接地抵抗の測定は不要である" (plist-get (nth 1 claims) :claim)))
    (should (eq 'fact (plist-get (nth 1 claims) :kind)))
    (should (equal '("A") (plist-get (nth 1 claims) :candidates)))))

(ert-deftest anvil-fusion-verify-test-parse-malformed-lines-skipped ()
  "Lines that do not match the CLAIM format are skipped silently."
  (let ((claims (anvil-fusion-verify--parse-claims
                 (concat "this is not a claim line\n"
                         "CLAIM: X is true | KIND: fact | CANDIDATES: A\n"
                         "random noise"))))
    (should (= 1 (length claims)))
    (should (equal "X is true" (plist-get (car claims) :claim)))))

(ert-deftest anvil-fusion-verify-test-parse-unknown-kind-falls-back-to-fact ()
  "An unrecognized KIND value normalizes to `fact'."
  (let ((claims (anvil-fusion-verify--parse-claims
                 "CLAIM: X | KIND: opinion | CANDIDATES: A")))
    (should (= 1 (length claims)))
    (should (eq 'fact (plist-get (car claims) :kind)))))

(ert-deftest anvil-fusion-verify-test-parse-drops-empty-claim-text ()
  "A line with empty claim text is dropped, well-formed lines survive."
  (let ((claims (anvil-fusion-verify--parse-claims
                 (concat "CLAIM:   | KIND: fact | CANDIDATES: A\n"
                         "CLAIM: real one | KIND: fact | CANDIDATES: A"))))
    (should (= 1 (length claims)))
    (should (equal "real one" (plist-get (car claims) :claim)))))

(ert-deftest anvil-fusion-verify-test-parse-candidates-drop-empties ()
  "Empty entries from stray/trailing commas in CANDIDATES are dropped."
  (let ((claims (anvil-fusion-verify--parse-claims
                 "CLAIM: X | KIND: fact | CANDIDATES: A,,B,")))
    (should (equal '("A" "B") (plist-get (car claims) :candidates)))))

(ert-deftest anvil-fusion-verify-test-parse-clamps-to-max-claims ()
  "Results are clamped to `anvil-fusion-verify-max-claims', order preserved."
  (let* ((anvil-fusion-verify-max-claims 2)
         (text (mapconcat
                (lambda (n) (format "CLAIM: claim %d | KIND: fact | CANDIDATES: A" n))
                '(1 2 3 4) "\n"))
         (claims (anvil-fusion-verify--parse-claims text)))
    (should (= 2 (length claims)))
    (should (equal "claim 1" (plist-get (nth 0 claims) :claim)))
    (should (equal "claim 2" (plist-get (nth 1 claims) :claim)))))

(ert-deftest anvil-fusion-verify-test-parse-none-returns-nil ()
  "Literal NONE output (any case, whitespace-padded) parses to nil."
  (should (null (anvil-fusion-verify--parse-claims "NONE")))
  (should (null (anvil-fusion-verify--parse-claims "  none  \n"))))

(ert-deftest anvil-fusion-verify-test-parse-empty-string-returns-nil ()
  "Empty string / nil input parses to nil."
  (should (null (anvil-fusion-verify--parse-claims "")))
  (should (null (anvil-fusion-verify--parse-claims "   ")))
  (should (null (anvil-fusion-verify--parse-claims nil))))

(ert-deftest anvil-fusion-verify-test-parse-no-matches-returns-nil ()
  "Text with no matching lines at all parses to nil."
  (should (null (anvil-fusion-verify--parse-claims "nothing here matches anything"))))

(ert-deftest anvil-fusion-verify-test-parse-bullet-and-lowercase-labels ()
  "Bullet/number-prefixed and lowercase field-label lines still parse."
  (let ((claims (anvil-fusion-verify--parse-claims
                 (concat "- claim: bulleted and lowercase | kind: FACT | candidates: A, B\n"
                         "1. Claim: numbered | Kind: Number | Candidates:  C "))))
    (should (= 2 (length claims)))
    (should (equal "bulleted and lowercase" (plist-get (nth 0 claims) :claim)))
    (should (eq 'fact (plist-get (nth 0 claims) :kind)))
    (should (equal '("A" "B") (plist-get (nth 0 claims) :candidates)))
    (should (equal "numbered" (plist-get (nth 1 claims) :claim)))
    (should (eq 'number (plist-get (nth 1 claims) :kind)))
    (should (equal '("C") (plist-get (nth 1 claims) :candidates)))))

;;;; --- extract-claims: orchestrator boundary stub ----------------------------

(defmacro anvil-fusion-verify-test--with-fake-orchestrator (extract-result-fn &rest body)
  "Run BODY with a faked single-task orchestrator round-trip.
EXTRACT-RESULT-FN is a form evaluating to the function used as
`anvil-orchestrator-extract-result' (TASK-ID FULL) -> plist, or one
that signals an error to simulate a hard failure.  `anvil-orchestrator-submit'
/ `-collect' / `-status' are stubbed to a single fixed task id
(\"t-extract\") under batch \"b1\".  Binds `submitted' to the single
task plist passed to `anvil-orchestrator-submit'."
  (declare (indent 1) (debug t))
  `(let ((submitted nil))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                (lambda (tasks) (setq submitted (car tasks)) "b1"))
               ((symbol-function 'anvil-orchestrator-collect)
                (lambda (&rest _) t))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (id)
                  (if (equal id "b1")
                      (list :tasks '((:id "t-extract")))
                    (list :tasks nil))))
               ((symbol-function 'anvil-orchestrator-extract-result)
                ,extract-result-fn))
       ,@body)))

(ert-deftest anvil-fusion-verify-test-extract-claims-success ()
  "Success path: stubbed LLM text is parsed into claims, and the
submitted prompt contains the question."
  (anvil-fusion-verify-test--with-fake-orchestrator
      (lambda (id full)
        (should (equal id "t-extract"))
        (should full)
        (list :status 'done
              :summary "CLAIM: DGR is load-bearing | KIND: fact | CANDIDATES: A,B"))
    (let ((claims (anvil-fusion-verify-extract-claims
                   "地絡保護に使う継電器は？" anvil-fusion-verify-test--candidates)))
      (should (= 1 (length claims)))
      (should (equal "DGR is load-bearing" (plist-get (car claims) :claim)))
      (should (eq 'fact (plist-get (car claims) :kind)))
      (should (equal '("A" "B") (plist-get (car claims) :candidates)))
      (should (string-match-p "地絡保護に使う継電器は？" (plist-get submitted :prompt)))
      (should (eq 'claude (plist-get submitted :provider))))))

(ert-deftest anvil-fusion-verify-test-extract-claims-none ()
  "A stubbed NONE response yields nil without error."
  (anvil-fusion-verify-test--with-fake-orchestrator
      (lambda (_id _full) (list :status 'done :summary "NONE"))
    (should (null (anvil-fusion-verify-extract-claims
                   "Q" anvil-fusion-verify-test--candidates)))))

(ert-deftest anvil-fusion-verify-test-extract-claims-failed-status-no-signal ()
  "A non-done terminal status returns nil without signaling."
  (anvil-fusion-verify-test--with-fake-orchestrator
      (lambda (_id _full) (list :status 'failed :error "boom"))
    (should (null (anvil-fusion-verify-extract-claims
                   "Q" anvil-fusion-verify-test--candidates)))))

(ert-deftest anvil-fusion-verify-test-extract-claims-error-no-signal ()
  "A raised orchestrator error is swallowed, not propagated to the caller."
  (anvil-fusion-verify-test--with-fake-orchestrator
      (lambda (_id _full) (error "network down"))
    (should (null (anvil-fusion-verify-extract-claims
                   "Q" anvil-fusion-verify-test--candidates)))))

(ert-deftest anvil-fusion-verify-test-extract-claims-provider-model-override ()
  ":provider / :model keywords reach the submitted task."
  (anvil-fusion-verify-test--with-fake-orchestrator
      (lambda (_id _full) (list :status 'done :summary "NONE"))
    (anvil-fusion-verify-extract-claims
     "Q" anvil-fusion-verify-test--candidates
     :provider 'ollama :model "llama3.1:8b")
    (should (eq 'ollama (plist-get submitted :provider)))
    (should (equal "llama3.1:8b" (plist-get submitted :model)))))

(provide 'anvil-fusion-verify-test)
;;; anvil-fusion-verify-test.el ends here
