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
max-claims number, and the CLAIM format line.  Format-call smoke: the
template still consumes exactly three %-placeholders (question,
candidate block, max-claims) without error."
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
             "CLAIM:.*|[ \t]*KIND:.*|[ \t]*CANDIDATES:" p))
    (should (string-match-p "NONE" p))))

(ert-deftest anvil-fusion-verify-test-extract-prompt-per-side-instruction ()
  "Template instructs per-side extraction on disagreement (Doc 61 §9
hardening): a stable substring of that instruction is present."
  (let ((p (anvil-fusion-verify--extract-prompt
            "Q" anvil-fusion-verify-test--candidates)))
    (should (string-match-p "立場ごとに別々の CLAIM 行" p))))

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
  "An explicit :model always wins, regardless of :provider."
  (anvil-fusion-verify-test--with-fake-orchestrator
      (lambda (_id _full) (list :status 'done :summary "NONE"))
    (anvil-fusion-verify-extract-claims
     "Q" anvil-fusion-verify-test--candidates
     :provider 'ollama :model "llama3.1:8b")
    (should (eq 'ollama (plist-get submitted :provider)))
    (should (equal "llama3.1:8b" (plist-get submitted :model)))))

(ert-deftest anvil-fusion-verify-test-extract-claims-default-model-claude ()
  "With defaults (provider `claude'), the submitted task carries the
`anvil-fusion-verify-extract-model' default (\"haiku\")."
  (anvil-fusion-verify-test--with-fake-orchestrator
      (lambda (_id _full) (list :status 'done :summary "NONE"))
    (anvil-fusion-verify-extract-claims "Q" anvil-fusion-verify-test--candidates)
    (should (eq 'claude (plist-get submitted :provider)))
    (should (equal "haiku" (plist-get submitted :model)))))

(ert-deftest anvil-fusion-verify-test-extract-claims-ollama-no-default-model ()
  "Provider `ollama' with no explicit :model submits NO :model key -- the
claude-CLI \"haiku\" alias default must not leak to other providers."
  (anvil-fusion-verify-test--with-fake-orchestrator
      (lambda (_id _full) (list :status 'done :summary "NONE"))
    (anvil-fusion-verify-extract-claims
     "Q" anvil-fusion-verify-test--candidates :provider 'ollama)
    (should (eq 'ollama (plist-get submitted :provider)))
    (should (null (plist-member submitted :model)))))

(ert-deftest anvil-fusion-verify-test-extract-claims-ollama-explicit-model ()
  "Provider `ollama' WITH an explicit :model carries it through."
  (anvil-fusion-verify-test--with-fake-orchestrator
      (lambda (_id _full) (list :status 'done :summary "NONE"))
    (anvil-fusion-verify-extract-claims
     "Q" anvil-fusion-verify-test--candidates
     :provider 'ollama :model "llama3.1:8b")
    (should (eq 'ollama (plist-get submitted :provider)))
    (should (equal "llama3.1:8b" (plist-get submitted :model)))))

;;;; ============================================================
;;;; Phase 6b — evidence check per claim
;;;; ============================================================

(defconst anvil-fusion-verify-test--6b-claims
  (list (list :claim "接地抵抗は10Ω以下である" :kind 'number :candidates '("A" "B"))
        (list :claim "漏電遮断器の設置は不要である" :kind 'fact :candidates '("A")))
  "Two Phase 6a claim plists used by the Phase 6b tests.")

;;;; --- term extractor ---------------------------------------------------------

(ert-deftest anvil-fusion-verify-test-search-terms-mixed ()
  "Mixed CJK/ASCII/number-unit claim extracts all three categories, deduped."
  (let ((terms (anvil-fusion-verify--claim-search-terms
                "絶縁抵抗は 0.1 MOhm、接地抵抗は10Ω以下、絶縁抵抗も再確認")))
    (should (member "絶縁抵抗" terms))
    (should (member "0.1" terms))
    (should (member "MOhm" terms))
    (should (member "接地抵抗" terms))
    (should (member "10Ω" terms))
    (should (= 1 (cl-count "絶縁抵抗" terms :test #'string=)))))

(ert-deftest anvil-fusion-verify-test-search-terms-cap-8 ()
  "Terms are capped at 8, preserving first-appearance order."
  (let* ((text (mapconcat (lambda (n) (format "kanjiterm%d" n)) (number-sequence 1 12) " "))
         (terms (anvil-fusion-verify--claim-search-terms text)))
    (should (= 8 (length terms)))
    (should (equal "kanjiterm1" (car terms)))
    (should (equal "kanjiterm8" (nth 7 terms)))))

(ert-deftest anvil-fusion-verify-test-search-terms-nil-empty ()
  "Nil / empty claim text yields no terms, no crash."
  (should (null (anvil-fusion-verify--claim-search-terms nil)))
  (should (null (anvil-fusion-verify--claim-search-terms ""))))

;;;; --- kb-search (local grep backend) -----------------------------------------

(ert-deftest anvil-fusion-verify-test-kb-search-fixture-ranking ()
  "KB search over a fixture dir ranks lines by distinct-term hit count and
formats :source as \"path:line\"."
  (skip-unless (or (executable-find "rg") (executable-find "grep")))
  (let* ((dir (make-temp-file "anvil-fusion-verify-kb-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.org" dir)
            (insert "* 接地\n"
                    "接地抵抗は10Ω以下とする。\n"
                    "絶縁抵抗の測定にはメガーを使う。\n"))
          (with-temp-file (expand-file-name "b.org" dir)
            (insert "* 絶縁\n"
                    "絶縁抵抗は測定機器で確認する。\n"))
          (let ((hits (anvil-fusion-verify--kb-search
                       "絶縁抵抗は 0.1 MOhm、接地抵抗は10Ω以下" (list dir))))
            (should hits)
            (should (string-match-p "接地抵抗は10Ω以下とする"
                                    (plist-get (car hits) :text)))
            (should (string-match-p "\\`.+:[0-9]+\\'" (plist-get (car hits) :source)))
            (should (<= (length hits) anvil-fusion-verify-max-evidence))))
      (delete-directory dir t))))

(ert-deftest anvil-fusion-verify-test-kb-search-clamps-to-max-evidence ()
  "Results are clamped to `anvil-fusion-verify-max-evidence'."
  (skip-unless (or (executable-find "rg") (executable-find "grep")))
  (let* ((dir (make-temp-file "anvil-fusion-verify-kb-" t))
         (anvil-fusion-verify-max-evidence 1))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.org" dir)
            (insert "接地抵抗は10Ω以下です。\n"
                    "接地抵抗は10Ω以下、再確認。\n"))
          (let ((hits (anvil-fusion-verify--kb-search "接地抵抗は10Ω以下" (list dir))))
            (should (= 1 (length hits)))))
      (delete-directory dir t))))

(ert-deftest anvil-fusion-verify-test-kb-search-nonexistent-root-nil ()
  "A nonexistent root is skipped silently; no other root means nil overall."
  (should (null (anvil-fusion-verify--kb-search
                 "絶縁抵抗" (list "/nonexistent/anvil-fusion-verify-test-root")))))

(ert-deftest anvil-fusion-verify-test-kb-search-nil-roots-nil ()
  "Nil ROOTS (`anvil-fusion-verify-kb-roots' also nil) disables the KB
backend entirely — returns nil without touching the filesystem."
  (let ((anvil-fusion-verify-kb-roots nil))
    (should (null (anvil-fusion-verify--kb-search "絶縁抵抗")))))

;;;; --- skeptic prompt -----------------------------------------------------------

(ert-deftest anvil-fusion-verify-test-skeptic-prompt-no-evidence ()
  "Skeptic prompt embeds question + claim and shows the no-evidence marker."
  (let ((p (anvil-fusion-verify--skeptic-prompt "接地について" "接地抵抗は10Ω以下である" nil)))
    (should (string-match-p "接地について" p))
    (should (string-match-p "接地抵抗は10Ω以下である" p))
    (should (string-match-p "(証拠なし)" p))
    (should (string-match-p "VERDICT:" p))
    (should (string-match-p "REASON:" p))))

(ert-deftest anvil-fusion-verify-test-skeptic-prompt-with-evidence ()
  "Skeptic prompt embeds KB evidence sources/snippets when present."
  (let ((p (anvil-fusion-verify--skeptic-prompt
            "Q" "claim text"
            (list (list :source "a.org:3" :text "接地抵抗は10Ω以下とする。")))))
    (should (string-match-p "a\\.org:3" p))
    (should (string-match-p "接地抵抗は10Ω以下とする" p))))

;;;; --- verdict parser -----------------------------------------------------------

(ert-deftest anvil-fusion-verify-test-parse-verdict-exact-two-lines ()
  "Well-formed two-line output parses verdict and reason."
  (let ((v (anvil-fusion-verify--parse-verdict "VERDICT: CONFIRMED\nREASON: 出典と一致")))
    (should (eq 'confirmed (plist-get v :verdict)))
    (should (equal "出典と一致" (plist-get v :reason)))))

(ert-deftest anvil-fusion-verify-test-parse-verdict-lowercase-labels ()
  "Lowercase VERDICT/REASON labels and values still parse."
  (let ((v (anvil-fusion-verify--parse-verdict "verdict: refuted\nreason: no evidence")))
    (should (eq 'refuted (plist-get v :verdict)))
    (should (equal "no evidence" (plist-get v :reason)))))

(ert-deftest anvil-fusion-verify-test-parse-verdict-surrounding-prose ()
  "Extra prose around the two lines is tolerated."
  (let ((v (anvil-fusion-verify--parse-verdict
            "検討した結果、以下の通り判定する。\nVERDICT: UNVERIFIED\nREASON: 根拠不十分\n以上。")))
    (should (eq 'unverified (plist-get v :verdict)))
    (should (equal "根拠不十分" (plist-get v :reason)))))

(ert-deftest anvil-fusion-verify-test-parse-verdict-garbage-unverified ()
  "Garbage / missing VERDICT label defaults to unverified with empty reason."
  (let ((v (anvil-fusion-verify--parse-verdict "this is not the expected format at all")))
    (should (eq 'unverified (plist-get v :verdict)))
    (should (equal "" (plist-get v :reason)))))

(ert-deftest anvil-fusion-verify-test-parse-verdict-nil-unverified ()
  "Nil input yields unverified with empty reason, no crash."
  (let ((v (anvil-fusion-verify--parse-verdict nil)))
    (should (eq 'unverified (plist-get v :verdict)))
    (should (equal "" (plist-get v :reason)))))

;;;; --- aggregation -----------------------------------------------------------

(ert-deftest anvil-fusion-verify-test-aggregate-2-0-refuted ()
  "Two refuted votes -> refuted, first reason kept."
  (let ((agg (anvil-fusion-verify--aggregate-verdicts
              (list (list :verdict 'refuted :reason "r1")
                    (list :verdict 'refuted :reason "r2")))))
    (should (eq 'refuted (plist-get agg :verdict)))
    (should (equal "r1" (plist-get agg :reason)))))

(ert-deftest anvil-fusion-verify-test-aggregate-2-1-confirmed ()
  "2 confirmed / 1 refuted -> strict majority confirmed."
  (let ((agg (anvil-fusion-verify--aggregate-verdicts
              (list (list :verdict 'confirmed :reason "c1")
                    (list :verdict 'refuted :reason "r1")
                    (list :verdict 'confirmed :reason "c2")))))
    (should (eq 'confirmed (plist-get agg :verdict)))
    (should (equal "c1" (plist-get agg :reason)))))

(ert-deftest anvil-fusion-verify-test-aggregate-tie-unverified ()
  "1-1 tie -> unverified, empty reason."
  (let ((agg (anvil-fusion-verify--aggregate-verdicts
              (list (list :verdict 'confirmed :reason "c1")
                    (list :verdict 'refuted :reason "r1")))))
    (should (eq 'unverified (plist-get agg :verdict)))
    (should (equal "" (plist-get agg :reason)))))

(ert-deftest anvil-fusion-verify-test-aggregate-all-unverified ()
  "All-unverified votes -> unverified."
  (let ((agg (anvil-fusion-verify--aggregate-verdicts
              (list (list :verdict 'unverified :reason "")
                    (list :verdict 'unverified :reason "")))))
    (should (eq 'unverified (plist-get agg :verdict)))))

(ert-deftest anvil-fusion-verify-test-aggregate-empty ()
  "Empty verdict list -> unverified, empty reason."
  (let ((agg (anvil-fusion-verify--aggregate-verdicts nil)))
    (should (eq 'unverified (plist-get agg :verdict)))
    (should (equal "" (plist-get agg :reason)))))

;;;; --- verify-claims: multi-task orchestrator boundary stub -------------------

(defmacro anvil-fusion-verify-test--with-fake-batch-orchestrator (extract-result-fn &rest body)
  "Run BODY with a faked MULTI-task orchestrator round-trip.
Generalizes `anvil-fusion-verify-test--with-fake-orchestrator' (6a,
single fixed task) for 6b's one-batch-many-tasks flow.
`anvil-orchestrator-submit' records the full submitted task list in
`submitted-tasks' (bound for BODY), assigns each task an :id equal to
its :name (unique within one batch, since production code names
tasks \"fusion-verify-skeptic-<claim-index>-<k>\"), and returns batch
id \"b1\".  `-collect' is a no-op (the fake batch is immediately
\"terminal\").  `-status' on \"b1\" returns the fabricated :id/:name
pairs.  EXTRACT-RESULT-FN is a form evaluating to the function used
as `anvil-orchestrator-extract-result' (TASK-ID FULL) -> plist; since
TASK-ID == the task's :name here, EXTRACT-RESULT-FN can branch on the
\"fusion-verify-skeptic-<i>-<k>\" pattern to vary the verdict per
claim / skeptic."
  (declare (indent 1) (debug t))
  `(let ((submitted-tasks nil))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                (lambda (tasks) (setq submitted-tasks tasks) "b1"))
               ((symbol-function 'anvil-orchestrator-collect)
                (lambda (&rest _) t))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (id)
                  (if (equal id "b1")
                      (list :tasks (mapcar (lambda (tk)
                                              (list :id (plist-get tk :name)
                                                    :name (plist-get tk :name)))
                                            submitted-tasks))
                    (list :tasks nil))))
               ((symbol-function 'anvil-orchestrator-extract-result)
                ,extract-result-fn))
       ,@body)))

(ert-deftest anvil-fusion-verify-test-verify-claims-success ()
  "2 claims x 2 skeptics = 4 tasks in one batch; majority computed per
claim; :verdict / :evidence present; inputs not mutated."
  (let ((anvil-fusion-verify-skeptics 2)
        (anvil-fusion-verify-kb-roots nil))
    (anvil-fusion-verify-test--with-fake-batch-orchestrator
        (lambda (id _full)
          (cond
           ((string-prefix-p "fusion-verify-skeptic-0-" id)
            (list :status 'done :summary "VERDICT: REFUTED\nREASON: 出典に一致しない"))
           ((string-prefix-p "fusion-verify-skeptic-1-" id)
            (list :status 'done :summary "VERDICT: CONFIRMED\nREASON: 妥当"))
           (t (list :status 'done :summary "VERDICT: UNVERIFIED\nREASON: "))))
      (let ((result (anvil-fusion-verify-claims
                     anvil-fusion-verify-test--6b-claims
                     :question "接地について")))
        (should (= 2 (length result)))
        (should (= 4 (length submitted-tasks)))
        (should (eq 'refuted (plist-get (nth 0 result) :verdict)))
        (should (equal "出典に一致しない" (plist-get (nth 0 result) :evidence)))
        (should (eq 'confirmed (plist-get (nth 1 result) :verdict)))
        (should (equal "妥当" (plist-get (nth 1 result) :evidence)))
        ;; inputs not mutated
        (should (null (plist-get (nth 0 anvil-fusion-verify-test--6b-claims) :verdict)))
        (should (null (plist-get (nth 0 anvil-fusion-verify-test--6b-claims) :evidence)))
        (should (null (plist-get (nth 1 anvil-fusion-verify-test--6b-claims) :verdict)))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-task-failure-unverified ()
  "A failed skeptic task counts as an unverified vote."
  (let ((anvil-fusion-verify-skeptics 1)
        (anvil-fusion-verify-kb-roots nil))
    (anvil-fusion-verify-test--with-fake-batch-orchestrator
        (lambda (_id _full) (list :status 'failed :error "timeout"))
      (let ((result (anvil-fusion-verify-claims
                     (list (car anvil-fusion-verify-test--6b-claims))
                     :question "Q")))
        (should (eq 'unverified (plist-get (car result) :verdict)))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-egress-violation-signals ()
  "local-only egress with a non-local provider signals BEFORE any submit."
  (let (called)
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (_tasks) (setq called t) "b1")))
      (should-error
       (anvil-fusion-verify-claims
        anvil-fusion-verify-test--6b-claims
        :question "Q" :egress 'local-only :provider 'claude)
       :type 'user-error)
      (should (null called)))))

(ert-deftest anvil-fusion-verify-test-verify-claims-local-only-passes-gate ()
  "A local provider on a local-only egress request passes the sovereignty
gate and proceeds to submit normally."
  (let ((anvil-fusion-verify-skeptics 1)
        (anvil-fusion-verify-kb-roots nil))
    (anvil-fusion-verify-test--with-fake-batch-orchestrator
        (lambda (_id _full) (list :status 'done :summary "VERDICT: UNVERIFIED\nREASON: n/a"))
      (let ((result (anvil-fusion-verify-claims
                     (list (car anvil-fusion-verify-test--6b-claims))
                     :question "Q" :egress 'local-only :provider 'ollama)))
        (should (= 1 (length submitted-tasks)))
        (should (eq 'ollama (plist-get (car submitted-tasks) :provider)))
        (should (eq 'unverified (plist-get (car result) :verdict)))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-wholesale-error-no-signal ()
  "A wholesale orchestrator error (submit itself fails) annotates every
claim unverified without signaling to the caller."
  (cl-letf (((symbol-function 'anvil-orchestrator-submit)
             (lambda (_tasks) (error "network down"))))
    (let ((result (anvil-fusion-verify-claims
                   anvil-fusion-verify-test--6b-claims :question "Q")))
      (should (= 2 (length result)))
      (dolist (c result)
        (should (eq 'unverified (plist-get c :verdict)))
        (should (equal "" (plist-get c :evidence)))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-empty-claims-no-call ()
  "Empty CLAIMS returns nil immediately without any orchestrator call."
  (let (called)
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (_t) (setq called t) "b1")))
      (should (null (anvil-fusion-verify-claims nil :question "Q")))
      (should (null called)))))

;;;; ============================================================
;;;; Phase 6d — verdict-annotated judge synthesis
;;;; ============================================================

(defconst anvil-fusion-verify-test--6d-claims
  (list (list :claim "接地抵抗は10Ω以下である" :kind 'number :candidates '("A" "B")
              :verdict 'confirmed :evidence "a.org:3 — 出典と一致")
        (list :claim "漏電遮断器の設置は不要である" :kind 'fact :candidates '("A")
              :verdict 'refuted :evidence "出典に一致しない"))
  "Two Phase 6b-annotated claims used by the Phase 6d tests.")

;;;; --- claims-block renderer ---------------------------------------------------

(ert-deftest anvil-fusion-verify-test-claims-block-renders-fields ()
  "Annotated claims render claim/kind/verdict/evidence/candidates."
  (let ((block (anvil-fusion-verify--format-claims-block
                anvil-fusion-verify-test--6d-claims)))
    (should (string-match-p "接地抵抗は10Ω以下である" block))
    (should (string-match-p "number" block))
    (should (string-match-p "confirmed" block))
    (should (string-match-p "a\\.org:3" block))
    (should (string-match-p "A, B" block))
    (should (string-match-p "漏電遮断器の設置は不要である" block))
    (should (string-match-p "refuted" block))
    (should (string-match-p "出典に一致しない" block))))

(ert-deftest anvil-fusion-verify-test-claims-block-missing-verdict-unverified ()
  "A claim lacking :verdict (e.g. a raw Phase 6a claim never sent
through `anvil-fusion-verify-claims') renders as unverified with the
no-evidence marker."
  (let ((block (anvil-fusion-verify--format-claims-block
                (list (list :claim "X" :kind 'fact :candidates '("A"))))))
    (should (string-match-p "unverified" block))
    (should (string-match-p "(証拠なし)" block))))

(ert-deftest anvil-fusion-verify-test-claims-block-empty ()
  "Nil claims render the (検証済み主張なし) sentinel."
  (should (equal "(検証済み主張なし)" (anvil-fusion-verify--format-claims-block nil))))

(ert-deftest anvil-fusion-verify-test-claims-block-empty-list ()
  "An empty (non-nil-but-zero-length) claims list is nil in Elisp, so
this is the same case as the nil test -- kept for the \"empty\"
wording in the spec."
  (should (equal "(検証済み主張なし)" (anvil-fusion-verify--format-claims-block '()))))

;;;; --- judge-template-for -------------------------------------------------------

(ert-deftest anvil-fusion-verify-test-judge-template-for-substitutes-marker ()
  "The {{CLAIMS}} marker is replaced by the rendered claims block, and
the base's two %s slots survive substitution (a two-argument `format'
call on the result does not error)."
  (let ((tmpl (anvil-fusion-verify-judge-template-for anvil-fusion-verify-test--6d-claims)))
    (should-not (string-match-p (regexp-quote "{{CLAIMS}}") tmpl))
    (should (string-match-p "接地抵抗は10Ω以下である" tmpl))
    (should (string-match-p "confirmed" tmpl))
    (should (stringp (format tmpl "Q" "CANDS")))))

(ert-deftest anvil-fusion-verify-test-judge-template-for-empty-claims ()
  "Nil claims substitutes the (検証済み主張なし) sentinel into the template."
  (let ((tmpl (anvil-fusion-verify-judge-template-for nil)))
    (should (string-match-p (regexp-quote "(検証済み主張なし)") tmpl))))

(ert-deftest anvil-fusion-verify-test-judge-template-for-missing-marker-errors ()
  "A BASE-TEMPLATE without the {{CLAIMS}} marker signals `user-error'."
  (should-error
   (anvil-fusion-verify-judge-template-for nil "no marker here: %s / %s")
   :type 'user-error))

(ert-deftest anvil-fusion-verify-test-judge-template-for-percent-survives-format ()
  "A literal % inside a claim text survives escaping + marker
substitution + the SAME `format' call
`anvil-fusion-build-judge-prompt' (and therefore
`anvil-fusion-judge-consensus' / `anvil-fusion-ask') applies to
:template, uncorrupted (not doubled, not misread as a directive) in
the final judge prompt."
  (let* ((claims (list (list :claim "効率は 95% です" :kind 'number
                              :verdict 'confirmed :evidence "a.org:1"
                              :candidates '("A"))))
         (tmpl (anvil-fusion-verify-judge-template-for claims))
         ;; The exact path `anvil-fusion-judge-consensus' / `anvil-fusion-ask'
         ;; use to turn a :template into the final judge prompt.
         (prompt (anvil-fusion-build-judge-prompt "Q" nil :template tmpl)))
    (should (string-match-p "効率は 95% です" prompt))
    (should-not (string-match-p "95%%" prompt))))

(provide 'anvil-fusion-verify-test)
;;; anvil-fusion-verify-test.el ends here
