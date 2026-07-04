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

;; Keep pre-cache tests deterministic even when a real `anvil-state' DB
;; exists in the environment; cache-focused tests bind this back on.
(setq anvil-fusion-verify-cache-ttl-sec nil)

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

(defmacro anvil-fusion-verify-test--with-cache-fallback (&rest body)
  "Run BODY with the verified-claim cache forced onto the hash fallback."
  (declare (indent 0) (debug t))
  `(let ((anvil-fusion-verify--cache-session-store (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'require)
                (let ((orig (symbol-function 'require)))
                  (lambda (feature &optional filename noerror)
                    (if (eq feature 'anvil-state)
                        nil
                      (funcall orig feature filename noerror))))))
       ,@body)))

(defun anvil-fusion-verify-test--cache-values ()
  "Return the fallback cache values as a plain list."
  (let (values)
    (maphash (lambda (_key value)
               (push value values))
             anvil-fusion-verify--cache-session-store)
    values))

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

;;;; --- verdict cache ----------------------------------------------------------

(ert-deftest anvil-fusion-verify-test-cache-key-normalizes-whitespace ()
  "Whitespace/newline variants collide in the cache key."
  (should
   (equal (anvil-fusion-verify--cache-key
           '(:claim "絶縁抵抗は 0.1 MΩ である" :kind number))
          (anvil-fusion-verify--cache-key
           '(:claim "  絶縁抵抗は\n0.1   MΩ\tである  " :kind number)))))

(ert-deftest anvil-fusion-verify-test-cache-key-distinguishes-kind ()
  "Different claim kinds produce distinct cache keys."
  (should-not
   (equal (anvil-fusion-verify--cache-key
           '(:claim "絶縁抵抗は 0.1 MΩ である" :kind number))
          (anvil-fusion-verify--cache-key
           '(:claim "絶縁抵抗は 0.1 MΩ である" :kind fact)))))

(ert-deftest anvil-fusion-verify-test-cache-put-get-roundtrip-fallback ()
  "Fallback cache stores decisive verdicts and skips unverified."
  (anvil-fusion-verify-test--with-cache-fallback
    (let ((anvil-fusion-verify-cache-ttl-sec 60)
          (anvil-fusion-verify--cache-now-fn (lambda () 1000.0))
          (claim '(:claim "絶縁抵抗は 0.1 MΩ である" :kind number)))
      (anvil-fusion-verify--cache-put claim 'confirmed "kb.org:1 — confirmed")
      (let ((entry (anvil-fusion-verify--cache-get claim)))
        (should (eq 'confirmed (plist-get entry :verdict)))
        (should (equal "kb.org:1 — confirmed" (plist-get entry :evidence)))
        (should (= 1000.0 (plist-get entry :ts))))
      (anvil-fusion-verify--cache-put claim 'unverified "should not persist")
      (should (= 1 (hash-table-count anvil-fusion-verify--cache-session-store))))))

(ert-deftest anvil-fusion-verify-test-cache-ttl-nil-disables-read-and-write ()
  "Nil TTL disables both cache reads and cache writes."
  (anvil-fusion-verify-test--with-cache-fallback
    (let ((anvil-fusion-verify-cache-ttl-sec nil)
          (anvil-fusion-verify--cache-now-fn (lambda () 1000.0))
          (claim '(:claim "絶縁抵抗は 0.1 MΩ である" :kind number)))
      (anvil-fusion-verify--cache-put claim 'confirmed "kb.org:1 — confirmed")
      (should (= 0 (hash-table-count anvil-fusion-verify--cache-session-store)))
      (should (null (anvil-fusion-verify--cache-get claim))))))

(ert-deftest anvil-fusion-verify-test-cache-expiry-deletes-entry ()
  "Expired fallback entries miss and are deleted eagerly."
  (anvil-fusion-verify-test--with-cache-fallback
    (let ((anvil-fusion-verify-cache-ttl-sec 10)
          (now 1000.0)
          (claim '(:claim "絶縁抵抗は 0.1 MΩ である" :kind number)))
      (let ((anvil-fusion-verify--cache-now-fn (lambda () now)))
        (anvil-fusion-verify--cache-put claim 'confirmed "kb.org:1 — confirmed")
        (setq now 1011.0)
        (should (null (anvil-fusion-verify--cache-get claim)))
        (should (= 0 (hash-table-count anvil-fusion-verify--cache-session-store)))))))

(ert-deftest anvil-fusion-verify-test-cache-kb-stamp-stale-misses ()
  "A newer KB-root directory mtime invalidates the cached entry."
  (skip-unless (fboundp 'set-file-times))
  (anvil-fusion-verify-test--with-cache-fallback
    (let* ((dir (make-temp-file "anvil-fusion-verify-cache-kb-" t))
           (anvil-fusion-verify-cache-ttl-sec 60)
           (anvil-fusion-verify-kb-roots (list dir))
           (anvil-fusion-verify--cache-now-fn (lambda () 1000.0))
           (claim '(:claim "絶縁抵抗は 0.1 MΩ である" :kind number)))
      (unwind-protect
          (progn
            (anvil-fusion-verify--cache-put claim 'confirmed "kb.org:1 — confirmed")
            (set-file-times dir (time-add (file-attribute-modification-time
                                           (file-attributes dir))
                                          1))
            (should (null (anvil-fusion-verify--cache-get claim))))
        (delete-directory dir t)))))

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

(ert-deftest anvil-fusion-verify-test-escalate-p-unverified ()
  (should (anvil-fusion-verify--escalate-p nil nil '(:verdict unverified :reason "long enough"))))

(ert-deftest anvil-fusion-verify-test-escalate-p-refuted-with-kb-evidence ()
  (should (anvil-fusion-verify--escalate-p
           nil '((:source "kb.org:1" :text "x")) '(:verdict refuted :reason "long enough"))))

(ert-deftest anvil-fusion-verify-test-escalate-p-refuted-without-kb-evidence-long-reason ()
  (should-not (anvil-fusion-verify--escalate-p
               nil nil '(:verdict refuted :reason "long enough reason"))))

(ert-deftest anvil-fusion-verify-test-escalate-p-confirmed-short-reason ()
  (let ((anvil-fusion-verify-sequential-weak-reason-chars 12))
    (should (anvil-fusion-verify--escalate-p
             nil nil '(:verdict confirmed :reason "short")))))

(ert-deftest anvil-fusion-verify-test-escalate-p-confirmed-long-reason ()
  (let ((anvil-fusion-verify-sequential-weak-reason-chars 12))
    (should-not (anvil-fusion-verify--escalate-p
                 nil nil '(:verdict confirmed :reason "long enough reason")))))

;;;; --- verify-claims: multi-task orchestrator boundary stub -------------------

(defmacro anvil-fusion-verify-test--with-fake-batch-orchestrator (extract-result-fn &rest body)
  "Run BODY with a faked MULTI-task orchestrator round-trip.
Generalizes `anvil-fusion-verify-test--with-fake-orchestrator' (6a,
single fixed task) for 6b's one-batch-many-tasks flow.
`anvil-orchestrator-submit' records the full submitted task list in
`submitted-tasks' (latest batch) and accumulates every submit in
`submitted-batches' (bound for BODY), assigning each task an :id equal
to its :name.  Returns batch ids \"b1\", \"b2\", ... and `-status'
reflects the tasks submitted for that specific batch id.
EXTRACT-RESULT-FN is a form evaluating to the function used as
`anvil-orchestrator-extract-result' (TASK-ID FULL) -> plist; since
TASK-ID == the task's :name here, EXTRACT-RESULT-FN can branch on the
\"fusion-verify-skeptic-<i>-<k>\" pattern to vary the verdict per
claim / skeptic."
  (declare (indent 1) (debug t))
  `(let ((submitted-tasks nil)
         (submitted-batches nil)
         (batch-table (make-hash-table :test 'equal))
         (submit-count 0))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                  (setq submit-count (1+ submit-count))
                  (setq submitted-tasks tasks)
                  (setq submitted-batches (append submitted-batches (list tasks)))
                  (let ((batch-id (format "b%d" submit-count)))
                    (puthash batch-id tasks batch-table)
                    batch-id)))
               ((symbol-function 'anvil-orchestrator-collect)
                (lambda (&rest _) t))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (id)
                  (let ((tasks (gethash id batch-table)))
                    (if tasks
                        (list :tasks
                              (mapcar (lambda (tk)
                                        (list :id (plist-get tk :name)
                                              :name (plist-get tk :name)))
                                      tasks))
                      (list :tasks nil)))))
               ((symbol-function 'anvil-orchestrator-extract-result)
                ,extract-result-fn))
       ,@body)))

(ert-deftest anvil-fusion-verify-test-verify-claims-success ()
  "2 claims x 2 skeptics = 4 tasks in one batch; majority computed per
claim; :verdict / :evidence present; inputs not mutated."
  (let ((anvil-fusion-verify-skeptics 2)
        (anvil-fusion-verify-sequential-skeptics nil)
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

(ert-deftest anvil-fusion-verify-test-verify-claims-sequential-happy-path ()
  "Round 1 settles both claims, so only one two-task submit is made."
  (let ((anvil-fusion-verify-skeptics 2)
        (anvil-fusion-verify-sequential-skeptics t)
        (anvil-fusion-verify-kb-roots nil))
    (anvil-fusion-verify-test--with-fake-batch-orchestrator
        (lambda (id _full)
          (cond
           ((equal id "fusion-verify-skeptic-0-0")
            (list :status 'done :summary "VERDICT: REFUTED\nREASON: clear contradiction in source"))
           ((equal id "fusion-verify-skeptic-1-0")
            (list :status 'done :summary "VERDICT: CONFIRMED\nREASON: supported by cited evidence"))
           (t
            (ert-fail (format "unexpected round-2 task: %s" id)))))
      (let* ((result (anvil-fusion-verify-claims
                      anvil-fusion-verify-test--6b-claims
                      :question "接地について"))
             (round1 (car submitted-batches)))
        (should (= 1 submit-count))
        (should (= 1 (length submitted-batches)))
        (should (= 2 (length round1)))
        (should (equal '("fusion-verify-skeptic-0-0" "fusion-verify-skeptic-1-0")
                       (mapcar (lambda (task) (plist-get task :name))
                               round1)))
        (should (equal '(refuted confirmed)
                       (mapcar (lambda (claim) (plist-get claim :verdict))
                               result)))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-sequential-escalation ()
  "Only the unresolved claim gets a second-round submit."
  (let ((anvil-fusion-verify-skeptics 3)
        (anvil-fusion-verify-sequential-skeptics t)
        (anvil-fusion-verify-kb-roots nil))
    (anvil-fusion-verify-test--with-fake-batch-orchestrator
        (lambda (id _full)
          (cond
           ((equal id "fusion-verify-skeptic-0-0")
            (list :status 'done :summary "VERDICT: REFUTED\nREASON: clear contradiction in source"))
           ((equal id "fusion-verify-skeptic-1-0")
            (list :status 'done :summary "VERDICT: UNVERIFIED\nREASON: uncertain"))
           ((equal id "fusion-verify-skeptic-1-1")
            (list :status 'done :summary "VERDICT: CONFIRMED\nREASON: supported by cited evidence"))
           ((equal id "fusion-verify-skeptic-1-2")
            (list :status 'done :summary "VERDICT: CONFIRMED\nREASON: second source agrees"))
           (t
            (ert-fail (format "unexpected task id: %s" id)))))
      (let* ((result (anvil-fusion-verify-claims
                      anvil-fusion-verify-test--6b-claims
                      :question "接地について"))
             (round1 (nth 0 submitted-batches))
             (round2 (nth 1 submitted-batches)))
        (should (= 2 submit-count))
        (should (= 2 (length submitted-batches)))
        (should (equal '("fusion-verify-skeptic-0-0" "fusion-verify-skeptic-1-0")
                       (mapcar (lambda (task) (plist-get task :name))
                               round1)))
        (should (equal '("fusion-verify-skeptic-1-1" "fusion-verify-skeptic-1-2")
                       (mapcar (lambda (task) (plist-get task :name))
                               round2)))
        (should (equal '("接地抵抗は10Ω以下である" "漏電遮断器の設置は不要である")
                       (mapcar (lambda (claim) (plist-get claim :claim)) result)))
        (should (equal '(refuted confirmed)
                       (mapcar (lambda (claim) (plist-get claim :verdict))
                               result)))
        (should (equal "clear contradiction in source" (plist-get (nth 0 result) :evidence)))
        (should (equal "supported by cited evidence" (plist-get (nth 1 result) :evidence)))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-nsk-1-single-round ()
  "nsk=1 performs one round only and never submits escalation tasks."
  (let ((anvil-fusion-verify-skeptics 1)
        (anvil-fusion-verify-sequential-skeptics t)
        (anvil-fusion-verify-kb-roots nil))
    (anvil-fusion-verify-test--with-fake-batch-orchestrator
        (lambda (_id _full)
          (list :status 'done :summary "VERDICT: CONFIRMED\nREASON: enough detail here"))
      (let ((result (anvil-fusion-verify-claims
                     anvil-fusion-verify-test--6b-claims
                     :question "接地について")))
        (should (= 1 submit-count))
        (should (= 2 (length submitted-tasks)))
        (should (cl-every (lambda (task)
                            (string-suffix-p "-0" (plist-get task :name)))
                          submitted-tasks))
        (should (equal '(confirmed confirmed)
                       (mapcar (lambda (claim) (plist-get claim :verdict))
                               result)))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-sequential-disabled-old-batch ()
  "Sequential nil keeps the pre-change one-batch N-tasks-per-claim flow."
  (let ((anvil-fusion-verify-skeptics 3)
        (anvil-fusion-verify-sequential-skeptics nil)
        (anvil-fusion-verify-kb-roots nil))
    (anvil-fusion-verify-test--with-fake-batch-orchestrator
        (lambda (_id _full)
          (list :status 'done :summary "VERDICT: CONFIRMED\nREASON: enough detail here"))
      (anvil-fusion-verify-claims anvil-fusion-verify-test--6b-claims :question "接地について")
      (should (= 1 submit-count))
      (should (= 1 (length submitted-batches)))
      (should (= 6 (length (car submitted-batches)))))))

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

(ert-deftest anvil-fusion-verify-test-verify-claims-round-2-error-falls-back ()
  "A wholesale round-2 failure falls back to all-unverified without signaling."
  (let ((anvil-fusion-verify-skeptics 3)
        (anvil-fusion-verify-sequential-skeptics t)
        (anvil-fusion-verify-kb-roots nil)
        (submitted-tasks nil))
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (let ((n 0))
                 (lambda (tasks)
                   (setq n (1+ n))
                   (cond
                    ((= n 1)
                     (setq submitted-tasks tasks)
                     "b1")
                    ((= n 2)
                     (error "round 2 broke"))
                    (t
                     (ert-fail "unexpected third submit"))))))
              ((symbol-function 'anvil-orchestrator-collect)
               (lambda (&rest _) t))
              ((symbol-function 'anvil-orchestrator-status)
               (lambda (_id)
                 (list :tasks
                       (mapcar (lambda (task)
                                 (list :id (plist-get task :name)
                                       :name (plist-get task :name)))
                               submitted-tasks))))
              ((symbol-function 'anvil-orchestrator-extract-result)
               (lambda (id _full)
                 (cond
                  ((equal id "fusion-verify-skeptic-0-0")
                   (list :status 'done :summary "VERDICT: REFUTED\nREASON: clear contradiction in source"))
                  ((equal id "fusion-verify-skeptic-1-0")
                  (list :status 'done :summary "VERDICT: UNVERIFIED\nREASON: uncertain"))
                  (t
                   (ert-fail (format "unexpected task id: %s" id)))))))
      (let ((result (anvil-fusion-verify-claims
                     anvil-fusion-verify-test--6b-claims
                     :question "接地について")))
        (should (equal '(unverified unverified)
                       (mapcar (lambda (claim) (plist-get claim :verdict))
                               result)))
        (should (cl-every (lambda (claim)
                            (equal "" (plist-get claim :evidence)))
                          result))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-empty-claims-no-call ()
  "Empty CLAIMS returns nil immediately without any orchestrator call."
  (let (called)
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (_t) (setq called t) "b1")))
      (should (null (anvil-fusion-verify-claims nil :question "Q")))
      (should (null called)))))

(defun anvil-fusion-verify-test--with-repo-fixture (fn)
  "Create a temporary repo fixture and call FN with its root."
  (let ((root (make-temp-file "anvil-fusion-verify-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sub" root) t)
          (with-temp-file (expand-file-name "fixture.el" root)
            (insert ";;; fixture.el --- test fixture -*- lexical-binding: t; -*-\n"
                    "(defun fixture-fn-alpha () t)\n"))
          (with-temp-file (expand-file-name "sub/x.org" root)
            (insert "* fixture\n"))
          (funcall fn root))
      (delete-directory root t))))

(defun anvil-fusion-verify-test--with-kb-fixture (files fn)
  "Create KB FILES under a temp dir, call FN with that root, then clean up.
FILES is an alist of (RELATIVE-PATH . CONTENTS)."
  (let ((root (make-temp-file "anvil-fusion-verify-kb-" t)))
    (unwind-protect
        (progn
          (dolist (file files)
            (let ((path (expand-file-name (car file) root)))
              (make-directory (file-name-directory path) t)
              (with-temp-file path
                (insert (cdr file)))))
          (funcall fn root))
      (delete-directory root t))))

(ert-deftest anvil-fusion-verify-test-repo-reality-checker-path-confirmed ()
  "An existing path token confirms the claim."
  (anvil-fusion-verify-test--with-repo-fixture
   (lambda (root)
     (let ((anvil-fusion-verify-repo-root root))
       (should (equal '(:verdict confirmed :evidence "exists: sub/x.org")
                      (anvil-fusion-verify-checker-repo-reality
                       '(:claim "see sub/x.org"))))))))

(ert-deftest anvil-fusion-verify-test-repo-reality-checker-path-missing-hint ()
  "A missing path token yields a mechanical hint, not a verdict."
  (anvil-fusion-verify-test--with-repo-fixture
   (lambda (root)
     (let* ((anvil-fusion-verify-repo-root root)
            (result (anvil-fusion-verify-checker-repo-reality
                     '(:claim "create tests/missing-test.el and docs/missing.org"))))
       (should (null (plist-get result :verdict)))
       (should (string-match-p "tests/missing-test\\.el, docs/missing\\.org"
                               (plist-get result :evidence)))))))

(ert-deftest anvil-fusion-verify-test-repo-reality-checker-symbol-confirmed ()
  "A defined symbol confirms the claim with a location."
  (anvil-fusion-verify-test--with-repo-fixture
   (lambda (root)
     (let* ((anvil-fusion-verify-repo-root root)
            (result (anvil-fusion-verify-checker-repo-reality
                     '(:claim "call `fixture-fn-alpha` from the plan"))))
       (should (eq 'confirmed (plist-get result :verdict)))
       (should (string-match-p "defined: fixture-fn-alpha at .*fixture\\.el:[0-9]+"
                               (plist-get result :evidence)))))))

(ert-deftest anvil-fusion-verify-test-repo-reality-checker-symbol-missing-hint ()
  "An unknown symbol yields a hint, not a verdict."
  (anvil-fusion-verify-test--with-repo-fixture
   (lambda (root)
     (let* ((anvil-fusion-verify-repo-root root)
            (result (anvil-fusion-verify-checker-repo-reality
                     '(:claim "use fixture-fn-beta next"))))
       (should (null (plist-get result :verdict)))
       (should (string-match-p "fixture-fn-beta" (plist-get result :evidence)))))))

(ert-deftest anvil-fusion-verify-test-repo-reality-checker-no-tokens ()
  "Claims with no path or symbol token return nil."
  (anvil-fusion-verify-test--with-repo-fixture
   (lambda (root)
     (let ((anvil-fusion-verify-repo-root root))
       (should (null (anvil-fusion-verify-checker-repo-reality
                      '(:claim "write a careful implementation plan"))))))))

(ert-deftest anvil-fusion-verify-test-repo-reality-checker-root-nil ()
  "A nil repo root makes the checker inert."
  (let ((anvil-fusion-verify-repo-root nil))
    (should (null (anvil-fusion-verify-checker-repo-reality
                   '(:claim "see sub/x.org"))))))

(ert-deftest anvil-fusion-verify-test-kb-numeric-checker-kind-gated ()
  "Non-number claims are ignored by the KB numeric checker."
  (skip-unless (or (executable-find "rg") (executable-find "grep")))
  (anvil-fusion-verify-test--with-kb-fixture
   '(("kb.org" . "低圧回路の絶縁抵抗は 0.1MΩ 以上。\n"))
   (lambda (root)
     (let ((anvil-fusion-verify-kb-roots (list root)))
       (should (null (anvil-fusion-verify-checker-kb-numeric
                      '(:claim "低圧回路の絶縁抵抗は 0.1MΩ 以上" :kind fact))))))))

(ert-deftest anvil-fusion-verify-test-kb-numeric-checker-confirmed ()
  "A matching number plus local entity context confirms the claim."
  (skip-unless (or (executable-find "rg") (executable-find "grep")))
  (anvil-fusion-verify-test--with-kb-fixture
   '(("rules.org" . "低圧回路の絶縁抵抗は 0.1MΩ 以上。\n"))
   (lambda (root)
     (let* ((anvil-fusion-verify-kb-roots (list root))
            (result (anvil-fusion-verify-checker-kb-numeric
                     '(:claim "低圧回路の絶縁抵抗は 0.1MΩ 以上" :kind number))))
       (should (eq 'confirmed (plist-get result :verdict)))
       (should (string-match-p "\\`.+rules\\.org:1 — .*0\\.1MΩ"
                               (plist-get result :evidence)))))))

(ert-deftest anvil-fusion-verify-test-kb-numeric-checker-space-variant ()
  "A spaced KB unit still matches a compact claim token."
  (skip-unless (or (executable-find "rg") (executable-find "grep")))
  (anvil-fusion-verify-test--with-kb-fixture
   '(("rules.org" . "低圧回路の絶縁抵抗は 0.1 MΩ 以上。\n"))
   (lambda (root)
     (let* ((anvil-fusion-verify-kb-roots (list root))
            (result (anvil-fusion-verify-checker-kb-numeric
                     '(:claim "低圧回路の絶縁抵抗は 0.1MΩ 以上" :kind number))))
       (should (eq 'confirmed (plist-get result :verdict)))
       (should (string-match-p "\\`.+rules\\.org:1 — .*0\\.1 MΩ"
                               (plist-get result :evidence)))))))

(ert-deftest anvil-fusion-verify-test-kb-numeric-checker-window-plus-2-confirms ()
  "Entity context two lines away still confirms via the +/-2-line window."
  (skip-unless (or (executable-find "rg") (executable-find "grep")))
  (anvil-fusion-verify-test--with-kb-fixture
   '(("rules.org" . "注意書き\n0.1MΩ\n別の行\n絶縁抵抗\n"))
   (lambda (root)
     (let* ((anvil-fusion-verify-kb-roots (list root))
            (result (anvil-fusion-verify-checker-kb-numeric
                     '(:claim "低圧回路の絶縁抵抗は 0.1MΩ 以上" :kind number))))
       (should (eq 'confirmed (plist-get result :verdict)))
       (should (string-match-p "rules\\.org:2" (plist-get result :evidence)))))))

(ert-deftest anvil-fusion-verify-test-kb-numeric-checker-window-too-far-hints ()
  "Entity terms outside the +/-2-line window do not confirm the claim."
  (skip-unless (or (executable-find "rg") (executable-find "grep")))
  (anvil-fusion-verify-test--with-kb-fixture
   '(("rules.org" . "注意書き\n0.1MΩ\n二行目\n三行目\n四行目\n絶縁抵抗\n"))
   (lambda (root)
     (let* ((anvil-fusion-verify-kb-roots (list root))
            (result (anvil-fusion-verify-checker-kb-numeric
                     '(:claim "低圧回路の絶縁抵抗は 0.1MΩ 以上" :kind number))))
       (should (null (plist-get result :verdict)))
       (should (string-match-p "number found without entity context: .*rules\\.org:2"
                               (plist-get result :evidence)))))))

(ert-deftest anvil-fusion-verify-test-kb-numeric-checker-number-not-found-hint ()
  "A missing numeric token yields only a hint, never a refutation."
  (skip-unless (or (executable-find "rg") (executable-find "grep")))
  (anvil-fusion-verify-test--with-kb-fixture
   '(("rules.org" . "低圧回路の絶縁抵抗は 0.1MΩ 以上。\n"))
   (lambda (root)
     (let* ((anvil-fusion-verify-kb-roots (list root))
            (result (anvil-fusion-verify-checker-kb-numeric
                     '(:claim "低圧回路の絶縁抵抗は 100MΩ 以上" :kind number))))
       (should (null (plist-get result :verdict)))
       (should (equal "number not found in KB: 100MΩ"
                      (plist-get result :evidence)))))))

(ert-deftest anvil-fusion-verify-test-kb-numeric-checker-root-nil-and-unreadable-safe ()
  "Nil roots disable the checker; unreadable roots do not signal."
  (skip-unless (or (executable-find "rg") (executable-find "grep")))
  (let ((anvil-fusion-verify-kb-roots nil))
    (should (null (anvil-fusion-verify-checker-kb-numeric
                   '(:claim "低圧回路の絶縁抵抗は 0.1MΩ 以上" :kind number)))))
  (let ((anvil-fusion-verify-kb-roots (list "/nonexistent/anvil-fusion-verify-kb-root")))
    (let ((result (anvil-fusion-verify-checker-kb-numeric
                   '(:claim "低圧回路の絶縁抵抗は 0.1MΩ 以上" :kind number))))
      (should (null (plist-get result :verdict)))
      (should (equal "number not found in KB: 0.1MΩ"
                     (plist-get result :evidence))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-mechanical-confirmed-prunes-batch ()
  "Mechanically confirmed claims are excluded from the skeptic batch."
  (let ((anvil-fusion-verify-skeptics 2)
        (anvil-fusion-verify-sequential-skeptics nil)
        (anvil-fusion-verify-kb-roots nil)
        (anvil-fusion-verify-mechanical-checkers
         (list (lambda (claim)
                 (when (string-match-p "sub/x\\.org" (plist-get claim :claim))
                   '(:verdict confirmed :evidence "exists: sub/x.org")))))
        (submitted-tasks nil))
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                 (setq submitted-tasks tasks)
                 "b1"))
              ((symbol-function 'anvil-orchestrator-collect)
               (lambda (&rest _) t))
              ((symbol-function 'anvil-orchestrator-status)
               (lambda (_id)
                 (list :tasks
                       (mapcar (lambda (task)
                                 (list :id (plist-get task :name)
                                       :name (plist-get task :name)))
                               submitted-tasks))))
              ((symbol-function 'anvil-orchestrator-extract-result)
               (lambda (_id _full)
                 (list :status 'done :summary "VERDICT: UNVERIFIED\nREASON: "))))
      (let ((result (anvil-fusion-verify-claims
                     (list (list :claim "touch sub/x.org" :kind 'code :candidates '("A"))
                           (list :claim "unknown" :kind 'fact :candidates '("B")))
                     :question "Q")))
        (should (= 2 (length submitted-tasks)))
        (should (cl-every (lambda (task)
                            (string-prefix-p "fusion-verify-skeptic-1-" (plist-get task :name)))
                          submitted-tasks))
        (should (eq 'confirmed (plist-get (car result) :verdict)))
        (should (equal "exists: sub/x.org"
                       (plist-get (car result) :evidence)))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-kb-numeric-confirmed-prunes-batch ()
  "A KB numeric mechanical confirmation skips the skeptic batch."
  (skip-unless (or (executable-find "rg") (executable-find "grep")))
  (anvil-fusion-verify-test--with-kb-fixture
   '(("rules.org" . "低圧回路の絶縁抵抗は 0.1MΩ 以上。\n"))
   (lambda (root)
     (let ((anvil-fusion-verify-skeptics 2)
           (anvil-fusion-verify-sequential-skeptics nil)
           (anvil-fusion-verify-kb-roots (list root))
           (anvil-fusion-verify-mechanical-checkers
            (list #'anvil-fusion-verify-checker-kb-numeric))
           (submitted-tasks nil))
       (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                  (lambda (tasks)
                    (setq submitted-tasks tasks)
                    "b1"))
                 ((symbol-function 'anvil-orchestrator-collect)
                  (lambda (&rest _) t))
                 ((symbol-function 'anvil-orchestrator-status)
                  (lambda (_id)
                    (list :tasks
                          (mapcar (lambda (task)
                                    (list :id (plist-get task :name)
                                          :name (plist-get task :name)))
                                  submitted-tasks))))
                 ((symbol-function 'anvil-orchestrator-extract-result)
                  (lambda (_id _full)
                    (list :status 'done :summary "VERDICT: UNVERIFIED\nREASON: "))))
         (let ((result (anvil-fusion-verify-claims
                        (list (list :claim "低圧回路の絶縁抵抗は 0.1MΩ 以上"
                                    :kind 'number :candidates '("A"))
                              (list :claim "unknown" :kind 'fact :candidates '("B")))
                        :question "Q")))
           (should (= 2 (length submitted-tasks)))
           (should (cl-every (lambda (task)
                               (string-prefix-p "fusion-verify-skeptic-1-"
                                                (plist-get task :name)))
                             submitted-tasks))
           (should (eq 'confirmed (plist-get (car result) :verdict)))
           (should (string-match-p "rules\\.org:1"
                                   (plist-get (car result) :evidence)))))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-mechanical-all-confirmed-no-submit ()
  "All mechanically confirmed claims skip the orchestrator entirely."
  (let ((anvil-fusion-verify-kb-roots nil)
        (anvil-fusion-verify-mechanical-checkers
         (list (lambda (_claim)
                 '(:verdict confirmed :evidence "defined: fixture-fn-alpha at fixture.el:2"))))
        called)
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (_tasks) (setq called t) "b1")))
      (let ((result (anvil-fusion-verify-claims
                     (list (list :claim "one" :kind 'fact :candidates '("A"))
                           (list :claim "two" :kind 'fact :candidates '("B")))
                     :question "Q")))
        (should-not called)
        (should (cl-every (lambda (claim)
                            (eq 'confirmed (plist-get claim :verdict)))
                          result))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-preserves-input-order ()
  "Mechanically confirmed and skeptic-judged claims are returned in input order."
  (let ((anvil-fusion-verify-skeptics 1)
        (anvil-fusion-verify-kb-roots nil)
        (anvil-fusion-verify-mechanical-checkers
         (list (lambda (claim)
                 (when (member (plist-get claim :claim) '("claim-1" "claim-3"))
                   (list :verdict 'confirmed
                         :evidence (format "mechanical:%s" (plist-get claim :claim)))))))
        (submitted-tasks nil))
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                 (setq submitted-tasks tasks)
                 "b1"))
              ((symbol-function 'anvil-orchestrator-collect)
               (lambda (&rest _) t))
              ((symbol-function 'anvil-orchestrator-status)
               (lambda (_id)
                 (list :tasks
                       (mapcar (lambda (task)
                                 (list :id (plist-get task :name)
                                       :name (plist-get task :name)))
                               submitted-tasks))))
              ((symbol-function 'anvil-orchestrator-extract-result)
               (lambda (_id _full)
                 (list :status 'done :summary "VERDICT: REFUTED\nREASON: skeptic"))))
      (let* ((claims (list (list :claim "claim-1" :kind 'fact :candidates '("A"))
                           (list :claim "claim-2" :kind 'fact :candidates '("B"))
                           (list :claim "claim-3" :kind 'fact :candidates '("C"))))
             (result (anvil-fusion-verify-claims claims :question "Q")))
        (should (equal '("claim-1" "claim-2" "claim-3")
                       (mapcar (lambda (claim)
                                 (plist-get claim :claim))
                               result)))
        (should (equal '(confirmed refuted confirmed)
                       (mapcar (lambda (claim)
                                 (plist-get claim :verdict))
                               result)))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-mechanical-hint-reaches-prompt ()
  "A mechanical hint is appended to the skeptic prompt evidence block."
  (let ((anvil-fusion-verify-skeptics 1)
        (anvil-fusion-verify-kb-roots nil)
        (anvil-fusion-verify-mechanical-checkers
         (list (lambda (_claim)
                 '(:evidence "not found under ROOT: tests/missing-test.el"))))
        (submitted-tasks nil))
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                 (setq submitted-tasks tasks)
                 "b1"))
              ((symbol-function 'anvil-orchestrator-collect)
               (lambda (&rest _) t))
              ((symbol-function 'anvil-orchestrator-status)
               (lambda (_id)
                 (list :tasks
                       (mapcar (lambda (task)
                                 (list :id (plist-get task :name)
                                       :name (plist-get task :name)))
                               submitted-tasks))))
              ((symbol-function 'anvil-orchestrator-extract-result)
               (lambda (_id _full)
                 (list :status 'done
                       :summary "VERDICT: UNVERIFIED\nREASON: "))))
      (anvil-fusion-verify-claims
       (list (list :claim "use tests/missing-test.el" :kind 'code :candidates '("A")))
       :question "Q")
      (should
       (string-match-p "mechanical — not found under ROOT: tests/missing-test\\.el"
                       (plist-get (car submitted-tasks) :prompt))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-cache-hit-prunes-batch ()
  "A cached decisive claim skips skeptic tasks, is marked cached, and order stays intact."
  (anvil-fusion-verify-test--with-cache-fallback
    (let ((anvil-fusion-verify-cache-ttl-sec 60)
          (anvil-fusion-verify--cache-now-fn (lambda () 1000.0))
          (anvil-fusion-verify-skeptics 2)
          (anvil-fusion-verify-sequential-skeptics nil)
          (anvil-fusion-verify-kb-roots nil)
          (submitted-tasks nil)
          (claims (list (list :claim "cached claim" :kind 'fact :candidates '("A"))
                        (list :claim "fresh claim" :kind 'fact :candidates '("B")))))
      (anvil-fusion-verify--cache-put (car claims) 'refuted "cached evidence")
      (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                 (lambda (tasks)
                   (setq submitted-tasks tasks)
                   "b1"))
                ((symbol-function 'anvil-orchestrator-collect)
                 (lambda (&rest _) t))
                ((symbol-function 'anvil-orchestrator-status)
                 (lambda (_id)
                   (list :tasks
                         (mapcar (lambda (task)
                                   (list :id (plist-get task :name)
                                         :name (plist-get task :name)))
                                 submitted-tasks))))
                ((symbol-function 'anvil-orchestrator-extract-result)
                 (lambda (_id _full)
                   (list :status 'done
                         :summary "VERDICT: CONFIRMED\nREASON: fresh ok"))))
        (let ((result (anvil-fusion-verify-claims claims :question "Q")))
          (should (= 2 (length submitted-tasks)))
          (should
           (cl-every (lambda (task)
                       (string-prefix-p "fusion-verify-skeptic-1-"
                                        (plist-get task :name)))
                     submitted-tasks))
          (should (equal '("cached claim" "fresh claim")
                         (mapcar (lambda (claim) (plist-get claim :claim))
                                 result)))
          (should (eq 'refuted (plist-get (car result) :verdict)))
          (should (equal "cached evidence" (plist-get (car result) :evidence)))
          (should (eq t (plist-get (car result) :cached)))
          (should (eq 'confirmed (plist-get (cadr result) :verdict))))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-cache-all-hit-no-submit ()
  "All cache hits bypass the orchestrator entirely."
  (anvil-fusion-verify-test--with-cache-fallback
    (let ((anvil-fusion-verify-cache-ttl-sec 60)
          (anvil-fusion-verify--cache-now-fn (lambda () 1000.0))
          (anvil-fusion-verify-kb-roots nil)
          (claims (list (list :claim "claim-1" :kind 'fact :candidates '("A"))
                        (list :claim "claim-2" :kind 'number :candidates '("B"))))
          called)
      (anvil-fusion-verify--cache-put (nth 0 claims) 'confirmed "cache-1")
      (anvil-fusion-verify--cache-put (nth 1 claims) 'refuted "cache-2")
      (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                 (lambda (_tasks)
                   (setq called t)
                   "b1")))
        (let ((result (anvil-fusion-verify-claims claims :question "Q")))
          (should-not called)
          (should (equal '(confirmed refuted)
                         (mapcar (lambda (claim) (plist-get claim :verdict))
                                 result)))
          (should (cl-every (lambda (claim) (plist-get claim :cached))
                            result)))))))

(ert-deftest anvil-fusion-verify-test-verify-claims-caches-decisive-results-only ()
  "Decisive skeptic verdicts are cached; unverified ones are not."
  (anvil-fusion-verify-test--with-cache-fallback
    (let ((anvil-fusion-verify-cache-ttl-sec 60)
          (anvil-fusion-verify--cache-now-fn (lambda () 1000.0))
          (anvil-fusion-verify-skeptics 2)
          (anvil-fusion-verify-sequential-skeptics nil)
          (anvil-fusion-verify-kb-roots nil)
          (claims (list (list :claim "decisive claim" :kind 'fact :candidates '("A"))
                        (list :claim "unclear claim" :kind 'fact :candidates '("B")))))
      (anvil-fusion-verify-test--with-fake-batch-orchestrator
          (lambda (id _full)
            (cond
             ((equal id "fusion-verify-skeptic-0-0")
              (list :status 'done :summary "VERDICT: CONFIRMED\nREASON: yes"))
             ((equal id "fusion-verify-skeptic-0-1")
              (list :status 'done :summary "VERDICT: CONFIRMED\nREASON: yes2"))
             ((equal id "fusion-verify-skeptic-1-0")
              (list :status 'done :summary "VERDICT: CONFIRMED\nREASON: maybe"))
             ((equal id "fusion-verify-skeptic-1-1")
              (list :status 'done :summary "VERDICT: REFUTED\nREASON: maybe not"))
             (t
              (list :status 'done :summary "VERDICT: UNVERIFIED\nREASON: "))))
        (let ((result (anvil-fusion-verify-claims claims :question "Q")))
          (should (equal '(confirmed unverified)
                         (mapcar (lambda (claim) (plist-get claim :verdict))
                                 result)))
          (should (= 1 (hash-table-count anvil-fusion-verify--cache-session-store)))
          (let ((cached (car (anvil-fusion-verify-test--cache-values))))
            (should (eq 'confirmed (plist-get cached :verdict)))
            (should (equal "yes" (plist-get cached :evidence)))))))))

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
