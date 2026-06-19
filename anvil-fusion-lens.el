;;; anvil-fusion-lens.el --- role-lens prefixes for panel members -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion

;;; Commentary:
;;
;; Phase 4 (part 1) of docs/design/01-fusion-harness.org: role-lens
;; injection.
;;
;; Diversity is ~1/4 of the ensemble lift.  Beyond using different
;; models, we can make each panel member explore a DIFFERENT facet of
;; the question by prepending a fixed "lens" (role) to its prompt — the
;; "役割と境界はエージェント側に固定する" idea (X #3).  This buys
;; diversity even when several members share a model (e.g. the sovereign
;; panel's three ollama models).
;;
;; A lens is referenced by symbol (looked up in `anvil-fusion-lenses'),
;; given as a literal prefix string, or nil (no lens).  Pure module.

;;; Code:

(defgroup anvil-fusion nil
  "Fusion judge harness layered over anvil-orchestrator."
  :group 'tools
  :prefix "anvil-fusion-")

(defcustom anvil-fusion-lenses
  '((correctness
     . "あなたは正確性を最優先するレビュアーです。事実誤り、数値や条文番号の誤り、\
論理の飛躍を徹底的に排除し、確実に正しいと言える内容のみで回答してください。")
    (completeness
     . "あなたは網羅性を最優先します。見落とされがちな例外・境界条件・前提・\
関連論点を漏れなく洗い出した上で回答してください。")
    (domain
     . "あなたは電気管理技術者（電気主任技術者）の実務専門家です。電気事業法・\
電気設備技術基準とその解釈・JEAC 等の根拠条文を引用しつつ、現場運用の観点から\
回答してください。")
    (skeptic
     . "あなたは批判的検証役です。安易な結論や通説を疑い、反例・リスク・失敗\
モードを指摘した上で回答してください。"))
  "Named role lenses.
An alist of LENS-SYMBOL -> PREFIX-STRING.  A lens is prepended to
a panel member's prompt so each member attacks the question from a
fixed angle, adding diversity on top of model differences."
  :type '(alist :key-type symbol :value-type string)
  :group 'anvil-fusion)

(defun anvil-fusion-apply-lens (prompt lens)
  "Return PROMPT prefixed with LENS.
LENS may be nil (return PROMPT unchanged), a literal prefix
string, or a symbol resolved through `anvil-fusion-lenses'.  An
unknown lens symbol signals `user-error' so typos surface rather
than silently dropping the role."
  (let ((prefix
         (cond
          ((null lens) nil)
          ((stringp lens) lens)
          ((symbolp lens)
           (or (cdr (assq lens anvil-fusion-lenses))
               (user-error "anvil-fusion: unknown lens %S (known: %s)"
                           lens (mapconcat (lambda (l) (symbol-name (car l)))
                                           anvil-fusion-lenses ", "))))
          (t (user-error "anvil-fusion: bad lens %S" lens)))))
    (if (and prefix (not (string-empty-p prefix)))
        (concat prefix "\n\n" prompt)
      prompt)))

(provide 'anvil-fusion-lens)
;;; anvil-fusion-lens.el ends here
