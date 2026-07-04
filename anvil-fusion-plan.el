;;; anvil-fusion-plan.el --- Plan fusion wrapper over anvil-fusion-ask -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Doc 61 §4 Phase 8a ("Plan fusion").  This module fans a task out as
;; implementation-plan requests, then reuses the verifier-grounded judge
;; plumbing to merge those plans into one verified plan.

;;; Code:

(require 'anvil-fusion)
(require 'anvil-fusion-verify)
(require 'anvil-fusion-ask)

(defgroup anvil-fusion nil
  "Fusion judge harness layered over anvil-orchestrator."
  :group 'tools
  :prefix "anvil-fusion-")

(defcustom anvil-fusion-plan-member-template
  "あなたは実装計画の立案者です。
以下のタスクに対する**実装計画**を出力してください（最終成果物そのものではなく計画）。
含めるもの: (1) 番号付き手順、(2) 触るファイル（既存 or 新規を明記）、
(3) 使う既存 API・関数名、(4) リスク・不明点。過度な一般論は書かず、このタスク固有の
計画にすること。

%s"
  "Template wrapped around each plan-member task.
Contains exactly one `%s' slot for the raw task text."
  :type 'string
  :group 'anvil-fusion)

(defcustom anvil-fusion-plan-judge-template
  "あなたは実装計画を統合する判定者です。

# 原問
%s

# 候補回答
%s

# 検証済み主張表
{{CLAIMS}}

# 手順
以下を順に行ってください。
1. 合意の背骨 — 複数計画が一致する手順を背骨として採用。
2. 相違の解決 — 食い違う手順は根拠と検証済み主張表を用いてどちらを採るか判定。REFUTED な主張（存在しない API・ファイル等）に依存する手順をマージ計画に残してはならない。CONFIRMED な主張（実在確認済みの API・ファイル）に基づく手順を優先。
3. 良い独自案の移植 — 単一の計画のみにある価値ある手順は背骨に統合。
4. リスク統合 — 各計画のリスク・不明点を集約。

# 出力形式
## 分析
（簡潔に）
## 統合計画
（番号付き手順・触るファイル・使う API・リスク）"
  "Verified judge template used for plan fusion.
Contains exactly two `%s' slots plus the literal `{{CLAIMS}}' marker."
  :type 'string
  :group 'anvil-fusion)

(cl-defun anvil-fusion-plan
    (task &key panel verify verify-args repo-root lenses cwd
          judge judge-model extra fidelity timeout-sec
          (max-wait-sec 1800) max-rounds converge-threshold)
  "Return a fused implementation plan plist for TASK.

TASK is first wrapped in `anvil-fusion-plan-member-template' so panel
members answer with implementation plans rather than direct solutions.
With VERIFY nil, the wrapper pre-builds the judge template from
`anvil-fusion-plan-judge-template' plus an empty claims block and
passes that as :template.  With VERIFY non-nil, it leaves :template
unset and instead forwards `anvil-fusion-plan-judge-template' as
:verify-base-template so `anvil-fusion-ask' can build the verified
judge prompt from extracted/annotated claims; if extraction yields no
claims and no explicit :template was provided, `anvil-fusion-ask'
falls back to that same base template with an empty claims block
rather than the generic answer template.  During the call, REPO-ROOT
and `anvil-fusion-verify-checker-repo-reality' are let-bound into the
mechanical verification layer so plan claims can be checked against
repository reality.  The returned plist is `anvil-fusion-ask''s result
plus the convenience alias :plan, equal to :answer."
  (let ((member-prompt (format anvil-fusion-plan-member-template task))
        (template (unless verify
                    (anvil-fusion-verify-judge-template-for
                     nil anvil-fusion-plan-judge-template))))
    (let ((anvil-fusion-verify-repo-root repo-root)
          (anvil-fusion-verify-mechanical-checkers
           (cons #'anvil-fusion-verify-checker-repo-reality
                 anvil-fusion-verify-mechanical-checkers)))
      (let ((result (anvil-fusion-ask
                     member-prompt
                     :panel panel
                     :fidelity fidelity
                     :judge judge
                     :judge-model judge-model
                     :extra extra
                     :lenses lenses
                     :cwd cwd
                     :max-rounds max-rounds
                     :converge-threshold converge-threshold
                     :timeout-sec timeout-sec
                     :max-wait-sec max-wait-sec
                     :template template
                     :verify verify
                     :verify-args verify-args
                     :verify-base-template anvil-fusion-plan-judge-template)))
        (append result (list :plan (plist-get result :answer)))))))

(provide 'anvil-fusion-plan)
;;; anvil-fusion-plan.el ends here
