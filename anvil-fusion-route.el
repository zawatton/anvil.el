;;; anvil-fusion-route.el --- Hard-prompt router over single ask and Fusion -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Doc 62 §4 Phase 2 ("hard-prompt router") and §6's probe-cost bound:
;; answer cheaply first, risk-check THAT answer cheaply, then pay for a
;; heavier panel only when the answer looks risky.  The motivation is
;; empirical, not aesthetic: Doc 61 Phase 6e's 2026-07-03 battery
;; measured a strong single model saturated on easy prompts (Tier A
;; plain judge 10/10) while fusion+verify cost ~4x and only paid for
;; harder multi-constraint prompts (Tier B 79% -> 100%).
;;
;; This module implements that cascade:
;;
;;   single ask -> cheap probe -> optional escalation
;;
;; Tier 1 and the probe each submit exactly ONE orchestrator task using
;; the same public-API submit -> collect -> first-task-id ->
;; extract-result pattern Phase 6a uses for claim extraction.  The
;; escalation step reuses `anvil-fusion-ask' unchanged.
;;
;; The module is load-time pure: it eagerly requires only
;; `anvil-fusion', `anvil-fusion-verify', and `anvil-fusion-ask', all of
;; which keep their orchestrator dependency lazy.  `anvil-orchestrator'
;; is required only at call time inside the thin runtime wrapper.
;;
;; Future optimization deliberately NOT in v1: when escalation happens,
;; the tier-1 answer is returned under :route :single-answer for later
;; comparison, but is not injected into the panel prompt itself.
;;
;; Wiring into optional-module registration and MCP exposure is deferred
;; by the task request; this file only provides the standalone router.

;;; Code:

(require 'cl-lib)
(require 'anvil-fusion)
(require 'anvil-fusion-verify)
(require 'anvil-fusion-ask)

(declare-function anvil-orchestrator-submit "anvil-orchestrator" (tasks))
(declare-function anvil-orchestrator-collect "anvil-orchestrator" (batch-id &rest _))
(declare-function anvil-orchestrator-extract-result "anvil-orchestrator" (task-id &optional full))

(defgroup anvil-fusion nil
  "Fusion judge harness layered over anvil-orchestrator."
  :group 'tools
  :prefix "anvil-fusion-")

(defcustom anvil-fusion-route-single-provider 'claude
  "Provider used for the tier-1 single ask."
  :type 'symbol
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-single-model nil
  "Model used for the tier-1 single ask, or nil for the provider default.
Unlike the probe tier, this is passed through as-is when non-nil."
  :type '(choice (const :tag "Provider default" nil) string)
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-probe-provider 'claude
  "Provider used for the cheap answer-risk probe."
  :type 'symbol
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-probe-model "haiku"
  "Model used for the cheap answer-risk probe when the provider is `claude'.
This is a claude-scoped alias, so it is applied only when the
effective probe provider is `claude'."
  :type '(choice (const :tag "Provider default" nil) string)
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-probe-template
  "あなたは回答の危険度を判定する監査役です。原問と回答を見て、(a) 独立した専門家が回答中の結論を左右する主張に異を唱える見込み、(b) 検証されていない重要な数値・条文・API 名の存在、(c) 多制約の取りこぼしの兆候、を評価してください。
Output EXACTLY two lines:
RISK: LOW|MEDIUM|HIGH
REASON: <one line>

# 原問
%s

# 回答
%s"
  "Probe prompt template for routing.
Contains exactly two `%s' slots: (1) the original prompt, (2) the
tier-1 single answer."
  :type 'string
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-escalation
  '((low    . (:tier single))
    (medium . (:tier panel :panel claude-pair :verify nil))
    (high   . (:tier panel :panel opus-solo  :verify t)))
  "Risk-to-action calibration knob for `anvil-fusion-route-ask'."
  :type '(alist :key-type symbol :value-type plist)
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-force-hard-regexps nil
  "Regexps whose match forces direct execution of the `high' action.
Intended for domain-critical prompt markers wired by the user in init."
  :type '(repeat regexp)
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-log-size 100
  "Maximum number of routing decisions retained in `anvil-fusion-route--log'."
  :type 'integer
  :group 'anvil-fusion)

(defconst anvil-fusion-route--risk-line-regexp
  "risk[ \t]*:[ \t]*\\([[:alpha:]]+\\)"
  "Case-insensitive regexp matching a probe `RISK:' line anywhere.")

(defconst anvil-fusion-route--reason-line-regexp
  "reason[ \t]*:[ \t]*\\(.*\\)"
  "Case-insensitive regexp matching a probe `REASON:' line anywhere.")

(defvar anvil-fusion-route--log nil
  "Newest-first routing decision log.
Each entry is a plist:
  (:time FLOAT :prompt-head STRING :risk SYMBOL :reason STRING
   :tier SYMBOL :panel SYMBOL-OR-NIL :verify BOOL)")

(defun anvil-fusion-route--probe-prompt (question answer)
  "Build the routing probe prompt from QUESTION and ANSWER."
  (format anvil-fusion-route-probe-template
          (or question "")
          (or answer "")))

(defun anvil-fusion-route--parse-risk (text)
  "Tolerantly parse probe TEXT into (:risk SYM :reason STR).
Searches for `RISK:' / `REASON:' labels anywhere, case-insensitively.
Unknown or absent risk normalizes to `medium'; absent reason to \"\"."
  (let ((case-fold-search t)
        (risk 'medium)
        (reason ""))
    (when (stringp text)
      (when (string-match anvil-fusion-route--risk-line-regexp text)
        (let ((parsed (intern-soft (downcase (match-string 1 text)))))
          (when (memq parsed '(low medium high))
            (setq risk parsed))))
      (when (string-match anvil-fusion-route--reason-line-regexp text)
        (setq reason (string-trim (match-string 1 text)))))
    (list :risk risk :reason reason)))

(defun anvil-fusion-route--decide (risk)
  "Return the action plist for RISK from `anvil-fusion-route-escalation'.
Unknown RISK keys fall back to the `medium' action."
  (copy-tree
   (or (cdr (assq risk anvil-fusion-route-escalation))
       (cdr (assq 'medium anvil-fusion-route-escalation)))))

(defun anvil-fusion-route--force-hard-p (prompt)
  "Return non-nil when PROMPT matches any force-hard regexp."
  (and (stringp prompt)
       (cl-some (lambda (re) (string-match-p re prompt))
                anvil-fusion-route-force-hard-regexps)))

(defun anvil-fusion-route--probe-model-for (provider)
  "Return the effective probe model for PROVIDER."
  (and (eq provider 'claude) anvil-fusion-route-probe-model))

(defun anvil-fusion-route--prompt-head (prompt)
  "Return the first 80 chars of PROMPT for logging."
  (substring (or prompt "") 0 (min 80 (length (or prompt "")))))

(defun anvil-fusion-route--push-log (prompt risk reason action)
  "Record one routing decision for PROMPT / RISK / REASON / ACTION."
  (let ((entry (list :time (float-time)
                     :prompt-head (anvil-fusion-route--prompt-head prompt)
                     :risk risk
                     :reason (or reason "")
                     :tier (plist-get action :tier)
                     :panel (plist-get action :panel)
                     :verify (and (plist-get action :verify) t))))
    (push entry anvil-fusion-route--log)
    (when (> (length anvil-fusion-route--log) anvil-fusion-route-log-size)
      (setq anvil-fusion-route--log
            (cl-subseq anvil-fusion-route--log 0 anvil-fusion-route-log-size)))
    (message "anvil-fusion-route: risk=%s tier=%s panel=%s verify=%s reason=%s"
             risk
             (plist-get action :tier)
             (or (plist-get action :panel) "-")
             (if (plist-get action :verify) "t" "nil")
             (or reason ""))
    entry))

(defun anvil-fusion-route-log ()
  "Return a copy of the routing decision log."
  (copy-tree anvil-fusion-route--log))

(cl-defun anvil-fusion-route--run-single-task
    (name prompt provider &key model cwd timeout-sec (max-wait-sec 1800))
  "Submit one orchestrator task and return its full result plist.
NAME, PROMPT, and PROVIDER define the task.  MODEL / CWD /
TIMEOUT-SEC / MAX-WAIT-SEC are forwarded when present."
  (require 'anvil-orchestrator)
  (let* ((task (append (list :name name :provider provider :prompt prompt)
                       (and model (list :model model))
                       (and cwd (list :cwd cwd))
                       (and timeout-sec (list :timeout-sec timeout-sec))))
         (batch (anvil-orchestrator-submit (list task))))
    (anvil-orchestrator-collect batch :wait t :max-wait-sec max-wait-sec)
    (anvil-orchestrator-extract-result
     (anvil-fusion--batch-first-task-id batch) t)))

(defun anvil-fusion-route--single-failure-route (prompt reason action ask-args)
  "Escalate on tier-1 failure using ACTION and ASK-ARGS."
  (let* ((route (list :risk 'high
                      :reason reason
                      :tier (plist-get action :tier)
                      :panel (plist-get action :panel)
                      :verify (and (plist-get action :verify) t)
                      :single-answer nil))
         (result (apply #'anvil-fusion-ask prompt
                        :panel (plist-get action :panel)
                        :verify (plist-get action :verify)
                        ask-args)))
    (anvil-fusion-route--push-log prompt 'high reason action)
    (append result (list :route route))))

(cl-defun anvil-fusion-route-ask
    (prompt &key cwd timeout-sec (max-wait-sec 1800) verify-args lenses extra)
  "Answer PROMPT via a routed cascade.
Run a single ask first, probe that answer cheaply, then escalate
only when the probe says the answer is risky.

Returns a plist whose :answer is the final answer and whose :route
describes the routing decision:
  (:risk SYM :reason STR :tier SYM :panel SYMBOL-OR-NIL
   :verify BOOL :single-answer STR-OR-NIL)."
  (let* ((high-action (anvil-fusion-route--decide 'high))
         (ask-args (append (and cwd (list :cwd cwd))
                           (and timeout-sec (list :timeout-sec timeout-sec))
                           (list :max-wait-sec max-wait-sec)
                           (and verify-args (list :verify-args verify-args))
                           (and lenses (list :lenses lenses))
                           (and extra (list :extra extra)))))
    (if (anvil-fusion-route--force-hard-p prompt)
        (let* ((route (list :risk 'forced
                            :reason "force-hard regexp matched"
                            :tier (plist-get high-action :tier)
                            :panel (plist-get high-action :panel)
                            :verify (and (plist-get high-action :verify) t)
                            :single-answer nil))
               (result (apply #'anvil-fusion-ask prompt
                              :panel (plist-get high-action :panel)
                              :verify (plist-get high-action :verify)
                              ask-args)))
          (anvil-fusion-route--push-log prompt 'forced "force-hard regexp matched" high-action)
          (append result (list :route route)))
      (condition-case nil
          (let* ((single-result
                  (anvil-fusion-route--run-single-task
                   "fusion-route-single"
                   prompt
                   anvil-fusion-route-single-provider
                   :model anvil-fusion-route-single-model
                   :cwd cwd
                   :timeout-sec timeout-sec
                   :max-wait-sec max-wait-sec))
                 (single-status (plist-get single-result :status))
                 (single-answer (plist-get single-result :summary)))
            (if (or (not (eq single-status 'done))
                    (not (stringp single-answer)))
                (anvil-fusion-route--single-failure-route
                 prompt
                 "single ask failed"
                 high-action
                 ask-args)
              (let* ((probe-result
                      (condition-case nil
                          (anvil-fusion-route--run-single-task
                           "fusion-route-probe"
                           (anvil-fusion-route--probe-prompt prompt single-answer)
                           anvil-fusion-route-probe-provider
                           :model (anvil-fusion-route--probe-model-for
                                   anvil-fusion-route-probe-provider)
                           :cwd cwd
                           :timeout-sec timeout-sec
                           :max-wait-sec max-wait-sec)
                        (error nil)))
                     (probe-status (and probe-result (plist-get probe-result :status)))
                     (probe-summary (and probe-result (plist-get probe-result :summary)))
                     (probe-parsed (if (and probe-result (eq probe-status 'done))
                                       (anvil-fusion-route--parse-risk probe-summary)
                                     (list :risk 'medium :reason "probe failed")))
                     (action (anvil-fusion-route--decide
                              (plist-get probe-parsed :risk)))
                     (route (list :risk (plist-get probe-parsed :risk)
                                  :reason (plist-get probe-parsed :reason)
                                  :tier (plist-get action :tier)
                                  :panel (plist-get action :panel)
                                  :verify (and (plist-get action :verify) t)
                                  :single-answer single-answer)))
                (anvil-fusion-route--push-log
                 prompt
                 (plist-get probe-parsed :risk)
                 (plist-get probe-parsed :reason)
                 action)
                (if (eq (plist-get action :tier) 'single)
                    (list :answer single-answer :route route)
                  (append
                   (apply #'anvil-fusion-ask prompt
                          :panel (plist-get action :panel)
                          :verify (plist-get action :verify)
                          ask-args)
                   (list :route route))))))
        (error
         (anvil-fusion-route--single-failure-route
          prompt
          "single ask failed"
          high-action
          ask-args))))))

(provide 'anvil-fusion-route)
;;; anvil-fusion-route.el ends here
