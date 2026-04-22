;;; anvil-orchestrator-routing.el --- Provider latency routing for the orchestrator  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 22 Phase 1 — pluggable provider selector that reads the live
;; `anvil-orchestrator-stats' table and picks a provider for tasks
;; submitted with `:provider 'auto'.
;;
;; The module is pure with respect to the orchestrator's task table:
;; it only reads via `anvil-orchestrator-stats', never mutates.  The
;; orchestrator itself stamps `:routing-policy' / `:routing-chose' /
;; `:routing-candidates' on the task plist when `:provider 'auto' is
;; resolved (see `anvil-orchestrator--coerce-task').
;;
;; Public entry points:
;;   (anvil-orchestrator-select-provider &key prompt policy candidates
;;                                       since min-samples stats-fn)
;;   -> plist (:provider SYM :policy SYM :candidates (SYM...)
;;             :reason STRING :per-candidate ALIST :cold-start BOOL)
;;
;; MCP tool (dry-run):
;;   orchestrator-routing-select
;;
;; Design:  docs/design/22-provider-routing.org
;; Phase 2 (persistence + auto-top-n consensus) and Phase 3 (adaptive
;; coefficient) intentionally stay defcustom-only for now.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-server)

(declare-function anvil-orchestrator-stats "anvil-orchestrator" (&rest args))
(defvar anvil-orchestrator--providers)


;;;; --- group + defcustoms ---------------------------------------------------

(defgroup anvil-orchestrator-routing nil
  "Stats-driven provider selection for `anvil-orchestrator'."
  :group 'anvil-orchestrator
  :prefix "anvil-orchestrator-routing-")

(defcustom anvil-orchestrator-routing-default-policy 'balanced
  "Policy used when a task arrives with `:provider' = `auto' and no `:policy'.
Legal values: `speed', `cost', `balanced', `quality', or a function
`(fn prompt candidates per-candidate) -> provider-symbol'."
  :type '(choice (const speed) (const cost) (const balanced) (const quality)
                 function)
  :group 'anvil-orchestrator-routing)

(defcustom anvil-orchestrator-routing-balance-coefficient 1000
  "Dollars-to-milliseconds weight for the `balanced' policy.
Score = p50-ms + cost-usd * COEFFICIENT.  Default 1000 means $1 of
cost is worth 1 wall-second of latency — tune up to prefer speed,
down to prefer savings."
  :type 'integer
  :group 'anvil-orchestrator-routing)

(defcustom anvil-orchestrator-routing-quality-order
  '(claude codex gemini aider ollama)
  "Preference list for the `quality' policy.
The first registered candidate appearing in this list wins,
regardless of measured latency or samples."
  :type '(repeat symbol)
  :group 'anvil-orchestrator-routing)

(defcustom anvil-orchestrator-routing-cold-start-provider 'claude
  "Fallback provider when no candidate meets `:min-samples' in the window."
  :type 'symbol
  :group 'anvil-orchestrator-routing)

(defcustom anvil-orchestrator-routing-min-samples 3
  "Default per-provider sample floor before a policy will consider it.
Raise for noisier tail-latency environments; lower for fresh installs."
  :type 'integer
  :group 'anvil-orchestrator-routing)

(defcustom anvil-orchestrator-routing-default-since-sec 3600
  "Default lookback window (seconds) for stats when `:since' is omitted.
Routing reads the live stats table over this sliding window."
  :type 'integer
  :group 'anvil-orchestrator-routing)


;;;; --- internals ------------------------------------------------------------

(defconst anvil-orchestrator-routing--server-id "emacs-eval"
  "Server id under which routing MCP tools register.")

(defun anvil-orchestrator-routing--candidates ()
  "Return the full list of registered provider symbols."
  (when (boundp 'anvil-orchestrator--providers)
    (mapcar #'car anvil-orchestrator--providers)))

(defun anvil-orchestrator-routing--collect-samples (candidates since min-samples stats-fn)
  "Call STATS-FN per CANDIDATE with SINCE filter; keep those with >= MIN-SAMPLES.
Returns an alist ((PROV :total N :p50 MS :avg MS :cost-usd TOTAL) ...)."
  (let (kept)
    (dolist (prov candidates)
      (let* ((stats (funcall stats-fn :provider prov :since since))
             (total (plist-get stats :total))
             (p50   (plist-get stats :elapsed-ms-p50))
             (avg   (plist-get stats :elapsed-ms-avg))
             (cost  (plist-get stats :total-cost-usd)))
        (when (and (integerp total) (>= total min-samples))
          (push (list prov
                      :total total
                      :p50 p50
                      :avg avg
                      :cost-usd (or cost 0.0))
                kept))))
    (nreverse kept)))

(defun anvil-orchestrator-routing--policy-speed (per-candidate)
  "Pick the provider with the lowest p50 (falling back to avg)."
  (let ((best nil) (best-score nil))
    (dolist (row per-candidate)
      (let* ((prov (car row))
             (plist (cdr row))
             (score (or (plist-get plist :p50)
                        (plist-get plist :avg))))
        (when (and (numberp score)
                   (or (null best-score) (< score best-score)))
          (setq best prov best-score score))))
    best))

(defun anvil-orchestrator-routing--policy-cost (per-candidate)
  "Pick the provider with the lowest total cost, breaking ties by p50."
  (let ((sorted (sort (copy-sequence per-candidate)
                      (lambda (a b)
                        (let* ((ca (or (plist-get (cdr a) :cost-usd) 0))
                               (cb (or (plist-get (cdr b) :cost-usd) 0))
                               (pa (or (plist-get (cdr a) :p50)
                                       (plist-get (cdr a) :avg)
                                       most-positive-fixnum))
                               (pb (or (plist-get (cdr b) :p50)
                                       (plist-get (cdr b) :avg)
                                       most-positive-fixnum)))
                          (if (= ca cb) (< pa pb) (< ca cb)))))))
    (car (car sorted))))

(defun anvil-orchestrator-routing--policy-balanced (per-candidate coefficient)
  "Pick the provider minimising p50-ms + cost-usd * COEFFICIENT."
  (let ((best nil) (best-score nil))
    (dolist (row per-candidate)
      (let* ((prov (car row))
             (plist (cdr row))
             (p50 (or (plist-get plist :p50)
                      (plist-get plist :avg)))
             (cost (or (plist-get plist :cost-usd) 0)))
        (when (numberp p50)
          (let ((score (+ (float p50) (* (float cost) coefficient))))
            (when (or (null best-score) (< score best-score))
              (setq best prov best-score score))))))
    best))

(defun anvil-orchestrator-routing--policy-quality (candidates order)
  "Pick the first member of ORDER that is a CANDIDATES member."
  (seq-find (lambda (prov) (memq prov candidates)) order))


;;;; --- public API -----------------------------------------------------------

;;;###autoload
(cl-defun anvil-orchestrator-select-provider
    (&key prompt policy candidates since min-samples stats-fn)
  "Return a routing decision plist for PROMPT + POLICY.

Arguments (all keyword, all optional except when noted):
  PROMPT       Informational only for the stock policies; a
               `function' policy receives it.
  POLICY       One of `speed' `cost' `balanced' `quality' or a
               function `(fn PROMPT CANDIDATES PER-CANDIDATE)';
               defaults to `anvil-orchestrator-routing-default-policy'.
  CANDIDATES   List of provider symbols to consider.  Default: all
               registered providers.
  SINCE        Float-time cutoff; default
               `anvil-orchestrator-routing-default-since-sec' ago.
  MIN-SAMPLES  Per-candidate sample floor; default
               `anvil-orchestrator-routing-min-samples'.
  STATS-FN     Injection seam — function compatible with
               `anvil-orchestrator-stats'.  Defaults to that.

Returns a plist:
  :provider       SYMBOL or nil (caller decides what to do on nil)
  :policy         SYMBOL or `user' for a function policy
  :candidates     list of symbols actually considered
  :per-candidate  alist described above
  :cold-start     t when falling back due to insufficient samples
  :reason         short human-readable explanation"
  (let* ((pol (or policy anvil-orchestrator-routing-default-policy))
         (cands (or candidates (anvil-orchestrator-routing--candidates)))
         (since (or since (- (float-time)
                             anvil-orchestrator-routing-default-since-sec)))
         (min-n (or min-samples anvil-orchestrator-routing-min-samples))
         (stats (or stats-fn #'anvil-orchestrator-stats))
         (per (anvil-orchestrator-routing--collect-samples
               cands since min-n stats))
         (picked nil)
         (cold nil)
         (reason nil))
    (cond
     ;; Quality: static ordering, no sample requirement.
     ((eq pol 'quality)
      (setq picked (anvil-orchestrator-routing--policy-quality
                    cands anvil-orchestrator-routing-quality-order)
            reason (if picked
                       (format "quality: first of %S present in candidates"
                               anvil-orchestrator-routing-quality-order)
                     "quality: no candidate appears in quality-order")))
     ;; Function policy: caller decides.  Given full per-candidate.
     ((functionp pol)
      (setq picked (funcall pol prompt cands per)
            reason "user-provided function policy"))
     ;; Insufficient samples across the board: cold-start fallback.
     ((null per)
      (setq picked (and (memq anvil-orchestrator-routing-cold-start-provider
                              cands)
                        anvil-orchestrator-routing-cold-start-provider)
            cold t
            reason (format "cold-start: %d candidates but none hit >=%d samples since %.0f"
                           (length cands) min-n since)))
     ((eq pol 'speed)
      (setq picked (anvil-orchestrator-routing--policy-speed per)
            reason "speed: min p50 (fallback avg) among sampled"))
     ((eq pol 'cost)
      (setq picked (anvil-orchestrator-routing--policy-cost per)
            reason "cost: min total-cost-usd, tie-break p50"))
     ((eq pol 'balanced)
      (setq picked (anvil-orchestrator-routing--policy-balanced
                    per anvil-orchestrator-routing-balance-coefficient)
            reason (format "balanced: min p50 + cost*%d"
                           anvil-orchestrator-routing-balance-coefficient)))
     (t
      (setq reason (format "unknown policy %S — returning nil" pol))))
    (list :provider picked
          :policy (if (functionp pol) 'user pol)
          :candidates cands
          :per-candidate per
          :cold-start cold
          :reason reason)))


;;;; --- MCP tool -------------------------------------------------------------

(defun anvil-orchestrator-routing--parse-symbol-list (s)
  "Parse S — comma-separated names — into a list of provider symbols.
Empty / nil returns nil."
  (when (and (stringp s) (not (string-empty-p s)))
    (mapcar #'intern
            (split-string s "[ ,]+" t "[ \t\n]+"))))

(defun anvil-orchestrator-routing--coerce-policy (s)
  "Coerce string S to a policy symbol; empty / nil -> nil."
  (when (and (stringp s) (not (string-empty-p s)))
    (intern s)))

(defun anvil-orchestrator-routing--tool-select
    (&optional prompt policy candidates since_sec min_samples)
  "MCP wrapper: dry-run provider selection with reasoning.

MCP Parameters:
  prompt      - Optional informational prompt text (forwarded to
                function policies; ignored by stock policies).
  policy      - Optional policy name: \"speed\", \"cost\",
                \"balanced\", \"quality\".  Default: configured
                `anvil-orchestrator-routing-default-policy'.
  candidates  - Optional comma-separated provider list
                (\"claude,codex,ollama\").  Default: every
                registered provider.
  since_sec   - Optional lookback window as seconds-ago string.
                Default: 3600 (1 hour).
  min_samples - Optional integer-as-string floor.  Default: 3."
  (anvil-server-with-error-handling
    (let* ((pol (anvil-orchestrator-routing--coerce-policy policy))
           (cands (anvil-orchestrator-routing--parse-symbol-list candidates))
           (since (and (stringp since_sec)
                       (not (string-empty-p since_sec))
                       (let ((n (string-to-number since_sec)))
                         (and (> n 0) (- (float-time) n)))))
           (min-n (and (stringp min_samples)
                       (not (string-empty-p min_samples))
                       (let ((n (string-to-number min_samples)))
                         (and (> n 0) n))))
           (result (anvil-orchestrator-select-provider
                    :prompt prompt
                    :policy pol
                    :candidates cands
                    :since since
                    :min-samples min-n)))
      result)))


;;;; --- lifecycle ------------------------------------------------------------

(defun anvil-orchestrator-routing--register-tools ()
  "Register routing-* MCP tools under the emacs-eval server."
  (anvil-server-register-tool
   #'anvil-orchestrator-routing--tool-select
   :id "orchestrator-routing-select"
   :intent '(orchestrator admin)
   :layer 'workflow
   :server-id anvil-orchestrator-routing--server-id
   :description
   "Dry-run provider selector.  Returns the provider a given policy
would pick right now, plus the reasoning plist (candidates, per-
candidate stats, policy, cold-start flag).  Does NOT submit a task —
use :provider 'auto on orchestrator-submit* tools for that.  Phase 1
of Doc 22 provider latency routing."
   :read-only t))

(defun anvil-orchestrator-routing--unregister-tools ()
  (anvil-server-unregister-tool
   "orchestrator-routing-select"
   anvil-orchestrator-routing--server-id))

;;;###autoload
(defun anvil-orchestrator-routing-enable ()
  "Register the Doc 22 routing MCP tools."
  (interactive)
  (anvil-orchestrator-routing--register-tools))

;;;###autoload
(defun anvil-orchestrator-routing-disable ()
  "Unregister the Doc 22 routing MCP tools."
  (interactive)
  (anvil-orchestrator-routing--unregister-tools))

(provide 'anvil-orchestrator-routing)

;;; anvil-orchestrator-routing.el ends here
