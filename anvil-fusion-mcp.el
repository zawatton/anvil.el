;;; anvil-fusion-mcp.el --- MCP tool wrappers for anvil-fusion -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion, mcp

;;; Commentary:
;;
;; Registers the fusion ask as MCP tools under the "emacs-eval" server,
;; following the anvil-orchestrator pattern (single required arg + N
;; optionals, "MCP Parameters:" docstrings, result JSON-encoded only at
;; the transport boundary).
;;
;; Because a full ask runs tens of seconds to minutes (it would blow the
;; MCP dispatch timeout), the tools are the async pair from
;; `anvil-fusion-async':
;;
;;   fusion-ask    -> submit a fusion ask, returns a job id immediately
;;   fusion-result -> poll a job id until :status is "done"
;;
;; This module is daemon-only: it requires `anvil-server' (for the
;; `anvil-server-with-error-handling' macro and registration) and is not
;; part of the isolated byte-compile / ERT run.  Call
;; `anvil-fusion-enable' once (e.g. from init or after load) to register.

;;; Code:

(require 'anvil-server)
(require 'anvil-fusion-async)

(defconst anvil-fusion--server-id "emacs-eval"
  "MCP server id the fusion tools register under (must match the
stdio shim's --server-id, like anvil-orchestrator).")

;;;; --- JSON encoding (self-contained) --------------------------------------

(defun anvil-fusion--plist-p (x)
  "Return non-nil when X is a keyword-keyed plist."
  (and (consp x) (keywordp (car x))))

(defun anvil-fusion--to-json (x)
  "Convert X into a `json-encode'-friendly shape.
Keywords/symbols -> strings, plists -> alists (string keys),
plain lists -> vectors (JSON arrays)."
  (cond
   ((null x) nil)
   ((eq x t) t)
   ((numberp x) x)
   ((stringp x) x)
   ((keywordp x) (substring (symbol-name x) 1))
   ((symbolp x) (symbol-name x))
   ((anvil-fusion--plist-p x)
    (let (alist (p x))
      (while p
        (push (cons (substring (symbol-name (car p)) 1)
                    (anvil-fusion--to-json (cadr p)))
              alist)
        (setq p (cddr p)))
      (nreverse alist)))
   ((listp x) (vconcat (mapcar #'anvil-fusion--to-json x)))
   (t (format "%S" x))))

(defun anvil-fusion--encode-for-mcp (value)
  "Encode VALUE as a JSON string for MCP transport (strings pass through)."
  (cond
   ((or (null value) (stringp value)) value)
   (t (json-encode (anvil-fusion--to-json value)))))

(defun anvil-fusion--encode-handler (handler)
  "Wrap HANDLER so its rich plist result is JSON-encoded for MCP."
  (anvil-server--make-encoded-handler
   handler #'anvil-fusion--encode-for-mcp))

;;;; --- argument coercion ---------------------------------------------------

(defun anvil-fusion--arg-sym (s)
  "Intern non-empty string S to a symbol, else nil."
  (and (stringp s) (not (string-empty-p s)) (intern s)))

(defun anvil-fusion--arg-list (s)
  "Split comma/space separated string S into a list of symbols, else nil."
  (and (stringp s) (not (string-empty-p s))
       (mapcar #'intern (split-string s "[, ]+" t))))

;;;; --- MCP tool wrappers ---------------------------------------------------

(defun anvil-fusion--tool-ask (prompt &optional panel fidelity lenses judge extra)
  "Submit a Fusion ask over a model panel; return a job id to poll.

MCP Parameters:
  prompt   - The question to answer (string, required).
  panel    - Panel name: \"sovereign\" (default, local / zero-egress),
             \"quality\", or \"budget\".
  fidelity - \"summary\" (default) or \"full\" (untruncated candidate
             answers; for code / long-form).
  lenses   - Comma-separated role lenses, e.g.
             \"correctness,completeness,domain\" (applied to members
             by position).
  judge    - Override judge provider name; refused for a local-only
             panel when non-local.
  extra    - Extra instruction appended to the judge prompt.

Returns a plist (:job-id :stage :panel :egress).  Poll the job id
with the `fusion-result' tool until :status is \"done\"."
  (anvil-server-with-error-handling
   (apply #'anvil-fusion-ask-async prompt
          (append (let ((p (anvil-fusion--arg-sym panel)))   (and p (list :panel p)))
                  (let ((f (anvil-fusion--arg-sym fidelity))) (and f (list :fidelity f)))
                  (let ((l (anvil-fusion--arg-list lenses)))  (and l (list :lenses l)))
                  (let ((j (anvil-fusion--arg-sym judge)))    (and j (list :judge j)))
                  (and extra (stringp extra) (not (string-empty-p extra))
                       (list :extra extra))))))

(defun anvil-fusion--tool-result (job_id)
  "Poll a Fusion ask job and return its current status / answer.

MCP Parameters:
  job_id - Job id returned by `fusion-ask'.

Returns a plist whose :status is \"running\" / \"done\" / \"error\";
when \"done\" it carries :answer (the synthesized reply), :panel,
:egress and :n-candidates."
  (anvil-server-with-error-handling
   (anvil-fusion-result job_id)))

;;;; --- lifecycle -----------------------------------------------------------

(defun anvil-fusion--register-tools ()
  "Register the fusion MCP tools under the emacs-eval server."
  (anvil-server-register-tool
   (anvil-fusion--encode-handler #'anvil-fusion--tool-ask)
   :id "fusion-ask"
   :server-id anvil-fusion--server-id
   :description
   "Submit a Fusion ask: fan a prompt out to a model panel (sovereign
= local / zero-egress by default, or quality / budget) and have a
judge synthesize one answer (consensus / contradictions / unique
insights -> final answer).  Recovers Fable-class output WITHOUT the
banned model.  Returns a job id immediately; poll `fusion-result'
until done (a full ask runs tens of seconds to minutes).")

  (anvil-server-register-tool
   (anvil-fusion--encode-handler #'anvil-fusion--tool-result)
   :id "fusion-result"
   :server-id anvil-fusion--server-id
   :read-only t
   :description
   "Poll a Fusion ask job started by `fusion-ask'.  Returns :status
running / done / error; when done, :answer holds the synthesized
reply plus :panel / :egress / :n-candidates."))

(defun anvil-fusion--unregister-tools ()
  "Unregister the fusion MCP tools."
  (dolist (id '("fusion-ask" "fusion-result"))
    (anvil-server-unregister-tool id anvil-fusion--server-id)))

;;;###autoload
(defun anvil-fusion-enable ()
  "Enable anvil-fusion: register the fusion MCP tools."
  (interactive)
  (anvil-fusion--register-tools))

(defun anvil-fusion-disable ()
  "Disable anvil-fusion: unregister the fusion MCP tools."
  (interactive)
  (anvil-fusion--unregister-tools))

;; anvil-optional-modules entry point: `anvil--load-module' loads
;; `anvil-<name>' and calls `anvil-<name>-enable'.  Expose this module as
;; `fusion-mcp' by aliasing to the real enable/disable defined above.
(defalias 'anvil-fusion-mcp-enable #'anvil-fusion-enable)
(defalias 'anvil-fusion-mcp-disable #'anvil-fusion-disable)

(provide 'anvil-fusion-mcp)
;;; anvil-fusion-mcp.el ends here
