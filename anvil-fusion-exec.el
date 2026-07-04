;;; anvil-fusion-exec.el --- Execution verification for Fusion code candidates -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Doc 61 §2 Phase 6c ("Execution-based verification").  For code
;; tasks, prose quality is not a sufficient verifier: the strongest
;; check is to apply a candidate's patch in isolation and run a local
;; verification command against the result.  This module ports the
;; execution-reward pattern from dev/nelisp-llm's agent harness into
;; the Fusion Phase-6 verifier flow.
;;
;; Each candidate is evaluated sequentially:
;;   1. Extract a patch from the candidate text.
;;   2. Create a disposable detached git worktree at HEAD.
;;   3. Apply the patch in that worktree.
;;   4. Run a caller-supplied check command under `timeout'.
;;   5. Convert the pass/fail result into Phase-6 claim plists.
;;
;; The extractor is intentionally pragmatic rather than a full diff
;; parser.  It recognizes:
;;   - fenced ```diff / ```patch blocks; and
;;   - raw unified-diff runs that begin at a `--- ' line.
;; Raw scanning is line-oriented and stops at the first non-diff line
;; after a hunk has started.  It does not try to recover malformed or
;; interleaved diffs, and a prose line that happens to look like a diff
;; header may be misclassified.
;;
;; Sovereignty note: execution is entirely local (git + the user's
;; check command).  No network access or model egress is involved.

;;; Code:

(require 'cl-lib)

(declare-function anvil-fusion--candidate-text "anvil-fusion" (candidate &optional fidelity))

(defgroup anvil-fusion nil
  "Fusion judge harness layered over anvil-orchestrator."
  :group 'tools
  :prefix "anvil-fusion-")

(defcustom anvil-fusion-exec-timeout-sec 300
  "Default timeout, in seconds, for one candidate execution check."
  :type 'integer
  :group 'anvil-fusion)

(defcustom anvil-fusion-exec-output-tail-chars 400
  "Maximum output tail, in characters, retained in execution evidence."
  :type 'integer
  :group 'anvil-fusion)

(defun anvil-fusion-exec--trim (text)
  "Return TEXT with leading and trailing whitespace removed."
  (replace-regexp-in-string
   "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" ""
   (or text "")))

(defun anvil-fusion-exec--tail (text)
  "Return the trailing evidence slice for TEXT."
  (let* ((raw (anvil-fusion-exec--trim text))
         (limit (max 0 anvil-fusion-exec-output-tail-chars))
         (len (length raw)))
    (if (<= len limit)
        raw
      (concat "..." (substring raw (- len limit))))))

(defun anvil-fusion-exec--patch-line-p (line)
  "Return non-nil when LINE is part of a unified diff hunk body."
  (or (string-prefix-p "@@ " line)
      (string-prefix-p "+" line)
      (string-prefix-p "-" line)
      (string-prefix-p " " line)
      (string= line "\\ No newline at end of file")))

(defun anvil-fusion-exec--extract-patch (text)
  "Extract unified diff text from TEXT, or nil when none is found.
Recognizes fenced ```diff / ```patch blocks and raw unified-diff runs
starting at a `--- ' file-header line.  Mixed inputs return the
concatenation of all recognized segments in source order, separated by
one blank line."
  (let* ((lines (split-string (or text "") "\n"))
         (segments nil)
         (current nil)
         (in-fence nil)
         (fence-kind nil)
         (i 0)
         (len (length lines)))
    (while (< i len)
      (let ((line (nth i lines)))
        (cond
         (in-fence
          (if (string-match-p "\\`[ \t]*```[ \t]*\\'" line)
              (progn
                (when current
                  (setq segments (append segments (list (mapconcat #'identity current "\n")))))
                (setq current nil
                      in-fence nil
                      fence-kind nil))
            (when fence-kind
              (setq current (append current (list line))))))
         ((string-match-p "\\`[ \t]*```\\(diff\\|patch\\)[ \t]*\\'" line)
          (setq in-fence t
                fence-kind t
                current nil))
         ((string-match-p "\\`[ \t]*```" line)
          (setq in-fence t
                fence-kind nil
                current nil))
         ((and (string-prefix-p "--- " line)
               (< (1+ i) len)
               (string-prefix-p "+++ " (nth (1+ i) lines)))
          (let ((run (list line (nth (1+ i) lines)))
                (j (+ i 2))
                (started nil)
                (done nil))
            (while (and (< j len) (not done))
              (let ((next (nth j lines)))
                (cond
                 ((or (string-prefix-p "diff --git " next)
                      (and (string-prefix-p "--- " next)
                           (< (1+ j) len)
                           (string-prefix-p "+++ " (nth (1+ j) lines))
                           started))
                  (setq done t))
                 ((or (string-prefix-p "index " next)
                      (string-prefix-p "new file mode " next)
                      (string-prefix-p "deleted file mode " next)
                      (string-prefix-p "similarity index " next)
                      (string-prefix-p "rename from " next)
                      (string-prefix-p "rename to " next))
                  (setq run (append run (list next)))
                  (setq j (1+ j)))
                 ((anvil-fusion-exec--patch-line-p next)
                  (setq run (append run (list next)))
                  (setq started t)
                  (setq j (1+ j)))
                 (t
                  (setq done t)))))
            (setq segments (append segments (list (mapconcat #'identity run "\n"))))
            (setq i (1- j))))
         (t nil)))
      (setq i (1+ i)))
    (when segments
      (mapconcat #'identity segments "\n\n"))))

(defun anvil-fusion-exec--call-with-output (program args &optional cwd)
  "Run PROGRAM with ARGS in CWD and return (:exit :output).
Signals from `call-process' are left to the caller."
  (with-temp-buffer
    (let ((default-directory (or cwd default-directory)))
      (list :exit (apply #'call-process program nil t nil args)
            :output (buffer-string)))))

(defun anvil-fusion-exec--worktree-path ()
  "Return a fresh path under `temporary-file-directory' for a worktree."
  (expand-file-name
   (format "anvil-fusion-exec-worktree-%d-%d"
           (emacs-pid)
           (random 1000000))
   temporary-file-directory))

(defun anvil-fusion-exec--result-claim (name status detail)
  "Convert candidate NAME / STATUS / DETAIL to one Phase-6 claim plist."
  (pcase status
    ('pass
     (list :claim (format "候補 %s のパッチは検証コマンドに合格した" name)
           :kind 'code
           :candidates (list name)
           :verdict 'confirmed
           :evidence detail))
    ('fail
     (list :claim (format "候補 %s のパッチは検証コマンドに不合格だった" name)
           :kind 'code
           :candidates (list name)
           :verdict 'refuted
           :evidence detail))
    ('error
     (list :claim (format "候補 %s のパッチは実行エラーになった" name)
           :kind 'code
           :candidates (list name)
           :verdict 'refuted
           :evidence detail))
    (_
     (list :claim (format "候補 %s の回答には検証可能なパッチが見つからなかった" name)
           :kind 'code
           :candidates (list name)
           :verdict 'unverified
           :evidence detail))))

(cl-defun anvil-fusion-exec-run-candidate
    (candidate repo-root check-cmd &key timeout-sec)
  "Run CANDIDATE's patch in a disposable worktree under REPO-ROOT.
CHECK-CMD is executed with `bash -c' under `timeout'.  Returns
`(:status pass|fail|no-patch|error :detail STR)' and never signals."
  (condition-case err
      (let* ((text (anvil-fusion--candidate-text candidate 'summary))
             (patch (anvil-fusion-exec--extract-patch text))
             (timeout (or timeout-sec anvil-fusion-exec-timeout-sec)))
        (if (null patch)
            (list :status 'no-patch :detail "no patch block")
          (let ((worktree (anvil-fusion-exec--worktree-path))
                (patchfile (make-temp-file "anvil-fusion-exec-patch-")))
            (unwind-protect
                (progn
                  (with-temp-file patchfile
                    (insert patch)
                    (unless (string-suffix-p "\n" patch)
                      (insert "\n")))
                  (let* ((add (anvil-fusion-exec--call-with-output
                               "git"
                               (list "-C" repo-root "worktree" "add" worktree "--detach" "HEAD")))
                         (add-exit (plist-get add :exit)))
                    (unless (eq add-exit 0)
                      (cl-return-from anvil-fusion-exec-run-candidate
                        (list :status 'error
                              :detail (format "worktree add failed: %s"
                                              (anvil-fusion-exec--tail
                                               (plist-get add :output)))))))
                  (let* ((apply-res (anvil-fusion-exec--call-with-output
                                     "git"
                                     (list "-C" worktree "apply" "--whitespace=nowarn" patchfile)))
                         (apply-exit (plist-get apply-res :exit)))
                    (if (not (eq apply-exit 0))
                        (list :status 'fail
                              :detail (format "patch does not apply: %s"
                                              (anvil-fusion-exec--tail
                                               (plist-get apply-res :output))))
                      (let* ((check-res (anvil-fusion-exec--call-with-output
                                         "timeout"
                                         (list (number-to-string timeout)
                                               "bash" "-c" check-cmd)
                                         worktree))
                             (exit (plist-get check-res :exit))
                             (output (plist-get check-res :output)))
                        (cond
                         ((eq exit 0)
                          (list :status 'pass :detail "exit 0"))
                         ((eq exit 124)
                          (list :status 'fail
                                :detail (format "check timed out after %s s" timeout)))
                         (t
                          (list :status 'fail
                                :detail (format "check failed with exit %s: %s"
                                                exit
                                                (anvil-fusion-exec--tail output)))))))))
              (ignore-errors
                (anvil-fusion-exec--call-with-output
                 "git" (list "-C" repo-root "worktree" "remove" "--force" worktree)))
              (ignore-errors
                (anvil-fusion-exec--call-with-output
                 "git" (list "-C" repo-root "worktree" "prune")))
              (ignore-errors
                (delete-file patchfile))))))
    (error
     (list :status 'error
           :detail (format "%s" err)))))

(cl-defun anvil-fusion-exec-verify-candidates
    (candidates &key repo-root check-cmd timeout-sec)
  "Run execution verification for CANDIDATES sequentially.
Returns `(:results ((:name NAME :status S :detail D)...) :claims CLAIMS)'."
  (let ((results nil)
        (claims nil))
    (dolist (candidate candidates)
      (let* ((name (or (plist-get candidate :name) "?"))
             (run (anvil-fusion-exec-run-candidate
                   candidate repo-root check-cmd :timeout-sec timeout-sec))
             (status (plist-get run :status))
             (detail (plist-get run :detail)))
        (setq results (append results (list (list :name name :status status :detail detail))))
        (setq claims (append claims (list (anvil-fusion-exec--result-claim
                                           name status detail))))))
    (list :results results :claims claims)))

(provide 'anvil-fusion-exec)
;;; anvil-fusion-exec.el ends here
