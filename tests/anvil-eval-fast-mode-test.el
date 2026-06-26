;;; anvil-eval-fast-mode-test.el --- ERT for org fast-mode buffer scoping -*- lexical-binding: t; -*-

;;; Commentary:

;; The org fast-mode advice runs `(font-lock-mode -1)' after a minimal
;; `org-mode' setup.  That is right for the throwaway temp buffers the
;; org tools create, but wrong for buffers that outlive the tool call:
;; an explicit `font-lock-mode' call sets
;; `font-lock-mode-set-explicitly', after which
;; `global-font-lock-mode' refuses to re-enable fontification — the
;; user's buffer stays uncoloured for the rest of the session.
;;
;; User agenda files are exactly the org buffers a user keeps open
;; while MCP tools operate on them, so the advice now skips the fast
;; path for files in `org-agenda-files'.

;;; Code:

(require 'ert)
(require 'org)
(require 'anvil)
(require 'anvil-eval)

(defmacro anvil-eval-fast-mode-test--with-org-file (var &rest body)
  "Bind VAR to a temp org file path around BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "anvil-fast-mode-test" nil ".org"
                               "* Heading\nBody.\n")))
     (unwind-protect
         (progn ,@body)
       (when-let* ((buf (find-buffer-visiting ,var)))
         (kill-buffer buf))
       (delete-file ,var))))

(ert-deftest anvil-eval-fast-mode-test-agenda-file-keeps-font-lock ()
  "The fast path is skipped for buffers visiting agenda files."
  (anvil-eval-fast-mode-test--with-org-file file
    (let ((anvil-eval-org-fast-mode t)
          (anvil-eval--in-org-fast-mode t)
          (org-agenda-files (list file)))
      (with-current-buffer (find-file-noselect file)
        ;; find-file-noselect already ran org-mode through the advice;
        ;; re-run explicitly to pin the decision under fast-mode flags.
        (anvil-eval--org-mode-fast-advice #'org-mode)
        (should-not delay-mode-hooks)
        (should-not (and (boundp 'font-lock-mode-set-explicitly)
                         font-lock-mode-set-explicitly))))))

(ert-deftest anvil-eval-fast-mode-test-other-files-stay-fast ()
  "Non-agenda buffers still get the minimal fast-path setup."
  (anvil-eval-fast-mode-test--with-org-file file
    (let ((anvil-eval-org-fast-mode t)
          (anvil-eval--in-org-fast-mode t)
          (org-agenda-files nil))
      (with-current-buffer (find-file-noselect file)
        (anvil-eval--org-mode-fast-advice #'org-mode)
        (should delay-mode-hooks)
        (when (boundp 'org-element-use-cache)
          (should-not org-element-use-cache))))))

(provide 'anvil-eval-fast-mode-test)
;;; anvil-eval-fast-mode-test.el ends here
