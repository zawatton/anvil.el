;;; anvil-elisp-read-check.el --- Reader gate for generated Elisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Read every top-level form of each named file with the real Lisp reader and
;; report the first one that does not read.  Runs under NeLisp
;;
;;   nelisp --load tools/anvil-elisp-read-check.el -- FILE...
;;
;; and under GNU Emacs
;;
;;   emacs --batch -l tools/anvil-elisp-read-check.el -- FILE...
;;
;; Why a reader and not a paren counter: a counter is wrong about `?\(', about
;; parens inside strings and comments, and about the character syntax that
;; Elisp keeps changing.  The reader is the same code that will later refuse to
;; load the file, so its verdict is the one that matters.
;;
;; Why it exists: every Elisp file written by an OpenRouter worker so far has
;; had an unbalanced parenthesis, and no worker ran the verification it was
;; told to run.  A generated file is not a deliverable until something
;; deterministic has read it.
;;
;; Exit status is 0 when every file reads and 1 when any file does not, so this
;; can gate a worker's result without anyone reading the output.

;;; Code:

(defun anvil-elisp-read-check--line-of (text position)
  "Return the 1-based line of POSITION in TEXT."
  (let ((line 1) (index 0) (limit (min position (length text))))
    (while (< index limit)
      (when (eq (aref text index) ?\n) (setq line (1+ line)))
      (setq index (1+ index)))
    line))

(defun anvil-elisp-read-check--only-filler-p (text position)
  "Non-nil when TEXT from POSITION holds nothing but whitespace and comments.
A file normally ends with a `;;; ... ends here' line, and the reader signals
`end-of-file' on that just as it does on a missing parenthesis.  Telling the
two apart is the whole job of this function."
  (let ((index position) (limit (length text)) (filler t))
    (while (and filler (< index limit))
      (let ((char (aref text index)))
        (cond
         ((memq char '(?\s ?\t ?\n ?\r ?\f)) (setq index (1+ index)))
         ((eq char ?\;)
          (while (and (< index limit) (not (eq (aref text index) ?\n)))
            (setq index (1+ index))))
         (t (setq filler nil)))))
    filler))

(defun anvil-elisp-read-check--file (path)
  "Read every top-level form in PATH.  Return nil when it reads, else a report."
  (if (not (file-readable-p path))
      (format "%s: not readable" path)
    (let* ((text (with-temp-buffer (insert-file-contents path) (buffer-string)))
           (length (length text))
           (position 0) (forms 0) (verdict nil) (done nil))
      (while (not done)
        (condition-case error
            (let ((result (read-from-string text position)))
              (setq forms (1+ forms) position (cdr result))
              (when (>= position length) (setq done t)))
          ;; The reader ran out of input inside a form: the usual shape of a
          ;; missing closing parenthesis.  The last position it accepted is
          ;; where the unclosed form began, which is the useful line to name.
          (end-of-file
           (setq verdict
                 (unless (anvil-elisp-read-check--only-filler-p text position)
                   (format "%s: unbalanced. The reader accepted %d top-level form%s and then hit end of file inside the form starting at line %d (byte %d of %d). The usual cause is one missing closing parenthesis at the end of that form."
                           path forms (if (= forms 1) "" "s")
                           (anvil-elisp-read-check--line-of text position)
                           position length))
                 done t))
          (error
           (setq verdict
                 (format "%s: does not read. After %d top-level form%s, at line %d (byte %d): %S"
                         path forms (if (= forms 1) "" "s")
                         (anvil-elisp-read-check--line-of text position)
                         position error)
                 done t))))
      verdict)))

(defconst anvil-elisp-read-check--negative-controls
  '("(a (b" "(a \"str")
  "Inputs every correct reader must refuse.
Both are truncated: one loses a closing parenthesis, the other a closing
quote.  A reader that returns a value for either will call a truncated file
complete, which is the exact failure this tool exists to catch.")

(defun anvil-elisp-read-check--reader-is-strict-p ()
  "Non-nil when this runtime's reader refuses every negative control.
Measured in this process rather than assumed, so the tool follows the runtime
it is actually running on instead of a claim about it.

NeLisp returned `((a (b)) . 5)' for the first control until 2026-09-23,
inventing the two closing parentheses GNU Emacs signals `end-of-file' for, and
a gate on that reader passed every broken file it was given.  Fixed on
nelisp branch `fix/reader-eof-signals' (commit 598284c7f); a NeLisp built from
that branch or later passes these controls and can run this tool.  An older
binary still fails them and is refused here rather than believed."
  (let ((strict t))
    (dolist (control anvil-elisp-read-check--negative-controls strict)
      (when (condition-case nil
                (progn (read-from-string control) t)
              (error nil))
        (setq strict nil)))))

(defun anvil-elisp-read-check-main (paths)
  "Check PATHS and return the number that failed, printing one line each."
  (let ((failed 0))
    (dolist (path paths)
      (let ((verdict (anvil-elisp-read-check--file path)))
        (if verdict
            (progn (setq failed (1+ failed)) (princ (concat verdict "\n")))
          (princ (format "%s: reads\n" path)))))
    failed))

(let* ((args (cdr (member "--" command-line-args)))
       (paths (or args (and (boundp 'command-line-args-left)
                            command-line-args-left))))
  (cond
   ((null paths)
    (princ "usage: anvil-elisp-read-check.el -- FILE...\n")
    (kill-emacs 2))
   ;; Refuse to report green from a reader that accepts truncated input.  A
   ;; gate that cannot fail is worse than no gate, because it is believed.
   ((not (anvil-elisp-read-check--reader-is-strict-p))
    (princ "anvil-elisp-read-check: this runtime's reader accepts truncated input, so it cannot judge these files. Run it under GNU Emacs: emacs --batch -l tools/anvil-elisp-read-check.el -- FILE...\n")
    (kill-emacs 3))
   (t
    (kill-emacs (if (> (anvil-elisp-read-check-main paths) 0) 1 0)))))

;;; anvil-elisp-read-check.el ends here
