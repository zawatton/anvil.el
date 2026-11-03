;;; standalone-shim-probe.el --- with-timeout and directory-files-recursively -*- lexical-binding: t; -*-

;; Load after tools/anvil-standalone-mcp.el with ANVIL_STANDALONE_TEST_NO_SERVER=1.
;; SHIM_TREE names a fixture tree.  Prints `SHIMTO NAME PASS|FAIL' lines and
;; `SHIMDFR VARIANT LIST' lines (paths relative to the tree) that the caller
;; compares with the same file run under GNU `emacs --batch -Q'.

;;; Code:

(defun standalone-shim-probe--rel (tree files)
  (mapcar (lambda (f) (substring f (length tree))) files))

(defun standalone-shim-probe--to (name ok)
  (princ (concat "SHIMTO " name (if ok " PASS" " FAIL") "\n")))

(let ((tree (directory-file-name (getenv "SHIM_TREE"))))
  (dolist (v (list (cons "plain" (lambda () (directory-files-recursively tree "\\.txt\\'")))
                   (cons "dirs" (lambda () (directory-files-recursively tree "^a" t)))
                   (cons "pred" (lambda () (directory-files-recursively
                                            tree "" nil
                                            (lambda (d) (not (string-match-p "skip" d))))))
                   (cons "follow" (lambda () (directory-files-recursively tree "\\.txt\\'" nil nil t)))
                   (cons "missing" (lambda ()
                                     (condition-case err
                                         (directory-files-recursively
                                          (concat tree "/nope") "")
                                       (file-error (list (car err))))))))
    (princ (concat "SHIMDFR " (car v) " "
                   (prin1-to-string (let ((r (funcall (cdr v)))) (if (stringp (car-safe r)) (standalone-shim-probe--rel tree r) r)))
                   "\n"))))

(condition-case err
    (progn
      (standalone-shim-probe--to
       "timeout-fires"
       (eq 'timed-out (with-timeout (0.2 'timed-out) (sit-for 3) 'finished)))
      (standalone-shim-probe--to
       "timeout-process-wait"
       (eq 'timed-out (with-timeout (0.2 (ignore) 'timed-out)
                        (accept-process-output nil 3) (sit-for 3) 'finished)))
      (standalone-shim-probe--to
       "body-value"
       (equal 42 (with-timeout (5 'timed-out) (+ 40 2))))
      (standalone-shim-probe--to
       "timer-cancelled"
       (let ((n 0))
         (with-timeout (0.2 (setq n (1+ n))) 'quick)
         (sit-for 0.5)
         (= n 0))))
  (error (princ (concat "SHIMTO ERROR " (prin1-to-string err) "\n"))))
;;; standalone-shim-probe.el ends here
