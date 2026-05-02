;;; anvil-config-test.el --- Tests for anvil-config -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the user-config loader bridge between the Emacs path
;; (init.el wins) and the NeLisp standalone path
;; ($ANVIL_CONFIG_DIR/config.el wins).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-config)


;;;; --- helpers ------------------------------------------------------------

(defmacro anvil-config-test--with-env (bindings &rest body)
  "Run BODY with environment variables temporarily set per BINDINGS.
BINDINGS is a list of (NAME VALUE) where VALUE nil unsets the var.
The previous values are restored on exit."
  (declare (indent 1))
  `(let ((--saved-- (mapcar (lambda (b) (cons (car b) (getenv (car b))))
                            ',bindings)))
     (unwind-protect
         (progn
           ,@(mapcar (lambda (b)
                       `(setenv ,(car b) ,(cadr b)))
                     bindings)
           ,@body)
       (dolist (cell --saved--)
         (setenv (car cell) (cdr cell))))))

(defmacro anvil-config-test--simulating-nelisp (&rest body)
  "Run BODY with `(featurep 'nelisp)' returning t."
  (declare (indent 0))
  `(let ((--was-feature-- (memq 'nelisp features)))
     (unwind-protect
         (progn (provide 'nelisp) ,@body)
       (unless --was-feature--
         (setq features (delq 'nelisp features))))))


;;;; --- anvil-config-dir resolution ----------------------------------------

(ert-deftest anvil-config-test/dir-defaults-to-xdg ()
  "With ANVIL_CONFIG_DIR unset, dir = $XDG_CONFIG_HOME/anvil."
  (anvil-config-test--with-env (("ANVIL_CONFIG_DIR" nil)
                                ("XDG_CONFIG_HOME" "/tmp/xdg-test"))
    (should (equal (anvil-config-dir) "/tmp/xdg-test/anvil"))))

(ert-deftest anvil-config-test/dir-honors-anvil-config-dir ()
  "Explicit ANVIL_CONFIG_DIR wins over XDG."
  (anvil-config-test--with-env (("ANVIL_CONFIG_DIR" "/explicit/path")
                                ("XDG_CONFIG_HOME" "/tmp/xdg-test"))
    (should (equal (anvil-config-dir) "/explicit/path"))))

(ert-deftest anvil-config-test/dir-falls-back-to-home-config ()
  "Both env vars unset → ~/.config/anvil."
  (anvil-config-test--with-env (("ANVIL_CONFIG_DIR" nil)
                                ("XDG_CONFIG_HOME" nil))
    (should (equal (anvil-config-dir)
                   (expand-file-name "~/.config/anvil")))))

(ert-deftest anvil-config-test/file-appends-config-el ()
  "anvil-config-file = dir + anvil-config-file-name."
  (anvil-config-test--with-env (("ANVIL_CONFIG_DIR" "/tmp/cfg-test"))
    (let ((anvil-config-file-name "config.el"))
      (should (equal (anvil-config-file) "/tmp/cfg-test/config.el")))
    (let ((anvil-config-file-name "alt.el"))
      (should (equal (anvil-config-file) "/tmp/cfg-test/alt.el")))))


;;;; --- active-p detection -------------------------------------------------

(ert-deftest anvil-config-test/active-p-false-under-emacs ()
  "Without `(provide 'nelisp)' active-p returns nil."
  ;; Sanity: under regular ERT batch, nelisp feature should not be provided.
  (let ((features (delq 'nelisp (copy-sequence features))))
    (should-not (anvil-config-active-p))))

(ert-deftest anvil-config-test/active-p-true-when-nelisp-provided ()
  "Once `nelisp' is provided, active-p returns non-nil."
  (anvil-config-test--simulating-nelisp
   (should (anvil-config-active-p))))


;;;; --- anvil-load-user-config behavior ------------------------------------

(ert-deftest anvil-config-test/load-skips-when-inactive ()
  "Under Emacs path (= no nelisp feature), loader returns nil and never loads."
  (let ((features (delq 'nelisp (copy-sequence features)))
        (tmp (make-temp-file "anvil-cfg-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(setq anvil-config-test--marker 'should-not-fire)\n"))
          (anvil-config-test--with-env (("ANVIL_CONFIG_DIR"
                                         (file-name-directory tmp)))
            (let ((anvil-config-file-name (file-name-nondirectory tmp))
                  (anvil-config-test--marker nil))
              (should (null (anvil-load-user-config)))
              (should (null (bound-and-true-p anvil-config-test--marker))))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest anvil-config-test/load-fires-when-active-and-file-present ()
  "NeLisp path + readable file → loads + returns absolute path."
  (let ((tmp (make-temp-file "anvil-cfg-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(setq anvil-config-test--marker 'fired)\n"))
          (anvil-config-test--with-env (("ANVIL_CONFIG_DIR"
                                         (file-name-directory tmp)))
            (anvil-config-test--simulating-nelisp
             (defvar anvil-config-test--marker nil)
             (setq anvil-config-test--marker nil)
             (let ((anvil-config-file-name (file-name-nondirectory tmp)))
               (let ((loaded (anvil-load-user-config)))
                 (should (equal loaded (expand-file-name tmp)))
                 (should (eq anvil-config-test--marker 'fired)))))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest anvil-config-test/load-silent-when-file-absent ()
  "NeLisp path + no file → nil, no error, no warning."
  (anvil-config-test--with-env (("ANVIL_CONFIG_DIR" "/tmp/anvil-cfg-no-such-dir"))
    (anvil-config-test--simulating-nelisp
     (let ((anvil-config-file-name "definitely-missing.el"))
       (should (null (anvil-load-user-config)))))))

(ert-deftest anvil-config-test/load-warns-on-broken-file-and-returns-nil ()
  "NeLisp path + parse / eval failure → display-warning + return nil, no throw."
  (let ((tmp (make-temp-file "anvil-cfg-broken-" nil ".el"))
        (warned nil))
    (unwind-protect
        (progn
          (with-temp-file tmp
            ;; Unbalanced — read will fail at eval time.
            (insert "(setq broken\n"))
          (anvil-config-test--with-env (("ANVIL_CONFIG_DIR"
                                         (file-name-directory tmp)))
            (anvil-config-test--simulating-nelisp
             (let ((anvil-config-file-name (file-name-nondirectory tmp)))
               (cl-letf (((symbol-function 'display-warning)
                          (lambda (&rest _) (setq warned t))))
                 (should (null (anvil-load-user-config)))
                 (should warned))))))
      (ignore-errors (delete-file tmp)))))


(provide 'anvil-config-test)

;;; anvil-config-test.el ends here
