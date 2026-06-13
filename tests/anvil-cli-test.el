;;; anvil-cli-test.el --- Tests for anvil-cli -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT for `anvil-cli' dispatch and formatting.  `anvil-pkg' is not
;; loaded in tests; the public `pkg-*' entry points are stubbed with
;; `cl-letf' so routing stays isolated from the separate repository.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil-cli)

(defmacro anvil-cli-test--with-pkg-stubs (bindings &rest body)
  "Run BODY with `anvil-pkg' availability and public API stubbed.

BINDINGS is a list of (FUNCTION LAMBDA-ARGS . BODY) forms."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'require)
              (let ((orig (symbol-function 'require)))
                (lambda (feature &optional filename noerror)
                  (if (eq feature 'anvil-pkg)
                      t
                    (funcall orig feature filename noerror)))))
             ,@(mapcar
                (lambda (binding)
                  (pcase-let ((`(,fn ,args . ,fn-body) binding))
                    `((symbol-function ',fn)
                      (lambda ,args ,@fn-body))))
                bindings))
     ,@body))

(defun anvil-cli-test--json-parse (text)
  "Parse JSON TEXT into a plist."
  (json-parse-string text :object-type 'plist :array-type 'list))

(ert-deftest anvil-cli-test-routes-pkg-subcommand ()
  "Top-level `pkg' dispatch reaches the pkg handler."
  (anvil-cli-test--with-pkg-stubs
      ((pkg-list () "pkg-list-ok"))
    (should (equal (anvil-cli-dispatch '("pkg" "list"))
                   "pkg-list-ok"))))

(ert-deftest anvil-cli-test-unknown-subcommand-errors-clearly ()
  "Unknown top-level subcommands raise a clear error."
  (should-error (anvil-cli-dispatch '("bogus"))
                :type 'user-error))

(ert-deftest anvil-cli-test-every-supported-verb-maps-to-its-public-function ()
  "Each supported verb dispatches to the matching `pkg-*' function."
  (let ((calls nil))
    (anvil-cli-test--with-pkg-stubs
        ((pkg-install (name) (push (list :fn 'pkg-install :args (list name)) calls) "ok")
         (pkg-search (query) (push (list :fn 'pkg-search :args (list query)) calls) "ok")
         (pkg-list () (push (list :fn 'pkg-list :args nil) calls) "ok")
         (pkg-uninstall (name) (push (list :fn 'pkg-uninstall :args (list name)) calls) "ok")
         (pkg-upgrade (&optional name) (push (list :fn 'pkg-upgrade :args (and name (list name))) calls) "ok")
         (pkg-info (name) (push (list :fn 'pkg-info :args (list name)) calls) "ok")
         (pkg-pin (name) (push (list :fn 'pkg-pin :args (list name)) calls) "ok")
         (pkg-unpin (name) (push (list :fn 'pkg-unpin :args (list name)) calls) "ok")
         (pkg-list-pins () (push (list :fn 'pkg-list-pins :args nil) calls) "ok")
         (pkg-doctor () (push (list :fn 'pkg-doctor :args nil) calls) "ok"))
      (dolist (args '(("pkg" "install" "alpha")
                      ("pkg" "search" "needle")
                      ("pkg" "list")
                      ("pkg" "uninstall" "alpha")
                      ("pkg" "upgrade")
                      ("pkg" "upgrade" "alpha")
                      ("pkg" "info" "alpha")
                      ("pkg" "pin" "alpha")
                      ("pkg" "unpin" "alpha")
                      ("pkg" "list-pins")
                      ("pkg" "doctor")))
        (should (equal (anvil-cli-dispatch args) "ok"))))
    (should (equal (nreverse calls)
                   '((:fn pkg-install :args ("alpha"))
                     (:fn pkg-search :args ("needle"))
                     (:fn pkg-list :args nil)
                     (:fn pkg-uninstall :args ("alpha"))
                     (:fn pkg-upgrade :args nil)
                     (:fn pkg-upgrade :args ("alpha"))
                     (:fn pkg-info :args ("alpha"))
                     (:fn pkg-pin :args ("alpha"))
                     (:fn pkg-unpin :args ("alpha"))
                     (:fn pkg-list-pins :args nil)
                     (:fn pkg-doctor :args nil))))))

(ert-deftest anvil-cli-test-coerces-required-and-optional-args ()
  "Argument arity rules match the public `pkg-*' API."
  (let ((upgrade-call nil))
    (anvil-cli-test--with-pkg-stubs
        ((pkg-install (name) name)
         (pkg-upgrade (&optional name) (setq upgrade-call name) "done")
         (pkg-list () '(:mode "list")))
      (should (equal (anvil-cli-dispatch '("pkg" "install" "pkg with spaces"))
                     "pkg with spaces"))
      (should (equal (anvil-cli-dispatch '("pkg" "upgrade"))
                     "done"))
      (should (null upgrade-call))
      (should (equal (anvil-cli-dispatch '("pkg" "upgrade" "solo"))
                     "done"))
      (should (equal upgrade-call "solo"))
      (should (string-match-p ":mode" (anvil-cli-dispatch '("pkg" "list"))))
      (should-error (anvil-cli-dispatch '("pkg" "doctor" "extra"))
                    :type 'user-error)
      (should-error (anvil-cli-dispatch '("pkg" "install"))
                    :type 'user-error)
      (should-error (anvil-cli-dispatch '("pkg" "upgrade" "a" "b"))
                    :type 'user-error))))

(ert-deftest anvil-cli-test-json-output-uses-underlying-result ()
  "The `--json' flag serializes the raw return value."
  (anvil-cli-test--with-pkg-stubs
      ((pkg-list ()
         (list :ok t
               :packages (vector
                          (list :name "alpha" :pinned :false)
                          (list :name "beta" :pinned t)))))
    (let* ((json (anvil-cli-dispatch '("pkg" "list" "--json")))
           (decoded (anvil-cli-test--json-parse json)))
      (should (eq t (plist-get decoded :ok)))
      (should (equal (plist-get (nth 0 (plist-get decoded :packages)) :name)
                     "alpha"))
      (should (eq :false
                  (plist-get (nth 0 (plist-get decoded :packages)) :pinned)))
      (should (eq t
                  (plist-get (nth 1 (plist-get decoded :packages))
                             :pinned))))))

(ert-deftest anvil-cli-test-human-output-prefers-plain-strings ()
  "Human-readable output returns strings unchanged."
  (anvil-cli-test--with-pkg-stubs
      ((pkg-info (name) (format "info:%s" name)))
    (should (equal (anvil-cli-dispatch '("pkg" "info" "alpha"))
                   "info:alpha"))))

(ert-deftest anvil-cli-test-json-flag-is-stripped-before-dispatch ()
  "The pkg verb only sees semantic arguments, not `--json'."
  (let ((captured nil))
    (anvil-cli-test--with-pkg-stubs
        ((pkg-install (name) (setq captured name) (list :name name)))
      (let ((json (anvil-cli-dispatch '("pkg" "--json" "install" "alpha"))))
        (should (equal captured "alpha"))
        (should (equal (plist-get (anvil-cli-test--json-parse json) :name)
                       "alpha"))))))

(ert-deftest anvil-cli-test-missing-anvil-pkg-is-actionable ()
  "Missing `anvil-pkg' never degrades to a bare `void-function'."
  (cl-letf (((symbol-function 'require)
             (let ((orig (symbol-function 'require)))
               (lambda (feature &optional filename noerror)
                 (if (eq feature 'anvil-pkg)
                     nil
                   (funcall orig feature filename noerror))))))
    (should-error (anvil-cli-dispatch '("pkg" "list"))
                  :type 'user-error)))

(ert-deftest anvil-cli-test-missing-pkg-function-is-actionable ()
  "Outdated `anvil-pkg' installs produce a clear error."
  (anvil-cli-test--with-pkg-stubs ()
    (should-error (anvil-cli-dispatch '("pkg" "install" "alpha"))
                  :type 'user-error)))

(ert-deftest anvil-cli-test-unknown-pkg-verb-errors-clearly ()
  "Unknown pkg verbs raise a clear error."
  (anvil-cli-test--with-pkg-stubs ()
    (should-error (anvil-cli-dispatch '("pkg" "bogus"))
                  :type 'user-error)))

(ert-deftest anvil-cli-test-emacsclient-entry-returns-string ()
  "The emacsclient entry point returns a printable string."
  (anvil-cli-test--with-pkg-stubs
      ((pkg-doctor () "healthy"))
    (should (equal (anvil-cli-emacsclient-entry '("pkg" "doctor"))
                   "healthy"))))

(provide 'anvil-cli-test)

;;; anvil-cli-test.el ends here
