;;; anvil-cli.el --- Command-line dispatcher for anvil pkg -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Thin command-line dispatcher for shell entry points such as:
;;
;;   anvil pkg install NAME
;;   anvil pkg list --json
;;
;; The `pkg' subcommand delegates to the separate `anvil-pkg'
;; package's public `pkg-*' API.  `anvil-pkg' is required lazily at
;; dispatch time so loading `anvil-cli' does not couple anvil.el to
;; that external repository.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'pp)
(require 'subr-x)

(defvar standard-error)

(defconst anvil-cli--pkg-verb-table
  '(("install" pkg-install 1 1)
    ("search" pkg-search 1 1)
    ("list" pkg-list 0 0)
    ("uninstall" pkg-uninstall 1 1)
    ("upgrade" pkg-upgrade 0 1)
    ("info" pkg-info 1 1)
    ("pin" pkg-pin 1 1)
    ("unpin" pkg-unpin 1 1)
    ("list-pins" pkg-list-pins 0 0)
    ("doctor" pkg-doctor 0 0))
  "Verb table for `anvil pkg'.

Each entry is (VERB FUNCTION MIN-ARGS MAX-ARGS).")

(defconst anvil-cli--subcommands '("pkg")
  "Supported top-level subcommands.")

(defun anvil-cli--pkg-verbs ()
  "Return supported `anvil pkg' verbs."
  (mapcar #'car anvil-cli--pkg-verb-table))

(defun anvil-cli--normalize-args (args)
  "Return ARGS as a proper list of strings."
  (cond
   ((null args) nil)
   ((vectorp args) (append args nil))
   ((listp args) args)
   (t (list args))))

(defun anvil-cli--extract-json-flag (args)
  "Return plist `(:args LIST :json BOOL)' from ARGS."
  (let ((json-output nil)
        (rest nil))
    (dolist (arg (anvil-cli--normalize-args args))
      (if (string= arg "--json")
          (setq json-output t)
        (push arg rest)))
    (list :args (nreverse rest)
          :json json-output)))

(defun anvil-cli--usage-error (fmt &rest args)
  "Signal a `user-error' using FMT and ARGS."
  (apply #'user-error fmt args))

(defun anvil-cli--format-supported (items)
  "Return ITEMS joined for error/help messages."
  (string-join items ", "))

(defun anvil-cli--json-encode (value)
  "Return VALUE encoded as JSON text."
  (if (fboundp 'json-serialize)
      (json-serialize value :null-object :null :false-object :false)
    (json-encode value)))

(defun anvil-cli--format-human (value)
  "Return VALUE as human-readable text."
  (cond
   ((stringp value) value)
   ((vectorp value)
    (mapconcat #'anvil-cli--format-human (append value nil) "\n"))
   ((and (listp value) (cl-every #'stringp value))
    (string-join value "\n"))
   (t
    (string-trim-right (pp-to-string value)))))

(defun anvil-cli--format-result (value json-output)
  "Return VALUE formatted for JSON-OUTPUT mode."
  (if json-output
      (anvil-cli--json-encode value)
    (anvil-cli--format-human value)))

(defun anvil-cli--pkg-spec (verb)
  "Return the verb table entry for VERB, or nil."
  (assoc-string verb anvil-cli--pkg-verb-table t))

(defun anvil-cli--validate-arity (verb args min-args max-args)
  "Validate ARGS for VERB against MIN-ARGS and MAX-ARGS."
  (let ((count (length args)))
    (unless (<= min-args count max-args)
      (cond
       ((= min-args max-args 0)
        (anvil-cli--usage-error "anvil pkg %s: expected no arguments" verb))
       ((= min-args max-args 1)
        (anvil-cli--usage-error
         "anvil pkg %s: expected exactly one argument" verb))
       ((= min-args 0)
        (anvil-cli--usage-error
         "anvil pkg %s: expected at most %d argument%s"
         verb max-args (if (= max-args 1) "" "s")))
       (t
        (anvil-cli--usage-error
         "anvil pkg %s: expected %d to %d arguments"
         verb min-args max-args))))))

(defun anvil-cli--coerce-pkg-args (verb args)
  "Coerce VERB ARGS into the public `anvil-pkg' call shape."
  (pcase verb
    ((or "list" "list-pins" "doctor") nil)
    ("upgrade" args)
    (_ (list (car args)))))

(defun anvil-cli--ensure-anvil-pkg ()
  "Load `anvil-pkg' lazily or signal an actionable error."
  (let ((loaded
         (condition-case err
             (require 'anvil-pkg nil t)
           (error
            (anvil-cli--usage-error
             (concat
              "anvil pkg: could not load anvil-pkg; install the separate "
              "anvil-pkg package or add its checkout to load-path "
              "(shell launcher: set ANVIL_PKG_DIR=/path/to/anvil-pkg). "
              "Original error: %s")
             (error-message-string err))))))
    (unless loaded
      (anvil-cli--usage-error
       (concat
        "anvil pkg: could not load anvil-pkg; install the separate "
        "anvil-pkg package or add its checkout to load-path "
        "(shell launcher: set ANVIL_PKG_DIR=/path/to/anvil-pkg)")))))

(defun anvil-cli--dispatch-pkg (args)
  "Dispatch `anvil pkg' ARGS to the public `anvil-pkg' API."
  (unless args
    (anvil-cli--usage-error
     "anvil pkg: missing verb; expected one of: %s"
     (anvil-cli--format-supported (anvil-cli--pkg-verbs))))
  (let* ((verb (car args))
         (verb-args (cdr args))
         (spec (anvil-cli--pkg-spec verb)))
    (unless spec
      (anvil-cli--usage-error
       "anvil pkg: unknown verb `%s'; expected one of: %s"
       verb
       (anvil-cli--format-supported (anvil-cli--pkg-verbs))))
    (pcase-let ((`(,_ ,fn ,min-args ,max-args) spec))
      (anvil-cli--validate-arity verb verb-args min-args max-args)
      (anvil-cli--ensure-anvil-pkg)
      (unless (fboundp fn)
        (anvil-cli--usage-error
         "anvil pkg: installed anvil-pkg is missing `%s'; upgrade or reinstall anvil-pkg"
         fn))
      (apply fn (anvil-cli--coerce-pkg-args verb verb-args)))))

;;;###autoload
(defun anvil-cli-dispatch (args)
  "Dispatch CLI ARGS and return formatted output as a string.

ARGS is a list of strings whose first element is the subcommand."
  (let* ((parsed (anvil-cli--extract-json-flag args))
         (clean-args (plist-get parsed :args))
         (json-output (plist-get parsed :json)))
    (unless clean-args
      (anvil-cli--usage-error
       "anvil-cli: missing subcommand; expected one of: %s"
       (anvil-cli--format-supported anvil-cli--subcommands)))
    (anvil-cli--format-result
     (pcase (car clean-args)
       ("pkg" (anvil-cli--dispatch-pkg (cdr clean-args)))
       (_
        (anvil-cli--usage-error
         "anvil-cli: unknown subcommand `%s'; expected one of: %s"
         (car clean-args)
         (anvil-cli--format-supported anvil-cli--subcommands))))
     json-output)))

(defun anvil-cli--print-line (text &optional error-output)
  "Print TEXT with a trailing newline.
When ERROR-OUTPUT is non-nil, write to `standard-error'."
  (let ((stream (if error-output
                    (if (boundp 'standard-error) standard-error t)
                  t)))
    (princ text stream)
    (unless (string-suffix-p "\n" text)
      (princ "\n" stream))))

(defun anvil-cli--current-argv ()
  "Return CLI args from the current batch process, if available."
  (let ((argv-sym (intern-soft "argv")))
    (or (and argv-sym
             (boundp argv-sym)
             (anvil-cli--normalize-args (symbol-value argv-sym)))
        (anvil-cli--normalize-args command-line-args-left))))

;;;###autoload
(defun anvil-cli-emacsclient-entry (args)
  "Return the formatted CLI output string for ARGS."
  (anvil-cli-dispatch args))

;;;###autoload
(defun anvil-cli-main (&optional cli-args)
  "Batch entry point for `anvil-cli'.

CLI-ARGS, when non-nil, overrides `argv' / `command-line-args-left'."
  (let ((args (or (anvil-cli--normalize-args cli-args)
                  (anvil-cli--current-argv))))
    (condition-case err
        (progn
          (anvil-cli--print-line (anvil-cli-dispatch args))
          (kill-emacs 0))
      (error
       (anvil-cli--print-line (error-message-string err) t)
       (kill-emacs 1)))))

(provide 'anvil-cli)

;;; anvil-cli.el ends here
