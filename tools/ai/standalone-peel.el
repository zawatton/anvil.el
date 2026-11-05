;;; standalone-peel.el --- peel missing-definition blockers  -*- lexical-binding: t; -*-

;; Diagnostic for the anvil NeLisp standalone runtime.  Evaluates a form and,
;; whenever it fails with a void-function / void-variable / missing `require'
;; that a vendored Emacs Lisp file can supply, evaluates that definition and
;; retries.  The loop stops at the first blocker it cannot fix mechanically.
;; Nothing is persisted: fixes live only in the throwaway process.
;;
;; Driver: tools/ai/standalone-peel.py.  Input: the file named by
;; ANVIL_PEEL_SPEC, one readable plist:
;;   (:form STRING :tool ID :args ALIST :max-fixes N :vendor-dirs (DIR...)
;;    :setup (STRING...) :result-chars N)
;; Output: one line "PEEL-RESULT <json>" on stdout.

;;; Code:

(defconst standalone-peel--fun-kinds
  '("defun" "defmacro" "defsubst" "defalias" "cl-defun" "cl-defmacro"
    "cl-defsubst" "define-inline"))
(defconst standalone-peel--var-kinds
  '("defvar" "defconst" "defcustom" "defvar-local"))

;;;; JSON output (hand-rolled: no dependence on json.el behaviour)

(defun standalone-peel--jstr (s)
  "Return S as a JSON string literal, ASCII only."
  (let ((out (list "\"")))
    (dolist (c (append s nil))
      (push (cond ((eq c ?\") "\\\"") ((eq c ?\\) "\\\\")
                  ((eq c ?\n) "\\n") ((eq c ?\t) "\\t") ((eq c ?\r) "\\r")
                  ((or (< c 32) (> c 126))
                   (if (< c #x10000)
                       (concat "\\u" (format "%04x" c))
                     (let ((v (- c #x10000)))
                       (concat "\\u" (format "%04x" (+ #xd800 (ash v -10)))
                               "\\u" (format "%04x" (+ #xdc00 (logand v #x3ff)))))))
                  (t (char-to-string c)))
            out))
    (push "\"" out)
    (apply #'concat (nreverse out))))

(defun standalone-peel--jarr (items) (concat "[" (mapconcat #'identity items ",") "]"))
(defun standalone-peel--jobj (pairs)
  (concat "{" (mapconcat (lambda (p) (concat (standalone-peel--jstr (car p)) ":" (cdr p)))
                         pairs ",")
          "}"))

(defun standalone-peel--trunc (s n)
  (if (> (length s) n) (substring s 0 n) s))

(defun standalone-peel--print (v)
  (condition-case nil (prin1-to-string v) (error "<unprintable>")))

;;;; vendor lookup

(defun standalone-peel--default-vendor-dirs ()
  (let ((d (and (fboundp 'anvil-standalone-org--vendor-dir)
                (anvil-standalone-org--vendor-dir))))
    (and d (list d))))

(defun standalone-peel--grep-files (pattern dirs)
  "Return sorted .el files under DIRS matching ERE PATTERN (cheap prefilter)."
  (let (files)
    (with-temp-buffer
      (apply #'call-process "grep" nil t nil "-rlE" "--include=*.el" "-e" pattern
             (mapcar #'expand-file-name dirs))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((l (buffer-substring (line-beginning-position) (line-end-position))))
          (unless (string= l "") (push l files)))
        (forward-line 1)))
    (sort files #'string<)))

(defun standalone-peel--ere-quote (name)
  (replace-regexp-in-string "[^A-Za-z0-9_-]" (lambda (m) (concat "\\" m)) name t t))

(defun standalone-peel--find-def (name kinds dirs)
  "Return (FORM KIND FILE) for the first top-level def of NAME in DIRS, or nil.
The file is picked with grep, the form is read with the real reader."
  (let* ((alt (mapconcat #'identity kinds "|"))
         (files (standalone-peel--grep-files
                 (concat "^\\((" alt ")[[:space:]]+'?"
                         (standalone-peel--ere-quote name) "([[:space:])]|$)")
                 dirs))
         (re (concat "^(\\(" (mapconcat #'identity kinds "\\|") "\\)[ \t\n]+'?"
                     (regexp-quote name) "[ \t\n)]"))
         found)
    (while (and files (not found))
      (let ((file (pop files)))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (and (not found) (re-search-forward re nil t))
            (let ((kind (match-string 1)) (start (match-beginning 0)) form)
              (goto-char start)
              (setq form (condition-case nil (read (current-buffer)) (error nil)))
              (cond
               ((null form) (goto-char (1+ start)))
               ;; A bare (defvar NAME) declares nothing; keep looking.
               ((and (member kind standalone-peel--var-kinds) (< (length form) 3))
                nil)
               (t (setq found (list form kind file)))))))))
    found))

(defun standalone-peel--find-feature (feature dirs)
  (let (found (want (concat feature ".el")))
    (dolist (d dirs)
      (unless found
        (with-temp-buffer
          (call-process "find" nil t nil (expand-file-name d) "-name" want)
          (goto-char (point-min))
          (let ((l (buffer-substring (line-beginning-position) (line-end-position))))
            (unless (string= l "") (setq found l))))))
    found))

;;;; error helpers

(defun standalone-peel--last-string (data)
  (let (r) (dolist (x data) (when (stringp x) (setq r x))) r))

(defun standalone-peel--err-json (err)
  (standalone-peel--jobj
   (list (cons "class" (standalone-peel--jstr (symbol-name (car err))))
         (cons "data" (standalone-peel--jstr
                       (standalone-peel--trunc (standalone-peel--print (cdr err)) 300))))))

;;;; tool call (handler applied directly so Lisp errors stay signals)

(defun standalone-peel-call-tool (id args)
  "Apply the registered handler of tool ID to ARGS, an alist of (NAME . VALUE)."
  (let* ((tbl (gethash "emacs-eval" anvil-server--tools))
         (tool (and tbl (gethash id tbl))))
    (unless tool (error "standalone-peel: unknown tool %s" id))
    (when (and (plist-get tool :lazy-placeholder) (functionp (plist-get tool :lazy-loader)))
      (funcall (plist-get tool :lazy-loader) id "emacs-eval")
      (setq tool (gethash id (gethash "emacs-eval" anvil-server--tools))))
    (let ((handler (plist-get tool :handler)) vals)
      (dolist (p (or (plist-get tool :arglist) (help-function-arglist handler t)))
        (let ((n (symbol-name p)))
          (unless (string= n "&optional")
            (push (if (string-prefix-p "_" n) nil
                    (cdr (assoc n args)))
                  vals))))
      (apply handler (nreverse vals)))))

;;;; main loop

(defun standalone-peel--run (spec)
  (let* ((t0 (float-time))
         (max-fixes (or (plist-get spec :max-fixes) 30))
         (rchars (or (plist-get spec :result-chars) 500))
         (dirs (or (plist-get spec :vendor-dirs) (standalone-peel--default-vendor-dirs)))
         (setup (mapcar (lambda (s) (car (read-from-string s))) (plist-get spec :setup)))
         (main (cond
                ((plist-get spec :tool)
                 (list (list 'standalone-peel-call-tool (plist-get spec :tool)
                             (list 'quote (plist-get spec :args)))))
                (t (let ((s (plist-get spec :form)) (pos 0) forms)
                     (condition-case nil
                         (while t
                           (let ((r (read-from-string s pos)))
                             (push (car r) forms) (setq pos (cdr r))))
                       (error nil))
                     (nreverse forms)))))
         (body (cons 'progn (append setup main)))
         (fixes nil) (status nil) (err nil) (extra nil) (result nil) (last-sig nil))
    (while (not status)
      (let ((r (condition-case e (cons 'ok (eval body t)) (error (cons 'err e)))))
        (if (eq (car r) 'ok)
            (setq status "ok" err nil result (cdr r))
          (let* ((e (cdr r)) (sym (car e)) (data (cdr e))
                 (sig (standalone-peel--print e)))
            (setq err e)
            (cond
             ((equal sig last-sig) (setq status "stopped" extra '("repeated")))
             ((progn (setq last-sig sig) nil))
             ((and (memq sym '(void-function void-variable)) (symbolp (car data)))
              (let* ((name (symbol-name (car data)))
                     (fn (eq sym 'void-function))
                     (known (seq-find (lambda (f) (equal (cdr (assq 'name f)) name)) fixes)))
                (cond
                 (known (setq status "stopped" extra '("fix-ineffective")))
                 ((>= (length fixes) max-fixes) (setq status "limit"))
                 (t
                  (let ((def (standalone-peel--find-def
                              name (if fn standalone-peel--fun-kinds standalone-peel--var-kinds)
                              dirs)))
                    (if (null def)
                        (setq status "stopped" extra '("not-found"))
                      (condition-case fe
                          (progn (eval (nth 0 def) t)
                                 (push (list (cons 'name name) (cons 'kind (nth 1 def))
                                             (cons 'file (nth 2 def)))
                                       fixes))
                        (error (setq err fe status "stopped"
                                     extra (list "fix-eval-failed" name))))))))))
             ((and (memq sym '(file-missing file-error))
                   (standalone-peel--last-string data))
              (let* ((feature (file-name-sans-extension
                               (file-name-nondirectory (standalone-peel--last-string data))))
                     (file (standalone-peel--find-feature feature dirs)))
                (cond
                 ((>= (length fixes) max-fixes) (setq status "limit"))
                 ((null file) (setq status "stopped" extra '("not-found")))
                 (t (condition-case fe
                        (progn (load file nil t)
                               (push (list (cons 'name feature) (cons 'kind "require")
                                           (cons 'file file))
                                     fixes))
                      (error (setq err fe status "stopped"
                                   extra (list "fix-eval-failed" feature))))))))
             (t (setq status "stopped" extra '("unfixable"))))))))
    (setq fixes (nreverse fixes))
    (let* ((fix-json
            (mapcar (lambda (f)
                      (standalone-peel--jobj
                       (mapcar (lambda (k) (cons (symbol-name k)
                                                 (standalone-peel--jstr (cdr (assq k f)))))
                               '(name kind file))))
                    fixes))
           (groups nil))
      (dolist (f fixes)
        (let ((g (assoc (cdr (assq 'file f)) groups)))
          (if g (setcdr g (append (cdr g) (list (cdr (assq 'name f)))))
            (setq groups (append groups (list (list (cdr (assq 'file f))
                                                    (cdr (assq 'name f)))))))))
      (princ
       (concat
        "PEEL-RESULT "
        (standalone-peel--jobj
         (append
          (list (cons "status" (standalone-peel--jstr status))
                (cons "fixes" (standalone-peel--jarr fix-json)))
          (and err (list (cons "error" (standalone-peel--err-json err))))
          (and extra (list (cons "reason" (standalone-peel--jstr (car extra)))))
          (and (cdr extra) (list (cons "while_fixing" (standalone-peel--jstr (cadr extra)))))
          (and (equal status "ok")
               (list (cons "result" (standalone-peel--jstr
                                     (standalone-peel--trunc (standalone-peel--print result)
                                                             rchars)))))
          (list (cons "suggested_loader"
                      (standalone-peel--jarr
                       (mapcar (lambda (g)
                                 (standalone-peel--jobj
                                  (list (cons "file" (standalone-peel--jstr (car g)))
                                        (cons "names" (standalone-peel--jarr
                                                       (mapcar #'standalone-peel--jstr
                                                               (cdr g)))))))
                               groups)))
                (cons "elapsed_s" (number-to-string
                                   (/ (round (* 10 (- (float-time) t0))) 10.0))))))
        "\n")))))

(let ((spec-file (getenv "ANVIL_PEEL_SPEC")))
  (when spec-file
    (standalone-peel--run
     (with-temp-buffer (insert-file-contents spec-file) (read (current-buffer))))))

(provide 'standalone-peel)
;;; standalone-peel.el ends here
