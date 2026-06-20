;;; anvil-defs-test.el --- Tests for anvil-defs -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 11 Phase 1 + Phase 2 of `anvil-defs'.  Each test
;; drives a private SQLite DB under a temp directory so the real
;; user-level DB (~/.emacs.d/anvil-defs-index.db) is untouched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-defs)


;;;; --- fixture helpers ----------------------------------------------------

(defvar anvil-defs-test--fixture "\
;;; fx.el --- test fixture -*- lexical-binding: t; -*-

;;; Commentary:
;; fixture for anvil-defs tests.

;;; Code:

(require 'subr-x)

(defvar fx-counter 0
  \"Count of greetings emitted by fx-greet.\")

(defun fx-greet (name &optional title)
  \"Greet NAME optionally prefixed by TITLE.
Note: this docstring mentions fx-greet but the string token
must not be counted as a reference.\"
  (setq fx-counter (1+ fx-counter))
  (format \"Hello %s%s\" (or title \"\") name))

(defun fx-greet-variadic (name &rest rest)
  \"Variadic greeting.\"
  (apply #'fx-greet name rest))

(defmacro fx-when-greeting (pred &rest body)
  \"Run BODY when PRED is truthy.\"
  (declare (indent 1))
  `(when ,pred ,@body))

(defun fx-call-greet ()
  \"Exercises a call to fx-greet and a quoted reference.\"
  (fx-greet \"world\")
  (let ((handlers (list #'fx-greet 'fx-greet-variadic)))
    (ignore handlers)))

(provide 'fx)
;;; fx.el ends here
")

(defun anvil-defs-test--with-fixture (fn)
  "Create a temp dir with a single fx.el fixture, call FN with (dir file)."
  (let* ((dir (make-temp-file "anvil-defs-test-" t))
         (file (expand-file-name "fx.el" dir))
         (db (expand-file-name "anvil-defs.db" dir))
         (anvil-defs-index-db-path db)
         (anvil-defs-paths (list dir))
         (anvil-defs--db nil)
         (anvil-defs--backend nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert anvil-defs-test--fixture))
          (funcall fn dir file))
      (when anvil-defs--db
        (anvil-defs--close anvil-defs--db)
        (setq anvil-defs--db nil))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(defun anvil-defs-test--rebuild (dir)
  "Run rebuild against DIR, return the stats plist."
  (anvil-defs-index-rebuild (list dir)))


;;;; --- smoke --------------------------------------------------------------

(ert-deftest anvil-defs-test-feature-provided ()
  (should (featurep 'anvil-defs)))

(ert-deftest anvil-defs-test-enable-disable-callable ()
  (should (fboundp 'anvil-defs-enable))
  (should (fboundp 'anvil-defs-disable)))


;;;; --- rebuild ------------------------------------------------------------

(ert-deftest anvil-defs-test-rebuild-empty-dir ()
  "Rebuilding an empty directory yields all-zero counts."
  (let* ((dir (make-temp-file "anvil-defs-empty-" t))
         (db (expand-file-name "db.sqlite" dir))
         (anvil-defs-index-db-path db)
         (anvil-defs-paths (list dir))
         (anvil-defs--db nil)
         (anvil-defs--backend nil))
    (unwind-protect
        (let ((r (anvil-defs-index-rebuild (list dir))))
          (should (= 0 (plist-get r :files)))
          (should (= 0 (plist-get r :defs)))
          (should (= 0 (plist-get r :refs)))
          (should (= 0 (plist-get r :features))))
      (when anvil-defs--db
        (anvil-defs--close anvil-defs--db)
        (setq anvil-defs--db nil))
      (when (file-directory-p dir) (delete-directory dir t)))))

(ert-deftest anvil-defs-test-rebuild-fixture-counts ()
  "Rebuild against fx.el finds the expected kinds of rows."
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (let ((r (anvil-defs-test--rebuild dir)))
       (should (= 1 (plist-get r :files)))
       (should (>= (plist-get r :defs) 4))   ; fx-counter, fx-greet, fx-greet-variadic, fx-when-greeting, fx-call-greet
       (should (>= (plist-get r :refs) 1))
       (should (>= (plist-get r :features) 2)) ; require subr-x + provide fx
       (should (numberp (plist-get r :duration-ms)))))))


;;;; --- search -------------------------------------------------------------

(ert-deftest anvil-defs-test-search-exact ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((hits (anvil-defs-search "fx-greet")))
       (should (= 1 (length hits)))
       (should (equal "fx-greet" (plist-get (car hits) :name)))
       (should (equal "defun" (plist-get (car hits) :kind)))))))

(ert-deftest anvil-defs-test-search-fuzzy ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((hits (anvil-defs-search "greet" :fuzzy t)))
       (should (>= (length hits) 3))
       (should (member "fx-greet"
                       (mapcar (lambda (h) (plist-get h :name)) hits)))))))

(ert-deftest anvil-defs-test-search-kind-filter ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((only-defvars
            (anvil-defs-search "fx" :kind 'defvar :fuzzy t)))
       (should (> (length only-defvars) 0))
       (dolist (h only-defvars)
         (should (equal (plist-get h :kind) "defvar")))))))


;;;; --- references --------------------------------------------------------

(ert-deftest anvil-defs-test-references-covers-call-and-quote ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let* ((hits (anvil-defs-references "fx-greet"))
            (kinds (delete-dups (mapcar (lambda (h) (plist-get h :kind))
                                        hits))))
       (should (member "call" kinds))
       (should (member "quote" kinds))))))

(ert-deftest anvil-defs-test-references-kind-filter ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((calls-only (anvil-defs-references "fx-greet" :kind "call"))
           (quotes-only (anvil-defs-references "fx-greet" :kind "quote")))
       (should (> (length calls-only) 0))
       (dolist (h calls-only)
         (should (equal "call" (plist-get h :kind))))
       (dolist (h quotes-only)
         (should (equal "quote" (plist-get h :kind))))))))

(ert-deftest anvil-defs-test-references-excludes-docstring ()
  "The fx-greet token inside the docstring body must not be a ref."
  (anvil-defs-test--with-fixture
   (lambda (dir file)
     (anvil-defs-test--rebuild dir)
     (let* ((hits (anvil-defs-references "fx-greet"))
            (lines (mapcar (lambda (h) (plist-get h :line)) hits)))
       ;; The docstring mention lives on the line carrying "but the"
       ;; — read the fixture to find it and assert no ref is on it.
       (with-temp-buffer
         (insert-file-contents file)
         (goto-char (point-min))
         (when (re-search-forward "mentions fx-greet but" nil t)
           (let ((line (line-number-at-pos (match-beginning 0))))
             (should-not (memq line lines)))))))))


;;;; --- signature --------------------------------------------------------

(ert-deftest anvil-defs-test-signature-optional ()
  "fx-greet has 1 required arg + 1 &optional => (1 . 2)."
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((sig (anvil-defs-signature "fx-greet")))
       (should (equal 1 (plist-get sig :arity-min)))
       (should (equal 2 (plist-get sig :arity-max)))
       (should (equal "defun" (plist-get sig :kind)))))))

(ert-deftest anvil-defs-test-signature-rest ()
  "fx-greet-variadic has &rest => arity-max nil."
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((sig (anvil-defs-signature "fx-greet-variadic")))
       (should (equal 1 (plist-get sig :arity-min)))
       (should (null (plist-get sig :arity-max)))))))

(ert-deftest anvil-defs-test-signature-unknown ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (should (null (anvil-defs-signature "no-such-symbol-ever"))))))


;;;; --- who-requires -----------------------------------------------------

(ert-deftest anvil-defs-test-who-requires ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((files (anvil-defs-who-requires "subr-x")))
       (should (= 1 (length files)))
       (should (string-suffix-p "fx.el" (car files)))))))


;;;; --- index-status + refresh-if-stale ---------------------------------

(ert-deftest anvil-defs-test-index-status-matches-rebuild ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (let ((built (anvil-defs-test--rebuild dir))
           (stat (anvil-defs-index-status)))
       (should (equal (plist-get built :files) (plist-get stat :files)))
       (should (equal (plist-get built :defs) (plist-get stat :defs)))
       (should (equal (plist-get built :refs) (plist-get stat :refs)))))))

(ert-deftest anvil-defs-test-refresh-if-stale-reingests-edit ()
  "Editing a file and calling refresh-if-stale picks the new def up."
  (anvil-defs-test--with-fixture
   (lambda (dir file)
     (anvil-defs-test--rebuild dir)
     (should-not (anvil-defs-signature "fx-brand-new"))
     ;; Edit the file — append a new defun — and advance mtime.
     (with-temp-buffer
       (insert-file-contents file)
       (goto-char (point-max))
       (re-search-backward ";;; fx.el ends here")
       (goto-char (match-beginning 0))
       (insert "(defun fx-brand-new () \"new.\" 42)\n\n")
       (write-region (point-min) (point-max) file nil 'silent))
     ;; `set-file-times' bumps mtime forward so refresh-if-stale sees it.
     (set-file-times file (time-add (current-time) 5))
     (let ((touched (anvil-defs-refresh-if-stale file)))
       (should (= 1 (length touched)))
       (should (anvil-defs-signature "fx-brand-new"))))))


;;;; --- trace-path (call graph) ------------------------------------------

(defvar anvil-defs-test--trace-fixture "\
;;; tc.el --- trace fixture -*- lexical-binding: t; -*-

;;; Commentary:
;; call chain: tc-a -> tc-b -> tc-c (leaf); tc-d -> tc-b (2nd caller).

;;; Code:

(defun tc-c ()
  \"Leaf — only calls a builtin.\"
  (+ 1 2))

(defun tc-b ()
  \"Mid — calls the leaf.\"
  (tc-c))

(defun tc-a ()
  \"Top — calls the mid.\"
  (tc-b))

(defun tc-d ()
  \"Second caller of tc-b.\"
  (tc-b))

(provide 'tc)
;;; tc.el ends here
")

(defun anvil-defs-test--with-trace-fixture (fn)
  "Create a temp dir with tc.el (a small call chain), rebuild, call FN with dir."
  (let* ((dir (make-temp-file "anvil-defs-trace-" t))
         (file (expand-file-name "tc.el" dir))
         (db (expand-file-name "anvil-defs.db" dir))
         (anvil-defs-index-db-path db)
         (anvil-defs-paths (list dir))
         (anvil-defs--db nil)
         (anvil-defs--backend nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert anvil-defs-test--trace-fixture))
          (anvil-defs-index-rebuild (list dir))
          (funcall fn dir))
      (when anvil-defs--db
        (anvil-defs--close anvil-defs--db)
        (setq anvil-defs--db nil))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(defun anvil-defs-test--names (rows)
  "Return the :name strings from trace ROWS."
  (mapcar (lambda (r) (plist-get r :name)) rows))

(defun anvil-defs-test--depth-of (rows name)
  "Return the :depth of the row in ROWS whose :name equals NAME."
  (plist-get (cl-find name rows
                      :key (lambda (r) (plist-get r :name)) :test #'equal)
             :depth))

(ert-deftest anvil-defs-test-trace-callers-direct ()
  "Direct callers of the leaf is the mid only; self is excluded."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let ((names (anvil-defs-test--names
                   (anvil-defs-trace-path "tc-c" :direction 'callers :depth 1))))
       (should (member "tc-b" names))
       (should-not (member "tc-a" names))
       (should-not (member "tc-c" names))))))

(ert-deftest anvil-defs-test-trace-callers-transitive ()
  "Transitive callers of the leaf cover the whole upstream chain with depths."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let* ((rows (anvil-defs-trace-path "tc-c" :direction 'callers :depth 3))
            (names (anvil-defs-test--names rows)))
       (should (member "tc-b" names))
       (should (member "tc-a" names))
       (should (member "tc-d" names))
       (should (= 1 (anvil-defs-test--depth-of rows "tc-b")))
       (should (= 2 (anvil-defs-test--depth-of rows "tc-a")))
       (should (= 2 (anvil-defs-test--depth-of rows "tc-d")))))))

(ert-deftest anvil-defs-test-trace-depth-cap ()
  "Depth 1 does not reach grand-callers."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let ((names (anvil-defs-test--names
                   (anvil-defs-trace-path "tc-c" :direction 'callers :depth 1))))
       (should (member "tc-b" names))
       (should-not (member "tc-a" names))))))

(ert-deftest anvil-defs-test-trace-callees ()
  "Callees of the top reach the mid then the leaf; all are index-defined."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let* ((rows (anvil-defs-trace-path "tc-a" :direction 'callees :depth 3))
            (names (anvil-defs-test--names rows)))
       (should (member "tc-b" names))
       (should (member "tc-c" names))
       (should (cl-every (lambda (r) (plist-get r :defined)) rows))))))

(ert-deftest anvil-defs-test-trace-internal-only-hides-builtins ()
  "internal-only drops builtin callees; nil includes them."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let ((internal (anvil-defs-test--names
                      (anvil-defs-trace-path "tc-c" :direction 'callees
                                             :internal-only t)))
           (raw (anvil-defs-test--names
                 (anvil-defs-trace-path "tc-c" :direction 'callees
                                        :internal-only nil))))
       (should (null internal))           ; tc-c only calls `+'
       (should (member "+" raw))))))

(ert-deftest anvil-defs-test-trace-tool-wrapper ()
  "The MCP wrapper parses string args and returns caller rows."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let ((names (anvil-defs-test--names
                   (anvil-defs--tool-trace-path "tc-b" "callers" "2"))))
       (should (member "tc-a" names))
       (should (member "tc-d" names))))))

(ert-deftest anvil-defs-test-trace-unknown-symbol ()
  "Tracing an unindexed symbol yields nil in both directions."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (should (null (anvil-defs-trace-path "no-such-symbol-xyz"
                                          :direction 'callers)))
     (should (null (anvil-defs-trace-path "no-such-symbol-xyz"
                                          :direction 'callees))))))


(provide 'anvil-defs-test)
;;; anvil-defs-test.el ends here
