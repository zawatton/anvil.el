;;; anvil-autoresearch-test.el --- Tests for anvil-autoresearch -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 43 Phase 0 — read-only local autoresearch
;; candidate scanner.

;;; Code:

(require 'ert)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'anvil-autoresearch)
(require 'anvil-server)


;;;; --- fixture helpers ----------------------------------------------------

(defmacro anvil-autoresearch-test--with-tree (&rest body)
  "Create a temp archive/design tree and run BODY inside it."
  (declare (indent 0))
  `(let* ((root (make-temp-file "anvil-ar-" t))
          (web (expand-file-name "capture/web" root))
          (yt (expand-file-name "capture/yt" root))
          (design (expand-file-name "docs/design" root)))
     (unwind-protect
         (progn
           (make-directory web t)
           (make-directory yt t)
           (make-directory design t)
           (write-region
            (string-join
             '("#+title: Web X note"
               "* Claude Code resources"
               "TODO DRAFT MCP tool 未実装: X投稿から tool docstring autoresearch 改善候補を抽出する"
               "普通の行")
             "\n")
            nil (expand-file-name "web_20260418_x_example.org" web))
           (write-region
            (string-join
             '("#+title: Autoresearch video"
               "* Runtime harness"
               "Karpathy autoresearch Phase 1 idea: budget heartbeat safety"
               "noise")
             "\n")
            nil (expand-file-name "yt_20260524_autoresearch.org" yt))
           (write-region
            (string-join
             '("#+title: Quiet"
               "* Done"
               "This file has no implementation signal.")
             "\n")
            nil (expand-file-name "quiet.org" design))
           ,@body)
       (delete-directory root t))))

(defmacro anvil-autoresearch-test--with-nelisp-root (&rest body)
  "Create a minimal NeLisp Phase 47 tree and run BODY inside it."
  (declare (indent 0))
  `(let* ((root (make-temp-file "anvil-ar-nelisp-" t))
          (anvil-autoresearch-nelisp-root root))
     (unwind-protect
         (progn
           (dolist (dir '("lisp" "test" "docs/design"))
             (make-directory (expand-file-name dir root) t))
           (dolist (file anvil-autoresearch-nelisp-codegen-target-files)
             (write-region
              "fixture\n"
              nil
              (expand-file-name file root)))
           ,@body)
       (delete-directory root t))))

(defmacro anvil-autoresearch-test--with-tools (&rest body)
  "Run BODY against a fresh in-memory MCP tool registry."
  (declare (indent 0))
  `(let ((anvil-server--tools (make-hash-table :test #'equal))
         (anvil-server--tools-list-cache (make-hash-table :test #'equal))
         (anvil-server-schema-cache-file
          (make-temp-file "anvil-ar-schema-cache-")))
     (unwind-protect
         (progn
           (anvil-server-register-tool
            (lambda () "ok")
            :server-id "anvil-test"
            :id "weak-generic"
            :description "Return info"
            :read-only t
            :intent '(test)
            :layer 'workflow)
           (anvil-server-register-tool
            (lambda () "ok")
            :server-id "anvil-test"
            :id "param-weak"
            :description
            "Use param-weak when a caller needs to inspect path input
handling.  Optional filters narrow the result and the tool returns a
compact report for review."
            :schema '((type . "object")
                      (properties . (("path" . ((type . "string"))))))
            :read-only t
            :intent '(test)
            :layer 'workflow)
           (anvil-server-register-tool
            (lambda () "ok")
            :server-id "anvil-test"
            :id "strong-doc-tool"
            :description
            "Use strong-doc-tool when testing a complete MCP tool
description.  Optional filters narrow the result, required arguments
are documented in the schema, and the tool returns a compact read-only
report for follow-up."
            :schema '((type . "object")
                      (properties
                       . (("query"
                           . ((type . "string")
                              (description
                               . "Optional substring used to filter the report."))))))
            :read-only t
            :intent '(test)
            :layer 'workflow)
           ,@body)
       (ignore-errors (delete-file anvil-server-schema-cache-file)))))

(defmacro anvil-autoresearch-test--with-real-module-tools (&rest body)
  "Run BODY with only the real autoresearch tools registered."
  (declare (indent 0))
  `(let ((anvil-server--tools (make-hash-table :test #'equal))
         (anvil-server--tools-list-cache (make-hash-table :test #'equal))
         (anvil-server-schema-cache-file
          (make-temp-file "anvil-ar-real-schema-cache-")))
     (unwind-protect
         (progn
           (anvil-autoresearch-enable)
           ,@body)
       (ignore-errors (delete-file anvil-server-schema-cache-file)))))

(defun anvil-autoresearch-test--description-site (file tool-id)
  "Return an autoresearch description site plist in FILE for TOOL-ID."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (re-search-forward (format ":id[ \t\n\r]+%S" tool-id))
    (re-search-forward ":description\\_>")
    (skip-chars-forward " \t\n\r")
    (let* ((start (point))
           (start-line (line-number-at-pos))
           (value (read (current-buffer)))
           (end (point))
           (end-line (line-number-at-pos end)))
      (list :file file
            :absolute-file file
            :start-offset start
            :end-offset end
            :start-line start-line
            :end-line end-line
            :old-description value
            :old-literal
            (buffer-substring-no-properties start end)))))


;;;; --- scanner ------------------------------------------------------------

(ert-deftest anvil-autoresearch-test-scan-ranks-candidates ()
  "Scan returns scored, sorted candidates from archive roots."
  (anvil-autoresearch-test--with-tree
    (let* ((roots-json (json-encode (vector web yt design)))
           (res (anvil-autoresearch-scan nil roots-json 10))
           (candidates (append (plist-get res :candidates) nil))
           (top (car candidates)))
      (should (= (plist-get res :files-scanned) 3))
      (should (>= (plist-get res :candidate-count) 2))
      (should (string= (plist-get top :kind) "x-post"))
      (should (string-match-p "MCP tool" (plist-get top :evidence)))
      (should (string= (plist-get top :suggested-action)
                       "candidate-mcp-tool"))
      (should (> (plist-get top :score)
                 (plist-get (cadr candidates) :score))))))

(ert-deftest anvil-autoresearch-test-query-filters-results ()
  "QUERY narrows candidates by evidence / kind / action."
  (anvil-autoresearch-test--with-tree
    (let* ((roots-json (json-encode (vector web yt design)))
           (res (anvil-autoresearch-scan "heartbeat" roots-json 10))
           (candidates (append (plist-get res :candidates) nil)))
      (should (= (length candidates) 1))
      (should (string= (plist-get (car candidates) :kind)
                       "youtube-summary"))
      (should (string-match-p "heartbeat"
                              (plist-get (car candidates) :evidence))))))

(ert-deftest anvil-autoresearch-test-limit-and-default-roots ()
  "Configured roots and LIMIT are honored."
  (anvil-autoresearch-test--with-tree
    (let* ((anvil-autoresearch-roots (list web yt design))
           (res (anvil-autoresearch-scan nil nil "1"))
           (candidates (append (plist-get res :candidates) nil)))
      (should (= (plist-get res :limit) 1))
      (should (= (length candidates) 1))
      (should (= (plist-get res :files-scanned) 3)))))

(ert-deftest anvil-autoresearch-test-tool-wrapper-json-encodes ()
  "MCP wrapper emits JSON when registered through encode-handler."
  (anvil-autoresearch-test--with-tree
    (let* ((roots-json (json-encode (vector web yt design)))
           (handler (anvil-server-encode-handler
                     #'anvil-autoresearch--tool-scan))
           (json (funcall handler "MCP" roots-json 5))
           (decoded (json-parse-string json
                                       :object-type 'alist
                                       :array-type 'list)))
      (should (stringp json))
      (should (alist-get 'candidates decoded))
      (should (= (alist-get 'candidate-count decoded) 1)))))


;;;; --- docstring target scoring ------------------------------------------

(ert-deftest anvil-autoresearch-test-docstrings-ranks-weak-tools ()
  "Docstring evaluator ranks weak descriptions and skips clean ones."
  (anvil-autoresearch-test--with-tools
    (let* ((res (anvil-autoresearch-docstrings nil "anvil-test" 10))
           (candidates (append (plist-get res :candidates) nil))
           (ids (mapcar (lambda (c) (plist-get c :id)) candidates))
           (top (car candidates)))
      (should (= (plist-get res :registered-count) 3))
      (should (member "weak-generic" ids))
      (should (member "param-weak" ids))
      (should-not (member "strong-doc-tool" ids))
      (should (string= (plist-get top :id) "weak-generic"))
      (should (seq-contains-p (append (plist-get top :issues) nil)
                              "short-description")))))

(ert-deftest anvil-autoresearch-test-docstrings-query-filters-issues ()
  "QUERY can target a specific issue class."
  (anvil-autoresearch-test--with-tools
    (let* ((res (anvil-autoresearch-docstrings
                 "param-description-missing" "anvil-test" 10))
           (candidates (append (plist-get res :candidates) nil))
           (candidate (car candidates)))
      (should (= (length candidates) 1))
      (should (string= (plist-get candidate :id) "param-weak"))
      (should (seq-contains-p (append (plist-get candidate :issues) nil)
                              "param-description-missing:path")))))

(ert-deftest anvil-autoresearch-test-docstrings-tool-wrapper-json-encodes ()
  "Docstring MCP wrapper emits JSON through encode-handler."
  (anvil-autoresearch-test--with-tools
    (let* ((handler (anvil-server-encode-handler
                     #'anvil-autoresearch--tool-docstrings))
           (json (funcall handler "" "anvil-test" 1))
           (decoded (json-parse-string json
                                       :object-type 'alist
                                       :array-type 'list)))
      (should (stringp json))
      (should (= (alist-get 'limit decoded) 1))
      (should (= (length (alist-get 'candidates decoded)) 1)))))


;;;; --- docstring rewrite planner -----------------------------------------

(ert-deftest anvil-autoresearch-test-docstring-plan-explicit-tool ()
  "Planner returns a read-only rewrite plan for an explicit tool."
  (anvil-autoresearch-test--with-tools
    (let ((plan (anvil-autoresearch-docstring-plan
                 "weak-generic" "anvil-test")))
      (should (string= (plist-get plan :id) "weak-generic"))
      (should (string= (plist-get plan :current-description)
                       "Return info"))
      (should (seq-contains-p (append (plist-get plan :issues) nil)
                              "short-description"))
      (should (string-match-p "Use `weak-generic'"
                              (plist-get plan :draft-description)))
      (should (string-match-p "Rewrite the MCP :description"
                              (plist-get plan :rewrite-prompt)))
      (should (equal (plist-get plan :suggested-action)
                     "apply-description-rewrite")))))

(ert-deftest anvil-autoresearch-test-docstring-plan-selects-top-candidate ()
  "Planner without TOOL-ID selects the top docstring candidate."
  (anvil-autoresearch-test--with-tools
    (let ((plan (anvil-autoresearch-docstring-plan nil "anvil-test")))
      (should (string= (plist-get plan :id) "weak-generic"))
      (should (> (plist-get plan :score) 0)))))

(ert-deftest anvil-autoresearch-test-docstring-plan-tool-wrapper-json-encodes ()
  "Docstring planner MCP wrapper emits JSON through encode-handler."
  (anvil-autoresearch-test--with-tools
    (let* ((handler (anvil-server-encode-handler
                     #'anvil-autoresearch--tool-docstring-plan))
           (json (funcall handler "param-weak" "anvil-test"))
           (decoded (json-parse-string json
                                       :object-type 'alist
                                       :array-type 'list)))
      (should (stringp json))
      (should (equal (alist-get 'id decoded) "param-weak"))
      (should (alist-get 'rewrite-prompt decoded))
      (should (= (length (alist-get 'params decoded)) 1)))))


;;;; --- docstring dry-run patch -------------------------------------------

(ert-deftest anvil-autoresearch-test-docstring-patch-explicit-tool ()
  "Dry-run patch returns replacement bounds and patch text."
  (anvil-autoresearch-test--with-real-module-tools
    (let* ((replacement
            "Use autoresearch-docstring-plan to inspect one MCP tool description rewrite plan. Read-only; returns the current description, scored issues, registration hint, draft, and prompt.")
           (res (anvil-autoresearch-docstring-patch
                 "autoresearch-docstring-plan"
                 "emacs-eval"
                 replacement)))
      (should (eq (plist-get res :dry-run) t))
      (should (string= (plist-get res :id)
                       "autoresearch-docstring-plan"))
      (should (string-suffix-p "anvil-autoresearch.el"
                               (plist-get res :file)))
      (should (string-match-p "Read-only Doc 43 Phase 1b"
                              (plist-get res :old-literal)))
      (should (string-match-p "Use autoresearch-docstring-plan"
                              (plist-get res :new-literal)))
      (should (string-match-p "--- a/anvil-autoresearch.el"
                              (plist-get res :patch)))
      (should (string= (plist-get res :suggested-action)
                       "review-dry-run-patch")))))

(ert-deftest anvil-autoresearch-test-docstring-patch-wrapper-json-encodes ()
  "Dry-run patch MCP wrapper emits JSON through encode-handler."
  (anvil-autoresearch-test--with-real-module-tools
    (let* ((handler (anvil-server-encode-handler
                     #'anvil-autoresearch--tool-docstring-patch))
           (json (funcall handler
                          "autoresearch-docstrings"
                          "emacs-eval"
                          "Use autoresearch-docstrings to rank weak MCP tool descriptions. Read-only; returns scored candidates."))
           (decoded (json-parse-string json
                                       :object-type 'alist
                                       :array-type 'list)))
      (should (stringp json))
      (should (eq (alist-get 'dry-run decoded) t))
      (should (equal (alist-get 'id decoded)
                     "autoresearch-docstrings"))
      (should (alist-get 'patch decoded)))))


;;;; --- docstring apply/eval loop -----------------------------------------

(ert-deftest anvil-autoresearch-test-docstring-apply-keeps-on-pass ()
  "Apply/eval rewrites the description and keeps it when eval passes."
  (anvil-autoresearch-test--with-tools
    (let* ((file (make-temp-file "anvil-ar-apply-" nil ".el"))
           (replacement
            "Use weak-generic when testing the autoresearch apply loop. Read-only; returns a compact result after validation."))
      (unwind-protect
          (progn
            (write-region
             "(anvil-server-register-tool #'ignore\n :id \"weak-generic\"\n :description \"Return info\")\n"
             nil file nil 'silent)
            (cl-letf (((symbol-function
                        'anvil-autoresearch--find-description-site)
                       (lambda (tool-id)
                         (anvil-autoresearch-test--description-site
                          file tool-id))))
              (let ((res (anvil-autoresearch-docstring-apply
                          "weak-generic" "anvil-test"
                          replacement "true")))
                (should (eq (plist-get res :kept) t))
                (should-not (plist-get res :reverted))
                (should (equal "passed"
                               (plist-get (plist-get res :eval)
                                          :status)))
                (should (equal "skipped"
                               (plist-get (plist-get res :holdout)
                                          :status)))
                (should (string-match-p
                         "autoresearch apply loop"
                         (with-temp-buffer
                           (insert-file-contents-literally file)
                           (buffer-string)))))))
        (ignore-errors (delete-file file))))))

(ert-deftest anvil-autoresearch-test-docstring-apply-reverts-on-failure ()
  "Apply/eval restores the original file when eval fails."
  (anvil-autoresearch-test--with-tools
    (let* ((file (make-temp-file "anvil-ar-revert-" nil ".el"))
           (original
            "(anvil-server-register-tool #'ignore\n :id \"weak-generic\"\n :description \"Return info\")\n")
           (replacement
            "Use weak-generic when testing a failing autoresearch eval. Read-only; returns a compact result."))
      (unwind-protect
          (progn
            (write-region original nil file nil 'silent)
            (cl-letf (((symbol-function
                        'anvil-autoresearch--find-description-site)
                       (lambda (tool-id)
                         (anvil-autoresearch-test--description-site
                          file tool-id))))
              (let ((res (anvil-autoresearch-docstring-apply
                          "weak-generic" "anvil-test"
                          replacement "false")))
                (should-not (plist-get res :kept))
                (should (eq (plist-get res :reverted) t))
                (should (equal "failed"
                               (plist-get (plist-get res :eval)
                                          :status)))
                (should (equal original
                               (with-temp-buffer
                                 (insert-file-contents-literally file)
                                 (buffer-string)))))))
        (ignore-errors (delete-file file))))))

(ert-deftest anvil-autoresearch-test-docstring-apply-holdout-reverts ()
  "Apply/eval restores the original file when holdout fails after eval passes."
  (anvil-autoresearch-test--with-tools
    (let* ((file (make-temp-file "anvil-ar-holdout-" nil ".el"))
           (original
            "(anvil-server-register-tool #'ignore\n :id \"weak-generic\"\n :description \"Return info\")\n")
           (replacement
            "Use weak-generic when testing a holdout autoresearch eval. Read-only; returns a compact result."))
      (unwind-protect
          (progn
            (write-region original nil file nil 'silent)
            (cl-letf (((symbol-function
                        'anvil-autoresearch--find-description-site)
                       (lambda (tool-id)
                         (anvil-autoresearch-test--description-site
                          file tool-id))))
              (let ((res (anvil-autoresearch-docstring-apply
                          "weak-generic" "anvil-test"
                          replacement "true" nil "false")))
                (should-not (plist-get res :kept))
                (should (eq (plist-get res :reverted) t))
                (should (equal "holdout-failed"
                               (plist-get res :revert-reason)))
                (should (equal "passed"
                               (plist-get (plist-get res :eval)
                                          :status)))
                (should (equal "failed"
                               (plist-get (plist-get res :holdout)
                                          :status)))
                (should (equal original
                               (with-temp-buffer
                                 (insert-file-contents-literally file)
                                 (buffer-string)))))))
        (ignore-errors (delete-file file))))))

(ert-deftest anvil-autoresearch-test-docstring-apply-wrapper-json-encodes ()
  "Apply/eval MCP wrapper emits JSON through encode-handler."
  (anvil-autoresearch-test--with-tools
    (let* ((file (make-temp-file "anvil-ar-wrapper-" nil ".el"))
           (replacement
            "Use weak-generic when testing the MCP wrapper apply loop. Read-only; returns JSON with eval status."))
      (unwind-protect
          (progn
            (write-region
             "(anvil-server-register-tool #'ignore\n :id \"weak-generic\"\n :description \"Return info\")\n"
             nil file nil 'silent)
            (cl-letf (((symbol-function
                        'anvil-autoresearch--find-description-site)
                       (lambda (tool-id)
                         (anvil-autoresearch-test--description-site
                          file tool-id))))
              (let* ((handler (anvil-server-encode-handler
                               #'anvil-autoresearch--tool-docstring-apply))
                     (json (funcall handler
                                    "weak-generic" "anvil-test"
                                    replacement "true" "false"))
                     (decoded (json-parse-string
                               json :object-type 'alist
                               :array-type 'list)))
                (should (stringp json))
                (should (eq (alist-get 'kept decoded) t))
                (should (equal "weak-generic" (alist-get 'id decoded))))))
        (ignore-errors (delete-file file))))))


;;;; --- docstring multi-candidate run -------------------------------------

(ert-deftest anvil-autoresearch-test-docstring-run-keeps-best-passing ()
  "Multi-candidate run keeps the lowest post-score passing rewrite."
  (anvil-autoresearch-test--with-tools
    (let* ((file (make-temp-file "anvil-ar-run-" nil ".el"))
           (original
            "(anvil-server-register-tool #'ignore\n :id \"weak-generic\"\n :description \"Return info\")\n")
           (weak "Return more info")
           (strong
            "Use weak-generic when testing the autoresearch multi-candidate loop. Read-only; returns a compact result after validation.")
           (candidates-json
            (json-encode
             (vector `((label . "weak") (description . ,weak))
                     `((label . "strong") (description . ,strong))))))
      (unwind-protect
          (progn
            (write-region original nil file nil 'silent)
            (cl-letf (((symbol-function
                        'anvil-autoresearch--find-description-site)
                       (lambda (tool-id)
                         (anvil-autoresearch-test--description-site
                          file tool-id))))
              (let* ((res (anvil-autoresearch-docstring-run
                           "weak-generic" "anvil-test"
                           candidates-json "true" "true"))
                     (attempts (append (plist-get res :attempts) nil))
                     (selected (plist-get res :selected))
                     (content (with-temp-buffer
                                (insert-file-contents-literally file)
                                (buffer-string))))
                (should (eq (plist-get res :kept) t))
                (should (= (length attempts) 2))
                (should (seq-every-p
                         (lambda (attempt)
                           (plist-get attempt :passed))
                         attempts))
                (should (equal (plist-get selected :label) "strong"))
                (should (< (plist-get selected :post-score)
                           (plist-get (car attempts) :post-score)))
                (should (string-match-p "multi-candidate loop" content))
                (should-not (string-match-p "Return more info" content)))))
        (ignore-errors (delete-file file))))))

(ert-deftest anvil-autoresearch-test-docstring-run-reverts-when-none-pass ()
  "Multi-candidate run restores the original file when every gate fails."
  (anvil-autoresearch-test--with-tools
    (let* ((file (make-temp-file "anvil-ar-run-revert-" nil ".el"))
           (original
            "(anvil-server-register-tool #'ignore\n :id \"weak-generic\"\n :description \"Return info\")\n")
           (candidates-json (json-encode (vector "Return more info"))))
      (unwind-protect
          (progn
            (write-region original nil file nil 'silent)
            (cl-letf (((symbol-function
                        'anvil-autoresearch--find-description-site)
                       (lambda (tool-id)
                         (anvil-autoresearch-test--description-site
                          file tool-id))))
              (let ((res (anvil-autoresearch-docstring-run
                          "weak-generic" "anvil-test"
                          candidates-json "false" "true")))
                (should-not (plist-get res :kept))
                (should (eq (plist-get res :reverted) t))
                (should (equal "eval-failed"
                               (plist-get
                                (aref (plist-get res :attempts) 0)
                                :reject-reason)))
                (should (equal original
                               (with-temp-buffer
                                 (insert-file-contents-literally file)
                                 (buffer-string)))))))
        (ignore-errors (delete-file file))))))

(ert-deftest anvil-autoresearch-test-docstring-run-wrapper-json-encodes ()
  "Multi-candidate run MCP wrapper emits JSON through encode-handler."
  (anvil-autoresearch-test--with-tools
    (let* ((file (make-temp-file "anvil-ar-run-wrapper-" nil ".el"))
           (replacement
            "Use weak-generic when testing the MCP wrapper multi-candidate loop. Read-only; returns JSON with candidate attempts.")
           (candidates-json (json-encode (vector replacement))))
      (unwind-protect
          (progn
            (write-region
             "(anvil-server-register-tool #'ignore\n :id \"weak-generic\"\n :description \"Return info\")\n"
             nil file nil 'silent)
            (cl-letf (((symbol-function
                        'anvil-autoresearch--find-description-site)
                       (lambda (tool-id)
                         (anvil-autoresearch-test--description-site
                          file tool-id))))
              (let* ((handler (anvil-server-encode-handler
                               #'anvil-autoresearch--tool-docstring-run))
                     (json (funcall handler
                                    "weak-generic" "anvil-test"
                                    candidates-json "true" "true" "3"))
                     (decoded (json-parse-string
                               json :object-type 'alist
                               :array-type 'list)))
                (should (stringp json))
                (should (eq (alist-get 'kept decoded) t))
                (should (equal "weak-generic" (alist-get 'id decoded)))
                (should (= (length (alist-get 'attempts decoded)) 1)))))
        (ignore-errors (delete-file file))))))


;;;; --- docstring orchestrator proposal -----------------------------------

(ert-deftest anvil-autoresearch-test-docstring-propose-submits-bounded-task ()
  "Proposal submit passes budget / timeout / heartbeat / sandbox to orchestrator."
  (anvil-autoresearch-test--with-tools
    (let (submitted)
      (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                 (lambda (tasks)
                   (setq submitted tasks)
                   "batch-1")))
        (let* ((res (anvil-autoresearch-docstring-propose
                     "weak-generic" "anvil-test"
                     "codex" "gpt-test" "0.07" "90" "30"
                     default-directory "workspace-write"))
               (task (car submitted)))
          (should (equal (plist-get res :batch-id) "batch-1"))
          (should (eq (plist-get task :provider) 'codex))
          (should (equal (plist-get task :model) "gpt-test"))
          (should (= (plist-get task :budget-usd) 0.07))
          (should (= (plist-get task :timeout-sec) 90))
          (should (= (plist-get task :heartbeat-timeout-sec) 30))
          (should (equal (plist-get task :sandbox) "workspace-write"))
          (should (string-match-p "weak-generic"
                                  (plist-get task :prompt)))
          (should (string-match-p "Return info"
                                  (plist-get task :prompt)))
          (should (equal (plist-get res :suggested-action)
                         "poll-orchestrator-batch-then-review-description")))))))

(ert-deftest anvil-autoresearch-test-docstring-propose-wrapper-json-encodes ()
  "Proposal MCP wrapper emits JSON through encode-handler."
  (anvil-autoresearch-test--with-tools
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (_tasks) "batch-json")))
      (let* ((handler (anvil-server-encode-handler
                       #'anvil-autoresearch--tool-docstring-propose))
             (json (funcall handler
                            "weak-generic" "anvil-test"
                            "codex" "" "0.05" "80" "25"
                            default-directory "workspace-write"))
             (decoded (json-parse-string
                       json :object-type 'alist
                       :array-type 'list))
             (task (alist-get 'task decoded)))
        (should (stringp json))
        (should (equal (alist-get 'batch-id decoded) "batch-json"))
        (should (equal (alist-get 'provider task) "codex"))
        (should (= (alist-get 'budget-usd task) 0.05))
        (should (= (alist-get 'heartbeat-timeout-sec task) 25))))))

(ert-deftest anvil-autoresearch-test-docstring-propose-worktree-controls ()
  "Proposal submit forwards explicit worktree control metadata."
  (anvil-autoresearch-test--with-tools
    (let (submitted)
      (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                 (lambda (tasks)
                   (setq submitted tasks)
                   "batch-wt")))
        (let* ((res (anvil-autoresearch-docstring-propose
                     "weak-generic" "anvil-test"
                     "codex" "" "0.05" "80" "25"
                     default-directory "workspace-write"
                     "false" "ar-docstring"))
               (task (car submitted)))
          (should (equal (plist-get res :batch-id) "batch-wt"))
          (should-not (plist-get task :no-worktree))
          (should (equal (plist-get task :worktree-name)
                         "ar-docstring")))))))

(ert-deftest anvil-autoresearch-test-docstring-propose-no-worktree-wins ()
  "A truthy no-worktree flag suppresses worktree-name metadata."
  (anvil-autoresearch-test--with-tools
    (let (submitted)
      (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                 (lambda (tasks)
                   (setq submitted tasks)
                   "batch-no-wt")))
        (anvil-autoresearch-docstring-propose
         "weak-generic" "anvil-test"
         "codex" "" "0.05" "80" "25"
         default-directory "workspace-write" "true" "ignored-name")
        (let ((task (car submitted)))
          (should (eq (plist-get task :no-worktree) t))
          (should-not (plist-get task :worktree-name)))))))

(ert-deftest anvil-autoresearch-test-docstring-propose-batch-submits-n-tasks ()
  "Batch proposal submit fans out bounded tasks with unique names."
  (anvil-autoresearch-test--with-tools
    (let (submitted)
      (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                 (lambda (tasks)
                   (setq submitted tasks)
                   "batch-n")))
        (let* ((res (anvil-autoresearch-docstring-propose-batch
                     "weak-generic" "anvil-test" "3"
                     "codex" "gpt-test" "0.07" "90" "30"
                     default-directory "workspace-write"
                     "false" "ar-batch"))
               (tasks (append (plist-get res :tasks) nil)))
          (should (equal (plist-get res :batch-id) "batch-n"))
          (should (= (plist-get res :count) 3))
          (should (= (length submitted) 3))
          (should (= (length tasks) 3))
          (should (equal (mapcar (lambda (task)
                                   (plist-get task :name))
                                 submitted)
                         '("autoresearch-docstring-weak-generic-01-of-03"
                           "autoresearch-docstring-weak-generic-02-of-03"
                           "autoresearch-docstring-weak-generic-03-of-03")))
          (should (seq-every-p
                   (lambda (task)
                     (and (eq (plist-get task :provider) 'codex)
                          (equal (plist-get task :model) "gpt-test")
                          (= (plist-get task :budget-usd) 0.07)
                          (= (plist-get task :timeout-sec) 90)
                          (= (plist-get task :heartbeat-timeout-sec) 30)
                          (equal (plist-get task :sandbox)
                                 "workspace-write")))
                   submitted))
          (should (equal (mapcar (lambda (task)
                                   (plist-get task :worktree-name))
                                 submitted)
                         '("ar-batch-01-of-03"
                           "ar-batch-02-of-03"
                           "ar-batch-03-of-03")))
          (should (string-match-p "candidate 1 of 3"
                                  (plist-get (car submitted) :prompt)))
          (should (string-match-p "candidate 3 of 3"
                                  (plist-get (caddr submitted) :prompt)))
          (should (equal (plist-get res :suggested-action)
                         "harvest-orchestrator-proposal-batch")))))))

(ert-deftest anvil-autoresearch-test-docstring-propose-batch-wrapper-json-encodes ()
  "Batch proposal MCP wrapper emits JSON through encode-handler."
  (anvil-autoresearch-test--with-tools
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (_tasks) "batch-json-n")))
      (let* ((handler (anvil-server-encode-handler
                       #'anvil-autoresearch--tool-docstring-propose-batch))
             (json (funcall handler
                            "weak-generic" "anvil-test" "2"
                            "codex" "" "0.05" "80" "25"
                            default-directory "workspace-write"))
             (decoded (json-parse-string
                       json :object-type 'alist
                       :array-type 'list))
             (tasks (alist-get 'tasks decoded)))
        (should (stringp json))
        (should (equal (alist-get 'batch-id decoded) "batch-json-n"))
        (should (= (alist-get 'count decoded) 2))
        (should (= (length tasks) 2))
        (should (equal (alist-get 'provider (car tasks)) "codex"))
        (should (= (alist-get 'heartbeat-timeout-sec (car tasks)) 25))))))


;;;; --- docstring proposal harvest ----------------------------------------

(ert-deftest anvil-autoresearch-test-docstring-harvest-keeps-best-summary ()
  "Harvest turns done task summaries into candidates and keeps the best."
  (anvil-autoresearch-test--with-tools
    (let* ((file (make-temp-file "anvil-ar-harvest-" nil ".el"))
           (original
            "(anvil-server-register-tool #'ignore\n :id \"weak-generic\"\n :description \"Return info\")\n")
           (weak "Return more info")
           (strong
            "Use weak-generic when testing proposal harvest. Read-only; returns a compact result after validation."))
      (unwind-protect
          (progn
            (write-region original nil file nil 'silent)
            (cl-letf (((symbol-function
                        'anvil-autoresearch--find-description-site)
                       (lambda (tool-id)
                         (anvil-autoresearch-test--description-site
                          file tool-id)))
                      ((symbol-function 'anvil-orchestrator-collect)
                       (lambda (batch-id &rest _args)
                         (should (equal batch-id "batch-harvest"))
                         (list (list :id "t1" :name "weak"
                                     :status 'done :summary weak)
                               (list :id "t2" :name "strong"
                                     :status 'done :summary strong)
                               (list :id "t3" :name "failed"
                                     :status 'failed :summary
                                     "ignored")))))
              (let* ((res (anvil-autoresearch-docstring-harvest
                           "batch-harvest" "weak-generic" "anvil-test"
                           "true" "true"))
                     (run (plist-get res :run))
                     (selected (plist-get run :selected))
                     (content (with-temp-buffer
                                (insert-file-contents-literally file)
                                (buffer-string))))
                (should (eq (plist-get res :kept) t))
                (should (= (plist-get res :candidate-count) 2))
                (should (equal (plist-get selected :label) "strong"))
                (should (string-match-p "proposal harvest" content))
                (should-not (string-match-p "Return more info" content)))))
        (ignore-errors (delete-file file))))))

(ert-deftest anvil-autoresearch-test-docstring-harvest-no-candidates-no-edit ()
  "Harvest does not edit source when no done summaries are usable."
  (anvil-autoresearch-test--with-tools
    (let* ((file (make-temp-file "anvil-ar-harvest-none-" nil ".el"))
           (original
            "(anvil-server-register-tool #'ignore\n :id \"weak-generic\"\n :description \"Return info\")\n"))
      (unwind-protect
          (progn
            (write-region original nil file nil 'silent)
            (cl-letf (((symbol-function
                        'anvil-autoresearch--find-description-site)
                       (lambda (tool-id)
                         (anvil-autoresearch-test--description-site
                          file tool-id)))
                      ((symbol-function 'anvil-orchestrator-collect)
                       (lambda (_batch-id &rest _args)
                         (list (list :id "t1" :status 'running
                                     :summary nil)
                               (list :id "t2" :status 'failed
                                     :summary "not used")))))
              (let ((res (anvil-autoresearch-docstring-harvest
                          "batch-empty" "weak-generic" "anvil-test"
                          "true" "true")))
                (should-not (plist-get res :kept))
                (should (= (plist-get res :candidate-count) 0))
                (should-not (plist-get res :run))
                (should (equal original
                               (with-temp-buffer
                                 (insert-file-contents-literally file)
                                 (buffer-string)))))))
        (ignore-errors (delete-file file))))))

(ert-deftest anvil-autoresearch-test-docstring-harvest-wrapper-json-encodes ()
  "Harvest MCP wrapper emits JSON through encode-handler."
  (anvil-autoresearch-test--with-tools
    (let* ((file (make-temp-file "anvil-ar-harvest-wrapper-" nil ".el"))
           (replacement
            "Use weak-generic when testing harvest wrapper JSON. Read-only; returns a compact JSON result."))
      (unwind-protect
          (progn
            (write-region
             "(anvil-server-register-tool #'ignore\n :id \"weak-generic\"\n :description \"Return info\")\n"
             nil file nil 'silent)
            (cl-letf (((symbol-function
                        'anvil-autoresearch--find-description-site)
                       (lambda (tool-id)
                         (anvil-autoresearch-test--description-site
                          file tool-id)))
                      ((symbol-function 'anvil-orchestrator-collect)
                       (lambda (_batch-id &rest _args)
                         (list (list :id "t1" :name "proposal"
                                     :status 'done
                                     :summary replacement)))))
              (let* ((handler (anvil-server-encode-handler
                               #'anvil-autoresearch--tool-docstring-harvest))
                     (json (funcall handler
                                    "batch-json" "weak-generic"
                                    "anvil-test" "true" "true" "false"
                                    "2"))
                     (decoded (json-parse-string
                               json :object-type 'alist
                               :array-type 'list)))
                (should (stringp json))
                (should (eq (alist-get 'kept decoded) t))
                (should (= (alist-get 'candidate-count decoded) 1))
                (should (alist-get 'run decoded)))))
        (ignore-errors (delete-file file))))))


;;;; --- memory promotion ---------------------------------------------------

(ert-deftest anvil-autoresearch-test-docstring-promote-memory-adds-row ()
  "Promotion stores the selected docstring run through anvil-memory-add."
  (let* ((run-json
          "{\"server-id\":\"emacs-eval\",\"id\":\"weak-generic\",\"file\":\"anvil-test.el\",\"selected\":{\"label\":\"best\",\"description\":\"Use weak-generic when testing promotion. Read-only; returns a compact result.\",\"post-score\":0}}")
         captured)
    (cl-letf (((symbol-function 'anvil-memory-add)
               (lambda (name type body &rest args)
                 (setq captured (list :name name
                                      :type type
                                      :body body
                                      :args args))
                 (list :file (concat "anvil-memory:db:" name)
                       :name name
                       :type type
                       :description (plist-get args :description)
                       :digest "sha1"))))
      (let ((res (anvil-autoresearch-docstring-promote-memory
                  run-json
                  "autoresearch_docstring_test"
                  "memo"
                  "Accepted docstring"
                  "[\"doc43\",\"test\"]")))
        (should (eq (plist-get res :promoted) t))
        (should (equal (plist-get captured :name)
                       "autoresearch_docstring_test"))
        (should (eq (plist-get captured :type) 'memo))
        (should (string-match-p "weak-generic"
                                (plist-get captured :body)))
        (should (string-match-p "Accepted description"
                                (plist-get captured :body)))
        (should (equal (plist-get (plist-get captured :args)
                                  :description)
                       "Accepted docstring"))
        (should (equal (plist-get (plist-get captured :args) :tags)
                       '("doc43" "test")))))))

(ert-deftest anvil-autoresearch-test-docstring-promote-memory-harvest-json ()
  "Promotion accepts a harvest JSON object with a nested run."
  (let* ((harvest-json
          "{\"batch-id\":\"batch-1\",\"run\":{\"server-id\":\"emacs-eval\",\"id\":\"weak-generic\",\"selected\":{\"label\":\"best\",\"description\":\"Use nested harvest promotion. Read-only; returns a compact result.\",\"post-score\":0}}}")
         captured)
    (cl-letf (((symbol-function 'anvil-memory-add)
               (lambda (name type body &rest args)
                 (setq captured (list :name name
                                      :type type
                                      :body body
                                      :args args))
                 (list :file (concat "anvil-memory:db:" name)
                       :name name
                       :type type
                       :digest "sha1"))))
      (let ((res (anvil-autoresearch-docstring-promote-memory
                  harvest-json nil nil nil nil)))
        (should (eq (plist-get res :promoted) t))
        (should (string-prefix-p "autoresearch_docstring_weak-generic_"
                                 (plist-get captured :name)))
        (should (eq (plist-get captured :type) 'memo))
        (should (string-match-p "nested harvest promotion"
                                (plist-get captured :body)))
        (should (equal (plist-get (plist-get captured :args) :tags)
                       '("doc43" "autoresearch" "docstring")))))))

(ert-deftest anvil-autoresearch-test-docstring-promote-memory-wrapper-json-encodes ()
  "Promotion MCP wrapper emits JSON through encode-handler."
  (let ((run-json
         "{\"server-id\":\"emacs-eval\",\"id\":\"weak-generic\",\"selected\":{\"label\":\"best\",\"description\":\"Use wrapper promotion. Read-only; returns JSON.\",\"post-score\":0}}"))
    (cl-letf (((symbol-function 'anvil-memory-add)
               (lambda (name type _body &rest args)
                 (list :file (concat "anvil-memory:db:" name)
                       :name name
                       :type type
                       :description (plist-get args :description)
                       :digest "sha1"))))
      (let* ((handler (anvil-server-encode-handler
                       #'anvil-autoresearch--tool-docstring-promote-memory))
             (json (funcall handler
                            run-json "autoresearch_wrapper"
                            "memo" "Wrapper promotion"
                            "[\"doc43\"]"))
             (decoded (json-parse-string
                       json :object-type 'alist
                       :array-type 'list)))
        (should (stringp json))
        (should (eq (alist-get 'promoted decoded) t))
        (should (equal (alist-get 'tool-id decoded) "weak-generic"))
        (should (alist-get 'memory decoded))))))


;;;; --- routing autoresearch ----------------------------------------------

(ert-deftest anvil-autoresearch-test-routing-evaluate-scores-issues ()
  "Routing evaluator compares policies and scores weak states."
  (cl-letf (((symbol-function 'anvil-orchestrator-select-provider)
             (lambda (&rest args)
               (pcase (plist-get args :policy)
                 ('speed
                  (list :provider 'codex
                        :policy 'speed
                        :candidates '(codex claude)
                        :per-candidate
                        '((codex :total 4 :p50 100 :avg 120 :cost-usd 0.01)
                          (claude :total 4 :p50 200 :avg 220 :cost-usd 0.03))
                        :cold-start nil
                        :reason "speed"))
                 ('cost
                  (list :provider nil
                        :policy 'cost
                        :candidates '(codex claude)
                        :per-candidate nil
                        :cold-start t
                        :reason "cold"))
                 (_
                  (list :provider 'claude
                        :policy (plist-get args :policy)
                        :candidates '(codex claude)
                        :per-candidate
                        '((claude :total 4 :p50 200 :avg 220 :cost-usd 0.03))
                        :cold-start nil
                        :reason "other"))))))
      (let* ((res (anvil-autoresearch-routing-evaluate
                 "route this" "speed,cost,balanced"
                 "codex,claude" "300" "3"))
           (decisions (append (plist-get res :decisions) nil))
           (cost (cadr decisions)))
      (should (eq (plist-get res :policy-disagreement) t))
      (should (> (plist-get res :score) 0))
      (should (= (length decisions) 3))
      (should (eq (plist-get (car decisions) :provider) 'codex))
      (should (seq-contains-p (append (plist-get cost :issues) nil)
                              "no-provider"))
      (should (seq-contains-p (append (plist-get cost :issues) nil)
                              "cold-start"))
      (should (equal (plist-get res :suggested-action)
                     "plan-routing-policy-improvement")))))

(ert-deftest anvil-autoresearch-test-routing-plan-builds-prompt ()
  "Routing plan packages evaluation into a future proposal prompt."
  (cl-letf (((symbol-function 'anvil-orchestrator-select-provider)
             (lambda (&rest args)
               (list :provider 'codex
                     :policy (plist-get args :policy)
                     :candidates '(codex claude)
                     :per-candidate
                     '((codex :total 4 :p50 100 :avg 120 :cost-usd 0.01))
                     :cold-start nil
                     :reason "picked"))))
    (let ((plan (anvil-autoresearch-routing-plan
                 "route this" "balanced" "codex,claude" "300" "3")))
      (should (plist-get plan :evaluation))
      (should (string-match-p "orchestrator provider routing"
                              (plist-get plan :rewrite-prompt)))
      (should (string-match-p "policy=balanced"
                              (plist-get plan :rewrite-prompt)))
      (should (equal (plist-get plan :suggested-action)
                     "submit-routing-improvement-proposal")))))

(ert-deftest anvil-autoresearch-test-routing-plan-wrapper-json-encodes ()
  "Routing plan MCP wrapper emits JSON through encode-handler."
  (cl-letf (((symbol-function 'anvil-orchestrator-select-provider)
             (lambda (&rest args)
               (list :provider 'codex
                     :policy (plist-get args :policy)
                     :candidates '(codex claude)
                     :per-candidate
                     '((codex :total 4 :p50 100 :avg 120 :cost-usd 0.01))
                     :cold-start nil
                     :reason "picked"))))
    (let* ((handler (anvil-server-encode-handler
                     #'anvil-autoresearch--tool-routing-plan))
           (json (funcall handler
                          "route this" "balanced"
                          "codex,claude" "300" "3"))
           (decoded (json-parse-string
                     json :object-type 'alist
                     :array-type 'list)))
      (should (stringp json))
      (should (alist-get 'evaluation decoded))
      (should (alist-get 'rewrite-prompt decoded)))))

(ert-deftest anvil-autoresearch-test-routing-propose-submits-bounded-task ()
  "Routing proposal submit passes bounded task metadata to orchestrator."
  (let (submitted)
    (cl-letf (((symbol-function 'anvil-orchestrator-select-provider)
               (lambda (&rest args)
                 (list :provider 'codex
                       :policy (plist-get args :policy)
                       :candidates '(codex claude)
                       :per-candidate
                       '((codex :total 4 :p50 100 :avg 120 :cost-usd 0.01))
                       :cold-start nil
                       :reason "picked")))
              ((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                 (setq submitted tasks)
                 "routing-batch")))
      (let* ((res (anvil-autoresearch-routing-propose
                   "route this" "balanced" "codex,claude" "300" "3"
                   "codex" "gpt-test" "0.07" "90" "30"
                   default-directory "workspace-write"
                   "false" "ar-routing"))
             (task (car submitted)))
        (should (equal (plist-get res :batch-id) "routing-batch"))
        (should (equal (plist-get task :name)
                       "autoresearch-routing-improvement"))
        (should (eq (plist-get task :provider) 'codex))
        (should (equal (plist-get task :model) "gpt-test"))
        (should (= (plist-get task :budget-usd) 0.07))
        (should (= (plist-get task :timeout-sec) 90))
        (should (= (plist-get task :heartbeat-timeout-sec) 30))
        (should (equal (plist-get task :sandbox) "workspace-write"))
        (should (equal (plist-get task :worktree-name) "ar-routing"))
        (should-not (plist-get task :no-worktree))
        (should (string-match-p "orchestrator provider routing"
                                (plist-get task :prompt)))
        (should (string-match-p "policy=balanced"
                                (plist-get task :prompt)))
        (should (plist-get res :plan))
        (should (equal (plist-get res :suggested-action)
                       "poll-routing-proposal-batch"))))))

(ert-deftest anvil-autoresearch-test-routing-propose-wrapper-json-encodes ()
  "Routing proposal MCP wrapper emits JSON through encode-handler."
  (cl-letf (((symbol-function 'anvil-orchestrator-select-provider)
             (lambda (&rest args)
               (list :provider 'codex
                     :policy (plist-get args :policy)
                     :candidates '(codex claude)
                     :per-candidate
                     '((codex :total 4 :p50 100 :avg 120 :cost-usd 0.01))
                     :cold-start nil
                     :reason "picked")))
            ((symbol-function 'anvil-orchestrator-submit)
             (lambda (_tasks) "routing-json")))
    (let* ((handler (anvil-server-encode-handler
                     #'anvil-autoresearch--tool-routing-propose))
           (json (funcall handler
                          "route this" "balanced" "codex,claude" "300" "3"
                          "codex" "" "0.05" "80" "25"
                          default-directory "workspace-write"))
           (decoded (json-parse-string
                     json :object-type 'alist
                     :array-type 'list))
           (task (alist-get 'task decoded)))
      (should (stringp json))
      (should (equal (alist-get 'batch-id decoded) "routing-json"))
      (should (equal (alist-get 'provider task) "codex"))
      (should (= (alist-get 'budget-usd task) 0.05))
      (should (= (alist-get 'heartbeat-timeout-sec task) 25))
      (should (alist-get 'plan decoded)))))

(ert-deftest anvil-autoresearch-test-routing-harvest-selects-best-proposal ()
  "Routing harvest ranks successful proposal summaries for review."
  (cl-letf (((symbol-function 'anvil-orchestrator-collect)
             (lambda (batch-id &rest args)
               (should (equal batch-id "routing-batch"))
               (should (plist-get args :wait))
               (list
                (list :id "weak" :name "weak"
                      :status 'done :provider 'codex
                      :summary "Tweak routing.")
                (list :id "strong" :name "strong"
                      :status 'done :provider 'codex
                      :summary
                      "Change anvil-orchestrator routing policy defaults to account for provider cold-start stats, min-samples, candidate sets, and balanced/speed/cost policy disagreement. Add ERT tests and byte-compile checks.")
                (list :id "failed" :name "failed"
                      :status 'failed :provider 'codex
                      :summary "routing provider policy test")))))
    (let* ((res (anvil-autoresearch-routing-harvest
                 "routing-batch" "true" "5"))
           (candidates (append (plist-get res :candidates) nil))
           (selected (plist-get res :selected)))
      (should (= (plist-get res :candidate-count) 2))
      (should (= (length candidates) 2))
      (should (equal (plist-get selected :label) "strong"))
      (should (> (plist-get selected :score)
                 (plist-get (cadr candidates) :score)))
      (should (string-match-p "cold-start"
                              (plist-get selected :proposal)))
      (should (equal (plist-get res :suggested-action)
                     "review-selected-routing-proposal")))))

(ert-deftest anvil-autoresearch-test-routing-harvest-no-candidates ()
  "Routing harvest leaves selection empty when no successful summaries exist."
  (cl-letf (((symbol-function 'anvil-orchestrator-collect)
             (lambda (_batch-id &rest _args)
               (list
                (list :id "queued" :status 'queued)
                (list :id "failed" :status 'failed
                      :summary "routing provider policy")))))
    (let ((res (anvil-autoresearch-routing-harvest
                "routing-batch" nil "5")))
      (should (= (plist-get res :candidate-count) 0))
      (should-not (plist-get res :selected))
      (should-not (plist-get res :kept))
      (should (equal (plist-get res :suggested-action)
                     "no-routing-proposal-candidates")))))

(ert-deftest anvil-autoresearch-test-routing-harvest-wrapper-json-encodes ()
  "Routing harvest MCP wrapper emits JSON through encode-handler."
  (cl-letf (((symbol-function 'anvil-orchestrator-collect)
             (lambda (_batch-id &rest _args)
               (list
                (list :id "strong" :name "strong"
                      :status 'done :provider 'codex
                      :summary
                      "Improve provider routing policy defaults, candidate stats, min-samples, cold-start handling, and add ERT tests.")))))
    (let* ((handler (anvil-server-encode-handler
                     #'anvil-autoresearch--tool-routing-harvest))
           (json (funcall handler "routing-batch" "" "3"))
           (decoded (json-parse-string
                     json :object-type 'alist
                     :array-type 'list))
           (selected (alist-get 'selected decoded)))
      (should (stringp json))
      (should (= (alist-get 'candidate-count decoded) 1))
      (should (equal (alist-get 'label selected) "strong"))
      (should (string-match-p "provider routing"
                              (alist-get 'proposal selected))))))

(ert-deftest anvil-autoresearch-test-routing-patch-plan-from-json ()
  "Routing patch plan turns selected proposal JSON into dry-run artifacts."
  (let* ((proposal
          "Improve anvil-orchestrator routing cold-start policy by tuning min-samples and candidate stats, then add ERT tests.")
         (proposal-json
          (json-encode
           `((selected . ((label . "strong")
                          (proposal . ,proposal)
                          (score . 42)))))))
    (let ((res (anvil-autoresearch-routing-patch-plan proposal-json)))
      (should (eq (plist-get res :dry-run) t))
      (should (equal (plist-get res :proposal) proposal))
      (should (seq-contains-p
               (append (plist-get res :target-files) nil)
               "anvil-orchestrator-routing.el"))
      (should (string-match-p "Primary target: anvil-orchestrator-routing.el"
                              (plist-get res :implementation-prompt)))
      (should (string-match-p "cold-start policy"
                              (plist-get res :implementation-prompt)))
      (should (string-match-p "byte-compile"
                              (plist-get res :eval-command)))
      (should (string-match-p "ert-run-tests-batch-and-exit"
                              (plist-get res :holdout-command)))
      (should (equal (plist-get res :suggested-action)
                     "review-routing-patch-plan-then-implement")))))

(ert-deftest anvil-autoresearch-test-routing-patch-plan-from-batch ()
  "Routing patch plan can harvest a batch when proposal JSON is omitted."
  (cl-letf (((symbol-function 'anvil-orchestrator-collect)
             (lambda (batch-id &rest args)
               (should (equal batch-id "routing-batch"))
               (should (plist-get args :wait))
               (list
                (list :id "strong" :name "strong"
                      :status 'done :provider 'codex
                      :summary
                      "Improve routing provider policy defaults, candidate stats, min-samples, and cold-start behavior. Add ERT tests.")))))
    (let ((res (anvil-autoresearch-routing-patch-plan
                nil "routing-batch" "true" "3")))
      (should (eq (plist-get res :dry-run) t))
      (should (string-match-p "min-samples"
                              (plist-get res :proposal)))
      (should (plist-get res :selected))
      (should (plist-get res :source)))))

(ert-deftest anvil-autoresearch-test-routing-patch-plan-wrapper-json-encodes ()
  "Routing patch plan MCP wrapper emits JSON through encode-handler."
  (let* ((proposal "Improve provider routing stats interpretation and add tests.")
         (proposal-json
          (json-encode
           `((selected . ((label . "strong")
                          (proposal . ,proposal)
                          (score . 30)))))))
    (let* ((handler (anvil-server-encode-handler
                     #'anvil-autoresearch--tool-routing-patch-plan))
           (json (funcall handler proposal-json "" "" ""))
           (decoded (json-parse-string
                     json :object-type 'alist
                     :array-type 'list)))
      (should (stringp json))
      (should (eq (alist-get 'dry-run decoded) t))
      (should (equal (alist-get 'proposal decoded) proposal))
      (should (alist-get 'implementation-prompt decoded)))))

(ert-deftest anvil-autoresearch-test-routing-implement-submits-bounded-task ()
  "Routing implement submits patch-plan prompt as a bounded task."
  (let* ((proposal
          "Improve routing cold-start behavior by adjusting min-samples and candidate stats, then add ERT tests.")
         (proposal-json
          (json-encode
           `((selected . ((label . "strong")
                          (proposal . ,proposal)
                          (score . 42))))))
         submitted)
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                 (setq submitted tasks)
                 "impl-batch")))
      (let* ((res (anvil-autoresearch-routing-implement
                   proposal-json "" "" ""
                   "codex" "gpt-test" "0.07" "90" "30"
                   default-directory "workspace-write"
                   "false" "ar-routing-impl"))
             (task (car submitted)))
        (should (equal (plist-get res :batch-id) "impl-batch"))
        (should (equal (plist-get task :name)
                       "autoresearch-routing-implementation"))
        (should (eq (plist-get task :provider) 'codex))
        (should (equal (plist-get task :model) "gpt-test"))
        (should (= (plist-get task :budget-usd) 0.07))
        (should (= (plist-get task :timeout-sec) 90))
        (should (= (plist-get task :heartbeat-timeout-sec) 30))
        (should (equal (plist-get task :sandbox) "workspace-write"))
        (should (equal (plist-get task :worktree-name)
                       "ar-routing-impl"))
        (should-not (plist-get task :no-worktree))
        (should (string-match-p "Doc 43 autoresearch Phase 3c"
                                (plist-get task :prompt)))
        (should (string-match-p "cold-start behavior"
                                (plist-get task :prompt)))
        (should (plist-get res :plan))
        (should (equal (plist-get res :suggested-action)
                       "poll-routing-implementation-batch"))))))

(ert-deftest anvil-autoresearch-test-routing-implement-no-worktree-wins ()
  "Routing implement suppresses worktree-name when no-worktree is truthy."
  (let* ((proposal "Improve routing provider policy and add tests.")
         (proposal-json
          (json-encode
           `((selected . ((label . "strong")
                          (proposal . ,proposal)
                          (score . 30))))))
         submitted)
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                 (setq submitted tasks)
                 "impl-no-wt")))
      (anvil-autoresearch-routing-implement
       proposal-json "" "" ""
       "codex" "" "0.05" "80" "25"
       default-directory "workspace-write" "true" "ignored")
      (let ((task (car submitted)))
        (should (eq (plist-get task :no-worktree) t))
        (should-not (plist-get task :worktree-name))))))

(ert-deftest anvil-autoresearch-test-routing-implement-wrapper-json-encodes ()
  "Routing implement MCP wrapper emits JSON through encode-handler."
  (let* ((proposal "Improve provider routing stats interpretation and add tests.")
         (proposal-json
          (json-encode
           `((selected . ((label . "strong")
                          (proposal . ,proposal)
                          (score . 30)))))))
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (_tasks) "impl-json")))
      (let* ((handler (anvil-server-encode-handler
                       #'anvil-autoresearch--tool-routing-implement))
             (json (funcall handler
                            proposal-json "" "" ""
                            "codex" "" "0.05" "80" "25"
                            default-directory "workspace-write"))
             (decoded (json-parse-string
                       json :object-type 'alist
                       :array-type 'list))
             (task (alist-get 'task decoded)))
        (should (stringp json))
        (should (equal (alist-get 'batch-id decoded) "impl-json"))
        (should (equal (alist-get 'provider task) "codex"))
        (should (= (alist-get 'budget-usd task) 0.05))
        (should (= (alist-get 'heartbeat-timeout-sec task) 25))
        (should (alist-get 'plan decoded))))))

(ert-deftest anvil-autoresearch-test-routing-implementation-harvest-selects-best ()
  "Routing implementation harvest ranks successful summaries for review."
  (cl-letf (((symbol-function 'anvil-orchestrator-collect)
             (lambda (batch-id &rest args)
               (should (equal batch-id "impl-batch"))
               (should (plist-get args :wait))
               (list
                (list :id "weak" :name "weak"
                      :status 'done :provider 'codex
                      :summary "Made a routing tweak.")
                (list :id "strong" :name "strong"
                      :status 'done :provider 'codex
                      :summary
                      "Changed anvil-orchestrator-routing.el to improve provider cold-start min-samples and candidate stats handling. Added tests/anvil-orchestrator-test.el ERT coverage. byte-compile passed and ERT tests passed.")
                (list :id "failed" :name "failed"
                      :status 'failed :provider 'codex
                      :summary "changed routing tests")))))
    (let* ((res (anvil-autoresearch-routing-implementation-harvest
                 "impl-batch" "true" "5"))
           (candidates (append (plist-get res :candidates) nil))
           (selected (plist-get res :selected)))
      (should (= (plist-get res :candidate-count) 2))
      (should (= (length candidates) 2))
      (should (equal (plist-get selected :label) "strong"))
      (should (> (plist-get selected :score)
                 (plist-get (cadr candidates) :score)))
      (should (string-match-p "anvil-orchestrator-routing.el"
                              (plist-get selected :summary)))
      (should (seq-contains-p
               (append (plist-get res :review-checklist) nil)
               "run-orchestrator-ert-holdout"))
      (should (equal (plist-get res :suggested-action)
                     "review-selected-routing-implementation")))))

(ert-deftest anvil-autoresearch-test-routing-implementation-harvest-no-candidates ()
  "Routing implementation harvest leaves selection empty without done summaries."
  (cl-letf (((symbol-function 'anvil-orchestrator-collect)
             (lambda (_batch-id &rest _args)
               (list
                (list :id "queued" :status 'queued)
                (list :id "failed" :status 'failed
                      :summary "changed routing tests")))))
    (let ((res (anvil-autoresearch-routing-implementation-harvest
                "impl-batch" nil "5")))
      (should (= (plist-get res :candidate-count) 0))
      (should-not (plist-get res :selected))
      (should-not (plist-get res :kept))
      (should (equal (plist-get res :suggested-action)
                     "no-routing-implementation-candidates")))))

(ert-deftest anvil-autoresearch-test-routing-implementation-harvest-wrapper-json-encodes ()
  "Routing implementation harvest MCP wrapper emits JSON through encode-handler."
  (cl-letf (((symbol-function 'anvil-orchestrator-collect)
             (lambda (_batch-id &rest _args)
               (list
                (list :id "strong" :name "strong"
                      :status 'done :provider 'codex
                      :summary
                      "Changed anvil-orchestrator-routing.el and tests/anvil-orchestrator-test.el for provider routing stats. ERT tests passed.")))))
    (let* ((handler (anvil-server-encode-handler
                     #'anvil-autoresearch--tool-routing-implementation-harvest))
           (json (funcall handler "impl-batch" "" "3"))
           (decoded (json-parse-string
                     json :object-type 'alist
                     :array-type 'list))
           (selected (alist-get 'selected decoded)))
      (should (stringp json))
      (should (= (alist-get 'candidate-count decoded) 1))
      (should (equal (alist-get 'label selected) "strong"))
      (should (string-match-p "provider routing"
                              (alist-get 'summary selected))))))

(ert-deftest anvil-autoresearch-test-routing-implementation-promote-memory-adds-row ()
  "Routing implementation promotion stores selected summary in memory."
  (let* ((run-json
          "{\"batch-id\":\"impl-batch\",\"selected\":{\"label\":\"strong\",\"task-id\":\"tid-1\",\"provider\":\"codex\",\"score\":42,\"summary\":\"Changed anvil-orchestrator-routing.el and tests/anvil-orchestrator-test.el for cold-start routing. ERT tests passed.\"}}")
         captured)
    (cl-letf (((symbol-function 'anvil-memory-add)
               (lambda (name type body &rest args)
                 (setq captured (list :name name
                                      :type type
                                      :body body
                                      :args args))
                 (list :file (concat "anvil-memory:db:" name)
                       :name name
                       :type type
                       :description (plist-get args :description)
                       :digest "sha1"))))
      (let ((res (anvil-autoresearch-routing-implementation-promote-memory
                  run-json
                  "autoresearch_routing_impl_test"
                  "memo"
                  "Accepted routing implementation"
                  "[\"doc43\",\"routing\"]")))
        (should (eq (plist-get res :promoted) t))
        (should (equal (plist-get captured :name)
                       "autoresearch_routing_impl_test"))
        (should (eq (plist-get captured :type) 'memo))
        (should (string-match-p "impl-batch"
                                (plist-get captured :body)))
        (should (string-match-p "Accepted implementation summary"
                                (plist-get captured :body)))
        (should (string-match-p "cold-start routing"
                                (plist-get captured :body)))
        (should (equal (plist-get (plist-get captured :args)
                                  :description)
                       "Accepted routing implementation"))
        (should (equal (plist-get (plist-get captured :args) :tags)
                       '("doc43" "routing")))))))

(ert-deftest anvil-autoresearch-test-routing-implementation-promote-memory-selected-json ()
  "Routing implementation promotion accepts a selected candidate object."
  (let* ((selected-json
          "{\"label\":\"strong\",\"summary\":\"Changed provider routing stats and tests. byte-compile passed.\",\"score\":30}")
         captured)
    (cl-letf (((symbol-function 'anvil-memory-add)
               (lambda (name type body &rest args)
                 (setq captured (list :name name
                                      :type type
                                      :body body
                                      :args args))
                 (list :file (concat "anvil-memory:db:" name)
                       :name name
                       :type type
                       :digest "sha1"))))
      (let ((res (anvil-autoresearch-routing-implementation-promote-memory
                  selected-json nil nil nil nil)))
        (should (eq (plist-get res :promoted) t))
        (should (string-prefix-p "autoresearch_routing_impl_strong_"
                                 (plist-get captured :name)))
        (should (eq (plist-get captured :type) 'memo))
        (should (string-match-p "provider routing stats"
                                (plist-get captured :body)))
        (should (equal (plist-get (plist-get captured :args) :tags)
                       '("doc43" "autoresearch" "routing"
                         "implementation")))))))

(ert-deftest anvil-autoresearch-test-routing-implementation-promote-memory-wrapper-json-encodes ()
  "Routing implementation promotion MCP wrapper emits JSON."
  (let ((run-json
         "{\"batch-id\":\"impl-batch\",\"selected\":{\"label\":\"strong\",\"summary\":\"Changed provider routing stats and tests. ERT passed.\",\"score\":30}}"))
    (cl-letf (((symbol-function 'anvil-memory-add)
               (lambda (name type _body &rest args)
                 (list :file (concat "anvil-memory:db:" name)
                       :name name
                       :type type
                       :description (plist-get args :description)
                       :digest "sha1"))))
      (let* ((handler (anvil-server-encode-handler
                       #'anvil-autoresearch--tool-routing-implementation-promote-memory))
             (json (funcall handler
                            run-json "autoresearch_routing_wrapper"
                            "memo" "Wrapper routing promotion"
                            "[\"doc43\"]"))
             (decoded (json-parse-string
                       json :object-type 'alist
                       :array-type 'list)))
        (should (stringp json))
        (should (eq (alist-get 'promoted decoded) t))
        (should (equal (alist-get 'name decoded)
                       "autoresearch_routing_wrapper"))
        (should (alist-get 'memory decoded))))))


;;;; --- NeLisp codegen -----------------------------------------------------

(ert-deftest anvil-autoresearch-test-nelisp-codegen-plan-builds-prompt ()
  "NeLisp codegen plan resolves target files and verification commands."
  (anvil-autoresearch-test--with-nelisp-root
    (let ((plan (anvil-autoresearch-nelisp-codegen-plan
                 "try register allocation heuristic" root)))
      (should (equal (plist-get plan :target)
                     "nelisp-phase47-codegen"))
      (should (string= (file-name-as-directory root)
                       (plist-get plan :root)))
      (should (= (length (plist-get plan :target-files)) 4))
      (should (= (length (plist-get plan :missing-files)) 0))
      (should (string-match-p "nelisp-phase47-compiler"
                              (plist-get plan :eval-command)))
      (should (string-match-p "register allocation"
                              (plist-get plan :implementation-prompt))))))

(ert-deftest anvil-autoresearch-test-nelisp-codegen-implement-submits-task ()
  "NeLisp codegen implementation submits one bounded orchestrator task."
  (anvil-autoresearch-test--with-nelisp-root
    (let (submitted)
      (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                 (lambda (tasks)
                   (setq submitted tasks)
                   "nelisp-batch")))
        (let* ((res (anvil-autoresearch-nelisp-codegen-implement
                     "byte identity" root "codex" "gpt" "0.2" "180"
                     "90" "workspace-write" nil "nl-codegen"))
               (task (car submitted)))
          (should (equal (plist-get res :batch-id) "nelisp-batch"))
          (should (equal (plist-get task :name)
                         "autoresearch-nelisp-codegen-implementation"))
          (should (eq (plist-get task :provider) 'codex))
          (should (equal (plist-get task :cwd)
                         (file-name-as-directory root)))
          (should (equal (plist-get task :worktree-name) "nl-codegen"))
          (should (string-match-p "byte identity"
                                  (plist-get task :prompt))))))))

(ert-deftest anvil-autoresearch-test-nelisp-codegen-harvest-selects-best ()
  "NeLisp codegen harvest ranks implementation summaries."
  (cl-letf (((symbol-function 'anvil-orchestrator-collect)
             (lambda (_batch-id &rest _args)
               (list
                (list :id "weak" :status 'done :provider 'codex
                      :summary "changed something")
                (list :id "strong" :status 'done :provider 'codex
                      :summary
                      "Changed nelisp-phase47-compiler codegen pass ordering while preserving Phase 47 byte identity. Added ERT tests and byte-compile passed.")
                (list :id "failed" :status 'failed
                      :summary "phase 47 codegen")))))
    (let* ((res (anvil-autoresearch-nelisp-codegen-harvest
                 "nelisp-batch" nil 5))
           (selected (plist-get res :selected)))
      (should (= (plist-get res :candidate-count) 2))
      (should (equal (plist-get selected :task-id) "strong"))
      (should (string-match-p "byte identity"
                              (plist-get selected :summary))))))


;;;; --- dashboard ----------------------------------------------------------

(ert-deftest anvil-autoresearch-test-dashboard-entries-filter-autoresearch ()
  "Dashboard entries show only autoresearch orchestrator tasks."
  (let ((anvil-orchestrator--tasks (make-hash-table :test #'equal)))
    (puthash "task-2"
             (list :name "ordinary-task"
                   :status 'done
                   :provider 'codex)
             anvil-orchestrator--tasks)
    (puthash "task-1"
             (list :name "autoresearch-docstring-weak-generic-01-of-03"
                   :status 'running
                   :provider 'codex
                   :model "gpt"
                   :elapsed-ms 12
                   :cost-usd 0.0012
                   :batch-id "batchabcdef"
                   :summary "candidate summary")
             anvil-orchestrator--tasks)
    (puthash "task-3"
             (list :name "autoresearch-routing-implementation"
                   :status 'done
                   :provider 'claude
                   :batch-id "implbatch"
                   :summary "implementation summary")
             anvil-orchestrator--tasks)
    (let* ((entries (anvil-autoresearch--dashboard-entries))
           (ids (mapcar #'car entries))
           (row (cadr (assoc "task-1" entries))))
      (should (equal ids '("task-1" "task-3")))
      (should (equal (aref row 0) "docstring proposal"))
      (should (equal (aref row 1) "weak-generic"))
      (should (equal (aref row 2) "running"))
      (should (equal (aref row 7) "batchabc")))))

(ert-deftest anvil-autoresearch-test-dashboard-mode-refreshes ()
  "Dashboard mode initializes tabulated-list and refreshes entries."
  (let ((anvil-orchestrator--tasks (make-hash-table :test #'equal)))
    (puthash "task-1"
             (list :name "autoresearch-routing-improvement"
                   :status 'queued
                   :provider 'codex
                   :batch-id "batch123")
             anvil-orchestrator--tasks)
    (with-temp-buffer
      (anvil-autoresearch-dashboard-mode)
      (anvil-autoresearch-dashboard-refresh)
      (should (derived-mode-p 'anvil-autoresearch-dashboard-mode))
      (should (vectorp tabulated-list-format))
      (should (= (length tabulated-list-entries) 1))
      (should (equal (caar tabulated-list-entries) "task-1")))))

(provide 'anvil-autoresearch-test)
;;; anvil-autoresearch-test.el ends here
