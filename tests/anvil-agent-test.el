;;; anvil-agent-test.el --- Tests for anvil-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT for `anvil-agent-spec-loader' — compact agent dispatch prompts.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-agent)


(defmacro anvil-agent-test--with-spec-file (var content &rest body)
  "Bind VAR to a temp spec file containing CONTENT for the dynamic extent of BODY."
  (declare (indent 2))
  `(let ((,var (make-temp-file "anvil-agent-spec-" nil ".md")))
     (unwind-protect
         (progn
           (with-temp-file ,var (insert ,content))
           ,@body)
       (ignore-errors (delete-file ,var)))))


(ert-deftest anvil-agent-test/spec-loader-returns-string-with-read-instruction ()
  "Loader returns a string telling the agent to Read the spec first."
  (anvil-agent-test--with-spec-file spec "# Test spec\nfoo bar\n"
    (let ((prompt (anvil-agent-spec-loader spec :goal "do the thing")))
      (should (stringp prompt))
      (should (string-match-p "First action: Read the full spec at " prompt))
      (should (string-match-p (regexp-quote spec) prompt))
      (should (string-match-p "Goal: do the thing" prompt)))))

(ert-deftest anvil-agent-test/spec-loader-renders-acceptance-bullets ()
  "Acceptance items render as `- ITEM' bullets."
  (anvil-agent-test--with-spec-file spec "x"
    (let ((prompt (anvil-agent-spec-loader
                   spec
                   :acceptance '("make test PASS"
                                 "value-correct gate 3/3"))))
      (should (string-match-p "- make test PASS" prompt))
      (should (string-match-p "- value-correct gate 3/3" prompt)))))

(ert-deftest anvil-agent-test/spec-loader-default-ship-target ()
  "When :ship-to omitted, default ship target line appears."
  (anvil-agent-test--with-spec-file spec "x"
    (let ((prompt (anvil-agent-spec-loader spec :goal "g")))
      (should (string-match-p (regexp-quote anvil-agent-default-ship-to)
                              prompt)))))

(ert-deftest anvil-agent-test/spec-loader-custom-ship-target-overrides ()
  "Caller-provided :ship-to overrides the default."
  (anvil-agent-test--with-spec-file spec "x"
    (let ((prompt (anvil-agent-spec-loader spec :ship-to "main hotfix push")))
      (should (string-match-p "Ship target: main hotfix push" prompt)))))

(ert-deftest anvil-agent-test/spec-loader-includes-coauthor-trailer ()
  "Standard contract section embeds the Co-Authored-By trailer."
  (anvil-agent-test--with-spec-file spec "x"
    (let ((prompt (anvil-agent-spec-loader spec)))
      (should (string-match-p "Co-Authored-By: Claude" prompt)))))

(ert-deftest anvil-agent-test/spec-loader-custom-coauthor-overrides ()
  "Caller-provided :coauthor replaces the default trailer."
  (anvil-agent-test--with-spec-file spec "x"
    (let ((prompt (anvil-agent-spec-loader spec :coauthor "Co-Authored-By: tester")))
      (should (string-match-p "Co-Authored-By: tester" prompt))
      (should-not (string-match-p
                   (regexp-quote anvil-agent-default-coauthor)
                   prompt)))))

(ert-deftest anvil-agent-test/spec-loader-raises-on-missing-spec ()
  "Unreadable spec path signals `file-missing'."
  (let ((bogus (concat (make-temp-name "/tmp/anvil-agent-no-such-") ".md")))
    (should-error (anvil-agent-spec-loader bogus :goal "x")
                  :type 'file-missing)))

(ert-deftest anvil-agent-test/spec-loader-resolves-relative-to-root ()
  "Relative SPEC-PATH resolves under `anvil-agent-spec-root' when set."
  (let* ((root (make-temp-file "anvil-agent-root-" t))
         (relpath "T999.md")
         (full (expand-file-name relpath root)))
    (unwind-protect
        (progn
          (with-temp-file full (insert "# T999\n"))
          (let ((anvil-agent-spec-root root))
            (let ((prompt (anvil-agent-spec-loader relpath :goal "x")))
              (should (string-match-p (regexp-quote full) prompt)))))
      (ignore-errors (delete-file full))
      (ignore-errors (delete-directory root t)))))

(ert-deftest anvil-agent-test/spec-loader-output-significantly-smaller-than-inline ()
  "Generated prompt is far smaller than embedding the spec body inline."
  (anvil-agent-test--with-spec-file spec (make-string 3000 ?x)
    (let ((prompt (anvil-agent-spec-loader
                   spec
                   :goal "implement T999 backend gap fix"
                   :acceptance '("make test PASS"
                                 "value-correct 3/3"
                                 "no regression on x86_64")
                   :preamble "Use worktree at /tmp/T999-wt for isolation.")))
      (should (< (length prompt) 1000))
      (should (< (length prompt) (/ 3000 2))))))


;;;; --- MCP handler --------------------------------------------------------

(ert-deftest anvil-agent-test/tool-handler-returns-shape-locked-plist ()
  "MCP handler returns the documented plist keys."
  (anvil-agent-test--with-spec-file spec "# spec\nbody\n"
    (let* ((result (anvil-agent--tool-spec-loader
                    spec "implement T999" "develop direct" "x,y" nil nil)))
      (should (plist-member result :prompt))
      (should (plist-member result :spec-path))
      (should (plist-member result :spec-bytes))
      (should (plist-member result :prompt-bytes))
      (should (plist-member result :coauthor))
      (should (stringp (plist-get result :prompt)))
      (should (string-match-p "- x" (plist-get result :prompt)))
      (should (string-match-p "- y" (plist-get result :prompt)))
      (should (> (plist-get result :prompt-bytes) 100)))))

(ert-deftest anvil-agent-test/tool-handler-empty-acceptance-omits-section ()
  "Empty :acceptance yields no checklist section."
  (anvil-agent-test--with-spec-file spec "x"
    (let* ((result (anvil-agent--tool-spec-loader spec "g" nil nil nil nil)))
      (should-not (string-match-p "Acceptance:" (plist-get result :prompt))))))


(provide 'anvil-agent-test)

;;; anvil-agent-test.el ends here
