;;; anvil-fusion-lens-test.el --- ERT for anvil-fusion role lenses -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure tests for lens application and panel-tasks lens wiring.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-fusion-lens)
(require 'anvil-fusion-panels)

(ert-deftest anvil-fusion-lens-test-nil-noop ()
  "A nil lens returns the prompt unchanged."
  (should (equal "Q" (anvil-fusion-apply-lens "Q" nil))))

(ert-deftest anvil-fusion-lens-test-string-prefix ()
  "A literal string lens is prepended with a blank line."
  (should (equal "ROLE\n\nQ" (anvil-fusion-apply-lens "Q" "ROLE"))))

(ert-deftest anvil-fusion-lens-test-symbol-lookup ()
  "A known lens symbol resolves to its registered prefix."
  (let ((out (anvil-fusion-apply-lens "Q" 'domain)))
    (should (string-suffix-p "\n\nQ" out))
    (should (string-match-p "電気管理技術者" out))))

(ert-deftest anvil-fusion-lens-test-unknown-symbol-errors ()
  "An unknown lens symbol signals rather than silently dropping."
  (should-error (anvil-fusion-apply-lens "Q" 'nope) :type 'user-error))

(ert-deftest anvil-fusion-lens-test-custom-registry ()
  "Lens lookup honors a rebound `anvil-fusion-lenses'."
  (let ((anvil-fusion-lenses '((x . "XP"))))
    (should (equal "XP\n\nQ" (anvil-fusion-apply-lens "Q" 'x)))))

;;;; --- panel-tasks lens wiring --------------------------------------------

(ert-deftest anvil-fusion-lens-test-panel-tasks-by-position ()
  "panel-tasks applies lenses to members by position."
  (let* ((anvil-fusion-lenses '((a . "AP") (b . "BP")))
         (body (anvil-fusion-panel-get 'sovereign))
         (tasks (anvil-fusion-panel-tasks body "Q" '(a b))))
    (should (string-prefix-p "AP\n\nQ" (plist-get (nth 0 tasks) :prompt)))
    (should (string-prefix-p "BP\n\nQ" (plist-get (nth 1 tasks) :prompt)))
    ;; third member has no lens -> unchanged
    (should (equal "Q" (plist-get (nth 2 tasks) :prompt)))))

(ert-deftest anvil-fusion-lens-test-panel-tasks-no-lenses ()
  "panel-tasks without lenses is the Phase 2 behavior (unchanged)."
  (let* ((body (anvil-fusion-panel-get 'sovereign))
         (tasks (anvil-fusion-panel-tasks body "Q")))
    (should (cl-every (lambda (tk) (equal "Q" (plist-get tk :prompt))) tasks))))

(provide 'anvil-fusion-lens-test)
;;; anvil-fusion-lens-test.el ends here
