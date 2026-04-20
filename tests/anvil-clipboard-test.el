;;; anvil-clipboard-test.el --- Tests for anvil-clipboard -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for `anvil-clipboard' tool selection logic.  The helpers
;; `anvil-clipboard--linux-get-command' / `-set-command' are pure
;; string builders whose behaviour depends on (a) whether
;; `WAYLAND_DISPLAY' is set and (b) which of wl-paste / wl-copy /
;; xclip / xsel are on PATH.  We cl-letf-stub both so the tests are
;; deterministic regardless of the CI runner's session type.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-clipboard)

(defmacro anvil-clipboard-test--with-env (wayland tools &rest body)
  "Run BODY with WAYLAND_DISPLAY and `executable-find' stubbed.
WAYLAND is nil for an X11 session or a non-empty string for a
Wayland session.  TOOLS is a list of executable names that should
appear present (others resolve to nil)."
  (declare (indent 2))
  `(cl-letf* ((wd ,wayland)
              (tools ,tools)
              ((symbol-function 'getenv)
               (lambda (name)
                 (if (string= name "WAYLAND_DISPLAY") wd nil)))
              ((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (when (member name tools) (concat "/usr/bin/" name)))))
     ,@body))

;;;; --- detection ----------------------------------------------------------

(ert-deftest anvil-clipboard-test-wayland-session-detected ()
  (anvil-clipboard-test--with-env "wayland-0" '()
    (should (anvil-clipboard--wayland-session-p))))

(ert-deftest anvil-clipboard-test-x11-session-when-wd-unset ()
  (anvil-clipboard-test--with-env nil '()
    (should-not (anvil-clipboard--wayland-session-p))))

(ert-deftest anvil-clipboard-test-x11-session-when-wd-empty ()
  (anvil-clipboard-test--with-env "" '()
    (should-not (anvil-clipboard--wayland-session-p))))

;;;; --- get-command selection ---------------------------------------------

(ert-deftest anvil-clipboard-test-get-prefers-wl-paste-on-wayland ()
  (anvil-clipboard-test--with-env "wayland-0" '("wl-paste" "xclip" "xsel")
    (should (equal (anvil-clipboard--linux-get-command)
                   "wl-paste --no-newline"))))

(ert-deftest anvil-clipboard-test-get-falls-back-to-xclip-on-wayland-without-wl ()
  (anvil-clipboard-test--with-env "wayland-0" '("xclip" "xsel")
    (should (equal (anvil-clipboard--linux-get-command)
                   "xclip -selection clipboard -o"))))

(ert-deftest anvil-clipboard-test-get-prefers-xclip-on-x11 ()
  (anvil-clipboard-test--with-env nil '("wl-paste" "xclip" "xsel")
    (should (equal (anvil-clipboard--linux-get-command)
                   "xclip -selection clipboard -o"))))

(ert-deftest anvil-clipboard-test-get-falls-back-to-xsel ()
  (anvil-clipboard-test--with-env nil '("xsel")
    (should (equal (anvil-clipboard--linux-get-command)
                   "xsel --clipboard --output"))))

(ert-deftest anvil-clipboard-test-get-nil-when-nothing-available ()
  (anvil-clipboard-test--with-env nil '()
    (should (null (anvil-clipboard--linux-get-command))))
  (anvil-clipboard-test--with-env "wayland-0" '()
    (should (null (anvil-clipboard--linux-get-command)))))

;;;; --- set-command selection ---------------------------------------------

(ert-deftest anvil-clipboard-test-set-prefers-wl-copy-on-wayland ()
  (anvil-clipboard-test--with-env "wayland-0" '("wl-copy" "xclip" "xsel")
    (let ((cmd (anvil-clipboard--linux-set-command "hello")))
      (should (string-match-p "| wl-copy\\'" cmd))
      (should (string-match-p "printf '%s' hello" cmd)))))

(ert-deftest anvil-clipboard-test-set-falls-back-to-xclip-on-wayland-without-wl ()
  (anvil-clipboard-test--with-env "wayland-0" '("xclip" "xsel")
    (let ((cmd (anvil-clipboard--linux-set-command "hi")))
      (should (string-match-p "| xclip -selection clipboard\\'" cmd)))))

(ert-deftest anvil-clipboard-test-set-prefers-xclip-on-x11 ()
  (anvil-clipboard-test--with-env nil '("wl-copy" "xclip" "xsel")
    (let ((cmd (anvil-clipboard--linux-set-command "hi")))
      (should (string-match-p "| xclip -selection clipboard\\'" cmd)))))

(ert-deftest anvil-clipboard-test-set-shell-quotes-string ()
  "Payloads with shell metacharacters must be quoted."
  (anvil-clipboard-test--with-env "wayland-0" '("wl-copy")
    (let ((cmd (anvil-clipboard--linux-set-command "a'b;rm -rf /")))
      ;; shell-quote-argument guarantees the dangerous fragment cannot
      ;; be interpreted as a shell command.
      (should-not (string-match-p ";rm -rf /[^']" cmd)))))

(ert-deftest anvil-clipboard-test-set-nil-when-nothing-available ()
  (anvil-clipboard-test--with-env nil '()
    (should (null (anvil-clipboard--linux-set-command "x"))))
  (anvil-clipboard-test--with-env "wayland-0" '()
    (should (null (anvil-clipboard--linux-set-command "x")))))

(provide 'anvil-clipboard-test)
;;; anvil-clipboard-test.el ends here
