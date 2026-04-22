;;; g1-accuracy.el --- Doc 31 Phase 0 G1: treesit-elisp accuracy  -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -Q -l /tmp/anvil-doc31-phase0/g1-accuracy.el
;;
;; Measures on 4 anvil source files (~10k LOC total):
;;   - parse success (no ERROR node)
;;   - byte-exact round-trip via (buffer-string) = source
;;   - comment nodes preserved (count + sample round-trip)

(require 'treesit)

(defconst g1--files
  '("/home/madblack-21/.emacs.d/external-packages/anvil.el/anvil.el"
    "/home/madblack-21/.emacs.d/external-packages/anvil.el/anvil-server.el"
    "/home/madblack-21/.emacs.d/external-packages/anvil.el/anvil-file.el"
    "/home/madblack-21/.emacs.d/external-packages/anvil.el/anvil-org.el"))

(defun g1--count-error-nodes (root)
  "Walk subtree ROOT counting nodes whose type is ERROR."
  (let ((count 0))
    (cl-labels ((walk (n)
                  (when (string= "ERROR" (treesit-node-type n))
                    (cl-incf count))
                  (dotimes (i (treesit-node-child-count n))
                    (walk (treesit-node-child n i)))))
      (walk root))
    count))

(defun g1--count-comment-nodes (root)
  "Walk subtree ROOT counting comment nodes.
The Wilfred grammar uses `comment' as the node type."
  (let ((count 0))
    (cl-labels ((walk (n)
                  (when (member (treesit-node-type n) '("comment"))
                    (cl-incf count))
                  (dotimes (i (treesit-node-child-count n))
                    (walk (treesit-node-child n i)))))
      (walk root))
    count))

(defun g1--collect-comment-texts (root)
  "Return a list of comment node text, in document order."
  (let (texts)
    (cl-labels ((walk (n)
                  (when (string= "comment" (treesit-node-type n))
                    (push (treesit-node-text n t) texts))
                  (dotimes (i (treesit-node-child-count n))
                    (walk (treesit-node-child n i)))))
      (walk root))
    (nreverse texts)))

(defun g1--measure (path)
  "Return a plist of measurements for PATH."
  (let ((t0 (current-time)))
    (with-temp-buffer
      (insert-file-contents path)
      (let* ((source (buffer-string))
             (_ (treesit-parser-create 'elisp))
             (root (treesit-buffer-root-node 'elisp))
             (parse-ms (* 1000 (float-time (time-subtract (current-time) t0))))
             (root-text (treesit-node-text root t))
             (roundtrip-ok (string= source root-text))
             (source-bytes (string-bytes source))
             (root-bytes (string-bytes root-text))
             (error-count (g1--count-error-nodes root))
             (comment-count (g1--count-comment-nodes root))
             (comment-texts (g1--collect-comment-texts root))
             ;; Regex reference for comments (starts with ';' up to EOL)
             (re-count (save-excursion
                         (goto-char (point-min))
                         (let ((n 0))
                           (while (re-search-forward "^[ \t]*;+\\|;+" nil t)
                             (cl-incf n)
                             ;; skip rest of this line to avoid double-count
                             (end-of-line))
                           n))))
        (list :path path
              :bytes source-bytes
              :lines (count-lines (point-min) (point-max))
              :parse-ms (round parse-ms)
              :error-nodes error-count
              :roundtrip-byte-exact roundtrip-ok
              :roundtrip-diff (- root-bytes source-bytes)
              :comment-nodes comment-count
              :comment-regex-count re-count
              :comment-sample (when comment-texts
                                (car comment-texts)))))))

(defun g1--format-result (r)
  (format (concat
           "  path:             %s\n"
           "  bytes / lines:    %d / %d\n"
           "  parse time:       %d ms\n"
           "  ERROR nodes:      %d\n"
           "  byte-exact:       %s (diff=%d)\n"
           "  comment nodes:    %d  (regex count %d)\n"
           "  comment sample:   %s\n")
          (file-name-nondirectory (plist-get r :path))
          (plist-get r :bytes)
          (plist-get r :lines)
          (plist-get r :parse-ms)
          (plist-get r :error-nodes)
          (if (plist-get r :roundtrip-byte-exact) "YES" "NO")
          (plist-get r :roundtrip-diff)
          (plist-get r :comment-nodes)
          (plist-get r :comment-regex-count)
          (truncate-string-to-width
           (or (plist-get r :comment-sample) "(none)") 80 nil nil "...")))

(let ((total-files 0)
      (pass-parse 0)
      (pass-roundtrip 0)
      (total-comment-nodes 0)
      (total-regex-comments 0)
      (total-bytes 0)
      (total-lines 0))
  (princ "\n=== Doc 31 Phase 0 G1 — treesit-elisp accuracy ===\n\n")
  (dolist (f g1--files)
    (cl-incf total-files)
    (let ((r (g1--measure f)))
      (princ (g1--format-result r))
      (princ "\n")
      (when (zerop (plist-get r :error-nodes))
        (cl-incf pass-parse))
      (when (plist-get r :roundtrip-byte-exact)
        (cl-incf pass-roundtrip))
      (cl-incf total-comment-nodes (plist-get r :comment-nodes))
      (cl-incf total-regex-comments (plist-get r :comment-regex-count))
      (cl-incf total-bytes (plist-get r :bytes))
      (cl-incf total-lines (plist-get r :lines))))
  (princ (format "=== summary ===\n"))
  (princ (format "  files:                       %d\n" total-files))
  (princ (format "  total bytes / lines:         %d / %d\n" total-bytes total-lines))
  (princ (format "  parse success (ERROR==0):    %d / %d\n" pass-parse total-files))
  (princ (format "  byte-exact round-trip:       %d / %d\n" pass-roundtrip total-files))
  (princ (format "  comment nodes (treesit):     %d\n" total-comment-nodes))
  (princ (format "  comment lines (regex est.):  %d\n" total-regex-comments))
  (princ (format "\n  G1 accept:\n"))
  (princ (format "    parse 100%%:              %s\n"
                 (if (= pass-parse total-files) "YES" "NO")))
  (princ (format "    round-trip ≥99.9%%:       %s (%d/%d files byte-exact)\n"
                 (if (= pass-roundtrip total-files) "YES" "NO")
                 pass-roundtrip total-files))
  (princ "\n"))
