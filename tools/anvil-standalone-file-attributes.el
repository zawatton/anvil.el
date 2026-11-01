;;; anvil-standalone-file-attributes.el --- GNU file attribute accessors -*- lexical-binding: t; -*-

;; Copyright (C) 1985-1987, 1992-2025 Free Software Foundation, Inc.
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; These definitions are extracted from GNU Emacs lisp/files.el.  Existing
;; runtime definitions remain authoritative and are never replaced.

(unless (fboundp 'file-attribute-modification-time)
(defsubst file-attribute-modification-time (attributes)
  "The modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of the last change to the file's contents, and
is a Lisp timestamp in the style of `current-time'."
  (nth 5 attributes))
)

(unless (fboundp 'file-attribute-status-change-time)
(defsubst file-attribute-status-change-time (attributes)
  "The status modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of last change to the file's attributes: owner
and group, access mode bits, etc., and is a Lisp timestamp in the
style of `current-time'."
  (nth 6 attributes))
)

(unless (fboundp 'file-attribute-inode-number)
(defsubst file-attribute-inode-number (attributes)
  "The inode number in ATTRIBUTES returned by `file-attributes'.
It is a nonnegative integer."
  (nth 10 attributes))
)

(unless (fboundp 'file-attribute-device-number)
(defsubst file-attribute-device-number (attributes)
  "The file system device number in ATTRIBUTES returned by `file-attributes'.
It is an integer or a cons cell of integers."
  (nth 11 attributes))
)

(provide 'anvil-standalone-file-attributes)
;;; anvil-standalone-file-attributes.el ends here
