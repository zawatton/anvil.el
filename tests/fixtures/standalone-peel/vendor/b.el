;;; b.el --- fixture  -*- lexical-binding: t; -*-
;; peel-b mentioned in a comment: (defun peel-b-fake () nil)
(defvar peel-v 40 "Fixture variable.")
(defun peel-b (x) (* x peel-v))
