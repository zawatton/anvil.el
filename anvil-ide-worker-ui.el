;;; anvil-ide-worker-ui.el --- tabulated-list view for anvil-worker metrics (IDE layer) -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 01 Phase 5 — status UI for the worker pool.
;;
;; `M-x anvil-worker-metrics' opens a read-only tabulated-list buffer
;; with per-lane classify / latency stats.  Keys:
;;
;;   g   refresh
;;   r   reset all metrics (classify + latency)
;;   q   quit window
;;
;; All data is pulled from the plists that anvil-worker.el already
;; maintains; this module adds no new state.

;;; Code:

(require 'anvil-worker)
(require 'tabulated-list)

(defgroup anvil-worker-ui nil
  "Status UI for `anvil-worker' metrics."
  :group 'anvil-worker
  :prefix "anvil-worker-ui-")

(defcustom anvil-worker-ui-buffer-name "*Anvil Worker Metrics*"
  "Name of the tabulated-list buffer used by `anvil-worker-metrics'."
  :type 'string
  :group 'anvil-worker-ui)

(defun anvil-worker-ui--lane-row (lane)
  "Build a tabulated-list entry for LANE.
Returns a list (ID VECTOR) or nil when LANE has no samples *and* no
classifier hits (to keep empty rows visible we still emit them)."
  (let* ((lane-name (anvil-worker--lane-name lane))
         (pool-size (anvil-worker--lane-size lane))
         (classify-hits
          (or (plist-get anvil-worker--metrics-classify lane) 0))
         (bucket
          (and anvil-worker--metrics-latency
               (plist-get anvil-worker--metrics-latency lane)))
         (n (or (and bucket (plist-get bucket :samples)) 0))
         (mean (if (zerop n)
                   "-"
                 (format "%.1f" (/ (plist-get bucket :total-ms-sum) n))))
         (spawn (if (zerop n)
                    "-"
                  (format "%.1f" (/ (plist-get bucket :spawn-ms-sum) n))))
         (wait (if (zerop n)
                   "-"
                 (format "%.1f" (/ (plist-get bucket :wait-ms-sum) n))))
         (p50 (if (zerop n)
                  "-"
                (format "%d"
                        (or (anvil-worker--percentile
                             (plist-get bucket :totals) 50)
                            0))))
         (p99 (if (zerop n)
                  "-"
                (format "%d"
                        (or (anvil-worker--percentile
                             (plist-get bucket :totals) 99)
                            0)))))
    (list lane
          (vector lane-name
                  (number-to-string pool-size)
                  (number-to-string n)
                  (number-to-string classify-hits)
                  mean spawn wait p50 p99))))

(defun anvil-worker-ui--entries ()
  "Return tabulated-list entries for all lanes."
  (mapcar #'anvil-worker-ui--lane-row anvil-worker--lanes))

(defun anvil-worker-ui--footer ()
  "Return a one-line summary string with classifier fallback count."
  (let* ((fallback
          (or (plist-get anvil-worker--metrics-classify :unknown-fallback)
              0))
         (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (format "  classifier unknown-fallback: %d    updated: %s"
            fallback timestamp)))

(defun anvil-worker-ui--refresh-header ()
  "Write the footer line into the buffer header."
  (setq header-line-format (anvil-worker-ui--footer)))

(defvar anvil-worker-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "r") #'anvil-worker-ui-reset)
    map)
  "Keymap for `anvil-worker-ui-mode'.")

(define-derived-mode anvil-worker-ui-mode tabulated-list-mode
  "Anvil Worker Metrics"
  "Major mode for the `anvil-worker' status UI."
  (setq tabulated-list-format
        [("Lane"     8  t)
         ("Pool"     5  t :right-align t)
         ("N"        6  t :right-align t)
         ("Classify" 9  t :right-align t)
         ("Mean ms"  9  t :right-align t)
         ("Spawn ms" 10 t :right-align t)
         ("Wait ms"  9  t :right-align t)
         ("p50 ms"   8  t :right-align t)
         ("p99 ms"   8  t :right-align t)])
  (setq tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook
            #'anvil-worker-ui--refresh-header nil t)
  (tabulated-list-init-header))

(defun anvil-worker-ui-reset ()
  "Zero out both classifier and latency metrics, then refresh."
  (interactive)
  (anvil-worker-classify-metrics-reset)
  (anvil-worker-latency-metrics-reset)
  (when (derived-mode-p 'anvil-worker-ui-mode)
    (revert-buffer)))

;;;###autoload
(defun anvil-worker-metrics ()
  "Show a tabulated-list view of anvil-worker classify/latency metrics."
  (interactive)
  (let ((buf (get-buffer-create anvil-worker-ui-buffer-name)))
    (with-current-buffer buf
      (anvil-worker-ui-mode)
      (setq tabulated-list-entries #'anvil-worker-ui--entries)
      (tabulated-list-print t)
      (anvil-worker-ui--refresh-header))
    (pop-to-buffer buf)))

(provide 'anvil-ide-worker-ui)
;;; anvil-ide-worker-ui.el ends here
