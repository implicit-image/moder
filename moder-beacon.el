;;; moder-beacons.el --- Batch Macro state in Moder  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; The file contains BEACON state implementation.

;;; Code:

(require 'moder-util)
(require 'moder-var)
(require 'kmacro)
(require 'seq)

(declare-function moder-replace "moder-command")
(declare-function moder-insert "moder-command")
(declare-function moder-change "moder-command")
(declare-function moder-change-char "moder-command")
(declare-function moder-append "moder-command")
(declare-function moder-kill "moder-command")
(declare-function moder--cancel-selection "moder-command")
(declare-function moder--selection-fallback "moder-command")
(declare-function moder--make-selection "moder-command")
(declare-function moder--select "moder-command")
(declare-function moder-beacon-mode "moder-core")
(declare-function moder-change-save "moder-command")
(declare-function moder-escape-or-normal-modal "moder-command")

(defvar-local moder--beacon-overlays nil)
(defvar-local moder--beacon-insert-enter-key nil)

(defun moder--beacon-add-overlay-at-point (pos)
  "Create an overlay to draw a fake cursor as beacon at POS."
  (let ((ov (make-overlay pos (1+ pos) nil t)))
    (overlay-put ov 'face 'moder-beacon-fake-cursor)
    (overlay-put ov 'moder-beacon-type 'cursor)
    (push ov moder--beacon-overlays)))

(defun moder--beacon-add-overlay-at-region (type p1 p2 backward)
  "Create an overlay to draw a fake selection as beacon from P1 to 12.

TYPE is used for selection type.
Non-nil BACKWARD means backward direction."
  (let ((ov (make-overlay p1 p2)))
    (overlay-put ov 'face 'moder-beacon-fake-selection)
    (overlay-put ov 'moder-beacon-type type)
    (overlay-put ov 'moder-beacon-backward backward)
    (push ov moder--beacon-overlays)))

(defun moder--beacon-remove-overlays ()
  "Remove all beacon overlays from current buffer."
  (mapc #'delete-overlay moder--beacon-overlays)
  (setq moder--beacon-overlays nil))

(defun moder--maybe-toggle-beacon-state ()
  "Maybe switch to BEACON state."
  (unless (or defining-kbd-macro executing-kbd-macro)
    (let ((inside (moder--beacon-inside-secondary-selection)))
      (cond
       ((and (moder-normal-mode-p)
             inside)
        (moder--switch-state 'beacon)
        (moder--beacon-update-overlays))
       ((moder-beacon-mode-p)
        (if inside
            (moder--beacon-update-overlays)
          (moder--beacon-remove-overlays)
          (moder--switch-state 'normal)))))))

(defun moder--beacon-shrink-selection ()
  "Shrink selection to one char width."
  (if moder-use-cursor-position-hack
      (let ((m (if (moder--direction-forward-p)
                   (1- (point))
                 (1+ (point)))))
        (moder--cancel-selection)
        (thread-first
          (moder--make-selection '(select . transient) m (point))
          (moder--select t)))
    (moder--cancel-selection)))

(defun moder--beacon-apply-command (cmd)
  "Apply CMD in BEACON state."
  (when moder--beacon-overlays
    (let ((bak (overlay-get (car moder--beacon-overlays)
                            'moder-beacon-backward)))
      (moder--wrap-collapse-undo
       (save-mark-and-excursion
         (cl-loop for ov in (if bak (reverse moder--beacon-overlays) moder--beacon-overlays) do
                  (when (and (overlayp ov))
                    (let ((type (overlay-get ov 'moder-beacon-type))
                          (backward (overlay-get ov 'moder-beacon-backward)))
                      ;; always switch to normal state before applying kmacro
                      (moder--switch-state 'normal)

                      (if (eq type 'cursor)
                          (progn
                            (moder--cancel-selection)
                            (goto-char (overlay-start ov)))
                        (thread-first
                          (if backward
                              (moder--make-selection
                               type (overlay-end ov) (overlay-start ov))
                            (moder--make-selection type (overlay-start ov) (overlay-end ov)))
                          (moder--select t)))

                      (call-interactively cmd))
                    (delete-overlay ov))))))))

(defun moder--beacon-apply-kmacros-from-insert ()
  "Apply kmacros in BEACON state, after exiting from insert.

This is treated separately because we must enter each insert state the
same way, and escape each time the macro is applied."
  (moder--beacon-apply-command (lambda ()
                                 (interactive)
                                 (moder--execute-kbd-macro
                                  (key-description
                                   (vector moder--beacon-insert-enter-key)))
                                 (call-interactively #'kmacro-call-macro)
                                 (moder-escape-or-normal-modal))))

(defun moder--beacon-apply-kmacros ()
  "Apply kmacros in BEACON state."
  (moder--beacon-apply-command 'kmacro-call-macro))

(defun moder--add-beacons-for-char ()
  "Add beacon for char movement."
  (save-restriction
    (let* ((bounds (moder--second-sel-bound))
           (beg (car bounds))
           (end (cdr bounds))
           (curr (point))
           (col (- (point) (line-beginning-position)))
           break)
      (save-mark-and-excursion
        (while (< (line-end-position) end)
          (forward-line 1)
          (let ((pos (+ col (line-beginning-position))))
            (when (<= pos (min end (line-end-position)))
              (moder--beacon-add-overlay-at-point pos)))))
      (save-mark-and-excursion
        (goto-char beg)
        (while (not break)
          (if (>= (line-end-position) curr)
              (setq break t)
            (let ((pos (+ col (line-beginning-position))))
              (when (and
                     (>= pos beg)
                     (<= pos (line-end-position)))
                (moder--beacon-add-overlay-at-point pos)))
            (forward-line 1))))))
  (setq moder--beacon-overlays (reverse moder--beacon-overlays))
  (moder--cancel-selection))

(defun moder--add-beacons-for-char-expand ()
  "Add beacon for char expand movement."
  (save-restriction
    (let* ((bounds (moder--second-sel-bound))
           (ss-beg (car bounds))
           (ss-end (cdr bounds))
           (curr (point))
           (bak (moder--direction-backward-p))
           (beg-col (- (region-beginning) (line-beginning-position)))
           (end-col (- (region-end) (line-beginning-position)))
           break)
      (save-mark-and-excursion
        (while (< (line-end-position) ss-end)
          (forward-line 1)
          (let ((beg (+ beg-col (line-beginning-position)))
                (end (+ end-col (line-beginning-position))))
            (when (<= end (min ss-end (line-end-position)))
              (moder--beacon-add-overlay-at-region
               '(expand . char)
               beg
               end
               bak)))))
      (save-mark-and-excursion
        (goto-char ss-beg)
        (while (not break)
          (if (>= (line-end-position) curr)
              (setq break t)
            (let ((beg (+ beg-col (line-beginning-position)))
                  (end (+ end-col (line-beginning-position))))
              (when (and
                     (>= beg ss-beg)
                     (<= end (line-end-position)))
                (moder--beacon-add-overlay-at-region
                 '(expand . char)
                 beg
                 end
                 bak)))
            (forward-line 1)))))
    (setq moder--beacon-overlays (reverse moder--beacon-overlays))))

(defun moder--add-beacons-for-thing (thing)
  "Add beacon for word movement."
  (save-restriction
    (moder--narrow-secondary-selection)
    (let ((orig (point)))
      (if (moder--direction-forward-p)
          ;; forward direction, add cursors at words' end
          (progn
            (save-mark-and-excursion
              (goto-char (point-min))
              (while (let ((p (point)))
                       (forward-thing thing 1)
                       (not (= p (point))))
                (unless (= (point) orig)
                  (moder--beacon-add-overlay-at-point (moder--hack-cursor-pos (point)))))))

        (save-mark-and-excursion
          (goto-char (point-max))
          (while (let ((p (point)))
                   (forward-thing thing -1)
                   (not (= p (point))))
            (unless (= (point) orig)
              (moder--beacon-add-overlay-at-point (point))))))))
  (moder--beacon-shrink-selection))

(defun moder--add-beacons-for-match (match)
  "Add beacon for match(mark, visit or search).

MATCH is the search regexp."
  (save-restriction
    (moder--narrow-secondary-selection)
    (let ((orig-end (region-end))
          (orig-beg (region-beginning))
          (back (moder--direction-backward-p)))
      (save-mark-and-excursion
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (while (re-search-forward match nil t)
            (unless (or (= orig-end (point))
                        (= orig-beg (point)))
              (let ((match (match-data)))
                (moder--beacon-add-overlay-at-region
                 '(select . visit)
                 (car match)
                 (cadr match)
                 back)))))
        (setq moder--beacon-overlays (reverse moder--beacon-overlays))))))

(defun moder--beacon-count-lines (beg end)
  "Count selected lines from BEG to END."
  (if (and (= (point) (line-beginning-position))
           (moder--direction-forward-p))
      (1+ (count-lines beg end))
    (count-lines beg end)))

(defun moder--beacon-forward-line (n bound)
  "Forward N line, inside BOUND."
  (cond
   ((> n 0)
    (when (> n 1) (forward-line (1- n)))
    (unless (<= bound (line-end-position))
      (forward-line 1)))
   ((< n 0)
    (when (< n -1) (forward-line (+ n 1)))
    (unless (>= bound (line-beginning-position))
      (forward-line -1)))
   (t
    (not (= (point) bound)))))

(defun moder--add-beacons-for-line ()
  "Add beacon for line movement."
  (save-restriction
    (moder--narrow-secondary-selection)
    (let* ((beg (region-beginning))
           (end (region-end))
           (ln (moder--beacon-count-lines beg end))
           (back (moder--direction-backward-p))
           prev)
      (save-mark-and-excursion
        (goto-char end)
        (forward-line)
        (setq prev (point))
        (while (moder--beacon-forward-line
                (1- ln)
                (point-max))
          (moder--beacon-add-overlay-at-region
           '(select . line)
           prev
           (line-end-position)
           back)
          (forward-line 1)
          (setq prev (point))))
      (save-mark-and-excursion
        (goto-char (point-min))
        (setq prev (point))
        (while (moder--beacon-forward-line
                (1- ln)
                beg)
          (moder--beacon-add-overlay-at-region
           '(select . line)
           prev
           (line-end-position)
           back)
          (forward-line 1)
          (setq prev (point)))))))

(defun moder--add-beacons-for-join ()
  "Add beacon for join movement."
  (save-restriction
    (moder--narrow-secondary-selection)
    (let ((orig (point)))
      (save-mark-and-excursion
        (goto-char (point-min))
        (back-to-indentation)
        (unless (= (point) orig)
          (moder--beacon-add-overlay-at-point (point)))
        (while (< (line-end-position) (point-max))
          (forward-line 1)
          (back-to-indentation)
          (unless (= (point) orig)
            (moder--beacon-add-overlay-at-point (point))))))
    (moder--cancel-selection)))

(defun moder--add-beacons-for-find ()
  "Add beacon for find movement."
  (let ((ch-str (if (eq moder--last-find 13)
                    "\n"
                  (char-to-string moder--last-find))))
    (save-restriction
      (moder--narrow-secondary-selection)
      (let ((orig (point))
            (case-fold-search nil))
        (if (moder--direction-forward-p)
            (save-mark-and-excursion
              (goto-char (point-min))
              (while (search-forward ch-str nil t)
                (unless (= orig (point))
                  (moder--beacon-add-overlay-at-point (moder--hack-cursor-pos (point))))))
          (save-mark-and-excursion
            (goto-char (point-max))
            (while (search-backward ch-str nil t)
              (unless (= orig (point))
                (moder--beacon-add-overlay-at-point (point))))))))
    (moder--beacon-shrink-selection)))

(defun moder--add-beacons-for-till ()
  "Add beacon for till movement."
  (let ((ch-str (if (eq moder--last-till 13)
                    "\n"
                  (char-to-string moder--last-till))))
    (save-restriction
      (moder--narrow-secondary-selection)
      (let ((orig (point))
            (case-fold-search nil))
        (if (moder--direction-forward-p)
            (progn
              (save-mark-and-excursion
                (goto-char (point-min))
                (while (search-forward ch-str nil t)
                  (unless (or (= orig (1- (point)))
                              (zerop (- (point) 2)))
                    (moder--beacon-add-overlay-at-point (moder--hack-cursor-pos (1- (point))))))))
          (save-mark-and-excursion
            (goto-char (point-max))
            (while (search-backward ch-str nil t)
              (unless (or (= orig (1+ (point)))
                          (= (point) (point-max)))
                (moder--beacon-add-overlay-at-point (1+ (point)))))))))
    (moder--beacon-shrink-selection)))

(defun moder--beacon-region-words-to-match ()
  "Convert the word selected in region to a regexp."
  (let ((s (buffer-substring-no-properties
            (region-beginning)
            (region-end)))
        (re (car regexp-search-ring)))
    (if (string-match-p (format "\\`%s\\'" re) s)
        re
      (format "\\<%s\\>" (regexp-quote s)))))

(defun moder--beacon-update-overlays ()
  "Update overlays for BEACON state."
  (moder--beacon-remove-overlays)
  (when (moder--beacon-inside-secondary-selection)
    (let* ((ex (car (moder--selection-type)))
           (type (cdr (moder--selection-type))))
      (cl-case type
        ((nil transient) (moder--add-beacons-for-char))
        ((word) (if (not (eq 'expand ex))
                    (moder--add-beacons-for-thing moder-word-thing)
                  (moder--add-beacons-for-match (moder--beacon-region-words-to-match))))
        ((symbol) (if (not (eq 'expand ex))
                      (moder--add-beacons-for-thing moder-symbol-thing)
                    (moder--add-beacons-for-match (moder--beacon-region-words-to-match))))
        ((visit) (moder--add-beacons-for-match (car regexp-search-ring)))
        ((line) (moder--add-beacons-for-line))
        ((join) (moder--add-beacons-for-join))
        ((find) (moder--add-beacons-for-find))
        ((till) (moder--add-beacons-for-till))
        ((char) (when (eq 'expand ex) (moder--add-beacons-for-char-expand)))))))

(defun moder-beacon-end-and-apply-kmacro ()
  "End or apply kmacro."
  (interactive)
  (call-interactively #'kmacro-end-macro)
  (moder--beacon-apply-kmacros))

(defun moder-beacon-start ()
  "Start kmacro recording, apply to all cursors when terminate."
  (interactive)
  (moder--switch-state 'normal)
  (call-interactively 'kmacro-start-macro)
  (setq-local moder--beacon-insert-enter-key nil)
  (setq moder--beacon-defining-kbd-macro 'record))

(defun moder-beacon-insert-exit ()
  "Exit insert mode and terminate kmacro recording."
  (interactive)
  (when defining-kbd-macro
    (end-kbd-macro)
    (moder--beacon-apply-kmacros-from-insert))
  (moder--switch-state 'beacon))

(defun moder-beacon-insert ()
  "Insert and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (moder-beacon-mode -1)
  (moder-insert)
  (call-interactively #'kmacro-start-macro)
  (setq-local moder--beacon-insert-enter-key last-input-event)
  (setq moder--beacon-defining-kbd-macro 'quick))

(defun moder-beacon-append ()
  "Append and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (moder-beacon-mode -1)
  (moder-append)
  (call-interactively #'kmacro-start-macro)
  (setq-local moder--beacon-insert-enter-key last-input-event)
  (setq moder--beacon-defining-kbd-macro 'quick))

(defun moder-beacon-change ()
  "Change and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (moder--with-selection-fallback
   (moder-beacon-mode -1)
   (moder-change)
   (call-interactively #'kmacro-start-macro)
   (setq-local moder--beacon-insert-enter-key last-input-event)
   (setq moder--beacon-defining-kbd-macro 'quick)))

(defun moder-beacon-change-save ()
  "Change and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (moder--with-selection-fallback
   (moder-beacon-mode -1)
   (moder-change-save)
   (call-interactively #'kmacro-start-macro)
   (setq-local moder--beacon-insert-enter-key last-input-event)
   (setq moder--beacon-defining-kbd-macro 'quick)))

(defun moder-beacon-change-char ()
  "Change and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (moder-beacon-mode -1)
  (moder-change-char)
  (call-interactively #'kmacro-start-macro)
  (setq-local moder--beacon-insert-enter-key last-input-event)
  (setq moder--beacon-defining-kbd-macro 'quick))

(defun moder-beacon-replace ()
  "Replace all selection with current kill-ring head."
  (interactive)
  (moder--with-selection-fallback
   (moder--wrap-collapse-undo
    (moder-replace)
    (save-mark-and-excursion
      (cl-loop for ov in moder--beacon-overlays do
               (when (and (overlayp ov)
                          (not (eq 'cursor (overlay-get ov 'moder-beacon-type))))
                 (goto-char (overlay-start ov))
                 (push-mark (overlay-end ov) t)
                 (moder-replace)
                 (delete-overlay ov)))))))

(defun moder--beacon-delete-region ()
  (moder--delete-region (region-beginning) (region-end)))

(defun moder-beacon-kill-delete ()
  "Delete all selections.

By default, this command will be remapped to `moder-kill'.
Because `moder-kill' are used for deletion on region.

Only the content in real selection will be saved to `kill-ring'."
  (interactive)
  (moder--with-selection-fallback
   (moder--wrap-collapse-undo
    (moder-kill)
    (save-mark-and-excursion
      (cl-loop for ov in moder--beacon-overlays do
               (when (and (overlayp ov)
                          (not (eq 'cursor (overlay-get ov 'moder-beacon-type))))
                 (goto-char (overlay-start ov))
                 (push-mark (overlay-end ov) t)
                 (moder--beacon-delete-region)
                 (delete-overlay ov)))))))

(defun moder-beacon-apply-kmacro ()
  (interactive)
  (moder--switch-state 'normal)
  (call-interactively #'kmacro-call-macro)
  (moder--beacon-apply-kmacros)
  (moder--switch-state 'beacon))

(defun moder-beacon-noop ()
  "Noop, to disable some keybindings in cursor state."
  (interactive))

(provide 'moder-beacon)
;;; moder-beacon.el ends here
