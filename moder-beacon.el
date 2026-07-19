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
(declare-function moder-noop "moder-command")
(declare-function moder-kill "moder-command")
(declare-function moder--select "moder-command")
(declare-function moder-beacon-mode "moder-core")
(declare-function moder-change-save "moder-command")
(declare-function moder-escape-or-normal-modal "moder-command")
(declare-function moder-open-above "moder-command")
(declare-function moder-open-below "moder-command")
(declare-function moder-save "moder-command")
(declare-function moder--kmacro-start-macro "moder-kmacro")
(declare-function moder--kmacro-end-macro "moder-kmacro")
(declare-function moder--cancel-selection "moder-selection")
(declare-function moder--selection-fallback "moder-selection")
(declare-function moder--make-selection "moder-selection")
(declare-function moder--selection-command "moder-selection")
(declare-function moder--selection-thing "moder-selection")
(declare-function moder--selection-type "moder-selection")
(declare-function moder--direction-backward-p "moder-selection")
(declare-function moder--direction-forward-p "moder-selection")
(declare-function moder--selection-maybe-update-current "moder-selection")

(eval-when-compile
  (require 'moder-selection))

(defvar-local moder--beacon-overlays nil)
(defvar-local moder--beacon-insert-enter-key nil)
(defvar-local moder--beacon-suspended-modes nil)
(defvar moder--beacon-kill-ring-append-entry nil)

(defvar moder--beacon-fake-selection-change nil)

(defun moder--beacon-pre-redisplay (&rest _)
  "Update beacon preview display."
  (unless (or executing-kbd-macro
              defining-kbd-macro)
    (moder--beacon-update-overlays-for-preview)))

(defun moder--beacon-suspend-modes ()
  "Suspend modes that would interfere with or slow down macro execution."
  (dolist (mode-cmd moder-kmacro-minor-mode-inhibit-commands)
    (when (and (boundp mode-cmd)
               (eq (symbol-value mode-cmd) t))
      (let ((inhibit-message t))
        (funcall-interactively mode-cmd -1)
        (message "turning off %S" mode-cmd))
      (push mode-cmd moder--beacon-suspended-modes))))

(defun moder--beacon-resume-modes ()
  "Resume modes suspended in `moder--beacon-suspend-modes'."
  (dolist (mode-cmd moder--beacon-suspended-modes)
    (let ((inhibit-message t))
      (funcall-interactively mode-cmd 1)
      (message "turning %S back on" mode-cmd)))
  (setq moder--beacon-suspended-modes nil))

(defun moder--beacon-get-preview-start (&optional limit-fn)
  "Get the start position of current beacon preview area.
If LIMIT-FN is provided, it is used to move backwards to find
the far preview bound."
  (let* ((w-start (window-start))
         (limit-fn (or limit-fn #'forward-line))
         (margin-start (- w-start moder-beacon-preview-start-margin))
         (ov-start (overlay-start mouse-secondary-overlay))
         (far-limit-start (save-mark-and-excursion
                            (goto-char margin-start)
                            (ignore-errors (funcall limit-fn -1))
                            (point)))
         (near-limit-start (save-mark-and-excursion
                             (goto-char margin-start)
                             (ignore-errors (funcall limit-fn 1))
                             (point))))
    (cond
     ((< ov-start far-limit-start)
      far-limit-start)
     ((< ov-start margin-start)
      margin-start)
     ((< ov-start near-limit-start)
      near-limit-start)
     ((< ov-start w-start)
      w-start)
     (t ov-start))))

(defun moder--beacon-get-preview-end (&optional limit-fn)
  "Get the end position of current beacon preview area.
If LIMIT-FN is provided, it is used to move forward to find
the far preview bound."
  (let* ((w-end (window-end (selected-window) t))
         (limit-fn (or limit-fn #'forward-line))
         (margin-end (+ w-end moder-beacon-preview-end-margin))
         (ov-end (overlay-end mouse-secondary-overlay))
         (far-limit-end (save-mark-and-excursion
                          (goto-char margin-end)
                          (ignore-errors (funcall limit-fn 1))
                          (point)))
         (near-limit-end (save-mark-and-excursion
                           (goto-char margin-end)
                           (ignore-errors (funcall limit-fn -1))
                           (point))))
    (cond
     ((> ov-end far-limit-end)
      far-limit-end)
     ((> ov-end margin-end)
      margin-end)
     ((> ov-end near-limit-end w-end)
      near-limit-end)
     ((> ov-end w-end)
      w-end)
     (t ov-end))))

(defmacro moder--beacon-in-range-p (preview start end &optional p)
  "Return an `or' form that checks if current point or P is within preview
bounds (START . END) or if PREVIEW is non-nil."
  `(or (null ,preview)
       (and (>= ,(or p '(point)) ,start)
            (<= ,(or p '(point)) ,end))))

(defmacro moder--beacon-eval-expr (&rest cmd)
  "Return an expression to evaluate CMD on each of `moder-beacon-overlays'."
  `(when moder--beacon-overlays
     (let ((gc-cons-threshold most-positive-fixnum))
       ;; turning off modes takes longer than macro execution with few overlays
       (when (length> moder--beacon-overlays 200)
         (moder--beacon-suspend-modes))
       (unwind-protect
           (let* ((bak (overlay-get (car moder--beacon-overlays)
                                    'moder-beacon-backward))
                  (pre-command-hook (let ((hook nil))
                                      (dolist (fn pre-command-hook (reverse hook))
                                        (when (not (memq fn moder-kmacro-pre-command-inhibit-functions))
                                          (push fn hook)))))
                  (post-command-hook (let ((hook nil))
                                       (dolist (fn post-command-hook (reverse hook))
                                         (when (not (memq fn moder-kmacro-post-command-inhibit-functions))
                                           (push fn hook)))))
                  (post-self-insert-hook (let ((hook nil))
                                           (dolist (fn post-self-insert-hook (reverse hook))
                                             (when (not (memq fn moder-kmacro-post-self-insert-inhibit-functions))
                                               (push fn hook)))))
                  (hl-line-mode nil)
                  (moder-executing-kmacro t)
                  (inhibit-redisplay moder-beacon-inhibit-redisplay-during-execution)
                  (inhibit-message moder-beacon-inhibit-message-during-execution)
                  (split (seq-split (if bak (reverse moder--beacon-overlays) moder--beacon-overlays)
                                    moder-beacon-max-overlay-list-length)))
             (moder--wrap-collapse-undo
              (save-mark-and-excursion
                (cl-loop
                 for l in split do
                 (cl-loop
                  for ov in l do
                  (when (overlayp ov)
                    (let ((type (overlay-get ov 'moder-beacon-type))
                          (ov-start (overlay-start ov))
                          (ov-end (overlay-end ov))
                          (backward (overlay-get ov 'moder-beacon-backward)))
                      (moder--switch-state 'normal t)
                      (if (eq type 'cursor)
                          (progn
                            (moder--cancel-selection)
                            (goto-char ov-start))
                        (thread-first
                          (if backward
                              (moder--create-selection type ov-end ov-start)
                            (moder--create-selection type ov-start ov-end))
                          (moder--select t)))
                      ,@cmd)
                    (delete-overlay ov)))))))
         (moder--beacon-resume-modes)))))

(defun moder--beacon-add-overlay-at-point (pos &optional count)
  "Create an overlay to draw a fake cursor as beacon at POS.
If COUNT is non-nil, use it to indicate selection count."
  (let ((ov (make-overlay pos (1+ pos) nil t)))
    (setq moder--beacon-fake-selection-change (not moder--beacon-fake-selection-change))
    (overlay-put ov 'face 'moder-beacon-fake-cursor)
    (overlay-put ov 'moder-beacon-type 'cursor)
    (overlay-put ov 'moder-beacon-count count)
    (push ov moder--beacon-overlays)))

(defun moder--beacon-add-overlay-at-region (type p1 p2 backward &optional count)
  "Create an overlay to draw a fake selection as beacon from P1 to 12.

TYPE is used for selection type.
Non-nil BACKWARD means backward direction."
  (let ((ov (make-overlay p1 p2)))
    (setq moder--beacon-fake-selection-change (not moder--beacon-fake-selection-change))
    (overlay-put ov 'face 'moder-beacon-fake-selection)
    (overlay-put ov 'moder-beacon-type type)
    (overlay-put ov 'moder-beacon-backward backward)
    (overlay-put ov 'moder-beacon-count count)
    (push ov moder--beacon-overlays)))

(defun moder--beacon-remove-overlays ()
  "Remove all beacon overlays from current buffer."
  (when moder--beacon-overlays
    (mapc #'delete-overlay moder--beacon-overlays)
    (setq moder--beacon-overlays nil
          moder--beacon-overlay-count 0)))

(defun moder--maybe-toggle-beacon-state (&rest _args)
  "Maybe switch to BEACON state."
  (unless (minibufferp)
    ;; update current selection information
    (moder--selection-maybe-update-current)
    ;; if last command start recording kmacro from beacon state
    ;; create all overlays instead of preview
    (if (and moder--beacon-started-kmacro defining-kbd-macro)
        (moder--beacon-ensure-all-overlays)
      (unless (or defining-kbd-macro executing-kbd-macro
                  (not (eq (current-buffer) (overlay-buffer mouse-secondary-overlay)))) ;; MAYBE: make sure this doesnt blow something up
        (let ((inside (moder--beacon-inside-secondary-selection)))
          (cond
           ((and (moder-normal-mode-p)
                 inside)
            (moder--switch-state 'beacon)
            (moder--beacon-update-overlays t))
           ((moder-beacon-mode-p)
            (if inside
                (moder--beacon-update-overlays t)
              (moder--beacon-remove-overlays)
              (moder--switch-state 'normal)))))))))

(defun moder--beacon-shrink-selection ()
  "Shrink selection to one char width."
  (if moder-use-cursor-position-hack
      (let ((m (if (moder--direction-forward-p)
                   (1- (point))
                 (1+ (point)))))
        (moder--cancel-selection)
        (thread-first
          (moder--create-selection '(select . transient) m (point))
          (moder--select t)))
    (moder--cancel-selection)))

(defun moder--beacon-apply-command (cmd)
  "Apply CMD in BEACON state."
  (moder--beacon-eval-expr (call-interactively cmd)))

(defun moder--beacon-apply-kmacros-from-insert ()
  "Apply kmacros in BEACON state, after exiting from insert.

This is treated separately because we must enter each insert state the
same way, and escape each time the macro is applied."
  (when last-kbd-macro
    (let ((last-kbd-macro (vconcat (vector moder--beacon-insert-enter-key) last-kbd-macro)))
      (moder--beacon-eval-expr
       (call-interactively #'kmacro-call-macro)
       (moder-escape-or-normal-modal t)))))

(defun moder--beacon-apply-kmacros ()
  "Apply kmacros in BEACON state."
  (moder--beacon-eval-expr
   (call-interactively 'kmacro-call-macro)))

(defun moder--add-beacons-for-char (&optional preview n)
  "Add beacon for char movement for N chars. If PREVIEW is non-nil, create overlays only in currently visible \
part of the buffer."
  (save-restriction
    (let* ((bounds (moder--second-sel-bound))
           (beg (car bounds))
           (end (cdr bounds))
           (curr (point))
           (col (- (point) (line-beginning-position)))
           (beacon-area-start (moder--beacon-get-preview-start))
           (beacon-area-end (moder--beacon-get-preview-end))
           break)
      (save-mark-and-excursion
        (while (< (line-end-position) end)
          (forward-line 1)
          (when (moder--beacon-in-range-p preview beacon-area-start beacon-area-end)
            (let ((pos (+ col (line-beginning-position))))
              (when (<= pos (min end (line-end-position)))
                (moder--beacon-add-overlay-at-point pos))))))
      (save-mark-and-excursion
        (goto-char beg)
        (while (not break)
          (if (>= (line-end-position) curr)
              (setq break t)
            (when (moder--beacon-in-range-p preview beacon-area-start beacon-area-end)
              (let ((pos (+ col (line-beginning-position))))
                (when (and
                       (>= pos beg)
                       (<= pos (line-end-position)))
                  (moder--beacon-add-overlay-at-point pos))))
            (forward-line 1))))))
  (setq moder--beacon-overlays (reverse moder--beacon-overlays))
  (moder--cancel-selection))

(defun moder--add-beacons-for-char-expand (&optional preview n)
  "Add beacon for char expand movement for N chars. If PREVIEW is non-nil, create overlays only in currently visible \
part of the buffer."
  (save-restriction
    (let* ((bounds (moder--second-sel-bound))
           (ss-beg (car bounds))
           (ss-end (cdr bounds))
           (curr (point))
           (bak (moder--direction-backward-p))
           (beg-col (- (region-beginning) (line-beginning-position)))
           (end-col (- (region-end) (line-beginning-position)))
           (beacon-area-start (moder--beacon-get-preview-start))
           (beacon-area-end (moder--beacon-get-preview-end))
           break)
      (save-mark-and-excursion
        (while (< (line-end-position) ss-end)
          (forward-line 1)
          (let ((beg (+ beg-col (line-beginning-position)))
                (end (+ end-col (line-beginning-position))))
            (when (and (<= end (min ss-end (line-end-position)))
                       (moder--beacon-in-range-p preview beacon-area-start beacon-area-end))
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
                     (<= end (line-end-position))
                     (moder--beacon-in-range-p preview beacon-area-start beacon-area-end))
                (moder--beacon-add-overlay-at-region
                 '(expand . char)
                 beg
                 end
                 bak)))
            (forward-line 1)))))
    (setq moder--beacon-overlays (reverse moder--beacon-overlays))))

(defun moder--add-beacons-for-thing (thing &optional preview n)
  "Add beacon for beginning or end for N THINGs. If PREVIEW is non-nil, create overlays only in visible\
part of the buffer."
  (if-let* ((forward-fn (cond
                         ;; TODO: add handling for custom values of `moder-word-thing'
                         ;; and `moder-symbol-thing'
                         ((eq thing 'word) #'forward-word)
                         ((eq thing 'symbol) #'forward-symbol)
                         (t
                          (moder--get-forward-function thing))))
            (_ (functionp forward-fn)))
      (progn
        (message "adding beacons for thing %S" thing)
        (save-restriction
          (moder--narrow-secondary-selection)
          (let ((orig (point))
                (last (point))
                (n (or n 1))
                (beacon-area-start (moder--beacon-get-preview-start forward-fn))
                (beacon-area-end (moder--beacon-get-preview-end forward-fn))
                (selection-found nil)
                (rb (region-beginning))
                (re (region-end))
                (backward (moder--direction-backward-p)))
            (if (not backward)
                ;; forward direction, add cursors at things' end
                (save-mark-and-excursion
                  (goto-char (point-min))
                  (while (let ((p (point)))
                           (ignore-errors (funcall forward-fn n))
                           (not (= p (point))))
                    (unless (not (moder--beacon-in-range-p preview beacon-area-start beacon-area-end))
                      (when (and (< last orig)
                                 (> (point) orig))
                        (setq selection-found (point)))
                      (moder--beacon-add-overlay-at-point (moder--hack-cursor-pos (point)))
                      (setq last (point)))))

              ;; backward direction, cursors at things' beginning
              (save-mark-and-excursion
                (goto-char (point-max))
                (while (let ((p (point)))
                         (ignore-errors (funcall forward-fn (- n)))
                         (not (= p (point))))
                  (unless (not (moder--beacon-in-range-p preview beacon-area-start beacon-area-end))
                    (when (and (> last orig)
                               (< (point) orig))
                      (setq selection-found (point)))
                    (moder--beacon-add-overlay-at-point (point))
                    (setq last (point))))))
            ;; adjust the main selection position if appropriate
            (when (numberp selection-found)
              (goto-char selection-found)
              ;; delete the overlay at point
              (mapc (lambda (ov)
                      (when (and (overlayp ov) (eq 'cursor (overlay-get ov 'moder-beacon-type)))
                        (delete-overlay ov)))
                    (overlays-at (point)))
              (recenter))))
        (moder--beacon-shrink-selection))
    (message "forward thing function for %S not found" thing)))

(defun moder--add-beacons-for-thing-bounds (thing &optional preview n inner)
  "Add beacons for bounds of N `moder' THINGs. If PREVIEW is non-nil, create overlays only in visible\
part of the buffer."
  (if-let* ((forward-fn (cond
                         ;; TODO: add handling for custom values of `moder-word-thing'
                         ;; and `moder-symbol-thing'
                         ((eq thing 'word) #'forward-word)
                         ((eq thing 'symbol) #'forward-symbol)
                         (t
                          (moder--get-forward-function thing))))
            (_ (functionp forward-fn))
            (_ (message "forward-fn is %S" forward-fn)))
      (progn
        (message "adding beacons for thing %S" thing)
        (save-restriction
          (moder--narrow-secondary-selection)
          (let ((orig (point))
                (n (or n 1))
                (backward (not (moder--direction-forward-p)))
                (beacon-area-start (moder--beacon-get-preview-start forward-fn))
                (beacon-area-end (moder--beacon-get-preview-end forward-fn))
                (selection-found nil)
                (rb (region-beginning))
                (re (region-end))
                (first t))
            (progn
              ;; if we are selecting one thing, there is no point in adjusting selection
              (setq selection-found (= (abs n) 1))
              (save-mark-and-excursion
                (goto-char (point-min))
                (while (or (prog1 first
                             (setq first nil))
                           ;; check if we moved
                           (let ((p (point)))
                             (ignore-errors (funcall-interactively forward-fn n))
                             (not (= (point) p))))
                  (unless (not (moder--beacon-in-range-p preview beacon-area-start beacon-area-end))
                    ;; parse bounds of thing at point
                    (when-let* ((bounds (moder--parse-range-of-thing thing (if inner 'inner 'range)))
                                (start (car bounds))
                                (end (cdr bounds)))
                      ;; if current selection was not found yest, check if current bounds
                      ;; overlap with with it
                      (when (and (not selection-found)
                                 (or
                                  (and (<= end re) (>= end rb))
                                  (and (>= start rb) (<= start re))))
                        (setq selection-found (cons start end)
                              moder--beacon-fake-selection-change (not moder--beacon-fake-selection-change)))
                      (moder--beacon-add-overlay-at-region '(select . thing) start end backward))))))
            ;; correct the current selection to maintain
            ;; continuity in case of multiple selections
            (when (consp selection-found)
              (moder--move-current-selection (car selection-found) (cdr selection-found))
              (mapc (lambda (ov)
                      (when (and (overlayp ov) (eq 'cursor (overlay-get ov 'moder-beacon-type)))
                        (delete-overlay ov)))
                    (overlays-at (point)))
              (recenter)))))
    (message "forward thing function for %S not found" thing)))

(defun moder--add-beacons-for-match (match &optional preview n)
  "Add beacon for match(mark, visit or search). If PREVIEW is non-nil, create overlays only in visible\
part of the buffer.

MATCH is the search regexp."
  (save-restriction
    (moder--narrow-secondary-selection)
    (let ((orig-end (region-end))
          (orig-beg (region-beginning))
          (back (moder--direction-backward-p))
          (beacon-area-start (moder--beacon-get-preview-start))
          (beacon-area-end (moder--beacon-get-preview-end)))
      (save-mark-and-excursion
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (while (re-search-forward match nil t)
            (unless (or (= orig-end (point))
                        (= orig-beg (point))
                        (not (moder--beacon-in-range-p preview beacon-area-start beacon-area-end)))
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

(defun moder--add-beacons-for-line (&optional preview)
  "Add beacon for line movement. If PREVIEW is non-nil, create overlays only in visible\
part of the buffer."
  (save-restriction
    (moder--narrow-secondary-selection)
    (let* ((beg (region-beginning))
           (end (region-end))
           (ln (moder--beacon-count-lines beg end))
           (back (moder--direction-backward-p))
           (beacon-area-start (moder--beacon-get-preview-start))
           (beacon-area-end (moder--beacon-get-preview-end))
           prev)
      (save-mark-and-excursion
        (goto-char end)
        (forward-line)
        (setq prev (point))
        (while (moder--beacon-forward-line (1- ln) (point-max))
          (when (moder--beacon-in-range-p preview beacon-area-start beacon-area-end)
            (moder--beacon-add-overlay-at-region
             '(select . line)
             prev
             (line-end-position)
             back))
          (forward-line 1)
          (setq prev (point))))
      (save-mark-and-excursion
        (goto-char (point-min))
        (setq prev (point))
        (while (moder--beacon-forward-line (1- ln) beg)
          (when (moder--beacon-in-range-p preview beacon-area-start beacon-area-end)
            (moder--beacon-add-overlay-at-region
             '(select . line)
             prev
             (line-end-position)
             back))
          (forward-line 1)
          (setq prev (point)))))))

(defun moder--add-beacons-for-join (&optional preview n)
  "Add beacon for N join movements. If PREVIEW is non-nil, create overlays only in visible\
part of the buffer."
  (save-restriction
    (moder--narrow-secondary-selection)
    (let ((orig (point))
          (beacon-area-start (moder--beacon-get-preview-start))
          (beacon-area-end (moder--beacon-get-preview-end)))
      (save-mark-and-excursion
        (goto-char (point-min))
        (back-to-indentation)
        (unless (or (= (point) orig)
                    (not (moder--beacon-in-range-p preview beacon-area-start beacon-area-end)))
          (moder--beacon-add-overlay-at-point (point)))
        (while (< (line-end-position) (point-max))
          (forward-line 1)
          (back-to-indentation)
          (unless (or (= (point) orig)
                      (not (moder--beacon-in-range-p preview beacon-area-start beacon-area-end)))
            (moder--beacon-add-overlay-at-point (point))))))
    (moder--cancel-selection)))

(defun moder--add-beacons-for-find (&optional preview n)
  "Add beacon for N find movements. If PREVIEW is non-nil. create overlays only in visible\
part of the buffer."
  (let ((ch-str (if (eq moder--last-find 13)
                    "\n"
                  (char-to-string moder--last-find))))
    (save-restriction
      (moder--narrow-secondary-selection)
      (let ((orig (point))
            (case-fold-search nil)
            (beacon-area-start (moder--beacon-get-preview-start))
            (beacon-area-end (moder--beacon-get-preview-end)))
        (if (moder--direction-forward-p)
            (save-mark-and-excursion
              (goto-char (point-min))
              (while (search-forward ch-str nil t)
                (unless (or (= orig (point))
                            (not (moder--beacon-in-range-p preview beacon-area-start beacon-area-end)))
                  (moder--beacon-add-overlay-at-point (moder--hack-cursor-pos (point))))))
          (save-mark-and-excursion
            (goto-char (point-max))
            (while (search-backward ch-str nil t)
              (unless (or (= orig (point))
                          (not (moder--beacon-in-range-p preview beacon-area-start beacon-area-end)))
                (moder--beacon-add-overlay-at-point (point))))))))
    (moder--beacon-shrink-selection)))

(defun moder--add-beacons-for-till (&optional preview n)
  "Add beacon for N till movements. If PREVIEW is non-nil, create overlays only in visible\
part of the buffer."
  (let ((ch-str (if (eq moder--last-till 13)
                    "\n"
                  (char-to-string moder--last-till))))
    (save-restriction
      (moder--narrow-secondary-selection)
      (let ((orig (point))
            (n (or n 1))
            (case-fold-search nil)
            (beacon-area-start (moder--beacon-get-preview-start))
            (beacon-area-end (moder--beacon-get-preview-end)))
        (if (moder--direction-forward-p)
            (progn
              (save-mark-and-excursion
                (goto-char (point-min))
                (while (search-forward ch-str nil t n)
                  (let ((here (point)))
                    (unless (or (= orig (1- here))
                                (zerop (- here 2))
                                (not (moder--beacon-in-range-p preview beacon-area-start beacon-area-end)))
                      (moder--beacon-add-overlay-at-point (moder--hack-cursor-pos (1- (point)))))))))
          (save-mark-and-excursion
            (goto-char (point-max))
            (while (search-backward ch-str nil t n)
              (unless (or (= orig (1+ (point)))
                          (= (point) (point-max))
                          (not (moder--beacon-in-range-p preview beacon-area-start beacon-area-end)))
                (moder--beacon-add-overlay-at-point (1+ (point)))))))))
    (moder--beacon-shrink-selection)))

;; TODO: implement some kind of sensible thing for block
(defun moder--add-beacons-for-block (preview)
  "Add beacon for block movement.

If PREVIEW is non-nil, create overlays only in visible parts of the buffer."
  (user-error "`moder--add-beacons-for-block' not implemented"))

;; TODO: how to actually implement this?
;; MAYBE: make some kind of mode for expanding current selection
;; pros:
;; - allow for more free sleection
;; cons:
;; - there are a lot of issues to iron out (overlapping beacons, etc)
;; (defun moder--add-beacons-for-region (preview)
;;   "Add beacons for regular region selection.
;;
;; If PREVIEW is non-nil, create overlays only in visible parts of the buffer."
;;   (message "adding beacons for region!")
;;   (save-restriction
;;     (moder--narrow-secondary-selection)
;;     (let ((orig (point))))))
;;
;; (defun moder--add-beacons-for-region-expand (preview)
;;   "Add beacons for regular region selection expansion.
;;
;; If PREVIEW is non-nil, create overlays only in visible parts of the buffer."
;;   (message "Adding beacons for region-expand")
;;   (save-restriction
;;     (moder--narrow-secondary-selection)
;;     ))

(defun moder--beacon-region-words-to-match ()
  "Convert the word selected in region to a regexp."
  (if moder--beacon-started-kmacro
      moder--beacon-last-match
    (let ((s (buffer-substring-no-properties
              (region-beginning)
              (region-end)))
          (re (car regexp-search-ring)))
      (if (string-match-p (format "\\`%s\\'" re) s)
          re
        (format "\\<%s\\>" (regexp-quote s))))))

(defvar moder--beacon-selection-history nil)

(defvar moder--beacon-started-kmacro nil)

(defvar moder--beacon-last-selection nil)

(defvar moder--beacon-last-match nil)

(defvar moder--beacon-last-selection-command nil)

(defun moder--beacon-update-overlays-for-preview (window start)
  "Update preview overlays for beacon mode for WINDOW starting at START."
  (when (and (not defining-kbd-macro)
             (not executing-kbd-macro))
    (moder--beacon-update-overlays t)))

(defun moder--beacon-ensure-all-overlays (&optional selection)
  "Make sure that all overlays for last selection are created."
  (let ((moder--selection (or selection
                              moder--beacon-last-selection)))
    (moder--beacon-update-overlays)
    (setq moder--beacon-started-kmacro nil
          moder--beacon-last-selection nil)))

(defun moder--beacon-save-current-selection ()
  "Save current selection data and match string."
  (unless moder--beacon-started-kmacro
    (setq moder--beacon-last-selection (moder--current-selection)
          moder--beacon-last-match (moder--beacon-region-words-to-match))))

(defun moder--beacon-update-overlays (&optional preview)
  "Update overlays for BEACON state. If PREVIEW is non-nil, update overlays only in\
visible part of the buffer (plus margins, see `moder-beacon-preview-start-margin', `moder-beacon-preview-end-margin'.)"
  (if (moder--beacon-inside-secondary-selection)
      (progn
        (moder--beacon-save-current-selection)
        (let* ((sel (moder--current-selection))
               (ex (car (moder--selection-type sel)))
               (type (cdr (moder--selection-type sel)))
               (thing (moder--selection-thing sel))
               (no-selection (null sel))
               (last (car-safe moder--beacon-selection-history)))
          ;; In most cases we should remove existing beacon overlays
          ;; but in case the current selection in region and there is
          ;; a previous one, keep the overlays to move them instead
          (unless (eq type 'region)
            (moder--beacon-remove-overlays))
          (if no-selection
              (setq moder--beacon-selection-history nil)
            (push moder--selection moder--beacon-selection-history))
          (cl-case type
            ((nil transient) (moder--add-beacons-for-char preview))
            ((word) (if (not (eq 'expand ex))
                        (moder--add-beacons-for-thing moder-word-thing preview)
                      (moder--add-beacons-for-match (moder--beacon-region-words-to-match) preview)))
            ((symbol) (if (not (eq 'expand ex))
                          (moder--add-beacons-for-thing moder-symbol-thing preview)
                        (moder--add-beacons-for-match (moder--beacon-region-words-to-match) preview)))
            ((visit) (moder--add-beacons-for-match (car regexp-search-ring)))
            ((line) (moder--add-beacons-for-line preview))
            ((join) (moder--add-beacons-for-join preview))
            ((find) (moder--add-beacons-for-find preview))
            ((block) (moder--add-beacons-for-block preview))
            ((till) (moder--add-beacons-for-till preview))
            ((char) (when (eq 'expand ex) (moder--add-beacons-for-char-expand)))
            ;; handle `moder-thing' selection
            ;; a THING must be defined by `moder-thing-register'
            ((thing) (cl-case (moder--selection-command)
                       ((moder-inner-of-thing moder-bounds-of-thing) (moder--add-beacons-for-thing-bounds thing preview))
                       ((moder-end-of-thing moder-beginning-of-thing) (moder--add-beacons-for-thing thing preview))))
            ;; ((region) (if (null last) ;; if the previous selection was cancelled, we are starting anew
            ;;               (moder--add-beacons-for-match (moder--beacon-region-words-to-match) preview)
            ;;             (moder--add-beacons-for-region-expand preview)))
            )
          (moder-update-display)))
    (moder--beacon-remove-overlays)
    (setq moder--beacon-selection-history nil)))

(defun moder--beacon-start-kmacro (switch-sym &rest args)
  "Create all overlays for current selection and start recording kmacro. Use SWITCH-SYM to\
switch modes. if SWITCH-SYM is a symbol corresponding to a moder state, call `moder--switch-state' with it\
as the argument, otherwise turn off `moder-beacon-mode' and call SWITCH-SYM as a function with ARGS."
  (let ((state moder--current-state))
    (cond
     ((functionp switch-sym)
      (moder-beacon-mode -1)
      (apply #'funcall-interactively switch-sym args))
     ((alist-get switch-sym moder-state-mode-alist)
      (moder--switch-state switch-sym))
     (t
      (user-error "%S is neither a function nor a `moder' state" switch-sym)))
    (setq moder--beacon-started-kmacro t)
    (call-interactively #'moder--kmacro-start-macro)
    (unless (eq state moder--current-state)
      (store-kbd-macro-event last-input-event))))

(defun moder-beacon-end-and-apply-kmacro ()
  "End or apply kmacro."
  (interactive)
  (call-interactively #'moder--kmacro-end-macro)
  (moder--beacon-apply-kmacros))

(defun moder-beacon-start ()
  "Start kmacro recording, apply to all cursors when terminate."
  (interactive)
  (moder--beacon-start-kmacro 'normal)
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
  (moder--beacon-start-kmacro #'moder-insert)
  (setq-local moder--beacon-insert-enter-key last-input-event)
  (setq moder--beacon-defining-kbd-macro 'quick))

(defun moder-beacon-append ()
  "Append and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (moder--beacon-start-kmacro #'moder-append)
  (setq-local moder--beacon-insert-enter-key last-input-event)
  (setq moder--beacon-defining-kbd-macro 'quick))

(defun moder-beacon-change ()
  "Change and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (moder--with-selection-fallback
   (moder--beacon-start-kmacro #'moder-change)
   (setq-local moder--beacon-insert-enter-key last-input-event)
   (setq moder--beacon-defining-kbd-macro 'quick)))

(defun moder-beacon-change-save ()
  "Change and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (moder--with-selection-fallback
   (moder--beacon-start-kmacro #'moder-change-save)
   (setq-local moder--beacon-insert-enter-key last-input-event)
   (setq moder--beacon-defining-kbd-macro 'quick)))

(defun moder-beacon-change-char ()
  "Change and start kmacro recording.

Will terminate recording when exit insert mode.
The recorded kmacro will be applied to all cursors immediately."
  (interactive)
  (moder--beacon-start-kmacro #'moder-change-char)
  (setq-local moder--beacon-insert-enter-key last-input-event)
  (setq moder--beacon-defining-kbd-macro 'quick))

(defun moder-beacon-replace ()
  "Replace all selection with current `kill-ring' head."
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

(defun moder-beacon-apply-next-command (keys)
  "Apply next key sequence KEYS to all beacons."
  (interactive (list (read-key-sequence-vector "Command: ")))
  (if-let* ((last-kbd-macro keys)
            ;; check if the sequence doesnt contain C-g
            (_ (seq-contains-p keys 7)))
      (progn
        (call-interactively #'kmacro-call-macro)
        (moder--beacon-eval-expr
         (call-interactively #'kmacro-call-macro)))
    (user-error "Command cancelled")))

(defun moder-beacon-open-above ()
  "Open a newline above, switch to insert state and start recording a kmacro."
  (interactive)
  (moder--beacon-start-kmacro #'moder-open-above)
  (setq-local moder--beacon-insert-enter-key last-input-event)
  (setq moder--beacon-defining-kbd-macro 'quick))

(defun moder-beacon-open-below ()
  "Open a newline below, switch to insert state and start recording a kmacro"
  (interactive)
  (moder--beacon-start-kmacro #'moder-open-below)
  (setq-local moder--beacon-insert-enter-key last-input-event)
  (setq moder--beacon-defining-kbd-macro 'quick))

;; NOTE: this should be examind further, as it seems to conflict with established usage patterns
(defun moder-beacon-save ()
  "Start recording a kmacro and save currently selected region."
  (interactive)
  (when (region-active-p)
    (moder--beacon-start-kmacro #'moder-save)
    (setq-local moder--beacon-insert-enter-key last-input-event)
    (setq moder--beacon-defining-kbd-macro 'quick)))

(provide 'moder-beacon)
;;; moder-beacon.el ends here
