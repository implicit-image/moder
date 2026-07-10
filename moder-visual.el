;;; moder-visual.el --- Visual effect in Moder  -*- lexical-binding: t; -*-

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
;; Implementation for all commands in Moder.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pcase)

(require 'moder-var)
(require 'moder-util)

(eval-when-compile
  (require 'moder-selection))

(declare-function hl-line-highlight "hl-line")
(declare-function moder--selection-type "moder-selection")


(defvar moder--match-overlays nil
  "Overlays used to highlight matches in buffer.")

(defvar moder--search-indicator-overlay nil
  "Overlays used to display search indicator in current line.")

(defvar-local moder--search-indicator-state nil
  "The state for search indicator.

Value is a list of (last-regexp last-pos idx cnt).")

(defvar moder--dont-remove-overlay nil
  "Indicate we should prevent removing overlay for once.")

(defvar moder--highlight-timer nil
  "Timer for highlight cleaner.")

(defun moder--remove-match-highlights ()
  ;; (mapc #'delete-overlay moder--match-overlays)
  (dolist (ov moder--match-overlays)
    (delete-overlay ov))
  (setq moder--match-overlays nil))

(defun moder--remove-search-highlight ()
  (when moder--search-indicator-overlay
    (delete-overlay moder--search-indicator-overlay)))

(defun moder--clean-search-indicator-state ()
  (setq moder--search-indicator-overlay nil
        moder--search-indicator-state nil))

(defun moder--remove-search-indicator ()
  (moder--remove-search-highlight)
  (moder--clean-search-indicator-state))

(defun moder--show-indicator (pos idx cnt)
  (goto-char pos)
  (goto-char (line-end-position))
  (if (= (point) (point-max))
      (let ((ov (make-overlay (point) (point))))
        (overlay-put ov 'after-string (propertize (format " [%d/%d]" idx cnt) 'face 'moder-search-indicator))
        (setq moder--search-indicator-overlay ov))
    (let ((ov (make-overlay (point) (1+ (point)))))
      (overlay-put ov 'display (propertize (format " [%d/%d] \n" idx cnt) 'face 'moder-search-indicator))
      (setq moder--search-indicator-overlay ov))))

(defun moder--highlight-match ()
  (let ((beg (match-beginning 0))
        (end (match-end 0)))
    (unless (cl-find-if (lambda (it)
                          (overlay-get it 'moder))
                        (overlays-at beg))
      (let ((ov (make-overlay beg end)))
        (overlay-put ov 'face 'moder-search-highlight)
        (overlay-put ov 'priority 0)
        (overlay-put ov 'moder t)
        (push ov moder--match-overlays)))))

(defun moder--highlight-regexp-in-buffer (regexp)
  "Highlight all regexp in this buffer."
  (when (and (moder-normal-mode-p)
             (region-active-p))
    (let* ((cnt 0)
           (idx 0)
           (pos (region-end))
           (hl-start (max (point-min) (- (point) 3000)))
           (hl-end (min (point-max) (+ (point) 3000))))
      (setq moder--expand-nav-function nil)
      (setq moder--visual-command this-command)
      (save-mark-and-excursion
        (moder--remove-search-indicator)
        (let ((case-fold-search nil))
          (goto-char (point-min))
          (while (re-search-forward regexp (point-max) t)
            (cl-incf cnt)
            (when (<= (match-beginning 0) pos (match-end 0))
              (setq idx cnt))
            (when (<= hl-start (point) hl-end)
              (moder--highlight-match)))
          (moder--show-indicator pos idx cnt))))))

(defun moder--select-expandable-p ()
  (when (moder-normal-mode-p)
    (when-let* ((sel (moder--selection-type)))
      (let ((type (cdr sel)))
        (member type '(word symbol line block find till))))))

(provide 'moder-visual)
;;; moder-visual.el ends here
