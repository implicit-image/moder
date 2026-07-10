;;; moder-repeat.el --- Repeating command sequences -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; commentary

;;; Code:

(defvar moder--repeat-last-command nil)

(defvar moder--repeat-omit-predicate #'moder--repeat-omit-default-function)

(defvar moder--repeat-last-op nil)

(defun moder--repeat-omit-default-function (cmd)
  "Default function to ignore CMD for purposes of repeating commands."
  (memq cmd '(self-insert-command)))

(defun moder--repeat-ensure-command ()
  "Ensure that `moder--repeat-last-command' is set correctly."
  (let ((should-omit (funcall moder--repeat-omit-default-function this-command)))
    (unless should-omit
      (setq moder--repeat-last-command this-command))))


(defvar-local moder--repeat-last-sequence nil)

(defvar moder--repeat-sequence-ring (make-ring kmacro-ring-max))

(defvar moder-repeat-ignored-commands '(kmacro-start-macro))

(defun moder-repeat-start-recording ())

(defun moder-repeat-stop-recording (remove-hooks)
  (when (and (vectorp moder--repeat-last-sequence)
             (length> moder--repeat-last-sequence 1))
    (ring-insert moder--repeat-sequence-ring moder--repeat-last-sequence)
    (setq moder--repeat-last-sequence nil))
  (when remove-hooks
    (remove-hook 'post-command-hook )))

(defun moder--repeat-pre-command-function ()
  (unless (or defining-kbd-macro executing-kbd-macro (memq this-command moder-repeat-ignored-commands))
    (setq moder--repeat-last-sequence (vconcat (this-command-keys-vector) moder--repeat-last-sequence))))

(defun moder--repeat-start-recording-change (start-func)
  (cond
   ((not (memq moder--current-state '(normal motion)))
    (message "Can only start recording in NORMAL or MOTION state."))
   ((not (functionp start-func))
    (user-error "START-FUNC should be function"))
   (t
    )))

(defun moder--repeat-execute ()
  (when (vectorp)))


(provide 'moder-repeat)
;;; moder-repeat.el ends here
