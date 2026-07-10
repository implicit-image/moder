;;; moder-exec.el --- interactive command execution -*- lexical-binding: t -*-

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

(require 'moder-var)
(require 'cl-lib)

(defvar moder-exec-shell-command-prefix ?!)

(defvar moder-exec-async-shell-command-prefix ?&)

(defvar moder--exec-command-alist nil)

;; (defmacro define-moder-exec-command (prefix component-list &rest body)
;;   (let ((fn-name (intern (format "moder--exec-eval-command-%s" prefix)))
;;         (arglist ))
;;   `(progn
;;      (defun ,fn-name ,arglist
;;        ,@body)
;;      ())))

(defun moder--exec-minibuffer-exit ()
  (remove-hook 'after-change-functions #'moder--exec-after-change t)
  (remove-hook 'minibuffer-exit-hook #'moder--exec-minibuffer-exit t))

(defun moder--exec-after-change (beg end len)
  (when (minibufferp (current-buffer))
    (let ((contents (minibuffer-contents))
          (inhibit-message t))
      (message "changes: %S %S %S (%s)" beg end len contents))))

(defun moder--exec-command-category (expr)
  (let ((first (aref expr 0)))
    (cond
     ((eq first moder-exec-shell-command-prefix) 'shell-command)
     ((eq first moder-exec-async-shell-command-prefix) 'async-shell-command))))

(defun moder--exec-completion-type (expr)
  (let ((split (split-string expr "[ \t]+")))
    (cl-case ())))

(defun moder--exec-narrow-to-content ()
  (when (minibufferp)
    (narrow-to-region (1+ (minibuffer-prompt-end)) (point-max))))

(defun moder--exec-minibuffer-parse-contents ()
  (when (minibufferp)
    (save-restriction
      (save-mark-and-excursion
        ()))
    (let ((contents ())))))

(defun moder--exec-capf ()
  (save-restriction
    (save-mark-and-excursion
      (cl-case (moder--exec-command-category (minibuffer-contents))
        ((shell-command async-shell-command)
         (when t
           (moder--exec-narrow-to-content)
           (shell-command-completion)))))))

(defun moder--exec-setup-hooks ()
  (add-hook 'minibuffer-exit-hook #'moder--exec-minibuffer-exit nil t)
  (add-hook 'after-change-functions #'moder--exec-after-change nil t)
  (add-hook 'completion-at-point-functions #'moder--exec-capf nil t))

(defun moder--exec-eval-cmd (cmd)
  (message "eval %S" cmd))

(defun moder-exec ()
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (message "setting up `moder-exec'")
        (moder--exec-setup-hooks))
    (let ((cmd (read-string ":")))
      (moder--exec-eval-cmd cmd))))

(provide 'moder-exec)
;;; moder-exec.el ends here
