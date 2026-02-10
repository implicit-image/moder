;;; moder-core.el --- Mode definitions for Moder  -*- lexical-binding: t; -*-

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

;;; Modes definition in Moder.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'moder-util)
(require 'moder-command)
(require 'moder-keypad)
(require 'moder-var)
(require 'moder-esc)
(require 'moder-shims)
(require 'moder-beacon)
(require 'moder-helpers)

(moder-define-state insert
                    "Moder INSERT state minor mode."
                    :lighter " [I]"
                    :keymap moder-insert-state-keymap
                    :face moder-insert-cursor
                    (if moder-insert-mode
                        (run-hooks 'moder-insert-enter-hook)
                      (when (and moder--insert-pos
                                 (not (= (point) moder--insert-pos)))
                        (thread-first
                          (moder--make-selection '(select . transient) moder--insert-pos (point))
                          (moder--select moder--insert-activate-mark)))
                      (run-hooks 'moder-insert-exit-hook)
                      (setq-local moder--insert-pos nil
                                  moder--insert-activate-mark nil)))

(moder-define-state normal
                    "Moder NORMAL state minor mode."
                    :lighter " [N]"
                    :keymap moder-normal-state-keymap
                    :face moder-normal-cursor)

(moder-define-state motion
                    "Moder MOTION state minor mode."
                    :lighter " [M]"
                    :keymap moder-motion-state-keymap
                    :face moder-motion-cursor)

(moder-define-state keypad
                    "Moder KEYPAD state minor mode."
                    :lighter " [K]"
                    :face moder-keypad-cursor
                    (when moder-keypad-mode
                      (setq moder--prefix-arg current-prefix-arg
                            moder--keypad-keymap-description-activated nil
                            moder--keypad-base-keymap nil
                            moder--use-literal nil
                            moder--use-meta nil
                            moder--use-both nil)))

(moder-define-state beacon
                    "Moder BEACON state minor mode."
                    :lighter " [B]"
                    :keymap moder-beacon-state-keymap
                    :face moder-beacon-cursor
                    (if moder-beacon-mode
                        (progn
                          (setq moder--beacon-backup-hl-line (bound-and-true-p hl-line-mode)
                                moder--beacon-defining-kbd-macro nil)
                          (hl-line-mode -1))
                      (when moder--beacon-backup-hl-line
                        (hl-line-mode 1))))

;;;###autoload
(define-minor-mode moder-mode
  "Moder minor mode.

This minor mode is used by moder-global-mode, should not be enabled directly."
  :init-value nil
  :interactive nil
  :global nil
  :keymap moder-keymap
  (if moder-mode
      (moder--enable)
    (moder--disable)))

;;;###autoload
(defun moder-indicator ()
  "Indicator showing current mode."
  (or moder--indicator (moder--update-indicator)))

;;;###autoload
(define-global-minor-mode moder-global-mode moder-mode
  (lambda ()
    (unless (minibufferp)
      (moder-mode 1)))
  :group 'moder
  (if moder-mode
      (moder--global-enable)
    (moder--global-disable)))

(defun moder--enable ()
  "Enable Moder.

This function will switch to the proper state for current major
mode. Firstly, the variable `moder-mode-state-list' will be used.
If current major mode derived from any mode from the list,
specified state will be used.  When no result is found, give a
test on the commands bound to the keys a-z. If any of the command
names contains \"self-insert\", then NORMAL state will be used.
Otherwise, MOTION state will be used.

Note: When this function is called, NORMAL state is already
enabled.  NORMAL state is enabled globally when
`moder-global-mode' is used, because in `fundamental-mode',
there's no chance for moder to call an init function."
  (let ((state (moder--mode-get-state)))
    (moder--disable-current-state)
    (moder--switch-state state t)))

(defun moder--disable ()
  "Disable Moder."
  (mapc (lambda (state-mode) (funcall (cdr state-mode) -1)) moder-state-mode-alist)
  (moder--beacon-remove-overlays)
  (when (secondary-selection-exist-p)
    (moder--cancel-second-selection)))

(defun moder--enable-theme-advice (theme)
  "Prepare face if the THEME to enable is `user'."
  (when (eq theme 'user)
    (moder--prepare-face)))

(defun moder--global-enable ()
  "Enable moder globally."
  (setq-default moder-normal-mode t)
  (moder--init-buffers)
  (add-hook 'window-state-change-functions #'moder--on-window-state-change)
  (add-hook 'minibuffer-setup-hook #'moder--minibuffer-setup)
  (add-hook 'pre-command-hook 'moder--highlight-pre-command)
  (add-hook 'post-command-hook 'moder--maybe-toggle-beacon-state)
  (add-hook 'suspend-hook 'moder--on-exit)
  (add-hook 'suspend-resume-hook 'moder--update-cursor)
  (add-hook 'kill-emacs-hook 'moder--on-exit)
  (add-hook 'desktop-after-read-hook 'moder--init-buffers)

  (moder--enable-shims)
  ;; moder-esc-mode fix ESC in TUI
  (moder-esc-mode 1)
  ;; raise Moder keymap priority
  (add-to-ordered-list 'emulation-mode-map-alists
                       `((moder-motion-mode . ,moder-motion-state-keymap)))
  (add-to-ordered-list 'emulation-mode-map-alists
                       `((moder-normal-mode . ,moder-normal-state-keymap)))
  (add-to-ordered-list 'emulation-mode-map-alists
                       `((moder-beacon-mode . ,moder-beacon-state-keymap)))
  (when moder-use-cursor-position-hack
    (setq redisplay-highlight-region-function #'moder--redisplay-highlight-region-function)
    (setq redisplay-unhighlight-region-function #'moder--redisplay-unhighlight-region-function))
  (moder--prepare-face)
  (advice-add 'enable-theme :after 'moder--enable-theme-advice))

(defun moder--global-disable ()
  "Disable Moder globally."
  (setq-default moder-normal-mode nil)
  (remove-hook 'window-state-change-functions #'moder--on-window-state-change)
  (remove-hook 'minibuffer-setup-hook #'moder--minibuffer-setup)
  (remove-hook 'pre-command-hook 'moder--highlight-pre-command)
  (remove-hook 'post-command-hook 'moder--maybe-toggle-beacon-state)
  (remove-hook 'suspend-hook 'moder--on-exit)
  (remove-hook 'suspend-resume-hook 'moder--update-cursor)
  (remove-hook 'kill-emacs-hook 'moder--on-exit)
  (remove-hook 'desktop-after-read-hook 'moder--init-buffers)
  (moder--disable-shims)
  (moder--remove-modeline-indicator)
  (when moder-use-cursor-position-hack
    (setq redisplay-highlight-region-function moder--backup-redisplay-highlight-region-function)
    (setq redisplay-unhighlight-region-function moder--backup-redisplay-unhighlight-region-function))
  (moder-esc-mode -1)
  (advice-remove 'enable-theme 'moder--enable-theme-advice))

(provide 'moder-core)
;;; moder-core.el ends here
