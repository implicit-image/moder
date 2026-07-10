;;; moder-keymap.el --- Default keybindings for Moder  -*- lexical-binding: t; -*-

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
;; Default keybindings.

;;; Code:

(require 'moder-var)

(declare-function moder-describe-key "moder-command")
(declare-function moder-end-or-call-kmacro "moder-command")
(declare-function moder-end-kmacro "moder-command")

(defvar moder-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [remap describe-key] #'moder-describe-key)
    keymap)
  "Global keymap for Moder.")

(defvar moder-insert-state-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap [escape] 'moder-insert-exit)
    (define-key keymap [remap kmacro-end-or-call-macro] #'moder-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'moder-end-kmacro)
    keymap)
  "Keymap for Moder insert state.")

(defvar moder-numeric-argument-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "1") 'digit-argument)
    (define-key keymap (kbd "2") 'digit-argument)
    (define-key keymap (kbd "3") 'digit-argument)
    (define-key keymap (kbd "4") 'digit-argument)
    (define-key keymap (kbd "5") 'digit-argument)
    (define-key keymap (kbd "6") 'digit-argument)
    (define-key keymap (kbd "7") 'digit-argument)
    (define-key keymap (kbd "8") 'digit-argument)
    (define-key keymap (kbd "9") 'digit-argument)
    (define-key keymap (kbd "0") 'digit-argument)
    keymap))

(defvar moder-normal-state-keymap
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "SPC") 'moder-keypad)
    (define-key keymap [remap kmacro-end-or-call-macro] #'moder-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'moder-end-kmacro)
    keymap)
  "Keymap for Moder normal state.")

(defvar moder-motion-state-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [escape] 'moder-last-buffer)
    (define-key keymap (kbd "SPC") 'moder-keypad)
    keymap)
  "Keymap for Moder motion state.")

(defvar moder-keypad-state-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap kmacro-end-or-call-macro] #'moder-end-or-call-kmacro)
    (define-key map [remap kmacro-end-macro] #'moder-end-kmacro)
    (define-key map (kbd "DEL") 'moder-keypad-undo)
    (define-key map (kbd "<backspace>") 'moder-keypad-undo)
    (define-key map (kbd "<escape>") 'moder-keypad-quit)
    (define-key map [remap keyboard-quit] 'moder-keypad-quit)
    map)
  "Keymap for Moder keypad state.")

(defvar moder-beacon-state-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map moder-normal-state-keymap)
    (suppress-keymap map t)

    ;; kmacros
    (define-key map [remap moder-insert] 'moder-beacon-insert)
    (define-key map [remap moder-append] 'moder-beacon-append)
    (define-key map [remap moder-change] 'moder-beacon-change)
    (define-key map [remap moder-change-save] 'moder-beacon-change-save)
    (define-key map [remap moder-replace] 'moder-beacon-replace)
    (define-key map [remap moder-kill] 'moder-beacon-kill-delete)

    (define-key map [remap kmacro-end-or-call-macro] 'moder-beacon-apply-kmacro)
    (define-key map [remap kmacro-start-macro-or-insert-counter] 'moder-beacon-start)
    (define-key map [remap kmacro-start-macro] 'moder-beacon-start)
    (define-key map [remap moder-end-or-call-kmacro] 'moder-beacon-apply-kmacro)
    (define-key map [remap moder-end-kmacro] 'moder-beacon-apply-kmacro)

    (define-key map [remap moder-open-above] 'moder-beacon-open-above)
    (define-key map [remap moder-open-below] 'moder-beacon-open-below)

    (define-key map [remap moder-save] 'moder-beacon-save)
    ;; noops
    (define-key map [remap moder-delete] 'moder-noop)
    (define-key map [remap moder-C-d] 'moder-noop)
    (define-key map [remap moder-C-k] 'moder-noop)
    (define-key map [remap moder-insert-exit] 'moder-noop)
    (define-key map [remap moder-save-append] 'moder-noop)
    (define-key map [remap moder-last-buffer] 'moder-noop)
    (define-key map [remap moder-swap-grab] 'moder-noop)
    (define-key map [remap moder-sync-grab] 'moder-noop)
    map)
  "Keymap for Moder cursor state.")

(defvar moder-keymap-alist
  `((insert . ,moder-insert-state-keymap)
    (normal . ,moder-normal-state-keymap)
    (keypad . ,moder-keypad-state-keymap)
    (motion . ,moder-motion-state-keymap)
    (beacon . ,moder-beacon-state-keymap)
    (leader . ,mode-specific-map))
  "Alist of symbols of state names to keymaps.")

(provide 'moder-keymap)
;;; moder-keymap.el ends here
