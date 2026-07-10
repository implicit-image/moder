;;; moder-helpers.el --- Moder helpers for customization  -*- lexical-binding: t; -*-

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
;;
;; Define custom keys in a state with function `moder-define-keys'.
;; Define custom keys in normal map with function `moder-normal-define-key'.
;; Define custom keys in global leader map with function `moder-leader-define-key'.
;; Define custom keys in leader map for specific mode with function `moder-leader-define-mode-key'.
;; Define a custom state with the macro `moder-define-state'
;;; Code:

(require 'cl-lib)

(require 'moder-util)
(require 'moder-var)
(require 'moder-keymap)

(defun moder-intern (name suffix &optional two-dashes prefix)
  "Convert a string into a moder symbol. Macro helper.
Concat the string PREFIX or \"moder\" if PREFIX is null, either
one or two hyphens based on TWO-DASHES, the string NAME, and the
string SUFFIX. Then, convert this string into a symbol."
  (intern (concat (if prefix prefix "moder") (if two-dashes "--" "-")
                  name suffix)))

(defun moder-define-keys (state &rest keybinds)
  "Define KEYBINDS in STATE.

Example usage:
  (moder-define-keys
    ;; state
    \\='normal

    ;; bind to a command
    \\='(\"a\" . moder-append)

    ;; bind to a keymap
    (cons \"x\" ctl-x-map)

    ;; bind to a keybinding which holds a keymap
    \\='(\"c\" . \"C-c\")

    ;; bind to a keybinding which holds a command
    \\='(\"q\" . \"C-x C-q\"))"
  (declare (indent 1))
  (let ((map (alist-get state moder-keymap-alist)))
    (pcase-dolist (`(,key . ,def) keybinds)
      (define-key map (kbd key) (moder--parse-def def)))))

(defun moder-normal-define-key (&rest keybinds)
  "Define key for NORMAL state with KEYBINDS.

Example usage:
  (moder-normal-define-key
    ;; bind to a command
    \\='(\"a\" . moder-append)

    ;; bind to a keymap
    (cons \"x\" ctl-x-map)

    ;; bind to a keybinding which holds a keymap
    \\='(\"c\" . \"C-c\")

    ;; bind to a keybinding which holds a command
    \\='(\"q\" . \"C-x C-q\"))"
  (apply #'moder-define-keys 'normal keybinds))

(defun moder-leader-define-key (&rest keybinds)
  "Define key in leader keymap with KEYBINDS.

Moder use `mode-specific-map' as leader keymap.
Usually, the command on C-c <key> can be called in Moder via SPC <key>.

Thus, users should not add a dispatching keybinding like (\"<key>\" . \"C-c <key>\")
with this helper, it will result in recursive calls.

Check `moder-normal-define-key' for usages."
  (apply #'moder-define-keys 'leader keybinds))

(defun moder-motion-define-key (&rest keybinds)
  "Define key for MOTION state.

Check `moder-normal-define-key' for usages."
  (apply #'moder-define-keys 'motion keybinds))

(defalias 'moder-motion-overwrite-define-key 'moder-motion-define-key)
(make-obsolete 'moder-motion-overwrite-define-key 'moder-motion-define-key "1.6.0")

(defun moder-setup-line-number ()
  (add-hook 'display-line-numbers-mode-hook #'moder--toggle-relative-line-number)
  (add-hook 'moder-insert-mode-hook #'moder--toggle-relative-line-number))

(defun moder-setup-indicator ()
  "Setup indicator appending the return of function
`moder-indicator' to the modeline.

This function should be called after you setup other parts of the mode-line
 and will work well for most cases.

If this function is not enough for your requirements,
use `moder-indicator' to get the raw text for indicator
and put it anywhere you want."
  (unless (cl-find '(:eval (moder-indicator)) mode-line-format :test 'equal)
    (setq-default mode-line-format (append '((:eval (moder-indicator))) mode-line-format))))

(defun moder--define-state-minor-mode (name
                                       init-value
                                       description
                                       keymap
                                       lighter
                                       form)
  "Generate a minor mode definition with name moder-NAME-mode,
DESCRIPTION and LIGHTER."
  `(define-minor-mode ,(moder-intern name "-mode")
     ,description
     :init-value ,init-value
     :lighter ,lighter
     :keymap ,keymap
     (if (not ,(moder-intern name "-mode"))
         (setq-local moder--current-state nil)
       (moder--disable-current-state)
       (setq-local moder--current-state ',(intern name))
       (moder-update-display))
     ,form))

(defun moder--define-state-active-p (name)
  "Generate a predicate function to check if moder-NAME-mode is
currently active. Function is named moder-NAME-mode-p."
  `(defun ,(moder-intern name "-mode-p") ()
     ,(concat "Whether " name " mode is enabled.\n"
              "Generated by moder-define-state-active-p")
     (bound-and-true-p ,(moder-intern name "-mode"))))

(defun moder--define-state-cursor-type (name)
  "Generate a cursor type moder-cursor-type-NAME."
  `(defvar ,(moder-intern name nil nil "moder-cursor-type")
     moder-cursor-type-default))

(defun moder--define-state-cursor-function (name &optional face)
  `(defun ,(moder-intern name nil nil "moder--update-cursor") ()
     (moder--set-cursor-type ,(moder-intern name nil nil "moder-cursor-type"))
     (moder--set-cursor-color ',(if face face 'moder-unknown-cursor))))

(defun moder-register-state (name mode activep cursorf &optional keymap)
  "Register a custom state with symbol NAME and symbol MODE
associated with it. ACTIVEP is a function that returns t if the
state is active, nil otherwise. CURSORF is a function that
updates the cursor when the state is entered. For help with
making a working CURSORF, check the variable
moder-update-cursor-functions-alist and the utility functions
moder--set-cursor-type and moder--set-cursor-color."
  (add-to-list 'moder-state-mode-alist `(,name . ,mode))
  (add-to-list 'moder-replace-state-name-list
               `(,name . ,(upcase (symbol-name name))))
  (add-to-list 'moder-update-cursor-functions-alist
               `(,activep . ,cursorf))
  (when keymap
    (add-to-list 'moder-keymap-alist `(,name . ,keymap))))

;;;###autoload
(defmacro moder-define-state (name-sym
                              description
                              &rest body)
  "Define a custom moder state.

The state will be called NAME-SYM, and have description
DESCRIPTION. Following these two arguments, pairs of keywords and
values should be passed, similarly to define-minor-mode syntax.

Recognized keywords:
:keymap - the keymap to use for the state
:lighter - the text to display in the mode line while state is active
:face - custom cursor face

The last argument is an optional lisp form that will be run when the minor
mode turns on AND off. If you want to hook into only the turn-on event,
check whether (moder-NAME-SYM-mode) is true.

Example usage:
(moder-define-state mystate
  \"My moder state\"
  :lighter \" [M]\"
  :keymap \\='my-keymap
  (message \"toggled state\"))

Also see moder-register-state, which is used internally by this
function, if you want more control over defining your state. This
is more helpful if you already have a keymap and defined minor
mode that you only need to integrate with moder.

This function produces several items:
1. moder-NAME-mode: a minor mode for the state. This is the main entry point.
2. moder-NAME-mode-p: a predicate for whether the state is active.
3. moder-cursor-type-NAME: a variable for the cursor type for the state.
4. moder--update-cursor-NAME: a function that sets the cursor type to 3.
 and face FACE or \\='moder-unknown cursor if FACE is nil."
  (declare (indent 1))
  (let ((name       (symbol-name name-sym))
        (init-value (plist-get body :init-value))
        (keymap     (plist-get body :keymap))
        (lighter    (plist-get body :lighter))
        (face       (plist-get body :face))
        (form       (unless (cl-evenp (length body))
                      (car (last body)))))
    `(progn
       ,(moder--define-state-active-p name)
       ,(moder--define-state-minor-mode name init-value description keymap lighter form)
       ,(moder--define-state-cursor-type name)
       ,(moder--define-state-cursor-function name face)
       (moder-register-state ',(intern name) ',(moder-intern name "-mode")
                             ',(moder-intern name "-mode-p")
                             #',(moder-intern name nil nil
                                              "moder--update-cursor")
                             ,keymap))))

(defun moder--is-self-insertp (cmd)
  (and (symbolp cmd)
       (string-match-p "\\`.*self-insert.*\\'"
                       (symbol-name cmd))))

(defun moder--mode-guess-state ()
  "Get initial state for current major mode.
If any of the keys a-z are bound to self insert, then we should
probably start in normal mode, otherwise we start in motion."
  (let ((state moder--current-state))
    (moder--disable-current-state)
    (let* ((letters (split-string "abcdefghijklmnopqrstuvwxyz" "" t))
           (bindings (mapcar #'key-binding letters))
           (any-self-insert (cl-some #'moder--is-self-insertp bindings)))
      (moder--switch-state state t)
      (if any-self-insert
          'normal
        'motion))))

(defun moder--mode-get-state (&optional mode)
  "Get initial state for MODE or current major mode if and only if
MODE is nil."
  (let* ((mode (if mode mode major-mode))
         (parent-mode (get mode 'derived-mode-parent))
         (state (alist-get mode moder-mode-state-list)))
    (cond
     (state state)
     (parent-mode (moder--mode-get-state parent-mode))
     (t (moder--mode-guess-state)))))

(provide 'moder-helpers)
;;; moder-helpers.el ends here
