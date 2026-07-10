;;; moder-shims.el --- Make Moder play well with other packages.  -*- lexical-binding: t; -*-

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
;; The file contains all the shim code we need to make moder
;; work with other packages.

;;; Code:

(require 'moder-var)
(require 'moder-command)
(require 'delsel)

(declare-function moder-normal-mode "moder")
(declare-function moder-motion-mode "moder")
(declare-function moder-insert-exit "moder-command")

(defun moder--switch-to-motion (&rest _ignore)
  "Switch to motion state, used for advice.
Optional argument IGNORE ignored."
  (moder--switch-state 'motion))

(defun moder--switch-to-normal (&rest _ignore)
  "Switch to normal state, used for advice.
Optional argument IGNORE ignored."
  (moder--switch-state 'normal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree

(defvar undo-tree-enable-undo-in-region)

(defun moder--setup-undo-tree (enable)
  "Setup `undo-tree-enable-undo-in-region' for undo-tree.

Command `moder-undo-in-selection' will call undo-tree undo.

Argument ENABLE non-nill means turn on."
  (when enable (setq undo-tree-enable-undo-in-region t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc

(defvar moder--eldoc-setup nil
  "Whether already setup eldoc.")

(defconst moder--eldoc-commands
  '(moder-head
    moder-tail
    moder-left
    moder-right
    moder-prev
    moder-next
    moder-insert
    moder-append)
  "A list of moder commands that trigger eldoc.")

(defun moder--setup-eldoc (enable)
  "Setup commands that trigger eldoc.

Basically, all navigation commands should trigger eldoc.
Argument ENABLE non-nill means turn on."
  (setq moder--eldoc-setup enable)
  (if enable
      (apply #'eldoc-add-command moder--eldoc-commands)
    (apply #'eldoc-remove-command moder--eldoc-commands)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company

(defvar moder--company-setup nil
  "Whether already setup company.")

(declare-function company--active-p "company")
(declare-function company-abort "company")

(defvar company-candidates)

(defun moder--company-maybe-abort-advice ()
  "Adviced for `moder-insert-exit'."
  (when company-candidates
    (company-abort)))

(defun moder--setup-company (enable)
  "Setup for company.
Argument ENABLE non-nil means turn on."
  (setq moder--company-setup enable)
  (if enable
      (add-hook 'moder-insert-exit-hook #'moder--company-maybe-abort-advice)
    (remove-hook 'moder-insert-exit-hook #'moder--company-maybe-abort-advice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; corfu

(declare-function corfu-quit "corfu")

(defvar moder--corfu-setup nil
  "Whether already setup corfu.")

(defun moder--corfu-maybe-abort-advice ()
  "Adviced for `moder-insert-exit'."
  (when (bound-and-true-p corfu-mode) (corfu-quit)))

(defun moder--setup-corfu (enable)
  "Setup for corfu.
Argument ENABLE non-nil means turn on."
  (setq moder--corfu-setup enable)
  (if enable
      (add-hook 'moder-insert-exit-hook #'moder--corfu-maybe-abort-advice)
    (remove-hook 'moder-insert-exit-hook #'moder--corfu-maybe-abort-advice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; repeat-map

(defvar moder--diff-hl-setup nil
  "Whether already setup diff-hl.")

(defun moder--setup-diff-hl (enable)
  "Setup diff-hl."
  (if enable
      (progn
        (advice-add 'diff-hl-show-hunk-inline-popup :before 'moder--switch-to-motion)
        (advice-add 'diff-hl-show-hunk-posframe :before 'moder--switch-to-motion)
        (advice-add 'diff-hl-show-hunk-hide :after 'moder--switch-to-normal))
    (advice-remove 'diff-hl-show-hunk-inline-popup 'moder--switch-to-motion)
    (advice-remove 'diff-hl-show-hunk-posframe 'moder--switch-to-motion)
    (advice-remove 'diff-hl-show-hunk-hide 'moder--switch-to-normal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep

(defvar moder--wgrep-setup nil
  "Whether already setup wgrep.")

(defun moder--setup-wgrep (enable)
  "Setup wgrep.

We use advice here because wgrep doesn't call its hooks.
Argument ENABLE non-nil means turn on."
  (setq moder--wgrep-setup enable)
  (if enable
      (progn
        (advice-add 'wgrep-change-to-wgrep-mode :after #'moder--switch-to-normal)
        (advice-add 'wgrep-exit :after #'moder--switch-to-motion)
        (advice-add 'wgrep-finish-edit :after #'moder--switch-to-motion)
        (advice-add 'wgrep-abort-changes :after #'moder--switch-to-motion)
        (advice-add 'wgrep-save-all-buffers :after #'moder--switch-to-motion))
    (advice-remove 'wgrep-change-to-wgrep-mode #'moder--switch-to-normal)
    (advice-remove 'wgrep-exit #'moder--switch-to-motion)
    (advice-remove 'wgrep-abort-changes #'moder--switch-to-motion)
    (advice-remove 'wgrep-finish-edit #'moder--switch-to-motion)
    (advice-remove 'wgrep-save-all-buffers #'moder--switch-to-motion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grep-edit


(defvar moder--grep-edit-setup nil
  "Wheter already setup grep-edit.")

(defvar grep-edit-mode-hook)

(declare-function grep-edit-save-changes "grep")

(defun moder--setup-grep-edit (enable)
  "Setup grep-edit.

Argument ENABLE non-nil means turn on."
  (if enable
      (progn
        (add-hook 'grep-edit-mode-hook #'moder--switch-to-normal)
        (advice-add #'grep-edit-save-changes :after #'moder--switch-to-motion))
    (remove-hook 'grep-edit-mode-hook #'moder--switch-to-normal)
    (advice-remove 'grep-edit-save-changes #'moder--switch-to-motion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wdired

(defvar moder--wdired-setup nil
  "Whether already setup wdired.")

(defvar wdired-mode-hook)

(declare-function wdired-exit "wdired")
(declare-function wdired-finish-edit "wdired")
(declare-function wdired-abort-changes "wdired")

(defun moder--setup-wdired (enable)
  "Setup wdired.

Argument ENABLE non-nil means turn on."
  (setq moder--wdired-setup enable)
  (if enable
      (progn
        (add-hook 'wdired-mode-hook #'moder--switch-to-normal)
        (advice-add #'wdired-exit :after #'moder--switch-to-motion)
        (advice-add #'wdired-abort-changes :after #'moder--switch-to-motion)
        (advice-add #'wdired-finish-edit :after #'moder--switch-to-motion))
    (remove-hook 'wdired-mode-hook #'moder--switch-to-normal)
    (advice-remove #'wdired-exit #'moder--switch-to-motion)
    (advice-remove #'wdired-abort-changes #'moder--switch-to-motion)
    (advice-remove #'wdired-finish-edit #'moder--switch-to-motion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rectangle-mark-mode

(defvar moder--rectangle-mark-setup nil
  "Whether already setup rectangle-mark.")

(defun moder--rectangle-mark-init ()
  "Patch the moder selection type to prevent it from being cancelled."
  (when (bound-and-true-p rectangle-mark-mode)
    (setq moder--selection
          '((expand . char) 0 0))))

(defun moder--setup-rectangle-mark (enable)
  "Setup `rectangle-mark-mode'.
Argument ENABLE non-nil means turn on."
  (setq moder--rectangle-mark-setup enable)
  (if enable
      (add-hook 'rectangle-mark-mode-hook 'moder--rectangle-mark-init)
    (remove-hook 'rectangle-mark-mode-hook 'moder--rectangle-mark-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; edebug

(defvar moder--edebug-setup nil)

(defun moder--edebug-hook-function ()
  "Switch moder state when entering/leaving edebug."
  (if (bound-and-true-p edebug-mode)
      (moder--switch-to-motion)
    (moder--switch-to-normal)))

(defun moder--setup-edebug (enable)
  "Setup edebug.
Argument ENABLE non-nil means turn on."
  (setq moder--edebug-setup enable)
  (if enable
      (add-hook 'edebug-mode-hook 'moder--edebug-hook-function)
    (remove-hook 'edebug-mode-hook 'moder--edebug-hook-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit

(defvar moder--magit-setup nil)

(defun moder--magit-blame-hook-function ()
  "Switch moder state when entering/leaving `magit-blame-read-only-mode'."
  (if (bound-and-true-p magit-blame-read-only-mode)
      (moder--switch-to-motion)
    (moder--switch-to-normal)))

(defun moder--setup-magit (enable)
  "Setup magit.
Argument ENABLE non-nil means turn on."
  (setq moder--magit-setup enable)
  (if enable
      (add-hook 'magit-blame-mode-hook 'moder--magit-blame-hook-function)
    (remove-hook 'magit-blame-mode-hook 'moder--magit-blame-hook-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cider (debug)

(defvar moder--cider-setup nil)

(defun moder--cider-debug-hook-function ()
  "Switch moder state when entering/leaving cider debug."
  (if (bound-and-true-p cider--debug-mode)
      (moder--switch-to-motion)
    (moder--switch-to-normal)))

(defun moder--setup-cider (enable)
  "Setup cider.
Argument ENABLE non-nil means turn on."
  (setq moder--cider-setup enable)
  (if enable
      (add-hook 'cider--debug-mode-hook 'moder--cider-debug-hook-function)
    (remove-hook 'cider--debug-mode-hook 'moder--cider-debug-hook-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sly (db)

(defvar moder--sly-setup nil)

(defun moder--sly-debug-hook-function ()
  "Switch moder state when entering/leaving sly-db-mode."
  (if (bound-and-true-p sly-db-mode-hook)
      (moder--switch-to-motion)
    (moder--switch-to-motion)))

(defun moder--setup-sly (enable)
  "Setup sly.
Argument ENABLE non-nil means turn on."
  (setq moder--sly-setup enable)
  (if enable
      (add-hook 'sly-db-hook 'moder--sly-debug-hook-function)
    (remove-hook 'sly-db-hook 'moder--sly-debug-hook-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macrostep

(defvar macrostep-overlays)
(defvar macrostep-mode)

(defvar moder--macrostep-setup nil)
(defvar moder--macrostep-setup-previous-state nil)

(defun moder--macrostep-inside-overlay-p ()
  "Return whether point is inside a `macrostep-mode' overlay."
  (seq-some (let ((pt (point)))
              (lambda (ov)
                (and (<= (overlay-start ov) pt)
                     (< pt (overlay-end ov)))))
            macrostep-overlays))

(defun moder--macrostep-post-command-function ()
  "Function to run in `post-commmand-hook' when `macrostep-mode' is enabled.

`macrostep-mode' uses a local keymap for the overlay showing the
expansion.  Switch to Motion state when we enter the overlay and
try to switch back to the previous state when leaving it."
  (if (moder--macrostep-inside-overlay-p)
      ;; The overlay is not editable, so the `macrostep-mode' commands are
      ;; likely more important than the Beacon-state commands and possibly more
      ;; important than any custom-state commands.  It is less important than
      ;; Keypad state.
      (unless (eq moder--current-state 'keypad)
        (moder--switch-to-motion))
    (moder--switch-state moder--macrostep-setup-previous-state)))

(defun moder--macrostep-record-outside-state (state)
  "Record the Moder STATE in most circumstances, so that we can return to it later.

This function receives the STATE to which one switches via `moder--switch-state'
inside `moder-switch-state-hook'.

Record the state if:
- We are outside the overlay and not in Keypad state.
- We are inside the overlay and not in Keypad or Motion state."
  ;; We assume that the user will not try to switch to Motion state for the
  ;; entire buffer while we are already in Motion state while inside an overlay.
  (unless (eq state 'keypad)
    (if (not (moder--macrostep-inside-overlay-p))
        (setq-local moder--macrostep-setup-previous-state state)
      (unless (eq state 'motion)
        (setq-local moder--macrostep-setup-previous-state state)))))

(defun moder--macrostep-hook-function ()
  "Switch Moder state when entering/leaving `macrostep-mode' or its overlays."
  (if macrostep-mode
      (progn
        (setq-local moder--macrostep-setup-previous-state moder--current-state)
        ;; Add to end of `post-command-hook', so that this function is run after
        ;; the check for whether we should switch to Beacon state.
        (add-hook 'post-command-hook #'moder--macrostep-post-command-function 90 t)
        (add-hook 'moder-switch-state-hook #'moder--macrostep-record-outside-state nil t))
    ;; The command `macrostep-collapse' does not seem to trigger
    ;; `post-command-hook', so we switch back manually.
    (moder--switch-state moder--macrostep-setup-previous-state)
    (setq-local moder--macrostep-setup-previous-state nil)
    (remove-hook 'moder-switch-state-hook #'moder--macrostep-record-outside-state t)
    (remove-hook 'post-command-hook #'moder--macrostep-post-command-function t)))

(defun moder--setup-macrostep (enable)
  "Setup macrostep.
Argument ENABLE non-nil means turn on."
  (setq moder--macrostep-setup enable)
  (if enable
      (add-hook 'macrostep-mode-hook 'moder--macrostep-hook-function)
    (remove-hook 'macrostep-mode-hook 'moder--macrostep-hook-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; realgud (debug)

(defvar moder--realgud-setup nil)

(defun moder--realgud-debug-hook-function ()
  "Switch moder state when entering/leaving realgud-short-key-mode."
  (if (bound-and-true-p realgud-short-key-mode)
      (moder--switch-to-motion)
    (moder--switch-to-normal)))

(defun moder--setup-realgud (enable)
  "Setup realgud.
Argument ENABLE non-nil means turn on."
  (setq moder--realgud-setup enable)
  (if enable
      (add-hook 'realgud-short-key-mode-hook 'moder--realgud-debug-hook-function)
    (remove-hook 'realgud-short-key-mode-hook 'moder--realgud-debug-hook-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key

(defvar which-key-mode)
(declare-function which-key--create-buffer-and-show "which-key"
                  (&optional prefix-keys from-keymap filter prefix-title))

(defvar moder--which-key-setup nil)

(defun moder--which-key-describe-keymap ()
  "Use which-key for keypad popup."
  (if which-key-mode
      (setq
       which-key-use-C-h-commands nil
       moder-keypad-describe-keymap-function
       (lambda (keymap)
         (which-key--create-buffer-and-show nil keymap nil (concat moder-keypad-message-prefix (moder--keypad-format-keys))))
       moder-keypad-clear-describe-keymap-function 'which-key--hide-popup)

    (setq moder-keypad-describe-keymap-function 'moder-describe-keymap
          moder-keypad-clear-describe-keymap-function nil
          which-key-use-C-h-commands t)))

(defun moder--setup-which-key (enable)
  "Setup which-key.
Argument ENABLE non-nil means turn on."
  (setq moder--which-key-setup enable)
  (if enable
      (add-hook 'which-key-mode-hook 'moder--which-key-describe-keymap)
    (remove-hook 'which-key-mode-hook 'moder--which-key-describe-keymap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input methods

(defvar moder--input-method-setup nil)

(defun moder--input-method-advice (fnc key)
  "Advice for `quail-input-method'.

Only use the input method in insert mode.
Argument FNC, input method function.
Argument KEY, the current input."
  (funcall (if (and (boundp 'moder-mode) moder-mode (not (moder-insert-mode-p))) #'list fnc) key))

(defun moder--setup-input-method (enable)
  "Setup input-method.
Argument ENABLE non-nil means turn on."
  (setq moder--input-method-setup enable)
  (if enable
      (advice-add 'quail-input-method :around 'moder--input-method-advice)
    (advice-remove 'quail-input-method 'moder--input-method-advice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ddskk

(defvar skk-henkan-mode)

(defvar moder--ddskk-setup nil)
(defun moder--ddskk-skk-previous-candidate-advice (fnc &optional arg)
  (if (and (not (eq skk-henkan-mode 'active))
           (not (eq last-command 'skk-kakutei-henkan))
           last-command-event
           (eq last-command-event
               (seq-first (car (where-is-internal
                                'moder-prev
                                moder-normal-state-keymap)))))
      (forward-line -1)
    (funcall fnc arg)))

(defun moder--setup-ddskk (enable)
  (setq moder--ddskk-setup enable)
  (if enable
      (advice-add 'skk-previous-candidate :around
                  'moder--ddskk-skk-previous-candidate-advice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; polymode

(defvar polymode-move-these-vars-from-old-buffer)

(defvar moder--polymode-setup nil)

(defun moder--setup-polymode (enable)
  "Setup polymode.

Argument ENABLE non-nil means turn on."
  (setq moder--polymode-setup enable)
  (when enable
    (dolist (v '(moder--selection
                 moder--selection-history
                 moder--current-state
                 moder-normal-mode
                 moder-insert-mode
                 moder-keypad-mode
                 moder-beacon-mode
                 moder-motion-mode))
      ;; These vars allow us the select through the polymode chunk
      (add-to-list 'polymode-move-these-vars-from-old-buffer v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eat-eshell

(defvar moder--eat-eshell-setup nil)
(defvar moder--eat-eshell-mode-override nil)

(declare-function eat-eshell-emacs-mode "eat")
(declare-function eat-eshell-semi-char-mode "eat")
(declare-function eat-eshell-char-mode "eat")

(declare-function moder-insert-mode "moder-core")

(defun moder--eat-eshell-mode-override-enable ()
  (setq-local moder--eat-eshell-mode-override t)
  (add-hook 'moder-insert-enter-hook #'eat-eshell-char-mode nil t)
  (add-hook 'moder-insert-exit-hook #'eat-eshell-emacs-mode nil t)
  (if (bound-and-true-p moder-insert-mode)
      (eat-eshell-char-mode)
    (eat-eshell-emacs-mode)))

(defun moder--eat-eshell-mode-override-disable ()
  (setq-local moder--eat-eshell-mode-override nil)
  (remove-hook 'moder-insert-enter-hook #'eat-eshell-char-mode t)
  (remove-hook 'moder-insert-exit-hook #'eat-eshell-emacs-mode t))

(defun moder--setup-eat-eshell (enable)
  (setq moder--eat-eshell-setup enable)
  (if enable
      (progn (add-hook 'eat-eshell-exec-hook #'moder--eat-eshell-mode-override-enable)
             (add-hook 'eat-eshell-exit-hook #'moder--eat-eshell-mode-override-disable)
             (add-hook 'eat-eshell-exit-hook #'moder--update-cursor))

    (remove-hook 'eat-eshell-exec-hook #'moder--eat-eshell-mode-override-enable)
    (remove-hook 'eat-eshell-exit-hook #'moder--eat-eshell-mode-override-disable)
    (remove-hook 'eat-eshell-exit-hook #'moder--update-cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ediff
(defvar moder--ediff-setup nil)

(defun moder--setup-ediff (enable)
  "Setup Ediff.
Argument ENABLE, non-nil means turn on."
  (if enable
      (add-hook 'ediff-mode-hook 'moder-motion-mode)
    (remove-hook 'ediff-mode-hook 'moder-motion-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xref-edit-mode

(defvar moder--xref-setup nil)

(defun moder--setup-xref (enable)
  "Setup `xref-edit-mode'.

Argument ENABLE, non-nil means turn om."
  (setq moder--xref-setup enable)
  (if enable
      (progn
        (advice-add 'xref-change-to-xref-edit-mode :after #'moder--switch-to-normal)
        (advice-add 'xref-edit-save-changes :after #'moder--switch-to-motion))
    (advice-remove 'xref-change-to-xref-edit-mode #'moder--switch-to-normal)
    (advice-remove 'xref-edit-save-changes #'moder--switch-to-motion)))

;; Enable / Disable shims

(defun moder--enable-shims ()
  "Use a bunch of shim setups."
  ;; This lets us start input without canceling selection.
  ;; We will backup `delete-active-region'.
  (setq moder--backup-var-delete-activate-region delete-active-region)
  (setq delete-active-region nil)
  (moder--setup-eldoc t)
  (moder--setup-rectangle-mark t)

  (eval-after-load "macrostep" (lambda () (moder--setup-macrostep t)))
  (eval-after-load "wdired" (lambda () (moder--setup-wdired t)))
  (eval-after-load "edebug" (lambda () (moder--setup-edebug t)))
  (eval-after-load "magit" (lambda () (moder--setup-magit t)))
  (eval-after-load "wgrep" (lambda () (moder--setup-wgrep t)))
  (eval-after-load "grep" (lambda () (moder--setup-grep-edit t)))
  (eval-after-load "company" (lambda () (moder--setup-company t)))
  (eval-after-load "corfu" (lambda () (moder--setup-corfu t)))
  (eval-after-load "polymode" (lambda () (moder--setup-polymode t)))
  (eval-after-load "cider" (lambda () (moder--setup-cider t)))
  (eval-after-load "sly" (lambda () (moder--setup-sly t)))
  (eval-after-load "realgud" (lambda () (moder--setup-realgud t)))
  (eval-after-load "which-key" (lambda () (moder--setup-which-key t)))
  (eval-after-load "undo-tree" (lambda () (moder--setup-undo-tree t)))
  (eval-after-load "diff-hl" (lambda () (moder--setup-diff-hl t)))
  (eval-after-load "quail" (lambda () (moder--setup-input-method t)))
  (eval-after-load "skk" (lambda () (moder--setup-ddskk t)))
  (eval-after-load "eat" (lambda () (moder--setup-eat-eshell t)))
  (eval-after-load "ediff" (lambda () (moder--setup-ediff t)))
  (eval-after-load "xref" (lambda () (moder--setup-xref-edit t))))

(defun moder--disable-shims ()
  "Remove shim setups."
  (setq delete-active-region moder--backup-var-delete-activate-region)
  (when moder--macrostep-setup (moder--setup-macrostep nil))
  (when moder--eldoc-setup (moder--setup-eldoc nil))
  (when moder--rectangle-mark-setup (moder--setup-rectangle-mark nil))
  (when moder--wdired-setup (moder--setup-wdired nil))
  (when moder--edebug-setup (moder--setup-edebug nil))
  (when moder--magit-setup (moder--setup-magit nil))
  (when moder--company-setup (moder--setup-company nil))
  (when moder--corfu-setup (moder--setup-corfu nil))
  (when moder--wgrep-setup (moder--setup-wgrep nil))
  (when moder--grep-edit-setup (moder--setup-grep-edit nil))
  (when moder--polymode-setup (moder--setup-polymode nil))
  (when moder--cider-setup (moder--setup-cider nil))
  (when moder--which-key-setup (moder--setup-which-key nil))
  (when moder--diff-hl-setup (moder--setup-diff-hl nil))
  (when moder--input-method-setup (moder--setup-input-method nil))
  (when moder--ddskk-setup (moder--setup-ddskk nil))
  (when moder--eat-eshell-setup (moder--setup-eat-eshell nil))
  (when moder--ediff-setup (moder--setup-ediff nil))
  (when moder--xref-edit-setup (moder--setup-xref-edit nil)))

;;; moder-shims.el ends here
(provide 'moder-shims)
