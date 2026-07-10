;;; moder-commands.el --- Commands in Moder -*- lexical-binding: t -*-

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
(require 'seq)

(require 'moder-var)
(require 'moder-util)
(require 'moder-visual)
(require 'moder-thing)
(require 'moder-beacon)
(require 'moder-keypad)
(require 'moder-selection)
(require 'array)

(defun moder-undo ()
  "Cancel current selection then undo."
  (interactive)
  (when (region-active-p)
    (moder--cancel-selection))
  (moder--execute-kbd-macro moder--kbd-undo))

(defun moder-undo-in-selection ()
  "Cancel undo in current region."
  (interactive)
  (when (region-active-p)
    (moder--execute-kbd-macro moder--kbd-undo)))

(defun moder-pop-selection ()
  (interactive)
  (moder--with-selection-fallback
   (moder--pop-selection)))

(defun moder-pop-all-selection ()
  (interactive)
  (while (moder--pop-selection)))

;;; exchange mark and point

(defun moder-reverse ()
  "Just exchange point and mark.

This command supports `moder-selection-command-fallback'."
  (interactive)
  (moder--with-selection-fallback
   (moder--execute-kbd-macro moder--kbd-exchange-point-and-mark)
   (if (member last-command
               '(moder-visit moder-search moder-mark-symbol moder-mark-word))
       (moder--highlight-regexp-in-buffer (car regexp-search-ring)))))

;;; Buffer

(defun moder-find-ref ()
  "Xref find."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-find-ref))

(defun moder-pop-marker ()
  "Pop marker."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-pop-marker))

;;; Clipboards

(defun moder-clipboard-yank ()
  "Yank system clipboard."
  (interactive)
  (call-interactively #'clipboard-yank))

(defun moder-clipboard-kill ()
  "Kill to system clipboard."
  (interactive)
  (call-interactively #'clipboard-kill-region))

(defun moder-clipboard-save ()
  "Save to system clipboard."
  (interactive)
  (call-interactively #'clipboard-kill-ring-save))

(defun moder-save ()
  "Copy, like command `kill-ring-save'.

This command supports `moder-selection-command-fallback'."
  (interactive)
  (moder--with-selection-fallback
   (let ((select-enable-clipboard moder-use-clipboard))
     (moder--prepare-region-for-kill)
     (moder--execute-kbd-macro moder--kbd-kill-ring-save))))

(defun moder-save-append ()
  "Copy, like command `kill-ring-save' but append to latest kill.

This command supports `moder-selection-command-fallback'."
  (interactive)
  (let ((select-enable-clipboard moder-use-clipboard))
    (moder--prepare-region-for-kill)
    (let ((s (buffer-substring-no-properties (region-beginning) (region-end))))
      (kill-append (moder--prepare-string-for-kill-append s) nil)
      (deactivate-mark t))))

(defun moder-save-empty ()
  "Copy an empty string, can be used with `moder-save-append' or `moder-kill-append'."
  (interactive)
  (kill-new ""))

(defun moder-save-char ()
  "Copy current char."
  (interactive)
  (when (< (point) (point-max))
    (save-mark-and-excursion
      (goto-char (point))
      (push-mark (1+ (point)) t t)
      (moder--execute-kbd-macro moder--kbd-kill-ring-save))))

(defun moder-yank ()
  "Yank."
  (interactive)
  (let ((select-enable-clipboard moder-use-clipboard))
    (moder--execute-kbd-macro moder--kbd-yank)))

(defun moder-yank-pop ()
  "Pop yank."
  (interactive)
  (when (moder--allow-modify-p)
    (moder--execute-kbd-macro moder--kbd-yank-pop)))

;;; Quit

(defun moder-cancel-selection ()
  "Cancel selection.

This command supports `moder-selection-command-fallback'."
  (interactive)
  (moder--with-selection-fallback
   (moder--cancel-selection)))

(defun moder-keyboard-quit ()
  "Keyboard quit."
  (interactive)
  (if (region-active-p)
      (deactivate-mark t)
    (moder--execute-kbd-macro moder--kbd-keyboard-quit)))

(defun moder-quit ()
  "Quit current window or buffer."
  (interactive)
  (if (> (seq-length (window-list (selected-frame))) 1)
      (quit-window)
    (previous-buffer)))

;;; Comment

(defun moder-comment ()
  "Comment region or comment line."
  (interactive)
  (when (moder--allow-modify-p)
    (moder--execute-kbd-macro moder--kbd-comment)))

;;; Delete Operations

(defun moder-kill ()
  "Kill region.

This command supports `moder-selection-command-fallback'."
  (interactive)
  (let ((select-enable-clipboard moder-use-clipboard))
    (when (moder--allow-modify-p)
      (moder--with-selection-fallback
       (cond
        ((equal '(expand . join) (moder--selection-type))
         (delete-indentation nil (region-beginning) (region-end)))
        (t
         (moder--prepare-region-for-kill)
         (moder--execute-kbd-macro moder--kbd-kill-region)))))))

(defun moder-kill-append ()
  "Kill region and append to latest kill.

This command supports `moder-selection-command-fallback'."
  (interactive)
  (let ((select-enable-clipboard moder-use-clipboard))
    (when (moder--allow-modify-p)
      (moder--with-selection-fallback
       (cond
        ((equal '(expand . join) (moder--selection-type))
         (delete-indentation nil (region-beginning) (region-end)))
        (t
         (moder--prepare-region-for-kill)
         (let ((s (buffer-substring-no-properties (region-beginning) (region-end))))
           (moder--delete-region (region-beginning) (region-end))
           (kill-append (moder--prepare-string-for-kill-append s) nil))))))))

(defun moder-C-k ()
  "Run command on C-k."
  (interactive)
  (moder--execute-kbd-macro moder--kbd-kill-line))

(defun moder-kill-whole-line ()
  (interactive)
  (when (moder--allow-modify-p)
    (moder--execute-kbd-macro moder--kbd-kill-whole-line)))

(defun moder-backspace ()
  "Backward delete one char."
  (interactive)
  (when (moder--allow-modify-p)
    (call-interactively #'backward-delete-char)))

(defun moder-C-d ()
  "Run command on C-d."
  (interactive)
  (moder--execute-kbd-macro moder--kbd-delete-char))

(defun moder-backward-kill-word (arg)
  "Kill characters backward until the beginning of a `moder-word-thing'.
With argument ARG, do this that many times."
  (interactive "p")
  (moder-kill-word (- arg)))

(defun moder-kill-word (arg)
  "Kill characters forward until the end of a `moder-word-thing'.
With argument ARG, do this that many times."
  (interactive "p")
  (moder-kill-thing moder-word-thing arg))

(defun moder-backward-kill-symbol (arg)
  "Kill characters backward until the beginning of a `moder-symbol-thing'.
With argument ARG, do this that many times."
  (interactive "p")
  (moder-kill-symbol (- arg)))

(defun moder-kill-symbol (arg)
  "Kill characters forward until the end of a `moder-symbol-thing'.
With argument ARG, do this that many times."
  (interactive "p")
  (moder-kill-thing moder-symbol-thing arg))


(defun moder-kill-thing (thing arg)
  "Kill characters forward until the end of a THING.
With argument ARG, do this that many times."
  (let ((start (point))
        (end (progn (forward-thing thing arg) (point))))
    (condition-case _
        (kill-region start end)
      ((text-read-only buffer-read-only)
       (condition-case err
           (moder--delete-region start end)
         (t (signal (car err) (cdr err))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAGE UP&DOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder-page-up ()
  "Page up."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-scoll-down))

(defun moder-page-down ()
  "Page down."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-scoll-up))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PARENTHESIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder-forward-slurp ()
  "Forward slurp sexp."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-forward-slurp))

(defun moder-backward-slurp ()
  "Backward slurp sexp."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-backward-slurp))

(defun moder-forward-barf ()
  "Forward barf sexp."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-forward-barf))

(defun moder-backward-barf ()
  "Backward barf sexp."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-backward-barf))

(defun moder-raise-sexp ()
  "Raise sexp."
  (interactive)
  (moder--cancel-selection)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (when bounds
      (goto-char (car bounds))))
  (moder--execute-kbd-macro moder--kbd-raise-sexp))

(defun moder-transpose-sexp ()
  "Transpose sexp."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-transpose-sexp))

(defun moder-split-sexp ()
  "Split sexp."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-split-sexp))

(defun moder-join-sexp ()
  "Split sexp."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-join-sexp))

(defun moder-splice-sexp ()
  "Splice sexp."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-splice-sexp))

(defun moder-wrap-round ()
  "Wrap round paren."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-wrap-round))

(defun moder-wrap-square ()
  "Wrap square paren."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-wrap-square))

(defun moder-wrap-curly ()
  "Wrap curly paren."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-wrap-curly))

(defun moder-wrap-string ()
  "Wrap string."
  (interactive)
  (moder--cancel-selection)
  (moder--execute-kbd-macro moder--kbd-wrap-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STATE TOGGLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder-insert-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((moder-keypad-mode-p)
    (moder--exit-keypad-state))
   ((and (moder-insert-mode-p)
         (eq moder--beacon-defining-kbd-macro 'quick))
    (setq moder--beacon-defining-kbd-macro nil)
    (moder-beacon-insert-exit))
   ((moder-insert-mode-p)
    (moder--switch-state 'normal))))

(defun moder-temp-normal ()
  "Switch to navigation-only NORMAL state."
  (interactive)
  (when (moder-motion-mode-p)
    (message "Enter temporary normal mode")
    (setq moder--temp-normal t)
    (moder--switch-state 'normal)))

(defun moder-insert ()
  "Move to the start of selection, switch to INSERT state."
  (interactive)
  (if moder--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (moder--switch-state 'motion))
    (moder--direction-backward)
    (moder--cancel-selection)
    (moder--switch-state 'insert)
    (setq-local moder--insert-pos (point))
    (when moder-select-on-insert
      (setq-local moder--insert-activate-mark t))))

(defun moder-append ()
  "Move to the end of selection, switch to INSERT state."
  (interactive)
  (if moder--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (moder--switch-state 'motion))
    (if (not (region-active-p))
        (when (and moder-use-cursor-position-hack
                   (< (point) (point-max)))
          (forward-char 1))
      (moder--direction-forward)
      (moder--cancel-selection))
    (moder--switch-state 'insert)
    (setq-local moder--insert-pos (point))
    (when moder-select-on-append
      (setq-local moder--insert-activate-mark t))))

(defun moder-open-above (arg)
  "Open a newline above and switch to INSERT state."
  (interactive "p")
  (if moder--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (moder--switch-state 'motion))
    (moder--switch-state 'insert)
    (goto-char (line-beginning-position))
    (save-mark-and-excursion
      (newline arg))
    (indent-according-to-mode)
    (setq-local moder--insert-pos (point))
    (when moder-select-on-open
      (setq-local moder--insert-activate-mark t))))

(defun moder-open-above-visual ()
  "Open a newline above and switch to INSERT state."
  (interactive)
  (if moder--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (moder--switch-state 'motion))
    (moder--switch-state 'insert)
    (goto-char (moder--visual-line-beginning-position))
    (save-mark-and-excursion
      (newline))
    (indent-according-to-mode)
    (setq-local moder--insert-pos (point))
    (when moder-select-on-open
      (setq-local moder--insert-activate-mark t))))

(defun moder-open-below (arg)
  "Open a newline below and switch to INSERT state."
  (interactive "p")
  (if moder--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (moder--switch-state 'motion))
    (moder--switch-state 'insert)
    (goto-char (line-end-position))
    (moder--execute-kbd-macro "RET")
    (setq-local moder--insert-pos (point))
    (when moder-select-on-open
      (setq-local moder--insert-activate-mark t))))

(defun moder-open-below-visual ()
  "Open a newline below and switch to INSERT state."
  (interactive)
  (if moder--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (moder--switch-state 'motion))
    (moder--switch-state 'insert)
    (goto-char (moder--visual-line-end-position))
    (moder--execute-kbd-macro "RET")
    (setq-local moder--insert-pos (point))
    (when moder-select-on-open
      (setq-local moder--insert-activate-mark t))))

(defun moder-change ()
  "Kill current selection and switch to INSERT state.

This command supports `moder-selection-command-fallback'."
  (interactive)
  (when (moder--allow-modify-p)
    (setq this-command #'moder-change)
    (moder--with-selection-fallback
     (moder--delete-region (region-beginning) (region-end))
     (moder--switch-state 'insert)
     (setq-local moder--insert-pos (point))
     (when moder-select-on-change
       (setq-local moder--insert-activate-mark t)))))

(defun moder-change-char ()
  "Delete current char and switch to INSERT state."
  (interactive)
  (when (< (point) (point-max))
    (moder--execute-kbd-macro moder--kbd-delete-char)
    (moder--switch-state 'insert)
    (setq-local moder--insert-pos (point))
    (when moder-select-on-change
      (setq-local moder--insert-activate-mark t))))

(defun moder-change-save ()
  (interactive)
  (let ((select-enable-clipboard moder-use-clipboard))
    (when (and (moder--allow-modify-p) (region-active-p))
      (kill-region (region-beginning) (region-end))
      (moder--switch-state 'insert)
      (setq-local moder--insert-pos (point))
      (when moder-select-on-change
        (setq-local moder--insert-activate-mark t)))))

(defun moder-replace ()
  "Replace current selection with yank.

This command supports `moder-selection-command-fallback'."
  (interactive)
  (moder--with-selection-fallback
   (let ((select-enable-clipboard moder-use-clipboard))
     (when (moder--allow-modify-p)
       (when-let* ((s (string-trim-right (current-kill 0 t) "\n")))
         (moder--delete-region (region-beginning) (region-end))
         (set-marker moder--replace-start-marker (point))
         (moder--insert s))))))

(defun moder-replace-char ()
  "Replace current char with selection."
  (interactive)
  (let ((select-enable-clipboard moder-use-clipboard))
    (when (< (point) (point-max))
      (when-let* ((s (string-trim-right (current-kill 0 t) "\n")))
        (moder--delete-region (point) (1+ (point)))
        (set-marker moder--replace-start-marker (point))
        (moder--insert s)))))

(defun moder-replace-save ()
  (interactive)
  (let ((select-enable-clipboard moder-use-clipboard))
    (when (moder--allow-modify-p)
      (when-let* ((curr (pop kill-ring-yank-pointer)))
        (let ((s (string-trim-right curr "\n")))
          (setq kill-ring kill-ring-yank-pointer)
          (if (region-active-p)
              (let ((old (save-mark-and-excursion
                           (moder--prepare-region-for-kill)
                           (buffer-substring-no-properties (region-beginning) (region-end)))))
                (progn
                  (moder--delete-region (region-beginning) (region-end))
                  (set-marker moder--replace-start-marker (point))
                  (moder--insert s)
                  (kill-new old)))
            (set-marker moder--replace-start-marker (point))
            (moder--insert s)))))))

(defun moder-replace-pop ()
  "Like `yank-pop', but for `moder-replace'.

If this command is called after `moder-replace',
`moder-replace-char', `moder-replace-save', or itself, replace the
previous replacement with the next item in the `kill-ring'.

Unlike `yank-pop', this command does not rotate the `kill-ring'.
For that, see the command `rotate-yank-pointer'.

For custom commands, see also the user option
`moder-replace-pop-command-start-indexes'."
  (interactive "*")
  (unless kill-ring (user-error "Can't replace; kill ring is empty"))
  (let ((select-enable-clipboard moder-use-clipboard))
    (when (moder--allow-modify-p)
      (setq moder--replace-pop-index
            (cond
             ((eq last-command 'moder-replace-pop) (1+ moder--replace-pop-index))
             ((alist-get last-command moder-replace-pop-command-start-indexes))
             (t (user-error "Can only run `moder-replace-pop' after itself or a command in `moder-replace-pop-command-start-indexes'"))))
      (when (>= moder--replace-pop-index (length kill-ring))
        (setq moder--replace-pop-index 0)
        (message "`moder-replace-pop': Reached end of kill ring"))
      (let ((txt (string-trim-right (current-kill moder--replace-pop-index t)
                                    "\n")))
        (moder--delete-region moder--replace-start-marker (point))
        (set-marker moder--replace-start-marker (point))
        (moder--insert txt))))
  (setq this-command 'moder-replace-pop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHAR MOVEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder-left ()
  "Move to left.

Will cancel all other selection, except char selection. "
  (interactive)
  (when (and (region-active-p)
             (not (equal '(expand . char) (moder--selection-type))))
    (moder-cancel-selection))
  (moder--execute-kbd-macro moder--kbd-backward-char))

(defun moder-right ()
  "Move to right.

Will cancel all other selection, except char selection. "
  (interactive)
  (let ((ra (region-active-p)))
    (when (and ra
               (not (equal '(expand . char) (moder--selection-type))))
      (moder-cancel-selection))
    (when (or (not moder-use-cursor-position-hack)
              (not ra)
              (equal '(expand . char) (moder--selection-type)))
      (moder--execute-kbd-macro moder--kbd-forward-char))))

(defun moder-left-expand ()
  "Activate char selection, then move left."
  (interactive)
  (if (region-active-p)
      (thread-first
        (moder--create-selection '(expand . char) (mark) (point))
        (moder--select t))
    (when moder-use-cursor-position-hack
      (forward-char 1))
    (thread-first
      (moder--create-selection '(expand . char) (point) (point))
      (moder--select t)))
  (moder--execute-kbd-macro moder--kbd-backward-char))

(defun moder-right-expand ()
  "Activate char selection, then move right."
  (interactive)
  (if (region-active-p)
      (thread-first
        (moder--create-selection '(expand . char) (mark) (point))
        (moder--select t))
    (thread-first
      (moder--create-selection '(expand . char) (point) (point))
      (moder--select t)))
  (moder--execute-kbd-macro moder--kbd-forward-char))

(defun moder-prev (arg)
  "Move to the previous line.

Will cancel all other selection, except char selection.

Use with universal argument to move to the first line of buffer.
Use with numeric argument to move multiple lines at once."
  (interactive "P")
  (unless (equal (moder--selection-type) '(expand . char))
    (moder--cancel-selection))
  (cond
   ((moder--with-universal-argument-p arg)
    (goto-char (point-min)))
   (t
    (setq this-command #'previous-line)
    (moder--execute-kbd-macro moder--kbd-backward-line))))

(defun moder-next (arg)
  "Move to the next line.

Will cancel all other selection, except char selection.

Use with universal argument to move to the last line of buffer.
Use with numeric argument to move multiple lines at once."
  (interactive "P")
  (unless (equal (moder--selection-type) '(expand . char))
    (moder--cancel-selection))
  (cond
   ((moder--with-universal-argument-p arg)
    (goto-char (point-max)))
   (t
    (setq this-command #'next-line)
    (moder--execute-kbd-macro moder--kbd-forward-line))))

(defun moder-prev-expand (arg)
  "Activate char selection, then move to the previous line.

See `moder-prev-line' for how prefix arguments work."
  (interactive "P")
  (if (region-active-p)
      (thread-first
        (moder--create-selection '(expand . char) (mark) (point))
        (moder--select t))
    (thread-first
      (moder--create-selection '(expand . char) (point) (point))
      (moder--select t)))
  (cond
   ((moder--with-universal-argument-p arg)
    (goto-char (point-min)))
   (t
    (setq this-command #'previous-line)
    (moder--execute-kbd-macro moder--kbd-backward-line))))

(defun moder-next-expand (arg)
  "Activate char selection, then move to the next line.

See `moder-next-line' for how prefix arguments work."
  (interactive "P")
  (if (region-active-p)
      (thread-first
        (moder--create-selection '(expand . char) (mark) (point))
        (moder--select t))
    (thread-first
      (moder--create-selection '(expand . char) (point) (point))
      (moder--select t)))
  (cond
   ((moder--with-universal-argument-p arg)
    (goto-char (point-max)))
   (t
    (setq this-command #'next-line)
    (moder--execute-kbd-macro moder--kbd-forward-line))))

(defun moder-mark-thing (thing type &optional backward regexp-format)
  "Make expandable selection of THING, with TYPE and forward/BACKWARD direction.

THING is a symbol usable by `forward-thing', which see.

TYPE is a symbol. Usual values are `word' or `line'.

The selection will be made in the \\='forward\\=' direction unless BACKWARD is
non-nil.

When REGEXP-FORMAT is non-nil and a string, the content of the selection will be
quoted to regexp, then pushed into `regexp-search-ring' which will be read by
`moder-search' and other commands. In this case, REGEXP-FORMAT is used as a
format-string to format the regexp-quoted selection content (which is passed as
a string to `format'). Further matches of this formatted search will be
highlighted in the buffer."
  (let* ((bounds (bounds-of-thing-at-point thing))
         (beg (car bounds))
         (end (cdr bounds)))
    (when beg
      (thread-first
        (moder--create-selection (cons 'expand type) beg end)
        (moder--select t backward))
      (when (stringp regexp-format)
        (let ((search (format regexp-format (regexp-quote (buffer-substring-no-properties beg end)))))
          (moder--push-search search)
          (moder--highlight-regexp-in-buffer search))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WORD/SYMBOL MOVEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder-mark-word (n)
  "Mark current word under cursor.

A expandable word selection will be created. `moder-next-word' and
`moder-back-word' can be used for expanding.

The content of selection will be quoted to regexp, then pushed into
`regexp-search-ring' which be read by `moder-search' and other commands.

This command will also provide highlighting for same occurs.

Use negative argument to create a backward selection."
  (interactive "p")
  (moder-mark-thing moder-word-thing 'word (< n 0) "\\<%s\\>"))

(defun moder-mark-symbol (n)
  "Mark current symbol under cursor.

This command works similar to `moder-mark-word'."
  (interactive "p")
  (moder-mark-thing moder-symbol-thing 'symbol (< n 0) "\\_<%s\\_>"))

(defun moder--forward-thing-1 (thing)
  (let ((pos (point)))
    (forward-thing thing 1)
    (when (not (= pos (point)))
      (moder--hack-cursor-pos (point)))))

(defun moder--backward-thing-1 (thing)
  (let ((pos (point)))
    (forward-thing thing -1)
    (when (not (= pos (point)))
      (point))))

(defun moder--fix-thing-selection-mark (thing pos mark include-syntax)
  "Return new mark for a selection of THING.
This will shrink the word selection only contains
those in INCLUDE-SYNTAX."
  (let ((backward (> mark pos)))
    (save-mark-and-excursion
      (goto-char
       (if backward pos
         ;; Point must be before the end of the word to get the bounds correctly
         (1- pos)))
      (let* ((bounds (or (bounds-of-thing-at-point thing) (cons mark mark)))
             (m (if backward
                    (min mark (cdr bounds))
                  (max mark (car bounds)))))
        (save-mark-and-excursion
          (goto-char m)
          (if backward
              (skip-syntax-forward include-syntax mark)
            (skip-syntax-backward include-syntax mark))
          (point))))))

(defun moder-next-thing (thing type n &optional include-syntax)
  "Create non-expandable selection of TYPE to the end of the next Nth THING.

If N is negative, select to the beginning of the previous Nth thing instead."
  (unless (equal type (cdr (moder--selection-type)))
    (moder--cancel-selection))
  (unless include-syntax
    (setq include-syntax
          (let ((thing-include-syntax
                 (or (alist-get thing moder-next-thing-include-syntax)
                     '("" ""))))
            (if (> n 0)
                (car thing-include-syntax)
              (cadr thing-include-syntax)))))
  (let* ((expand (equal (cons 'expand type) (moder--selection-type)))
         (_ (when expand
              (if (< n 0) (moder--direction-backward)
                (moder--direction-forward))))
         (new-type (if expand (cons 'expand type) (cons 'select type)))
         (m (point))
         (p (save-mark-and-excursion
              (forward-thing thing n)
              (unless (= (point) m)
                (point)))))
    (when p
      (thread-first
        (moder--create-selection
         new-type
         (moder--fix-thing-selection-mark thing p m include-syntax)
         p
         expand)
        (moder--select t)))))

(defun moder-next-word (n)
  "Select to the end of the next Nth word.

A non-expandable, word selection will be created.

To select continuous words, use following approaches:

1. start the selection with `moder-mark-word'.

2. use prefix digit arguments.

3. use `moder-expand' after this command.
"
  (interactive "p")
  (moder-next-thing moder-word-thing 'word n))

(defun moder-next-symbol (n)
  "Select to the end of the next Nth symbol.

A non-expandable, word selection will be created.
There's no symbol selection type in Moder.

To select continuous symbols, use following approaches:

1. start the selection with `moder-mark-symbol'.

2. use prefix digit arguments.

3. use `moder-expand' after this command."
  (interactive "p")
  (moder-next-thing moder-symbol-thing 'symbol n))

(defun moder-back-word (n)
  "Select to the beginning the previous Nth word.

A non-expandable word selection will be created.
This command works similar to `moder-next-word'."
  (interactive "p")
  (moder-next-thing moder-word-thing 'word (- n)))

(defun moder-back-symbol (n)
  "Select to the beginning the previous Nth symbol.

A non-expandable word selection will be created.
This command works similar to `moder-next-symbol'."
  (interactive "p")
  (moder-next-thing moder-symbol-thing 'symbol (- n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LINE SELECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder--forward-line-1 ()
  (let ((orig (point)))
    (forward-line 1)
    (if moder--expanding-p
        (progn
          (goto-char (line-end-position))
          (line-end-position))
      (when (< orig (line-beginning-position))
        (line-beginning-position)))))

(defun moder--backward-line-1 ()
  (forward-line -1)
  (line-beginning-position))

(defun moder-line (n &optional expand)
  "Select the current line, eol is not included.

Create selection with type (expand . line).
For the selection with type (expand . line), expand it by line.
For the selection with other types, cancel it.

Prefix:
numeric, repeat times.
"
  (interactive "p")
  (let* ((cancel-sel (not (or expand (equal '(expand . line) (moder--selection-type)))))
         (backward (unless cancel-sel (moder--direction-backward-p)))
         (orig (if cancel-sel (point) (mark t)))
         (n (if backward
                (- n)
              n))
         (forward (> n 0)))
    (cond
     ((not cancel-sel)
      (let (p)
        (save-mark-and-excursion
          (forward-line n)
          (goto-char
           (if forward
               (setq p (line-end-position))
             (setq p (line-beginning-position)))))
        (thread-first
          (moder--create-selection '(expand . line) orig p expand)
          (moder--select t))))
     (t
      (let ((m (if forward
                   (line-beginning-position)
                 (line-end-position)))
            (p (save-mark-and-excursion
                 (if forward
                     (progn
                       (unless (= n 1)
                         (forward-line (1- n)))
                       (line-end-position))
                   (progn
                     (forward-line (1+ n))
                     (when (moder--empty-line-p)
                       (backward-char 1))
                     (line-beginning-position))))))
        (thread-first
          (moder--create-selection '(expand . line) m p expand)
          (moder--select t)))))))

(defun moder-line-expand (n)
  "Like `moder-line', but always expand."
  (interactive "p")
  (moder-line n t))

(defun moder-goto-line ()
  "Goto line, recenter and select that line.

This command will expand line selection."
  (interactive)
  (let* ((rbeg (when (use-region-p) (region-beginning)))
         (rend (when (use-region-p) (region-end)))
         (expand (equal '(expand . line) (moder--selection-type)))
         (orig-p (point))
         (beg-end (save-mark-and-excursion
                    (if moder-goto-line-function
                        (call-interactively moder-goto-line-function)
                      (moder--execute-kbd-macro moder--kbd-goto-line))
                    (cons (line-beginning-position)
                          (line-end-position))))
         (beg (car beg-end))
         (end (cdr beg-end)))
    (thread-first
      (moder--create-selection '(expand . line)
                               (if (and expand rbeg) (min rbeg beg) beg)
                               (if (and expand rend) (max rend end) end))
      (moder--select t (> orig-p beg)))
    (recenter)))

;; visual line versions
(defun moder--visual-line-beginning-position ()
  (save-excursion
    (beginning-of-visual-line)
    (point)))

(defun moder--visual-line-end-position ()
  (save-excursion
    (end-of-visual-line)
    (point)))

(defun moder--forward-visual-line-1 ()
  (let ((orig (point)))
    (line-move-visual 1)
    (if moder--expanding-p
        (progn
          (goto-char (moder--visual-line-end-position))
          (moder--visual-line-end-position))
      (when (< orig (moder--visual-line-beginning-position))
        (moder--visual-line-beginning-position)))))

(defun moder--backward-visual-line-1 ()
  (line-move-visual -1)
  (moder--visual-line-beginning-position))

(defun moder-visual-line (n &optional expand)
  "Select the current visual line, eol is not included.

Create selection with type (expand . line).
For the selection with type (expand . line), expand it by line.
For the selection with other types, cancel it.

Prefix:
numeric, repeat times.
"
  (interactive "p")
  (unless (or expand (equal '(expand . line) (moder--selection-type)))
    (moder--cancel-selection))
  (let* ((orig (mark t))
         (n (if (moder--direction-backward-p)
                (- n)
              n))
         (forward (> n 0)))
    (cond
     ((region-active-p)
      (let (p)
        (save-mark-and-excursion
          (line-move-visual n)
          (goto-char
           (if forward
               (setq p (moder--visual-line-end-position))
             (setq p (moder--visual-line-beginning-position)))))
        (thread-first
          (moder--create-selection '(expand . line) orig p expand)
          (moder--select t))))
     (t
      (let ((m (if forward
                   (moder--visual-line-beginning-position)
                 (moder--visual-line-end-position)))
            (p (save-mark-and-excursion
                 (if forward
                     (progn
                       (line-move-visual (1- n))
                       (moder--visual-line-end-position))
                   (progn
                     (line-move-visual (1+ n))
                     (when (moder--empty-line-p)
                       (backward-char 1))
                     (moder--visual-line-beginning-position))))))
        (thread-first
          (moder--create-selection '(expand . line) m p expand)
          (moder--select t)))))))

(defun moder-visual-line-expand (n)
  "Like `moder-line', but always expand."
  (interactive "p")
  (moder-visual-line n t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder--backward-block ()
  (let ((orig-pos (point))
        (pos (save-mark-and-excursion
               (let ((depth (car (syntax-ppss))))
                 (while (and (re-search-backward "\\s(" nil t)
                             (> (car (syntax-ppss)) depth)))
                 (when (= (car (syntax-ppss)) depth)
                   (point))))))
    (when (and pos (not (= orig-pos pos)))
      (goto-char pos))))

(defun moder--forward-block ()
  (let ((orig-pos (point))
        (pos (save-mark-and-excursion
               (let ((depth (car (syntax-ppss))))
                 (while (and (re-search-forward "\\s)" nil t)
                             (> (car (syntax-ppss)) depth)))
                 (when (= (car (syntax-ppss)) depth)
                   (point))))))
    (when (and pos (not (= orig-pos pos)))
      (goto-char pos)
      (moder--hack-cursor-pos (point)))))

(defun moder-block (arg)
  "Mark the block or expand to parent block."
  (interactive "P")
  (let ((ra (region-active-p))
        (back (xor (moder--direction-backward-p) (< (prefix-numeric-value arg) 0)))
        (depth (car (syntax-ppss)))
        (orig-pos (point))
        p m)
    (save-mark-and-excursion
      (while (and (if back (re-search-backward "\\s(" nil t) (re-search-forward "\\s)" nil t))
                  (or (moder--in-string-p)
                      (if ra (>= (car (syntax-ppss)) depth) (> (car (syntax-ppss)) depth)))))
      (when (and (if ra (< (car (syntax-ppss)) depth) (<= (car (syntax-ppss)) depth))
                 (not (= (point) orig-pos)))
        (setq p (point))
        (when (ignore-errors (forward-list (if back 1 -1)) t)
          (setq m (point)))))
    (when (and p m)
      (thread-first
        (moder--create-selection '(expand . block) m p)
        (moder--select t)))))

(defun moder-to-block (arg)
  "Expand to next block.

Will create selection with type (expand . block)."
  (interactive "P")
  ;; We respect the direction of block selection.
  (let ((back (or (when (equal 'block (cdr (moder--selection-type)))
                    (moder--direction-backward-p))
                  (< (prefix-numeric-value arg) 0)))
        (depth (car (syntax-ppss)))
        (orig-pos (point))
        p m)
    (save-mark-and-excursion
      (while (and (if back (re-search-backward "\\s(" nil t) (re-search-forward "\\s)" nil t))
                  (or (moder--in-string-p)
                      (> (car (syntax-ppss)) depth))))
      (when (and (= (car (syntax-ppss)) depth)
                 (not (= (point) orig-pos)))
        (setq p (point))
        (when (ignore-errors (forward-list (if back 1 -1)) t)
          (setq m (point)))))
    (when (and p m)
      (thread-first
        (moder--create-selection '(expand . block) orig-pos p t)
        (moder--select t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JOIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder--join-forward ()
  (let (mark pos)
    (save-mark-and-excursion
      (goto-char (line-end-position))
      (setq pos (point))
      (when (re-search-forward "[[:space:]\n\r]*" nil t)
        (setq mark (point))))
    (when pos
      (thread-first
        (moder--create-selection '(expand . join) pos mark)
        (moder--select t)))))

(defun moder--join-backward ()
  (let* (mark
         pos)
    (save-mark-and-excursion
      (back-to-indentation)
      (setq pos (point))
      (goto-char (line-beginning-position))
      (while (looking-back "[[:space:]\n\r]" 1 t)
        (forward-char -1))
      (setq mark (point)))
    (thread-first
      (moder--create-selection '(expand . join) mark pos)
      (moder--select t))))

(defun moder--join-both ()
  (let* (mark
         pos)
    (save-mark-and-excursion
      (while (looking-back "[[:space:]\n\r]" 1 t)
        (forward-char -1))
      (setq mark (point)))
    (save-mark-and-excursion
      (while (looking-at "[[:space:]\n\r]")
        (forward-char 1))
      (setq pos (point)))
    (thread-first
      (moder--create-selection '(expand . join) mark pos)
      (moder--select t))))

(defun moder-join (arg)
  "Select the indentation between this line to the non empty previous line.

Will create selection with type (select . join)

Prefix:
with NEGATIVE ARGUMENT, forward search indentation to select.
with UNIVERSAL ARGUMENT, search both side."
  (interactive "P")
  (cond
   ((or (equal '(expand . join) (moder--selection-type))
        (moder--with-universal-argument-p arg))
    (moder--join-both))
   ((moder--with-negative-argument-p arg)
    (moder--join-forward))
   (t
    (moder--join-backward))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIND & TILL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder--find-continue-forward ()
  (when moder--last-find
    (let ((case-fold-search nil)
          (ch-str (char-to-string moder--last-find)))
      (when (search-forward ch-str nil t 1)
        (moder--hack-cursor-pos (point))))))

(defun moder--find-continue-backward ()
  (when moder--last-find
    (let ((case-fold-search nil)
          (ch-str (char-to-string moder--last-find)))
      (search-backward ch-str nil t 1))))

(defun moder--till-continue-forward ()
  (when moder--last-till
    (let ((case-fold-search nil)
          (ch-str (char-to-string moder--last-till)))
      (when (< (point) (point-max))
        (forward-char 1)
        (when (search-forward ch-str nil t 1)
          (backward-char 1)
          (moder--hack-cursor-pos (point)))))))

(defun moder--till-continue-backward ()
  (when moder--last-till
    (let ((case-fold-search nil)
          (ch-str (char-to-string moder--last-till)))
      (when (> (point) (point-min))
        (backward-char 1)
        (when (search-backward ch-str nil t 1)
          (forward-char 1)
          (point))))))

(defun moder-find (n ch &optional expand)
  "Find the next N char read from minibuffer."
  (interactive "p\ncFind:")
  (let* ((case-fold-search nil)
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (beg (point))
         end)
    (save-mark-and-excursion
      (setq end (search-forward ch-str nil t n)))
    (if (not end)
        (message "char %s not found" ch-str)
      (thread-first
        (moder--create-selection '(select . find)
                                 beg end expand)
        (moder--select t))
      (setq moder--last-find ch))))

(defun moder-find-expand (n ch)
  (interactive "p\ncExpand find:")
  (moder-find n ch t))

(defun moder-till (n ch &optional expand)
  "Forward till the next N char read from minibuffer."
  (interactive "p\ncTill:")
  (let* ((case-fold-search nil)
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (beg (point))
         (fix-pos (if (< n 0) 1 -1))
         end)
    (save-mark-and-excursion
      (if (> n 0) (forward-char 1) (forward-char -1))
      (setq end (search-forward ch-str nil t n)))
    (if (not end)
        (message "char %s not found" ch-str)
      (thread-first
        (moder--create-selection '(select . till)
                                 beg (+ end fix-pos) expand)
        (moder--select t))
      (setq moder--last-till ch))))

(defun moder-till-expand (n ch)
  (interactive "p\ncExpand till:")
  (moder-till n ch t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VISIT and SEARCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder-search (arg)
  "Search and select with the car of current `regexp-search-ring'.

If the contents of selection doesn't match the regexp, will push
it to `regexp-search-ring' before searching.

To search backward, use \\[negative-argument]."
  (interactive "P")
  ;; Test if we add current region as search target.
  (when (and (region-active-p)
             (let ((search (car regexp-search-ring)))
               (or (not search)
                   (not (string-match-p
                         (format "^%s$" search)
                         (buffer-substring-no-properties (region-beginning) (region-end)))))))
    (moder--push-search (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end)))))
  (when-let* ((search (car regexp-search-ring)))
    (let ((reverse (xor (moder--with-negative-argument-p arg) (moder--direction-backward-p)))
          (case-fold-search nil))
      (if (or (if reverse
                  (re-search-backward search nil t 1)
                (re-search-forward search nil t 1))
              ;; Try research from buffer beginning/end
              ;; if we are already at the last/first matched
              (save-mark-and-excursion
                ;; Recalculate search indicator
                (moder--clean-search-indicator-state)
                (goto-char (if reverse (point-max) (point-min)))
                (if reverse
                    (re-search-backward search nil t 1)
                  (re-search-forward search nil t 1))))
          (let* ((m (match-data))
                 (marker-beg (car m))
                 (marker-end (cadr m))
                 (beg (if reverse (marker-position marker-end) (marker-position marker-beg)))
                 (end (if reverse (marker-position marker-beg) (marker-position marker-end))))
            (thread-first
              (moder--create-selection '(select . visit) beg end)
              (moder--select t))
            (if reverse
                (message "Reverse search: %s" search)
              (message "Search: %s" search))
            (moder--ensure-visible))
        (message "Searching %s failed" search))
      (moder--highlight-regexp-in-buffer search))))

(defun moder-pop-search ()
  "Searching for the previous target."
  (interactive)
  (when-let* ((search (pop regexp-search-ring)))
    (message "current search is: %s" (car regexp-search-ring))
    (moder--cancel-selection)))

(defun moder--visit-point (text reverse)
  "Return the point of text for visit command.
Argument TEXT current search text.
Argument REVERSE if selection is reversed."
  (let ((func (if reverse #'re-search-backward #'re-search-forward))
        (func-2 (if reverse #'re-search-forward #'re-search-backward))
        (case-fold-search nil))
    (save-mark-and-excursion
      (or (funcall func text nil t 1)
          (funcall func-2 text nil t 1)))))

(defun moder-visit (arg)
  "Read a string from minibuffer, then find and select it.

The input will be pushed into `regexp-search-ring'.  So
\\[moder-search] can be used for further searching with the same
condition.

A list of words and symbols in the current buffer will be
provided for completion.  To search for regexp instead, set
`moder-visit-sanitize-completion' to nil.  In that case,
completions will be provided in regexp form, but also covering
the words and symbols in the current buffer.

To search backward, use \\[negative-argument]."
  (interactive "P")
  (let* ((reverse arg)
         (pos (point))
         (text (moder--prompt-symbol-and-words
                (if arg "Visit backward: " "Visit: ")
                (point-min) (point-max) t))
         (visit-point (moder--visit-point text reverse)))
    (if visit-point
        (let* ((m (match-data))
               (marker-beg (car m))
               (marker-end (cadr m))
               (beg (if (> pos visit-point) (marker-position marker-end) (marker-position marker-beg)))
               (end (if (> pos visit-point) (marker-position marker-beg) (marker-position marker-end))))
          (thread-first
            (moder--create-selection '(select . visit) beg end)
            (moder--select t))
          (moder--push-search text)
          (moder--ensure-visible)
          (moder--highlight-regexp-in-buffer text)
          (setq moder--dont-remove-overlay t))
      (message "Visit: %s failed" text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder-thing-prompt (prompt-text)
  (when-let* ((char (read-char
                     (if moder-display-thing-help
                         (concat (moder--render-char-thing-table) "\n" prompt-text)
                       prompt-text)))
              (thing (cdr (assoc char moder-local-char-thing-table))))
    thing))

(defun moder--thing-get-direction (cmd)
  (or
   (alist-get cmd moder-thing-selection-directions)
   'forward))

(defun moder-beginning-of-thing (thing n)
  "Select to the beginning of Nth THING."
  (interactive (list (moder-thing-prompt "Beginning of: ") (or current-prefix-arg 1)))
  (save-window-excursion
    (let ((back (equal 'backward (moder--thing-get-direction 'beginning)))
          (bounds (moder--parse-range-of-thing thing 'inner)))
      (when bounds
        (thread-first
          (moder--create-selection '(select . thing)
                                   (if back (point) (car bounds))
                                   (if back (car bounds) (point))
                                   nil
                                   thing)
          (moder--select t)))))
  (moder--thing-set-transient-map))

(defun moder-end-of-thing (thing n)
  "Select to the end of Nth THING."
  (interactive (list (moder-thing-prompt "End of: ") (or current-prefix-arg 1)))
  (save-window-excursion
    (let ((back (equal 'backward (moder--thing-get-direction 'end)))
          (bounds (moder--parse-range-of-thing thing 'inner)))
      (when bounds
        (thread-first
          (moder--create-selection '(select . thing)
                                   (if back (cdr bounds) (point))
                                   (if back (point) (cdr bounds))
                                   nil
                                   thing)
          (moder--select t)))))
  (moder--thing-set-transient-map))

(defun moder--select-range (back bounds &optional thing)
  (when bounds
    (thread-first
      (moder--create-selection '(select . transient)
                               (if back (cdr bounds) (car bounds))
                               (if back (car bounds) (cdr bounds))
                               nil
                               thing)
      (moder--select t))))

(defun moder--select-thing (back bounds thing &optional expand count)
  (when bounds
    (thread-first
      (moder--create-selection '(select . thing)
                               (if back (cdr bounds) (car bounds))
                               (if back (car bounds) (cdr bounds))
                               expand
                               thing)
      (moder--select t))))

(defun moder-inner-of-thing (thing)
  "Select inner (excluding delimiters) of THING."
  (interactive (list (moder-thing-prompt "Inner of: ")))
  (let ((back (equal 'backward (moder--thing-get-direction 'inner)))
        (bounds (moder--parse-range-of-thing thing 'inner)))
    (moder--select-thing back bounds thing))
  (moder--thing-set-transient-map))

(defun moder-bounds-of-thing (thing n)
  "Select bounds (including delimiters) of N THING."
  (interactive (list (moder-thing-prompt "Bounds of: ") (or current-prefix-arg 1)))
  (let ((back (equal 'backward (moder--thing-get-direction 'bounds)))
        (bounds (moder--parse-range-of-thing thing 'bounds)))
    (moder--select-thing back bounds thing))
  (moder--thing-set-transient-map))

(defun moder-indent ()
  "Indent region or current line."
  (interactive)
  (moder--execute-kbd-macro moder--kbd-indent-region))

(defun moder-M-x ()
  "Just Meta-x."
  (interactive)
  (moder--execute-kbd-macro moder--kbd-excute-extended-command))

(defun moder-unpop-to-mark ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (moder--cancel-selection)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))
    (when moder-mark-recenter-after-unpop
      (recenter nil t))))

(defun moder-pop-to-mark ()
  "Alternative command to `pop-to-mark-command'.

Before jump, a mark of current location will be created."
  (interactive)
  (moder--cancel-selection)
  (unless (member last-command '(moder-pop-to-mark moder-unpop-to-mark moder-pop-or-unpop-to-mark))
    (setq mark-ring (append mark-ring (list (point-marker)))))
  (pop-to-mark-command)
  (when moder-mark-recenter-after-pop
    (recenter nil t)))

(defun moder-pop-or-unpop-to-mark (arg)
  "Call `moder-pop-to-mark' or `moder-unpop-to-mark', depending on ARG.

With a negative prefix ARG, call `moder-unpop-to-mark'. Otherwise, call
`moder-pop-to-mark.'

See also `moder-pop-or-unpop-to-mark-repeat-unpop'."
  (interactive "p")
  (if (or (and moder-pop-or-unpop-to-mark-repeat-unpop
               (eq last-command 'moder-unpop-to-mark))
          (< arg 0))
      (progn
        (setq this-command 'moder-unpop-to-mark)
        (moder-unpop-to-mark))
    (moder-pop-to-mark)))

(defun moder-pop-to-global-mark ()
  "Alternative command to `pop-global-mark'.

Before jump, a mark of current location will be created."
  (interactive)
  (moder--cancel-selection)
  (unless (member last-command '(moder-pop-to-global-mark moder-pop-to-mark moder-unpop-to-mark))
    (setq global-mark-ring (append global-mark-ring (list (point-marker)))))
  (moder--execute-kbd-macro moder--kbd-pop-global-mark)
  (when moder-mark-recenter-after-global-pop
    (recenter nil t)))

(defun moder-back-to-indentation ()
  "Back to indentation."
  (interactive)
  (moder--execute-kbd-macro moder--kbd-back-to-indentation))

(defun moder-query-replace ()
  "Query replace."
  (interactive)
  (moder--execute-kbd-macro moder--kbd-query-replace))

(defun moder-query-replace-regexp ()
  "Query replace regexp."
  (interactive)
  (moder--execute-kbd-macro moder--kbd-query-replace-regexp))

(defun moder-last-buffer (arg)
  "Switch to last buffer.
Argument ARG if not nil, switching in a new window."
  (interactive "P")
  (cond
   ((minibufferp)
    (keyboard-escape-quit))
   ((not arg)
    (mode-line-other-buffer))
   (t)))

(defun moder-minibuffer-quit ()
  "Keyboard escape quit in minibuffer."
  (interactive)
  (if (minibufferp)
      (if (fboundp 'minibuffer-keyboard-quit)
          (call-interactively #'minibuffer-keyboard-quit)
        (call-interactively #'abort-recursive-edit))
    (call-interactively #'keyboard-quit)))

(defun moder-escape-or-normal-modal (&optional no-hook)
  "Keyboard escape quit or switch to normal state."
  (interactive "p")
  (cond
   ((minibufferp)
    (if (fboundp 'minibuffer-keyboard-quit)
        (call-interactively #'minibuffer-keyboard-quit)
      (call-interactively #'abort-recursive-edit)))
   ((moder-keypad-mode-p)
    (moder--exit-keypad-state))
   ((moder-insert-mode-p)
    (moder--switch-state 'normal (bound-and-true-p no-hook)))
   (t
    (moder--switch-state 'normal))))

(defun moder-eval-last-exp ()
  "Eval last sexp."
  (interactive)
  (moder--execute-kbd-macro moder--kbd-eval-last-exp))

(defun moder-expand (&optional n)
  (interactive)
  (moder--with-selection-fallback
   (when (and (region-active-p)
              (moder--selection-type))
     (let* ((n (or n (string-to-number (char-to-string last-input-event))))
            (n (if (= n 0) 10 n))
            (fn (moder--get-forward-function (moder--selection-thing)))
            ;; TODO: figure out what does `moder-expand-selection-type' change
            (sel-type (cons moder-expand-selection-type (cdr (moder--selection-type)))))
       (when (functionp fn)
         (thread-first
           (moder--create-selection sel-type (mark)
                                    (save-mark-and-excursion
                                      (let ((moder--expanding-p t))
                                        (dotimes (_ n)
                                          (funcall fn (if (moder--direction-forward) 1 -1))))
                                      (point)))
           (moder--select t)))))))

(defun moder-expand-thing (&optional n)
  "Expand current selection by N THINGS."
  (interactive "p")
  (moder--with-selection-fallback
   (when (and (region-active-p) (moder--selection-type))
     (let* ((n (if (= n 0) 10 n))
            (thing (alist-get last-input-event moder-local-char-thing-table))
            (fn (moder--get-forward-function thing))
            (arg (if (moder--direction-forward-p) 1 -1)))
       (when (functionp fn)
         (thread-first
           (moder--expand-selection '(select . thing) (mark)
                                    (save-mark-and-excursion
                                      (dotimes (_ n)
                                        (funcall fn arg))
                                      (point))
                                    thing
                                    (abs n)
                                    'moder-expand-thing)
           (moder--select t)))))))

(defun moder-expand-1 () (interactive) (moder-expand 1))
(defun moder-expand-2 () (interactive) (moder-expand 2))
(defun moder-expand-3 () (interactive) (moder-expand 3))
(defun moder-expand-4 () (interactive) (moder-expand 4))
(defun moder-expand-5 () (interactive) (moder-expand 5))
(defun moder-expand-6 () (interactive) (moder-expand 6))
(defun moder-expand-7 () (interactive) (moder-expand 7))
(defun moder-expand-8 () (interactive) (moder-expand 8))
(defun moder-expand-9 () (interactive) (moder-expand 9))
(defun moder-expand-0 () (interactive) (moder-expand 0))

(defun moder-digit-argument ()
  (interactive)
  (set-transient-map moder-numeric-argument-keymap)
  (call-interactively #'digit-argument))

(defun moder-universal-argument ()
  "Replacement for universal-argument."
  (interactive)
  (if current-prefix-arg
      (call-interactively 'universal-argument-more)
    (call-interactively 'universal-argument)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; KMACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder-kmacro-lines ()
  "Apply KMacro to each line in region."
  (interactive)
  (moder--with-selection-fallback
   (let ((beg (caar (region-bounds)))
         (end (cdar (region-bounds)))
         (ov-list))
     (moder--wrap-collapse-undo
      ;; create overlays as marks at each line beginning.
      ;; apply kmacro at those positions.
      ;; these allow user executing kmacro those create newlines.
      (save-mark-and-excursion
        (goto-char beg)
        (while (< (point) end)
          (goto-char (line-beginning-position))
          (push (make-overlay (point) (point)) ov-list)
          (forward-line 1)))
      (cl-loop for ov in (reverse ov-list) do
               (goto-char (overlay-start ov))
               (thread-first
                 (moder--create-selection 'line (line-end-position) (line-beginning-position))
                 (moder--select t))
               (call-last-kbd-macro)
               (delete-overlay ov))))))

(defun moder-kmacro-regex-matches (arg)
  "Apply KMacro by search.

Use negative argument for backward application."
  (interactive "P")
  (let ((s (car regexp-search-ring))
        (case-fold-search nil)
        (back (moder--with-negative-argument-p arg)))
    (moder--wrap-collapse-undo
     (while (if back
                (re-search-backward s nil t)
              (re-search-forward s nil t))
       (thread-first
         (moder--create-selection '(select . visit)
                                  (if back
                                      (point)
                                    (match-beginning 0))
                                  (if back
                                      (match-end 0)
                                    (point)))
         (moder--select t))
       (let ((ov (make-overlay (region-beginning) (region-end))))
         (unwind-protect
             (progn
               (kmacro-call-macro nil))
           (progn
             (if back
                 (goto-char (min (point) (overlay-start ov)))
               (goto-char (max (point) (overlay-end ov))))
             (delete-overlay ov))))))))

(defun moder-kmacro-search-matches (arg)
  "Apply KMacro by search."
  (interactive "p")
  (let ((s (car search-ring))
        (case-fold-search nil)
        (back (moder--with-negative-argument-p arg)))
    (moder--wrap-collapse-undo
     (while (if back
                (re-search-backward s nil t)
              (re-search-forward s nil t))
       (thread-first
         (moder--create-selection '(select . visit)
                                  (if back
                                      (point)
                                    (match-beginning 0))
                                  (if back
                                      (match-end 0)
                                    (point)))
         (moder--select t))
       (let ((ov (make-overlay (region-beginning) (region-end))))
         (unwind-protect
             (progn
               (kmacro-call-macro nil))
           (progn
             (if back
                 (goto-char (min (point) (overlay-start ov)))
               (goto-char (max (point) (overlay-end ov))))
             (delete-overlay ov))))))))

(defun moder-end-or-call-kmacro ()
  "End kmacro recording or call macro.

This command is a replacement for built-in `kmacro-end-or-call-macro'."
  (interactive)
  (cond
   ((and moder--keypad-this-command defining-kbd-macro)
    (message "Can't end kmacro with KEYPAD command"))
   ((eq moder--beacon-defining-kbd-macro 'record)
    (setq moder--beacon-defining-kbd-macro nil)
    (moder-beacon-end-and-apply-kmacro))
   ((or (moder-normal-mode-p)
        (moder-motion-mode-p))
    (call-interactively #'kmacro-end-or-call-macro))
   (t
    (message "Can only end or call kmacro in NORMAL or MOTION state."))))

(defun moder-end-kmacro ()
  "End kmacro recording or call macro.

This command is a replacement for built-in `kmacro-end-macro'."
  (interactive)
  (cond
   ((or (moder-normal-mode-p)
        (moder-motion-mode-p))
    (call-interactively #'kmacro-end-or-call-macro))
   (t
    (message "Can only end or call kmacro in NORMAL or MOTION state."))))

(defun moder-repeat-change (_arg)
  "Repeat last change recorded in current buffer.

"
  (interactive "p")
  (moder--kmacro-repeat-last-change))

(defun moder-last-change-to-kmacro (arg)
  "Copy last change to the head of `kmacro-ring'.

Pushes old head to the ring. If ARG is non-nil, copies ARGth change instead."
  (interactive "p")
  (let ((change (moder--kmacro-get-change (abs (- (abs arg) 1)))))
    (unless (vectorp change)
      (user-error "%s change is nil" (if (arg )(moder--ordinal arg))))
    (when (and (vectorp change) (vectorp last-kbd-macro))
      (kmacro-push-ring))
    (setq last-kbd-macro change)
    (message "Set `last-kbd-macro' to %S%s"
             (seq-take change 10)
             (if (length> change 10) "..." ""))))

(defvar moder--edit-matches-overlays nil)

(defun moder--edit-matches-mark ()
  (when (and moder--match-overlays (not executing-kbd-macro)
             (not defining-kbd-macro) (not (moder-beacon-mode-p)))
    (mapc (lambda (ov)
            (let ((beg (overlay-start ov))
                  (end (overlay-end ov))
                  (back (moder--direction-backward-p)))
              (moder--beacon-add-overlay-at-region
               '(select . visit)
               beg
               end
               back)))
          moder--match-overlays)))

(defun moder-edit-matches-insert ()
  ""
  (interactive)
  (moder--edit-matches-mark)
  (moder--switch-state 'beacon)
  (moder-beacon-insert))

(defun moder-edit-matches-append ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRAB SELECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local moder--secondary-selection-cookie nil)

(defun moder--cancel-second-selection ()
  (when (bound-and-true-p moder--secondary-selection-cookie)
    (face-remap-remove-relative moder--secondary-selection-cookie))
  (delete-overlay mouse-secondary-overlay)
  (setq mouse-secondary-start (make-marker))
  (move-marker mouse-secondary-start (point)))

(defun moder-grab ()
  "Create secondary selection or a marker if no region available."
  (interactive)
  (if (region-active-p)
      (cond
       ((moder--selection-rectangle-p moder--selection)))
    (secondary-selection-from-region)
    (moder--cancel-second-selection))
  (moder--cancel-selection))

(defun moder-pop-grab ()
  "Pop to secondary selection."
  (interactive)
  (cond
   ((moder--second-sel-buffer)
    (pop-to-buffer (moder--second-sel-buffer))
    (secondary-selection-to-region)
    (setq mouse-secondary-start (make-marker))
    (move-marker mouse-secondary-start (point))
    (moder--beacon-remove-overlays))
   ((markerp mouse-secondary-start)
    (or
     (when-let* ((buf (marker-buffer mouse-secondary-start)))
       (pop-to-buffer buf)
       (when-let* ((pos (marker-position mouse-secondary-start)))
         (goto-char pos)))
     (message "No secondary selection")))))

(defun moder-swap-grab ()
  "Swap region and secondary selection."
  (interactive)
  (let* ((rbeg (region-beginning))
         (rend (region-end))
         (region-str (when (region-active-p) (buffer-substring-no-properties rbeg rend)))
         (sel-str (moder--second-sel-get-string))
         (next-marker (make-marker)))
    (when region-str (moder--delete-region rbeg rend))
    (when sel-str (moder--insert sel-str))
    (move-marker next-marker (point))
    (moder--second-sel-set-string (or region-str ""))
    (when (overlayp mouse-secondary-overlay)
      (delete-overlay mouse-secondary-overlay))
    (setq mouse-secondary-start next-marker)
    (moder--cancel-selection)))

(defun moder-sync-grab ()
  "Sync secondary selection with current region."
  (interactive)
  (moder--with-selection-fallback
   (let* ((rbeg (region-beginning))
          (rend (region-end))
          (region-str (buffer-substring-no-properties rbeg rend))
          (next-marker (make-marker)))
     (move-marker next-marker (point))
     (moder--second-sel-set-string region-str)
     (when (overlayp mouse-secondary-overlay)
       (delete-overlay mouse-secondary-overlay))
     (setq mouse-secondary-start next-marker)
     (moder--cancel-selection))))

(defun moder-describe-key (key-list &optional buffer)
  (interactive (list (help--read-key-sequence)))
  (if (= 1 (length key-list))
      (let* ((key (format-kbd-macro (cdar key-list)))
             (cmd (key-binding key)))
        (if-let* ((dispatch (and (commandp cmd)
                                 (get cmd 'moder-dispatch))))
            (describe-key (kbd dispatch) buffer)
          (describe-key key-list buffer)))
    ;; for mouse events
    (describe-key key-list buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RING UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder-rotate-kmacro-ring (n)
  "Rotate the current head of `kmacro-ring' by N places."
  (interactive "p")
  (let* ((rotated (moder--rotate-list kmacro-ring n 'last-kbd-macro #'kmacro))
         (head (car rotated))
         (keys (kmacro--keys head))
         (format (kmacro--format head))
         (counter (kmacro--counter head)))
    (setq kmacro-ring (cdr rotated)
          last-kbd-macro head
          kmacro-counter counter
          kmacro-counter-format format))
  (moder--ring-rotate-message "Kmacro" 'kmacro-ring n 'last-kbd-macro)
  (message "Current kmacro is %S" (kmacro-ring-head)))

(defun moder-rotate-search-ring (n)
  "Rotate the current `search-ring' head by N places."
  (interactive "p")
  (setq search-ring (moder--rotate-seq search-ring n))
  (message "Current search is %S" (car search-ring)))

(defun moder-rotate-regexp-search-ring (n)
  "Rotate the current `regexp-search-ring' head by N places."
  (interactive "p")
  (setq regexp-search-ring (moder--rotate-seq regexp-search-ring n))
  (message "Current regexp search is %S" (car regexp-search-ring)))

(defun moder-rotate-kill-ring (n)
  "Rotate the current `kill-ring' head by N places."
  (interactive "p")
  (setq kill-ring (moder--rotate-seq kill-ring n))
  ;; TODO: update the yank pointer?
  (message "Current kill is %S" (current-kill 0 t)))

(defun moder-rotate-mark-ring (n)
  "Rotate the current `mark-ring' head by N places."
  (interactive "p")
  (setq mark-ring (moder--rotate-seq mark-ring n))
  (message "Current mark is %S" (car mark-ring)))

(defun moder-rotate-global-mark-ring  (n)
  "Rotate the current `global-mark-ring' head by N places."
  (interactive "p")
  (setq global-mark-ring (moder--rotate-seq global-mark-ring n))
  (message "Current global mark is %S" (car global-mark-ring)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REPEAT COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder-repeat (arg)
  "Repeat last command according to `moder--repeat-last-command'."
  (interactive "P")
  (let ((last-repeatable-command moder--repeat-last-command))
    (repeat arg)))

(defun moder-repeat-op (arg)
  (interactive "p"))

(defun moder-noop ()
  "Noop, to disable some keybindings in beacon state."
  (interactive))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REGISTERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder-selection-to-register (selection r)
  "Save SELECTION to register R."
  (interactive (list moder--selection (register-read-with-preview "Save to: ")))
  (if (and selection r)
      (set-register r moder--selection)
    (error "No selection active")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DIRED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: add kmacro selection
(defun moder-dired-apply-kmacro-in-files (files how arg)
  (interactive (list (dired-get-marked-files (moder-kmacro-how-prompt "How to apply kmacro: ") current-prefix-arg)) dired-mode)
  (moder--kmacro-apply-in-files files how nil last-kbd-macro))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IBUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun moder-ibuffer-apply-kmacro-in-buffers (bufs how)
  (interactive (list (ibuffer-get-marked-buffers) (moder-kmacro-how-prompt "How to apply kmacro: ")) ibuffer-mode)
  (moder--kmacro-appl))

;; aliases
(defalias 'moder-backward-delete 'moder-backspace)
(defalias 'moder-c-d 'moder-C-d)
(defalias 'moder-c-k 'moder-C-k)
(defalias 'moder-delete 'moder-C-d)
(defalias 'this is a testmoder-cancel 'moder-cancel-selection)

;; removed commands

(defmacro moder--remove-command (orig rep)
  `(defun ,orig ()
     (interactive)
     (message "Command removed, use `%s' instead." ,(symbol-name rep))))


(provide 'moder-command)
;;; moder-command.el ends here
