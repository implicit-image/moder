;;; moder-var.el --- Moder variables  -*- lexical-binding: t; -*-

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
;; Internal variables and customizable variables.

;;; Code:

(defgroup moder nil
  "Custom group for moder."
  :group 'moder-module)

;; Behaviors

(defcustom moder-use-cursor-position-hack nil
  "Whether to use cursor position hack."
  :group 'moder
  :type 'boolean)

(defcustom moder-use-enhanced-selection-effect nil
  "Whether to use enhanced cursor effect.

This will affect how selection is displayed."
  :group 'moder
  :type 'boolean)

(defcustom moder-expand-exclude-mode-list
  '(markdown-mode org-mode)
  "A list of major modes where after command expand should be disabled."
  :group 'moder
  :type '(repeat sexp))

(defcustom moder-keypad-execute-on-beacons nil
  "Execute keypad command directly on beacons when using it directly from
beacon state.

This doesn't affect how keypad works on recording or executing a kmacro."
  :group 'moder
  :type 'boolean)

(defcustom moder-selection-command-fallback
  '((moder-change . moder-change-char)
    (moder-kill . moder-C-k)
    (moder-save . kill-ring-save)
    (moder-cancel-selection . keyboard-quit)
    (moder-pop-selection . moder-pop-grab)
    (moder-beacon-change . moder-beacon-change-char)
    (moder-expand . moder-digit-argument))
  "Fallback commands for selection commands when there is no available selection."
  :group 'moder
  :type '(alist :key-type (function :tag "Command")
                :value-type (function :tag "Fallback")))

(defcustom moder-replace-state-name-list
  '((normal . "NORMAL")
    (motion . "MOTION")
    (keypad . "KEYPAD")
    (insert . "INSERT")
    (beacon . "BEACON"))
  "A list of mappings for how to display state in indicator."
  :group 'moder
  :type '(alist :key-type (symbol :tag "Moder state")
                :value-type (string :tag "Indicator")))

(defvar moder-indicator-face-alist
  '((normal . moder-normal-indicator)
    (motion . moder-motion-indicator)
    (keypad . moder-keypad-indicator)
    (insert . moder-insert-indicator)
    (beacon . moder-beacon-indicator))
  "Alist of moder states -> faces")

(defcustom moder-select-on-change t
  "Whether to activate region when exiting INSERT mode
 after `moder-change', `moder-change-char' and `moder-change-save'."
  :group 'moder
  :type 'boolean)

(defcustom moder-select-on-append nil
  "Whether to activate region when exiting INSERT mode after `moder-append'."
  :group 'moder
  :type 'boolean)

(defcustom moder-select-on-insert nil
  "Whether to activate region when exiting INSERT mode after `moder-insert'."
  :group 'moder
  :type 'boolean)

(defcustom moder-select-on-open nil
  "Whether to activate region when exiting INSERT mode after
`moder-open-above', `moder-open-below',`moder-open-above-visual' and
`moder-open-below-visual'."
  :group 'moder
  :type 'boolean)

(defcustom moder-expand-hint-remove-delay 1.0
  "The delay before the position hint disappears."
  :group 'moder
  :type 'number)

(defcustom moder-next-thing-include-syntax
  '((word " _w" " _w")
    (symbol " _w" " _w"))
  "The syntax to include selecting with moder-next-THING.

Each item is a (THING FORWARD_SYNTAX_TO_INCLUDE BACKWARD-SYNTAX_TO_INCLUDE)."
  :group 'moder
  :type '(repeat (list (symbol :tag "Thing")
                       (string :tag "Forward Syntax")
                       (string :tag "Backward Syntax"))))

(defcustom moder-expand-hint-counts
  '((word . 30)
    (line . 30)
    (block . 30)
    (find . 30)
    (till . 30)
    (symbol . 30))
  "The maximum numbers for expand hints of each type."
  :group 'moder
  :type '(alist :key-type (symbol :tag "Hint type")
                :value-type (integer :tag "Value")))

(defcustom moder-keypad-message t
  "Whether to log keypad messages in minibuffer."
  :group 'moder
  :type 'boolean)

(defcustom moder-char-thing-table
  '((?r . round)
    (?s . square)
    (?c . curly)
    (?g . string)
    (?e . symbol)
    (?w . window)
    (?b . buffer)
    (?p . paragraph)
    (?l . line)
    (?v . visual-line)
    (?d . defun)
    (?. . sentence)
    (?/ . search-string)
    (?% . search-regexp))
  "Mapping from char to thing."
  :group 'moder
  :type '(alist :key-type (character :tag "Char")
                :value-type (symbol :tag "Thing")))

(defcustom moder-thing-selection-directions
  '((inner . forward)
    (bounds . backward)
    (beginning . backward)
    (end . forward))
  "Selection directions for each thing command."
  :group 'moder
  :type '(alist :key-type (symbol :tag "Command")
                :value-type (symbol :tag "Direction")))

(defvar moder-word-thing 'word
  "The \\='thing\\=' used for marking and movement by words.

The values is a \\='thing\\=' as understood by `thingatpt' - a symbol that will
be passed to `forward-thing' and `bounds-of-thing-at-point', which see.

This means that they must, at minimum, have a function as the value of their
`forward-op' symbol property (or the function should be defined as
`forward-SYMBOLNAME'). This function should accept a single argument, a number
N, and should move over the next N things, in either the forward or backward
direction depending on the sign of N. Examples of such functions include
`forward-word', `forward-symbol' and `forward-sexp', which `thingatpt' uses for
the `word', `symbol' and `sexp' things, respectively.")

(defvar moder-symbol-thing 'symbol
  "The \\='thing\\=' used for marking and movement by symbols.

The values is a \\='thing\\=' as understood by `thingatpt' - a symbol that will
be passed to `forward-thing' and `bounds-of-thing-at-point', which see.

This means that they must, at minimum, have a function as the value of their
`forward-op' symbol property (or the function should be defined as
`forward-SYMBOLNAME'). This function should accept a single argument, a number
N, and should move over the next N things, in either the forward or backward
direction depending on the sign of N. Examples of such functions include
`forward-word', `forward-symbol' and `forward-sexp', which `thingatpt' uses for
the `word', `symbol' and `sexp' things, respectively.")

(defcustom moder-display-thing-help t
  "Whether to display the help prompt for moder-inner/bounds/begin/end-of-thing."
  :group 'moder
  :type 'boolean)

(defcustom moder-pop-or-unpop-to-mark-repeat-unpop nil
  "Non-nil means that calling `moder-pop-or-unpop-to-mark'
after calling it with a negative argument unpops the mark again.

This variable is meant to be similar to `set-mark-command-repeat-pop'."
  :group 'moder
  :type 'boolean)


(defcustom moder-keypad-describe-delay
  0.5
  "The delay in seconds before popup keybinding descriptions appear."
  :group 'moder
  :type 'number)

(defcustom moder-grab-fill-commands
  '(moder-query-replace moder-query-replace-regexp)
  "A list of commands that moder will auto fill with grabbed content."
  :group 'moder
  :type '(repeat function))

(defcustom moder-visit-collect-min-length 1
  "Minimal length when collecting symbols for `moder-visit'."
  :group 'moder
  :type 'integer)

(defcustom moder-visit-sanitize-completion t
  "Whether let `moder-visit' display symbol regexps in a sanitized format."
  :group 'moder
  :type 'boolean)

(defcustom moder-use-clipboard nil
  "Whether to use system clipboard."
  :group 'moder
  :type 'boolean)

(defcustom moder-use-keypad-when-execute-kbd t
  "Whether to use KEYPAD when the result of executing kbd string is a keymap."
  :group 'moder
  :type 'boolean)

(defcustom moder-use-dynamic-face-color t
  "Whether to use dynamic calculated face color.

This option will affect the color of position hint and fake region cursor."
  :group 'moder
  :type 'boolean)

(defcustom moder-mode-state-list
  '((conf-mode . normal)
    (fundamental-mode . normal)
    (help-mode . motion)
    (prog-mode . normal)
    (text-mode . normal))
  "A list of rules, each is (major-mode . init-state).

The init-state can be any state, including custom ones."
  :group 'moder
  :type '(alist :key-type (sexp :tag "Major-mode")
                :value-type (symbol :tag "Initial state")))

(defcustom moder-update-display-in-macro 'except-last-macro
  "Whether update cursor and mode-line when executing kbd macro.

Set to `nil' for no update in macro,
may not work well with some packages. (e.g. key-chord).

Set to `except-last-macro'
for no update only when executing last macro.

Set to `t' to always update.
"
  :group 'moder
  :type '(choice boolean
                 (const except-last-macro)))

(defcustom moder-expand-selection-type 'select
  "Whether to create transient selection for expand commands."
  :group 'moder
  :type '(choice (const select)
                 (const expand)))

(defcustom moder-keypad-leader-transparent 'motion
  "Use transparent behaivor when a bound command is not found in leader dispatch.

Value `t' stands for always be transparent.
Value `motion' stands for only be transparent in MOTION state.
Value `normal' stands for only be transparent in NORMAL state.
Value `nil' stands for never be transparent."
  :group 'moder
  :type '(choice (const t :tag "Always be transparent")
                 (const motion :tag "Transparent only in MOTION state")
                 (const normal :tag "Transparent only in NORMAL state")
                 (const nil :tag "Never be transparent")))

(defcustom moder-keypad-leader-dispatch nil
  "The fallback dispatching in KEYPAD when there's no translation.

The value can be either a string or a keymap:
A keymap stands for a base keymap used for further translation.
A string stands for finding the keymap at a specified key binding.
Nil stands for taking leader keymap from `moder-keymap-alist'."
  :group 'moder
  :type '(choice (string :tag "Keys")
                 (variable :tag "Keymap")
                 (const nil)))

(defcustom moder-keypad-meta-prefix ?m
  "The prefix represent M- in KEYPAD state."
  :group 'moder
  :type 'character)

(defcustom moder-keypad-ctrl-meta-prefix ?g
  "The prefix represent C-M- in KEYPAD state."
  :group 'moder
  :type 'character)

(defcustom moder-keypad-literal-prefix 32
  "The prefix represent no modifier in KEYPAD state."
  :group 'moder
  :type 'character)

(defcustom moder-keypad-start-keys
  '((?c . ?c)
    (?h . ?h)
    (?x . ?x))
  "Alist of keys to begin keypad translation. When a key char is pressed,
it's corresponding value is appended to C- and the user is
prompted to finish the command."
  :group 'moder
  :type '(alist :key-type (character :tag "From")
                :value-type (character :tag "To")))

(defcustom moder-goto-line-function nil
  "Function to use in `moder-goto-line'.

Nil means find the command by key binding."
  :group 'moder
  :type '(choice function (const nil)))

(defvar moder-state-mode-alist
  '((normal . moder-normal-mode)
    (insert . moder-insert-mode)
    (keypad . moder-keypad-mode)
    (motion . moder-motion-mode)
    (beacon . moder-beacon-mode))
  "Alist of moder states -> modes")

(defvar moder-update-cursor-functions-alist
  '((moder--cursor-null-p . moder--update-cursor-default)
    (minibufferp         . moder--update-cursor-default)
    (moder-insert-mode-p  . moder--update-cursor-insert)
    (moder-normal-mode-p  . moder--update-cursor-normal)
    (moder-motion-mode-p  . moder--update-cursor-motion)
    (moder-keypad-mode-p  . moder--update-cursor-motion)
    (moder-beacon-mode-p  . moder--update-cursor-beacon)
    ((lambda () t)       . moder--update-cursor-default))
  "Alist of predicates to functions that set cursor type and color.")

(defvar moder-keypad-describe-keymap-function 'moder-describe-keymap
  "The function used to describe (KEYMAP) during keypad execution.

To integrate WhichKey-like features with keypad.
Currently, keypad is not working well with which-key,
so Moder ships a default `moder-describe-keymap'.
Use (setq moder-keypad-describe-keymap-function \\='nil) to disable popup.")

(defvar moder-keypad-clear-describe-keymap-function nil
  "The function used to clear the effect of `moder-keypad-describe-keymap-function'.")

(defvar moder-keypad-get-title-function 'moder-keypad-get-title
  "The function used to get the title of a keymap or command.")

;; Cursor types

(defvar moder-cursor-type-default 'box)
(defvar moder-cursor-type-normal 'box)
(defvar moder-cursor-type-motion 'box)
(defvar moder-cursor-type-beacon 'box)
(defvar moder-cursor-type-region-cursor '(bar . 2))
(defvar moder-cursor-type-insert '(bar . 2))
(defvar moder-cursor-type-keypad 'hollow)

;; Keypad states

(defvar moder--keypad-keys nil)
(defvar moder--keypad-previous-state nil)

(defvar moder--prefix-arg nil)
(defvar moder--use-literal nil)
(defvar moder--use-meta nil)
(defvar moder--use-both nil)

;;; KBD Macros
;; We use kbd macro instead of direct command/function invocation,
;; this allows us to avoid hard coding the command/function name.
;;
;; The benefit is an out-of-box integration support for other plugins, like: paredit.
;;
;; NOTE: moder assumes that the user does not modify vanilla Emacs keybindings, otherwise extra complexity will be introduced.

(defvar moder--kbd-undo "C-/"
  "KBD macro for command `undo'.")

(defvar moder--kbd-backward-char "C-b"
  "KBD macro for command `backward-char'.")

(defvar moder--kbd-forward-char "C-f"
  "KBD macro for command `forward-char'.")

(defvar moder--kbd-keyboard-quit "C-g"
  "KBD macro for command `keyboard-quit'.")

(defvar moder--kbd-find-ref "M-."
  "KBD macro for command `xref-find-definitions'.")

(defvar moder--kbd-pop-marker "M-,"
  "KBD macro for command `xref-pop-marker-stack'.")

(defvar moder--kbd-comment "M-;"
  "KBD macro for comment command.")

(defvar moder--kbd-kill-line "C-k"
  "KBD macro for command `kill-line'.")

(defvar moder--kbd-kill-whole-line "<C-S-backspace>"
  "KBD macro for command `kill-whole-line'.")

(defvar moder--kbd-delete-char "C-d"
  "KBD macro for command `delete-char'.")

(defvar moder--kbd-yank "C-y"
  "KBD macro for command `yank'.")

(defvar moder--kbd-yank-pop "M-y"
  "KBD macro for command `yank-pop'.")

(defvar moder--kbd-kill-ring-save "M-w"
  "KBD macro for command `kill-ring-save'.")

(defvar moder--kbd-kill-region "C-w"
  "KBD macro for command `kill-region'.")

(defvar moder--kbd-exchange-point-and-mark "C-x C-x"
  "KBD macro for command `exchange-point-and-mark'.")

(defvar moder--kbd-back-to-indentation "M-m"
  "KBD macro for command `back-to-indentation'.")

(defvar moder--kbd-indent-region "C-M-\\"
  "KBD macro for command `indent-region'.")

(defvar moder--kbd-delete-indentation "M-^"
  "KBD macro for command `delete-indentation'.")

(defvar moder--kbd-forward-slurp "C-)"
  "KBD macro for command forward slurp.")

(defvar moder--kbd-backward-slurp "C-("
  "KBD macro for command backward slurp.")

(defvar moder--kbd-forward-barf "C-}"
  "KBD macro for command forward barf.")

(defvar moder--kbd-backward-barf "C-{"
  "KBD macro for command backward barf.")

(defvar moder--kbd-scoll-up "C-v"
  "KBD macro for command `scroll-up'.")

(defvar moder--kbd-scoll-down "M-v"
  "KBD macro for command `scroll-down'.")

(defvar moder--kbd-just-one-space "M-SPC"
  "KBD macro for command `just-one-space.")

(defvar moder--kbd-wrap-round "M-("
  "KBD macro for command wrap round.")

(defvar moder--kbd-wrap-square "M-["
  "KBD macro for command wrap square.")

(defvar moder--kbd-wrap-curly "M-{"
  "KBD macro for command wrap curly.")

(defvar moder--kbd-wrap-string "M-\""
  "KBD macro for command wrap string.")

(defvar moder--kbd-excute-extended-command "M-x"
  "KBD macro for command `execute-extended-command'.")

(defvar moder--kbd-transpose-sexp "C-M-t"
  "KBD macro for command transpose sexp.")

(defvar moder--kbd-split-sexp "M-S"
  "KBD macro for command split sexp.")

(defvar moder--kbd-splice-sexp "M-s"
  "KBD macro for command splice sexp.")

(defvar moder--kbd-raise-sexp "M-r"
  "KBD macro for command raise sexp.")

(defvar moder--kbd-join-sexp "M-J"
  "KBD macro for command join sexp.")

(defvar moder--kbd-eval-last-exp "C-x C-e"
  "KBD macro for command eval last exp.")

(defvar moder--kbd-query-replace-regexp "C-M-%"
  "KBD macro for command `query-replace-regexp'.")

(defvar moder--kbd-query-replace "M-%"
  "KBD macro for command `query-replace'.")

(defvar moder--kbd-forward-line "C-n"
  "KBD macro for command `forward-line'.")

(defvar moder--kbd-backward-line "C-p"
  "KBD macro for command `backward-line'.")

(defvar moder--kbd-search-forward-regexp "C-M-s"
  "KBD macro for command `search-forward-regexp'.")

(defvar moder--kbd-search-backward-regexp "C-M-r"
  "KBD macro for command `search-backward-regexp'.")

(defvar moder--kbd-goto-line "M-g g"
  "KBD macro for command `goto-line'.")

(defvar moder--delete-region-function #'delete-region
  "The function used to delete the selection.

Allows support of modes that define their own equivalent of
`delete-region'.")

(defvar moder--insert-function #'insert
  "The function used to insert text in Normal state.

Allows support of modes that define their own equivalent of `insert'.")

(defvar-local moder--indicator nil
  "Indicator for current buffer.")

(defvar-local moder--selection nil
  "Current selection.

Has a structure of (sel-type point mark).")

(defvar moder--kbd-pop-global-mark "C-x C-@"
  "KBD macro for command `pop-global-mark'.")

;;; Hooks

(defvar moder-switch-state-hook nil
  "Hooks run when switching state.")

(defvar moder-insert-enter-hook nil
  "Hooks run when enter insert state.")

(defvar moder-insert-exit-hook nil
  "Hooks run when exit insert state.")

;;; Internal variables

(defvar-local moder--current-state 'normal
  "A symbol represent current state.")

(defvar-local moder--end-kmacro-on-exit nil
  "Whether we end kmacro recording when exit insert state.")

(defvar-local moder--temp-normal nil
  "Whether we are in temporary normal state. ")

(defvar moder--selection-history nil
  "The history of selections.")

(defvar moder--expand-nav-function nil
  "Current expand nav function.")

(defvar moder--last-find nil
  "The char for last find command.")

(defvar moder--last-till nil
  "The char for last till command.")

(defvar moder--visual-command nil
  "Current command to highlight.")

(defvar moder--keypad-this-command nil
  "Command name for current keypad execution.")

(defvar moder--expanding-p nil
  "Whether we are expanding.")

(defvar moder--keypad-keymap-description-activated nil
  "Whether KEYPAD keymap description is already activated.")

(defvar moder--keypad-help nil
  "If keypad in help mode.")

(defvar moder--keypad-base-keymap nil
  "The keymap used to lookup keys in KEYPAD state.

Nil means to lookup in top-level.")

(defvar moder--beacon-backup-hl-line
  nil
  "Whether hl-line is enabled by user.")

(defvar moder--beacon-defining-kbd-macro nil
  "Whether we are defining kbd macro at BEACON state.

The value can be nil, quick or record.")

(defvar-local moder--insert-pos nil
  "The position where we enter INSERT state.")

(defvar-local moder--insert-activate-mark nil
  "Whether we should activate the selection after exiting INSERT state.")

(defvar moder-full-width-number-position-chars
  '((0 . "０")
    (1 . "１")
    (2 . "２")
    (3 . "３")
    (4 . "４")
    (5 . "５")
    (6 . "６")
    (7 . "７")
    (8 . "８")
    (9 . "９"))
  "Map number to full-width character.")

(defvar moder-cheatsheet-ellipsis "…"
  "Ellipsis character used in cheatsheet.")

(defvar moder-command-to-short-name-list
  '((moder-expand-0 . "ex →0")
    (moder-expand-1 . "ex →1")
    (moder-expand-2 . "ex →2")
    (moder-expand-3 . "ex →3")
    (moder-expand-4 . "ex →4")
    (moder-expand-5 . "ex →5")
    (moder-expand-6 . "ex →6")
    (moder-expand-7 . "ex →7")
    (moder-expand-8 . "ex →8")
    (moder-expand-9 . "ex →9")
    (digit-argument . "num-arg")
    (moder-inner-of-thing . "←thing→")
    (moder-bounds-of-thing . "[thing]")
    (moder-beginning-of-thing . "←thing")
    (moder-end-of-thing . "thing→")
    (moder-reverse . "reverse")
    (moder-prev . "↑")
    (moder-prev-expand . "ex ↑")
    (moder-next . "↓")
    (moder-next-expand . "ex ↓")
    (moder-head . "←")
    (moder-head-expand . "ex ←")
    (moder-tail . "→")
    (moder-tail-expand . "ex →")
    (moder-left . "←")
    (moder-left-expand . "ex ←")
    (moder-right . "→")
    (moder-right-expand . "ex →")
    (moder-yank . "yank")
    (moder-find . "find")
    (moder-find-expand . "ex find")
    (moder-till . "till")
    (moder-till-expand . "ex till")
    (moder-keyboard-quit . "C-g")
    (moder-cancel-selection . "quit sel")
    (moder-change . "chg")
    (moder-change-save . "chg-save")
    (moder-replace . "rep")
    (moder-replace-save . "rep-save")
    (moder-append . "append")
    (moder-open-below . "open ↓")
    (moder-insert . "insert")
    (moder-open-above . "open ↑")
    (moder-block . "block")
    (moder-to-block "→block")
    (moder-line . "line")
    (moder-delete . "del")
    (moder-search . "search")
    (moder-undo . "undo")
    (moder-undo-in-selection . "undo-sel")
    (moder-pop-search . "popsearch")
    (negative-argument . "neg-arg")
    (moder-quit . "quit")
    (moder-join . "join")
    (moder-kill . "kill")
    (moder-save . "save")
    (moder-next-word . "word→")
    (moder-next-symbol . "sym→")
    (moder-back-word . "←word")
    (moder-back-symbol . "←sym")
    (moder-pop-all-selection . "pop-sels")
    (moder-pop-selection . "pop-sel")
    (moder-mark-word . "←word→")
    (moder-mark-symbol . "←sym→")
    (moder-visit . "visit")
    (moder-kmacro-lines . "macro-ln")
    (moder-kmacro-matches . "macro-re")
    (moder-end-or-call-kmacro . "callmacro")
    (moder-cheatsheet . "help")
    (moder-keypad-describe-key . "desc-key")
    (moder-backspace . "backspace")
    (moder-pop-to-mark . "<-mark")
    (moder-unpop-to-mark . "mark->"))
  "A list of (command . short-name)")

(defcustom moder-replace-pop-command-start-indexes
  '((moder-replace . 1)
    (moder-replace-char . 1)
    (moder-replace-save . 2))
  "Alist of commands and their starting indices for use by `moder-replace-pop'.

If `moder-replace-pop' is run and the previous command is not
`moder-replace-pop' or a command which is present in this alist,
`moder-replace-pop' signals an error."
  :type '(alist :key-type function :value-type natnum))

(defcustom moder-keypad-message-prefix "Keypad: "
  "The prefix string for keypad messages."
  :type 'string)

(defvar moder--replace-pop-index nil
  "The index of the previous replacement in the `kill-ring'.
See also the command `moder-replace-pop'.")

(defvar moder--replace-start-marker (make-marker)
  "The beginning of the replaced text.

This marker stays before any text inserted at the location, to
account for any automatic formatting that happens after inserting
the replacement text.")

;;; Backup variables

(defvar moder--backup-var-delete-activae-region nil
  "The backup for `delete-active-region'.

It is used to restore its value when disable `moder'.")

(defvar moder--backup-redisplay-highlight-region-function
  redisplay-highlight-region-function)

(defvar moder--backup-redisplay-unhighlight-region-function
  redisplay-unhighlight-region-function)

(defvar moder--backup-var-delete-activate-region
  delete-active-region)

(provide 'moder-var)
;;; moder-var.el ends here
