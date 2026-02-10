;;; moder-util.el --- Utilities for Moder  -*- lexical-binding: t; -*-

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
;; Utilities for Moder.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'color)

(require 'moder-var)
(require 'moder-keymap)
(require 'moder-face)

;; Modes

(defvar moder-normal-mode)

(declare-function moder--remove-match-highlights "moder-visual")
(declare-function moder--remove-expand-highlights "moder-visual")
(declare-function moder--remove-search-highlight "moder-visual")
(declare-function moder-insert-mode "moder-core")
(declare-function moder-motion-mode "moder-core")
(declare-function moder-normal-mode "moder-core")
(declare-function moder-keypad-mode "moder-core")
(declare-function moder-beacon-mode "moder-core")
(declare-function moder-mode "moder-core")
(declare-function moder--keypad-format-keys "moder-keypad")
(declare-function moder--keypad-format-prefix "moder-keypad")
(declare-function moder-minibuffer-quit "moder-command")
(declare-function moder--enable "moder-core")
(declare-function moder--beacon-apply-command "moder-beacon")
(declare-function moder-keypad-start-with "moder-keypad")

(defun moder--execute-kbd-macro (kbd-macro-or-defun)
  "Execute the function bound to `KBD-MACRO-OR-DEFUN'. If `KBD-MACRO-OR-DEFUN' is a string,
instead execute the keyboard macro it corresponds to."
  (when-let* ((ret (if (and (symbolp kbd-macro-or-defun) (fboundp kbd-macro-or-defun))
                       kbd-macro-or-defun
                     (key-binding (read-kbd-macro kbd-macro-or-defun)))))
    (cond
     ((commandp ret)
      (setq this-command ret)
      (call-interactively ret))

     ((and (not moder-use-keypad-when-execute-kbd) (keymapp ret))
      (set-transient-map ret nil nil))

     ((and moder-use-keypad-when-execute-kbd (keymapp ret))
      (moder-keypad-start-with kbd-macro-or-defun)))))

(defun moder-insert-mode-p ()
  "Whether insert mode is enabled."
  (bound-and-true-p moder-insert-mode))

(defun moder-motion-mode-p ()
  "Whether motion mode is enabled."
  (bound-and-true-p moder-motion-mode))

(defun moder-normal-mode-p ()
  "Whether normal mode is enabled."
  (bound-and-true-p moder-normal-mode))

(defun moder-keypad-mode-p ()
  "Whether keypad mode is enabled."
  (bound-and-true-p moder-keypad-mode))

(defun moder-beacon-mode-p ()
  "Whether keypad mode is enabled."
  (bound-and-true-p moder-beacon-mode))

(defun moder--disable-current-state ()
  (when moder--current-state
    (funcall (alist-get moder--current-state moder-state-mode-alist) -1)
    (setq moder--current-state nil)))

(defun moder--read-cursor-face-color (face)
  "Read cursor color from face."
  (let ((f (face-attribute face :inherit)))
    (if (equal 'unspecified f)
        (let ((color (face-attribute face :background)))
          (if (equal 'unspecified color)
              (face-attribute 'default :foreground)
            color))
      (moder--read-cursor-face-color f))))

(defun moder--set-cursor-type (type)
  (if (display-graphic-p)
      (setq cursor-type type)
    (let* ((shape (or (car-safe type) type))
           (param (cond ((eq shape 'bar) "6")
                        ((eq shape 'hbar) "4")
                        (t "2"))))
      (send-string-to-terminal (concat "\e[" param " q")))))

(defun moder--set-cursor-color (face)
  "Set cursor color by face."
  (let ((color (moder--read-cursor-face-color face)))
    (unless (equal (frame-parameter nil 'cursor-color) color)
      (set-cursor-color color))))

(defun moder--update-cursor-default ()
  "Set default cursor type and color"
  (moder--set-cursor-type moder-cursor-type-default)
  (moder--set-cursor-color 'moder-unknown-cursor))

(defun moder--update-cursor-insert ()
  "Set insert cursor type and color"
  (moder--set-cursor-type moder-cursor-type-insert)
  (moder--set-cursor-color 'moder-insert-cursor))

(defun moder--update-cursor-normal ()
  "Set normal cursor type and color"
  (if moder-use-cursor-position-hack
      (unless (use-region-p)
        (moder--set-cursor-type moder-cursor-type-normal))
    (moder--set-cursor-type moder-cursor-type-normal))
  (moder--set-cursor-color 'moder-normal-cursor))

(defun moder--update-cursor-motion ()
  "Set motion cursor type and color"
  (moder--set-cursor-type moder-cursor-type-motion)
  (moder--set-cursor-color 'moder-motion-cursor))

(defun moder--update-cursor-beacon ()
  "Set beacon cursor type and color"
  (moder--set-cursor-type moder-cursor-type-beacon)
  (moder--set-cursor-color 'moder-beacon-cursor))

(defun moder--cursor-null-p ()
  "Check if cursor-type is null"
  (null cursor-type))

(defun moder--update-cursor ()
  "Update cursor type according to the current state.

This uses the variable moder-update-cursor-functions-alist, finds the first
item in which the car evaluates to true, and runs the cdr. The last item's car
in the list will always evaluate to true."
  (with-current-buffer (window-buffer)
    (thread-last moder-update-cursor-functions-alist
                 (cl-remove-if-not (lambda (el) (funcall (car el))))
                 (cdar)
                 (funcall))))

(defun moder--get-state-name (state)
  "Get the name of the current state.

Looks up the state in moder-replace-state-name-list"
  (alist-get state moder-replace-state-name-list))

(defun moder--render-indicator ()
  "Renders a short indicator based on the current state."
  (when (bound-and-true-p moder-global-mode)
    (let* ((state (moder--current-state))
           (state-name (moder--get-state-name state))
           (indicator-face (alist-get state moder-indicator-face-alist)))
      (if state-name
          (propertize
           (format " %s " state-name)
           'face indicator-face)
        ""))))

(defun moder--update-indicator ()
  (let ((indicator (moder--render-indicator)))
    (setq-local moder--indicator indicator)))

(defun moder--state-p (state)
  (funcall (intern (concat "moder-" (symbol-name state) "-mode-p"))))

(defun moder--current-state ()
  moder--current-state)

(defun moder--should-update-display-p ()
  (cl-case moder-update-display-in-macro
    ((t) t)
    ((except-last-macro)
     (or (null executing-kbd-macro)
         (not (equal executing-kbd-macro last-kbd-macro))))
    ((nil)
     (null executing-kbd-macro))))

(defun moder-update-display ()
  (when (moder--should-update-display-p)
    (moder--update-indicator)
    (moder--update-cursor)))

(defun moder--switch-state (state &optional no-hook)
  "Switch to STATE execute `moder-switch-state-hook' unless NO-HOOK is non-nil."
  (unless (eq state (moder--current-state))
    (let ((mode (alist-get state moder-state-mode-alist)))
      (funcall mode 1))
    (unless (bound-and-true-p no-hook)
      (run-hook-with-args 'moder-switch-state-hook state))))

(defvar moder--beacon-apply-command "moder-beacon")

(defun moder--exit-keypad-state ()
  "Exit keypad state."
  (moder-keypad-mode -1)
  (when (and (eq 'beacon moder--keypad-previous-state)
             moder--current-state)
    (moder--beacon-apply-command moder--keypad-this-command))
  (when moder--keypad-previous-state
    (moder--switch-state moder--keypad-previous-state)))

(defun moder--direction-forward ()
  "Make the selection towards forward."
  (when (and (region-active-p) (< (point) (mark)))
    (exchange-point-and-mark)))

(defun moder--direction-backward ()
  "Make the selection towards backward."
  (when (and (region-active-p) (> (point) (mark)))
    (exchange-point-and-mark)))

(defun moder--direction-backward-p ()
  "Return whether we have a backward selection."
  (and (region-active-p)
       (> (mark) (point))))

(defun moder--direction-forward-p ()
  "Return whether we have a forward selection."
  (and (region-active-p)
       (<= (mark) (point))))

(defun moder--selection-type ()
  "Return current selection type."
  (when (region-active-p)
    (car moder--selection)))

(defun moder--in-string-p (&optional pos)
  "Return whether POS or current position is in string."
  (save-mark-and-excursion
    (when pos (goto-char pos))
    (nth 3 (syntax-ppss))))

(defun moder--in-comment-p (&optional pos)
  "Return whether POS or current position is in string."
  (save-mark-and-excursion
    (when pos (goto-char pos))
    (nth 4 (syntax-ppss))))

(defun moder--sum (sequence)
  (seq-reduce #'+ sequence 0))

(defun moder--reduce (fn init sequence)
  (seq-reduce fn sequence init))

(defun moder--string-pad (s len pad &optional start)
  (if (<= len (length s))
      s
    (if start
        (concat (make-string (- len (length s)) pad) s)
      (concat s (make-string (- len (length s)) pad)))))

(defun moder--truncate-string (len s ellipsis)
  (if (> (length s) len)
      (concat (substring s 0 (- len (length ellipsis))) ellipsis)
    s))

(defun moder--string-join (sep s)
  (string-join s sep))

(defun moder--prompt-symbol-and-words (prompt beg end &optional disallow-empty)
  "Completion with PROMPT for symbols and words from BEG to END."
  (let ((completions))
    (save-mark-and-excursion
      (goto-char beg)
      (while (re-search-forward "\\_<\\(\\sw\\|\\s_\\)+\\_>" end t)
        (let ((result (match-string-no-properties 0)))
          (when (>= (length result) moder-visit-collect-min-length)
            (if moder-visit-sanitize-completion
                (push (cons result (format "\\_<%s\\_>" (regexp-quote result))) completions)
              (push (format "\\_<%s\\_>" (regexp-quote result)) completions))))))
    (setq completions (delete-dups completions))
    (let ((selected (completing-read prompt completions nil nil)))
      (while (and (string-empty-p selected)
                  disallow-empty)
        (setq selected (completing-read
                        (concat "[Input must be non-empty] " prompt)
                        completions nil nil)))
      (if moder-visit-sanitize-completion
          (or (cdr (assoc selected completions))
              (regexp-quote selected))
        selected))))

(defun moder--on-window-state-change (&rest _args)
  "Update cursor style after switching window."
  (moder--update-cursor)
  (moder--update-indicator))

(defun moder--on-exit ()
  (unless (display-graphic-p)
    (send-string-to-terminal "\e[2 q")))

(defun moder--get-indent ()
  "Get indent of current line."
  (save-mark-and-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

(defun moder--empty-line-p ()
  "Whether current line is empty."
  (string-match-p "^ *$" (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))

(defun moder--ordinal (n)
  (cl-case n
    ((1) "1st")
    ((2) "2nd")
    ((3) "3rd")
    (t (format "%dth" n))))

(defun moder--allow-modify-p ()
  (and (not buffer-read-only)
       (not moder--temp-normal)))

(defun moder--with-universal-argument-p (arg)
  (equal '(4) arg))

(defun moder--with-negative-argument-p (arg)
  (< (prefix-numeric-value arg) 0))

(defun moder--with-shift-p ()
  (member 'shift last-input-event))

(defun moder--bounds-with-type (type thing)
  (when-let* ((bounds (bounds-of-thing-at-point thing)))
    (cons type bounds)))

(defun moder--insert (&rest args)
  "Use `moder--insert-function' to insert ARGS at point."
  (apply moder--insert-function args))

(defun moder--delete-region (start end)
  "Use `moder--delete-region-function' to delete text between START and END."
  (funcall moder--delete-region-function start end))

(defun moder--push-search (search)
  (unless (string-equal search (car regexp-search-ring))
    (add-to-history 'regexp-search-ring search regexp-search-ring-max)))

(defun moder--remove-text-properties (text)
  (set-text-properties 0 (length text) nil text)
  text)

(defun moder--toggle-relative-line-number ()
  (when display-line-numbers
    (if (bound-and-true-p moder-insert-mode)
        (setq display-line-numbers t)
      (setq display-line-numbers 'relative))))

(defun moder--render-char-thing-table ()
  (let* ((ww (frame-width))
         (w 25)
         (col (min 5 (/ ww w))))
    (thread-last
      moder-local-char-thing-table
      (seq-group-by #'cdr)
      (seq-sort-by #'caadr #'<)
      (seq-map-indexed
       (lambda (th-pairs idx)
         (let* ((th (car th-pairs))
                (pairs (cdr th-pairs))
                (pre (thread-last
                       pairs
                       (mapcar (lambda (it) (char-to-string (car it))))
                       (moder--string-join " "))))
           (format "%s%s%s%s"
                   (propertize
                    (moder--string-pad pre 8 32 t)
                    'face 'font-lock-constant-face)
                   (propertize " → " 'face 'font-lock-comment-face)
                   (propertize
                    (moder--string-pad (symbol-name th) 13 32 t)
                    'face 'font-lock-function-name-face)
                   (if (= (1- col) (mod idx col))
                       "\n"
                     " ")))))
      (string-join)
      (string-trim-right))))

(defun moder--transpose-lists (lists)
  (when lists
    (let* ((n (seq-max (mapcar #'length lists)))
           (rst (apply #'list (make-list n ()))))
      (mapc (lambda (l)
              (seq-map-indexed
               (lambda (it idx)
                 (cl-replace rst
                             (list (cons it (nth idx rst)))
                             :start1 idx
                             :end1 (1+ idx)))
               l))
            lists)
      (mapcar #'reverse rst))))

(defun moder--get-event-key (e)
  (if (and (integerp (event-basic-type e))
           (member 'shift (event-modifiers e)))
      (upcase (event-basic-type e))
    (event-basic-type e)))

(defun moder--ensure-visible ()
  (let ((overlays (overlays-at (1- (point))))
        ov expose)
    (while (setq ov (pop overlays))
      (if (and (invisible-p (overlay-get ov 'invisible))
               (setq expose (overlay-get ov 'isearch-open-invisible)))
          (funcall expose ov)))))

(defun moder--minibuffer-setup ()
  (local-set-key (kbd "<escape>") #'moder-minibuffer-quit)
  (setq-local moder-normal-mode nil)
  (when (or (member this-command moder-grab-fill-commands)
            (member moder--keypad-this-command moder-grab-fill-commands))
    (when-let* ((s (moder--second-sel-get-string)))
      (moder--insert s))))

(defun moder--parse-string-to-keypad-keys (str)
  (let ((strs (split-string str " ")))
    (thread-last
      strs
      (mapcar
       (lambda (str)
         (cond
          ((string-prefix-p "C-M-" str)
           (cons 'both (substring str 4)))
          ((string-prefix-p "C-" str)
           (cons 'control (substring str 2)))
          ((string-prefix-p "M-" str)
           (cons 'meta (substring str 2)))
          (t
           (cons 'literal str)))))
      (reverse))))

(defun moder--parse-input-event (e)
  (cond
   ((equal e 32)
    "SPC")
   ((characterp e)
    (string e))
   ((equal 'tab e)
    "TAB")
   ((equal 'return e)
    "RET")
   ((equal 'backspace e)
    "DEL")
   ((equal 'escape e)
    "ESC")
   ((symbolp e)
    (format "<%s>" e))
   (t nil)))

(defun moder--prepare-region-for-kill ()
  (when (and (equal 'line (cdr (moder--selection-type)))
             (moder--direction-forward-p)
             (< (point) (point-max)))
    (forward-char 1)))

(defun moder--prepare-string-for-kill-append (s)
  (let ((curr (current-kill 0 nil)))
    (cl-case (cdr (moder--selection-type))
      ((line) (concat (unless (string-suffix-p "\n" curr) "\n")
                      (string-trim-right s "\n")))
      ((word block) (concat (unless (string-suffix-p " " curr) " ")
                            (string-trim s " " "\n")))
      (t s))))

(defun moder--event-key (e)
  (let ((c (event-basic-type e)))
    (if (and (char-or-string-p c)
             (member 'shift (event-modifiers e)))
        (upcase c)
      c)))



(defun moder--make-button (string callback &optional data help-echo)
  "Copy from buttonize, which is available in Emacs 29.1"
  (let ((string
         (apply #'propertize string
                (list 'font-lock-face 'button
                      'mouse-face 'highlight
                      'help-echo help-echo
                      'button t
                      'follow-link t
                      'category t
                      'button-data data
                      'keymap button-map
                      'action callback))))
    ;; Add the face to the end so that it can be overridden.
    (add-face-text-property 0 (length string) 'button t string)
    string))

(defun moder--parse-def (def)
  "Return a command or keymap for DEF.

If DEF is a string, return a command that calls the command or keymap
that bound to DEF. Otherwise, return DEF."
  (if (stringp def)
      (let ((cmd-name (gensym 'moder-dispatch_)))
        ;; dispatch command
        (defalias cmd-name
          (lambda ()
            (:documentation
             (format "Execute the command which is bound to %s."
                     (moder--make-button def 'describe-key (kbd def))))
            (interactive)
            (moder--execute-kbd-macro def)))
        (put cmd-name 'moder-dispatch def)
        cmd-name)
    def))

(defun moder--second-sel-set-string (string)
  (cond
   ((moder--second-sel-buffer)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (goto-char (overlay-start mouse-secondary-overlay))
      (moder--delete-region (overlay-start mouse-secondary-overlay) (overlay-end mouse-secondary-overlay))
      (moder--insert string)))
   ((markerp mouse-secondary-start)
    (with-current-buffer (marker-buffer mouse-secondary-start)
      (goto-char (marker-position mouse-secondary-start))
      (moder--insert string)))))

(defun moder--second-sel-get-string ()
  (when (moder--second-sel-buffer)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (buffer-substring-no-properties
       (overlay-start mouse-secondary-overlay)
       (overlay-end mouse-secondary-overlay)))))

(defun moder--second-sel-buffer ()
  (and (overlayp mouse-secondary-overlay)
       (overlay-buffer mouse-secondary-overlay)))

(defun moder--second-sel-bound ()
  (and (secondary-selection-exist-p)
       (cons (overlay-start mouse-secondary-overlay)
             (overlay-end mouse-secondary-overlay))))

(defmacro moder--with-selection-fallback (&rest body)
  `(if (region-active-p)
       (progn ,@body)
     (moder--selection-fallback)))

(defmacro moder--wrap-collapse-undo (&rest body)
  "Like `progn' but perform BODY with undo collapsed."
  (declare (indent 0) (debug t))
  (let ((handle (make-symbol "--change-group-handle--"))
        (success (make-symbol "--change-group-success--")))
    `(let ((,handle (prepare-change-group))
           ;; Don't truncate any undo data in the middle of this.
           (undo-outer-limit nil)
           (undo-limit most-positive-fixnum)
           (undo-strong-limit most-positive-fixnum)
           (,success nil))
       (unwind-protect
           (progn
             (activate-change-group ,handle)
             (prog1 ,(macroexp-progn body)
               (setq ,success t)))
         (if ,success
             (progn
               (accept-change-group ,handle)
               (undo-amalgamate-change-group ,handle))
           (cancel-change-group ,handle))))))

(defun moder--highlight-pre-command ()
  (unless (member this-command '(moder-search))
    (moder--remove-match-highlights))
  (moder--remove-expand-highlights)
  (moder--remove-search-highlight))

(defun moder--remove-fake-cursor (rol)
  (when (overlayp rol)
    (when-let* ((ovs (overlay-get rol 'moder-face-cursor)))
      (mapc (lambda (o) (when (overlayp o) (delete-overlay o)))
            ovs))))

(defvar moder--region-cursor-faces '(moder-region-cursor-1
                                     moder-region-cursor-2
                                     moder-region-cursor-3))

(defun moder--add-fake-cursor (rol)
  (if (and moder-use-enhanced-selection-effect
           (or (moder-normal-mode-p)
               (moder-beacon-mode-p)))
      (when (overlayp rol)
        (let ((start (overlay-start rol))
              (end (overlay-end rol)))
          (unless (= start end)
            (let (ovs)
              (if (moder--direction-forward-p)
                  (progn
                    (let ((p end)
                          (i 0))
                      (while (and (> p start)
                                  (< i 3))
                        (let ((ov (make-overlay (1- p) p)))
                          (overlay-put ov 'face (nth i moder--region-cursor-faces))
                          (overlay-put ov 'priority 10)
                          (overlay-put ov 'window (overlay-get rol 'window))
                          (cl-decf p)
                          (cl-incf i)
                          (push ov ovs)))))
                (let ((p start)
                      (i 0))
                  (while (and (< p end)
                              (< i 3))
                    (let ((ov (make-overlay p (1+ p))))
                      (overlay-put ov 'face (nth i moder--region-cursor-faces))
                      (overlay-put ov 'priority 10)
                      (overlay-put ov 'window (overlay-get rol 'window))
                      (cl-incf p)
                      (cl-incf i)
                      (push ov ovs)))))
              (overlay-put rol 'moder-face-cursor ovs)))
          rol))
    rol))

(defun moder--redisplay-highlight-region-function (start end window rol)
  (when (and (or (moder-normal-mode-p)
                 (moder-beacon-mode-p))
             (equal window (selected-window)))
    (if (use-region-p)
        (moder--set-cursor-type moder-cursor-type-region-cursor)
      (moder--set-cursor-type moder-cursor-type-normal)))
  (when moder-use-enhanced-selection-effect
    (moder--remove-fake-cursor rol))
  (thread-first
    (funcall moder--backup-redisplay-highlight-region-function start end window rol)
    (moder--add-fake-cursor)))

(defun moder--redisplay-unhighlight-region-function (rol)
  (moder--remove-fake-cursor rol)
  (when (and (overlayp rol)
             (equal (overlay-get rol 'window) (selected-window))
             (or (moder-normal-mode-p)
                 (moder-beacon-mode-p)))
    (moder--set-cursor-type moder-cursor-type-normal))
  (funcall moder--backup-redisplay-unhighlight-region-function rol))

(defun moder--mix-color (color1 color2 n)
  (mapcar (lambda (c) (apply #'color-rgb-to-hex c))
          (color-gradient (color-name-to-rgb color1)
                          (color-name-to-rgb color2)
                          n)))

(defun moder--beacon-inside-secondary-selection ()
  (and
   (secondary-selection-exist-p)
   (< (overlay-start mouse-secondary-overlay)
      (overlay-end mouse-secondary-overlay))
   (<= (overlay-start mouse-secondary-overlay)
       (point)
       (overlay-end mouse-secondary-overlay))))

(defun moder--narrow-secondary-selection ()
  (narrow-to-region (overlay-start mouse-secondary-overlay)
                    (overlay-end mouse-secondary-overlay)))

(defun moder--hack-cursor-pos (pos)
  "Hack the point when `moder-use-cursor-position-hack' is enabled."
  (if moder-use-cursor-position-hack
      (1- pos)
    pos))

(defun moder--remove-modeline-indicator ()
  (setq-default mode-line-format
                (cl-remove '(:eval (moder-indicator)) mode-line-format
                           :test 'equal)))

(defun moder--init-buffers ()
  "Enable moder in existing buffers."
  (dolist (buf (buffer-list))
    (unless (minibufferp buf)
      (with-current-buffer buf
        (moder--enable)))))

(defun moder--get-leader-keymap ()
  (cond
   ((keymapp moder-keypad-leader-dispatch)
    moder-keypad-leader-dispatch)

   ((null moder-keypad-leader-dispatch)
    (alist-get 'leader moder-keymap-alist))))

(provide 'moder-util)
;;; moder-util.el ends here
