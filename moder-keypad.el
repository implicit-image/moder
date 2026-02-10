;;; moder-keypad.el --- Moder keypad mode -*- lexical-binding: t -*-

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
;; Keypad state is a special state to simulate C-x and C-c key sequences.
;;
;; Useful commands:
;;
;; moder-keypad
;; Enter keypad state.
;;
;; moder-keypad-start
;; Enter keypad state, and simulate this key with Control modifier.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'moder-var)
(require 'moder-util)
(require 'moder-helpers)
(require 'moder-beacon)

(defun moder--keypad-format-upcase (k)
  "Return S-k for upcase K."
  (let ((case-fold-search nil))
    (if (and (stringp k)
             (string-match-p "^[A-Z]$" k))
        (format "S-%s" (downcase k))
      k)))

(defun moder--keypad-format-key-1 (key)
  "Return a display format for input KEY."
  (cl-case (car key)
    (meta (format "M-%s" (cdr key)))
    (control (format "C-%s" (moder--keypad-format-upcase (cdr key))))
    (both (format "C-M-%s" (moder--keypad-format-upcase (cdr key))))
    (literal (cdr key))))

(defun moder--keypad-format-prefix ()
  "Return a display format for current prefix."
  (cond
   ((equal '(4) moder--prefix-arg)
    "C-u ")
   (moder--prefix-arg
    (format "%s " moder--prefix-arg))
   (t "")))

(defun moder--keypad-lookup-key (keys)
  "Lookup the command which is bound at KEYS."
  (let* ((keybind (if moder--keypad-base-keymap
                      (lookup-key moder--keypad-base-keymap keys)
                    (key-binding keys))))
    keybind))

(defun moder--keypad-has-sub-meta-keymap-p ()
  "Check if there's a keymap belongs to Meta prefix.

A key sequences starts with ESC is accessible via Meta key."
  (and (not moder--use-literal)
       (not moder--use-both)
       (not moder--use-meta)
       (or (not moder--keypad-keys)
           (let* ((key-str (moder--keypad-format-keys nil))
                  (keymap (moder--keypad-lookup-key (kbd key-str))))
             (and (keymapp keymap)
                  (lookup-key keymap ""))))))

(defun moder--keypad-format-keys (&optional prompt)
  "Return a display format for current input keys.

The message is prepended with an optional PROMPT."
  (let ((result ""))
    (setq result
          (thread-first
            (mapcar #'moder--keypad-format-key-1 moder--keypad-keys)
            (reverse)
            (string-join " ")))
    (cond
     (moder--use-both
      (setq result
            (if (string-empty-p result)
                "C-M-"
              (concat result " C-M-"))))
     (moder--use-meta
      (setq result
            (if (string-empty-p result)
                "M-"
              (concat result " M-"))))
     (moder--use-literal
      (setq result (concat result " ○")))

     (prompt
      (setq result (concat result " C-"))))
    result))

(defun moder--keypad-quit ()
  "Quit keypad state."
  (setq moder--keypad-keys nil
        moder--use-literal nil
        moder--use-meta nil
        moder--use-both nil
        moder--keypad-help nil)
  (moder--keypad-clear-message)
  (moder--exit-keypad-state)
  ;; Return t to indicate the keypad loop should be stopped
  t)

(defun moder-keypad-quit ()
  "Quit keypad state."
  (interactive)
  (setq this-command last-command)
  (when moder-keypad-message
    (message "KEYPAD exit"))
  (moder--keypad-quit))

(defun moder--make-keymap-for-describe (keymap control)
  "Parse the KEYMAP to make it suitable for describe.

Argument CONTROL, non-nils stands for current input is prefixed with Control."
  (let ((km (make-keymap)))
    (suppress-keymap km t)
    (when (keymapp keymap)
      (map-keymap
       (lambda (key def)
         (unless (member (event-basic-type key) '(127))
           (when (if control (member 'control (event-modifiers key))
                   (not (member 'control (event-modifiers key))))
             (define-key km (vector (moder--get-event-key key))
                         (funcall moder-keypad-get-title-function def)))))
       keymap))
    km))

(defun moder--keypad-get-keymap-for-describe ()
  "Get a keymap for describe."
  (let* ((input (thread-first
                  (mapcar #'moder--keypad-format-key-1 moder--keypad-keys)
                  (reverse)
                  (string-join " ")))
         (meta-both-keymap (moder--keypad-lookup-key
                            (read-kbd-macro
                             (if (string-blank-p input)
                                 "ESC"
                               (concat input " ESC"))))))
    (cond
     (moder--use-meta
      (when meta-both-keymap
        (moder--make-keymap-for-describe meta-both-keymap nil)))
     (moder--use-both
      (when meta-both-keymap
        (moder--make-keymap-for-describe meta-both-keymap t)))
     (moder--use-literal
      (when-let* ((keymap (moder--keypad-lookup-key (read-kbd-macro input))))
        (when (keymapp keymap)
          (moder--make-keymap-for-describe keymap nil))))

     ;; For leader popup
     ;; moder-keypad-leader-dispatch can be string, keymap or nil
     ;; - string, dynamically find the keymap
     ;; - keymap, just use it
     ;; - nil, take the one in moder-keymap-alist
     ;; Leader keymap may contain moder-dispatch commands
     ;; translated names based on the commands they refer to
     ((null moder--keypad-keys)
      (when-let* ((keymap (if (stringp moder-keypad-leader-dispatch)
                              (moder--keypad-lookup-key (read-kbd-macro moder-keypad-leader-dispatch))
                            (or moder-keypad-leader-dispatch
                                (alist-get 'leader moder-keymap-alist)))))
        (let ((km (make-keymap)))
          (suppress-keymap km t)
          (map-keymap
           (lambda (key def)
             (when (and (not (member 'control (event-modifiers key)))
                        (not (member key (list moder-keypad-meta-prefix
                                               moder-keypad-ctrl-meta-prefix
                                               moder-keypad-literal-prefix)))
                        (not (alist-get key moder-keypad-start-keys)))
               (let ((keys (vector (moder--get-event-key key))))
                 (unless (lookup-key km keys)
                   (define-key km keys (funcall moder-keypad-get-title-function def))))))
           keymap)
          km)))

     (t
      (when-let* ((keymap (moder--keypad-lookup-key (read-kbd-macro input))))
        (when (keymapp keymap)
          (let* ((km (make-keymap))
                 (has-sub-meta (moder--keypad-has-sub-meta-keymap-p))
                 (ignores (if has-sub-meta
                              (list moder-keypad-meta-prefix
                                    moder-keypad-ctrl-meta-prefix
                                    moder-keypad-literal-prefix
                                    127)
                            (list moder-keypad-literal-prefix 127))))
            (suppress-keymap km t)
            (map-keymap
             (lambda (key def)
               (when (member 'control (event-modifiers key))
                 (unless (member (moder--event-key key) ignores)
                   (when def
                     (let ((k (vector (moder--get-event-key key))))
                       (unless (lookup-key km k)
                         (define-key km k (funcall moder-keypad-get-title-function def))))))))
             keymap)
            (map-keymap
             (lambda (key def)
               (unless (member 'control (event-modifiers key))
                 (unless (member key ignores)
                   (let ((k (vector (moder--get-event-key key))))
                     (unless (lookup-key km k)
                       (define-key km (vector (moder--get-event-key key)) (funcall moder-keypad-get-title-function def)))))))
             keymap)
            km)))))))

(defun moder--keypad-clear-message ()
  "Clear displayed message by calling `moder-keypad-clear-describe-keymap-function'."
  (when moder-keypad-clear-describe-keymap-function
    (funcall moder-keypad-clear-describe-keymap-function)))

(defun moder--keypad-display-message ()
  "Display a message for current input state."
  (when moder-keypad-describe-keymap-function
    (when (or
           moder--keypad-keymap-description-activated

           (setq moder--keypad-keymap-description-activated
                 (sit-for moder-keypad-describe-delay t)))
      (let ((keymap (moder--keypad-get-keymap-for-describe)))
        (funcall moder-keypad-describe-keymap-function keymap)))))

(defun moder--describe-keymap-format (pairs &optional width)
  (let* ((fw (or width (frame-width)))
         (cnt (length pairs))
         (best-col-w nil)
         (best-rows nil))
    (cl-loop for col from 5 downto 2  do
             (let* ((row (1+ (/ cnt col)))
                    (v-parts (seq-partition pairs row))
                    (rows (moder--transpose-lists v-parts))
                    (col-w (thread-last
                             v-parts
                             (mapcar
                              (lambda (col)
                                (cons (seq-max (or (mapcar (lambda (it) (length (car it))) col) '(0)))
                                      (seq-max (or (mapcar (lambda (it) (length (cdr it))) col) '(0))))))))
                    ;; col-w looks like:
                    ;; ((3 . 2) (4 . 3))
                    (w (thread-last
                         col-w
                         ;; 4 is for the width of arrow(3) between key and command
                         ;; and the end tab or newline(1)
                         (mapcar (lambda (it) (+ (car it) (cdr it) 4)))
                         (moder--sum))))
               (when (<= w fw)
                 (setq best-col-w col-w
                       best-rows rows)
                 (cl-return nil))))
    (if best-rows
        (thread-last
          best-rows
          (mapcar
           (lambda (row)
             (thread-last
               row
               (seq-map-indexed
                (lambda (it idx)
                  (let* ((key-str (car it))
                         (def-str (cdr it))
                         (l-r (nth idx best-col-w))
                         (l (car l-r))
                         (r (cdr l-r))
                         (key (moder--string-pad key-str l 32 t))
                         (def (moder--string-pad def-str r 32)))
                    (format "%s%s%s"
                            key
                            (propertize " → " 'face 'font-lock-comment-face)
                            def))))
               (moder--string-join " "))))
          (moder--string-join "\n"))
      (propertize "Frame is too narrow for KEYPAD popup" 'face 'moder-keypad-cannot-display))))



(defun moder-describe-keymap (keymap)
  (when (and keymap (not defining-kbd-macro) (not moder--keypad-help))
    (let* ((rst))
      (map-keymap
       (lambda (key def)
         (let ((k (if (consp key)
                      (format "%s .. %s"
                              (key-description (list (car key)))
                              (key-description (list (cdr key))))
                    (key-description (list key)))))
           (let (key-str def-str)
             (cond
              ((and (commandp def) (symbolp def))
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize (symbol-name def) 'face 'font-lock-function-name-face)))
              ((symbolp def)
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize (concat "+" (symbol-name def)) 'face 'font-lock-keyword-face)))
              ((functionp def)
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize "?closure" 'face 'font-lock-function-name-face)))
              (t
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize "+prefix" 'face 'font-lock-keyword-face))))
             (push (cons key-str def-str) rst))))
       keymap)
      (setq rst (reverse rst))
      (let ((msg (moder--describe-keymap-format rst)))
        (let ((message-log-max)
              (max-mini-window-height 1.0))
          (save-window-excursion
            (with-temp-message
                (format "%s\n%s%s%s"
                        msg
                        moder-keypad-message-prefix
                        (let ((pre (moder--keypad-format-prefix)))
                          (if (string-blank-p pre)
                              ""
                            (propertize pre 'face 'font-lock-comment-face)))
                        (propertize (moder--keypad-format-keys nil) 'face 'font-lock-string-face))
              (sit-for 1000000 t))))))))

(defun moder-keypad-get-title (def)
  "Return a symbol as title or DEF.

Returning DEF will result in a generated title."
  (if-let* ((cmd (and (symbolp def)
                      (commandp def)
                      (get def 'moder-dispatch))))
      (moder--keypad-lookup-key (kbd cmd))
    def))

(defun moder-keypad-undo ()
  "Pop the last input."
  (interactive)
  (setq this-command last-command)
  (cond
   (moder--use-both
    (setq moder--use-both nil))
   (moder--use-literal
    (setq moder--use-literal nil))
   (moder--use-meta
    (setq moder--use-meta nil))
   (t
    (pop moder--keypad-keys)))
  (if moder--keypad-keys
      (progn
        (moder--update-indicator)
        (moder--keypad-display-message))
    (when moder-keypad-message
      (message "KEYPAD exit"))
    (moder--keypad-quit)))

(defun moder--keypad-show-message ()
  "Show message for current keypad input."
  (let ((message-log-max))
    (message "%s%s%s%s"
             moder-keypad-message-prefix
             (if moder--keypad-help "(describe key)" "")
             (let ((pre (moder--keypad-format-prefix)))
               (if (string-blank-p pre)
                   ""
                 (propertize pre 'face 'font-lock-comment-face)))
             (propertize (moder--keypad-format-keys nil) 'face 'font-lock-string-face))))

(defun moder--keypad-in-beacon-p ()
  "Return whether keypad is started from BEACON state."
  (and (moder--beacon-inside-secondary-selection)
       moder--beacon-overlays))

(defun moder--keypad-execute (command)
  "Execute the COMMAND.

If there are beacons, execute it at every beacon."
  (if (moder--keypad-in-beacon-p)
      (cond
       ((member command '(kmacro-start-macro kmacro-start-macro-or-insert-counter))
        (call-interactively 'moder-beacon-start))
       ((member command '(kmacro-end-macro moder-end-kmacro))
        (call-interactively 'moder-beacon-end-and-apply-kmacro))
       ((and (not defining-kbd-macro)
             (not executing-kbd-macro)
             moder-keypad-execute-on-beacons)
        (call-interactively command)
        (moder--beacon-apply-command command)))
    (call-interactively command)))

(defun moder--keypad-try-execute ()
  "Try execute command, return t when the translation progress can be ended.

This function supports a fallback behavior, where it allows to use `SPC
x f' to execute `C-x C-f' or `C-x f' when `C-x C-f' is not bound."
  (unless (or moder--use-literal
              moder--use-meta
              moder--use-both)
    (let* ((key-str (moder--keypad-format-keys nil))
           (cmd (moder--keypad-lookup-key (kbd key-str))))
      (cond
       ((keymapp cmd)
        (when moder-keypad-message (moder--keypad-show-message))
        (moder--keypad-display-message)
        nil)
       ((commandp cmd t)
        (setq current-prefix-arg moder--prefix-arg
              moder--prefix-arg nil)
        (if moder--keypad-help
            (progn
              (moder--keypad-quit)
              (describe-function cmd)
              t)
          (let ((moder--keypad-this-command cmd))
            (moder--keypad-quit)
            (setq real-this-command cmd
                  this-command cmd)
            (moder--keypad-execute cmd)
            t)))
       ((equal 'control (caar moder--keypad-keys))
        (setcar moder--keypad-keys (cons 'literal (cdar moder--keypad-keys)))
        (moder--keypad-try-execute))
       (t
        (setq moder--prefix-arg nil)
        (moder--keypad-quit)
        (if (or (eq t moder-keypad-leader-transparent)
                (eq moder--keypad-previous-state moder-keypad-leader-transparent))
            (let* ((key (moder--parse-input-event last-input-event))
                   (origin-cmd (cl-some (lambda (m)
                                          (when (and (not (eq m moder-normal-state-keymap))
                                                     (not (eq m moder-motion-state-keymap)))
                                            (let ((cmd (lookup-key m (kbd key))))
                                              (when (commandp cmd)
                                                cmd))))
                                        (current-active-maps)))
                   (remapped-cmd (command-remapping origin-cmd))
                   (cmd-to-call (if (member remapped-cmd '(undefined nil))
                                    (or origin-cmd 'undefined)
                                  remapped-cmd)))
              (moder--keypad-execute cmd-to-call))
          (message "%s is undefined" key-str))
        t)))))

(defun moder--keypad-handle-input-with-keymap (input-event)
  "Handle INPUT-EVENT with `moder-keypad-state-keymap'.

Return t if handling is completed."
  (if (equal 'escape last-input-event)
      (moder--keypad-quit)
    (setq last-command-event last-input-event)
    (let ((kbd (single-key-description input-event)))
      (if-let* ((cmd (lookup-key moder-keypad-state-keymap (read-kbd-macro kbd))))
          (call-interactively cmd)
        (moder--keypad-handle-input-event input-event)))))

(defun moder--keypad-handle-input-event (input-event)
  "Handle the INPUT-EVENT.

Add a parsed key and its modifier to current key sequence. Then invoke a
command when there's one available on current key sequence."
  (moder--keypad-clear-message)
  (when-let* ((key (single-key-description input-event)))
    (let ((has-sub-meta (moder--keypad-has-sub-meta-keymap-p)))
      (cond
       (moder--use-literal
        (push (cons 'literal key)
              moder--keypad-keys)
        (setq moder--use-literal nil))
       (moder--use-both
        (push (cons 'both key) moder--keypad-keys)
        (setq moder--use-both nil))
       (moder--use-meta
        (push (cons 'meta key) moder--keypad-keys)
        (setq moder--use-meta nil))
       ((and (equal input-event moder-keypad-meta-prefix)
             (not moder--use-meta)
             has-sub-meta)
        (setq moder--use-meta t))
       ((and (equal input-event moder-keypad-ctrl-meta-prefix)
             (not moder--use-both)
             has-sub-meta)
        (setq moder--use-both t))
       ((and (equal input-event moder-keypad-literal-prefix)
             (not moder--use-literal)
             moder--keypad-keys)
        (setq moder--use-literal t))
       (moder--keypad-keys
        (push (cons 'control key) moder--keypad-keys))
       ((alist-get input-event moder-keypad-start-keys)
        (push (cons 'control (moder--parse-input-event
                              (alist-get input-event moder-keypad-start-keys)))
              moder--keypad-keys))
       (t
        (if-let* ((keymap (moder--get-leader-keymap)))
            (setq moder--keypad-base-keymap keymap)
          (setq moder--keypad-keys (moder--parse-string-to-keypad-keys moder-keypad-leader-dispatch)))
        (push (cons 'literal key) moder--keypad-keys))))

    ;; Try execute if the input is valid.
    (if (or moder--use-literal
            moder--use-meta
            moder--use-both)
        (progn
          (when moder-keypad-message (moder--keypad-show-message))
          (moder--keypad-display-message)
          nil)
      (moder--keypad-try-execute))))

(defun moder-keypad ()
  "Enter keypad state and convert inputs."
  (interactive)
  (moder-keypad-start-with nil))

(defun moder-keypad-start ()
  "Enter keypad state with current input as initial key sequences."
  (interactive)
  (setq this-command last-command
        moder--keypad-keys nil
        moder--keypad-previous-state (moder--current-state)
        moder--prefix-arg current-prefix-arg)
  (moder--switch-state 'keypad)
  (unwind-protect
      (progn
        (moder--keypad-handle-input-with-keymap last-input-event)
        (while (not (moder--keypad-handle-input-with-keymap (read-key)))))
    (when (bound-and-true-p moder-keypad-mode)
      (moder--keypad-quit))))

(defun moder-keypad-start-with (input)
  "Enter keypad state with INPUT.

A string INPUT, stands for initial keys.
When INPUT is nil, start without initial keys."
  (setq this-command last-command
        moder--keypad-keys (when input (moder--parse-string-to-keypad-keys input))
        moder--keypad-previous-state (moder--current-state)
        moder--prefix-arg current-prefix-arg)
  (moder--switch-state 'keypad)
  (unwind-protect
      (progn
        (moder--keypad-show-message)
        (moder--keypad-display-message)
        (while (not (moder--keypad-handle-input-with-keymap (read-key)))))
    (when (bound-and-true-p moder-keypad-mode)
      (moder--keypad-quit))))

(defun moder-keypad-describe-key ()
  "Describe key via KEYPAD input."
  (interactive)
  (setq moder--keypad-help t)
  (moder-keypad))

(provide 'moder-keypad)
;;; moder-keypad.el ends here
