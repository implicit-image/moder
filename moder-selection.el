;;; moder-selection.el --- Multi selection support -*- lexical-binding: t -*-

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
(require 'cl)
(require 'edmacro)
(eval-when-compile
  (require 'register))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STORING AND APPLYING CHANGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (moder-selection-sequence
               (:conc-name moder--selseq-)
               (:predicate moder--selseq-p))
  "A text-change is a selection + switch command (maybe nil) + a kmacro to edit the selection."
  (selection-kmacro nil :type vector)
  (init-state nil :type symbol)
  (end-state nil :type symbol)
  (final-point nil :type marker)
  (final-mark nil :type marker)
  (switch-command nil :type symbol)
  (change-kmacro nil :type kmacro))

(defun moder--selection-sequence-to-string (seq)
  "Produce a string representation of `moder-selection-sequence' SEQ."
  (if (moder--selseq-p seq)
      (let* ((point (moder--selseq-final-point seq))
             (mark (moder--selseq-final-mark seq))
             (backward (> mark point)))
        (concat "\["
                (format "%d %s %d"
                        (if backward point mark)
                        (if backward "<-" "->")
                        (if backward mark point))
                " | "
                (format "%S -> %S -> %S"
                        (moder--selseq-init-state seq)
                        (moder--selseq-switch-command seq)
                        (moder--selseq-end-state seq))
                " | "
                (format "change: %S" (moder--selseq-change-kmacro seq))
                "\]"))
    "\[Invalid selection sequence\]"))

(defun moder--selection-kmacro-from-history (hist)
  "Create a kmacro for selection commands from selection history HIST."
  (let ((list (take-while (lambda (sel)
                            (not (equal (moder--sel-type sel) '(select . nil))))
                          hist))
        (res (vector)))
    (when (and (listp hist) (length> hist 0) (moder--selection-p (car hist)))
      (dolist (sel hist res)
        (setq res (vconcat (moder--sel-keys sel) res))))))

(defun moder--make-selection-sequence (hist switch change)
  "Create a selection sequence from selection history HIST, switch command SWITCH and change macro CHANGE."
  (when (and (listp hist) (length> hist 0) (moder--selection-p (car hist)))
    (let* ((p (moder--sel-point (car hist)))
           (m (moder--sel-mark (car hist)))
           (init-state (moder--sel-init-state (car (last hist))))
           (sel-kmacro (moder--selection-kmacro-from-history hist))
           (change (if (kmacro-p change) change (kmacro change))))
      (make-moder-selection-sequence :selection-kmacro sel-kmacro
                                     :init-state init-state
                                     :final-point p
                                     :final-mark m
                                     :switch-command switch
                                     :change-kmacro change))))

(defun moder--selection-sequence-select (seq &optional no-update no-history)
  "Replay the selection macro of `moder-selection-sequence' SEQ.

If NO-UPDATE is non-nil, dont update current `moder--selection'. If NO-HISTORY is non-nil,
dont record the selection in the selection history."
  (if-let* ((state (moder--selseq-init-state seq))
            (kmacro (moder--selseq-selection-kmacro seq)))
      (progn
        (moder--switch-state state)
        (execute-kbd-macro kmacro)
        (unless no-update
          (funcall (if no-history #'moder--select-without-history #'moder--select)
                   (moder--make-selection '(select . region) (mark) (point)
                                          nil nil
                                          (moder--selection-rectangle-cols)
                                          real-this-command
                                          nil
                                          (prefix-numeric-value current-prefix-arg)))))
    (error "Selection sequence needs to have well defined int state and selection macro")))

(defun moder--selection-sequence-execute-switch-command (seq)
  "Execute the command to switch from selection to editing in SEQ."
  (let ((switch (moder--selseq-switch-command seq)))
    (cond
     ((eventp switch)
      (execute-kbd-macro (vector switch)))
     ((commanp switch)
      (command-execute switch))
     ((functionp switch)
      (funcall switch)))))

(defun moder--selection-sequence-execute (seq &optional no-update no-history)
  ""
  (when (moder--selseq-p seq)
    (let ((curr-state moder--current-state)
          (buf (current-buffer)))
      (message "Executing selseq %s" (moder--selection-sequence-to-string seq))
      (setq moder--executing-selection-sequence t)
      (unwind-protect
          (moder--selection-sequence-select seq no-update no-history)
        (cond
         ((and (region-active-p) (or (null no-update) moder--selection))
          (moder--selection-sequence-execute-switch-command seq)
          (execute-kbd-macro (moder--selseq-change-kmacro seq))
          (setq moder--executing-selection-sequence nil))
         (t
          (setq moder--executing-selection-sequence nil)
          (user-error "Selection failed")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STORING SELECTIONS IN REGISTERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; implementing these methods is necessary to store selection information
;; in standard registers
(cl-defmethod register-val-jump-to ((sel moder-selection-sequence))
  "Select current buffer contents according to moder-selection SEL."
  (let ((changes (moder--selseq-changes sel)))
    (when (region-active-p)
      (deactivate-mark))
    (moder--selection-sequence-select changes)))

(cl-defmethod register-val-describe ((sel moder-selection-sequence))
  "Output the printed representation of moder-selection SEL."
  (princ "a `moder' selection sequence:\n %S" (moder--selseq-changes sel)))

(cl-defmethod register-val-insert ((sel moder-selection-sequence))
  "Insert moder-selection SEL at point."
  (insert (prin1-to-string (moder--selseq-changes sel))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SELECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (moder-selection
               (:conc-name moder--sel-)
               (:predicate moder--selection-p))
  "Representation of an active selection."
  (init-state nil :type symbol)
  (type nil :type cons)
  (mark 0 :type number)
  (point 0 :type number)
  (rect nil :type cons)
  (command nil :type symbol)
  (thing nil :type symbol)
  (count 0 :type number)
  (keys nil :type vector)
  (prefix-arg nil :type cons))

(defun moder--selection-to-string (selection)
  "Create a printable representation of moder-selection SELECTION."
  (if (moder--selection-p selection)
      (let* ((mark (moder--sel-mark selection))
             (point (moder--sel-point selection))
             (backward (> mark point)))
        (concat
         "\["
         (format "%d %s %d"
                 (if backward point mark)
                 (if backward "<-" "->")
                 (if backward mark point))
         " | "
         (let* ((cmd (moder--sel-command selection))
                (keys (moder--sel-keys selection)))
           (if (or cmd keys)
               (format "%S (%s) x %d" (or cmd "-") (or (edmacro-format-keys keys) "-")
                       (or (prefix-numeric-value (moder--sel-prefix-arg selection))
                           1))
             "-"))
         " | "
         (if-let ((type (moder--sel-type selection)))
             (format "%S" type)
           "-")
         " | "
         (if-let ((thing (moder--sel-thing selection)))
             (format "%S x %d" thing (or (moder--sel-count selection) 1))
           "-")
         " | "
         (if-let* ((rect (moder--sel-rect selection)))
             (format "R: %S" rect)
           "R: -")
         "\]"))
    "\[invalid-selection\]"))

(defmacro moder--selection-rectangle-cols ()
  "Return (START-COL . END-COL) of the active rectangle region, if any."
  `(and (bound-and-true-p rectangle-mark-mode)
        (region-active-p)
        (rectangle--pos-cols (region-beginning) (region-end))))

(define-inline moder--make-selection (type mark pos &optional rect command thing count keys prefix state)
  "Wrapper for `make-moder-selection' constructor."
  (inline-quote
   (make-moder-selection :type ,type
                         :mark ,mark
                         :point ,pos
                         :rect ,rect
                         :command ,command
                         :thing ,thing
                         :count ,count
                         :keys ,keys
                         :prefix-arg ,prefix)))

(defun moder--expand-selection (type mark pos &optional thing count command)
  "Create a selection of type TYPE, from MARK to POS, expanding the current one.

Parameters THING, COUNT, COMMAND as in `moder--make-selection'."
  ;; TODO: figure out how to count selected things in case of expansion of the current selection
  (when (region-active-p)
    (let* ((mark (funcall (if (< mark pos) #'min #'max) (mark) (point)))
           (rect (moder--selection-rectangle-cols))
           (keys (this-command-keys-vector)))
      (moder--make-selection type mark pos rect command thing count keys current-prefix-arg moder--current-state))))

(define-inline moder--create-selection (type mark pos &optional expand thing count command keys prefix)
  "Create selection of type TYPE with mark MARK and point POS.

If EXPAND is non-nil, expand currently existing region.
If COMMAND is non-nil, save it as the selection's command, otherwise use
`real-this-command'.
If THING is non-nil, save it as the selection's thing.
If COUNT is non-nil save it as the prefix arg of COMMAND."
  (inline-quote
   (let ((count ,count)
         (cmd ,(or cmd real-this-command))
         (type ,type)
         (mark ,mark)
         (pos ,pos)
         (keys ,keys)
         (prefix ,prefix)
         (expand ,expand)
         (thing ,thing))
     (if expand
         (moder--expand-selection type mark pos thing count cmd)
       (moder--make-selection type mark pos
                              (moder--selection-rectangle-cols)
                              cmd
                              thing
                              count
                              keys
                              prefix
                              moder--current-state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CREATING SELECTION FROM OTHER DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder--selection-from-active-region (&optional type)
  "Create s selection of TYPE from currently active region.
If TYPE is nil, use `(select . region)'. Retuen nil if there is no active region."
  (when (region-active-p)
    (moder--make-selection (or (and (consp type) type) '(select . region)) (mark) (point)
                           nil nil
                           (moder--selection-rectangle-cols)
                           real-this-command
                           nil
                           (prefix-numeric-value current-prefix-arg))))

(defun moder--selection-from-beacon-overlay (ov)
  "Create a selection from beacon overlay OV."
  (unless (eq (overlay-get ov 'moder-beacon-type) 'cursor)
    (let ((backward (overlay-get ov 'moder-beacon-backward)))
      (moder--make-selection (overlay-get ov 'moder-beacon-type)
                             (if backward (overlay-end ov) (overlay-start ov))
                             (if backward (overlay-start ov) (overlay-end ov))
                             (overlay-get ov 'moder-beacon-rect)
                             nil
                             nil ;;TODO: maybe store thing on overlay?
                             (overlay-get ov count)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SELECTION INFORMATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro moder--current-selection ()
  "Return the current selection to operate on."
  `(cond
    ;; first make sure we return the last saved selection if we started
    ;; recorded kmacro from beacon state
    (moder--beacon-started-kmacro moder--beacon-last-selection)
    ((region-active-p) (or moder--selection
                           (moder--selection-from-active-region
                            (unless (null moder--selection-history)
                              '(expand . region)))))))

(defun moder--selection-valid-p (selection)
  "Return non-nil of SELECTION is valid."
  (and (moder-selection-p selection) (consp (moder--sel-type selection))))

(define-inline moder--selection-init-state (&optional selection)
  "Return the initial state before executing the SELECTION command."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection))))
     (moder--sel-init-state sel))))

(define-inline moder--selection-type (&optional selection)
  "Return current selection type of SELECTION."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection))))
     (moder--sel-type sel))))

(define-inline moder--selection-mark (&optional selection)
  "Return the beginning position of SELECTION."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection))))
     (moder--sel-mark sel))))

(define-inline moder--selection-point (&optional selection)
  "Return the end position of SELECTION."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection))))
     (moder--sel-point  sel))))

(define-inline moder--selection-beg (&optional selection)
  "Return the beginning (smaller position) of SELECTION."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection))))
     (if (moder--selection-forward-p sel)
         (moder--sel-mark sel)
       (moder--sel-pos sel)))))

(define-inline moder--selection-end (&optional selection)
  "Return the end (larger position) of SELECTION."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection))))
     (if (moder--selection-forward-p sel)
         (moder--sel-pos sel)
       (moder--sel-mark sel)))))

(define-inline moder--selection-thing (&optional selection)
  "Return the type of thing selected by SELECTION, if any."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection))))
     (moder--sel-thing sel))))

(define-inline moder--selection-command (&optional selection)
  "Return the last command that expanded or created SELECTION."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection))))
     (moder--sel-command sel))))

(define-inline moder--selection-rectangle-p (&optional selection)
  "Return non-nil if SELECTION is a `rectangle-mark-mode' selection."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection))))
     (consp (moder--sel-rect sel)))))

(define-inline moder--selection-direction (&optional selection)
  "Return direction of SELECTION, It can be either symbol `forward' or `backward'."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection)))
               (mark (moder--sel-mark sel))
               (pos (moder--sel-point sel)))
     (if (> mark pos) 'backward 'forward))))

(define-inline moder--selection-backward-p (&optional selection)
  "Return non-nil if SELECTION is backwards."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection)))
               (mark (moder--sel-mark sel))
               (point (moder--sel-point sel)))
     (> mark pos))))

(define-inline moder--selection-forward-p (&optional selection)
  "Return non-nil if SELECTION is forwards."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection)))
               (mark (moder--sel-mark sel))
               (point (moder--sel-point sel)))
     (>= mark pos))))

(define-inline moder--direction-backward-p ()
  "Return non-nil if current direction is backwards."
  (inline-quote
   (if moder--beacon-started-kmacro
       (moder--selection-backward-p moder--beacon-last-selection)
     (when (region-active-p)
       (> (mark) (point))))))

(define-inline moder--direction-forward-p ()
  "Return whether we have a forward selection."
  (inline-quote
   (if moder--beacon-started-kmacro
       (moder--selection-forward-p moder--beacon-last-selection)
     (when (region-active-p)
       (<= (mark) (point))))))

(define-inline moder--selection-count (&optional selection)
  "Return the count of things in SELECTION."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection))))
     (moder--sel-count sel))))

(define-inline moder--selection-keys (&optional selection)
  "Return the keys that executed the selection command."
  (inline-quote
   (when-let* ((sel ,(or selection `(moder--current-selection))))
     (moder--sel-keys sel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYNCING CURRENT SELECTION TO ACTIVE REGION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder--selection-push-new (new &optional select activate)
  "Set NEW as the current selection. If SELECT is non-nil, call `moder--select' with it.
If ACTIVATE is non-nil, activate the mark after selecting."
  (let ((curr moder--selection)
        (last (car moder--selection-history)))
    (when (not (equal new curr))
      (when (not (equal curr last))
        (push curr moder--selection-history))
      (setq moder--selection new)
      (when select
        (moder--select moder--selection activate)))))

(defun moder--selection-update-needed-p (selection)
  "Return non-nil if SELECTION needs updating to reflect current region.

Assumes that region is active."
  (or (null selection)
      (not (eq (moder--selection-command selection) this-command))
      (not (and (= (point) (moder--selection-mark selection))
                (= (mark) (moder--selection-point selection))))))

(defun moder--selection-maybe-update-current ()
  "Update `moder--selection' to reflect current region."
  (if (region-active-p)
      (when (moder--selection-update-needed-p moder--selection)
        (moder--selection-push-new (moder--selection-from-active-region
                                    (unless (null moder--selection-history)
                                      '(expand . region)))))
    (when (and moder--selection (not (eq moder--selection (car moder--selection-history))))
      (push moder--selection moder--selection-history)
      ;; push an empty selection in to signal that the selection has been cancelled
      (push (moder--make-selection '(select . nil) (point) (point) nil this-command nil nil) moder--selection-history)
      (setq moder--selection nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder--selection-adjust-rectangle (old new)
  "Adjust the state of rectangle mark mode before selecting new selection NEW with current selection OLD."
  (cond
   ((and (moder--selection-rectangle-p old)
         (not (moder--selection-rectangle-p new))
         (bound-and-true-p rectangle-mark-mode))
    (rectangle-mark-mode -1))
   ((and (moder--selection-rectangle-p new)
         (not (moder--selection-rectangle-p old))
         (not (bound-and-true-p rectangle-mark-mode)))
    (rectangle-mark-mode 1))))

(defun moder--select-beacon-overlay (ov)
  "Create a selection from OV and activate it."
  (thread-first (moder--selection-from-beacon-overlay ov)
                (moder--select t)))

(defun moder--direction-forward ()
  "Make the selection towards forward."
  (when (and (region-active-p) (< (point) (mark)))
    (exchange-point-and-mark)))

(defun moder--direction-backward ()
  "Make the selection towards backward."
  (when (and (region-active-p) (> (point) (mark)))
    (exchange-point-and-mark)))

(defun moder--selection-fallback ()
  "Run selection fallback commands."
  (if-let* ((fallback (alist-get this-command moder-selection-command-fallback)))
      (call-interactively fallback)
    (error "No selection")))

(defun moder--pop-selection ()
  "Pop a selection from variable `moder--selection-history' and activate."
  (when moder--selection-history
    (let ((sel (pop moder--selection-history)))
      (moder--select-without-history sel))))

(defun moder--set-mark (&optional location nomsg activate)
  "Set mark at LOCATION. As `push-mark', but don't push old mark to mark ring.

If NOMSG is non-nil, dont show notification message. If ACTIVATE is
non-nil, activate the mark."
  (setq location (or location (point)))
  (if (or activate (not transient-mark-mode))
      (set-mark location)
    (set-marker (mark-marker) location))
  (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
      (message "Mark set"))
  nil)

(defun moder--select (selection &optional activate backward)
  "Mark the SELECTION.

If ACTIVATE is non-nil, actovate the mark. If BACKWARD is non-nil, swap point and mark."
  (let* ((old-sel-type (moder--selection-type))
         (sel-type (moder--selection-type selection))
         (to-go (moder--selection-pos selection))
         (to-mark (moder--selection-mark selection)))
    (when sel-type
      (if moder--selection
          (unless (equal moder--selection (car moder--selection-history))
            (push moder--selection moder--selection-history))
        (push (moder--make-selection nil (point) (point)) moder--selection-history))
      (moder--selection-adjust-rectangle moder--selection selection)
      (cond
       ((null old-sel-type)
        (goto-char to-go)
        (push-mark to-mark t activate))
       (t
        (goto-char to-go)
        (set-mark to-mark)))
      (setq moder--selection selection))))

(defun moder--select-without-history (selection)
  "Mark the SELECTION without recording it in `moder--selection-history'."
  (let ((sel-type (moder--selection-type selection))
        (mark (moder--selection-mark selection))
        (pos (moder--selection-point selection)))
    (goto-char pos)
    (if (not sel-type)
        (progn
          (deactivate-mark)
          (message "No previous selection.")
          (moder--cancel-selection))
      (push-mark mark t t)
      (setq moder--selection selection)
      (when (and (null rectangle-mark-mode)
                 (moder--selection-rectangle-p selection))
        (rectangle-mark-mode 1)))))

(defun moder--cancel-selection ()
  "Cancel current selection, clear selection history and deactivate the mark.

If there's a selection history, move the mark to the beginning position
in the history before deactivation."
  (when moder--selection-history
    (let ((orig-pos (moder--selection-beg (car (last moder--selection-history)))))
      (set-marker (mark-marker) orig-pos))
    ;; push current selection history to the sequence history
    ;;TODO: implement selection-sequence handling
    ;;(push moder--selection-sequence-history (moder--selseq-from-history moder--selection-history)))
    )
  (setq moder--selection-history nil
        moder--selection nil)
  (deactivate-mark t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SELECTION HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: implement a way of recalling and replaying selection sequences
;; (defvar moder--selection-history-ring nil)
;;
;;
;; ;; history is a list of vectors ([type mark pos thing command rect direction count])
;; ;; we need to collect commands, directions and counts to
;; (defun moder--selection-pack-history ())
;;
;; (defun moder--selection-replay-history (&optional hist)
;;   (when-let* ((hist (or hist moder--selection-history))
;;               (_ (listp hist)))
;;     (dolist (sel (reverse hist))
;;       (let* ((prefix-arg (moder--selection-count sel))
;;              (cmd (moder--selection-command)))
;;         (when (commandp cmd)
;;           (command-execute cmd))))))

(provide 'moder-selection)
;;; moder-selection.el ends here
