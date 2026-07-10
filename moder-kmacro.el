;;; moder-kmacro.el --- Moder kmacro extensions -*- lexical-binding: t -*-

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar moder--kmacro-forward-char-event 6)

(defvar moder--kmacro-backward-char-event 2)

(defvar moder--kmacro-universal-arg-event 21)

(defvar moder--kmacro-last-macro-sequence nil)

(defun moder--store-kbd-macro-events (&rest events)
  "Store EVENTS in currently defined keyboard macro."
  (mapc #'store-kbd-macro-event events))

(defun moder--kmacro-store-move-char-event (times &optional backward)
  "Store TIMES kbd macro events for forward char movement. If BACKWARD is non-nil,\
store an event for backward char movement instead."
  (cond
   ;; if only one is inserted there is no need for prefix arg
   ((and (numberp times) (eq times 1))
    (store-kbd-macro-event (if backward
                               moder--kmacro-backward-char-event
                             moder--kmacro-forward-char-event)))
   ((numberp times)
    (let* ((num-arr (string-to-vector (number-to-string times)))
           (len (length num-arr))
           (i 1))
      ;; store numeric prefix arg first
      ;; (store-kbd-macro-event (aref (kbd (format "C-%c" (aref num-arr 0))) 0))
      ;; store digits of the numeric arg
      ;; (while (and (< i len))
      ;;   (store-kbd-macro-event (aref num-arr i))
      ;;   (incf i))
      (store-kbd-macro-event moder--kmacro-universal-arg-event)
      (moder--kmacro-store-sequence num-arr)
      ;; store movement key
      (store-kbd-macro-event (if backward
                                 moder--kmacro-backward-char-event
                               moder--kmacro-forward-char-event))))))

(defun moder--kmacro-store-sequence (seq)
  "Store all events from SEQ in currently defined kbd macro."
  (if (sequencep seq)
      (mapc #'store-kbd-macro-event seq)
    (error "Object stored by `moder--kmacro-store-sequence' should be a sequence, not %S" (type-of seq))))

;; custom event markers for substitution
(defun moder--kmacro-event (type val)
  (vector (intern (concat "moder-" (symbol-name type))) val))

(defun moder--kmacro-store-prefix-arg (arg)
  "Store prefix arg ARG in currently defined kmacro."
  (store-kbd-macro-event (moder--kmacro-event 'prefix arg)))

;; TODO: figure out what to do with this
(defun moder--kmacro-store-state-change (state command keys)
  "Record that a `moder' state has been changed to STATE."
  (when-let* ((symbolp state)
              (state (alist-get state moder-state-mode-alist)))
    (cancel-kbd-macro-events)
    (store-kbd-macro-event (moder--kmacro-event 'state-change state))))

(defun moder--kmacro-store-initial-state (state)
  (store-kbd-macro-event (moder--kmacro-event 'initial-state state)))

(defun moder--kmacro-marker-event-p (event)
  "Return non-nil if EVENT is a custom `moder' marker event."
  (and (vectorp event)
       (length> event 1)
       (symbolp (aref event 0))
       (string-prefix-p "moder-" (symbol-name (aref event 0)))))

(defun moder--kmacro-apply-event-substitution (keys)
  "Substitute marker events in KEYS."
  (cond
   ((vectorp keys)
    (let ((n 0)
          (sub-starts (list))
          (sub-ends (list))
          curr)
      (while (< n (length keys))
        (setq curr (aref keys n))
        (when (moder--kmacro-marker-event-p curr)
          (let ((type (aref curr 0))
                (value (aref curr 1)))
            (cond
             ((and value (eq type 'moder-start)) (push n sub-starts))
             ((eq type 'moder-end)
              (let ((start (pop sub-starts)))
                (when start
                  (dotimes (i (- n start))
                    ;; avoid resetting custom events
                    (unless (moder--kmacro-marker-event-p (aref keys i))
                      (aset keys (+ i start) nil)))
                  (aset keys n (vector 'moder-sub value))))))))
        (incf n))
      ;; substitute final event values
      (dotimes (i (length keys))
        (when-let* ((curr (aref keys i))
                    (event (and (moder--kmacro-marker-event-p curr)
                                (aref curr 1))))
          (pcase (aref curr 0)
            ('(moder-sub moder-state-change) (aset keys i event)))))
      keys))))

(defun moder--kmacro-parse-kbd-macro (kbd-macro)
  "Remove custom markers from KBD-MACRO."
  (cond
   ((vectorp kbd-macro)
    (thread-last
      (moder--kmacro-apply-event-substitution kbd-macro)
      (seq-filter #'eventp)
      (moder--list-to-vector)))
   ((kmacro-p kbd-macro)
    (let* ((keys (kmacro--keys kbd-macro))
           (counter (kmacro--counter kbd-macro))
           (format (kmacro--format kbd-macro))
           (parsed-keys (thread-last
                          (moder--kmacro-apply-event-substitution keys)
                          (seq-filter #'eventp)
                          (moder--list-to-vector))))
      (kmacro keys counter format)))
   (t kbd-macro)))

(defun moder-kmacro-enable-recording-advice ()
  "Enable advice for optimizing kmacro recording."
  (when defining-kbd-macro
    (dolist (adv moder-kmacro-advice-list)
      (when-let* ((fn (car adv))
                  (functionp fn)
                  (before (nth 1 adv))
                  (after (nth 2 adv)))
        (advice-add fn :before before)
        (advice-add fn :after after)))
    (dolist (readfn '(read-command read-extended-command read-string read-buffer read-number read-file-name))
      (advice-add readfn :around #'moder--kmacro-read-input-record-advice))))

(defun moder-kmacro-disable-recording-advice ()
  "Disable advice for optimizing kmacro recording."
  (when defining-kbd-macro
    (dolist (adv moder-kmacro-advice-list)
      (when-let* ((fn (car adv))
                  (functionp fn))
        (dolist (advice-fn (cdr adv))
          (advice-remove fn advice-fn))
        (dolist (readfn '(read-command read-extended-command read-string read-buffer read-number read-file-name))
          (advice-remove readfn #'moder--kmacro-read-input-record-advice))))))

(defun moder-kmacro-enable-executing-advice ())

(defun moder-kmacro-disable-executing-advice ())

(defun moder-kmacro-setup-recording-hooks ()
  (add-hook 'post-command-hook #'moder-kmacro--post-command-hook 100))

(defun moder-kmacro-cleanup-recording-hooks ()
  (remove-hook 'post-command-hook #'moder-kmacro--post-command-hook))

;;;###autoload
(defun moder--kmacro-start-macro (&rest args)
  ""
  (interactive "p")
  (when (null defining-kbd-macro)
    (call-interactively #'kmacro-start-macro)
    (when defining-kbd-macro
      (moder--kmacro-store-)
      (moder-kmacro-setup-recording-hooks)
      (moder-kmacro-enable-recording-advice)))

;;;###autoload
  (defun moder--kmacro-end-macro (&rest args)
    ""
    (interactive "p")
    (cond
     (defining-kbd-macro
      (moder-kmacro-disable-recording-advice)
      (moder-kmacro-cleanup-recording-hooks)
      (call-interactively #'kmacro-end-macro)
      (setq last-kbd-macro (moder--kmacro-parse-kbd-macro last-kbd-macro)))
     (executing-kbd-macro
      (user-error "`moder--kmacro-end' cannot be called during kmacro execution"))
     (t
      (user-error "`moder--kmacro-end cannot be calld while not defining a kmacro'"))))

  (defun moder--kmacro-call-macro ()
    (interactive))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fast electric pair insertion for kmacro execution
;;; `electric-pair-mode' parsing is very expensive, so we save inserted parens as
;;; input events to avoid repeating the parsing on macro execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defvar moder--kmacro-electric-pair-start-pos nil)

  (defun moder--kmacro-electric-pair-before-advice (char times)
    (when defining-kbd-macro
      (setq moder--kmacro-electric-pair-start-pos (point))))

  (defun moder--kmacro-electric-pair-after-advice (char times)
    (when (and defining-kbd-macro (> times 0) (characterp char))
      (let ((distance (- (point) moder--kmacro-electric-pair-start-pos))
            (backward (> moder--kmacro-electric-pair-start-pos (point))))
        (when (> distance 1)
          (moder--kmacro-store-move-char-event distance backward))
        (dotimes (n times)
          (store-kbd-macro-event char))
        (when (> (+ distance times) 1)
          (moder--kmacro-store-move-char-event distance (not backward))))))


  ;; input reading advice

  (defvar moder--kmacro-cached-input-table (make-hash-table :test 'eq :size 10))

  (defvar moder--kmacro-clear-input-cache ()
    (clrhash moder--kmacro-cached-input-table))

  (defun moder--kmacro-get-input-hash (prompt command-event function-name)
    (secure-hash 'sha256 (concat command-event prompt function-name)))

  (defun moder--kmacro-get-cached-input (prompt cmd-event function-name)
    (gethash (moder--kmacro-get-input-hash prompt cmd-event function-name)
             moder--kmacro-cached-input-table))


  ;; advice functions

  (defun moder--kmacro-read-input-record-advice (og prompt &rest args)
    (let ((cmd-event last-command-event)
          (val (apply og prompt args)))
      (prog1 val
        (puthash (moder--kmacro-get-input-hash prompt cmd-event (symbol-name og))
                 val
                 moder--kmacro-cached-input-table))))

  (defun moder--kmacro-read-input-execute-advice (og prompt &rest args)
    (moder--kmacro-get-cached-input (symbol-name og) prompt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUTOMATIC KMACRO FROM EDITS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (cl-defstruct (moder-edit
                 (:conc-name moder--edit-)
                 (:predicate moder--edit-p))
    (selection nil :type record)
    (start-cmd nil :type symbol)
    (edit-kmacro nil :type kmacro))

  (defvar moder--kmacro-started-edit nil)

  (defvar-local moder--kmacro-local-edit-in-progress nil)

  (defvar moder--kmacro-edit-buffer nil)

  (defvar moder-last-edit nil)

  (defvar moder-edit-ring nil)

  (defvar-local moder--kmacro-partial-edit nil)

  (defvar moder--kmacro-edit-last-buffer nil)

  (defvar-local moder--kmacro-edit-starting-state nil)

  (defun moder--kmacro-resume-edit ()
    "Resume recording last edit in current buffer"
    (when defining-kbd-macro
      (cancel-kbd-macro-events)
      (let ((keys last-kbd-macro))
        (funcall-interactively #'kmacro-end-macro)
        (set-buffer-local-toplevel-value 'moder--kmacro-partial-edit last-kbd-macro moder--kmacro-edit-buffer)
        (setq last-kbd-macro keys)
        (when (eq keys (kmacro--keys (car kmacro-ring)))
          (setq kmacro-ring (cdr kmacro-ring)))))
    ;; resume recording in current buffer
    (funcall-interactively #'kmacro-start-macro)
    (setq moder--kmacro-edit-buffer (current-buffer))
    (setq-local moder--kmacro-edit-starting-state moder--current-state)
    ;; resume current recorded keys from current buffer
    (when (vectorp moder--kmacro-partial-edit)
      (moder--kmacro-store-sequence moder--kmacro-partial-edit)
      (setq-local moder--kmacro-partial-edit nil)))

  (defun moder--kmacro-edit-pre-command-function ()
    (setq moder--kmacro-edit-last-buffer (current-buffer)))

  (defun moder--kmacro-edit-post-command-function ()
    ""
    ;; dont record macro for stuff outside the buffer
    (let ((same-buffer (or (minibufferp) (eq (current-buffer) moder--kmacro-edit-buffer)))
          (local-edit (bound-and-true-p moder--kmacro-local-edit-in-progress))
          (changed-buffers (equal moder--kmacro-edit-last-buffer (current-buffer))))
      (when changed-buffers (cancel-kbd-macro-events))
      (if (and same-buffer local-edit defining-kbd-macro)
          (unless (eq moder--kmacro-edit-starting-state moder--current-state)
            (cancel-kbd-macro-events)
            (moder--kmacro-finish-edit))
        (cond
         ((and (not same-buffer) local-edit defining-kbd-macro)
          (moder--kmacro-resume-edit)
          (message "Pausing %S kmacro, starting new one" moder--kmacro-edit-buffer))))))

  (defun moder--kmacro-start-edit (selection start-cmd)
    (setq moder--kmacro-started-edit t
          moder--kmacro-edit-buffer (current-buffer)
          moder--kmacro-edit-command start-cmd
          moder--kmacro-edit-selection selection)
    (add-hook 'post-command-hook #'moder--kmacro-edit-post-command-function)
    (add-hook 'pre-command-hook #'moder--kmacro-edit-pre-command-function)
    (funcall-interactively #'kmacro-start-macro))

  (defun moder--kmacro-finish-edit ()
    (remove-hook 'post-command-hook #'moder--kmacro-edit-post-command-function)
    (let ((last last-kbd-macro))
      (funcall-interactively #'kmacro-end-macro)
      (setq moder--kmacro-started-edit nil
            moder--kmacro-edit-buffer nil)
      (when last-kbd-macro
        (setq moder-last-edit (make-moder-edit :selection moder--kmacro-edit-selection
                                               :start-cmd moder--kmacro-edit-command
                                               :kmacro (kmacro last-kbd-macro))))
      (setq last-kbd-macro last
            kmacro-ring (cdr kmacro-ring)
            moder--kmacro-edit-command nil
            moder--kmacro-edit-selection nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kmacro from last change
  ;; based on the way vim does things
  ;; tries not to interfere with keyboard macros as much as possible
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defcustom moder-kmacro-change-ring-size 32
    "Size of `moder--kmacro-change-ring'")

  (defvar-local moder-kmacro-last-change nil)

  (defvar-local moder--kmacro-current-change nil)

  (defvar moder--kmacro-change-ring nil)

  (defvar moder--kmacro-record-change-states '(insert))

  (defun moder--kmacro-should-record-change-p ()
    (and (memq moder--current-state moder--kmacro-record-change-states)
         (not executing-kbd-macro)))

  (defun moder--kmacro-pre-command-record-change ()
    (cond
     ((moder--kmacro-should-record-change-p)
      (message "recording " (this-command-keys-vector))
      (let ((keys (this-command-keys-vector)))
        (setq moder--kmacro-current-change (vconcat moder--kmacro-current-change keys))))
     (t
      (moder--kmacro-stop-recording-change t))))

  (defun moder--kmacro-get-change (idx)
    (let ((idx (mod idx moder-kmacro-change-ring-size)))
      (cond
       ((= idx 0) (symbol-value 'moder-kmacro-last-change))
       ((> idx 0)
        (elt moder--kmacro-change-ring (- idx 1))))))

  (defun moder--kmacro-start-recording-change ()
    (when (moder--kmacro-should-record-change-p)
      (when (and (vectorp moder--kmacro-last-change)
                 (not (equal (car moder--kmacro-change-ring) moder--kmacro-last-change)))
        (add-to-history 'moder--kmacro-change-ring moder-kmacro-last-change moder-kmacro-change-ring-size))
      (setq-local moder-kmacro-last-change nil)
      (setq-local moder--kmacro-current-change (this-command-keys-vector))
      (add-hook 'pre-command-hook 'moder--kmacro-pre-command-record-change nil t)))

  (defun moder--kmacro-stop-recording-change (discard-exit)
    (message "finishing recording")
    (remove-hook 'pre-command-hook #'moder--kmacro-pre-command-record-change t)
    (when discard-exit
      (let ((len (length moder--kmacro-current-change)))
        (when (eq 'escape (aref moder--kmacro-current-change (- len 1)))
          (setq-local moder--kmacro-current-change (seq-subseq moder--kmacro-current-change 0 (- len 2))))))
    (setq-local moder-kmacro-last-change moder--kmacro-current-change
                moder--kmacro-current-change nil))

  (defun moder--kmacro-repeat-last-change ()
    (when (and (vectorp moder-kmacro-last-change) (vectorp last-kbd-macro))
      (kmacro-push-ring))
    (setq last-kbd-macro moder-kmacro-last-change)
    (call-interactively #'kmacro-call-macro))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MASS KMACRO EXECUTION IN BUFFERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defvar-local moder--kmacro-was-read-only nil)

  (defvar moder--kmacro-temp-buffers nil)

  (defvar moder--kmacro-executing-buffers nil)

  (defvar moder--kmacro-target-types '(once regex search lines until-end))

  (defvar moder--kmacro-target-type-history nil)

  (defun moder--kmacro-keys (kmacro)
    (cond
     ((vectorp kmacro) kmacro)
     ((kmacro-p kmacro) (kmacro--keys kmacro))))

  (defun moder--kmacro-how-requires-number-p (sym)
    (or (member sym (moder--kmacro-get-thing-target-type-list))
        (member sym '(lines))))

  (defun moder--kmacro-check-buffer-p (buf)
    (let ((ro (buffer-local-value 'read-only-mode buf))
          (modified (buffer-modified-p buf))
          (buf-name (buffer-name buf)))
      (when (and ro (y-or-n-p "Buffer %s is read-only, override?" buf-name))
        (with-current-buffer buf
          (setq-local moder--kmacro-was-read-only t)))
      (when (and modified (y-or-n-p "Buffer %s is modified, save changes to %s?" buf-name (buffer-file-name buf)))
        (with-current-buffer buf))))

  (defun moder--kmacro-check-buffers (bufs)
    (let (()))
    (dolist (buf bufs)
      (moder--kmacro-check-buffer-p buf)))

  (defun moder--kmacro-file-prompt (pattern &optional choose-dir)
    (let ((dir (if choose-dir
                   (read-directory-name "Directory to search: ")
                 (or (project-root (project-current nil))
                     default-directory)))
          (pattern (read-regexp "Pattern: ")))
      (seq-filter #'file-directory-p (directory-files-recursively dir pattern t t))))

  (defun moder--kmacro-buffer-prompt (pattern)
    (let ((pattern (read-regexp "Pattern: ")))
      (match-buffers )))

  (defun moder--kmacro-get-file-buffers (file)
    (let ((truename (file-truename file))
          (abbrev-name (abbreviate-file-name file))
          (bufs (buffer-list))
          res)
      (dolist (buf bufs)
        (let ((name (buffer-file-name buf)))
          (when (or (equal name file)
                    (equal (file-truename name) truename)
                    (equal (abbreviate-file-name name) abbrev-name))
            (push buf res))))
      res))

  (defun moder--kmacro-ensure-file-buffers (files)
    "Make sure that all FILES have their buffers prepared."
    (dolist (file files (setq moder--kmacro-executing-buffers (nreverse moder--kmacro-executing-buffers)))
      (unless (not (file-exists-p file))
        (let ((file-buf (find-buffer-visiting file)))
          (cond
           (file-buf
            )))
        (if-let* ((buf (find-buffer-visiting file)))
            (when (moder--kmacro-check-buffer buf)
              (push buf moder--kmacro-executing-buffers))
          (let ((buf (create-file-buffer file)))
            (push buf moder--kmacro-temp-buffers)
            (push buf moder--kmacro-executing-buffers))))))

  (defun moder--kmacro-lock-buffers (bufs)
    (mapc (lambda (buf)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq-local moder--kmacro-was-read-only buffer-read-only)
                (read-only-mode 1))))
          bufs))

  (defun moder--kmacro-cleaup-temp-buffers ()
    "Cleanup temprary buffers that were created only for kmacro application."
    (mapc (lambda (buf)
            (if (not (buffer-live-p))
                (kill-buffer buf)
              (when (and (buffer-modified-p buf) (y-or-n-p "Save buffer %s" (buffer-file-name buf)))
                (with-current-buffer buf
                  (save-buffer)
                  (kill-buffer)))))
          moder--kmacro-temp-buffers))

  (defun moder--kmacro-apply-on-matches (search &optional regex)
    (let ((search-func (if regex #'search-forward-regexp #'search-forward)))
      (while (funcall search-func search (point-max) t 1)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (thread-first
            (if backward
                (moder--create-selection nil end beg)
              (moder--create-selection nil beg end))
            (moder--select t))
          (call-interactively #'kmacro-call-macro)
          (moder--cancel-selection)))))

  (defun moder--kmacro-apply-on-things (thing count)
    (let ((func (moder--get-forward-function thing)))
      (user-error " `moder--kmacro-apply-on-things' is not implemented yet!!!!")))

  (defun moder--kmacro-apply-on-lines (count)
    (while (and (not (eobp)))
      (goto-char (line-beginning-position))
      (set-mark (point))
      (line-move )
      (goto-char (line-end-position))))

  (defun moder--kmacro-get-thing-target-type-list ()
    (seq-remove #'null (mapcar (lambda (elt)
                                 (when (symbolp elt)
                                   (make-symbol (concat "thing-" (symbol-name elt)))))
                               moder--thing-registry)))

  (defun moder--kmacro-cleanup-temp-buffers ()
    (mapc #'kill-buffer moder--kmacro-temp-buffers)
    (setq moder--kmacro-executing-buffers nil))

  (defun moder--kmacro-apply-in-buffer (buf how &optional start-pos kmacro save widen)
    "Apply KMACRO in buffer BUF at position START-POS based on target info HOW.
If SAVE is non-nil, save buffer after applying macros. If WIDEN is non-nil,
ignore buffer restriction.

HOW is a cons cell (TYPE . TARGET) where:
 TYPE can be:
  symbol `once', meaning to apply the macro once at START-POS
  symbol `regex', meaning to apply the macro on every regex match for TARGET after START-POS
  symbol `search', meaning to apply the macro on every search  for TARGET after START-POS
  symbol `lines', meaning to apply the macro on every TARGET lines in the buffer
  symbol `thing-<name-of-thing>', meaning to apply the macro on every TARGET moder things in the buffer
  symbol `until-end', meaning to apply the macro until the end of the buffer; TARGET is ignored."
    (if (moder--kmacro-check-buffer-p buf)
        (with-current-buffer buf
          (let ((inhibit-read-only (list nil)))
            (moder--wrap-collapse-undo
             (save-mark-and-excursion
               (save-restriction
                 (when widen (widen))
                 (goto-char start-pos)
                 (when moder-kmacro-buffer-setup-hook
                   (save-mark-and-excursion
                     (run-hooks 'moder-kmacro-buffer-setup-hook)))
                 (let* ((last-kbd-macro (or kmacro last-kbd-macro))
                        (start-pos (or start-pos (point-min)))
                        (target-type (car how))
                        (target (cdr how)))
                   (message "Applying macro in %s" (buffer-name buf))
                   (cond
                    ((eq target-type 'once)
                     (call-interactively #'kmacro-call-macro))
                    ((eq target-type 'regex)
                     (moder--kmacro-apply-on-matches target t))
                    ((eq target-type 'search)
                     (moder--kmacro-apply-on-matches target nil))
                    ((eq target-type 'lines)
                     (moder--kmacro-apply-on-lines target))
                    ((memq target-type (moder--kmacro-get-thing-target-type-list))
                     (let ((thing (make-symbol (string-remove-prefix "thing-" (symbol-name target-type)))))
                       (moder--kmacro-apply-on-things thing target)))
                    ((eq target-type 'until-end)
                     (let ((p (point)))
                       (call-interactively #'kmacro-call-macro)
                       (if (= (point) p)
                           (message "last macro call did not move point, moving on to next buffer")
                         (while (and (not (eobp)))
                           (call-interactively #'kmacro-call-macro))))))
                   (when moder-kmacro-buffer-finish-hook
                     (save-mark-and-excursion
                       (run-hooks 'moder-kmacro-buffer-finish-hook)))
                   (when save (save-buffer))))))))
      (message "buffer is not live, skipping")))

  (defun moder--kmacro-apply-in-buffers (bufs how &optional start kmacro save)
    (let ((keys (moder--kmacro-keys kmacro)))
      (dolist (buf bufs)
        (let ((long (length> keys 10)))
          (message "Applying kmacro %S%s" (if long (take 10 keys) keys)
                   (if long "..." ""))
          (moder--kmacro-apply-in-buffer buf how start kmacro save t)))))

  (defun moder--kmacro-apply-in-file-buffers (files how &optional start kmacro save)
    (let ((bufs (moder--kmacro-ensure-file-buffers files)))
      (unwind-protect
          (moder--kmacro-apply-in-buffers bufs how start kmacro save)
        (moder--kmacro-cleanup-temp-buffers))))

  (defun moder--kmacro-how-prompt (prompt)
    (when-let* ((name (completing-read prompt
                                       (append (moder--kmacro-get-thing-target-type-list)
                                               moder--kmacro-target-types)
                                       nil t nil 'moder--kmacro-target-type-history))
                (sym (make-symbol name)))
      (cond
       ((moder--kmacro-how-requires-number-p sym)
        (let ((number (read-number "Apply every N %s: " (if (eq sym 'lines) "lines" "things"))))
          (cons sym number)))
       ((eq sym 'regex)
        (let ((regex (read-regexp "Apply on matches for: ")))
          (cons sym regex)))
       ((eq sym 'search)
        (let ((search (read-string "Apply on matches for search: " nil 'regexp-history)))
          (cons sym search)))
       (t (cons sym nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUTOMATIC KMACRO FROM REPEATED KEYSTROKES
  ;; based on https://github.com/emacs-jp/dmacro
  ;; licensed under FSFAP License
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defvar moder-kmacro-last-repeated nil)

  (defvar moder--kmacro-repeat-input-keys nil)

  (defvar moder--kmacro-repeat-input-subkeys nil)

  (defun moder-kmacro-get-repeated ()
    "Get last repeated key sequence."
    (let* ((lce (vector last-command-event))
           (keys (vconcat lce lce))
           (rkeys (recent-keys))
           arr)
      (if (equal keys (cl-subseq rkeys (- (length keys))))
          ;; if the last two match, no need to search
          (progn
            (setq moder--kmacro-repeat-input-subkeys nil)
            moder--kmacro-repeat-input-keys)
        ;; otherwise search for previously repeated commands
        (setq arr (moder-kmacro-repeat-search (cl-subseq rkeys 0 (- (length lce))) lce))
        (if (null arr)
            (setq moder--kmacro-repeat-input-keys nil)
          (let ((s1 (car arr))
                (s2 (cdr arr)))
            (setq moder--kmacro-repeat-input-keys (vconcat s2 s1))
            (setq moder--kmacro-repeat-input-subkeys (if (equal s1 "") nil s1))
            (setq last-kbd-macro moder--kmacro-repeat-input-keys)
            (if (equal s1 "") moder--kmacro-repeat-input-keys s1))))))

  (defun moder-kmacro-repeat-search (arr key)
    "Search array ARR for a subsequence matching KEY."
    (let* ((array (reverse arr))
           (sptr 1)
           (dptr0 (moder--kmacro-repeat-array-search (cl-subseq array 0 sptr) array sptr))
           (dptr dptr0)
           maxptr)
      (while (and dptr0
                  (not (moder--kmacro-repeat-array-search key (cl-subseq array sptr dptr0))))
        (when (= dptr0 sptr)
          (setq maxptr sptr))
        (setq sptr (1+ sptr))
        (setq dptr dptr0)
        (setq dptr0 (moder--kmacro-repeat-array-search (cl-subseq array 0 sptr) array sptr)))
      (if (null maxptr)
          (let ((predict-arry (reverse (cl-subseq array (1- sptr) dptr))))
            (if (moder--kmacro-repeat-array-search key predict-arry)
                nil
              (cons predict-arry (reverse (cl-subseq array 0 (1- sptr))))))
        (cons "" (reverse (cl-subseq array 0 maxptr))))))

  (defun moder--kmacro-repeat-array-search (pattern arr &optional start)
    ""
    (let* ((len (length pattern))
           (max (- (length arr) len))
           (p (or start 0))
           found)
      (while (and (not found) (<= p max))
        (setq found (equal pattern (cl-subseq arr p (+ p len))))
        (unless found (setq p (1+ p))))
      (if found p nil)))

;;;###autoload
  (defun moder-kmacro-exec-repeated (arg)
    (interactive "p")
    (let ((keys (if (and arg moder-kmacro-last-repeated)
                    moder-kmacro-last-repeated
                  (moder-kmacro-get-repeated))))
      (if keys
          (progn
            (when (not arg) (setq moder-kmacro-last-repeated keys))
            (execute-kbd-macro keys))
        (user-error "No repeated operation found"))))

  (provide 'moder-kmacro)
;;; moder-kmacro.el ends here
