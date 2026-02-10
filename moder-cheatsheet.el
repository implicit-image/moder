;;; moder-cheatsheet.el --- Cheatsheet for Moder  -*- lexical-binding: t; -*-

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
;; Cheatsheet for Moder.

;;; Code:

(require 'moder-var)
(require 'moder-util)
(require 'moder-cheatsheet-layout)

(defconst moder--cheatsheet-note
  (format "
NOTE:
%s means this command will expand current region.
" (propertize "ex" 'face 'moder-cheatsheet-highlight)))

(defun moder--render-cheatsheet-thing-table ()
  (concat
   (format
    "%s, %s, %s and %s require a %s as input:\n"
    (propertize "←thing→ (inner)" 'face 'moder-cheatsheet-highlight)
    (propertize "[thing] (bounds)" 'face 'moder-cheatsheet-highlight)
    (propertize "←thing (begin)" 'face 'moder-cheatsheet-highlight)
    (propertize "thing→ (end)" 'face 'moder-cheatsheet-highlight)
    (propertize "THING" 'face 'moder-cheatsheet-highlight))
   (moder--cheatsheet-render-char-thing-table 'moder-cheatsheet-highlight)))

(defvar moder-cheatsheet-physical-layout moder-cheatsheet-physical-layout-ansi
  "Physical keyboard layout used to display cheatsheet.

Currently `moder-cheatsheet-physical-layout-ansi' is supported.")

(defvar moder-cheatsheet-layout moder-cheatsheet-layout-qwerty
  "Keyboard layout used to display cheatsheet.

Currently `moder-cheatsheet-layout-qwerty', `moder-cheatsheet-layout-dvorak',
`moder-cheatsheet-layout-dvp' and `moder-cheatsheet-layout-colemak' is supported.")

(defun moder--short-command-name (cmd)
  (or
   (when (symbolp cmd)
     (when-let* ((s
                  (or (alist-get cmd moder-command-to-short-name-list)
                      (cl-case cmd
                        (undefined "")
                        (t (thread-last
                             (symbol-name cmd)
                             (replace-regexp-in-string "moder-" "")))))))
       (if (<= (length s) 9)
           (format "% 9s" s)
         (moder--truncate-string 9 s moder-cheatsheet-ellipsis))))
   "         "))

(defun moder--cheatsheet-replace-keysyms ()
  (dolist (it moder-cheatsheet-layout)
    (let* ((keysym (car it))
           (lower (cadr it))
           (upper (caddr it))
           (tgt (concat "  " (symbol-name keysym) " "))
           (lower-cmd (key-binding (read-kbd-macro lower)))
           (upper-cmd (key-binding (read-kbd-macro upper))))
      (goto-char (point-min))
      (when (search-forward tgt nil t)
        (let ((x (- (point) (line-beginning-position))))
          (delete-char -9)
          (insert (concat "       " upper " "))
          (forward-line 1)
          (forward-char x)
          (delete-char -9)
          (insert (propertize (moder--short-command-name upper-cmd) 'face 'moder-cheatsheet-highlight))
          (forward-line 2)
          (forward-char x)
          (delete-char -9)
          (insert (concat "       " lower " "))
          (forward-line 1)
          (forward-char x)
          (delete-char -9)
          (insert (propertize (moder--short-command-name lower-cmd) 'face 'moder-cheatsheet-highlight)))))))

(defun moder--cheatsheet-render-char-thing-table (&optional key-face)
  (let* ((ww (frame-width))
         (w 16)
         (col (min 5 (/ ww w))))
    (thread-last
      (seq-map-indexed
       (lambda (it idx)
         (let ((c (car it))
               (th (cdr it)))
           (format "% 9s ->% 3s%s"
                   (symbol-name th)
                   (propertize (char-to-string c) 'face (or key-face 'font-lock-keyword-face))
                   (if (= (1- col) (mod idx col))
                       "\n"
                     " "))))
       moder-local-char-thing-table)
      (string-join)
      (string-trim-right))))

(defun moder-cheatsheet ()
  (interactive)
  (cond
   ((not moder-cheatsheet-physical-layout)
    (message "`moder-cheatsheet-physical-layout' is not specified"))
   ((not moder-cheatsheet-layout)
    (message "`moder-cheatsheet-layout' is not specified"))
   (t
    (let ((buf (get-buffer-create (format "*Moder Cheatsheet*"))))
      (with-current-buffer buf
        (text-mode)
        (setq buffer-read-only nil)
        (erase-buffer)
        (apply #'insert (make-list 63 " "))
        (insert "Moder Cheatsheet\n")
        (insert moder-cheatsheet-physical-layout)
        (moder--cheatsheet-replace-keysyms)
        (goto-char (point-max))
        (insert moder--cheatsheet-note)
        (insert (moder--render-cheatsheet-thing-table))
        (add-face-text-property (point-min) (point-max) 'moder-cheatsheet-command)
        (setq buffer-read-only t))
      (switch-to-buffer buf)))))

(provide 'moder-cheatsheet)
;;; moder-cheatsheet.el ends here
