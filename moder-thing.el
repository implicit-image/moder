;;; moder-thing.el --- Calculate bounds of thing in Moder  -*- lexical-binding: t -*-

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

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'moder-var)
(require 'moder-util)

(declare-function moder--visual-line-end-position "moder-command")
(declare-function moder--visual-line-beginning-position "moder-command")

(declare-function moder--search-regexp "moder-helpers")
(declare-function moder--skip-syntax "moder-helpers")
(declare-function moder--skip-not-syntax "moder-helpers")
(declare-function moder--syntax-at-point-p "moder-helpers")
(declare-function moder--fmt-soft-intern "moder-helpers")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder--bounds-of-symbol ()
  "Return bounds of symbol at point."
  (when-let* ((bounds (bounds-of-thing-at-point moder-symbol-thing)))
    (let ((beg (car bounds))
          (end (cdr bounds)))
      (save-mark-and-excursion
        (goto-char end)
        (if (not (looking-at-p "\\s)"))
            (while (looking-at-p " \\|,")
              (goto-char (cl-incf end)))
          (goto-char beg)
          (while (looking-back " \\|," 1)
            (goto-char (cl-decf beg))))
        (cons beg end)))))

(defun moder--bounds-of-string-1 ()
  "Return the bounds of the string under the cursor.

The thing `string' is not available in Emacs 27.'"
  (if (version< emacs-version "28")
      (when (moder--in-string-p)
        (let (beg end)
          (save-mark-and-excursion
            (while (moder--in-string-p)
              (backward-char 1))
            (setq beg (point)))
          (save-mark-and-excursion
            (while (moder--in-string-p)
              (forward-char 1))
            (setq end (point)))
          (cons beg end)))
    (bounds-of-thing-at-point 'string)))

(defun moder--inner-of-symbol ()
  "Return inner bounds of symbol at point."
  (bounds-of-thing-at-point moder-symbol-thing))

(defun moder--bounds-of-string (&optional inner)
  "Return bounds of string at point.

If INNER is non-nil, return outer bounds, else return inner bounds."
  (when-let* ((bounds (moder--bounds-of-string-1)))
    (let ((beg (car bounds))
          (end (cdr bounds)))
      (cons
       (save-mark-and-excursion
         (goto-char beg)
         (funcall (if inner #'skip-syntax-forward #'skip-syntax-backward) "\"|")
         (point))
       (save-mark-and-excursion
         (goto-char end)
         (funcall (if inner #'skip-syntax-backward #'skip-syntax-forward) "\"|")
         (point))))))

(defun moder--inner-of-string ()
  "Return inner bounds of string at point."
  (moder--bounds-of-string t))

(defun moder--inner-of-window ()
  "Return inner bounds of current window."
  (cons (window-start) (window-end)))

(defun moder--inner-of-line ()
  "Return inner bounds of line at point."
  (cons (save-mark-and-excursion (back-to-indentation) (point))
        (line-end-position)))

(defun moder--inner-of-visual-line ()
  "Return inner bounds of visual line at point."
  (cons (moder--visual-line-beginning-position)
        (moder--visual-line-end-position)))

(defun moder--inner-of-search-string (&optional arg)
  "Return the positions of first and last search match.
If ARG is non-nil or `search-ring' is empty read a string to search from minibuffer,"
  (let* ((str (if (or arg (null search-ring))
                  (read-string "Search: ")
                (car search-ring)))
         (beg (save-mark-and-excursion
                (goto-char (point-min))
                (search-forward str (point-max) t 1)
                (match-beginning 0)))
         (end (save-mark-and-excursion
                (goto-char (point-max))
                (search-backward str beg t 1)
                (match-end 0))))
    (when (and beg end)
      (cons beg end))))

(defun moder--inner-of-search-regexp (&optional arg)
  "Return the positions of first and last regexp search match.
If ARG is non-nil or `regexp-search-ring' is empty read a regexp from a minibuffer."
  (let* ((str (if (or arg (null regexp-search-ring))
                  (read-string "Search regex: ")
                (car regexp-search-ring)))
         (beg (save-mark-and-excursion
                (goto-char (point-min))
                (search-forward-regexp str (point-max) t 1)
                (match-beginning 0)))
         (end (save-mark-and-excursion
                (goto-char (point-max))
                (search-backward-regexp str beg t 1)
                (match-end 0))))
    (when (and beg end)
      (cons beg end))))

(defun moder--forward-symbol (&optional n)
  "Move point forward N symbols."
  (forward-symbol (or n 1)))

(defun moder--forward-string (&optional n)
  "Move point forward N string literals."
  (let* ((back (and (numberp n) (< n 0)))
         (times (or (abs n) 1))
         (count 0)
         (bounds (bounds-of-thing-at-point 'string)))
    (cond
     (back
      (when bounds (goto-char (car bounds)))
      (while (< count times)
        (moder--skip-not-syntax "\"" back)
        (backward-sexp)
        (incf count)))
     (t
      (when bounds (goto-char (cdr bounds)))
      (while (< count times)
        (moder--skip-not-syntax "\"" back)
        (forward-sexp)
        (incf count))))
    (point)))

;;; Registry

(defvar moder--thing-registry nil
  "Thing registry.

This is a plist mapping from thing to (inner-fn . bounds-fn).
Both inner-fn and bounds-fn returns a cons of (start . end) for that thing.")

(defun moder--thing-register (thing inner-fn bounds-fn next-fn)
  "Register INNER-FN and BOUNDS-FN to a THING."
  (setq moder--thing-registry
        (plist-put moder--thing-registry
                   thing
                   (list inner-fn bounds-fn next-fn))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THING EXPAND MAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder--thing-transient-map ()
  "Construct a transient map for expanding current selection."
  (let ((map (make-sparse-keymap))
        (fmt-str (cl-case moder-thing-transient-map-key-style
                   ((control) "C-%c")
                   ((meta) "M-%c")
                   ((control-meta) "C-M-%c")
                   (t (if (stringp moder-thing-transient-map-key-style)
                          moder-thing-transient-map-key-style
                        "%c")))))
    (dolist (pair moder-local-char-thing-table)
      (let* ((char (car pair))
             (key (kbd (format fmt-str char))))
        (when (key-valid-p key)
          (define-key map key 'moder-expand-thing))))
    (dotimes (i 10)
      (define-key map (kbd (number-to-string i)) 'digit-argument))
    map))

;;TODO: actually implement nicer transient map display
(defun moder--thing-transient-map-message ()
  "Return the message to use with transient map generated by `moder--thing-transient-map'."
  (if moder-thing-transient-map-use-prompt
      (moder--render-char-thing-table)
    nil))

(defun moder--thing-transient-map-keep-p ()
  "Predicate function for `set-transient-map'."
  (or (eq this-command 'moder-expand-thing)
      (memq this-command moder--thing-transient-map-keep-commands)))

(defun moder--thing-set-transient-map ()
  "Set transient map for expanding selection by things."
  (set-transient-map (moder--thing-transient-map)
                     #'moder--thing-transient-map-keep-p
                     nil
                     (moder--thing-transient-map-message)
                     moder-thing-transient-map-timeout))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BOUNDS OF THING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder--thing-syntax-function (syntax)
  "Search for bounds of thing at point described by SYNTAX."
  (cons
   (save-mark-and-excursion
     (when (use-region-p)
       (goto-char (region-beginning)))
     (skip-syntax-backward (cdr syntax))
     (point))
   (save-mark-and-excursion
     (when (use-region-p)
       (goto-char (region-end)))
     (skip-syntax-forward (cdr syntax))
     (point))))

(defun moder--thing-regexp-function (b-re f-re near)
  "Search for bounds of thing at point with beginning matching B-RE and end\
matching F-RE. If NEAR is non-nil, search for inner bounds."
  (let ((beg (save-mark-and-excursion
               (when (use-region-p)
                 (goto-char (region-beginning)))
               (when (re-search-backward b-re nil t)
                 (if near (match-end 0) (point)))))
        (end (save-mark-and-excursion
               (when (use-region-p)
                 (goto-char (region-end)))
               (when (re-search-forward f-re nil t)
                 (if near (match-beginning 0) (point))))))
    (when (and beg end)
      (cons beg end))))

(defun moder--thing-parse-pair-search (push-token pop-token back near)
  (let* ((search-fn (if back #'re-search-backward #'re-search-forward))
         (match-fn (if back #'match-end #'match-beginning))
         (cmp-fn (if back #'> #'<))
         (push-next-pos nil)
         (pop-next-pos nil)
         (push-pos (save-mark-and-excursion
                     (when (funcall search-fn push-token nil t)
                       (setq push-next-pos (point))
                       (if near (funcall match-fn 0) (point)))))
         (pop-pos (save-mark-and-excursion
                    (when (funcall search-fn pop-token nil t)
                      (setq pop-next-pos (point))
                      (if near (funcall match-fn 0) (point))))))
    (cond
     ((and (not pop-pos) (not push-pos))
      nil)
     ((not pop-pos)
      (goto-char push-next-pos)
      (cons 'push push-pos))
     ((not push-pos)
      (goto-char pop-next-pos)
      (cons 'pop pop-pos))
     ((funcall cmp-fn push-pos pop-pos)
      (goto-char push-next-pos)
      (cons 'push push-pos))
     (t
      (goto-char pop-next-pos)
      (cons 'pop pop-pos)))))

(defun moder--thing-pair-function (push-token pop-token near)
  (let* ((found nil)
         (depth  0)
         (beg (save-mark-and-excursion
                (prog1
                    (let ((case-fold-search nil))
                      (while (and (<= depth 0)
                                  (setq found (moder--thing-parse-pair-search push-token pop-token t near)))
                        (let ((push-or-pop (car found)))
                          (if (eq 'push push-or-pop)
                              (cl-incf depth)
                            (cl-decf depth))))
                      (when (> depth 0) (cdr found)))
                  (setq depth 0
                        found nil))))
         (end (save-mark-and-excursion
                (let ((case-fold-search nil))
                  (while (and (>= depth 0)
                              (setq found (moder--thing-parse-pair-search push-token pop-token nil near)))
                    (let ((push-or-pop (car found)))
                      (if (eq 'push push-or-pop)
                          (cl-incf depth)
                        (cl-decf depth))))
                  (when (< depth 0) (cdr found))))))
    (when (and beg end)
      (cons beg end))))

(defun moder--thing-make-syntax-function (x)
  (lambda () (moder--thing-syntax-function x)))

(defun moder--thing-make-regexp-function (x near)
  (let* ((b-re (cadr x))
         (f-re (caddr x)))
    (lambda () (moder--thing-regexp-function b-re f-re near))))

(defun moder--thing-make-pair-function (x near)
  (let* ((push-token (let ((tokens (cadr x)))
                       (string-join (mapcar #'regexp-quote tokens) "\\|")))
         (pop-token (let ((tokens (caddr x)))
                      (string-join (mapcar #'regexp-quote tokens) "\\|"))))
    (lambda () (moder--thing-pair-function push-token pop-token near))))

(defun moder--thing-make-pair-regexp-function (x near)
  (let* ((push-token (let ((tokens (cadr x)))
                       (string-join  tokens "\\|")))
         (pop-token (let ((tokens (caddr x)))
                      (string-join  tokens "\\|"))))
    (lambda () (moder--thing-pair-function push-token pop-token near))))

(defun moder--thing-parse (x near)
  (cond
   ((functionp x)
    x)
   ((symbolp x)
    (lambda () (bounds-of-thing-at-point x)))
   ((equal 'syntax (car x))
    (moder--thing-make-syntax-function x))
   ((equal 'regexp (car x))
    (moder--thing-make-regexp-function x near))
   ((equal 'pair (car x))
    (moder--thing-make-pair-function x near))
   ((equal 'pair-regexp (car x))
    (moder--thing-make-pair-regexp-function x near))
   ((listp x)
    (moder--thing-parse-multi x near))
   (t
    (lambda ()
      (message "Moder: THING definition broken")
      (cons (point) (point))))))

(defun moder--thing-parse-multi (xs near)
  (let ((chained-fns (mapcar (lambda (x) (moder--thing-parse x near)) xs)))
    (lambda ()
      (let ((fns chained-fns)
            ret)
        (while (and fns (not ret))
          (setq ret (funcall (car fns))
                fns (cdr fns)))
        ret))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FORWARD THING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moder--forward-thing-syntax-function (syntax n back)
  (let* ((snt (cdr syntax)))
    (when (moder--syntax-at-point-p snt (if back (1- (point)) (point)))
      (moder--skip-syntax snt back))
    (when (> n 1)
      (when-let* ((count (- 1 n))
                  (ok (> count 0)))
        (while (> count 0)
          (moder--skip-not-syntax snt back)
          (moder--skip-syntax snt back)
          (decf count))
        (point)))))

(defun moder--forward-thing-regexp-function (b-re f-re n back)
  (let* ((atp (let ((beg (save-mark-and-excursion
                           (when (use-region-p)
                             (goto-char (region-beginning)))
                           (when (re-search-backward b-re nil t)
                             (point))))
                    (end (save-mark-and-excursion
                           (when (use-region-p)
                             (goto-char (region-end)))
                           (when (re-search-forward f-re nil t)
                             (point)))))
                (when (and beg end)
                  (cons beg end))))
         (main-rx (if back b-re f-re))
         (inter-rx (if back f-re b-re))
         (lim (if back (point-min) (point-max)))
         (res (point))
         (count 0))
    ;; go to beginning or end of thing found at point
    (when atp (goto-char (if back (car atp) (cdr atp))))
    (let ((main (point))
          (inter (point)))
      (while (and (< count n)
                  (not (if back (bobp) (eobp))))
        (setq inter (moder--search-regexp back inter-rx lim t 1)
              main (moder--search-regexp back main-rx lim t 1))
        (incf count))
      (if back (match-beginning 0) (match-end 0)))))

;; FIXME: correclty implement forward function for pairs
;; MAYBE: use `forward-list' to navigate (in the context of current subexp)
;; MAYBE: use the current approach as a base: select outer pairs
;; for forward selections move one level up, forward to the next cluster
;; and continue searching for selection on the same nesting level.
;; this fails without some complicate parsing
(defun moder--forward-thing-parse-pair-search (push-token pop-token n back)
  (let* ((search-fn (if back #'re-search-backward #'re-search-forward))
         (match-fn (if back #'match-end #'match-beginning))
         (cmp-fn (if back #'> #'<))
         (push-next-pos nil)
         (pop-next-pos nil)
         (push-pos (save-mark-and-excursion
                     (when (funcall search-fn push-token nil t)
                       (setq push-next-pos (point))
                       (point))))
         (pop-pos (save-mark-and-excursion
                    (when (funcall search-fn pop-token nil t)
                      (setq pop-next-pos (point))
                      (point)))))
    (cond
     ((and (not pop-pos) (not push-pos))
      nil)
     ((not pop-pos)
      (goto-char push-next-pos)
      (cons 'push push-pos))
     ((not push-pos)
      (goto-char pop-next-pos)
      (cons 'pop pop-pos))
     ((funcall cmp-fn push-pos pop-pos)
      (goto-char push-next-pos)
      (cons 'push push-pos))
     (t
      (goto-char pop-next-pos)
      (cons 'pop pop-pos)))))

(defun moder--forward-thing-pair-function (push-token pop-token n back)
  (let* ((found nil)
         (depth  0)
         (beg (save-mark-and-excursion
                (prog1
                    (let ((case-fold-search nil))
                      (while (and (<= depth 0)
                                  (setq found (moder--forward-thing-parse-pair-search push-token pop-token n back)))
                        (let ((push-or-pop (car found)))
                          (if (eq 'push push-or-pop)
                              (cl-incf depth)
                            (cl-decf depth))))
                      (when (> depth 0) (cdr found)))
                  (setq depth 0
                        found nil))))
         (end (save-mark-and-excursion
                (let ((case-fold-search nil))
                  (while (and (>= depth 0)
                              (setq found (moder--forward-thing-parse-pair-search push-token pop-token n back)))
                    (let ((push-or-pop (car found)))
                      (if (eq 'push push-or-pop)
                          (cl-incf depth)
                        (cl-decf depth))))
                  (when (< depth 0) (cdr found))))))
    (when (and beg end)
      (cons beg end))))

(defun moder--forward-thing-make-syntax-function (x)
  (lambda (&optional n)
    (let ((n (or n 1)))
      (moder--forward-thing-syntax-function x (abs n) (< n 0)))))

(defun moder--forward-thing-make-regexp-function (x)
  (let* ((b-re (cadr x))
         (f-re (caddr x)))
    (lambda (n)
      (let ((n (or n 1)))
        (moder--forward-thing-regexp-function b-re f-re (abs n) (< n 0))))))

(defun moder--forward-thing-make-pair-function (x)
  (let* ((push-token (let ((tokens (cadr x)))
                       (string-join (mapcar #'regexp-quote tokens) "\\|")))
         (pop-token (let ((tokens (caddr x)))
                      (string-join (mapcar #'regexp-quote tokens) "\\|"))))
    (lambda (n)
      (let ((n (or n 1)))
        (moder--forward-thing-pair-function push-token pop-token (abs n) (< n 0))))))

(defun moder--forward-thing-make-pair-regexp-function (x)
  (let* ((push-token (let ((tokens (cadr x)))
                       (string-join  tokens "\\|")))
         (pop-token (let ((tokens (caddr x)))
                      (string-join  tokens "\\|"))))
    (lambda (n)
      (let ((n (or n 1)))
        (moder--forward-thing-pair-function push-token pop-token (abs n) (< n 0))))))

(defun moder--forward-thing-parse-multi (xs)
  (let ((chained-fns (mapcar (lambda (x) (moder--forward-thing-parse x)) xs)))
    (lambda (n)
      (let ((fns chained-fns)
            ret)
        (while (and fns (not ret))
          (setq ret (funcall (car fns) n)
                fns (cdr fns)))
        ret))))

(defun moder--forward-thing-parse (x)
  (cond
   ((functionp x)
    x)
   ((and (symbolp x)
         (functionp (or (get x 'forward-op)
                        (moder--fmt-soft-intern "forward-%s" x))))
    (or (get x 'forward-op)
        (moder--fmt-soft-intern "forward-%s" x)))
   ((and (consp x) (equal 'syntax (car x)))
    (moder--forward-thing-make-syntax-function x))
   ((and (consp x) (equal 'regexp (car x)))
    (moder--forward-thing-make-regexp-function x))
   ((and (consp x) (equal 'pair (car x)))
    (moder--forward-thing-make-pair-function x))
   ((and (consp x) (equal 'pair-regexp (car x)))
    (moder--forward-thing-make-pair-regexp-function x))
   ((listp x)
    (moder--forward-thing-parse-multi x))
   (t
    (lambda (&rest args)
      (message "Moder: THING definition broken")
      (point)))))


(defun moder-thing-register (thing inner bounds &optional forward)
  "Register a THING with INNER and BOUNDS.

Argument THING should be symbol, which specified in `moder-char-thing-table'.
Argument INNER, BOUNDS and FORWARD support following expressions:

  EXPR ::= FUNCTION | SYMBOL | SYNTAX-EXPR | REGEXP-EXPR
         | PAIRED-EXPR | MULTI-EXPR
  SYNTAX-EXPR ::= (syntax . STRING)
  REGEXP-EXPR ::= (regexp STRING STRING)
  PAIRED-EXPR ::= (pair TOKENS TOKENS)
  PAIRED-REGEXP-EXPR ::= (pair-regexp TOKENS-REGEXP TOKENS-REGEXP)
  MULTI-EXPR ::= (EXPR ...)
  TOKENS ::= (STRING ...)

FUNCTION is a function receives no arguments, return a cons which
  the car is the beginning of thing, and the cdr is the end of
  thing.

SYMBOL is a symbol represent a builtin thing.

  Example: url

    (moder-thing-register \\='url \\='url \\='url)

SYNTAX-EXPR contains a syntax description used by `skip-syntax-forward'

  Example: non-whitespaces

    (moder-thing-register \\='non-whitespace
                         \\='(syntax . \"^-\")
                         \\='(syntax . \"^-\"))

  You can find the description for syntax in current buffer with
  \\[describe-syntax].

REGEXP-EXPR contains two regexps, the first is used for
  beginning, the second is used for end. For inner/beginning/end
  function, the point of near end of match will be used.  For
  bounds function, the point of far end of match will be used.

  Example: quoted

    (moder-thing-register \\='quoted
                         \\='(regexp \"\\=`\" \"\\=`\\\\|\\='\")
                         \\='(regexp \"\\=`\" \"\\=`\\\\|\\='\"))

PAIR-EXPR contains two string token lists. The tokens in first
  list are used for finding beginning, the tokens in second list
  are used for finding end.  A depth variable will be used while
  searching, thus only matched pair will be found.

  Example: do/end block

    (moder-thing-register \\='do/end
                         \\='(pair (\"do\") (\"end\"))
                         \\='(pair (\"do\") (\"end\")))

PAIR-REGEXP-EXPR contains two regexp lists. The regexp in first
  list are used for finding beginning, the regexp in second list
  are used for finding end.  A depth variable will be used while
  searching, thus only matched pair will be found.

  Example: The inner block of `{}` will ignore newlines and spaces
           after \\='{\\=' before \\='}\\='.
    (moder-thing-register \\='code-block
                         \\='(pair-regexp (\"{[\\n\\t ]*\")  (\"[\\n\\t ]*}\") )
                         \\='(pair (\"{\") (\"}\")))"
  (let ((inner-fn (moder--thing-parse inner t))
        (bounds-fn (moder--thing-parse bounds nil))
        (forward-fn (moder--forward-thing-parse forward)))
    (moder--thing-register thing inner-fn bounds-fn forward-fn)))

(moder-thing-register 'round '(pair ("(") (")")) '(pair ("(") (")")) '(pair ("(") (")")))

(moder-thing-register 'square '(pair ("[") ("]")) '(pair ("[") ("]")) '(pair ("[") ("]")))

(moder-thing-register 'curly '(pair ("{") ("}")) '(pair ("{") ("}")) '(pair ("{") ("}")))

;; FIXME: experimental
(moder-thing-register 'xml-tag '(pair-regexp ("\<[^\>/]+\>*") ("\<*/[^\</]*\>")) '(pair-regexp ("\<[^\>/]+\>*") ("\<*/[^\</]*\>")) '(pair-regexp ("\<[^\>/]+\>*") ("\<*/[^\</]*\>")))

(moder-thing-register 'paragraph 'paragraph 'paragraph #'forward-paragraph)

(moder-thing-register 'sentence 'sentence 'sentence #'forward-sentence)

(moder-thing-register 'buffer 'buffer 'buffer
                      (lambda (n)
                        (if (>= n 0)
                            (end-of-buffer)
                          (beginning-of-buffer))))

(moder-thing-register 'defun 'defun 'defun 'defun)

(moder-thing-register moder-symbol-thing #'moder--inner-of-symbol #'moder--bounds-of-symbol #'moder--forward-symbol)

(moder-thing-register 'string #'moder--inner-of-string #'moder--bounds-of-string #'moder--forward-string)

(moder-thing-register 'window #'moder--inner-of-window #'moder--inner-of-window #'moder--inner-of-window)

(moder-thing-register 'line #'moder--inner-of-line 'line 'line)

(moder-thing-register 'visual-line #'moder--inner-of-visual-line #'moder--inner-of-visual-line #'moder--inner-of-visual-line)

(moder-thing-register 'search-string #'moder--inner-of-search-string #'moder--inner-of-search-string #'moder--inner-of-search-string)

(moder-thing-register 'search-regexp #'moder--inner-of-search-regexp #'moder--inner-of-search-regexp #'moder--inner-of-search-regexp)

(defun moder--thing-p (sym)
  "Return non-nil if SYM is a registered moder thing."
  (and (symbolp sym) (plist-member moder--thing-registry sym)))

(defun moder--thing-get-function (func thing)
  "Get FUNC function for THING.
FUNC can be sybol `inner' for inner-of-thing function, `bounds' for bounds-of-thing function
or `forward', for forward-thing function."
  (when-let* ((funs (plist-get moder--thing-registry thing))
              (fun (nth (pcase func
                          ('inner 0)
                          ('bounds 1)
                          ('forward 2))
                        funs)))
    (when (functionp fun)
      fun)))

(defun moder--parse-range-of-thing (thing inner)
  "Parse either inner or bounds of THING at point. If INNER is non-nil then parse inner."
  (if-let ((fun (moder--thing-get-function (if inner 'inner 'bounds) thing)))
      (funcall fun)
    (user-error "Error in %S: No %s function defined for %S" this-command (if inner "inner" "bounds") thing)))

(defun moder--forward-thing (thing back n)
  "Move point N THINGs forward, If BACK is non-nil, move backward instead."
  (if-let ((fun (moder--thing-get-function 'forward thing)))
      (dotimes (_ n)
        (funcall fun back))
    (user-error "Error in %S: No forward function defined for %S" this-command thing)))

(provide 'moder-thing)
;;; moder-thing.el ends here
