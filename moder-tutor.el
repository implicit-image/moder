;;; moder-tutor.el --- Tutor for Moder  -*- lexical-binding: t; -*-

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
;; A tutorial for Moder.
;;
;; To start, with M-x moder-tutor

;;; Code:

(require 'moder-var)

(defconst moder--tutor-content
  "
                           888
888 888 8e   e88 88e   e88 888  ,e e,  888,8,
888 888 88b d888 888b d888 888 d88 88b 888 '
888 888 888 Y888 888P Y888 888 888   , 888
888 888 888  '88 88'   '88 888  *YeeP* 888

==================================================================
=                      MODER INTRODUCTION                         =
==================================================================

 Moder is yet another modal editing mode for Emacs.
 What's modal editing? How do I use Moder? Let's start our journey!

 If you wonder what a keystroke means when reading this, just ask
 Emacs! Press C-h k then press the key you want to query.

==================================================================
=                     BASIC CURSOR MOVEMENT                      =
==================================================================

  To move up, press \\[moder-prev]
  To move down, press \\[moder-next]
  To move left, press \\[moder-left]
  To move right, press \\[moder-right]
       ↑
       \\[moder-prev]
   ← \\[moder-left]   \\[moder-right] →
       \\[moder-next]
       ↓

 You can move the cursor using the \\[moder-left], \\[moder-next], \\[moder-prev], \\[moder-right] keys, as shown
 above. Arrow keys also work, but it is faster to use the \\[moder-left]\\[moder-next]\\[moder-prev]\\[moder-right]
 keys as they are closer to the other keys you will be using.
 Try moving around to get a feel for \\[moder-left]\\[moder-next]\\[moder-prev]\\[moder-right].
 Once you're ready, hold \\[moder-next] to continue to the next lesson.

 Moder provides modal editing which means you have different
 modes for inserting and editing text. The primary modes you will
 use are Normal mode and Insert mode. While in Normal mode, the
 keys you press won't actually type text. Instead, they will
 perform various actions with the text. This allows for more
 efficient editing. This tutor will teach you how you can make
 use of Moder's modal editing features. To begin, ensure your
 caps-lock key is not pressed and hold the \\[moder-next] key until you reach
 the first lesson.

=================================================================
=                           DELETION                            =
=================================================================

 Pressing the \\[moder-delete] key deletes the character under the cursor.
 \\[moder-backward-delete] key deletes the character before the cursor (backspace).

 1. Move the cursor to the line below marked -->.
 2. Move the cursor to each extra character, and press \\[moder-delete] to
    delete it.

 --> Thhiss senttencee haass exxtra charracterss.
     This sentence has extra characters.

 Once both sentences are identical, move to the next lesson.

=================================================================
=                          INSERT MODE                          =
=================================================================

 Pressing the \\[moder-insert] key enters the Insert mode. In that mode you can
 enter text. <ESC> returns you back to Normal mode. The modeline
 will display your current mode. When you press \\[moder-insert], '%s'
 changes to '%s'.

 1. Move the cursor to the line below marked -->.
 2. Insert the missing characters with \\[moder-insert] key.
 3. Press <ESC> to return back to Normal mode.
 4. Repeat until the line matches the line below it.

 --> Th stce misg so.
     This sentence is missing some text.

 Note: If you want to move the cursor while in Insert mode, you
       can use the arrow keys instead of exiting and re-entering
       Insert mode.

=================================================================
=                      MORE ON INSERT MODE                      =
=================================================================

 Pressing \\[moder-insert] is not the only way to enter Insert Mode. Here are
 some other ways to enter Insert mode at different locations.

 Common examples of insertion commands include:

   \\[moder-insert]   - Insert cursor before the selection.
   \\[moder-append]   - Insert cursor after the selection.
   \\[moder-open-above]   - Insert new line above the current line.
   \\[moder-open-below]   - Insert new line below the current line.
   \\[moder-join] \\[moder-append] - Insert cursor at the start of the line.
   \\[moder-line] \\[moder-append] - Insert cursor at the end of the line.

 These commands are composable. \\[moder-join] will select the beginning of the
 current line up until the end of the non-empty line above.
 \\[moder-append] switches to Insert mode at the end of current selection.
 Using both commands together will result in the cursor position being at
 the beginning of the line (Insert mode). \\[moder-line] selects the whole
 line and enables the use of the same insertion commands.

 1. Move to anywhere in the line below marked -->.
 2. Press \\[moder-line] \\[moder-append], your cursor will move to the end of the line
    and you will be able to type.
 3. Type the necessary text to match the line below.
 4. Press \\[moder-join] \\[moder-append] for the cursor to move to the beginning of the line.
    This will place the cursor before -->. For now just return to
    Normal mode and move cursor past it.

 -->  sentence is miss
     This sentence is missing some text.

=================================================================
=                             RECAP                             =
=================================================================

 + Use the \\[moder-left], \\[moder-next], \\[moder-prev], \\[moder-right] keys to move the cursor.

 + Press \\[moder-delete] to delete the character under the cursor.

 + Press \\[moder-backward-delete] to delete the character before the cursor.

 + Press \\[moder-insert] to enter Insert mode to input text. Press <ESC> to
   return to Normal mode.

 + Press \\[moder-join] to select the start of the current line and
   the non-empty line above.

 + Press \\[moder-append] to enter Insert mode, with the cursor position being
   at the end of the selected region.

=================================================================
=                    MOTIONS AND SELECTIONS                     =
=================================================================

 Pressing \\[moder-next-word] will select everything from the cursor position
 until the end of the current word.
 Numbers that show up on the screen indicate a quick way to extend your selection.
 You can unselect the region with the \\[moder-cancel-selection] key.

 Pressing \\[moder-kill] will delete the current selection.

 The \\[moder-delete] key deletes the character below the cursor, while
 \\[moder-kill] deletes all of the selected text.

 1. Move the cursor to the line below marked -->.
 2. Move to the beginning of a word that needs to be deleted.
 3. Press \\[moder-next-word] to select a word.
 4. Press \\[moder-kill] to delete the selection.
 5. Repeat for all extra words in the line.

 --> This sentence pencil has vacuum extra words in the it.
     This sentence has vacuum words in it.

 Note: Pressing \\[moder-kill] without a selection will delete everything
       from cursor position until the end of line.

=================================================================
=                       WORDS VS SYMBOLS                        =
=================================================================

 Pressing \\[moder-mark-word] will select the whole word under the cursor. \\[moder-mark-symbol] will
 select the whole symbol. Symbols are separated only by whitespace,
 whereas words can also be separated by other characters.

 To understand the difference better, do the following exercise:

 1. Move the cursor to the line below marked -->.
 2. Use \\[moder-mark-word] and \\[moder-mark-symbol] on each word in a sentence.
 3. Observe the difference in selection.

 --> Select-this and this.

=================================================================
=                    EXTENDING SELECTION                        =
=================================================================

 Motions are useful for extending the current selection and for
 quick movement around the text.

   \\[moder-next-word] - Moves forward to the end of the current word.
   \\[moder-back-word] - Moves backward to the beginning of the current word.
   \\[moder-next-symbol] - Moves to the end of the current symbol.
   \\[moder-back-symbol] - Moves to the start of the current symbol.

 After selecting the word under the cursor with \\[moder-mark-word] you can
 extend the selection using the same commands.

   \\[moder-next-word] - Adds the next word to the selection.
   \\[moder-back-word] - Adds the previous word to the selection.
   \\[moder-next-symbol] - Adds the next symbol to the selection.
   \\[moder-back-symbol] - Adds the previous symbol to the selection.

 In-case too much gets selected, you can undo the previous selection
 with \\[moder-pop-selection] key.

 1. Move the cursor to the line below marked -->.
 2. Select the word with \\[moder-mark-word].
 3. Extend the selection with \\[moder-next-word].
 4. Press \\[moder-kill] to delete the selection.

 --> This sentence is most definitelly not at all short.
     This sentence is short.

=================================================================
=                        SELECTING LINES                        =
=================================================================

 Pressing \\[moder-line] will select the whole line. Pressing it again will
 add the next line to the selection. Numbers can also be used
 to select multiple lines at once. Cursor position can be reversed with
 \\[moder-reverse] to extend the selection in the other direction.

 1. Move the cursor to the second line below marked -->.
 2. Press \\[moder-line] to select the current line, and \\[moder-kill] to delete it.
 3. Move to the fourth line.
 4. Select 2 lines either by hitting \\[moder-line] twice or \\[moder-line] 1 in combination.
 5. Delete the selection with \\[moder-kill].
 6. (Optional) Try reversing the cursor and extending the selection.

 --> 1) Roses are red,
 --> 2) Mud is fun,
 --> 3) Violets are blue,
 --> 4) I have a car,
 --> 5) Clocks tell time,
 --> 6) Sugar is sweet,
 --> 7) And so are you.

=================================================================
=                 EXTENDING SELECTION BY OBJECT                 =
=================================================================

 Expanding the selected region is easy. In fact every motion
 command has its own expand type. Motions can be expanded in
 different directions and units.

 Common selection expanding motions by a THING:

   \\[moder-beginning-of-thing] - expand before cursor until beginning of...
   \\[moder-end-of-thing] - expand after cursor until end of...
   \\[moder-inner-of-thing] - select the inner part of...
   \\[moder-bounds-of-thing] - select the whole part of...

 Some of THING modifiers may include:

  r - round parenthesis
  s - square parenthesis
  c - curly parenthesis
  g - string
  p - paragraph
  l - line
  d - defun
  b - buffer

 1. Move the cursor to the paragraph below.
 2. Type \\[moder-bounds-of-thing] p to select the whole paragraph.
 3. Type \\[moder-cancel-selection] to cancel the selection.
 4. Type \\[moder-inner-of-thing] l to select one line.
 5. Type \\[moder-cancel-selection] to cancel the selection.
 6. Play with the commands you learned this section. You can do anything
    you want with these powerful commands!

 War and Peace by Leo Tolstoy, is considered one of the greatest works of
 fiction.It is regarded, along with Anna Karenina (1873–1877), as Tolstoy's
 finest literary achievement. Epic in scale, War and Peace delineates in graphic
 detail events leading up to Napoleon's invasion of Russia, and the impact of the
 Napoleonic era on Tsarist society, as seen through the eyes of five Russian
 aristocratic families.Newsweek in 2009 ranked it top of its list of Top 100
 Books.Tolstoy himself, somewhat enigmatically, said of War and Peace that it was
 \"not a novel, even less is it a poem, and still less an historical chronicle.\"

=================================================================
=                      MOVE AROUND THINGs                       =
=================================================================

 You can also move around things. In fact, Moder combines move and
 selection together. Every time you select something, the cursor
 will move to the beginning/end/inner/bound of things depending
 on your commands. Let's practice!

 * How to jump to the beginning of buffer quickly?

   Type \\[moder-beginning-of-thing] and \"b\". Remember to come
   back by typing \\[moder-pop-selection].

 * How to jump to the end of buffer quickly?

   I believe you could figure it out. Do it!

 * How to jump to the end of the current function quickly?

   1. Move cursor to the function below marked -->.
   2. Type \\[moder-bounds-of-thing] and \"c\", then \\[moder-append].

   -->
   fn count_ones(mut n: i64) -> usize {
    let mut count: usize = 0;
    while 0 < n {
        count += (1 & n) as usize;
        n >>= 1;
    }
    count
   }

 Note that Moder needs the major mode for the programming language
 to find functions correctly. Then if you type \\[moder-bounds-of-thing] and \"d\" to
 select the whole function here, it won't work. Go to your
 favorite programming language mode and practice!

=================================================================
=                   THE FIND/TILL COMMAND                       =
=================================================================

 Type \\[moder-till] to select until the next specific character.

 1. Move the cursor to the line below marked -->.
 2. Press \\[moder-till]. A prompt will appear in minibuffer.
 4. Type 'a'. The correct position for the next 'a' will be
    selected.

 --> I like to eat apples since my favorite fruit is apples.

 Note: If you want to go backwards, use \\[negative-argument] as a prefix; there is also
       a similar command on \\[moder-find], which will jump over that
       character.

=================================================================
=                            RECAP                              =
=================================================================

 + Unselect region with \\[moder-cancel-selection] key.

 + Reverse cursor position in selected region with \\[moder-reverse] key.

 + Undo selection with \\[moder-pop-selection].

 + Press \\[moder-next-word] to select until the end of current word.

 + Press \\[moder-back-word] to select until the start of closest word.

 + Press \\[moder-next-symbol] to select until the end of symbol.

 + Press \\[moder-back-symbol] to select until the start of symbol.

 + Press \\[moder-line] to select the entire current line. Type \\[moder-line] again to
   select the next line.

 + Motion can be repeated multiple times by using a number modifier.

 + Extend selection by using THING modifiers
   Motion Prefix: (\\[moder-beginning-of-thing] \\[moder-end-of-thing] \\[moder-inner-of-thing] \\[moder-bounds-of-thing])
   THING as a Suffix: (r,s,c,g,p,l,d,b)

 + Find by a single character with \\[moder-till] and \\[moder-find].

=================================================================
=                      THE CHANGE COMMAND                       =
=================================================================

 Pressing \\[moder-change] will delete the current selection and switch to
 Insert mode. If there is no selection it will only delete
 the character under the cursor and switch to Insert mode.
 It is a shorthand for \\[moder-delete] \\[moder-insert].

 1. Move the cursor to the line below marked -->.
 2. Select the incorrect word with \\[moder-next-word].
 3. Press \\[moder-change] to delete the word and enter Insert mode.
 4. Replace it with correct word and return to Normal mode.
 5. Repeat until the line matches the line below it.

 --> This paper has heavy words behind it.
     This sentence has incorrect words in it.

=================================================================
=                         KILL AND YANK                         =
=================================================================

 The \\[moder-kill] key also copies the deleted content which can then be
 pasted with \\[moder-yank].

 1. Move the cursor to the line below marked -->.
 2. Type \\[moder-line] to select the line.
 3. Type \\[moder-kill] to cut the current selection.
 4. Type \\[moder-yank] to paste the copied content.
 5. You can paste as many times as you want.

 --> Violets are blue, and I love you.

=================================================================
=                         SAVE AND YANK                         =
=================================================================

 Pressing \\[moder-save] copies the selection, which can then be pasted
 with \\[moder-yank] under the cursor.

 1. Move the cursor to the line below marked -->.
 2. Press \\[moder-line] to select one line forward.
 3. Press \\[moder-save] to copy the current selection.
 4. Press \\[moder-yank] to paste the copied content.
 5. You can paste as many times as you want.

 --> Violets are blue, and I love you.

=================================================================
=                            UNDOING                            =
=================================================================

 Pressing \\[moder-undo] triggers undo. The \\[moder-undo-in-selection] key will only undo the changes
 in the selected region.

 1. Move the cursor to the line below marked -->.
 2. Move to the first error, and press \\[moder-delete] to delete it.
 3. Type \\[moder-undo] to undo your deletion.
 4. Fix all the errors on the line.
 5. Type \\[moder-undo] several times to undo your fixes.

 --> Fiix the errors on thhis line and reeplace them witth undo.
     Fix the errors on this line and replace them with undo.

=================================================================
=                             RECAP                             =
=================================================================

 + Press \\[moder-change] to delete the selection and enter Insert mode.

 + Press \\[moder-save] to copy the selection.

 + Press \\[moder-yank] to paste the copied or deleted text.

 + Press \\[moder-undo] to undo last change.

 + Press \\[moder-undo-in-selection] to only undo changes in the selected region.

=================================================================
=               BEACON (BATCHED KEYBOARD MACROS)                =
=================================================================

 Keyboard macro is a function that is built-in to Emacs. Now with Moder, it's
 more powerful. We can do things like multi-editing with Beacon
 mode in Moder.

 Select a region, then press \\[moder-grab] to \"grab\" it, then enter
 Insert mode, moder will now enter Beacon mode. Moder will create multiple
 cursors and all edits you do to one cursor will be synced to other
 cursors after you exit Insert mode. Type \\[moder-grab] again to cancel
 grabbing.

 1. Move the cursor to the first line below marked -->.
 2. Select the six lines.
 3. Type \\[moder-grab] to grab the selection. Edits you
    make will be synced to the other cursors.
 4. Use Insert mode to correct the lines. Then exit Insert mode.
    Other cursors will fix the other lines after you exit Insert mode.
 5. Type \\[moder-grab] to cancel the grabbing.

 --> Fix th six nes at same ime.
 --> Fix th six nes at same ime.
 --> Fix th six nes at same ime.
 --> Fix th six nes at same ime.
 --> Fix th six nes at same ime.
 --> Fix th six nes at same ime.
     Fix these six lines at the same time.

=================================================================
=                         MORE ON BEACON                        =
=================================================================

 BEACON is powerful! Let's do some more practice.

 Ex. A. How to achieve this?
        1 2 3
        =>
        [| \"1\" |] [| \"2\" |] [| \"3\" |]

 1. Move the cursor to the line below marked -->
 2. Select the \"1 2 3\"
 3. Press \\[moder-grab] to grab the selection
 4. Press \\[moder-back-word] to create fake cursors at the beginning of each word
    in the backwards direction.
 5. Enter Insert Mode then edit.
 6. Press \\[moder-normal-mode] to stop macro recording and apply
    your edits to all fake cursors.
 7. Press \\[moder-grab] to cancel grab.
 --> 1 2 3
     [| \"1\" |] [| \"2\" |] [| \"3\" |]

 Ex. B. How to achieve this?
        x-y-foo-bar-baz
        =>
        x_y_foo_bar_baz

 1. Move the cursor to the line below marked -->
 2. Select the whole symbol with \\[moder-mark-symbol]
 3. Press \\[moder-grab] to activate secondary selection
 4. Press \\[negative-argument] \\[moder-find] and - to backward search for
    character -, will create fake cursor at each -
 5. Moder will start recording. Press \\[moder-change] to switch to Insert mode
    (character under current cursor is deleted)
 6. type _
 7. Press ESC to go back to NORMAL, then the macro will
    be applied to all fake cursors.
 8. Press \\[moder-grab] again to cancel the grab

 --> x-y-foo-bar-baz
     x_y_foo_bar_baz

=================================================================
=                     QUICK VISIT AND SEARCH                    =
=================================================================

 The visit command \\[moder-visit] can help to select a symbol in your
 buffer with completion. Once you have something selected with the \\[moder-visit] key,
 you can use \\[moder-search] to search for the next occurrence of that selection.

 If you want a backward search, you can reverse the selection with \\[moder-reverse]
 because \\[moder-search] will respect the direction of the current selection.

 1. Move the cursor to the line below marked -->.
 2. Select the word \"dog\" with \\[moder-visit] dog RET.
 3. Change it to \"cat\" with \\[moder-change] cat ESC.
 4. Save it with \\[moder-save].
 5. Search for next \"dog\" and replace it with \\[moder-search] \\[moder-replace].
 6. Repeat 5 to replace next \"dog\".

 --> I'm going to tell you something:
     dog is beautiful
     and dog is agile
     the last one, dog says moder

 Note: You can also start searching after \\[moder-mark-word] or \\[moder-mark-symbol]. Actually, you
       can use \\[moder-search] whenever you have any kind of selection. The search command
       is built on regular expression. The symbol boundary will be
       added to your search if the selection is created with \\[moder-visit], \\[moder-mark-word] and \\[moder-mark-symbol].

=================================================================
=                    KEYPAD                                     =
=================================================================

 One of the most notable features of Moder is the Keypad. It
 enables the use of modifier keybinds without pressing modifiers.

 To enter Keypad mode, press SPC in Normal mode or Motion mode.

 Once Keypad is started, your single key input, will be translated
 based on following rules:

 1. The first letter input, except x, c, h, m, g will be
 translated to C-c <key>.

 Example: a => C-c a

 Press SPC a, call the command on C-c a, which is
 undefined by default.

 2. m will be translated to M-, means next input should be
 modified with Meta.

 Example: m h => M-h

 Press SPC m h, call the command on M-h, which is
 mark-paragraph by default.

 3. Several keys are bound to prefixes similarly. Specifically,
 x -> C-x
 h -> C-h
 c -> C-c
 m -> M-
 g -> C-M-

 4. Any key following a prefix is interpreted as C-<key>.

 Example: x f => C-x C-f

 Press SPC x f, call the command on C-x C-f, which is
 find-file by default.

 5. Use SPC to indicate a literal key, which will not be modified with C-

 Example: m g SPC g => M-g g

 Press SPC m g SPC g, call the command on M-g g, which is
 goto-line by default.

 Sometimes, you can omit this SPC when there's no ambiguity.

 6. After one execution, regardless of success or failure, Keypad will
 quit automatically, and the previous mode will be enabled.

 7. To undo one input, press BACKSPACE. To cancel and exit Keypad
 immediately, press ESC or C-g.

=================================================================
=                     MODER CHEAT SHEET                          =
=================================================================

 All these keybinds are shown on the cheat sheet which can be
 opened by pressing \\[moder-cheatsheet].

=================================================================
")

(defun moder-tutor ()
  "Open a buffer with moder tutor."
  (interactive)
  (let ((buf (get-buffer-create "*Moder Tutor*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format (substitute-command-keys moder--tutor-content)
                      (alist-get 'normal moder-replace-state-name-list)
                      (alist-get 'insert moder-replace-state-name-list)))
      (goto-char (point-min))
      (display-line-numbers-mode))
    (switch-to-buffer buf)))

(provide 'moder-tutor)
;;; moder-tutor.el ends here
