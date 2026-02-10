;;; moder.el --- Yet Another modal editing -*- lexical-binding: t; -*-

;; Author: Shi Tianshu
;; Keywords: convenience, modal-editing
;; Package-Requires: ((emacs "27.1"))
;; Version: 1.5.0
;; URL: https://www.github.com/DogLooksGood/moder
;;
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

;; Enable `moder-global-mode' to activate modal editing.

;;; Code:

;;; Modules

(require 'moder-var)
(require 'moder-face)
(require 'moder-keymap)
(require 'moder-helpers)
(require 'moder-util)
(require 'moder-keypad)
(require 'moder-command)
(require 'moder-core)
(require 'moder-cheatsheet)
(require 'moder-tutor)

(provide 'moder)
;;; moder.el ends here
