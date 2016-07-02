;;; se.el -- Stack Exchange content extraction and clean-up utilities

;; Copyright (C) 2016 Troy Pracy.
;;
;; Author: Troy Pracy
;; Maintainer: Troy Pracy
;; Created: 1 Jul 2016
;; Version: 0.01
;; Package-Requires: web-mode
;; Keywords: html

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'se-extract)

;;; Code:

;; -----------------------------------------------------------------------------


(defun see-remove-up-vote-html ()
  (interactive)
  (query-replace "<a class=\"vote-up-off\" title=\"This question shows research effort; it is useful and clear\">up vote</a>" "")
  (query-replace "<a class=\"vote-up-off\" title=\"This answer is useful\">up vote</a>" "")
  (query-replace "<a class=\"comment-up comment-up-off\" title=\"this comment adds something useful to the post\">upvote</a>" ""))
(defun see-remove-down-vote-html ()
  (interactive)
  (query-replace "<a class=\"vote-down-off\" title=\"This question does not show any research effort; it is unclear or not useful\">down vote</a>" "")
  (query-replace "<a class=\"vote-down-off\" title=\"This answer is not useful\">down vote</a>" ""))
(defun see-remove-flag-html ()
  (interactive)
  (query-replace "<a class=\"comment-flag\" title=\"Flag this comment for serious problems or moderator attention\">flag</a>" ""))
(defun see-remove-doubled-blank-lines ()
  (interactive)
  (replace-regexp "\n[[:space:]]*\n\\([[:space:]]*\n\\)+" "\n\n"))

(defun see-add-style (&optional s)
  (interactive "sCSS: ")
  (save-excursion
    (beginning-of-buffer)
    (search-forward "<head>")
    (while (web-mode-is-comment-or-string (point))
      (search-forward "<head>"))
    (web-mode-insert-and-indent (concat "\n<style>\n" s "\n</style>"))))

(defun see-cleanup ()
  (interactive)
  (see-remove-up-vote-html)
  (see-remove-down-vote-html)
  (see-remove-flag-html)
  (see-remove-doubled-blank-lines)
  (delete-trailing-whitespace))


;; -----------------------------------------------------------------------------

(provide 'se-extract)

;;; se.el ends here
