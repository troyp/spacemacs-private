;;; tspjs --- Javascript configuration.

;; Copyright (C) 2016 Troy Pracy.
;;
;; Author: Troy Pracy
;; Maintainer: Troy Pracy
;; Created:
;; Version:
;; Package-Requires:
;; Keywords: major-mode mode

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
;;   (require 'tspjs)

;;; Code:

;; -----------------------------------------------------------------------------

;; *********
;; *       *
;; *  C++  *
;; *       *
;; *********


(defun expand-bookmarklet ()
  (interactive)
  (let ((a (region-beginning))
	(b (region-end)))
    (defun replacefn (FROM TO)
      (save-excursion
	(goto-char a)
	(while (search-forward FROM b t)
	  (replace-match TO nil t))))
    ;; (shell-command-on-region a b
    ;; 			     "sh -c xargs -0 | /usr/bin/urlencode -d"
    ;; 			     (current-buffer)
    ;; 			     t
    ;; 			     "*Errors: expand-bookmarklet*" t)
    (replacefn "%20" " ")
    (replacefn "{" "{\n")
    (replacefn "}" "\n}\n")
    (replacefn ";" ";\n")))

(defun expand-css ()
  (interactive)
  (let ((a (region-beginning))
	(b (region-end)))
    (defun replacefn (FROM TO)
      (save-excursion
	(goto-char a)
	(while (search-forward FROM b t)
	  (replace-match TO nil t))))
    (replacefn "{" "{\n")
    (replacefn "}" "\n}\n")
    (replacefn ";" ";\n")))

;; -----------------------------------------------------------------------------

(provide 'tspjs)

;;; tspjs ends here
