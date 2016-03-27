;;; tspc++ --- C++ configuration.

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
;;   (require 'tspc++)

;;; Code:

;; -----------------------------------------------------------------------------

;; *********
;; *       *
;; *  C++  *
;; *       *
;; *********


;; chomp: from Emacs Lisp Cookbook
(defun chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)


(defun insert-enum-class-operator<< (enum-name contents)
  (interactive)
  (insert "ostream& operator<<(ostream& out, " enum-name " x) {")
  (c-indent-line-or-region)
  (insert "\nswitch(x) {")
  (c-indent-line-or-region)
  (let* ((names (mapcar #'chomp (split-string contents ",")))
	 (name-field-length (apply #'max (mapcar #'length names))))
    (cl-loop for name in names
	     do (progn
		  (let ((filler (make-string (- name-field-length (length name))
					     32)))
		    (insert "\n\tcase " enum-name "::" name filler ":\t" 
			    "return out << \"" name "\";")
		    (c-indent-line-or-region))))
    (insert "\n}") (c-indent-line-or-region)
    (insert "\n}") (c-indent-line-or-region)
    (insert "\n")))

(defun align-c-end-comment (BEG END)
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\*/"))

;; -----------------------------------------------------------------------------

(provide 'tspc++)

;;; tspc++ ends here
