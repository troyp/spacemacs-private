
;;; tsplib-pure.el --- Pure functions and macros

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: functional
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(defun troyp-proper-list-p (x)
  (loop for rest on x
        by 'cdr
        while (consp rest)
        finally return (not rest)))


(provide 'tsplib-pure)

;;; tsplib-pure ends here
