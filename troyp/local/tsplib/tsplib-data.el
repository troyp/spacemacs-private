
;;; tsplib-data.el --- functions and macros that act on data types

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: data collection list
;; Version: 0.0.1

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


(defmacro range (&rest clauses)
  "Return a range of numbers (or other values) as a list.

The CLAUSES follow `cl-loop' syntax. The macro call is equivalent to:

  (cl-loop for n CLAUSE... collect n)

\(fn CLAUSE...)"
  (declare (indent 'cl-loop)
           (debug body))
  `(cl-loop for n ,@clauses collect n))


(provide 'tsplib-data)

;;; tsplib-data ends here
