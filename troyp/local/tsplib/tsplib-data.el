
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

Examples:

  (range to 5)
  ;; (0 1 2 3 4 5)

  (range below 5)
  ;; (0 1 2 3 4)

  (range from 1 to 10 by 2)
  ;; (1 3 5 7 9)

  (range from 1 to 10 by 1.5)
  ;; (1 2.5 4.0 5.5 7.0 8.5 10.0)

  (range from 5 downto -5)
  ;; (5 4 3 2 1 0 -1 -2 -3 -4 -5)

  (range in '(a b c d e))
  ;; (a b c d e)

  (range in '(a b c d e) by 'cddr)
  ;; (a c e)

  (range on '(a b c d e))
  ;; ((a b c d e) (b c d e) (c d e) (d e) (e))

  (range on (range to 3))
  ;; ((0 1 2 3) (1 2 3) (2 3) (3))

  (-map 'string (range from ?A to ?K))
  ;; (\"A\" \"B\" \"C\" \"D\" \"E\" \"F\" \"G\" \"H\" \"I\" \"J\" \"K\")

\(fn CLAUSE...)"
  (declare (indent 'cl-loop)
           (debug body))
  `(cl-loop for n ,@clauses collect n))


(provide 'tsplib-data)

;;; tsplib-data ends here
