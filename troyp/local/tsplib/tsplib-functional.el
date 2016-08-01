;; tspfunc.el --- functional utilities  -*- lexical-binding: t -*-

;;; tsplib-functional.el --- functional programming combinators and utilities

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: functional function combinator hof
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

(require 'dash-functional)


(defalias 'id 'identity)

(defun fork (merge-function &rest functions)
  "Returns a function that applies each function of FUNCTIONS to its arguments,
and collects the results using MERGE-FUNCTION.

Examples:
  (funcall (fork 'list '+ '*) 2 3 5)
  ;; (10 30)
  ;; Note that this is equivalent to: (funcall (-juxt '+ '*) 2 3 5)

  (funcall (fork '/ '-sum 'length)
           '(2.0 0 2.0 3.0 5.0 1.0 8.0))
  ;; 3.0     ;; computes the mean of a list

(mapcar (fork '+
          (-rpartial #'expt 2)
          'identity
          (lambda(_) 1))
        '(2 3 5))
  ;; (7 13 31)    ;; computes the polynomial x^2+x+1 at the points 2, 3, 5

Note that this is variadic analogue of J's \"fork\" phrasal form. It is also a
generalization of dash.el's `-juxt' function, which is equivalent to `fork' with
the `list' function passed as the first argument."
  (declare (indent 1))
  (lambda (&rest args)
    (apply merge-function
           (mapcar (lambda (f) (apply f args))
                   functions))))


(provide 'tsplib-functional)

;;; tsplib-functional ends here
