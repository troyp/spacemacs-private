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

(defmacro fn (&rest body)
  "Return a function defined by BODY.

The definition BODY may use the anaphoric parameters _ and _1 to refer to the
first argument, _2 to refer to the second, and so on up to _9.

Example:
  (funcall (fn (/ (-sum _) (length _)))
           '(3.0 4.0 5.0 5.0 10.0))
  ;; 5.4"
  (declare (debug 'body))
  `(lambda (&rest args)
     (let* ((_  (nth 0 args))
            (_1 (nth 0 args))
            (_2 (nth 1 args))
            (_3 (nth 2 args))
            (_4 (nth 3 args))
            (_5 (nth 4 args))
            (_6 (nth 5 args))
            (_7 (nth 6 args))
            (_8 (nth 7 args))
            (_9 (nth 8 args)))
       ,@body)))


(provide 'tsplib-functional)

;;; tsplib-functional ends here
