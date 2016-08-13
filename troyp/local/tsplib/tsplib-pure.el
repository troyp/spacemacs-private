
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



;; ,-----------------------------------,
;; | Higher-Order Functions and Macros |
;; '-----------------------------------'



(defun fn-bindl (function &rest largs)
  "Partial application of FUNCTION to LARGS."
  `(closure (t) (&rest args)
            (apply ',function
                   ,@(mapcar (lambda (x) `',x)
                             largs)
                   args)))

(defun fn-bindr (function &rest rargs)
  "Right-hand partial application of FUNCTION to RARGS."
  `(closure (t) (&rest args)
            (apply ',function
                   (append args ',rargs))))

(defun fn-fork (merge-function &rest functions)
  "Apply MERGE-FUNCTION to FUNCTIONS to produce a composed function.

Examples:

  (fn-call (fn-fork 'list '+ '*) 2 3 5)
  ;; (10 30)
  ;; compute the sum and product
  ;; equivalent to: (fn-call (-juxt '+ '*) 2 3 5)

  (fn-call (fn-fork '/ '-sum 'length)
           '(2.0 0 2.0 3.0 5.0 1.0 8.0))
  ;; 3.0
  ;; compute the mean of a list

  (-map (fn-fork '+
          (fn (expt <> 5))
          (fn <>)
          (fn 1))
        '(1 2 10))
  ;; (3 35 100011)
  ;; compute the polynomial x^5+x+1 at the points 1, 2, 10

Notes:

This is variadic analogue of J's \"fork\" phrasal form.

It is also a generalization of dash.el's `-juxt' function, which is equivalent
to `fn-fork'with the `list' function passed as the first argument."
  (lambda (&rest args)
    (apply merge-function
           (-map (lambda (f) (apply f args))
                 functions))))



(defalias 'fn-call 'funcall)



(defun fn-pos? (transform)
  "Return a predicate that returns t if TRANSFORM of its argument is positive.

Example:
  (-filter (fn-pos? 'car)
          '(( 3 .  0)
            (-2 .  5)
            ( 0 .  1)
            ( 9 . -3)))
  ;; ((3 . 0) (9 . -3))"
  (lambda (x)
    (> (funcall transform x) 0)))

(defun fn-neg? (transform)
  "Return a predicate that returns t if TRANSFORM of its argument is negative.

Example:
  (-filter (fn-neg? 'car)
          '(( 2 . -2)
            (-2 .  3)
            ( 0 .  1)
            (-9 .  0)))
  ;; ((-2 . 3) (-9 . 0))"
  (lambda (x)
    (< (funcall transform x) 0)))

(defun fn-zero? (transform)
  "Return a predicate that returns t if TRANSFORM of its argument is zero.

Example:
  (-filter (fn-zero? 'car)
          '(( 0 .  0)
            (-1 .  1)
            ( 0 .  1)
            ( 7 . -5)))
  ;; ((0 . 0) (0 . 1))"
  (lambda (x)
    (= (funcall transform x) 0)))



;; ,--------------------,
;; | Function Factories |
;; '--------------------'

(defun fn-n*_ (n)
  "Return partially applied arithmetic function (* N _)."
  (lambda (x) (* n x)))

(defun fn-_*n (n)
  "Return partially applied arithmetic function (* _ N)."
  (lambda (x) (* x n)))

(defun fn-n+_ (n)
  "Return partially applied arithmetic function (+ N _)."
  (lambda (x) (+ n x)))

(defun fn-_+n (n)
  "Return partially applied arithmetic function (+ _ N)."
  (lambda (x) (+ x n)))

(defun fn-n-_ (n)
  "Return partially applied arithmetic function (- N _)."
  (lambda (x) (- n x)))

(defun fn-_-n (n)
  "Return partially applied arithmetic function (- _ N)."
  (lambda (x) (- x n)))

(defun fn-n/_ (n)
  "Return partially applied arithmetic function (/ N _)."
  (lambda (x) (/ n x)))

(defun fn-_/n (n)
  "Return partially applied arithmetic function (/ _ N)."
  (lambda (x) (/ x n)))

(defun fn-_^n (n)
  "Return partially applied arithmetic function (expt _ N)."
  (lambda (x) (expt x n)))

(defun fn-n^_ (n)
  "Return partially applied arithmetic function (expt N _)."
  (lambda (x) (expt n x)))

(defun fn-n%_ (n)
  "Return partially applied arithmetic function (mod N _)."
  (lambda (x) (mod n x)))

(defun fn-_%n (n)
  "Return partially applied arithmetic function (mod _ N)."
  (lambda (x) (mod x n)))



(provide 'tsplib-pure)

;;; tsplib-pure ends here
