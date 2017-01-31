
;;; troyp-macros.el --- Macros for Emacs Lisp Programming    -*- lexical-binding: t -*-

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: macro
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

;; Use with:
;;     (eval-when-compile
;;       (require 'troyp-macros))

;; Names/prefixes used by macros:
;;     fn seq range

;;; Code:

(require 'cl-lib)
(require 'dash-functional)


;; ,-------------------,
;; | Function Creation |
;; '-------------------'

(defmacro fn (&rest body)
  "Return a function defined by BODY.

Intended for inline use where concision is desired.  If creating a function to
bind as a function value, use `lambda' or `-lambda'.

The definition BODY may use anaphoric parameters to refer to the arguments. For
a single-argument function, use <> or it. For a multiple-argument function, use
<1> to refer to the first argument, <2> to refer to the second, and so on up to
<9>.

If applied to a literal, creates a constant function, or equivalently, a thunk (
since it can be called with any number of arguments).

Examples:

  (-map (fn (* <> <>)) (number-sequence 0 10))
  ;; (0 1 4 9 16 25 36 49 64 81 100)

  (-map (fn (/ (-sum <>)
               (length <>)))
        '((3.0 4.0 5.0 5.0 10.0)
          (1.0 2.0 2.0 2.0)
          (1 5)))
  ;; (5.4 1.75 3)
    ;; find average of each list

  (-filter (fn (zerop (mod <> 3)))
           (number-sequence 1 10))
  ;; (3 6 9)

  (funcall (fn 7))
  ;; 7"
  (declare (debug 'body))
  (let* ((argsym (make-symbol "ARGS"))
         (symbol-vars '(<> it))
         (digit-vars '(<1> <2> <3> <4> <5> <6> <7> <8> <9>))
         (symbols (eval (backquote (-flatten ',body))))
         (digit-vars-used (-intersection digit-vars symbols))
         (symbol-vars-used (-intersection symbol-vars symbols))
         bindings)
    (--map (!cons (list  it
                         `(nth 0 ,argsym))
                  bindings)
           symbol-vars-used)
    (--map (!cons (list  it
                         `(nth ,(-elem-index it digit-vars) ,argsym))
                  bindings)
           digit-vars-used)
    `(lambda (&rest ,argsym)
       (let (,@bindings)
         ,@body))))


(defmacro fn: (&rest body)
  "Return a function defined by (BODY).

Intended for inline use where concision is desired.  If creating a function to
bind as a function value, use `lambda' or `-lambda'.

The definition BODY may use the anaphoric parameters <>, it or <1> to refer to the
first argument, <2> to refer to the second, and so on up to <9>.

Examples:

  (-map (fn: * <> <>) (number-sequence 0 10))
  ;; (0 1 4 9 16 25 36 49 64 81 100)

  (-filter (fn: > it 0)
           '(-5 2 0 0 3 -1 0 4))
  ;; (2 3 4)"
  (declare (debug 'body))
  (let* ((argsym (make-symbol "ARGS"))
         (symbol-vars '(<> it))
         (digit-vars '(<1> <2> <3> <4> <5> <6> <7> <8> <9>))
         (symbols (eval (backquote (-flatten ',body))))
         (digit-vars-used (-intersection digit-vars symbols))
         (symbol-vars-used (-intersection symbol-vars symbols))
         bindings)
    (--map-indexed (!cons (list  it
                                 `(nth 0 ,argsym))
                          bindings)
                   symbol-vars-used)
    (--map-indexed (!cons (list  it
                                 `(nth ,(-elem-index it digit-vars) ,argsym))
                          bindings)
                   digit-vars-used)
    `(lambda (&rest ,argsym)
       (let (,@bindings)
         (,@body)))))



(defmacro fn-case (expr &rest cases)
  "Eval EXPR and choose value by sequentially testing predicates.

CASES is a list of (PREDICATE VALUE) pairs.

Example:
  (fn-case 77
    ((fn (> it 100))  'too-big)
    (oddp             'odd-number)
    (evenp            'even-number))
  odd-number

Ideal for fairly simple case expressions involving predicates.  If you only
need to test equality against values, use regular `case'.  If you need more
complex predicates, consider using the more powerful but verbose `pcase'.

\(fn EXPR (PREDICATE VALUE)...)"
  (declare (indent 1)
           (debug (form &rest (sexp body))))
  (let ((transformed-cases
         (-map (-lambda ((function definition))
                 (if (symbolp function)
                     `((funcall ',function ,expr)
                       ,definition)
                   `((funcall ,function ,expr)
                     ,definition)))
               cases)))
    `(cond ,@transformed-cases)))

(defmacro fn-alias (bindings &rest body)
  "Execute with temporary function definitions.

Each DEFINITION in BINDINGS may be a either a symbol, in which case FUNC is
bound to its `symbol-function', or an expression evaluating to a function value.

Example:
  (fn-alias ((seq    'number-sequence)
            (double  (fn (* 2 <>))))
    (--map (double it) (seq 1 10)))
  ;; (2 4 6 8 10 12 14 16 18 20)

\(fn ((FUNC DEFINITION) ...) BODY...)"
  (declare (indent 1)
           (debug let))
  (let ((set-forms
         (cl-loop
          for (name defn) in bindings
          collect
          `(fset ',name
                 ,(if (symbolp defn)
                      (symbol-function defn)
                    defn)))))
    `(unwind-protect
         (progn
           ,@set-forms
           ,@body))))

(defmacro fn-setq (&rest bindings)
  "Set the FUNCTION definition of each SYMBOL in a set of BINDINGS.

Example:
  (fn-setq my-increment (fn-bindl '+ 1)
           my-double    (fn-bindl '* 2))

\(fn [SYMBOL FUNCTION] ...)"
  (declare (debug t))
  (let ((set-forms
         (cl-loop for (name defn) on bindings by 'cddr
                  collect `(fset ',name ,defn))))
    `(progn ,@set-forms)))



;; ,---------------,
;; | List Creation |
;; '---------------'

(defmacro range (&rest clauses)
  "Return a range of numbers (or other values) as a list.

The CLAUSES follow `cl-loop' syntax and an anaphoric loop variable i is exposed.
This is equivalent to:

  (cl-loop for i CLAUSES... collect i)

For more information on clauses, see Info Node `(cl)Loop Facility'

Examples:

  (range to 5)
  ;; (0 1 2 3 4 5)

  (range below 5)
  ;; (0 1 2 3 4)

  (range from 1 to 10 by 2)
  ;; (1 3 5 7 9)

  (range from 1 to 8 by 1.5)
  ;; (1 2.5 4.0 5.5 7.0)

  (range from 3 downto -3)
  ;; (3 2 1 0 -1 -2 -3)

  (range from 1 to 10 when (oddp i))
  ;; (1 3 5 7 9)

  (range from 1 until (> (* i i) 50))
  ;; (1 2 3 4 5 6 7)

  (range in '(a b c d e) by 'cddr)
  ;; (a c e)

  (range on '(a b c d))
  ;; ((a b c d) (b c d) (c d) (d))

  (range on (range to 3))
  ;; ((0 1 2 3) (1 2 3) (2 3) (3))

  (-map 'string (range from ?A to ?K))
  ;; (\"A\" \"B\" \"C\" \"D\" \"E\" \"F\" \"G\" \"H\" \"I\" \"J\" \"K\")

\(fn CLAUSE...)"
  (declare (indent 0)
           (debug body))
  `(cl-loop for i ,@clauses collect i))


(defmacro seq (from &optional to inc)
  "Return a sequence of numbers from FROM to TO inclusive, step size INC.

INC defaults to 1. If TO is nil, return (FROM)."
  ;; (declare (debug form &optional form form))
  (list 'number-sequence from to inc))



(provide 'troyp-macros)

;;; troyp-macros ends here
