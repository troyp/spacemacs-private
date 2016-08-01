;;; t.el --- hash-table functions and macros

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: hash-table data-types
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

(require 'cl)
(require 'dash)

(setq t--key-absent (gensym "t-"))

(defmacro t-make-hash (proplist &rest tabledata)
  "Create a new hash-table, specifying properties and initial data.

Examples:
  (t-make-hash nil :a 1 :b 2)
  ;; #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8
  ;;               data (:a 1 :b 2))
  (t-make-hash (:size 120 :rehash-threshold 0.9)
    :a 1
    :d 4)
  ;; #s(hash-table size 120 test eql rehash-size 1.5 rehash-threshold 0.9
  ;;               data (:a 1 :d 4))

See also `t-hash'

\(fn ([PROP-NAME1 PROP-VALUE1...]) [KEY1 VAL1...])"
  (declare (debug (form body))
           (indent 1))
  `(cl-loop with    table = (apply #'make-hash-table ',proplist)
            for     (k v) on ',tabledata by #'cddr
            do      (puthash k v table)
            finally return table))

(defmacro t-hash (&rest data)
  "Create a new hash-table with default properties from keys and values.

Example:
  (t-hash :a 1 :b 2)
  ;; #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8
  ;;               data (:a 1 :b 2))

See also `t-make-hash'

  \(fn [KEY1 VAL1...])"
  (declare (debug (body)))
  `(cl-loop with    table = (make-hash-table)
            for     (k v) on ',data by #'cddr
            do      (puthash k v table)
            finally return table))

(defmacro t-zip-to-hash (keylist valuelist)
  "Create a hash-table from corresponding lists of keys and values.

Example:
  (t-zip-to-hash '(:a :b :c) '(2 3 5))
  ;; #s(hash-table ... data (:a 2 :b 3 :c 5))"
  (declare (debug (form form)))
  `(cl-loop with    table = (make-hash-table)
            for     k in ,keylist
            for     v in ,valuelist
            do      (puthash k v table)
            finally return table))

(defun t-contains? (table key)
  "Return `t' if TABLE contains the key KEY, `nil' otherwise. Identical to
`t-in-hash-p' except with the arguments in the other order."
  (not (eql (gethash key table t--key-absent) t--key-absent)))

(defun t-in-hash-p (key table)
  "Return `t' if TABLE contains the key KEY, `nil' otherwise. Identical to
`t-contains?' except with the arguments in the other order."
  (t-contains? table key))

(defun t-test (table key value)
  "Test if the value of KEY in TABLE is VALUE under TABLE's test function.

Uses the `hash-table-test' predicate to perform the comparison."
  (funcall (hash-table-test table)
           (gethash key table)
           value))

(defun t-get (table key &optional default)
  "Return the value of KEY in TABLE, or optionally DEFAULT if absent."
  (gethash key table default))

(defun t-get-values (table &rest keys)
  "Return an ordered list of values associated with KEYS in TABLE.

\(fn TABLE KEY...)"
  (declare (indent 1))
  (let (result)
    (dolist (k keys)
      (!cons (gethash k table) result))
    (reverse result)))

;; FIXME: indent decl not working? Test later
(function-put 't-get-values 'lisp-indent-function 1)

(defun t-put (table key value)
  "Add a new key-value association to TABLE.

See also `t-conj!'"
  (puthash key value table))

(defun t-conj (table &rest args)
  "Return a new hash-table created by adding new key-value associations to TABLE.

\(fn TABLE [KEY1 VAL1 ...])"
  (declare (indent 1))
  (let ((result (copy-hash-table table)))
    (cl-loop for (key val) on args by #'cddr
             do (puthash key val result))
    result))

(defun t-conj! (table &rest args)
  "Add new key-value associations to TABLE.

The return value is TABLE.

\(fn TABLE [KEY1 VAL1...])"
  (declare (indent 1))
  (cl-loop for (key val) on args by #'cddr
           do (puthash key val table))
  table)

(defalias 't-put-values 't-conj!)

;; TODO: should some/all map functions allow multiple tables?
;;       if not, should they take TABLE first, like all other 1-table functions?

(defun t-map (fn table)
  "Return a new hash-table formed by associating each key in TABLE with the
result of applying FN to its associated value.

See also `t-map!', `t-map-with-key', `t-map-with-key!'"
  (let ((result (make-hash-table)))
    (maphash (lambda (k v) (puthash k (funcall fn v) result))
             table)
    result))

(defun t-map-with-key (fn table)
  "Return a new hash-table formed by associating each key in table with the
result of (fn key val).

See also `t-map-with-key!', `t-map', `t-map!'"
  (let ((result (make-hash-table)))
    (maphash (lambda (key val)
               (puthash key (funcall fn key val) result))
             table)
    result))

(alias 't-maphash 't-map-with-key)

(defun t-map! (fn table)
  "Transform TABLE by associating each key in TABLE with the result of applying
FN to its associated value.

See also `t-map', `t-map-with-key!', `t-map-with-key'"
  (maphash (lambda (k v)
             (puthash k (funcall fn v) table))
             table)
    table)

(defun t-map-with-key! (fn table)
  "Transform TABLE by associating each key in table with the result of
(FN key val). Returns TABLE.

See also `t-map-with-key', `t-map', `t-map!'"
  (maphash (lambda (k v)
             (puthash k (funcall fn k v) table))
           table)
    table)

(defalias 't-maphash! 't-map-with-key!)

(defmacro t-do (spec &rest body)
  "Iterate through TABLE, executing BODY for each key-value pair.

For each iteration, KEYVAR is bound to the key and VALUEVAR is bound to the value.

The return value is obtained by evaluating RESULT (nil by default).

Example:
  (t-do ((key value) h sum)
    (when (symbolp key)
      (setf sum (+ (or sum 0) value))))
  ;; add values associated with all keys that are symbols.

(t-do ((k v) h)
  (insert (format \"%S\t%S\n\" k v)))
  ;; print keys and values

See also `t--do'.

\(fn ((KEYVAR VALUEVAR) TABLE [RESULT]) BODY...)"
  (declare (debug (((sexp sexp) form) body))
           (indent 1))
  (let* ((vars  (car spec))
         (kvar  (car vars))
         (vvar  (cadr vars))
         (table (cadr spec))
         (result (caddr spec)))
    (if result
        `(let (,result)
           (maphash (lambda (,kvar ,vvar)
                      ,@body)
                    ,table)
           ,result)
      `(maphash (lambda (,kvar ,vvar)
                 ,@body)
               ,table))))

(defmacro t--do (table &rest body)
  "Iterate through TABLE, executing BODY for each key-value pair.

This is a more concise, anaphoric variant of `t-do', without the need to specify
iteration or result variables.

For each iteration, the pre-bound anaphoric variables 'k and 'v refer to the key
and value repectively, while the variable 'result (initially nil), can be set
within BODY and used to accumulate the result.

Example:
  (t--do h
    (when (symbolp k)
      (setf result (+ (or result 0) v))))
  ;; add values associated with all keys that are symbols.

  (t--do h
    (insert (format \"%S\t%S\n\" k v)))
  ;; print keys and values

\(fn TABLE BODY...)"
  (declare (debug (form body))
           (indent 1))
  `(let (result)
     (maphash (lambda (k v) ,@body) ,table)
     result))

(defmacro t-do-collect (table &rest body)
  "Iterate through the keys of TABLE, binding the variables 'key and 'val to
each key-value pair in turn, and executes BODY... .

The last expression in BODY... in each iteration is collected into a list and
returned as the result.

\(fn TABLE BODY...)"
  (declare (debug (form body))
           (indent 1))
  `(progn
     (let (result)
       (t-do ,table
              (!cons (progn ,@body)
                     result))
       (reverse result))))

(defun t-transform! (table &rest fns)
  "Transform TABLE by associating each key with the result of successively
applying each function in FNS (from left to right) to its associated value.

See also `t-transform'

\(fn TABLE FUNCTION...)"
  (declare (indent 1))
  (t-map! (apply #'-compose (reverse fns)) table))

(defun t-transform (table &rest fns)
  "Return a new hash-table formed by associating each key in TABLE with the
result of successively applying each function in FNS to its associated value.

See also `t-transform!'

\(fn TABLE FUNCTION...)"
  (declare (indent 1))
  (t-map (apply #'-compose (reverse fns)) table))

(defun t-merge (&rest tables)
  "Merge TABLES to create a combined hash-table. When multiple tables share a
key, the last (rightmost) table takes precedence. The equalitiy test used is
`eql'.

See also `t-merge!', `t-merge-with!', `t-merge-with'

\(fn TABLE...)"
  (let ((result (make-hash-table)))
    (cl-flet ((result-put (key val) (puthash key val result)))
        (cl-loop for table in tables
                 do (maphash #'result-put table)))
    result))

(defun t-merge! (base-table &rest other-tables)
  "Update BASE-TABLE with the keys and values in OTHER-TABLES. OTHER-TABLES are
processed from left to right, so when multiple tables share a key, the last
table takes precedence. The equality test used is `eql'.

The return value is BASE-TABLE.

See also `t-merge', `t-merge-with!', `t-merge-with'"
  (cl-flet ((result-put (key val) (puthash key val base-table)))
    (cl-loop for table in other-tables
             do (maphash #'result-put table)))
  base-table)

(defalias 't-update 't-merge!)

(defun t-ziphash (&rest tables)
  "Return a new hash-table formed by applying `nconc' to the values in each
table associated with a given key."
  (let ((ziptable (make-hash-table)))
    (cl-loop for tab in tables
             do (maphash (lambda (k v)
                           (puthash k
                                    (nconc (list v)
                                           (gethash k ziptable))
                                    ziptable))
                         tab))
    ziptable))

(defun t-merge-with (fn &rest tables)
  "Return a new hash-table formed by merging TABLES. For each key that occurs
in one or more tables, the associated values in each table are combined by
applying FN.

See also `t-merge-with!', `t-merge', `t-merge!'"
  (declare (indent 1))
  (let ((ziptable (t-ziphash tables)))
    (maphash (lambda (k v)
               (puthash k
                        (apply fn v)
                        ziptable))
             ziptable)
    ziptable))

;; TODO: allow multiple predicates for filter functions

(defun t-filter-keys (table predicate)
  "Filter out entries in TABLE whose keys do not satisfy PREDICATE.

Returns a new hash-table consisting of all key-value pairs for which
  (PREDICATE KEY) returns a true result.

Example:
  (setq squares (t-hash 1 1  2 4  3 9  4 16  5 25))
  ;; #s(hash-table ... data (1 1 2 4 3 9 4 16 5 25))
  (t-filter-keys squares 'oddp)
  ;; #s(hash-table ... data (1 1 3 9 5 25))"
  (let ((result (copy-hash-table table)))
    (maphash (lambda (key value)
               (when (not (funcall predicate key))
                 (remhash key result)))
             result)
    result))

(defun t-filter-values (table predicate)
  "Filter out entries in TABLE whose values do not satisfy PREDICATE.

Returns a new hash-table consisting of all key-value pairs for which
  (PREDICATE VALUE) returns a true result.

  Example:
  (setq squares (t-hash 1 1  2 4  3 9  4 16  5 25))
  ;; #s(hash-table ... data (1 1 2 4 3 9 4 16 5 25))
  (t-filter-values squares (-rpartial #'< 10))
  ;; #s(hash-table ... data (1 1 2 4 3 9))"
  (let ((result (copy-hash-table table)))
    (maphash (lambda (key value)
               (when (not (funcall predicate value))
                 (remhash key result)))
             result)
    result))

(defun t-filter (table predicate)
  "Filter out key-value pairs in TABLE which do not satisfy PREDICATE.

Returns a new hash-table consisting of all key-value pairs for which
  (PREDICATE KEY VALUE) returns a true result.
Example:
  (setq fib (t-hash 1 1  2 1  3 2  4 3  5 5  6 8  7 13  8 21))
  ;; #s(hash-table ... data (1 1 2 1 3 2 4 3 5 5 6 8 7 13 8 21))
  (t-filter fib (lambda (k v) (> k v)))
  ;; #s(hash-table ... data ( 2 1 3 2 4 3))"
  (let ((result (copy-hash-table table)))
    (maphash (lambda (key value)
               (when (not (funcall predicate key value))
                 (remhash key result)))
             result)
    result))

(defun t-collect (table &rest fns)
  "Create a list by applying functions to each key-value pair.

Each function takes as argument a cons cell (KEY . VALUE) and its result is
appended to the list in turn. This process repeats for each key in the table.
Thus, the resulting list has one element per function argument per key.

Example:
  (setq mytable (t-hash :a 1 :b 2 :c 3))
  (t-collect mytable #'identity)
  ;; ((:a . 1) (:b . 2) (:c . 3))  ;; alist
  (t-collect mytable (lambda (p) (list (car p) (cdr p))))
  ;; ((:a 1) (:b 2) (:c 3))        ;; ziplist
  (t-collect mytable #'car #'cdr)
  ;; (:a 1 :b 2 :c 3)              ;; plist
  (t-collect mytable #'car (lambda (p) (* (cdr p) (cdr p))))
  ;; (:a 1 :b 4 :c 9)

See also `t-to-alist', `t-to-plist'

\(fn TABLE FUNCTION...)"
  (declare (indent 1))
  (let (result)
    (maphash (lambda (k v)
               (dolist (fn fns)
                 (!cons (funcall fn (cons k v))
                        result)))
             table)
    (reverse result)))

(defun t-to-alist (table)
  "Return an alist representing the hash-table."
  (t-collect table #'identity))

(defun t-to-plist (table)
  "Return a plist representing the hash-table."
  (t-collect table #'car #'cdr))

(defun t-fold (table init function)
  "Reduce TABLE using FUNCTION on the values, starting with value INIT.

FUNCTION should take a hash-table value and the accumulated result and return
an updated result."
  (let ((result init))
    (t-dohash
        ((_ value) table)
      (setq result (funcall function value result)))
    result))

(defun t-fold-with-key (table init function)
  "Reduce TABLE using FUNCTION, starting with value INIT.

FUNCTION should take a key, its value and the accumulated result and return an
updated result."
  (let ((result init))
    (t-dohash
        ((key value) table)
      (setq result (funcall function key value result)))
    result))

(defun t-keys (&rest tables)
  "Return the set of keys in TABLES as a sorted list."
  (let ((-compare-fn #'eql))
    (->> tables
         (mapcar #'hash-table-keys)
         (apply #'-concat)
         (-uniq)
         (-sort (lambda(a b) (string< (format "%S" a)
                                      (format "%S" b)))))))

(defun t-values (&rest tables)
  "Return the set of values in TABLES as a sorted list."
  (let ((-compare-fn #'eql))
    (->> tables
         (mapcar #'hash-table-values)
         (apply #'-concat)
         (-uniq)
         (-sort (lambda(a b) (string< (format "%S" a)
                                      (format "%S" b)))))))

(provide 't)

;;; t ends here
