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
  ;; #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8 data (:a 1 :b 2))
  (t-make-hash (:size 120 :rehash-threshold 0.9)
    :a 1
    :d 4)
  ;; #s(hash-table size 120 test eql rehash-size 1.5 rehash-threshold 0.9 data (:a 1 :d 4))

See also `t-hash'

\(fn ([PROP-NAME1 PROP-VALUE1...]) [KEY1 VAL1...])"
  (declare (indent 1))
  `(cl-loop with    table = (apply #'make-hash-table ',proplist)
            for     (k v) on ',tabledata by #'cddr
            do      (puthash k v table)
            finally return table))

(defmacro t-hash (&rest data)
  "Create a new hash-table with default properties from keys and values.

Example:
  (t-hash :a 1 :b 2)
  ;; #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8 data (:a 1 :b 2))

See also `t-make-hash'

  \(fn [KEY1 VAL1...])"
  `(cl-loop with    table = (make-hash-table)
            for     (k v) on ',data by #'cddr
            do      (puthash k v table)
            finally return table))

(defmacro t-zip-to-hash (keylist valuelist)
  "Create a hash-table from corresponding lists of keys and values.

Example:
  (t-zip-to-hash '(:a :b :c) '(2 3 5))
  ;; #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8 data (:a 2 :b 3 :c 5))"
  `(cl-loop with    table = (make-hash-table)
            for     k in ,keylist
            for     v in ,valuelist
            do      (puthash k v table)
            finally return table))

(defun t-contains? (table key)
  "Returns `t' if TABLE contains the key KEY, `nil' otherwise. Identical to
`t-in-hash-p' except with the arguments in the other order."
  (not (eql (gethash key table t--key-absent) t--key-absent)))

(defun t-in-hash-p (key table)
  "Returns `t' if TABLE contains the key KEY, `nil' otherwise. Identical to
`t-contains?' except with the arguments in the other order."
  (t-contains? table key))

(defun t-conj (table &rest args)
  "Returns a new hash-table created by adding new key-value associations to TABLE.
The new data is given as an even number of additional arguments KEY1 VAL1 KEY2 VAL2...

\(fn TABLE [KEY1 VAL1...])"
  (declare (indent 1))
  (let ((result (copy-hash-table table)))
    (cl-loop for (key val) on args by #'cddr
             do (puthash key val result))
    result))

(defun t-conj! (table &rest args)
  "Adds new key-value associations to TABLE. The new data is given as an even
number of additional arguments KEY1 VAL1 KEY2 VAL2 ...

The return value is TABLE.

\(fn TABLE [KEY1 VAL1...])"
  (declare (indent 1))
  (cl-loop for (key val) on args by #'cddr
           do (puthash key val table))
  table)

(defun t-do! (table &rest fns)
  "Executes each function in FNS for each key-value pair in TABLE. Each function
takes 2 arguments, (KEY VAL).

Note that the return value is the original table. This is useful if the table is
to be used in a sequence of operations, eg. with dash.el's threading macro `->'."
  (declare (indent 1))
  (maphash (lambda (k v)
             (cl-loop for fn in fns
                      do (funcall fn k v)))
           table)
  table)

(defun t-map (fn table)
  "Returns a new hash-table formed by associating each key in TABLE with the
result of applying FN to its associated value.

See also `t-map!', `t-maphash', `t-maphash!'"
  (let ((result (make-hash-table)))
    (maphash (lambda (k v) (puthash k (funcall fn v) result))
             table)
    result))

(defun t-map! (fn table)
  "Transforms TABLE by associating each key in TABLE with the result of applying
FN to its associated value.

See also `t-map', `t-maphash!', `t-maphash'"
  (maphash (lambda (k v)
             (puthash k (funcall fn v) table))
             table)
    table)

(defmacro t-dohash (spec &rest forms)
  "Iterate through TABLE, executing BODY for each key-value pair.
For each iteration, KEYVAR is bound to the key and VALUEVAR is bound to the value.

\(fn ((KEYVAR VALUEVAR) TABLE) BODY...)"
  (declare (indent 1))
   (let* ((vars  (car spec))
          (kvar  (car vars))
          (vvar  (cadr vars))
          (table (cadr spec)))
     `(maphash (lambda (,kvar ,vvar)
                 ,@forms)
               ,table)))

(defmacro t-for (table &rest forms)
  "Iterates through the keys of TABLE, binding the variables 'k and 'v to each
key-value pair in turn, and executes FORMS. Returns nil. This is a more concise
alternative to `t-dohash'."
  (declare (indent 1))
  `(maphash (lambda (k v)
              ,@forms)
            ,table))

(defmacro t-do-collect (table &rest forms)
  "Iterates through the keys of TABLE, binding the variables 'key and 'val to
each key-value pair in turn, and executes FORMS. The last expression in FORMS
in each iteration is collected into a list and returned as the result."
  (declare (indent 1))
  `(progn
     (let (result)
       (t-for ,table
              (!cons (progn ,@forms)
                     result))
       (reverse result))))

(defun t-transform! (table &rest fns)
  "Transforms TABLE by associating each key with the result of successively
applying each function in FNS (from left to right) to its associated value.

See also `t-transform'"
  (declare (indent 1))
  (t-map! (apply #'-compose (reverse fns)) table))

(defun t-transform (table &rest fns)
  "Returns a new hash-table formed by associating each key in TABLE with the
result of successively applying each function in FNS to its associated value.

See also `t-transform!'"
  (declare (indent 1))
  (t-map (apply #'-compose (reverse fns)) table))

(defun t-merge (&rest maps)
  "Merges MAPS to create a combined hash-map. When multiple maps share a key,
the last (rightmost) map takes precedence. The equalitiy test used is `eql'.

See also `t-merge!', `t-merge-with!', `t-merge-with'"
  (let ((result (make-hash-table)))
    (cl-flet ((result-put (key val) (puthash key val result)))
        (cl-loop for map in maps
                 do (maphash #'result-put map)))
    result))

(defun t-merge! (base-map &rest other-maps)
  "Updates BASE-MAP with the keys and values in OTHER-MAPS. OTHER-MAPS are
processed from left to right, so when multiple maps share a key, the last
map takes precedence. The equality test used is `eql'.

The return value is BASE-MAP.

See also `t-merge', `t-merge-with!', `t-merge-with'"
  (cl-flet ((result-put (key val) (puthash key val base-map)))
    (cl-loop for map in other-maps
             do (maphash #'result-put map)))
  base-map)

(defalias 't-update 't-merge!)

(defun t-ziphash (&rest tables)
  "Returns a new hash-table formed by applying `nconc' to the values in each
table associated with a given key."
  (let ((zipmap (make-hash-table)))
    (cl-loop for tab in tables
             do (maphash (lambda (k v)
                           (puthash k
                                    (nconc (list v)
                                           (gethash k zipmap))
                                    zipmap))
                         tab))
    zipmap))

(defun t-merge-with (fn &rest tables)
  "Returns a new hash-table formed by merging TABLES. For each key that occurs
in one or more tables, the associated values in each map are combined by
applying FN.

See also `t-merge-with!', `t-merge', `t-merge!'"
  (declare (indent 1))
  (let ((zipmap (t-ziphash tables)))
    (maphash (lambda (k v)
               (puthash k
                        (apply fn v)
                        zipmap))
             zipmap)
    zipmap))

(defun t-maphash (fn table)
  "returns a new hash-table formed by associating each key in table with the
result of (fn key val).

See also `t-maphash!', `t-map', `t-map!'"
  (let ((result (make-hash-table)))
    (maphash (lambda (key val)
               (puthash key (funcall fn key val) result))
             table)
    result))

(defun t-maphash! (fn table)
  "Transforms TABLE by associating each key in table with the result of (FN key val).
Returns TABLE.

See also `t-maphash', `t-map', `t-map!'"
  (maphash (lambda (k v)
             (puthash k (funcall fn k v) table))
           table)
    table)

(defun t-collect (table &rest fns)
  "Returns a list created by collecting the results of each function in FNS to
a cons pair (KEY . VAL) for each KEY,VAL pair in TABLE. For each key in the table,
one value is appended to the list per function argument.

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

See also `t-to-alist', `t-to-plist'"
  (declare (indent 1))
  (let (result)
    (maphash (lambda (k v) (dolist (fn fns)
                             (!cons (funcall fn (cons k v))
                                    result)))
             table)
    (reverse result)))

(defun t-to-alist (table)
  "Returns an alist representing the hash-table."
  (t-collect table #'identity))

(defun t-to-plist (table)
  "Returns a plist representing the hash-table."
  (t-collect table #'car #'cdr))

(defun t-keys (&rest tables)
  "Returns the set of keys in TABLES as a sorted list."
  (let ((-compare-fn #'eql))
    (->> tables
         (mapcar #'hash-table-keys)
         (apply #'-concat)
         (-uniq)
         (-sort (lambda(a b) (string< (format "%S" a)
                                      (format "%S" b)))))))

(defun t-values (&rest tables)
  "Returns the set of values in TABLES as a sorted list."
  (let ((-compare-fn #'eql))
    (->> tables
         (mapcar #'hash-table-values)
         (apply #'-concat)
         (-uniq)
         (-sort (lambda(a b) (string< (format "%S" a)
                                      (format "%S" b)))))))

(provide 't)

;;; t ends here
