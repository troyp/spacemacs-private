;;; fn-arith.el --- Functional utilities for Emacs Lisp   -*- lexical-binding: t -*-

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: functional math
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

;; (require 'fn)

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



(provide 'fn-arith)

;;; fn-arith.el ends here
