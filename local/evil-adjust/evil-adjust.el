;;; evil-adjust.el --- Adjustments for evil-mode.

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: evil
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

  (defun evil-eval-print-last-sexp (&optional arg)
    "Evaluate the sexp before point and print it on a new line.

This function is a wrapper around `eval-print-last-sexp' which corrects for
cursor position in normal/visual states when `evil-move-cursor-back' is set to
`t' (as by default).

Long output is truncated. See the variables `eval-expression-print-length' and
`eval-expression-print-level'.
A prefix argument of 0 inhibits truncation and prints integers with additional
octal, hexadecimal and character representations, in the format: 1 (#o1, #x1,
?\C-a).
Errors start the debugger unless an argument of `nil' is passed for
`eval-expression-debug-on-error'."
    (interactive "P")
    (cl-case evil-state
      ('normal (progn
                 (evil-append 1)
                 (eval-print-last-sexp arg)
                 (evil-normal-state)
                 ))
      ('visual (progn
                 (evil-append 1)
                 (eval-print-last-sexp arg)
                 (evil-visual-restore)
                 ))
      (otherwise (eval-print-last-sexp arg))
      ))

(defun evil-adjust ()
  "Initialize evil adjustments.

This function must be called after the variable `evil-move-cursor-back' is set."
  (interactive)
  (when evil-move-cursor-back
    (define-key lisp-interaction-mode-map
      [remap eval-print-last-sexp] 'evil-eval-print-last-sexp)
    ))

(provide 'evil-adjust)

;;; evil-adjust ends here
