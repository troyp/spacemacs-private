;;; tspjava --- Java configuration.

;; Copyright (C) 2016 Troy Pracy.
;;
;; Author: Troy Pracy
;; Maintainer: Troy Pracy
;; Created:
;; Version:
;; Package-Requires:
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'tspjava)

;;; Code:

;; -----------------------------------------------------------------------------


;; ********
;; *      *
;; * JAVA *
;; *      *
;; ********

(defface font-lock-type-param-face
  `((t (:inherit font-lock-type-face
	:foreground "#DF00E800EF00")))
  "Face for type parameters")

;; crude test version
(defun font-lock-java-extra ()
  (font-lock-add-keywords
   nil
   '(("<\\([A-Za-z_][A-Za-z0-9_]*\\)>" 1 font-lock-type-face)
     ;; ("\\([A-Za-z_][A-Za-z0-9_]*\\)\\(<\\)\\([?]\\|[A-Za-z_][A-Za-z0-9_]*\\)\\s-+\\(super\\|extends\\)\\s-+[^>]*\\(>\\)"
     ;;  1 font-lock-type-face
      ;; 2 font-lock-keyword-face
      ;; 3 font-lock-type-param-face
      ;; 4 font-lock-keyword-face
      ;; 5 font-lock-keyword-face)
     )))

;; ;; FIXME
;; (defun font-lock-java-extra ()
;;   (font-lock-add-keywords
;;    nil
;;    '(
;;      ("\\([A-Za-z_][A-Za-z0-9_]*\\)\\(<\\)\\([A-Za-z_][A-Za-z0-9_]*\\)\\(>\\)"
;;       1 font-lock-type-face
;;       2 font-lock-keyword-face
;;       3 font-lock-type-param-face
;;       4 font-lock-keyword-face)
;;      ("\\([A-Za-z_][A-Za-z0-9_]*\\)\\(<\\)\\([?]\\|[A-Za-z_][A-Za-z0-9_]*\\)\\s-+\\(super\\|extends\\)\\s-+[^>]*\\(>\\)"
;;       1 font-lock-type-face
;;       2 font-lock-keyword-face
;;       3 font-lock-type-param-face
;;       4 font-lock-keyword-face
;;       5 font-lock-keyword-face)
;;      )))

(add-hook 'java-mode-hook 'font-lock-java-extra)

(defun font-lock-fixme ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend))))

(add-hook 'java-mode-hook 'font-lock-fixme)

;; -----------------------------------------------------------------------------

(provide 'tspjava)

;;; tspjava ends here
