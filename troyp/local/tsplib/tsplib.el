
;;; tsplib.el --- Utility functions and macros.

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: library functions
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


;; ======CURRENT LINE OR REGION======

(defun my/line-visible-beginning-position ()
  "Return the position of the first visible character on the current line."
  (save-excursion
    (back-to-indentation)
    (point)))

(defun my/line-visible-end-position ()
  "Return the position after the last visible character on the current line."
  (save-excursion
    (end-of-line)
    (let* (start (line-beginning-position)
          (search-result (re-search-backward "[^ \t\n]" start t)))
      (1+ (or search-result start)))))

(defun my/get-current-line ()
  (when (my/line-visible-end-position)
    (buffer-substring (my/line-visible-beginning-position) (my/line-visible-end-position)))

  (defun match-line (regexp)
    "Check the current line against regexp and return the match position the or nil
 if it fails."
    (save-excursion
      (beginning-of-line)
      (re-search-forward regexp (line-end-position) t))))

(defun get-region-or-buffer ()
  "Return cons representing the bounds of the current region or the entire buffer."
  ;; see also 'evil-get-visual-region-or-buffer
  (let ((r (if (region-active-p)
               (cons (region-beginning) (region-end))
             (cons (point-min) (point-max)))))
    r))

;; =====CURRENT POINT POSITION=====
(defun set-region-to-symbol-at-point ()
  "Set region to the symbol at point."
  ;; see also evil-set-region-to-symbol-at-point
  (posn-set-point (posn-at-point (cadr (symbol-at-point-with-bounds))))
  (push-mark                     (cddr (symbol-at-point-with-bounds))))

(defun get-region-or-symbol-at-point ()
  "Return cons representing the bounds of the region or symbol at point."
  ;; see also 'evil-get-visual-region-or-symbol-at-point.
  ;; TODO: handle error when no region and not on symbol
  (let ((r (if (region-active-p)
               (cons (region-beginning) (region-end))
             (cons
              (cadr (symbol-at-point-with-bounds))
              (- (cddr (symbol-at-point-with-bounds)) 1)))))
    r))

;; =====FILE & DIRECTORY FUNCTIONS=====
(defun concat-as-directory (&rest parts)
  "Concatenate a group of path components, adding trailing separators where needed."
  (cl-loop for part in parts concat (file-name-as-directory part)))
(defun concat-as-file-path (&rest parts)
  "Concatenate a group of path components, with a final filename, adding trailing
 separators where needed."
  (cl-loop for partsleft on parts
           concat (let ((part (car partsleft)))
                    (if (cdr partsleft)
                        (file-name-as-directory part)
                      part))))



(provide 'tsplib)

;;; tsplib ends here
