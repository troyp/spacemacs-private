;;; tsplib-evil.el --- utility functions related to evil-mode

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: evil library
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

(require 'tsplib)


(defun evil-set-region-to-symbol-at-point ()
  "Set evil region to the symbol at point.

Evil-mode equivalent of `set-region-to-symbol-at-point'."
  (evil-visual-make-selection
   (cadr (symbol-at-point-with-bounds))
   (- (cddr (symbol-at-point-with-bounds)) 1)))

(defun evil-get-visual-region-or-symbol-at-point ()
  "Return cons representing the bounds of the evil region or symbol at point."
  (let ((r (if (region-active-p)
               (cons (region-beginning) (- (region-end) 1))
             (cons
              (cadr (symbol-at-point-with-bounds))
              (- (cddr (symbol-at-point-with-bounds)) 1)))))
    r))

(defun evil-get-visual-region-or-buffer ()
  "Return cons representing the bounds of the current evil region or the entire buffer."
  (let ((r (if (region-active-p)
               (cons (region-beginning) (- (region-end) 1))
             (cons (point-min) (point-max)))))
    r))

;; ,---------------------,
;; | BUFFERS AND WINDOWS |
;; '---------------------'

;; code adapted from spacemacs' 'spacemacs/switch-to-scratch-buffer
(defun my/switch-to-scratch-buffer-other-window ()
  "Switch to the `*scratch*' buffer. Create it first if needed."
  (interactive)
  (let ((exists (get-buffer "*scratch*")))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (when (and (not exists)
               (not (eq major-mode dotspacemacs-scratch-mode))
               (fboundp dotspacemacs-scratch-mode))
      (funcall dotspacemacs-scratch-mode))))


(provide 'tsplib-evil)

;;; tsplib-evil ends here
