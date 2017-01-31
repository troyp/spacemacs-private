;;; region-swap.el --- Swap two regions

;; Copyright (C) 2017 Troy Pracy

;; Author: Troy Pracy
;; Keywords: region
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") ())

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

(defvar region-swap-location-A nil)
(defvar region-swap-region-A "")
(defvar region-swap-region-B "")
(setq region-swap-location-A nil)
(setq region-swap-region-A "")
(setq region-swap-region-B "")

(defun region-swap-mark (beg end)
  (interactive "r")
  (setq region-swap-location-A (list (current-buffer) (min beg end) (max beg end)))
  (setq region-swap-region-A (buffer-substring beg end)))

(defun region-swap-execute (beg end)
  (interactive "r")
  ;; copy region B, replace with region A
  (setq region-swap-region-B (buffer-substring beg end))
  (kill-region beg end)
  (insert region-swap-region-A)
  (setq region-swap-region-A "")
  ;; replace region A with region B
  (destructuring-bind (bufA begA endA) region-swap-location-A
    (save-excursion
      (with-current-buffer bufA
        (goto-char begA)
        (kill-region begA endA)
        (insert region-swap-region-B)
        (setq region-swap-region-B "")
        (setq region-swap-location-A nil)))))

(defun region-swap (beg end)
  (interactive "r")
  (if (member region-swap-region-A '("" nil))
      (region-swap-mark beg end)
    (region-swap-execute beg end)))

(provide 'region-swap)

;;; region-swap ends here
