;;; mode-ring.el --- Switch between major modes maintained in a mode-ring.

;; Copyright (C) 2016 Troy Pracy.
;;
;; Author: Troy Pracy
;; Maintainer: Troy Pracy
;; Created: 16 Feb 2016
;; Version: 0.01
;; Package-Requires: ((dash "2.12.1"))
;; Keywords: major-mode mode

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
;;   (require 'mode-ring)

;;; Code:

;; -----------------------------------------------------------------------------


;; *************
;; *           *
;; * MODE-RING *
;; *           *
;; *************

(setq mode-rings (make-hash-table))

(defun mode-ring-push (&optional mode)
  "Insert mode at the front of the mode-ring."
  (interactive)
  (letrec ((ring (gethash (current-buffer) mode-rings))
           (newring (-distinct (cons (or mode major-mode) ring))))
    (puthash (current-buffer) newring mode-rings)))

(defun mode-ring-enqueue (&optional mode)
  "Insert mode at the end of the mode-ring."
  (interactive)
  (letrec ((ring (gethash (current-buffer) mode-rings))
           (newring (reverse (-distinct (reverse (-snoc ring (or mode major-mode)))))))
    (puthash (current-buffer) newring mode-rings)))

(defun mode-ring-pop ()
  "Remove the mode from the front of the mode-ring and switch to it."
  (interactive)
  (if (boundp 'mode-rings)
      (let ((ring (gethash (current-buffer) mode-rings)))
        (if (not (null ring))
            (progn
              (funcall (car ring))
              (setq ring (cdr ring))
              (puthash (current-buffer) ring mode-rings))))))

(defun mode-ring-cycle ()
  "Switch to the mode at the front of the mode-ring and move that mode to the end of the ring."
  (interactive)
  (if (boundp 'mode-rings)
      (letrec ((ring (gethash (current-buffer) mode-rings))
               (newring (-rotate -1 ring)))
        (if (not (null newring))
            (progn
              (funcall (car ring))
              (puthash (current-buffer) newring mode-rings))))))

(defun mode-ring-cycle-to-text-mode ()
  "If the mode-ring is empty, add the current mode. Then switch to text-mode and place text-mode at the end of the ring."
  (interactive)
  (letrec ((ring (gethash (current-buffer) mode-rings))
           (ring-nonempty (or ring (list major-mode)))
           (newring (reverse
                     (-distinct
                      (reverse
                       (-snoc ring-nonempty 'text-mode))))))
    (text-mode)
    (puthash (current-buffer) newring mode-rings)))

(defun mode-ring-list ()
  "Display the list of modes in the mode-ring in the minibuffer."
  (interactive)
  (message (format "%s" (gethash (current-buffer) mode-rings))))

 
;; -----------------------------------------------------------------------------

(define-prefix-command 'mode-ring-prefix-key-map)
(define-key 'mode-ring-prefix-key-map        "q"  'mode-ring-enqueue)
(define-key 'mode-ring-prefix-key-map (kbd "SPC") 'mode-ring-cycle)
(define-key 'mode-ring-prefix-key-map        "u"  'mode-ring-push)
(define-key 'mode-ring-prefix-key-map        "p"  'mode-ring-pop)
(define-key 'mode-ring-prefix-key-map        "l"  'mode-ring-list)
(define-key 'mode-ring-prefix-key-map        "t"  'mode-ring-cycle-to-text-mode)

;; -----------------------------------------------------------------------------

(provide 'mode-ring)

;;; mode-ring.el ends here
