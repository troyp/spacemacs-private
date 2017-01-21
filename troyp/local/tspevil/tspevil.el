;;; tspevil --- Utility functions for Evil Mode.

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Version:
;; Package-Requires:
;; Keywords:
;; URL:

;;; Commentary:

;; -------------------------------------------------------------------------------
;; ********************
;; *                  *
;; * HELPER FUNCTIONS *
;; *                  *
;; ********************
(defun printf (s)
  (print s (lambda (c) nil)))

(defun print-list-elements (list &optional PRINTCHARFUN)
  (while list
    (print (car list) PRINTCHARFUN)
    (setq list (cdr list))))

;; -------------------------------------------------------------------------------
;; ***************
;; *             *
;; * VISUAL MODE *
;; *             *
;; ***************

(defun my/line-end-after-pos (pos)
  (save-excursion
	(goto-char pos)
	(line-end-position)))

(defun move-right ()
  "Hack to move right in visual mode without cancelling it.
   Needed for evil-forward-char-or-extend, since I couldn't
   work out a better way that worked."
  (kmacro-exec-ring-item (quote ([right] 0 "%d")) 1))

;; FIXME:
(defun evil-forward-char-or-extend ()
   (interactive)
   (letrec ((pos (marker-position evil-visual-point))
			(line-end (my/line-end-after-pos pos)))
	   (if (eq pos line-end)
		   (progn
			 (save-excursion
			   (replace-regexp "$" " " nil pos (+ pos 1)))
			 (evil-visual-restore))
		 (progn
		   (move-right)))))
(defun evil-forward-char-or-extend ()
   (interactive)
   (let ((pt (point)))
     (move-right)
     (if (eq pt (point))
         (insert-space-visual))))


(global-set-key [s-right]  'evil-forward-char-or-extend)

;; -------------------------------------------------------------------------------
;; *************
;; *           *
;; * JUMP LIST *
;; *           *
;; *************

(if (boundp 'popwin:special-display-config)
    (push "*jumps*" popwin:special-display-config))
(defun jumps ()
  (interactive)
  (with-output-to-temp-buffer "*jumps*"
    (print-list-elements evil-jump-list)))


;; -------------------------------------------------------------------------------
;; ************
;; *          *
;; * DIGRAPHS *
;; *          *
;; ************

(defun evil-enter-digraphs nil
  (interactive)
  (with-demoted-errors
    (evil-insert-digraph 1)
    (evil-enter-digraphs)))


;; ***********************************
;; *                                 *
;; * LOWER-CASE PARENS IN LISP MODES *
;; *                                 *
;; ***********************************

;; racket-mode, emacs-lisp-mode, lisp-mode
(defun insert-lparen ()
  (interactive)
  (insert ?\())
(defun insert-rparen ()
  (interactive)
  (insert ?\)))
(defun insert-lbrack ()
  (interactive)
  (insert ?\[))
(defun insert-rbrack ()
  (interactive)
  (insert ?\]))
(defun lowercase-parens-in-mode-map (mode-map)
  (evil-define-key 'insert mode-map (kbd "[") 'insert-lparen)
  (evil-define-key 'insert mode-map (kbd "]") 'insert-rparen)
  (evil-define-key 'insert mode-map (kbd "(") 'insert-lbrack)
  (evil-define-key 'insert mode-map (kbd ")") 'insert-rbrack))
(defun uppercase-parens-in-mode-map (mode-map)
  (evil-define-key 'insert mode-map (kbd "[") 'self-insert-command)
  (evil-define-key 'insert mode-map (kbd "]") 'self-insert-command)
  (evil-define-key 'insert mode-map (kbd "(") 'self-insert-command)
  (evil-define-key 'insert mode-map (kbd ")") 'self-insert-command))
(defvar lisp-mode-maps
  '( emacs-lisp-mode-map
     lisp-mode-map
     racket-mode-map ))
(defun lowercase-parens-in-lisp-modes ()
  (interactive)
  (if (not (boundp 'lowercase-parens-p)) (defvar lowercase-parens-p nil))
  (if (not lowercase-parens-p)
      (progn
	(setf lowercase-parens-p t)
	(dolist (mode-map lisp-mode-maps)
	  (lowercase-parens-in-mode-map mode-map)))
    (progn
	(setf lowercase-parens-p nil)
	(dolist (mode-map lisp-mode-maps)
	  (uppercase-parens-in-mode-map mode-map)))))


(provide 'tspevil)

;;; tspevil ends here
