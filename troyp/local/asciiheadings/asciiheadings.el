;;; asciiheadings --- Functions for ascii headings and comments.

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Version:
;; Package-Requires:
;; Keywords:
;; URL:

;;; Commentary:

;;; Code:

(require 's)

;; ===========================================================================
;;        ___________________
;;       |                   |
;;       | HEADING FUNCTIONS |
;;       |___________________|

(require 'tsputils)

;; TODO: rename functions with module prefix
;; TODO: refactor

;; ---------------------------------------------------------------------------
;; ,-----------------------,
;; | Box Heading Functions |
;; '-----------------------'

(defvar *box-heading-symbol* ?*)
(defvar *box-heading-margin* 1)
(defvar *box-heading-vmargin* 1)
(defvar *banner-heading-symbol* ?=)
(defvar *heading-symbol* ?-)

(defun box-heading-lines (s)
  (unless (has-value-p *box-heading-symbol*) (setq *box-heading-symbol* "**"))
  (unless (has-value-p *box-heading-margin*) (setq *box-heading-margin* 1))
  (unless (has-value-p *box-heading-vmargin*) (setq *box-heading-vmargin* *box-heading-margin*))
  (let* ((lines (split-string s "\n"))
	 (n     (apply #'max (mapcar 'length lines)))
	 (d     (or *box-heading-margin* 1))
	 (syms  (if (characterp *box-heading-symbol*)
		    (string *box-heading-symbol* *box-heading-symbol*)
		  *box-heading-symbol*))
	 (nsyms (length syms))
	 (hsym         (elt syms 0))
	 (hsym-bottom  (if (< nsyms 8) hsym
			 (elt syms 1)))
	 (vsym         (if (< nsyms 8) (elt syms 1)
			 (elt syms 2)))
	 (vsym-right   (if (< nsyms 8) vsym
			 (elt syms 3)))
	 (cnrs         (cond ((= nsyms 2) (make-string 4 hsym))
			     ((= nsyms 3) (make-string 4 (elt syms 2)))
			     ((= nsyms 6) (subseq syms 2))
			     ((= nsyms 8) (subseq syms 4))))
	 (nw (string (aref cnrs 0)))
	 (ne (string (aref cnrs 1)))
	 (sw (string (aref cnrs 2)))
	 (se (string (aref cnrs 3)))
	 (spc   32)    ; space
	 (N     (+ n 2 (* 2 d)))
	 (v-margin         (make-string d spc))
	 (h-border-top     (concat nw (make-string (- N 2) hsym) ne))
	 (h-border-bottom  (concat sw (make-string (- N 2) hsym-bottom) se))
	 (h-margin-line    (concat (string vsym) (make-string (- N 2) spc) (string vsym-right)))
	 (h-margin         (make-list *box-heading-vmargin* h-margin-line))
	 (textlines        (mapcar (lambda (s)
				     (concat (string vsym) v-margin s
					     (make-string (- n (length s)) spc)
					     v-margin (string vsym-right)))
				   lines)))
    (flatlines h-border-top
	       h-margin
	       textlines
	       h-margin
	       h-border-bottom)))

(defun box-heading-custom-lines (s syms)
  (let ((*box-heading-symbol* syms))
    (box-heading-lines s)))

;; ---------------------------------------------------------------------------
;; ,-------------------------------,
;; | Rectangular Heading Functions |
;; '-------------------------------'

;; Borders can be omitted to join adjacent rectangles

(defvar *rect-heading-omit-left*)
(defvar *rect-heading-omit-right*)
(defvar *rect-heading-omit-top*)
(defun rect-heading-lines (s &optional no-trailing-space)
  (unless (has-value-p *rect-heading-omit-top*)   (setq *rect-heading-omit-top* nil))
  (unless (has-value-p *rect-heading-omit-left*)  (setq *rect-heading-omit-left* nil))
  (unless (has-value-p *rect-heading-omit-right*) (setq *rect-heading-omit-right* nil))
  (let* ((lines (split-string s "\n"))
	 (n     (apply #'max (mapcar 'length lines)))
	 (d     (or *box-heading-margin* 1))
	 (spc   32)    ; space
	 (N     (+ n 2 (* 2 d)))
	 (lspc  (if *rect-heading-omit-left* "" " "))
   (rspc  (unless no-trailing-space (if *rect-heading-omit-right* "" " ")))
	 (l  (if *rect-heading-omit-left* "" "|"))
	 (r  (if *rect-heading-omit-right* "" "|"))

	 (v-margin         (make-string d spc))
	 (h-top            (concat lspc (make-string (- N 2) ?_) rspc))
	 (h-bottom         (concat l (make-string (- N 2) ?_) r))
	 (h-margin         (concat l (make-string (- N 2) spc) r))
	 (textlines        (mapcar
			    (lambda (s)
			      (concat l v-margin s
				      (make-string (- n (length s)) spc)
				      v-margin r))
			    lines))
	 (lines            (append
			    (if *rect-heading-omit-top* nil (list h-top))
			    (list h-margin textlines h-bottom))))
    (funcall 'flatlines lines)))

(defun short-rect-heading-lines (s)
  (let ((*box-heading-vmargin* 0)
	(*box-heading-symbol* "--||,,''"))
    (box-heading-lines s)))


;; ===========================================================================
;;        __________________
;;       |                  |
;;       | HEADING COMMANDS |
;;       |__________________|


;; ---------------------------------------------------------------------------
;; ,-----------------------------,
;; | Plain Text Heading Commands |
;; '-----------------------------'

(defun make-heading-command (heading-lines-fn-sym)
  `(lambda (s)
     (beginning-of-line)
     (interactive "s")
     (insert-lines (,heading-lines-fn-sym s))))

(fset 'box-heading (make-heading-command 'box-heading-lines))
(fset 'rect-heading (make-heading-command (lambda (s) (rect-heading-lines s 'notrailing))))
(fset 'short-rect-heading (make-heading-command 'short-rect-heading-lines))

(defun rect-heading-join-under (s)
  (let ((*rect-heading-omit-top* 't))    (rect-heading s)))
(defun rect-heading-join-right (s)
  (let ((*rect-heading-omit-left* 't))   (rect-heading s)))
(defun rect-heading-join-left (s)
  (let ((*rect-heading-omit-right* 't))  (rect-heading s)))
(defun rect-heading-join-above (s)
  (let ((*rect-heading-omit-bottom* 't)) (rect-heading s)))

;; ---------------------------------------------------------------------------
;; ,--------------------------,
;; | Heading Comment Commands |
;; '--------------------------'

(defun make-heading-comment (heading-fn-sym)
  `(lambda (s)
     (interactive "s" "heading: ")
     (beginning-of-line)
     (let ((start (point)))
       (,heading-fn-sym s)
       (comment-region start (point))
       (indent-region start (point)))))

(fset 'box-heading-comment  (make-heading-comment 'box-heading))
(fset 'rect-heading-comment (make-heading-comment 'rect-heading))
(fset 'short-rect-heading-comment (make-heading-comment 'short-rect-heading))

;; -----------------------------------------------------------------------------
;; ,--------------------,
;; | Underline Commands |
;; '--------------------'

(defun underline (&optional char length-adjustment start-new-line)
  "Underlines the non-whitespace portion of the current line.
   CHAR (default ?-) is the character used in the underline.
   LENGTH-ADJUSTMENT is the amount (possibly negative) to be added to the underline length.
   START-NEW-LINE, if true, opens a new line after the underline and leaves the cursor on it."
  (interactive)
  (unless char (setf char ?-))
  (unless length-adjustment (setf length-adjustment 0))
  (let* ((beg (save-excursion (back-to-indentation)
                              (point)))
         (end (line-end-position))
         (content-end (progn
                        (goto-char end)
                        (skip-syntax-backward " " beg)
                        (point)))
         (indent (current-indentation))
         (length (+ (- content-end beg)
                    length-adjustment))
         (end-of-underline-pos nil))
    (save-excursion
      (end-of-line)
      (newline)
      (indent-to indent)
      (insert-char char length)
      (setf end-of-underline-pos (point)))
    (when start-new-line
      (goto-char end-of-underline-pos)
      (newline))))

(defun banner-heading-comment-current-line ()
  ;; TODO: make undo-able in single step
  ;; TODO: rewrite in terms of box-heading
  (interactive)
  (let ((char (cond
               ((equal current-prefix-arg '(4)) ?-)
               ((equal current-prefix-arg '(16)) ?=)
               ((characterp current-prefix-arg) current-prefix-arg)
               (t ?=))))
    (underline-comment char)
    (save-excursion
      (next-line 1)
      (copy-current-line)
      (kill-whole-line)
      (previous-line 2)
      (beginning-of-line)
      (yank))))

(defun underline-and-open-line-below (&optional char length-adjustment)
  (interactive)
  (underline char length-adjustment t))

(defun underline-comment (&optional char length-adjustment start-new-line)
  (interactive)
  (unless length-adjustment (setf length-adjustment 0))
  (let* ((comment-str-len     (comment-str-total-length (string= comment-start ";")))
         (length-delta        (- length-adjustment comment-str-len))
         (end-of-comment-pos  nil))
    (save-excursion
      (underline char length-delta nil)
      (next-line)
      (comment-region (line-beginning-position) (line-end-position))
      (setf end-of-comment-pos (line-end-position)))
    (when start-new-line
      (goto-char end-of-comment-pos)
      (newline))))

(defun underline-comment-and-open-line-below (&optional char length-adjustment)
  (interactive)
  (underline-comment char length-adjustment t))

(cl-defun comment-str-total-length (&optional (adjusted nil))
  "Returns the total length of comment characters used in comment commands.
Comment commands double a single-char comment-start string when there is no
comment-end string. ADJUSTED (default value: 't) corrects the result for this."
  (let* (;; don't combine padding in comment-start and comment-padding
         (comment-padding    (if (s-ends-with? " " comment-start) "" comment-padding))
         (comment-start-len  (length comment-start))
         (comment-end-len    (length comment-end))
         (comment-pad-len    (length comment-padding))
         (line-end-comment-p (string-empty-p comment-end))
         (block-comment-p    (not line-end-comment-p))
         (comment-adjusted-start-len  (if (and adjusted
                                               line-end-comment-p
                                               (= comment-start-len 1))
                                          2
                                        comment-start-len))
         ;; adjustment: if no comment-end string and comment-start is a single char,
         ;;             comment functions use two comment-start characters.
         (comment-total-pad-len   (cond (line-end-comment-p       comment-pad-len)
                                        (block-comment-p       (* 2 comment-pad-len)))))
    (+ comment-adjusted-start-len
       comment-end-len
       comment-total-pad-len)))

;; ===========================================================================
;;        ____________________________
;;       |                            |
;;       | DIVIDER & SECTION COMMANDS |
;;       |____________________________|


;; ---------------------------------------------------------------------------
;; ,------------------,
;; | Divider Commands |
;; '------------------'

(defvar *divider-char* ?-)
(defvar divider-use-np nil
 "Indicates that divider commands should use an NP form feed character:

rather than a string of regular characters by default.")

(defun divider (n)
  "Insert a divider above the current line."
  (interactive "p")
  (if (= n 1) (setf n 79))
  (move-beginning-of-line nil)
  (if divider-use-np
      (insert "")
  (insert (make-string n *divider-char*)))
  (newline))

(defun divider-np (n)
  (interactive "p")
  (let ((divider-use-np t))
    (divider n)))

(defun divider-thick (n)
  (interactive "p")
  (let ((*divider-char* ?=))
    (if (= n 1) (setf n 79))
    (move-beginning-of-line nil)
    (insert (make-string n *divider-char*))
    (newline)))

(defun divider-comment (n)
  (interactive "p")
  (with-comment divider n))

(defun divider-thick-comment (n)
  (interactive "p")
  (with-comment divider-thick n))

;; ---------------------------------------------------------------------------
;; ,------------------,
;; | Section Commands |
;; '------------------'

(defun srh-section-comment (s &optional n)
  "Simple rectangular section heading. Using comment syntax, inserts a
divider followed by a simple rectangular heading."
  (interactive "sheading: \np")
  (if (= n 1) (setf n 79))
  (beginning-of-line)
  (push-mark (point))
  (divider n)
  (insert-lines (short-rect-heading-lines s))
  (open-line 1)
  (comment-region (mark) (point))
  (indent-region  (mark) (point))
  (push-mark (point))
  (next-line)
  (pop-mark))

(defun rh-section-comment (s &optional n &optional heading-indent)
  "Rectangular section heading. Using comment syntax,
inserts a divider followed by a rectangular heading."
  (interactive "sheading: \np")
  (if (= n 1) (setf n 79))
  (beginning-of-line)
  (unless heading-indent
    (setf heading-indent (/ (- n (length s)) 2) ))
  (push-mark (point))
  (let ((*divider-char* ?=))
    (divider n))
  (insert-lines (indent-lines (rect-heading-lines s)
			      heading-indent))
  (open-line 1)
  (comment-region (mark) (point))
  (next-line)
  (pop-mark))

;; -------------------------------------------------------------------------------
;; ,--------------,
;; | Bar Headings |
;; '--------------'

(fset 'comment-bar-heading-5=
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item
         (quote ("gu$vils=vils=vils=vils=vils= ;;" 0 "%d"))
         arg)))

;; -----------------------------------------------------------------------------

(define-prefix-command 'asciiheadings-prefix-key-map)
(define-prefix-command 'asciiheadings-comment-prefix-key-map)
(define-key 'asciiheadings-prefix-key-map   ";"  'asciiheadings-comment-prefix-key-map)
(define-key 'asciiheadings-prefix-key-map   "_"  'underline)
(define-key 'asciiheadings-prefix-key-map   ";_"  'underline-comment)
(define-key 'asciiheadings-prefix-key-map   "u"  'underline-and-open-line-below)
(define-key 'asciiheadings-prefix-key-map   ";u"  'underline-comment-and-open-line-below)
(define-key 'asciiheadings-prefix-key-map   "d"  'divider)
(define-key 'asciiheadings-prefix-key-map   ";d"  'divider-comment)
(define-key 'asciiheadings-prefix-key-map   "t"  'divider-thick)
(define-key 'asciiheadings-prefix-key-map   ";t"  'divider-thick-comment)
(define-key 'asciiheadings-prefix-key-map   "b"  'box-heading)
(define-key 'asciiheadings-prefix-key-map   ";b"  'box-heading-comment)
(define-key 'asciiheadings-prefix-key-map   "R"  'rect-heading)
(define-key 'asciiheadings-prefix-key-map   ";R"  'rect-heading-comment)
(define-key 'asciiheadings-prefix-key-map   "r"  'short-rect-heading)
(define-key 'asciiheadings-prefix-key-map   ";r"  'short-rect-heading-comment)
(define-key 'asciiheadings-prefix-key-map   "S"  'rh-section-comment)
(define-key 'asciiheadings-prefix-key-map   "s"  'srh-section-comment)
(define-key 'asciiheadings-prefix-key-map   "="  'banner-heading-comment-current-line)

;; -----------------------------------------------------------------------------


(provide 'asciiheadings)

;;; asciiheadings ends here
