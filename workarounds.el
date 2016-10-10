;; ==================================
;; fix deprecated 'avy--with-avy-keys
;; ==================================
(eval-after-load "avy"
  '(when (and (funboundp 'avy--with-avy-keys)
              (fboundp   'avy-with))
     (defalias 'avy--with-avy-keys 'avy-with)))


;; ==========================================================
;; describe-symbol command taken from emacs 25 sources (GPL3)
;; ==========================================================
(global-set-key (kbd "C-h y") 'describe-symbol)

;;;###autoload
(defun describe-symbol (symbol &optional buffer frame)
  "Display the full documentation of SYMBOL.
Will show the info of SYMBOL as a function, variable, and/or face."
  (interactive
   ;; FIXME: also let the user enter a face name.
   (let* ((v-or-f (variable-at-point))
          (found (symbolp v-or-f))
          (v-or-f (if found v-or-f (function-called-at-point)))
          (found (or found v-or-f))
          (enable-recursive-minibuffers t)
          val)
     (setq val (completing-read (if found (format "Describe symbol (default %s): " v-or-f)
                                  "Describe symbol: ")
                                obarray
                                (lambda (vv) (cl-some (lambda (x) (funcall (nth 1 x) vv))
                                                      describe-symbol-backends))
                                t
                                nil
                                nil
                                (if found (symbol-name v-or-f))))
     (list (if (equal val "")
               v-or-f (intern val)))))
  (if (not (symbolp symbol))
      (user-error "You didn't specify a function or variable"))
  (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
  (unless (frame-live-p frame) (setq frame (selected-frame)))
  (with-current-buffer (help-buffer)
    ;; Push the previous item on the stack before clobbering the output buffer.
    (help-setup-xref nil nil)
    (let* ((docs
            (nreverse
             (delq nil
                   (mapcar (pcase-lambda (`(,name ,testfn ,descfn))
                             (when (funcall testfn symbol)
                               ;; Don't record the current entry in the stack.
                               (setq help-xref-stack-item nil)
                               (cons name
                                     (funcall descfn symbol buffer frame))))
                           describe-symbol-backends))))
           (single (null (cdr docs))))
      (while (cdr docs)
        (goto-char (point-min))
        (let ((inhibit-read-only t)
              (name (caar docs))        ;Name of doc currently at BOB.
              (doc (cdr (cadr docs))))  ;Doc to add at BOB.
          (insert doc)
          (delete-region (point) (progn (skip-chars-backward " \t\n") (point)))
          (insert "\n\n"
                  (eval-when-compile
                    (propertize "\n" 'face '(:height 0.1 :inverse-video t)))
                  "\n")
          (when name
            (insert (symbol-name symbol)
                    " is also a " name "." "\n\n")))
        (setq docs (cdr docs)))
      (unless single
        ;; Don't record the `describe-variable' item in the stack.
        (setq help-xref-stack-item nil)
        (help-setup-xref (list #'describe-symbol symbol) nil))
      (goto-char (point-min)))))

(defvar describe-symbol-backends
  `((nil ,#'fboundp ,(lambda (s _b _f) (describe-function s)))
    ("face" ,#'facep ,(lambda (s _b _f) (describe-face s)))
    (nil
     ,(lambda (symbol)
        (or (and (boundp symbol) (not (keywordp symbol)))
            (get symbol 'variable-documentation)))
     ,#'describe-variable)))

;;;###autoload
(defmacro pcase-lambda (lambda-list &rest body)
  "Like `lambda' but allow each argument to be a pattern.
I.e. accepts the usual &optional and &rest keywords, but every
formal argument can be any pattern accepted by `pcase' (a mere
variable name being but a special case of it)."
  (declare (doc-string 2) (indent defun)
           (debug ((&rest pcase-PAT) body)))
  (let* ((bindings ())
         (parsed-body (macroexp-parse-body body))
         (args (mapcar (lambda (pat)
                         (if (symbolp pat)
                             ;; Simple vars and &rest/&optional are just passed
                             ;; through unchanged.
                             pat
                           (let ((arg (make-symbol
                                       (format "arg%s" (length bindings)))))
                             (push `(,pat ,arg) bindings)
                             arg)))
                       lambda-list)))
    `(lambda ,args ,@(car parsed-body)
       (pcase-let* ,(nreverse bindings) ,@(cdr parsed-body)))))

;;;###autoload
(defun macroexp-parse-body (body)
  "Parse a function BODY into (DECLARATIONS . EXPS)."
  (let ((decls ()))
    (while (and (cdr body)
                (let ((e (car body)))
                  (or (stringp e)
                      (memq (car-safe e)
                            '(:documentation declare interactive cl-declare)))))
      (push (pop body) decls))
    (cons (nreverse decls) body)))
