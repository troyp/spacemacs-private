;; ==================================
;; fix deprecated 'avy--with-avy-keys
;; ==================================
(eval-after-load "avy"
  '(when (and (funboundp 'avy--with-avy-keys)
              (fboundp   'avy-with))
     (defalias 'avy--with-avy-keys 'avy-with)))

;; ===================
;; fix 'move-text-down
;; ===================
;; Getting a strange error about 'save-mark-and-excursion being an invalid function,
;; even though it works when I call it.
;; I can only fix it by redefining move-text--last-line-is-just-newline
;; Even:
;;     (fmkunbound 'move-text--last-line-is-just-newline)
;;     (unload-feature 'move-text)
;;     (require 'move-text)
;; isn't sufficient to fix it.
;; Save-mark-and-excursion is new in Emacs 25: is move-text somehow getting
;; stuck looking in Emacs 24 definitions?
(defun move-text--last-line-is-just-newline ()
  "Predicate, is last line just a newline?"
  (save-mark-and-excursion
   (goto-char (point-max))
   (beginning-of-line)
   (= (point-max) (point))))

;; ==========================================================
;; describe-symbol command taken from emacs 25 sources (GPL3)
;; ==========================================================
(when (< (string-to-number emacs-version)
         25)

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

  )

;; =============
;; avy-goto-word
;; =============

;; redefine - make avy work with form-feed
(defun avy-goto-word-1 (char &optional arg)
  "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (if (= char 12)
      (avy-goto-subword-0 1 (lambda () (= 12 (char-after))))
    (avy-with avy-goto-word-1
              (let* ((str (string char))
                     (regex (cond ((string= str ".")
                                   "\\.")
                                  ((and avy-word-punc-regexp
                                        (string-match avy-word-punc-regexp str))
                                   (regexp-quote str))
                                  (t
                                   (concat
                                    "\\b"
                                    str)))))
                (avy--generic-jump regex arg avy-style)))))

;; =========
;; move-text
;; =========
(eval-after-load "move-text.el"
  `(progn
     (undefun 'move-text--last-line-is-just-newline)
     (workarounds)))


;; ===================================
;; TLS: https://bugs.debian.org/766397
;; ===================================
(setq tls-program '("gnutls-cli --x509cafile %t -p %p %h"))


;; ==================
;; FIX EVIL-PASTE-POP
;; ==================
;; update list of paste functions to include `spacemacs/evil-mc-paste-after'
;; and `spacemacs/evil-mc-paste-before'.
;; see: https://github.com/syl20bnr/spacemacs/issues/8823
(defun evil-paste-pop (count)
  "Replace the just-yanked stretch of killed text with a different stretch.
This command is allowed only immediatly after a `yank',
`evil-paste-before', `evil-paste-after' or `evil-paste-pop'.
This command uses the same paste command as before, i.e., when
used after `evil-paste-after' the new text is also yanked using
`evil-paste-after', used with the same paste-count argument.

The COUNT argument inserts the COUNTth previous kill.  If COUNT
is negative this is a more recent kill."
  (interactive "p")
  (unless (memq last-command
                '(evil-paste-after
                  evil-paste-before
                  evil-visual-paste
                  spacemacs/evil-mc-paste-after
                  spacemacs/evil-mc-paste-before
                  ))
    (user-error "Previous command was not an evil-paste: %s" last-command))
  (unless evil-last-paste
    (user-error "Previous paste command used a register"))
  (evil-undo-pop)
  (goto-char (nth 2 evil-last-paste))
  (setq this-command (nth 0 evil-last-paste))
  ;; use temporary kill-ring, so the paste cannot modify it
  (let ((kill-ring (list (current-kill
                          (if (and (> count 0) (nth 5 evil-last-paste))
                              ;; if was visual paste then skip the
                              ;; text that has been replaced
                              (1+ count)
                            count))))
        (kill-ring-yank-pointer kill-ring))
    (when (eq last-command 'evil-visual-paste)
      (let ((evil-no-display t))
        (evil-visual-restore)))
    (funcall (nth 0 evil-last-paste) (nth 1 evil-last-paste))
    ;; if this was a visual paste, then mark the last paste as NOT
    ;; being the first visual paste
    (when (eq last-command 'evil-visual-paste)
      (setcdr (nthcdr 4 evil-last-paste) nil))))
