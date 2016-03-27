;;; tspplmodes --- Configuration for Programming Languages

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
;;   (require 'tspplmodes)

;;; Code:

;;-----------------------------------------------------------------------------

;; *************************
;; *                       *
;; * PROGRAMMING LANGUAGES *
;; *                       *
;; *************************
;; ,-------,
;; | C/C++ |
;; '-------'

;; (setq c-basic-offset 4)
(c-add-style "troy"
	     '("stroustrup"
	       (c-basic-offset . 4)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist
		(case-label . 1)
		(statement-case-intro . 3)
		(access-label . -3))))
(setq c-default-style '((c-mode . "troy")
			(java-mode . "java")
			(awk-mode . "awk")
			(c++-mode . "troy")
			(other . "gnu")))
(load "c++-fontlock-custom")
(load "c++-fontlock-fix-enum-class.el")
(load "troyp/c++-utils.el")

;; make _ part of words
(defun c-mode-include-underscore-in-words ()
  (interactive)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table))
(add-hook 'c-mode-common-hook 'c-mode-include-underscore-in-words)


;; EMACS-CLANG-COMPLETE-ASYNC

;; (add-to-list 'load-path "~/.emacs.d/emacs-clang-complete-async")
;; (require 'auto-complete-config)
;; (require 'auto-complete-clang-async)

;; (defun ac-cc-mode-setup ()
;;   (setq ac-clang-complete-executable "~/.emacs.d/emacs-clang-complete-async/clang-complete")
;;   (setq ac-sources '(ac-source-clang-async))
;;   (ac-clang-launch-completion-process)
;; )

;; (defun my-ac-config ()
;;   (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   ;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-cc-mode-setup)
;;   (global-auto-complete-mode t))

;; (my-ac-config)

(defun comment-use-line-comments ()
  (interactive)
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local comment-type "line")
  (message "Using line comments"))

(defun comment-use-block-comments ()
  (interactive)
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
  (setq-local comment-type "block")
  (message "Using block comments"))

(defun comment-toggle-line-block ()
  (interactive)
  (if (and (boundp 'comment-type)
           (string= comment-type "line"))
      (comment-use-block-comments)
    (comment-use-line-comments)))

;; -------------------------------------------------------------------------------
;; ,---,
;; | D |
;; '---'

(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))


;; -------------------------------------------------------------------------------
;; ,--------,
;; | Erlang |
;; '--------'

;; (setq load-path (cons (car (file-expand-wildcards "/usr/lib/erlang/lib/tools-*/emacs"))
;; 		      load-path))
;; (setq erlang-root-dir "/usr/lib/erlang")
;; (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
;; (require 'erlang-start)

;; (setq inferior-erlang-machine-options '("-sname" "emacs"))
;; (add-to-list 'load-path "~/.emacs.d/distel/elisp")
;; (require 'distel)
;; (distel-setup)


;; -------------------------------------------------------------------------------
;; ,--------,
;; | Factor |
;; '--------'

(defun run-fuel ()
  (interactive)
  (load-file "/opt/factor/misc/fuel/fu.el")
  (run-factor))
(add-hook 'factor-mode-hook (lambda () (variable-pitch-mode t)))


;; -------------------------------------------------------------------------------
;; ,---------,
;; | Haskell |
;; '---------'

(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'inf-haskell-mode)
(define-key haskell-mode-map (kbd "C-c .") 'hoogle-lookup)
(defun hoogle-lookup (s)
  (interactive "sHoogle: ")
  (message
   (shell-command-to-string
	(concat "/home/troy/.cabal/bin/hoogle " s))))



;; ;; Scion.
;; ;; ======
;; (add-to-list 'load-path "~/.emacs.d/plugins/scion-master/emacs")
;; (require 'scion)
;; (setq scion-program "~/.cabal/bin/scion-server")
;; (defun my-haskell-hook ()
;;   ;; Whenever we open a file in Haskell mode, also activate Scion
;;   (scion-mode 1)
;;   ;; Whenever a file is saved, immediately type check it and
;;   ;; highlight errors/warnings in the source.
;;   (scion-flycheck-on-save 1))
;; (add-hook 'haskell-mode-hook 'my-haskell-hook)
;; ;; Use ido-mode completion (matches anywhere, not just beginning)
;; ;; WARNING: This causes some versions of Emacs to fail so badly
;; ;; that Emacs needs to be restarted.
;; (setq scion-completing-read-function 'ido-completing-read)


;; -------------------------------------------------------------------------------
;; ,---,
;; | J |
;; '---'

(autoload 'j-mode "j-mode.el"  "Major mode for J." t)
(autoload 'j-shell "j-mode.el" "Run J from emacs." t)
(setq auto-mode-alist
      (cons '("\\.ij[rstp]" . j-mode) auto-mode-alist))
(setq j-path "/opt/j64-701/")
;; if don't need plotting/graphics...
(setq j-command "bin/jconsole")


;; -------------------------------------------------------------------------------
;; ,------------,
;; | Javascript |
;; '------------'

;; add js2-minor-mode to js-mode
(add-hook 'js-mode-hook
	  '(lambda ()
	     (js2-minor-mode)
	     (define-key js2-mode-map (kbd "C-c C-u") 'js2-cancel-error-face)
		 (setq indent-tabs-mode nil)))
(add-hook 'js2-minor-mode-hook 'ac-js2-mode)

;; default js mode: js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsm$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)
(defun js2-cancel-error-face ()
  (interactive)
  (face-remap-add-relative 'js2-error nil))


;; -------------------------------------------------------------------------------
;; ,----------,
;; | Markdown |
;; '----------'

;; ------------------------
;;   Polymode - RMarkdown
;; ------------------------
(require 'poly-markdown)
(defalias 'rmd-mode 'poly-markdown+r-mode)
;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))


;; -------------------------------------------------------------------------------
;; ,--------,
;; | Octave |
;; '--------'

(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
		(font-lock-mode 1))))


;; -------------------------------------------------------------------------------
;; ,----,
;; | Oz |
;; '----'

;; (or (getenv "OZHOME") (setenv "OZHOME" "/opt/mozart2"))
(or (getenv "OZHOME") (setenv "OZHOME" "/usr"))
(add-to-list 'load-path (concat (getenv "OZHOME") "/share/mozart/elisp"))
(require 'oz)
(add-to-list 'auto-mode-alist '("\\.oz\\'" . oz-mode))
(add-to-list 'auto-mode-alist '("\\.ozg\\'" . oz-gump-mode))
(autoload 'run-oz "oz" "" t)
(autoload 'oz-mode "oz" "" t)
(autoload 'oz-gump-mode "oz" "" t)
(autoload 'oz-new-buffer "oz" "" t)
(define-key oz-mode-map (kbd "C-c C-c") 'oz-feed-line)
(define-key oz-mode-map (kbd "C-c C-p") 'oz-feed-paragraph)
(define-key oz-mode-map (kbd "C-c C-r") 'oz-feed-region)
(define-key oz-mode-map (kbd "C-c C-b") 'oz-feed-buffer)
(define-key oz-mode-map (kbd "M-p") 'evil-scroll-line-up)
(define-key oz-mode-map (kbd "M-n") 'evil-scroll-line-down)


;; -------------------------------------------------------------------------------
;; ,------,
;; | Perl |
;; '------'

(add-to-list 'load-path "/home/troy/.emacs.d/Emacs-PDE-0.2.16/lisp/")
(load "pde-load")
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(defun perl-completion-on ()
  (require 'perl-completion)
  (perl-completion-mode t))
(add-hook 'cperl-mode-hook 'perl-completion-on)


;; -------------------------------------------------------------------------------
;; ,--------,
;; | Prolog |
;; '--------'

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist
      (append
       '(
		 ;; ("\\.pl$" . prolog-mode)
		 ("\\.m$" . mercury-mode)
		 ("\\.prolog$" . prolog-mode)
		 ("\\.pro$" . prolog-mode))
       auto-mode-alist))


;; -------------------------------------------------------------------------------
;; ,--------,
;; | Python |
;; '--------'

;; ----------------
;; emacs for python
;; ----------------
(defun epy-load () (interactive) (load-file "/home/troy/.emacs.d/emacs-for-python/epy-init.el"))
;(epy-django-snippets)
;(epy-setup-ipython)
;(global-hl-line-mode t) ;; To enable
;(set-face-background 'hl-line "black") ;; change with the color that you like
 ;(require 'highlight-indentation)
 ;(add-hook 'python-mode-hook 'highlight-indentation)

(add-to-list 'load-path "/home/troy/.emacs.d/python-mode.el/")
(setq py-install-directory "/home/troy/.emacs.d/python-mode.el/")
(require 'python-mode)
(setq py-shell-name "ipython")
(setq py-shell-name "/usr/bin/ipython")
(defun py-splith ()
  (interactive)
  (custom-set-variables
   '(py-split-windows-on-execute-function (quote split-window-horizontally))))
(defun py-splitv ()
  (interactive)
  (custom-set-variables
   '(py-split-windows-on-execute-function (quote split-window-vertically))))

;; Ein.
(require 'ein)
;; start server with "ipython notebook --pylab=inline"
;; open notebook with "M-x ein:notebooklist-open" or "M-x ein:notebooklist-new"

;; Jedi.
(defun my-jedi-mode ()
  ;; (global-set-key [C-tab] 'next-multiframe-window)
  ;; (global-set-key (kbd "C-`") 'jedi:complete)
  (jedi:setup)
  (define-key jedi-mode-map (kbd "C-`") 'jedi:complete)
  (define-key jedi-mode-map [C-tab] 'next-multiframe-window))
(add-hook 'python-mode-hook 'my-jedi-mode)
; (add-hook 'python-mode-hook 'jedi:ac-setup)  ; autocomplete only
(setq jedi:setup-keys t)  ; keybindings - must be set *before* jedi.el loaded
(setq jedi:complete-on-dot t)


;; -------------------------------------------------------------------------------
;; ,---,
;; | R |
;; '---'

;; -------
;;   ESS
;; -------
;; C-c C-e C-t: tags for directory
;; C-c C-d C-e: describe object at point
(require 'ess)
(setq ess-eval-visibly nil)
(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
;; (setq ansi-color-for-comint-mode 'filter)
;; (setq comint-prompt-read-only t)
;; (setq comint-scroll-to-bottom-on-input t)
;; (setq comint-scroll-to-bottom-on-output t)
;; (setq comint-move-point-for-output t)
(require 'r-utils)
(add-hook 'ess-mode-hook
	  '(lambda()
	     (local-set-key [(shift return)] 'ess-shift-enter)))
;; ess-autoyas: use yas for function arguments
(require 'r-autoyas)
(add-hook 'ess-mode-hook 'r-autoyas-ess-activate)
;; ess-auto-complete
(defalias 'ess-auto-complete-start 'ess-ac-start)
(setq ess-use-auto-complete 'script-only)

;; speedbar
(add-hook 'speedbar-mode-hook '(lambda () (speedbar-add-supported-extension ".R")))

;; RMarkdown (polymode): see Markdown.

;; -------------------------------------------------------------------------------
;; ,------,
;; | Sage |
;; '------'

;; (add-to-list 'load-path "/usr/lib/sagemath/local/share/emacs/site-lisp/sage-mode/")
;; (require 'sage "sage")
;; (setq sage-command "/usr/lib/sagemath/sage")


;; -------------------------------------------------------------------------------
;; ,-------,
;; | Scala |
;; '-------'

;; Scala-mode.
;; (add-to-list 'load-path "~/.emacs.d/scala-mode2-master")
(require 'scala-mode2)
(add-to-list 'exec-path "/opt/scala/bin/")

;; Ensime.
(add-to-list 'load-path "~/.emacs.d/ensime-master/src/main/elisp/")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Scala Keybindings.
(add-hook 'scala-mode-hook '(lambda ()
  (local-set-key (kbd "RET") '(lambda ()
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment)))
  (local-set-key (kbd "M-RET") 'join-line)
  (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)))

;; SBT.
(add-to-list 'exec-path "/opt/sbt/bin/")


;; -------------------------------------------------------------------------------
;; ,--------,
;; | Scheme |
;; '--------'

(add-hook 'scheme-mode-hook 'my-scheme-mode-hook)
(defun my-scheme-mode-hook ()
  (setq font-lock-defaults
	'((scheme-font-lock-keywords
	   scheme-font-lock-keywords-1
	   my-scheme-font-lock-keywords)
	  nil t (("+-*/.<>=!?$%_&~^:" . "w")) beginning-of-defun
	  (font-lock-mark-block-function . mark-defun)))
  (setq my-scheme-font-lock-keywords
	(append scheme-font-lock-keywords-2
		(eval-when-compile
		  (list
		   (regexp-opt '("compensate" "when") t)
		   ;;This must come before the errors specification, or
		   ;;"misc-error" will not be colored correctly.
		   (cons (regexp-opt '("wrong-type-arg" "misc-error"
				       "out-of-range" "system-error") t)
			 'font-lock-constant-face)
		   (cons (regexp-opt '("scm-error" "error"
				       "false-if-exception") t)
			 'font-lock-warning-face))))))
(defconst my-scheme-font-lock-keywords
  '()
  "Custom highlighting in Scheme modes.")


;; -------------------------------------------------------------------------------
;; ,-------,
;; | Shell |
;; '-------'

;; shell-script
(add-to-list 'auto-mode-alist '("\\.bash" . sh-mode))

;; defaults
(setq-default sh-indent-for-then 0)
(setq-default sh-indent-for-do 0)
(setq-default sh-indent-after-do '+)
(setq-default sh-indent-for-case-label '*)
(setq-default sh-indent-for-case-alt '+)
(setq-default sh-indent-comment t)

;; ANSI-TERM-MODE.
(setq term-bind-key-alist-defaults
      '(("C-c C-c" . term-interrupt-subjob)
	("C-p" . previous-line)
	("C-n" . next-line)
	("C-s" . isearch-forward)
	("C-r" . isearch-backward)
	("C-m" . term-send-raw)
	("M-f" . term-send-forward-word)
	("M-b" . term-send-backward-word)
	("M-o" . term-send-backspace)
	("M-p" . term-send-up)
	("M-n" . term-send-down)
	("M-M" . term-send-forward-kill-word)
	("M-N" . term-send-backward-kill-word)
	("M-r" . term-send-reverse-search-history)
	("M-," . term-send-input)
	("M-." . comint-dynamic-complete)))
(setq my-term-bind-key-alist
      '(("C-c C-j"   .  term-line-mode)
	("C-c C-k"   .  term-char-mode)
	;; ("M-DEL"     .  term-send-backward-kill-word)
	;; ("M-d"	.  term-send-forward-kill-word)
	;; ("<C-left>"	.  term-send-backward-word)
	;; ("<C-right>" .  term-send-forward-word)
	;; ("C-r"	.  term-send-reverse-search-history)
	;; ("M-p"	.  term-send-raw-meta)
	;; ("M-y"	.  term-send-raw-meta)
	;; ("C-y"	.  term-send-raw)
	))
(defun my-ansi-mode-hook ()
  (setq term-bind-key-alist
	(append term-bind-key-alist-defaults
		my-term-bind-key-alist)))
(add-hook 'term-mode-hook 'my-ansi-mode-hook)

(require 'multi-term)
(global-set-key (kbd "<f5>") 'multi-term)
(global-set-key (kbd "<C-next>") 'multi-term-next)
(global-set-key (kbd "<C-prior>") 'multi-term-prev)
(setq multi-term-program "/bin/bash")


;; -------------------------------------------------------------------------------
;; ,------,
;; | VimL |
;; '------'

;; ;; defined by Gilles in:
;; ;; http://stackoverflow.com/questions/4236808/syntax-highlight-a-vimrc-file-in-emacs
;; (define-generic-mode 'vimrc-generic-mode
;;   '()
;;   '()
;;   '(("^[\t ]*:?\\(!\\|ab\\|map\\|unmap\\)[^\r\n\"]*\"[^\r\n\"]*\\(\"[^\r\n\"]*\"[^\r\n\"]*\\)*$"
;;      (0 font-lock-warning-face))
;;     ("\\(^\\|[\t ]\\)\\(\".*\\)$"
;;      (2 font-lock-comment-face))
;;     ("\"\\([^\n\r\"\\]\\|\\.\\)*\""
;;      (0 font-lock-string-face)))
;;   '("/vimrc\\'" "\\.vim\\(rc\\)?\\'")
;;   '((lambda ()
;;       (modify-syntax-entry ?\" ".")))
;;   "Generic mode for Vim configuration files.")

(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?-?\\w*$" . vimrc-mode))
(add-to-list 'auto-mode-alist '("\\.penta$" . vimrc-mode))
(add-to-list 'auto-mode-alist '("\\.pentadactylrc-?\\w*$" . vimrc-mode))

;; -------------------------------------------------------------------------------
;; ,---------,
;; | Web Dev |
;; '---------'

(defun htmlentify (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (beginning-of-buffer)
      (replace-string "&" "&amp;")
      (beginning-of-buffer)
      (replace-string "<" "&lt;")
      (beginning-of-buffer)
      (replace-string ">" "&gt;"))))

;; -------------------------------------------------------------------------------

(provide 'tspplmodes)

;;; tspplmodes ends here
