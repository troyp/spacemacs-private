;;; packages.el --- perl5 layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Troy Pracy <troy@clockwork>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `perl5-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `perl5/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `perl5/pre-init-PACKAGE' and/or
;;   `perl5/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst perl5-packages
  '(
    cperl-mode
    ;; (pde :location (recipe :fetcher github :repo "wenbinye/emacs-pde"))
    (pde :location (recipe :fetcher file
                           :path "~/.emacs.d/private/layers/perl5/local/pde/lisp/pde.el"))
    plsense-direx
    )
  "The list of Lisp packages required by the perl5 layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun perl5/init-cperl-mode ()
  (use-package cperl-mode
    :init
    :config
    (add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
    (define-key cperl-mode-map (kbd "C-c C-c") 'mode-compile)
    (add-hook 'cperl-mode-hook 'perl5/add-chmod-hook)
    ))

(defun perl5/add-chmod-hook ()
  (add-hook 'after-save-hook 'perl5/chmodx nil t))

(defun perl5/chmodx ()
  (when (string-match "[[:space:]]*#!" (buffer-string))
    (shell-command (concat "chmod a+x '" buffer-file-name "'"))
    (remove-hook 'after-save-hook 'perl5/chmodx)
    (message "set executable bits")))

(defun perl5/init-pde ()
  (use-package pde
    :init
    (add-to-list 'load-path "~/.emacs.d/private/layers/perl5/local/pde/lisp/")
    :config
    ))

(defun perl5/init-plsense-direx ()
  (use-package plsense-direx
    :init
    :config
    (setq plsense-direx:open-explorer-key "C-x j")
    (setq plsense-direx:open-explorer-other-window-key "C-x J")
    (setq plsense-direx:open-referer-key "C-x M-j")
    (setq plsense-direx:open-referer-other-window-key "C-x C-M-J")
    (plsense-direx:config-default)
    (add-hook 'cperl-mode-hook 'plsense-server-start)
    (spacemacs/set-leader-keys-for-major-mode 'cperl-mode
        "?"  'plsense-display-help-buffer
        "/"  'plsense-popup-help
        )
    ))

;;; packages.el ends here
