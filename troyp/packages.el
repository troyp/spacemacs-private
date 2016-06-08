;;; packages.el --- troyp layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Troy Pracy <troy@oceanic>
;; URL: https://github.com/syl20bnr/spacemacs
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
;; added to `troyp-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `troyp/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `troyp/pre-init-PACKAGE' and/or
;;   `troyp/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst troyp-packages
  '(
    (tsputils            :location local)
    (asciiheadings       :location local)
    (evil-visual-replace :location local)
    (mode-ring           :location local)
    (tspevil             :location local)
    )
  "The list of Lisp packages required by the troyp layer.

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
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format"
  )

(defun troyp/init-asciiheadings       () (use-package asciiheadings       :init :after tsputils))
(defun troyp/init-evil-visual-replace () (use-package evil-visual-replace :init))
(defun troyp/init-mode-ring           () (use-package mode-ring           :init))
(defun troyp/init-tspevil             () (use-package tspevil             :init))
(defun troyp/init-tsputils            () (use-package tsputils            :init))

;;; packages.el ends here
