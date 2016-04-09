;;; extensions.el --- troyp Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Troy Pracy <troy@oceanic>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Pre extensions are loaded *before* the packages

;; Post extensions are loaded *after* the packages
(setq spacemacs-post-extensions
  '(
    (use-package tsputils)
    (use-package tspevil)
    (use-package asciiheadings)
    (use-package mode-ring)
    ))

;; Initialize the extensions
