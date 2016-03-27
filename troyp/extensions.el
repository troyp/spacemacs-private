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
(setq python-pre-extensions '(
                              tsputils
                              tspevil
                              asciiheadings
                              mode-ring
                              ))

;; Post extensions are loaded *after* the packages
(setq python-post-extensions
  '(
    (use-package tsputils)
    (use-package tspevil)
    (use-package asciiheadings)
    (use-package mode-ring)
    ))

;; Initialize the extensions
