
(require 's)
(require 'evil-nerd-commenter)

(require 'tspevil)


;; TODO: finish
(defun divider-heading-convert ()
  (interactive)
  (let ((comment? (evilnc--in-comment-p (point))))
    (when comment? (evilnc-comment-or-uncomment-lines))
    (evil-visual-char)
    (call-interactively #'evil-inner-line)
    ()
    ))
