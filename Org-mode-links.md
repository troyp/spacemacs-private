### Org mode links

Modify or remove Org mode links.  The unlinkify function was posted to the Org mode mailing list by John Kitchin and was modified following a suggestion from Nicolas Goaziou.

```lisp
(add-to-list 'load-path "path/to/org-link-edit/")
(require 'org-link-edit)
(defun jk/unlinkify ()
  "Replace an org-link with the description, or if this is absent, the path."
  (interactive)
  (let ((eop (org-element-context)))
    (when (eq 'link (car eop))
      (message "%s" eop)
      (let* ((start (org-element-property :begin eop))
             (end (org-element-property :end eop))
             (contents-begin (org-element-property :contents-begin eop))
             (contents-end (org-element-property :contents-end eop))
             (path (org-element-property :path eop))
             (desc (and contents-begin
                        contents-end
                        (buffer-substring contents-begin contents-end))))
        (setf (buffer-substring start end)
              (concat (or desc path)
                      (make-string (org-element-property :post-blank eop) ?\s)))))))

 (define-key org-mode-map (kbd "C-c )")
      (defhydra hydra-org-link-edit (:color red)
        "Org Link Edit"
        ("j" org-link-edit-forward-slurp "forward slurp")
        ("k" org-link-edit-forward-barf "forward barf")
        ("u" org-link-edit-backward-slurp "backward slurp")
        ("i" org-link-edit-backward-barf "backward barf")
        ("r" jk/unlinkify "remove link")
        ("q" nil "cancel" :color blue)))
```