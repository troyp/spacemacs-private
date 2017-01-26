
(defvar swap-region-location nil)
(defvar swap-region-A "")
(defvar swap-region-B "")

(defun swap-region-mark (beg end)
  (interactive "r")
  (setq swap-region-location (list (current-buffer) (min beg end) (max beg end)))
  (setq swap-region-A (buffer-substring beg end)))

(defun swap-region (beg end)
  (interactive "r")
  ;; copy region B, replace with region A
  (setq swap-region-B (buffer-substring beg end))
  (kill-region beg end)
  (insert swap-region-A)
  (setq swap-region-A "")
  ;; replace region A with region B
  (destructuring-bind (bufA begA endA) swap-region-location
    (save-excursion
      (with-current-buffer bufA
        (goto-char begA)
        (kill-region begA endA)
        (insert swap-region-B)
        (setq swap-region-B "")
        ))))

(defun swap-region-dispatch (beg end)
  (interactive "r")
  (if (member swap-region-A '("" nil))
      (swap-region-mark beg end)
    (swap-region beg end)))
