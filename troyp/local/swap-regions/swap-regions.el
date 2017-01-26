
(defvar swap-region-marker-beginning (make-marker))
(defvar swap-region-marker-end (make-marker))
(defvar swap-region-A nil)
(defvar swap-region-B nil)

(defun swap-region-mark (beg end)
  (interactive "r")
  (set-marker swap-region-marker-beginning (min beg end))
  (set-marker swap-region-marker-end (max beg end))
  (setf swap-region-A (buffer-substring beg end)))

(defun swap-region (beg end)
  (interactive "r")
  ;; copy region B, replace with region A
  (setf swap-region-B (buffer-substring beg end))
  (kill-region beg end)
  (insert swap-region-A)
  (setq swap-region-A nil)
  ;; replace region A with region B
  (save-excursion
    (with-current-buffer (marker-buffer swap-region-marker-beginning)
      (goto-char (marker-position swap-region-marker-beginning))
      (kill-region (marker-position swap-region-marker-beginning)
                     (marker-position swap-region-marker-end))
      (insert swap-region-B)
      (setq swap-region-B nil)
      )))

(defun swap-region-dispatch (beg end)
  (interactive "r")
  (if swap-region-A
      (swap-region beg end)
    (swap-region-mark beg end)))
