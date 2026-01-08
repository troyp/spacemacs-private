;; https://gist.github.com/tomfaulhaber/519635

(defun outline-copy-visible (keepp)
  "Create a copy of the visible part of the current buffer and add
it to the kill ring so it can be copied into other buffers or programs.
The copy is created in a temporary buffer and removed after use.
As a special case, if you have a prefix arg KEEPP, the temporary
buffer will not be removed but presented to you so that you can
continue to use it.
This function is derived from org-export-visible."
  (interactive "P")
  (let* ((file buffer-file-name)
	 (buffer (get-buffer-create "*Outline Yank Visible*"))
	 s e)
    (with-current-buffer buffer (erase-buffer))
    (save-excursion
      (setq s (goto-char (point-min)))
      (while (not (= (point) (point-max)))
	(goto-char (outline-find-invisible))
	(append-to-buffer buffer s (point))
	(setq s (goto-char (outline-find-visible))))
      (goto-char (point-min))
      (set-buffer buffer)
      (kill-new (buffer-substring (point-min) (point-max)))
    (if (not keepp)
	(kill-buffer buffer)
      (switch-to-buffer-other-window buffer)
      (goto-char (point-min))))))

(defun outline-find-visible ()
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
		(get-char-property s 'invisible)))
    s))

(defun outline-find-invisible ()
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
		(not (get-char-property s 'invisible))))
    s))
