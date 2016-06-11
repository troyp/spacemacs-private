
(defmacro build-string-with-current-context (&rest body)
  "Evaluate BODY in the context of the current buffer (up to point) and return a string result, without altering the current buffer. The result consists of all text added after point.
See also `with-temp-buffer', `with-temp-file'"
  `(let ((pos      (point))
         (contents (buffer-string)))
     (with-temp-buffer
       (insert contents)
       (delete-region pos (buffer-end 1))
       ,@body
       (buffer-substring-no-properties pos (buffer-end 1)))))

(defmacro with-current-region (&rest body)
  "Evaluate BODY in a temporary buffer containing the current region."
  `(let* ((pt       (point))
          (mk       (mark t))
          (contents (buffer-substring pt mk)))
     (with-temp-buffer
       (insert contents)
       (goto-char 0)
       (set-mark (- mk pt))
       ,@body)))

(defmacro with-current-region-and-position (&rest body)
  "Evaluate BODY in the context of the current buffer, with everything outside the region replaced by spaces."
  `(let ((pt       (point))
         (mk       (mark))
         (contents (buffer-string)))
     (with-temp-buffer
       (insert contents)
       (replace-regexp "." " " nil (buffer-end -1) (- pt 1))
       (replace-regexp "." " " nil (+ mk 1) (buffer-end 1))
       (goto-char pt)
       (set-mark mk)
       ,@body)))
