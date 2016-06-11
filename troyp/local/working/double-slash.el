
(defun //comment-string (s)
  (a-with-current-context
   ((let ((beg (point)))
      (insert s)
      (let ((end (point)))
        (indent-region beg end)
        (comment-region beg end))))))

