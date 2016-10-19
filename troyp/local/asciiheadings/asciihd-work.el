(require 's)

;; extract-rectangle
(defun asciihd-string-surround (s surroundstr &optional padding padchar)
  (let ((padstr (make-string (or padding 0) (or padchar ? ))))
    (concat surroundstr padstr s padstr surroundstr)))
