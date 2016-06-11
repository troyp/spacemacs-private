(defun a-trim-line-ends (s)
  "Remove whitespace from the end of lines. Similar to `s-trim-right', but works on multi-line strings."
  (replace-regexp-in-string "[ \t]+\n" "\n" s))

