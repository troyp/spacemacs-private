;; *****************************************
;; *                                       *
;; * mls.el -- Multi-line String Functions *
;; *                                       *
;; *****************************************

(defun mls-trim-line-ends (s)
  "Remove whitespace from the end of lines. Similar to `s-trim-right', but works on multi-line strings."
  (replace-regexp-in-string "[ \t]+\n" "\n" s))

(defun mls-split (s)
  "Split a multiline string into a list of single-line strings."
  (split-string s "\n"))
