```elisp
(defun my/name-of-buffers (n)
  "Return the names of the first N buffers from `buffer-list'."
  (let ((bns
         (delq nil
               (mapcar
                (lambda (b)
                  (unless (string-match "^ " (setq b (buffer-name b)))
                    b))
                (buffer-list)))))
    (subseq bns 1 (min (1+ n) (length bns)))))

;; Given ("a", "b", "c"), return "1. a, 2. b, 3. c".
(defun my/number-names (list)
  "Enumerate and concatenate LIST."
  (let ((i 0))
    (mapconcat
     (lambda (x)
       (format "%d. %s" (cl-incf i) x))
     list
     ", ")))

(defvar my/last-buffers nil)

(defun my/switch-to-buffer (arg)
  (interactive "p")
  (switch-to-buffer
   (nth (1- arg) my/last-buffers)))

(defun my/switch-to-buffer-other-window (arg)
  (interactive "p")
  (switch-to-buffer-other-window
   (nth (1- arg) my/last-buffers)))

(global-set-key
 "\C-o"
 (defhydra my/switch-to-buffer (:exit t
                                :body-pre (setq my/last-buffers
                                                (my/name-of-buffers 4)))
   "
_o_ther buffers: %s(my/number-names my/last-buffers)

"
   ("o" my/switch-to-buffer "this window")
   ("O" my/switch-to-buffer-other-window "other window")
   ("<escape>" nil)))
```
Pressing <kbd>C-o</kbd> activates the hydra with names of the first 4 most recently visited "other" buffers listed:

- A single <kbd>o</kbd> switches to the first buffer in that list, same as what <kbd>M-:</kbd>`(switch-to-buffer (other-buffer))` would do. In other words, you can quickly switch between two buffers back and forth with <kbd>C-o o</kbd>.

- Prefixed with a numeric prefix, say, <kbd>2 o</kbd>, selects the 2nd buffer from the list, <kbd>3 o</kbd> selects the 3rd, ... Negative prefixes and the numeric prefix 1 are same as no prefix - all select the first buffer.

- <kbd>O</kbd> does the same as <kbd>o</kbd> except that the buffer is switched in the "other window" - i.e. by `switch-to-buffer-other-window`.

In practice, this hydra usually can have more heads to call, for example, `ido-find-file`, `ido-switch-buffer`, etc.

If you don't need to visit buffers in the "other window", a variant of this hydra can be:
```elisp
(global-set-key
 "\C-o"
 (defhydra my/switch-to-buffer (:exit t
                                :body-pre (setq my/last-buffers
                                                (my/name-of-buffers 4)))
   "
other buffers: %s(my/number-names my/last-buffers)

"
   ("o" (my/switch-to-buffer 1))
   ("1" (my/switch-to-buffer 1))
   ("2" (my/switch-to-buffer 2))
   ("3" (my/switch-to-buffer 3))
   ("4" (my/switch-to-buffer 4))
   ("<escape>" nil)))
```
Then <kbd>C-o 1</kbd> selects the 1st buffer, <kbd>C-o 2</kbd> selects the 2nd, and so on, and still retain the quick <kbd>C-o o</kbd>.
