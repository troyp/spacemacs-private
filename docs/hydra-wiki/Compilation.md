With this hydra, you can press <kbd>C-x `</kbd> as usual to go to the next compilation error. Subsequently, you can use <kbd>h</kbd>, <kbd>j</kbd>, <kbd>k</kbd> or <kbd>l</kbd> respectively to navigate to the first, next, previous or last error.

```lisp
(defhydra hydra-next-error
    (global-map "C-x")
    "
Compilation errors:
_j_: next error        _h_: first error    _q_uit
_k_: previous error    _l_: last error
"
    ("`" next-error     nil)
    ("j" next-error     nil :bind nil)
    ("k" previous-error nil :bind nil)
    ("h" first-error    nil :bind nil)
    ("l" (condition-case err
             (while t
               (next-error))
           (user-error nil))
     nil :bind nil)
    ("q" nil            nil :color blue))
```