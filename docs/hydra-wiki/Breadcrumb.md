# Bookmarks using breadcrumb

This hydra exposes some useful [breadcrumb](https://github.com/pheaver/breadcrumb) commands. Breadcrumb is a proper bookmark implementation for emacs with file local and global persistent bookmarks and the ability to quickly cycle through them.

```elisp
(defhydra hydra-breadcrumb
  (:exit t)
  "
Breadcrumb bookmarks:
  _<up>_:   prev   _S-<up>_:   local prev
  _<down>_: next   _S-<down>_: local next
  _s_: set  _c_: clear  _l_: list  _q_: quit
"
  ("<down>" bc-next nil :exit nil)
  ("<up>" bc-previous nil :exit nil)
  ("S-<down>" bc-local-next nil :exit nil)
  ("S-<up>" bc-local-previous nil :exit nil)
  ("l" bc-list nil)
  ("s" bc-set nil)
  ("c" bc-clear nil)
  ("q" nil nil))
```
