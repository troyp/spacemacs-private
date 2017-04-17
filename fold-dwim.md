# Hydra for Fold “Do What I Mean”

This is a simple Hydra for [fold-dwim](https://github.com/emacsmirror/fold-dwim).

```elisp
(defhydra hydra-fold (:pre (hs-minor-mode 1))
  "fold"
  ("t" fold-dwim-toggle "toggle")
  ("h" fold-dwim-hide-all "hide-all")
  ("s" fold-dwim-show-all "show-all")
  ("q" nil "quit"))
```
