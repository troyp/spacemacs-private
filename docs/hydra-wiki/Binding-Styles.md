Binding style decides how you call a certain hydra's functions. There are two styles with their own advantages and disadvantages.  You can decide which one you prefer, and switch easily at any point.

## Style 1: don't mess with the prefix

This style does not limit you from binding other commands to the prefix:

```elisp
(defhydra hydra-zoom (global-map "C-c")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))
```

It results in the following generated code:

```elisp
(define-key global-map (kbd "C-c g") 'hydra-zoom/text-scale-increase)
(define-key global-map (kbd "C-c l") 'hydra-zoom/text-scale-decrease)
```

The advantage of this approach is that the <kbd>C-c</kbd> isn't taken over, and you can still use e.g.:

```elisp
(global-set-key (kbd "C-c o") 'occur)
```

The disadvantage is that you don't get the hydra hint when you press <kbd>C-c</kbd>, only after you press either <kbd>C-c g</kbd> or <kbd>C-c l</kbd>.

## Style 2: take over the prefix

This code will exchange the advantage and the disadvantage:

```elisp
(defhydra hydra-zoom ()
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(global-set-key (kbd "C-c") 'hydra-zoom/body)
```

You'll see the hint as soon as you press <kbd>C-c</kbd>. But you won't be able to use the <kbd>C-c</kbd> prefix for anything but this hydra. The <kbd>C-c</kbd> prefix is chosen here just to show how drastic the decision can be, since the use typically puts many commands on that prefix.  However, the following setting can be a nice approach:

```elisp
(global-set-key (kbd "C-c z") 'hydra-zoom/body)
```

Unlike <kbd>C-c</kbd>, <kbd>C-c z</kbd> in an unused prefix. It's OK to have this hydra take it over completely.

## Style 1 or 2?

The decision is up to you. Once you're aware of them, you can mix and match them as you see fit.
