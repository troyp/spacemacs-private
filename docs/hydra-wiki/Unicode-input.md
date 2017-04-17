Hydras are great for documenting key bindings that are seldom used. Unicode characters are a prime example: they are available in all modern emacsen as long as you have the right font to display them. The document string takes all guesswork out from selecting the right one by showing you the Unicode characters.

See [the story behind this hydra](http://heikkil.github.io/blog/2015/03/22/hydra-for-unicode-input-in-emacs/).

````cl
     (defun my/insert-unicode (unicode-name)
       "Same as C-x 8 enter UNICODE-NAME."
       (insert-char (cdr (assoc-string unicode-name (ucs-names)))))

     (global-set-key
      (kbd "C-x 9")
      (defhydra hydra-unicode (:hint nil)
        "
        Unicode  _e_ €  _s_ ZERO WIDTH SPACE
                 _f_ ♀  _o_ °   _m_ µ
                 _r_ ♂  _a_ →
        "
        ("e" (my/insert-unicode "EURO SIGN"))
        ("r" (my/insert-unicode "MALE SIGN"))
        ("f" (my/insert-unicode "FEMALE SIGN"))
        ("s" (my/insert-unicode "ZERO WIDTH SPACE"))
        ("o" (my/insert-unicode "DEGREE SIGN"))
        ("a" (my/insert-unicode "RIGHTWARDS ARROW"))
        ("m" (my/insert-unicode "MICRO SIGN"))))
````