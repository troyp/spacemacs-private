When you type in a pair that smartparens [[auto-complete|Permissions]], it keeps the information about this pair in a special structure called overlay. These overlays can also accept text properties, which means they can be highlighted to inform the user that something "special" is going on. For example, while the overlay is active, the closing pair is automatically skipped if it is typed in (so as not to end up with `(word)|)`).

Similar technique is used for the partial wrapping overlays and tag insertion mode (used in [[wrapping with tags|wrapping]]).

Smartparens uses its own faces (something similar to "css classes") to manage the colors and text properties. These are:

```scheme
(defface sp-pair-overlay-face
  '((t (:inherit highlight)))
  "The face used to highlight pair overlays."
  :group 'smartparens)

(defface sp-wrap-overlay-face
  '((t (:inherit sp-pair-overlay-face)))
  "The face used to highlight wrap overlays."
  :group 'smartparens)

(defface sp-wrap-tag-overlay-face
  '((t (:inherit sp-pair-overlay-face)))
  "The face used to highlight wrap tag overlays."
  :group 'smartparens)
```

You can customize any of them using `M-x customize-face name-of-face` facility.

If you do not wish to have any highlightning at all, you can customize variables:

    sp-highlight-pair-overlay
    sp-highlight-wrap-overlay
    sp-highlight-wrap-tag-overlay

and set them to nil (use `M-x customize-variable name-of-variable`). In that case, smartparens will not apply any face or text properties on the overlays, and will use them only for internal tracking purposes.

Another UI related feature is the [[show smartparens mode]] which highlights the "active" pair if the point is near the opening or closing delimiter.

# Show the enclosing pair after a command

You can add commands to a customize variable `sp-show-enclosing-pair-commands`. Smartparens will then automatically highlight the enclosing pair after any command on this list is executed. After the next command is executed, the highlightning will automatically disappear.

This is by default turned on for the [[slurp and barf|Working with expressions]] commands and a special command `sp-show-enclosing-pair`. This command does nothing, it will only trigger the highlightning.

# Highlight the expression returned by next command

The special prefix command `sp-highlight-current-sexp` will prompt you for the next command (which has to be bound to a key) using `read-key-sequence`. This command will then get executed and the returned expression -- that is the expression "worked on" -- will be highlighted. This command will not move the point. This command is mostly only useful for presentation purposes and/or to quickly check what the next command would do without moving the point.
