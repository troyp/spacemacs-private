`show-smartparens-mode` provides functionality similar to `show-paren-mode`, but works for all the user-defined pairs which are recognized by `sp-get-sexp`.

It is available as a globalized minor mode and is allowed in all modes where `smartparens-mode` is allowed, that is any mode which is not on the `sp-ignore-mode-list`.

You can turn it on with:

```scheme
(show-smartparens-global-mode t)

```

If you want to only turn it on in specific modes, use after-load hooks for these modes and call `(show-smartparens-mode t)`.

You can customize the delay after which smartparens will initiate the search by modifying the variable `sp-show-pair-delay`. Default value is 0.125 seconds.

Note that the pair-search is somewhat slower than `show-paren-mode`, which uses C libraries to do the parsing. If you have a very long buffer and a mis-matched pair, this can sometimes result in a noticeable lag (1 second or more) when you place cursor in front/behind this pair. For most of the normal editing business however, the difference is not noticeable.

# Customizing the looks

Smartparens uses the same default faces as `show-paren-mode`, but has them renamed so you can differentiate the two modes if you wish. The face names are:

    sp-show-pair-match-face
    sp-show-pair-mismatch-face

You can customize them using `M-x customize-group show-smartparens` or `M-x customize-face name-of-face`.
