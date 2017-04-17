1. [Random differences](#random-differences)
2. [Killing](#killing)
3. [Movement and navigation](#movement-and-navigation)
4. [Depth changing commands](#depth-changing-commands)
5. [Barfage and slurpage](#barfage-and-slurpage)
6. [Misc](#misc)

Smartparens and paredit share some functionality. This article is meant to explain some key differences between the two. It is of interest mostly to the users of paredit.

In each section about specific group of commands we will list the paredit function and the corresponding smartparens function together with the differences. I will omit the backward versions since they work in the exact analogous manner.

All the smartparens functions work with all the user-defined pairs too, not just the built-in `()`, `[]`, `{}` and `""`.

Only the commands bound by default in paredit are listed. There are some other commands not bound that also have SP counterparts. You can find the complete list of SP commands in [[working with expressions]]. If I didn't miss anything, all the unlisted paredit functions are replicated and enhanced much in the same spirit (accept numeric arguments, special arguments, type objects etc.)

<a name="random-differences" />
# Random differences

By default smartparens is much less strict about the "balancedness" of the buffer and it usually allows you to delete whatever you please. However, there are settings (like `smartparens-strict-mode`) you can use to have it behave more like paredit.

* In smartparens, by default when you try to delete a delimiter it will be deleted, not skipped over like with paredit. You can change this behaviour by binding `sp-delete-char`, `sp-kill-symbol` and their backward variants to whatever keys you normally use for this purpose. Since revision 446 you can enable minor mode `smartparens-strict-mode` to automatically remap these commands (`SP` in the modeline changes to `SP/s` if strict mode is on). *Note: to fall back to the regular behaviour SP uses C-0 (zero) prefix, not C-u prefix like paredit!*
* SP doesn't bind `)` key to the "jump out of sexp" command, instead it just inserts the `)` into the buffer. Same for `]`, `}` etc. You can bind `sp-up-sexp` there if you want. Also read the description of this function for more info.  You can also customize the option `sp-autoskip-closing-pair` to force SP to jump out of the sexp if you insert the closing delimiter (this is enabled by default in strict mode).
* The `C-k` command that kills the line really kills the line in SP. In paredit it tries to kill the next balanced expression or kill the line in a way that won't break the balance. If you want, you can bind `sp-kill-sexp` to `C-k`. Personally, I like to have both versions and I bind `sp-kill-sexp` to `C-M-k`. Check the docs for `sp-kill-sexp`, it has *many* modes of operation.  See also next section.  In strict mode, SP binds `sp-kill-hybrid-sexp` to `C-k` which behaves much like the paredit kill command.

Smartparens recognizes the pairs in comments and strings as well. If the point is outside a string or comment, these are ignored and skipped over. However, if point is inside string or comment the expressions there are recognized normally.

<a name="killing" />
# Killing

This was mostly mentioned in the section above, but for completeness here's the list:

    paredit-forward-delete   =   sp-delete-char

Work basically the same way, skipping over the delimiter to keep the structure balanced. If the sexp is empty, kill both ends. SP functions accept numeric prefix argument.

    paredit-kill   =   sp-kill-sexp

`paredit-kill` operates on "line" basis, or always tries to kill as much as possible. This is roughly equivalent to calling `sp-kill-sexp` with prefix `C-u`. By default, `sp-kill-sexp` only kills the next balanced expression after point. `sp-kill-sexp` also accepts numeric prefix arguments.

The function `sp-kill-hybrid-sexp` is a reimplementation of `paredit-kill`, so you can safely use that instead of `kill-line`. If `smartparens-strict-mode` is on, this is remapped automatically. Note however, that it also does other things (see the customization options)

    paredit-forward-kill-word   =   sp-kill-word

There is also variant `sp-kill-symbol` that kills the entire symbol, not just the next word.

Smartparens also implements `sp-copy-sexp` that does the same thing as `sp-kill-sexp` but without removing text from the buffer (that is, only copy to kill-ring).

<a name="movement-and-navigation" />
# Movement and navigation

    paredit-forward   =   sp-forward-sexp

Moves forward one sexp. Both functions call "up list" if at the end of the current list. SP functions accept numeric prefix.

    paredit-forward-down   =   sp-down-sexp

`paredit-forward-down` can descend one level down in hierarchy, correctly handling strings.  Smartparens currently doesn't descend into strings unless `sp-navigate-consider-stringlike-sexp` is enabled in current major mode.  Both paredit and SP functions can accept numeric argument to descend that many times.  Smartparens accept additional raw prefix arguments `C-u` and `C-u C-u`, see the description.  Paredit function throws an error if there is no expression to which it is possible to descend.  Smartparens in this case jumps to the front of the current list, which is reasonable.  After all, descending to "current" list means going to the front.

    paredit-forward-up   =   sp-up-sexp

`paredit-forward-up` is symmetrical to `paredit-forward-up`. `sp-up-sexp` can also re-indent the expression automatically (like paredit `)`) and close unbalanced expressions. Both can accept numeric prefix argument.

Functions without paredit counterpart:

    sp-next-sexp (&optional arg)
    sp-beginning-of-sexp (&optional arg)

See [[navigation functions|Working with expressions]].


<a name="depth-changing-commands" />
# Depth changing commands

    paredit-wrap-round

Wraps the next expression in additional pair of `()`. Smartparens doesn't have corresponding interactive functions. You can instead enable wrapping for *any* pair you define using the `:wrap` argument in `sp-local-pair` definition, see [Adding pairs](https://github.com/Fuco1/smartparens/wiki/Pair-management#wiki-adding-pairs).

There is also a non-interactive helper function you can use in your own code, or you can turn it into an interactive function, for example:

    (lambda (&optional arg)
      (interactive \"P\")
      (sp-wrap-with-pair "["))

Other depth-changing functions:

    paredit-splice-sexp                  = sp-splice-sexp
    paredit-splice-sexp-killing-backward = sp-splice-sexp-killing-backward
    paredit-splice-sexp-killing-forward  = sp-splice-sexp-killing-forward
    paredit-raise-sexp                   = sp-raise-sexp

These functions work in principle the same, but the way the prefix arguments are handled is different. You can read the built-in docs to see the difference. `sp-raise-sexp` also provides some new functionality.

<a name="barfage-and-slurpage" />
# Barfage and slurpage

    paredit-forward-slurp-sexp   =   sp-forward-slurp-sexp

The basic function is the same. SP version can also accept numeric prefix argument to slurp that many items. It also accepts `C-u` prefix to slurp "all the way" to the end of enclosing list. If slurping from string, paredit escapes the string to-be-added, smartparens instead joins the strings together (much more common operation).

    paredit-forward-barf-sexp   =   sp-forward-barf-sexp

The basic function is the same. SP version can also accept numeric prefix or `C-u` to barf "all the way", that is everything from point forward. Can also barf out of strings.

Smartparens also implements useful variants of these:

    sp-absorb-sexp (&optional arg)
    sp-emit-sexp (&optional arg)
    sp-extract-before-sexp (&optional arg)
    sp-extract-after-sexp (&optional arg)

See [[working with expressions]].

<a name="misc" />
# Misc

    paredit-split-sexp   =   sp-split-sexp

Same

    paredit-join-sexps   =   sp-join-sexp

In basic variant works the same. SP version can accept numeric prefix to join that many expressions (if they are all of the same type).

There's also:

    sp-indent-defun
    sp-newline
    sp-comment

that somewhat mirror similar paredit functions.  Read the built-in documentation for more info.
