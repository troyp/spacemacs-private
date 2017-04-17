1. [Arguments and return values](#arguments-and-return-values)
2. [Special prefix commands](#special-prefix-commands)
3. [Decorators](#decorators)
    1. [Restrict functions to specific pairs](#restrict-functions-to-specific-pairs)
    2. [Restrict functions to specific objects](#restrict-functions-to-specific-objects)
4. [Navigation functions](#navigation-functions)
5. [Manipulation functions](#manipulation-functions)

Smartparens provides multitude of functions to navigate and manipulate balanced expressions (s-expressions, sexps).

In the function lists, after each function in comment is the "recommended" binding, however, no function is bound by default. You should put these into `smartparens-mode-map` so they would only be enabled in `smartparens-mode`. You can use:

```scheme
(define-key smartparens-mode-map (kbd "your-key") 'function)
```

to do the local binding. Note that this has to occur *after* `smartparens-mode` is loaded, otherwise the `smartparens-mode-map` variable will be void. See the [[example configuration]] for the example of working code of how to set the bindings.

You can use the function `sp-use-paredit-bindings` to setup the key bindings to smartparens functions with key bindings of corresponding paredit functions. However, not all functions will get a key binding so if you wish to add the functions from smartparens that are *not* in paredit you have to bind them manually.

You can use the function `sp-use-smartparens-bindings` to setup the key bindings to smartparens functions to the same values as seen in [[example configuration]] (or after each function on the lists below, in the "comment").

From commit 280 you can also use the customize options `sp-base-key-bindings` and `sp-override-key-bindings` to set up keybindings for smartparens.

Each function where it makes sense comes with forward and backward version, where the backward version does the same thing as forward but in reverse direction. Therefore, if you only bind the forward version you can still access the backward version using `negative-prefix`, by default bound to `C--`, `M--` and `C-M--`. Therefore, if you have `sp-kill-sexp` on `C-M-k` then using `C-M-- C-M-k` will behave the same as calling `sp-backward-kill-sexp`.

These functions mostly mirror names of the built-in emacs functions and work in roughly same way, as would be expected by most users. However, if you are experiencing "weird" or unexpected behaviour, check the built-in description with `C-h f name-of-function`, where the behaviour and accepted arguments are explained in detail.

You can bring out a **list of all commands** (interactive functions) by calling `M-x sp-cheat-sheet`. This command will show the commands and example usage, without printing full documentation for each. If you wish to get the list with a full documentation and examples, use the prefix `C-u` before executing the command, that is `C-u M-x sp-cheat-sheet`.

For additional tips on how to use these functions in concord, read [[tips and tricks]].

<a name="arguments-and-return-values" />
# Arguments and return values

Most of the provided functions can accept a numeric prefix argument in which case they do the thing that many times or instead of next expression they operate on arg-th expression. The forward versions also accept negative argument, in which case they behave just as the backward versions. In fact, backward versions just call forward ones with negative argument. This means you can also call the backward versions with negative argument, in which case they behave as forward versions. This is useful if you have bound only one of them to a key, then you can use `C--` prefix to turn it into "other direction" version.

Some functions accept raw prefix argument `C-u`. This **always** act as "infinity" argument, in a sense that they repeat the thing as much as possible (but within reason). This usually means "until the end of the enclosing list" (instead of unreasonable "until end of file") for functions that can operate inside list (`sp-kill-sexp`, `sp-select-next-thing`, `sp-forward-barf-sexp`, ...). For other functions, like `sp-down-sexp` it means "move down to the deepst possible expression", which is the same as calling it with numeric argument "1000" (hence the "infinity" semantics).

Some functions accept raw prefix argument `C-u C-u`. This makes the function operate on the *enclosing list*. Therefore `C-u C-u sp-kill-sexp` will kill the list enclosing point (same as doing `sp-up-list sp-backward-kill-sexp`).

To see if a function accepts raw prefix arguments, you can read its built-in documentation using `C-h f name-of-function`.

When it makes sense, these functions return information about the expression on which they operated most recently. The return value is the same as that of `sp-get-sexp`, that is a plist with keywords `:beg :end :op :cl :prefix` for beginning of the expression, end of expression, opening and closing delimiter and expression prefix (like quotes or splice (`,@`) prefixes in lisp). **However, you should never access this structure directly, as it is subject to change.** Instead, use the `sp-get` macro which can also handle queries for other properties. For a complete description, read the built-in documentation (`C-h f sp-get RET`).

For example, `(sp-next-sexp 2)` would return the information about 2nd next expression. This, in combination with `(save-excursion)` macro can be used to quickly query for information about sexps in your own functions.

> **Legacy info, before commit 161**

> Before the redesign of `sp-get-sexp` family of functions, the return format was a simple 4-tuple `(beg-of-expr end-of-expr opening-pair closing-pair)`. If you use this in your own code, you should adapt it to the new version using `sp-get` macro to access all the sexp properties.

<a name="special-prefix-commands" />
# Special prefix commands

In addition to the built-in emacs `universal-argument`, `digit-argument` and `negative-argument` (commonly bound to `C-u`, `C-<number>` and `C--`), smartparens provide additional prefix arguments of its own. These are used to further modify the behaviour of the smartparens commands. Currently, these prefix commands are available:

```scheme
sp-prefix-tag-object
sp-prefix-pair-object
sp-prefix-symbol-object
sp-prefix-save-excursion
```

If you wish to pass the built-in emacs universal argument to the command, you have to supply this to the smartparens' prefix command instead. For example, having `sp-prefix-tag-object` bound to `C-c t`, you would execute `C-2 C-c t C-M-f` to give `C-M-f` the numeric prefix argument 2 and a special tag object argument.

Here's a quick summary of what these commands do:

* `sp-prefix-tag-object` - The next command will operate on the next SGML tag, ignoring things of any other type.
* `sp-prefix-pair-object` - The next command will operate on the paired object (as defined by pairs on `sp-pair-list`), ignoring things of any other type.
* `sp-prefix-symbol-object` - The next command will operate on symbols (as defined by the major-mode's syntax table), ignoring any possible structure such as pairs, SGML tags, strings etc.
* `sp-prefix-save-excursion` - Execute the command while keeping the point fixed. This is useful to modify commands that move the point as their default behaviour, for example `sp-extract-before-sexp`.

See [tips and tricks](https://github.com/Fuco1/smartparens/wiki/Tips-and-tricks#wiki-use-the-type-prefixes) for some concrete example usage.

<a name="decorators" />
# Decorators

Sometimes it is useful to restrict the navigation and manipulation functions (listed below) to specific pairs or object types. For example, in a `C++` file it is very useful to have a version of `sp-down-sexp` that only operates on the `{ }` pair, so you can quickly navigate the code blocks. Imagine the following example (taken from [issue #209](https://github.com/Fuco1/smartparens/issues/209))

```c
int
main(int argc, char *argv[])
{
  Kinetic::start(argc,argv);

  if( argc > 1 )
  {
    for( int i=1; i<argc; i++ )
    {
      line=argv[i];
      if( line=="noplot" || line=="nopause" || line=="abortOnEnd" ) continue;
      else
      {
         const char * dot = strrchr((const char*) line, '.');
      }
    }
  }
}
```

If the point is before the word `Kinetic` on line 4 and we want to descend into the body of the if statement, we would need to call `sp-down-sexp` and `sp-up-sexp` three times (because it would first descend into `()` pairs), or use `sp-forward-sexp` a couple times to get before the `{` delimiter.

Smartparens provides functions that can "wrap" the regular functions and let them operate only on specific subset of pairs. It follows the [Decorator pattern](http://en.wikipedia.org/wiki/Decorator_pattern), so the "decorators" are easily composable for cumulative effect.

We provide two "flavours" of each function, one to be used from elisp and one that generates interactive lambdas that can be readily bound to a keybinding.

In the following sections we explain each of these decorators.

To see more examples and for a related discussion, you can also read the [issue #209](https://github.com/Fuco1/smartparens/issues/209) in the bug tracker.

<a name="restrict-functions-to-specific-pairs" />
## Restrict functions to specific pairs

The function `sp-restrict-to-pairs` (and its interactive sibling `sp-restrict-to-pairs-interactive`) restrict a function to specific subset of defined pairs.

We will explain the working on examples, you can read the documentation inside emacs by invoking `C-h f sp-restrict-to-pairs RET`.

To call `sp-down-sexp` from elisp restricted to `{` pair, we can do:

```scheme
(defun sp-pair-curly-down-sexp (&optional arg)
  (interactive "P")
  (sp-restrict-to-pairs "{" 'sp-down-sexp))
```

To restrict it to more than one pair, we can supply a list of pairs: `(sp-restrict-to-pairs '("(" "{") 'sp-down-sexp)`.

Often it is needless to give a name to this restricted function, and so we provide a function that will generate a lambda you can simply bind to a key and forget about it. To use the same restriction on `sp-down-sexp` as above bound to key `M-{` we can do:

```scheme
(define-key c++-mode-map (kbd "M-{") (sp-restrict-to-pairs-interactive "{" 'sp-down-sexp))
```

Then calling `M-{` would descend one level deeper in the `{ }` hierarchy.

This function support multiple pairs restrictions as well.

<a name="restrict-functions-to-specific-objects" />
## Restrict functions to specific objects

We can restrict functions to specific object types (or more generally, to functions with a [prefix](#special-prefix-commands) applied automatically). To do this, we use the function `sp-restrict-to-object`. Its first argument is the name of the prefix (therefore it must be a quoted symbol!) and the second argument the name of the function.

For example, to restrict the `sp-forward-sexp` function to only operate on pairs (that is, not symbols or tags):

```scheme
(defun sp-pair-forward-sexp (&optional arg)
  (interactive "P")
  (sp-restrict-to-object 'sp-prefix-pair-object 'sp-forward-sexp))
```

To create an anonymous function and bind it to a key:

```scheme
(define-key c++-mode-map (kbd "M->") (sp-restrict-to-object-interactive 'sp-prefix-pair-object 'sp-forward-sexp))
```

As explained above, the restriction functions can be composed. To restrict a function only to a specific pair object (for example `{ }`), we can do (using the interactive example):

```scheme
(define-key c++-mode-map (kbd "C-M->") (sp-restrict-to-object-interactive
                                        'sp-prefix-pair-object
                                        (sp-restrict-to-pairs-interactive "{" 'sp-forward-sexp)))
```

This function will only operate on `{ }` pair and no other pair or object type (in particular it will skip over symbols or strings).

<a name="navigation-functions" />
# Navigation functions

List of navigation functions:

```scheme
sp-forward-sexp (&optional arg)                 ;; C-M-f
sp-backward-sexp (&optional arg)                ;; C-M-b
sp-down-sexp (&optional arg)                    ;; C-M-d
sp-backward-down-sexp (&optional arg)           ;; C-M-a
sp-up-sexp (&optional arg)                      ;; C-M-e
sp-backward-up-sexp (&optional arg)             ;; C-M-u
sp-next-sexp (&optional arg)                    ;; C-M-n
sp-previous-sexp (&optional arg)                ;; C-M-p
sp-beginning-of-sexp (&optional arg)            ;; C-S-d
sp-end-of-sexp (&optional arg)                  ;; C-S-a
sp-beginning-of-next-sexp (&optional arg)       ;; none
sp-beginning-of-previous-sexp (&optional arg)   ;; none
sp-end-of-next-sexp (&optional arg)             ;; none
sp-end-of-previous-sexp (&optional arg)         ;; none
```

These functions work pretty much exactly the same as the emacs-built in versions without `sp-` prefix, but operate on all user defined strictly balanced expressions. Strictly balanced means that `|( [ ) ]` will jump to `( [ |) ]`, not `( [ ) |]` as the default `forward-sexp` would.

These functions never signal the "Unbalanced parentheses" scan error and by default jump to the beginning or end of next/previous sexp, which is reasonable behaviour. If there is some special behaviour, it is documented.

If you want to also operate on symbols that are not wrapped, such as `(defun >name-of-fun< (arg) nil)` (leq/geq mark the symbol boundary), set `sp-navigate-consider-symbols` to `t`. Emacs built-in functions `forward-sexp` and `backward-sexp` recognize these as "expressions". If you set this option to `t`, all functions where it makes sense (that is, not unwrapping functions etc.) will consider symbols as balanced expressions. *Strings* enclosed with "" are also considerd as being one symbol. **This option is turned on by default.**

Note: the above option will be removed in near future. You can use the special prefix commands to navigate only on paired expressions.

> Lastly, the navigation with expressions where opening and closing pair is the same is troublesome, as it is impossible to detect the beginning and end without maintaining a count in the whole buffer (where odd occurences would be opening pairs and even occurences closing pairs). **Therefore, at the moment, these are not recognized as balanced expressions**. If you have an idea for a good heuristic or a method how to implement this, please file an issue with the suggestion.

From commit 297, the pairs with same opening and closing delimiter are supported. There are still some problems in modes where the delimiters serve multiple purposes, like `*` in markdown (unary list item, binary emphatic text). Use the variable `sp-navigate-consider-stringlike-sexp` to set the major modes where these should be supported. `latex-mode` is added by default. If you find any inconsistencies or general weirdness in major mode you use, please file an issue on our GitHub project page. Smartparens comes with tons of hooks and customizations that can fix most of the minor issues easily (and also most of the major issues after a bit of work :).

Here's a quick summary for each navigation function. For the complete info consult the built-in documentation.

* `sp-forward-sexp` - Jump *after* the next balanced expression. If inside one and there is no forward exp., jump after its closing pair.
* `sp-backward-sexp` - Jump *before* the previous balanced expression. If inside one and there is no previous exp., jump before its opening pair.
* `sp-down-sexp` - Jump *after* the opening pair of next balanced expression. This effectively descends one level down in the "expression hierarchy". If there is no expression to descend to, jump *after* current expression's opening pair. This can be used to quickly navigate to the beginning of current balanced expression.
* `sp-backward-down-sexp` - Jump *before* the closing pair of previous balanced expression. If there is no expr. to descend to, jump *before* current expression's closing pair.
* `sp-up-sexp` - Jump up one level from the current balanced expression. This means skipping all the enclosed expressions within *this* and then jumping *after* the closing pair. For example `(if (= a b) | (some call) (some other call))` -> `(if ...)|`.
* `sp-backward-up-sexp` - Jump up backwards one level from the current balanced expressions. This means skipping all the enclosed expressions within *this* backwards and then jumping *before* the opening pair.
* `sp-next-sexp` - Jump to the *beginning* of following balanced expression. If there is no following expression on the current level, jump one level up backward, effectively doing `sp-backward-up-sexp`.
* `sp-previous-sexp` - Jump to the *end* of the previous balanced expression. If there is no previous expression on the current level, jupm one level up forward, effectively doing `sp-up-sexp`.
* `sp-beginning-of-sexp` - Jump to the beginning of current sexp, that is after the opening delimiter.
* `sp-end-of-sexp` - Jump to the end of current sexp, that is before the closing delimiter.
* `sp-beginning-of-next-sexp` - Jump to the beginning of the next sexp on the same depth.
* `sp-beginning-of-previous-sexp` - Jump to the beginning of the previous sexp on the same depth.
* `sp-end-of-next-sexp` - Jump to the end of the next sexp on the same depth.
* `sp-end-of-previous-sexp` - Jump to the end of the previous sexp on the same depth.

<a name="manipulation-functions" />
# Manipulation functions

Some functions, especially slurp/barf functions are inspired by [paredit](http://emacswiki.org/emacs/ParEdit) package and work roughly the same. However, they can accept *optional prefix arguments* to modify their behaviour.

List of manipulation functions:

```scheme
sp-kill-sexp (&optional arg)                        ;; C-M-k
sp-backward-kill-sexp (&optional arg)               ;; C-- C-M-k

sp-copy-sexp (&optional arg)                        ;; C-M-w
sp-backward-copy-sexp (&optional arg)               ;; C-- C-M-w

sp-unwrap-sexp (&optional arg)                      ;; M-<delete>
sp-backward-unwrap-sexp (&optional arg)             ;; M-<backspace>

sp-transpose-sexp                                   ;; C-M-t

sp-splice-sexp (&optional arg)                      ;; M-D
sp-splice-sexp-killing-forward (&optional arg)      ;; C-M-<delete>
sp-splice-sexp-killing-backward (&optional arg)     ;; C-M-<backspace>
sp-splice-sexp-killing-around (&optional arg)       ;; C-S-<backspace>

sp-convolute-sexp (&optional arg)                   ;; none
sp-absorb-sexp (&optional arg)                      ;; none
sp-emit-sexp (&optional arg)                        ;; none
sp-extract-before-sexp (&optional arg)              ;; none
sp-extract-after-sexp (&optional arg)               ;; none

sp-split-sexp (arg)                                 ;; none
sp-join-sexp (&optional arg)                        ;; none

sp-rewrap-sexp (&optional arg)                      ;; none
sp-swap-enclosing-sexp (&optional arg)              ;; none

sp-forward-slurp-sexp (&optional arg)               ;; C-<right>
sp-forward-barf-sexp (&optional arg)                ;; C-<left>
sp-backward-slurp-sexp (&optional arg)              ;; C-M-<left>
sp-backward-barf-sexp (&optional arg)               ;; C-M-<right>

sp-add-to-next-sexp (&optional arg)                 ;; none
sp-add-to-previous-sexp (&optional arg)             ;; none

sp-select-next-thing (&optional arg)                ;; C-M-]
sp-select-previous-thing (&optional arg)            ;; C-[

sp-select-next-thing-exchange (&optional arg)       ;; C-]
sp-select-previous-thing-exchange (&optional arg)   ;; C-- C-]
```

Here's a quick summary for each manipulation function. For the complete info consult the built-in documentation.

* `sp-kill-sexp` - Kill the *next* balanced expression. If point is inside one and there's no following expression, kill the enclosing expression instead.
* `sp-backward-kill-sexp` - Kill the *previous* balanced expression.
* `sp-copy-sexp` - Copy the *next* balanced expression in the kill ring without actually killing it. Prefix arguments work like for `sp-kill-sexp`.
* `sp-backward-copy-sexp` - Copy the *previous* balanced expression.
* `sp-unwrap-sexp` - Remove the wrapping pair from the *following* expression. Following expression is one returned by `sp-forward-sexp`.
* `sp-backward-unwrap-sexp` - Remove the wrapping pair from the *previous* expression. Previous expression is one returned by `sp-backward-sexp`.
* `sp-transpose-sexp` - Transpose the adjacent balanced expressions.
* `sp-splice-sexp` - Remove the wrapping pair from *this* expression. With arg, do this on Nth enclosing expression as if first navigated with `sp-up-sexp`.
* `sp-splice-sexp-killing-forward` -  Remove the wrapping pair from *this* expression and kill everything from the end of expression before `(point)` to end of this expression.
* `sp-splice-sexp-killing-backward` -  Remove the wrapping pair from *this* expression and kill everything from the beginning of this expression to beginning of expression after `(point)`.
* `sp-splice-sexp-killing-around` - Remove the wrapping pair from *this* expression and kill everything inside save for ARG next expressions.
* `sp-convolute-sexp` - Move the expressions before point in the current list before the enclosing list.
* `sp-absorb-sexp` - Absorb the preceding expression. This is like backward slurp but also moves the expressions before point with the opening delimiter.
* `sp-emit-sexp` - Move all the expressions preceding point except the first one in the list out of the list. This is (not strictly) inverse operation to absorb.
* `sp-extract-before-sexp` - Move the expression following point before the enclosing list. The point moves with the expression.
* `sp-extract-after-sexp` - Move the expression following point after the enclosing list. The point moves with the expression.
* `sp-split-sexp` - Split the current list using the enclosing delimiters.
* `sp-join-sexp` - Join the expressions before and after point into one.
* `sp-rewrap-sexp` - Rewrap the current enclosing list with a new pair.
* `sp-swap-enclosing-sexp` - Swap the enclosing delimiters between this and parent lists.
* `sp-forward-slurp-sexp` - Extend the current list by one balanced expression or symbol by moving the *closing* delimiter.
* `sp-forward-barf-sexp` - Contract the current list by one balanced expression or symbol by moving the *closing* delimiter.
* `sp-backward-slurp-sexp` - Extend the current list by one balanced expression or symbol by moving the *opening* delimiter.
* `sp-backward-barf-sexp` - Contract the current list by one balanced expression or symbol by moving the *opening* delimiter.
* `sp-add-to-next-sexp` - Add the expression *preceding* point to the list *following* point.
* `sp-add-to-previous-sexp` - Add the expression *following* point to the list *preceding* point.
* `sp-select-next-thing` - Select next balanced expression as returned by `sp-forward-sexp`. Examples of usage: Can be cleverly used with wrapping features, for example if you want to wrap next expression in additional pair of parens. It can also be used to select expressions followed by `M-w` to copy to ring but not kill.
* `sp-select-previous-thing` - Select previous balanced expression as returned by `sp-backward-sexp`.
* `sp-select-next-thing-exchange` - Same as `sp-select-next-thing` but execute `exchange-point-and-mark` afterwards.
* `sp-select-previous-thing-exchange` - Same as `sp-select-previous-thing` but execute `exchange-point-and-mark` afterwards.
