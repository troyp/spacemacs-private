1. [Use the infinity argument](#use-the-infinity-argument)
2. [Use the self argument](#use-the-self-argument)
3. [Use sp-select-next-thing with wrapping](#use-sp-select-next-thing-with-wrapping)
4. [Use the splicing functions](#use-the-splicing-functions)
5. [Use the type prefixes](#use-the-type-prefixes)
6. [Use wrapping with tags to create shortcuts for common wrapping operations](#use-wrapping-with-tags-to-create-shortcuts-for-common-wrapping-operations)
7. [Miscellaneous movement functions](#miscellaneous-movement-functions)
8. [Binding smartparens functions with a Hydra](#binding-smartparens-functions-with-a-hydra)


This page contains various tips and tricks on using smartparens. Most of the examples show just one or two functions, but the "trick" is usually as generic as possible. You are therefore advised to read the documentation of all functions.

The key-bindings present here are accoding to my [[configuration|example configuration]], so check it out before you plunge in.

In all the following examples `|` represent the point.

<a name="use-the-infinity-argument" />
# Use the infinity argument

Some functions besides accepting only the numeric argument also accept "raw" argument `C-u`, which is interpreted as "infinity". This means, that they will repeat the action for as long as it makes sense. You can read about the precise meaning in the built-in documentation, here are some usefull cases I often use during editing:

I wish to turn `|(1 2 3 4 5 6 ... 185)` into `(1 2)`. Simply do: `C-M-d C-2 C-M-f C-u C-M-k`, "down, forward 2, kill infinity"

Imagine you have a let binding:

```scheme
|(when x
  (let ((a b)
        (c d))
    (stuff)))
```

To quickly get to the `(c d)` expression, I just hit `C-u C-M-d C-n` to "nest infinitely deep (in this case 4) and go to next line". Combining "semantic" navigation with basic forward/backward/nextline/prevline functions is also a good thing to consider :P You don't always need to travel "by expressions".

Or, you realize that after you wrote the "then" form you want to swap the "branches" (which sometimes happen if the condition is negated).

```scheme
(if (condition)
    (progn|
      (code 1)
      (code 2)
      (code 3))
  )

;; just hit C-u C-<left> (sp-forward-barf-sexp everthing) and the code becomes:

(if (condition)
    (progn|)     ;; now fill the progn. Done!
  (code 1)
  (code 2)
  (code 3)
  )
```

<a name="use-the-self-argument" />
# Use the self argument

Besides the infinity argument and standard numeric prefix, some functions also accept "self" argument `C-u C-u`, which makes the function operate on the current enclosing expression. So for example `C-u C-u C-M-k` will kill the list point is in.

Another useful use is to jump to beginning or end of the list. You can do this with `sp-down-sexp` and `sp-backward-down-sexp`.

```scheme
(this is (some long) and | {complicated list} of [various things] hah)

;; after hitting C-u C-u C-M-d
(|this is (some long) and {complicated list} of [various things] hah)

;; after hitting C-u C-u C-M-a
(this is (some long) and {complicated list} of [various things] hah|)
```

**Note:** from commit 181 smartparens provides function abbreviations for these operations: `sp-beginning-of-sexp` and `sp-end-of-sexp`.

<a name="use-sp-select-next-thing-with-wrapping" />
# Use sp-select-next-thing with wrapping

The function family `sp-select-next-thing` is exceptionally handy in combination with wrapping. If you wish to wrap the next expression in a pair of parens, you can simply select it using `sp-select-next-thing-exchange` and hit `(` key.

This can also be easily turned into a macro and bound to a key, for example `C-(`:

```scheme
(fset 'my-wrap-with-paren "\C-](") ;; C-] is my binding for `sp-select-next-thing-exchange'
(define-key smartparens-mode-map (kbd "C-(") 'my-wrap-with-paren)

;; or you can define it as a function. But then undoing the wrapping
;; pair with backspace doesn't work because this interactive command
;; overrides the "last action" SP recognizes. BUT you get the added
;; benefit of numeric argument, so now you can wrap any number of items.
(defun my-wrap-with-paren (&optional arg)
  (interactive "p")
  (sp-select-next-thing-exchange arg)
  (execute-kbd-macro (kbd "(")))
(define-key smartparens-mode-map (kbd "C-(") 'my-wrap-with-paren)
```

Of course, if you wish to wrap more than one thing, you can simply prefix the selection command with a numeric argument: `C-2 C-] (` will select two things and wrap them with (. Or, in the case of `my-wrap-with-paren` function bound to `C-(` simply use `C-2 C-(`.

From commit 250, smartparens can generate these functions automatically if you provide a `:bind <keybinding>` argument in the `sp-pair` or `sp-local-pair` definition. See [[Pair management]] for more information.

<a name="use-the-splicing-functions" />
# Use the splicing functions

Often, I have a form where I sum things, only to realize I don't want to add anything and to simply have the first argument as a value. To turn `(+ |(stuff) 10 14)` into `|(stuff)`, I simply hit `C-S-<backspace>` which calls `sp-splice-sexp-killing-around`. That means kill everything except the next (arg) expression(s) and also unwrap the current list. Handy! The `killing-forward` and `killing-backward` methods work similarly, but kill everything from point to the end or beginning of the list. Thus:

```scheme
(when (some boring condition)|
  (this should execute always)
  (FOREVER))

;; after calling sp-splice-sexp-killing-backward
  (this should execute always)
  (FOREVER)
```

<a name="use-the-type-prefixes" />
# Use the type prefixes

This feature is similar to what `vim` calls "text objects". Instead of operating on all "things" as smartparens sees them, you can specifically select one type of things that the next command will recognize. Currently, three types of objects are supported: symbols, paired expressions and SGML tags.

To select the type, you use the "prefix" argument:

```scheme
(sp-prefix-symbol-object)
(sp-prefix-pair-object)
(sp-prefix-tag-object)
```

and then call the command normally. One limitation is that this command has to be bound to a keyboard sequence.

Here are some examples of possible use. The | symbol will represent the point. Remember that all the commands need to be bound to a key sequence. I fully write them out for clarity.

    ;; I wish to jump out of the <p> tag, but calling sp-up-sexp would jump after the )
    ;; use: sp-prefix-tag-object sp-up-sexp
    <p> this is some text (with a |note in parens) and more </p>
    results into
    <p> this is some text (with a note in parens) and more </p>|

    ;; I wish to jump over two paired expressions, ignoring the stuff inbetween
    ;; use: C-u 2 sp-prefix-pair-object sp-forward-sexp
    ;; or shorter: C-2 sp-prefix-pair-object sp-forward-sexp
    |foo bar baz (pair number one) and more things we don't care about (and pair two) and more
    results into
    foo bar baz (pair number one) and more things we don't care about (and pair two)| and more

    ;; I wish to jump over four symbols ignoring any structure.
    ;; use: C-4 sp-prefix-symbol-object sp-forward-sexp
    a |word <p> inside (tag and pair) </p>
    results into
    a word <p> inside (tag| and pair) </p>

    ;; I wish to transpose two SGML tags
    ;; use: sp-prefix-tag-object sp-transpose-sexp
    first <b>word</b> and |another <b>expression</b>
    results into
    first <b>expression</b> and another <b>word</b>|

You can see that these commands can be really powerful and precise. If you come up with some clever use, please let me know, I'll add it here ;)

<a name="use-wrapping-with-tags-to-create-shortcuts-for-common-wrapping-operations" />
# Use wrapping with tags to create shortcuts for common wrapping operations

Instead of wrapping with "regular" tags, like html `<span></span>` pairs, you can also simply wrap the region with predefined text for opening and closing part. This is handy to wrap blocks of code in "code highlight" syntax in markdown. Just select the region and hit `s`:

```scheme
(sp-local-tag 'markdown-mode "s" "```scheme" "```")
```

Another use I have for this is inserting "italian quotes" (also used in other languages, called Guillemets) in LaTeX:

```scheme
(sp-local-tag 'latex-mode "i" "\"<" "\">")
```

Typing it out is annoying, especially when you are adding them later on or changing the style from other language. Just select the sentence, hit `i` and be done with it! Of course, to do the bulk replacing, you should use `replace-regexp`.

You can even add shortcuts for common tags you insert in html or xml. Instead of typing them out each time, you just hit one key. For example:

```scheme
(sp-local-tag 'html-mode "b" "<span class=\"bold\">" "</span>")
```

<a name="miscellaneous-movement-functions" />
# Miscellaneous movement functions

In addition to the [[standard navigation functions|working with expressions]], smartparens define few useful functions that are used mostly internally, but also have a "command" interface. Here's the list:

```scheme
sp-forward-symbol (&optional arg)
sp-backward-symbol (&optional arg)

sp-forward-whitespace ()
sp-backward-whitespace ()

sp-skip-forward-to-symbol (&optional stop-at-string stop-after-string)
sp-skip-backward-to-symbol (&optional stop-at-string stop-after-string)
```

The backward versions always do the same but in backward direction. Here's a short description for each command.

* `sp-forward-symbol` - moves *after* the next symbol. Symbol is a chunk of text in the word or symbol constituent syntax class.
* `sp-forward-whitespace` - skips any whitespace characters until hitting any non-whitespace character.
* `sp-skip-forward-to-symbol` - Skips forward over whitespace, comments and string quotes until a word or symbol constituent syntax class character is hit. Note: when used interactively, both optional arguments are nil. This function also *does not* accept numeric prefix!

<a name="binding-smartparens-functions-with-a-hydra" />
# Binding smartparens functions with a Hydra

With the [Hydra](https://github.com/abo-abo/hydra) package users can create "local command modes". You enter it with a prefix binding and then, while the hydra is active, you use the local keymap it provides.  This is useful with smartparens where you can enter a "local" smartparens map and then execute complex chains of smartparens invocations with just single-key bindings.

The ever so awesome [lunaryorn](https://github.com/lunaryorn) created a nice one with a help popup and everything, you can find it in the [init.el](https://github.com/lunaryorn/.emacs.d/blob/master/init.el) under smartparens `use-package` declaration.
