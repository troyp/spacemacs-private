1. [Adding pairs](#adding-pairs)
2. [Removing pairs](#removing-pairs)
3. [Default pairs](#default-pairs)
4. [Local pair definitions](#local-pair-definitions)

**Note:** Starting with commit 167, a new pair management interface was implemented. If you are interested in the development process, you can read the discussion [here](https://github.com/Fuco1/smartparens/issues/31).

<a name="adding-pairs" />
# Adding pairs

To define new pair, you use the `sp-pair` function. Example:

```scheme
(sp-pair "\{" "\}") ;; latex literal brackets (included by default)
(sp-pair "<#" "#>")
(sp-pair "$" "$")   ;; latex inline math mode. Pairs can have same opening and closing string
```

Pairs defined by this function are used both for wrapping and auto insertion. However, you can disable certain pairs for auto insertion and only have them for wrapping, or other way. This is achieved by setting the pair's [[permissions]] to allow or disable certain operations in certain contexts.

Pairs have to be **prefix-free**, that means no opening pair should be a prefix of some other pair. This is reasonable and in fact necessary for correct function. For example, with auto insertion of pair `"("  ")"` and pair `"(/"  "/)"` (which has as a prefix the one paren version), the program wouldn't know you might want to insert the longer version and simply inserts `(|)`. This can technically be fixed with "look-ahead" and then backward alteration of input text, but it will be confusing and probably not very useful anyway. If such functionality will ever get implemented, it will be disabled by default.

You can add a binding for a "wrapping" action. Smartparens will then automatically bind a function that will wrap the next expression with this pair to the supplied key. It accepts the same prefix arguments as `sp-select-next-thing`. In addition, if a region is already active, it will wrap this region\*. To do this, use the `:wrap` argument:

```scheme
(sp-pair "(" ")" :wrap "C-(")

;; |foobar
;; hit C-(
;; becomes (|foobar)
```

**\* Note**: this is useful in combination with evil visual selection mode, since with regular emacs, smartparens wraps the active regions automatically when you press the delimiter.

You can also add a binding for "insert" action. This is done the exact same way as for wrapping, but the keyword is `:insert`. Calling this will simply insert the pair in the buffer. This is useful if you want to insert the pair with a modifier hotkey. To simply provide a shorter trigger you can type, you can specify a `:trigger` keyword.

```scheme
(sp-local-pair 'LaTeX-mode "\\left(" "\\right)" :insert "C-b l" :trigger "\\l(")
```

This will make smartparens insert `\left(|\right)` when you type `\l(` or hit `C-b l` (where `|` is the point). Typing out the entire opening delimiter `\left(` will also work.

**Note**: many such commands for LaTeX are provided in configuration file `smartparens-latex.el`. Check it out!

It is generally better to add these bindings only to certain major modes where you wish you use this functionality instead of binding them globally to avoid hotkey clashes. See the section about [local pair definitions](#local-pair-definitions).

<a name="removing-pairs" />
# Removing pairs

You can remove pairs by calling `sp-pair` using the optional key argument `:actions` with value `:rem`. This will also automatically delete any assigned [[permissions]]! This command is mostly only useful for debugging or removing built-in pairs.

```scheme
;; the second argument is the closing delimiter, so you need to skip it with nil
(sp-pair "\{" nil :actions :rem)
(sp-pair "'" nil :actions :rem)
```

<a name="default-pairs" />
# Default pairs

Since some pairs are so common that virtually every user would insert them, smartparens comes with a list of global default pairs. At the moment, this list includes:

```scheme
("\\\\(" . "\\\\)") ;; emacs regexp parens
("\\{"   . "\\}")   ;; latex literal braces in math mode
("\\("   . "\\)")   ;; capture parens in regexp in various languages
("\\\""  . "\\\"")  ;; escaped quotes in strings
("/*"    . "*/")    ;; C-like multi-line comment
("\""    . "\"")    ;; string double quotes
("'"     . "'")     ;; string single quotes/character quotes
("("     . ")")     ;; parens (yay lisp)
("["     . "]")     ;; brackets
("{"     . "}")     ;; braces (a.k.a. curly brackets)
("`"     . "`")     ;; latex strings. tap twice for latex double quotes
```

> From commit 233 the default pair for \` is \`, not single tick '. The default configuration reflects this and adds `' pairing for emacs modes and latex modes.

<a name="local-pair-definitions" />
# Local pair definitions

Sometimes, a globally defined pair is not appropriate for certain major modes. You can redefine globally defined pairs to have different definition in specific major modes. For example, globally defined pair \`\` is used in `markdown-mode` to insert inline code. However, `emacs-lisp-mode` uses \`' for links in comments and in `LaTeX-mode` this pair is used for quotes. Since they share the opening sequence (the "trigger"), it's impossible to have both defined globally at the same time. Therefore, it is desired to redefine this global pair to this new value locally.

That is accomplished by using this function:

```scheme
(sp-local-pair 'emacs-lisp-mode "`" "'") ;; adds `' as a local pair in emacs-lisp-mode
```

If a global pair with the same trigger does not exist, this pair is defined locally and will only be used in the specified mode. Therefore, you do not need to define a pair globally and then overload it locally. The local definition is sufficient.

Instead of one mode, you can also specify a list to handle multiple modes at the same time (for example `'(emacs-lisp-mode LaTeX-mode)`).

Local pairs can be removed by calling `sp-local-pair` with optional keyword argument `:actions` with value `:rem`:

```elisp
(sp-local-pair 'LaTeX-mode "`" nil :actions :rem)
```

**Important**: this only removes the pairs you have previously added using `sp-local-pair`. It does not remove/disable a global pair in the specified mode. If you want to disable some pair in specific modes, set its permissions accordingly.
