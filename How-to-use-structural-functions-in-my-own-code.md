1. [Getting things](#getting-things)
2. [Getting context](#getting-context)
3. [Example funciton](#example-funciton)

It is very simple to use smartparens internal functions in your own code. If you ever have a need to search the buffer for balanced expressions, this is the way to do it!

<a name="getting-things" />
# Getting things

There are four functions that return what we call a "thing", either a symbol, string or a balanced expression. The returned thing is simply the first one encountered when searching forward. These are:

```scheme
sp-get-sexp (&optional back)
sp-get-paired-expression (&optional back)
sp-get-sgml-tag (&optional back)
sp-get-string (&optional back)
sp-get-symbol (&optional back)
sp-get-thing (&optional back)
sp-get-whitespace () ;; special, return the whitespace around point.
```

All of them accept an optional argument `back`. When set to `t` the function will search for the thing in backward direction. None of these functions move the point or change the buffer in any way.

`sp-get-sexp` returns the first balanced expression it encounters, skipping symbols and strings to get there. Right now, only expressions where the opening and closing delimiter is different are supported. This function also consider SGML tags and other special kinds of balanced expressions. If you want to search for just a specific type of a balanced expression, use the specialized functions.

For example, in the foolowing text the call to `sp-get-sexp` would return information about the `(gravida vitae)` expression:

```scheme
|Lorem ipsum dolor sit amet, consectetuer adipiscing
elit. Praesent libero orci, auctor sed, faucibus vestibulum, (gravida
vitae), arcu. Nunc posuere. Suspendisse potenti. Praesent in arcu ac
nisl ultricies ultricies. Fusce eros.
```

`sp-get-paired-expression` works the same way as `sp-get-sexp` but only considers the pairs delimited by entries on `sp-pair-list`

`sp-get-sgml-tag` works the same way as `sp-get-sexp` but only considers the SGML tags (html/xml etc.).

`sp-get-string` returns the first closed string it encounters, skipping symbols to get there. Strings are detected using the `(nth 3 (syntax-ppss))` call, which uses an internal emacs expression parser. Therefore, strings are only recognized when the mode sets the syntax-table accordingly.

`sp-get-symbol` returns the first symbol. Symbol is any sequence of characters from either word constituent or symbol constituent syntax class. Symbols are delimited by whitespace or opening or closing pairs. Thus:

```scheme
0Lorem ipsum 1(dolor sit) amet, consectetuer  2   "adipiscing elit".
```

Point at 0 return `Lorem`, point at 1 return `dolor` and point at 2 return `adipiscing`

Finally, `sp-get-thing` simply returns the closest of all the things above.

All of these functions use the same return value format, which is a plist containing 5 key-value pairs (for example `(:beg 1 :end 23 :op "(" :cl ")" :prefix "")`). However, you should *never* access this return value directly using `plist-get` and always use the `sp-get` macro. This allows us to make internal changes to the return format without breaking your code.

You can query for lots of information about the expression using the `sp-get` macro. The calling format is  `(sp-get expression :value)`, where `:value` is one of the following:

* `:beg` - point in buffer before the opening delimiter
* `:end` - point in the buffer after the closing delimiter
* `:beg-in` - point in buffer after the opening delimiter
* `:end-in` - point in buffer before the closing delimiter
* `:beg-prf` - point in buffer before the prefix of this expression
* `:op` - opening delimiter
* `:cl` - closing delimiter
* `:op-l` - length of the opening pair
* `:cl-l` - length of the closing pair
* `:len` - length of the entire expression, including enclosing delimiters and the prefix
* `:len-out` - length of the the pair ignoring the prefix, including delimiters
* `:len-in` - length of the pair inside the delimiters
* `:prefix` - expression prefix
* `:prefix-l` - expression prefix length

<a name="getting-context" />
# Getting context

There are also functions that operate on context around the point:

```scheme
sp-get-enclosing-sexp (&optional arg)
sp-get-list-items (&optional lst)
```

`sp-get-enclosing-sexp` will return the list that encloses the point. The enclosing list is the list at the same "depth" as the point. For example:

```scheme
(one two | (three four) five) ;; return the list (one ... five)
(one two (three | four) five) ;; return the list (three four)
```

You can set the optional argument `arg` to a positive number, then the expression returned will be the enclosing list `arg` levels up. This is the same as doing `(sp-up-sexp (1- arg))` and then calling `sp-get-enclosing-sexp`.

`sp-get-list-items` will return a list of all the expressions inside the current list. The first item of this list is the information about the enclosing list itself. With optional argument `lst` parse that list instead. The `lst` argument should be a plist of the format retuned by `sp-get-thing`. For example, calling it inside this list:

```scheme
(one | (two three) four)
```

would return:

```scheme
((:beg 1 :end 23 :op "(" :cl ")" :prefix "")    ;; the enclosing list
 (:beg 2 :end 5 :op " " :cl " " :prefix "")     ;; first item
 (:beg 6 :end 17 :op "(" :cl ")" :prefix "")    ;; second item
 (:beg 18 :end 22 :op " " :cl " " :prefix ""))  ;; third item
```

You can then traverse the list and use `sp-get` macro to retrieve information about each item.

<a name="example-funciton" />
# Example funciton

Here's an example function that unwraps a latex command.

```scheme
(defun my-latex-remove-command ()
  "Unwrap the command that point is in.  By command we understand
a symbol starting with \\ and followed by a block of text
enclosed in {}."
  (interactive)
  (let ((ok (sp-get-enclosing-sexp)))
    (when ok
      (save-excursion                  ;; this deletes the command name and \
        (goto-char (sp-get ok :beg))
        (zap-to-char -1 ?\\ ))
      (sp-splice-sexp))))              ;; remove the enclosing {}
```

Thus, calling this function in this situation:

```latex
\latexcommand{some very long | list of \emph{complicated} arguments}
```

results in

```latex
some very long | list of \emph{complicated} arguments
```
