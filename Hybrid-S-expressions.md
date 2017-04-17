# Hybrid S-expressions

While direct manipulation of S-expressions (sexps, balanced expressions) is especially rewarding in languages like elisp, scheme, clojure and friends, in most "regular" C-like languages (C, Java, C#, ...), the notion of "sexp" is very weak for useful manipulation of source code.

To partially solve this smartparens introduces a notion of "hybrid sexp". First discussed in [Issue #95](https://github.com/Fuco1/smartparens/issues/95), a general framework for retrieving and manipulating hybrid sexps is being added into smartparens.

A definition of hybrid sexp is as follows: it is the smallest balanced region containing the point that does not expand further than current line, with the only exception to keep the balance. Here, by balance we mean any hanging opened sexps that are not closed on the same line. Here are some examples. A `()` pair will represent sexp delimiters while `[]` will represent the hybrid sexp. As usual, `|` represents point.

*Note: the definition is really clumsy but I really have no better idea on explaining it. If you understand what's going on and can provide better description, please start an issue and tell me! (or email me)*

```scheme
[foo ba|r baz]
quux flux

;;---

[foo ba|r (baz
           quux)] flux

;;---

([foo b|ar baz]) quux flux

;;---

(foo
 [bar b|az]) quux flux

;;---

([foo|]
 bar baz) quux flux
```

Here's a list of functions that can operate on hybrid sexps, with brief descriptions. You should read the documentation in emacs via `C-h f function-name RET` to get more information.

```scheme
sp-kill-hybrid-sexp (arg)
sp-transpose-hybrid-sexp (&optional arg)
sp-push-hybrid-sexp ()
sp-slurp-hybrid-sexp ()
sp-indent-adjust-sexp ()
sp-dedent-adjust-sexp ()
```

* `sp-kill-hybrid-sexp` - Kill a line as if with `kill-line`, but respecting delimiters. This works almost the same as `paredit-kill`.
* `sp-transpose-hybrid-sexp` - Transpose the hybrid line the point is at with the one before it. `sp-backward-sexp` is used to "travel" into the previous hybrid sexp.
* `sp-push-hybrid-sexp` - Similar to translate, but instead of transposing previous expression, it pushes the following one over the next.
* `sp-slurp-hybrid-sexp` - Like `sp-forward-slurp-sexp` but slurps a hybrid sexps. At the moment does not support any prefix arguments.
* `sp-indent-adjust-sexp` - Add the hybrid sexp at point to the previous S-expression
* `sp-dedent-adjust-sexp` - Barf everything after the point. This is almost the same as `sp-forward-slurp-sexp` but handles hanging delimiters in a special way.

We will now illustrate these functions on examples. In all examples `|` marks the point

### sp-kill-hybrid-sexp

```c
for | (int i = 1; i < 10) {
  bar = i + 1;
}

  :=>

for |

//---

|int baz = (bar
            /
            2);

  :=>

|
```

### sp-transpose-hybrid-sexp

```c
for (int i = 1; i < 10) {
  bar = i + 1;
}

int |baz = (bar
            /
            2);

  :=>

int baz = (bar
           /
           2);

for (int i = 1; i < 10) {
  bar = i + 1;
}
|
```

### sp-push-hybrid-sexp

```c
|for (int i = 1; i < 10) {
  bar = i + 1;
}

int baz = (bar
           /
           2);

  :=>

|int baz = (bar
            /
            2);

for (int i = 1; i < 10) {
  bar = i + 1;
}
```

### sp-slurp-hybrid-sexp

```c
for (int i = 1; i < 10) {
 |bar = i + 1;
}
int baz = (bar
           /
           2);
return 0;

  :=>

for (int i = 1; i < 10) {
 |bar = i + 1;
  int baz = (bar
             /
             2);
}
return 0;

  :=>

for (int i = 1; i < 10) {
 |bar = i + 1;
  int baz = (bar
             /
             2);
  return 0;
  }
```
