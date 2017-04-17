1. [Repeated wrapping](#repeated-wrapping)
2. [Wrapping with tags](#wrapping-with-tags)

If you select a region and start typing any of the pairs, the active region will be wrapped with the pair. For multi-character pairs, a special insertion mode is entered, where point jumps to the beginning of the region. If you insert a complete pair, the region is wrapped and point returns to the original position.

If you insert a character that can't possibly complete a pair, the wrapping is canceled, the point returns to the original position and the typed text is inserted. If you use `delete-selection-mode` or `cua-delete-selection`, the content of the region is removed first.

At any time in the insertion mode you can use `C-g` to cancel the insertion. In this case, both the opening and closing pairs are removed and the point returns to the original position. The region is not deleted even if some "delete-selection" mode is active.

If `sp-autodelete-wrap` is `t` (by default on) the most recent wrap can be removed if you invoke `delete-backward-char` (usually bound to backspace key) immediately after the wrapping is done.

You can use functions `sp-select-next-thing` and `sp-select-previous-thing` to activate a region around next or previous expression for convenient wrapping. Read [tips and tricks](Tips-and-tricks#wiki-use-sp-select-next-thing-with-wrapping) for more suggestions.

<a name="repeated-wrapping" />
# Repeated wrapping

After wrapping a region and immediately afterward inserting another *basic* pair (that is, defined by `sp-pair`), it is often desired to apply this pair as another wrap around the just wrapped region. Imagine, in LaTeX mode, wrapping "word" with quotes to produce <code>\`word'</code>. Now, hitting another back-tick should produce double-quoted word <code>\`\`word''</code>. The same can apply to `markdown-mode` and \* character to mark italics/bold text.

You can set variable `sp-wrap-repeat-last` to "No repeat", "Repeat only if same", "Re-wrap region with any pair". Read the built-in description for more info. By default, wrap is only repeated if you use the same pair as the last wrapping.

Note that this behaviour is only active if you *type in* the pair immediately after the wrapping without invoking any additional command, such as backspace or navigation command. However, since this is mostly intended for single character pairs, this does not impose any real limitation.

<a name="wrapping-with-tags" />
# Wrapping with tags

Wrapping with more structured tags is also supported. For example, in `html-mode` you might want to automatically wrap a region `some code` and change it into `<span class="code">some code</span>`. For this purpose, you can define tag pairs. These allow you to enter special tag wrapping insertion mode, where you can enter arbitrary text. It also automatically mirror the opening tag text into the closing tag. Furthermore, the closing tag can be automatically transformed with any function to a different string. For example, the opening tag's content `span class="code"` can be transformed to just `span`.

The tag wrapping pairs have higher priority than regular tags, that is, if it is possible to start tag-wrapping, the regular wrap mode is exited and the tag insertion mode is entered *even if* there is possible continuation of the currently inserted opening wrap pair. For example, if tag insertion trigger is `<` and there is a regular pair `<< >>`, this is ignored and the tag insertion mode is entered immediately after `<` is inserted.

When in tag insertion mode, special key-bindings are active. These are:

* `C-a`, `C-e` jumps to the beginning/end of the tag section.
* `C-g` terminate the tag insertion mode.

Tag insertion mode is also terminated if you leave the area of the opening tag pair overlay, for example with search function or `previous-line` command.

Tags are defined by following function:

```scheme
;; these pairs are already present in default configuration
(sp-local-tag '(sgml-mode html-mode) "<" "<_>" "</_>" :transform 'sp-match-sgml-tags)
(sp-local-tag '(tex-mode plain-tex-mode latex-mode) "\\b" "\\begin{_}" "\\end{_}")
```

1. Modes where this is allowed. Tag pairs can't be defined globally. The rationale is that they are highly specialized and the idea of specific tags for specific modes makes more sense.
2. Tag trigger
3. Opening tag format
4. Closing tag format

The character `_` in the format strings is replaced with the inserted text and mirrored to the closing pair. Before inserting text in the closing pair, content of the opening pair is transformed with transformation function. Only one `_` per pair is allowed. The closing tag does not have to contain `_` (then no text is inserted there). If the opening pair doesn't have `_` either, the tag is simply inserted as text and tag insertion mode is not entered. This can be used to form "shortcuts" for commonly used wrappings, such as:

```scheme
(sp-local-tag 'markdown-mode "2" "**" "**")
```

to mark selected text as bold.

Similarly to the `sp-pair` function, `sp-local-tag` can accept several keyword arguments. These are:

* `:transform` - a function that is used to transform the content of the opening pair before inserting it into the closing pair. This function should accept one argument, the content of the opening pair, and return the content of the closing pair. For example, see built-in function `sp-match-sgml-tags`. Defaults to `'identity`.
* `:post-handlers` - list of functions that are called after the tag is inserted. Each function on this list should accept two arguments: the trigger string and the action (currently only `'wrap`).
* `:actions` - currently, the only supported action is wrapping, so you don't need to specify this argument. It is present for possible future enhancements.

You can add different tags for the same trigger in different modes.

Tags can be removed by calling `sp-local-tag` with argument `:actions` set to `nil`:

```scheme
;; the nils here stand for the opening and closing pairs. Since you are removing a pair, you don't have to specify them
(sp-local-tag 'sgml-mode "<" nil nil :actions nil)
```
