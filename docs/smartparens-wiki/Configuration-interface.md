**Note: this is a proposed change in the configuration interface. This is not implemented yet.** This article serves as a reference to the so far discussed changes.

Function `sp-pair` sets global properties of a pair. If a pair is not defined yet, it adds its definition with the specified properties. If the pair is already specified, the provided arguments will override the current arguments, leaving the empty arguments unchanged. This way you can update a definition "interactively" while testing your configuration.

The function `sp-pair` is defined as:

```scheme
(defun* sp-pair (open
                 close
                 &key
                 (actions '(wrap insert))
                 inhibit
                 inhibit-wrap ;; do we want this? see documentation string
                 post-handler)
  "Adds a pair definition.

OPEN is the opening delimiter (pair for short).

CLOSE is the closing delimiter.

ACTIONS is a list of actions that smartparens will perform with this pair. Possible values are:

- insert: autoinsert the closing pair when opening pair is typed.
- wrap: wrap an active region with the pair defined by opening delimiter if this is typed while region is active.

If this list is empty (nil), no actions are performed with this pair. It can still be used for expression highlighting with `show-smartparens-mode'.

  (IMPLEMENTATION NOTE: do we want this? Maybe add another action "highlight"? Most likely the pair shouldn't be even loaded if action set is nil.)

INHIBIT: list of predicates that test whether the pair should be autoinserted in current context. You should not use lambda expressions as they can't be tested for inheritance. Named predicates are recommended.

INHIBIT-WRAP: list of predicates that test whether the pair should be used for wrapping in current context.

  (IMPLEMENTATION NOTE: maybe this should be somehow handled together with INHIBIT? Altho, there probably won't be any other actions---I can't think of any anyway---so if we add one or two more it won't hurt much. Inhibit functions can take two arguments: current pair and current action. Pair because one could possibly be re-used for more pairs with maybe only minor modifications).

POST-HANDLER: list of functions that are called after there has been some action caused by this pair. The arguments are pair and the action (should we also add POST-HANDLER-WRAP? It might be annoying to test (when (eq action 'insert)) in each of these).

  (IMPLEMENTATION NOTE: Maybe we should provide a ritcher structure to this function, like: beginning of wrapped area, end of wrapped area, lengths of the tags, length of changed region etc... so there's possibility to do some indentation and realignment etc. I'm working on new internal framework for passing expressions around, so we might possibly use that.)
")
```

To define properties of pairs specific to a major mode active in a buffer, you can use `sp-local-pair`. If the pair is not globally defined, it will only exist in these modes.

```scheme
(defun* sp-local-pair (modes
                       open
                       close
                       &key
                       (actions '(add))
                       (inhibit '(add))
                       (inhibit-wrap '(add))
                       (post-handler '(add)))
  "Adds a local pair definition or override a global definition.

MODES can be a single mode or a list of modes where these settings should apply.

The rest of the arguments have same semantics as in `sp-pair'.

The pairs are uniquely identified by the opening delimiter.  If you replace the closing one with a different string in the local definition, this will override the global closing delimiter.

The list arguments can optionally be of form starting with `add' or `remove' when these mean \"add to the global list\" and \"remove from the global list\" respectivelly.  Otherwise, the global list is replaced. If you wish to both add and remove things with single call, use ((add ...) (remove ...)) as an argument. Therefore,

  :inhibit '(add my-test)

would mean \"use the global settings for this pair, but also this additional test\".

To disable a pair in a major mode, simply set its actions set to nil. This will ensure the pair is not even loaded when the mode is active.")
```

To define tags, we can use function `sp-local-tag`. By tags we mean more elaborate pairs than simple wrapping strings. For example, consider LaTeX \\being{\_} \\end{\_} as a pair, where _ is replaced with the name of the enviroment.

(Note: most of the lists take the same type of functions as above)

```scheme
(defun* sp-local-tag (modes
                      trigger
                      open
                      close
                      &key
                      (transform 'identity)
                      (actions '(wrap insert))
                      inhibit
                      post-handler
                      )
  "Adds a tag definition.

MODES is a list of modes where this tag is supported.

TRIGGER is a sequence of keys that will trigger this tag.

OPEN is the opening tag. (HERE COMES the description about _ etc will be somewhere in the docs, possibly link to the data structure where it will be explained)

CLOSE is the closing tag.

TRANSFORM is a function that is applied to the content of the opening tag before mirroring to closing tag. It can be any unary function that as an argument takes string inside the opening tag and returns string that should be inserted into the close tag.

ACTIONS is a list of actions that smartparens will perform with this tag. (bare insertion not supported yet)

INHIBIT is a list of predicates that test whether the current action should be carried out.

POST-HANDLER is list of functions that are called after the wrapping is done.")
```
