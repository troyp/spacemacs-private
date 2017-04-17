This article describe advice and hooks used by smartparens.

Advices
============

Both of these advices postpone the testing of "delete active selection" to let smartparens handle the wrapping properly. The original hooks are called from `sp-delete-selection-mode-handle` function in proper order *after* the tests for wrapping is done.

We also redefine `self-insert-command` in `cua--region-keymap` and route it through a filter function `sp-cua-replace-region`. That function simply translate back to `self-insert-command` if smartparens is active and call the `cua-replace-region` directly otherwise.

```scheme
(defadvice cua-mode (after cua-mode-fix-selection activate)
  (when (and cua-mode)
    (define-key cua--region-keymap [remap self-insert-command] 'sp-cua-replace-region)
    (remove-hook 'pre-command-hook 'cua--pre-command-handler)))

(defadvice delete-selection-mode (after delete-selection-mode-fix-selection activate)
  (when (and delete-selection-mode)
    (remove-hook 'pre-command-hook 'delete-selection-pre-hook)))
```

This advice is called before `delete-backward-char` to handle various things that deal with deletion, such as: automatically deleting whole pair (`\{\}| -> \{`, `\{|\} -> |`) and deleting wrapping pair immediately after wrapping.

```scheme
(defadvice delete-backward-char (before sp-delete-pair-advice activate)
  (sp-delete-pair (ad-get-arg 0)))
```

The most complex advice is around `self-insert-command`. This advice handles all the "invokations" smartparens make: region wrapping, pair auto-insertion, skipping closing pairs after insertion, auto-escaping of quotes. It also updates `sp-last-operation` which is an internal variable used to determine the last action made by smartparens (such as wrapping, pair insertion, deletion etc.). You can see the code in the repositary, it is too long to needlessly copy here.

Hooks
============

Smartparens adds its own hooks to `post-command-hook` and `pre-command-hook` called `sp-*-command-hook-handler` respectively.

The `sp-post-command-hook-handler` handles the wrap-overlays. If you are in wrap mode (either regular or tag-insertion), this command cancels the wrapping if you move point outside the tag/wrapped region. It also updates the `sp-last-operation` on various occasions.

The `sp-pre-command-hook-handler` simply calls `sp-delete-selection-mode-handle` function. This function properly handles deletion of selected regions if `cua-mode` or `delete-selection-mode` are active. When smartparens is inactive, it simply calls the original handlers (that are removed by advices mentioned above). Otherwise it postpones the handling if `this-command` is `self-insert-command`. In that case, the decision whether to wrap or delete is done in `sp-wrap-region-init` that calls back this function if we want to delete the region.
