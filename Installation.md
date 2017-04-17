Install smartparens by placing `smartparens.el` in `/path/to/elisp`, a directory of your choice, and adding to your .emacs file:

```scheme
(add-to-list 'load-path "/path/to/elisp")
```

Then, the basic setup is as follows:

```scheme
(smartparens-global-mode 1)
```

You can also install this as a package with `M-x package-install smartparens`. This package is available in `melpa` repository.

If you've installed smartparens as a package, you don't need to require it, as there is an autoload on `smartparens-global-mode`.

You can disable smartparens in specific global modes by customizing `sp-ignore-mode-list`. Of course, you can also only turn it on in specific modes via the hook mechanisms.

This package *depends* on [dash](https://github.com/magnars/dash.el). If you've installed smartparens via `package-install`, it should resolve dependencies automatically (dash is on melpa and marmalade). If not, you'd need to install it manually. See the installation information on their homepage for instructions.

Smartparents enhances the behaviour of certain keys, namely those that are part of any pair or tag. To do this, it automatically rebinds them to `sp-self-insert-command` that handles the additional tasks. Therefore, you should not re-bind any of the trigger keys yourself. If some modes, like `org-mode`, rebind these keys themselves, then smartparents might not fully work (but so far, no serious problems were reported regarding org).

Make sure you read about [[compatibility issues]]. If you use some mode that isn't 100% compatible with smartparens, it would be very helpful if you reported any sort of weird behaviour so it can be fixed.

To get a quick overview of additional useful configuration, see [[example configuration]]. Please have a look at it as it contains a working example and explains many features "visually".
