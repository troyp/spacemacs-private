## Colorful Hydras

Each hydra head has a certain associated color, red or blue, that determines
whether or not the hydra will continue after the header is called (red being continue, blue being stop).
Additionally, the hydra body can have one of five colors (amaranth, teal, pink, red, blue)
that (1) determines the default color of all the heads; and (2) determines
what happens when a key that is not associated to a head is pressed.
The following table inspired by @kaushalmodi summarizes the effects of the different colors.
```
|----------+-----------+-----------------------+-----------------|
| Body     | Head      | Executing NON-HEADS   | Executing HEADS |
| Color    | Inherited |                       |                 |
|          | Color     |                       |                 |
|----------+-----------+-----------------------+-----------------|
| amaranth | red       | Disallow and Continue | Continue        |
| teal     | blue      | Disallow and Continue | Quit            |
| pink     | red       | Allow and Continue    | Continue        |
| red      | red       | Allow and Quit        | Continue        |
| blue     | blue      | Allow and Quit        | Quit            |
|----------+-----------+-----------------------+-----------------|
```

So for example, calling a red head will not vanquish the hydra,
while calling a blue head will.

The third column, "executing non-heads", determines what happens
when the users presses a key that is not associated to any head.
The hydra can then either disallow this key press (i.e., the key press
is blocked and nothing happens) or allow it (the command bound to the key according
to the normal non-Hydra key bindings is executed),
and additionally,
the hydra can either quit or continue.

As is clear from this description,
a header can only be red or blue:
the other 3 colors determine what happens when a key *not* belonging to a header is pressed.

### Examples

In all the older examples, all heads are red by default. You can specify blue heads like this:

```cl
(global-set-key
 (kbd "C-c C-v")
 (defhydra toggle ()
   "toggle"
   ("a" abbrev-mode "abbrev" :color blue)
   ("d" toggle-debug-on-error "debug" :color blue)
   ("f" auto-fill-mode "fill" :color blue)
   ("t" toggle-truncate-lines "truncate" :color blue)
   ("w" whitespace-mode "whitespace" :color blue)
   ("q" nil "cancel")))
```

Or, since the heads can inherit the color from the body, the following is equivalent:

```cl
(global-set-key
 (kbd "C-c C-v")
 (defhydra toggle (:color blue)
   "toggle"
   ("a" abbrev-mode "abbrev")
   ("d" toggle-debug-on-error "debug")
   ("f" auto-fill-mode "fill")
   ("t" toggle-truncate-lines "truncate")
   ("w" whitespace-mode "whitespace")
   ("q" nil "cancel")))
```

The above Hydra is very similar to this code:

```cl
(global-set-key (kbd "C-c C-v t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c C-v f") 'auto-fill-mode)
(global-set-key (kbd "C-c C-v a") 'abbrev-mode)
```

However, there are two important differences:

- you get a hint like this right after <kbd>C-c C-v</kbd>:

        toggle: [t]: truncate, [f]: fill, [a]: abbrev, [q]: cancel.

- you can cancel <kbd>C-c C-v</kbd> with a command while executing that command, instead of e.g.
getting an error `C-c C-v C-n is undefined` for <kbd>C-c C-v C-n</kbd>.

## Amaranth color

According to [Wikipedia](http://en.wikipedia.org/wiki/Amaranth):

> The word amaranth comes from the Greek word amaranton, meaning "unwilting" (from the
> verb marainesthai, meaning "wilt").  The word was applied to amaranth because it did not
> soon fade and so symbolized immortality.

Hydras with amaranth body are impossible to quit with any binding *except* a blue head.
A check for at least one blue head exists in `defhydra`, so that you don't get stuck by accident.

Here's an example of an amaranth Hydra:

```cl
(global-set-key
 (kbd "C-z")
 (defhydra hydra-vi
     (:pre
      (set-cursor-color "#40e0d0")
      :post
      (set-cursor-color "#ffffff")
      :color amaranth)
   "vi"
   ("l" forward-char)
   ("h" backward-char)
   ("j" next-line)
   ("k" previous-line)
   ("q" nil "quit")))
```

The only way to exit it, is to press <kbd>q</kbd>. No other methods will work.  You can use an
amaranth Hydra instead of a red one, if for you the cost of being able to exit only though certain
bindings is less than the cost of accidentally exiting a red Hydra by pressing the wrong prefix.

Note that it does not make sense to define a single amaranth head, so this color can only be
assigned to the body. An amaranth body will always have some amaranth heads and some blue heads
(otherwise, it's impossible to exit), no reds.


## More info
http://oremacs.com/2015/02/19/hydra-colors-reloaded/