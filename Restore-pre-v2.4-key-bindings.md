Please also see the [release announcement](/2016/01/18/magit-2.4)
for **v2.4** and the documentation about
[Branching](https://magit.vc/manual/magit/Branching.html).

## What changed?

Starting with version **2.4**, Magit supports the *push-remote* in
addition to the *upstream*.  This made it necessary to add and change
some key bindings as summarized in the following table.

|                           | < v2.4                   |                           | >= 2.4                              |
| ------------------------- | ------------------------ | ------------------------- | ----------------------------------- |
| <kbd>f</kbd>              | **`magit-fetch-popup`**  | <kbd>f</kbd>              | **`magit-fetch-popup`**             |
|                           |                          | <kbd>f</kbd> <kbd>p</kbd> | `magit-fetch-from-push-remote`      |
| <kbd>f</kbd> <kbd>f</kbd> | `magit-fetch-current`    | <kbd>f</kbd> <kbd>u</kbd> | `magit-fetch-from-upstream`         |
| <kbd>f</kbd> <kbd>o</kbd> | `magit-fetch`            | <kbd>f</kbd> <kbd>e</kbd> | `magit-fetch` from elsewhere        |
|                                                                                                                        |
| <kbd>F</kbd>              | **`magit-pull-popup`**   | <kbd>F</kbd>              | **`magit-pull-popup`**              |
|                           |                          | <kbd>F</kbd> <kbd>p</kbd> | `magit-pull-from-push-remote`       |
| <kbd>F</kbd> <kbd>F</kbd> | `magit-pull-current`     | <kbd>F</kbd> <kbd>u</kbd> | `magit-pull-from-upstream`          |
| <kbd>F</kbd> <kbd>o</kbd> | `magit-pull`             | <kbd>F</kbd> <kbd>e</kbd> | `magit-pull` from elsewhere         |
|                                                                                                                        |
| <kbd>r</kbd>              | **`magit-rebase-popup`** | <kbd>r</kbd>              | **`magit-rebase-popup`**            |
|                           |                          | <kbd>r</kbd> <kbd>p</kbd> | `magit-rebase-onto-remote`          |
| <kbd>r</kbd> <kbd>l</kbd> | `magit-rebase-unpushed`  | <kbd>r</kbd> <kbd>u</kbd> | `magit-rebase-onto-upstream`        |
| <kbd>r</kbd> <kbd>r</kbd> | `magit-rebase`           | <kbd>r</kbd> <kbd>e</kbd> | `magit-rebase` onto elsewhere       |
|                                                                                                                        |
| <kbd>P</kbd>              | **`magit-push-popup`**   | <kbd>P</kbd>              | **`magit-push-popup`**              |
| <kbd>P</kbd> <kbd>Q</kbd> | `magit-push-quickly`     | <kbd>P</kbd> <kbd>p</kbd> | `magit-push-current-to-push-remote` |
| <kbd>P</kbd> <kbd>P</kbd> | `magit-push-current`     | <kbd>P</kbd> <kbd>u</kbd> | `magit-push-current-to-upstream`    |
| <kbd>P</kbd> <kbd>e</kbd> | `magit-push-elsewhere`   | <kbd>P</kbd> <kbd>e</kbd> | `magit-push-current` to elsewhere   |

As you can see, the new bindings are consistent across these four
popups.  <kbd>p</kbd> always acts on the **p**ush-remote, <kbd>u</kbd>
always acts on the **u**pstream, and <kbd>e</kbd> (for **e**lsewhere)
always reads a branch or remote from the user and then acts on that.

### The old "same-key-twice" bindings

Previously, pressing the same key, which was used to enter the popup
buffer, a second time once inside the popup buffer, did invoke the
"most useful and commonly used of the available commands":

* <kbd>f f</kbd> fetched from the upstream of the current branch.
* <kbd>F F</kbd> pulled into the current branch from its upstream.
* <kbd>P P</kbd> pushed the current branch to its upstream.

Unfortunately there was an inconsistency: <kbd>r r</kbd> which, by
that logic, *should have* rebased the current branch onto its
upstream, did instead ask for a branch to rebase onto.  <kbd>r l</kbd>
did rebase onto the upstream.

It is understandable, that some users would like to preserve these key
bindings, even at the cost of the consistency and added safety
introduced by the new bindings.  I actually did investigate whether I
could preserve these bindings by default and offer an option to allow
users to opt-in to the new bindings.

Unfortunately it turned out that, due to the added push-remote support
and the old inconsistencies, there is not just a single way in which
the old bindings could be preserved.  Furthermore the most useful
variant is also the most dangerous one.

So I decided play it safe and to instead go with the new, consistent
bindings by default and document the various ways in which the old
bindings can be restored.

### Restoring the old bindings

#### The compromise

I considered using the bindings established by the below code block by
default.  They constitute a compromise between the bindings which
actually ended up as the new default and the old same-key-twice bindings.

```lisp
(with-eval-after-load 'magit
  (magit-change-popup-key 'magit-fetch-popup  :action ?u ?f)
  (magit-change-popup-key 'magit-pull-popup   :action ?u ?F)
  (magit-change-popup-key 'magit-rebase-popup :action ?e ?r)
  (magit-change-popup-key 'magit-push-popup   :action ?p ?P))
```

The rule "<kbd>u</kbd> acts on **u**pstream, <kbd>p</kbd> acts on
**p**ush-remote, and <kbd>e</kbd> acts on **e**lsewhere" is followed;
*except* when the respective command is the most useful command, for
that command a same-key-twice binding is used instead.

But this does not actually *preserve* the old bindings.  With these
bindings in effect, some of these keys (<kbd>P P</kbd> and <kbd>r
r</kbd>) would do something *different* from what they used to do.
Instead this preserves the idea that pressing the same key twice
should call the most useful variant.

Because Magit now supports the *push-remote*, pushing to the upstream
is no longer the most useful push command.  On the contrary, except
for branches such as `master` and `maint`, pushing to the upstream
usually is a *mistake*.

This also fixes the inconsistency regarding <kbd>r r</kbd>.

The **2.1** release contained a change which took some users by
surprise and caused them to accidentally push to the wrong branch.  I
very much wanted to prevent something similar from happening again, so
this variant was off the table.  In my opinion the other variants
described below don't make much sense, so I went with the new, fully
consistent bindings.

#### Mostly default to upstream

If you would like the same-key-twice bindings to acted on the upstream
*even for pushing*, then do this:

```lisp
(with-eval-after-load 'magit
  (magit-change-popup-key 'magit-fetch-popup  :action ?u ?f)  ; 2x upstream
  (magit-change-popup-key 'magit-pull-popup   :action ?u ?F)  ; 2x upstream
  (magit-change-popup-key 'magit-rebase-popup :action ?e ?r)  ; 2x elsewhere
  (magit-change-popup-key 'magit-rebase-popup :action ?u ?l)  ; -- upstream
  (magit-change-popup-key 'magit-push-popup   :action ?u ?P)) ; 2x upstream
```

The <kbd>r r</kbd> inconsistency is faithfully preserved.

#### Always default to upstream

If you would like the same-key-twice bindings to *always* acted on the
upstream even for pushing *and rebasing*, then do this:

```lisp
(with-eval-after-load 'magit
  (magit-change-popup-key 'magit-fetch-popup  :action ?u ?f)  ; 2x upstream
  (magit-change-popup-key 'magit-pull-popup   :action ?u ?F)  ; 2x upstream
  (magit-change-popup-key 'magit-rebase-popup :action ?u ?r)  ; 2x upstream
  (magit-change-popup-key 'magit-push-popup   :action ?u ?P)) ; 2x upstream
```

This fixes the <kbd>r r</kbd> inconsistency.

#### Muscle memory is sacred

If you would like to undo each and every change to key bindings, then
do this:

```lisp
(with-eval-after-load 'magit
  (magit-change-popup-key 'magit-fetch-popup  :action ?u ?f)  ; upstream
  (magit-change-popup-key 'magit-fetch-popup  :action ?e ?o)  ; elsewhere
  (magit-change-popup-key 'magit-pull-popup   :action ?u ?F)  ; upstream
  (magit-change-popup-key 'magit-pull-popup   :action ?e ?o)  ; elsewhere
  (magit-change-popup-key 'magit-rebase-popup :action ?u ?l)  ; upstream
  (magit-change-popup-key 'magit-rebase-popup :action ?e ?r)  ; elsewhere
  (magit-change-popup-key 'magit-rebase-popup :action ?s ?o)  ; subset
  (magit-change-popup-key 'magit-rebase-popup :action ?m ?e)  ; edit
  (magit-change-popup-key 'magit-push-popup   :action ?p ?Q)  ; push-remote
  (magit-change-popup-key 'magit-push-popup   :action ?u ?P)) ; upstream
```

This faithfully restores all the old inconsistencies, and due to the
added push-remote support also adds a new inconsistency.

* <kbd>f p</kbd>, <kbd>F p</kbd>, and <kbd>r p</kbd> vs. <kbd>P Q</kbd> (push-remote)
* <kbd>f f</kbd>, <kbd>F F</kbd>, and <kbd>P P</kbd> vs. <kbd>r l</kbd> (upstream)
* <kbd>f o</kbd> and <kbd>F o</kbd> vs. <kbd>P e</kbd> vs. <kbd>r r</kbd> (elsewhere)

I also changed some bindings in the branch popup (but so far nobody
complained about that).  If you want to undo that too , then you also
need this:

```lisp
(with-eval-after-load 'magit
  (magit-change-popup-key 'magit-branch-popup :action ?c ?B)
  (magit-change-popup-key 'magit-branch-popup :action ?n ?c))
```

#### The nuclear option

If you don't want to make use of the push-remote support at all, then
do this:

```lisp
(with-eval-after-load 'magit
  (magit-remove-popup-key 'magit-branch-popup :variable ?p)
  (magit-remove-popup-key 'magit-branch-popup :variable ?\M-p)
  (magit-remove-popup-key 'magit-fetch-popup  :action ?p)     ; push-remote
  (magit-change-popup-key 'magit-fetch-popup  :action ?u ?f)  ; upstream
  (magit-change-popup-key 'magit-fetch-popup  :action ?e ?o)  ; elsewhere
  (magit-remove-popup-key 'magit-pull-popup   :action ?p)     ; push-remote
  (magit-change-popup-key 'magit-pull-popup   :action ?u ?F)  ; upstream
  (magit-change-popup-key 'magit-pull-popup   :action ?e ?o)  ; elsewhere
  (magit-remove-popup-key 'magit-rebase-popup :action ?p)     ; push-remote
  (magit-change-popup-key 'magit-rebase-popup :action ?u ?l)  ; upstream
  (magit-change-popup-key 'magit-rebase-popup :action ?e ?r)  ; elsewhere
  (magit-change-popup-key 'magit-rebase-popup :action ?s ?o)  ; subset
  (magit-change-popup-key 'magit-rebase-popup :action ?m ?e)  ; edit
  (magit-remove-popup-key 'magit-push-popup   :action ?p)     ; push-remote
  (magit-change-popup-key 'magit-push-popup   :action ?u ?P)) ; upstream
```
