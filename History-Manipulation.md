Magit now fully supports the sequence operations `git
(rebase|cherry-pick|revert|am) ...`, and implements several variants
of `git commit` which can not only be used to manipulate the commit at
the tip of a branch but also a detached `HEAD` during a rebase.

When any of these sequence operation is in progress then the status
buffer displays the yet-to-be-applied (and in case of rebase the
already-applied commits).  The equivalent is also done for stopped
merges.  Multiple commits can be selected using the region, where
appropriate.  Magit also supports the creation of patches (which can
then be applied again using `git am`).

The various "apply variants"
`magit-{apply,stage,unstage,discard,reverse}{-*}` also have been
carefully redesigned from the ground up.  Most importantly the
distinction between what it means to "discard" ("remove (staged or
unstaged) change", if staged then remove from index *and* worktree ),
"reverse" ("remove from working tree only", if staged keep it staged)
and "revert" (the inverse of cherry-pick, therefor similar to reverse
but limited to commits) are now drawn consistently.

While documentation is still sparse, the `next` branch is now stable
enough for everyone who can tolerate some fluctuation.  Some workflow
documentation follows below.

----

### Important entry points related to history manipulation

* `a`: apply changes at point (including commits)
* `A`: Start, continue etc. cherry-pick
* `c`: Various commit variants
* `r`: Start, continue etc. rebase
* `k`: discard change at point
* `v` on non-commit: reverse change at point
* `v` on commit: revert but don't commit the commit at point or
  commits in region [1]
* `V`: Start, continue etc. revert
* `w`: Start, continue etc. applying patches (from files or mailbox)
* `W`: create patch files
* `x`: reset to commit (`C-u x` hard reset)

[1] Here the distinction between "revert" and "reverse" blurs a bit.
Both terms would make sense, but for consistency with Git (`git revert
--no-commit`) we use "revert" even though we use the same key binding
as the one which generally does "reverse".

When there is no "change" (i.e. "hunk", "several hunks", or in some
cases commits) at point the above keys might perform a different,
though related, operation.  I.e. we reserve the term "discard" for
operations on hunks, but `k` on a branch does something similar, it
*deletes* that branch.

----

### Workflow issues

Two concrete workflow issue have been raised.

**When interactive rebase stops at a commit, then it does so with that commit already committed.**

When Magit stops at a commit during a sequence action, either because
that was requested by the user or due to a conflict, then the cursor
is automatically places on the stopped at commit.  So in order to
reset to the previous commit, all you have to do is this: `n x RET`.

Resetting to `HEAD~` causes the message of `HEAD` to be saved in the
commit message ring, which can be accessed in the commit message
buffer using `M-p`/`M-n`.  So you can do this without making it hard
to get the commit message back, just use (`M-p`) in the commit buffer.

This is the brute-force approach. In most cases I do not recommend it.

----

**Getting accidentally included changes out of a commit.**

This can be divided into two steps:

1. Getting changes out of `HEAD`
2. Make some commit the `HEAD` commit

Lets assume the commit in question is already the `HEAD`.  Here rebase
does not even come into play but we do make use of the improved "apply
variant" commands.

There are two variants: some change made it into the commit which (a)
should have been committed later, or (b) should have been committed
before.

*(a) Move change after original commit.*

* Show the commit in the revision buffer.
* Move to the change in question and use `u` to reverse it in the index only (requires `magit-unstage-committed` to be non-nil, the default)

At this point the index contains the reversal and the working tree
the reversal of the reversal. If you unstage the staged changes you
would end up with an unmodified working tree.

* Now amend the staged changes (the partial reversal) to `HEAD` using `c e`.
* And finally stage and then commit the extracted changes, or stash
  them for later.

*(b) Move change before original commit; brute-force.*

As we will see later we could use interactive rebase, but using the
brute-force variant actually works pretty well here, so let's do that.

* Soft reset to `HEAD~` using `x HEAD~ RET`.
* Stage `s` the changes that should be "committed before".
* Commit using a new message.
* Stage `S` the remaining changes which should be "part of 'the'
  commit".
* Initiate the commit, press `M-p` to get to the original message,
  finish.

*How to stop worrying and learn to love the detached HEAD.*


If you don't notice your mistake until a few commits later, then you
have to use interactive rebase.  The most generic variant is `r i`
which start by showing the recent commits in a buffer.  One can then
set the actions for each commit individually.  This also allows
reordering commits etc.  But we only want to modify a single commit,
so we use the "edit single commit" variant.

* Move to the commit which has to be split, most conveniently in the
  "Unpushed commits" section.
* Initiate the edit `r m`.
* The commit in question is now the `HEAD` commit.
* Manipulate it as above.
* When done, continue with `r r`, i.e. replay the commits "between
  here and the tip".

[This is in part a response to claims that rebasing in Magit is modal
and that this is a very bad thing.]

That's it.  This only differs from manipulating the tip of a branch in
that this tool allows manipulating another commit *as if* it were the
tip of a branch. It is "modal" in the sense that it keeps track of the
commits that "have to be re-played once the user is done, in order to
get back to the tip". It is *not* modal in that you can do whatever
before doing the "re-playing the rest" -- you could branch, merge,
revert, cherry-pick, shut down the computer, go on vacation -
everything except invoking another rebase -- rebase doesn't nest. You
could even decide that the currently detached `HEAD` should become the
tip of a new branch, and that the branch you started with should stay
as is. The only complication is that until you either "continue" or
"abort" the `HEAD` is a detached `HEAD`, which, as implied by the
title of this section, is something that is not worth worrying about.

![dr-strangelove-still-580](https://cloud.githubusercontent.com/assets/25046/4781215/8f42c040-5c90-11e4-9c6d-2925077ef02a.jpg)

*(b') Move change before original commit; like a ninja.*

This is a variant of (b) above, the goal is to do the same without
reverting `x`.

* Initiate an edit `r m` of "the commit (C-1) before the commit we
  actually want to edit (C)".
* C-1 is now the `HEAD`, it's already applied.  C is the first commit
  that is not yet applied.
* Move to C in the "Rebasing" section, show it `RET`.
* Move to the change that should be committed in between C-1 and C,
  apply it `a`.
* Back in the status buffer, stage the result `s`/`S`.
* Create a new commit.
* Continue with `r r`.
