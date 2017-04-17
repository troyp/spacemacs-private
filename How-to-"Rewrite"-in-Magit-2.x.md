This tutorial was contributed by a user.  For an official tutorial see
[[History Manipulation|History Manipulation]].

### What *is* Rewrite, actually?

The [Rewrite](https://magit.vc/manual/1.4/Magit/Rewriting.html#Rewriting) feature in 1.x was exquisitely useful to some users and incomprehensible to others. It enabled users to rewrite commit history, working incrementally to clean up their history commit by commit, by moving hunks around between commits, eliminating or creating new interim commits, rewording commit messages, etc'.  You could describe it as a wacky mix of `git rebase -i`, `git add -p`, `git commit -a`, `git cherry-pick` and perhaps a few other things.

### Sounds great, so what happened?

"Rewrite" was removed from v2.x (See [#966](https://github.com/Magit/Magit/issues/2330)) as part of a large scale rewrite of Magit and replaced by enhanced facilities in what became Magit v2.x. In general "Rewrite" has been subsumed into "Rebase Interactive" but in many cases you can now simply use the new commands added
to the "Rebase" popup (Reword, Extend, Instant/Fixup etc') directly, without resorting to rewrite.

### `git rebase -i`? I use that all the time...

Magit's "Rebase Interactive" should be familiar to you If that's the case, but it also has a few new tricks
up its sleeves as well. Like many powerful tools it seem intimidating at first but, trust me, if you use
git often (and of course, you do) there are few things that level up your abilities as mastering this skill. If you've never learned to use `git rebase -i` on the command line, let alone magit's powerful version of it, you're really missing out on the good things in (coding) life. Make the effort, it'll be worth it.

Instead of pointing out the differences between v1.x's "Rewrite" and Magit 2.x new and improved "Interactive rebase", we'll just have ourselves a little tutorial on how to things work now. By the end of it you should not only have a firm grasp on how to use v2.x "Rebase Interactive" to great effect,  but if you're a long time user of "Rewrite" you'll probably agree that Magit 2.x is just as capable, and in some ways more so.

### Show Me How It Works

Using "Rebase interactive" in Magit, looks something like this:

1. From the Rebase pop up you select "Rebase interactive".
2. This opens the log view where you select (using `.`) the base commit. Only the commits from the base commit to HEAD will be included in the rebase.
3. The next step is the appearance of `git-rebase-todo` buffer, where you can perform actions on individual commits (but, not on their contents). You can rearrange commits, mark them for editing, amending, squash, fixup, etc'.  This corresponds exactly to the use of `git rebase -i` on the command-line. If you do not mark
any commits for further editing, Hitting C-c C-c will execute the rebase and you're done.  
**Note**: Admirers of V1.x's "Rewrite", for ultimate control simply mark all the commits for "edit"
and do all your work in the next step.
4. If you've marked some commits for editing (of some kind), the status buffer is now switched to interactive rebase mode. You will see a list of the commits included in the rebase and you can now proceed to rewrite
the history with a great amount of control.
**Note**: Admirers of V1.x's "Rewrite", this is where you want to be.
5. At any point in the rebase, you can hit "r" to open `magit-rebase-popup` where you can tell the rebase
to continue or abort, just as you would on the command line.
6. The list of commits in the rebase is now displayed, and each commit has a status attached to it: `onto`/`same`/`edit`/etc'. `onto` is the commit onto which you're rebasing, it will not be modified.
`same` indicates a commit that hasn't been altered. Remember, we can edit the existing commits (hunks,messages, ordering, etc') as we create the branch's new history. The old `Rewrite` feature 
let you do this as well, but it is pretty much equivalent to marking commits for "edit" in "git rebase -i".
7. Positioning point on a commit and hitting `A A`(cherry-pick) will cherry pick that commit (after confirming the selection) without changes.
8. Positioning point on a commit and hitting `A a` (cherry apply) will only apply the commit (after confirming the selection) to the working tree. Once you have uncommitted changed in your working tree, the familiar `un/staged changes` sections will appear, and you're now free to stage hunks and create commits as usual in whatever order you like. This allows you to break up a commit into smaller commits. When composing the commit message, You can cycle through the commit messages in the rebase commit range using `M-p` and `M-n`.  You can also edit files as usual while you're doing this, and include those changes in your new commits as well.
9. You can use `A a` multiple times to gather the changes in your working directory, and select hunks which originate in separate commits, so you can also merge parts of commits to form a new hybrid one.
10. What if you want to *remove* something from a commit? Sure, you can "A a" (cherry apply) the commit to the working tree, stage everything *except* that one bit, and make a new commit - but that's not very pleasant. What would be really nice is you could Cherry Pick a commit first, and then simply "subtract" something from it, removing it from the commit while keeping the changes in your working directory for you to do something else with. **Enter (magit-reverse)**.
10. Magit now let's you "Uncommit" parts of a commit with the `magit-reverse` command. In fact it works on commits (And ranges) too, but we'll focused on hunks here. Currently (Magit v2.2.2) there is no `magit-reverse` command in the `V` (magit-revert)  popup, but it looks like this will be added in a coming version. 
11. To "Uncommit" a hunk from a cherry-picked commit, position point on it and hit `RET` to open the `revision buffer`, where you can see the hunks included in that commit. Position point on the hunk you wish to Uncommit (It works with region selection too!) and execute `M-x magit-reverse`, confirm and Magit will
"reverse" the hunk for you. What does that mean? well, the commit is unchanged at this point. The working
directort is unchanged is well (the hunk is still present), but the index now contains an "Anti-Hunk", if you will. That is, the "undoing" of that hunk from the commit has been staged for you. If you hit "C e" (Commit extend) Magit will apply that change to the commit. The end result? The commit no longer contains that hunk,
but it still exists as an "Unstaged changed" for you to do what you will with it. Magic!
12. Once you're done playing, Hit "r r" (rebase continue) until the rebase is finished. Congratulations! You've mastered an Advanced (Ma)git Skill!
