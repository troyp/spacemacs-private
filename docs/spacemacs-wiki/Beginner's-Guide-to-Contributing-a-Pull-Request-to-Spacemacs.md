This guide is mostly a reproduction of [@travisbhartwell](https://github.com/travisbhartwell)'s great [introduction post](http://iam.travishartwell.net/2015/07/29/how-to-make-a-pr-for-spacemacs/) on his blog. The primary changes pertain to Git branching strategy (and nomenclature) and the addition of `git` CLI commands. They're not meant to be used side-by-side and can be confusing if you do so. Please use either one or the other as a guide. (P.S. If reading such a long post is not your forte, he has also created a handy [video tutorial](https://www.youtube.com/watch?v=pWzJ6IGXwmk) covering the same topic.)

## How to do it

### Fork the Repository
Create the [fork](https://help.github.com/articles/fork-a-repo/) by visiting the Spacemacs [project page](https://github.com/syl20bnr/spacemacs/) on Github. If you happen to belong to [organizations](https://help.github.com/categories/organizations/) on Github, you will be prompted to select where to fork the repository; select your user in this case.

This will create a new repository for your user that is a mirror of the current state of the [upstream](https://help.github.com/articles/github-glossary/#upstream) repository.

Note/Copy the repository clone URL that is shown on the right-hand side of the page, part way down. This is needed when you clone your repository to your local filesystem in the next step.

### Cloning your forked repository
Next, you need to clone your fork of Spacemacs to a local directory (preferably `~/.emacs.d`). This can be done by typing `git clone git@github.com:<YOUR USERNAME>/spacemacs.git ~/.emacs.d` from the command line. Please ensure to backup your existing dot-emacs directory by typing `mv ~/.emacs.d ~/.emacs.d.bk`; just in case you decide to change your mind later.

### Adding upstream Spacemacs
You now need to add Spacemacs [upstream](https://github.com/syl20bnr/spacemacs) as an upstream to your clone.

1. Go to `magit-status` for the Spacemacs repository by doing `SPC u SPC g s` (_Hit the spacebar, then u, then the spacebar again, then g, then s_). Then enter the path of the Spacemacs repository, `~/.emacs.d`, and hit `RETURN`.

    If you're using the command line, open your Spacemacs directory by entering `cd ~/.emacs.d` and then hitting `RETURN`. Once you are in the Spacemacs directory, enter `git status` and then hit `RETURN` to know the current status (branch you are on, how many commits ahead or behind of `origin` you are etc.) of the repository.

2. Type `M a` to open the `magit` interface to add a new remote. Type the Remote name as `upstream` and Remote url as `git@github.com:syl20bnr/spacemacs.git` (or `https://github.com/syl20bnr/spacemacs.git` if you're using HTTPS). Type `n` when `magit` asks to set your `remote.pushDefault` to `upstream`.

    From the command line, type `git remote add -f upstream git@github.com:syl20bnr/spacemacs.git` to achieve the same effect as above.

### Updating your local repository
As it can happen from time-to-time, your fork of Spacemacs (which we will refer to as `origin`) can go out-of-sync with `upstream`. You need to ensure that you consistently keep updating from `upstream`.

1. Open `magit-status` for `~/emacs.d` using the same sequence of steps detailed in the above section.

2. Switch to the `develop` branch, if you are not already there. Hit `b b` and type `develop`. If you haven't previously checked out the `develop` branch, instead of `b b` (or, *branch checkout*), you will need to type `b c` (or, *create and checkout*). It will prompt you for the branch to base it off of, select or type `origin/develop`, hit `RETURN` and then name the branch `develop`. Note that any time in the `magit-status` buffer, if you want to know what commands are available, just hit `?`.

    From the command line, you can type `git checkout develop` to check out the `develop` branch. If you haven't checked it out before, type `git checkout -b develop origin/develop` to check out a local `develop` branch based off of the `origin/develop` branch.

3. After you have the `develop` branch checked out, update it against the remote `upstream` repository. Hit `F` for Pulling. Then type `r` to rebase. Even though we don't yet have any local commits, it makes sense to rebase any unpushed commits on the remote branch so that they are up to date and can be merged without merge commits. Hit `e` to pull and rebase from elsewhere. Type `upstream/develop` in the subsequent popup to pull rebase from `upstream`.

    From the command line, type `git pull --rebase upstream develop` to achieve the same effect as above.

    After this is done, we want to restart Emacs to run any of the latest changes. Hit `SPC q r` to restart Emacs.

4. You will also need to push the newly rebased changes to your fork (`origin` repository) on Github. To do this, type `P` in the `magit-status` buffer. Then type `u` to push to `origin/develop`.

    From the command line, type `git push origin develop` to achieve the same effect.

### Creating a branch for your changes

Before any changes are made, you need to make a branch to put them in. You could just commit to the `develop` branch, push your changes to your fork, and then do a pull request off of that. However, this is not recommended since it is common to have multiple pull requests open at one time. Therefore it is simpler to do a branch per pull request. This also allows your branches to have meaningful names, and makes updating pull requests with maintainer feedback a much simpler process.

1. From the `magit-status` buffer, hit `b c` (or, *create and checkout branch*).

2. Magit will then prompt you for the branch you are branching off of. Choose `develop`.

3. Next, it will prompt you for a branch name. Seeing as git doesn't restrict branch names to a particular length, you can go for long descriptive names (like `fix/spacemacs-home-buffer-bookmarks-jump`) if you so desire.

From the command line, type `git checkout -b fix/spacemacs-home-buffer-bookmarks-jump develop` to achieve the same result as above.

Check out the [Git book chapter on branching](http://git-scm.com/book/en/v2/Git-Branching-Branches-in-a-Nutshell) for an in-depth discussion of branching.

### Making, committing, and pushing changes

Now that we have a branch we can make your changes. Go ahead and make the changes you want. After you have tested and are satisfied with them, you are ready to commit and push.

1. Go to the `magit-status` buffer again: `SPC u SPC g s ~/.emacs.d RET`.

    On the command line, you can type `git status` to see a list of all the changes you have made to existing files or to see any new files that have been added.

2. Stage any unstaged changes you have made. Quickly: hit `s` on each line under *Unstaged changes* corresponding to a file you've changed. If you have added any new files, they will appear under *Untracked files*. Be sure to stage them as well by pressing `s`.

    From the command line, type `git add <FULL PATH TO FILE>` to stage the changes. You can then type `git diff --cached` to see a diff of all the staged changes to verify whether everything is in order.

3. Hit `c c` to commit all staged changes.

4. Type a meaningful commit message. (See [this post](http://chris.beams.io/posts/git-commit/) for a great explanation of what a good commit message is.) Helpfully, the diff of what you have changed is shown on the right-hand side for you to see what you did. Make the first line be a short subject describing the commit. Add a blank line and then in lines wrapped at 78, write a brief description of the changes and why. If you are fixing an issue that has been filed, include something along the lines of 'Fixes #2431'. Hit `C-c C-c` (*that's Control and c together, and then again*).

    On the command line, type `git commit` and press `RETURN`. This will open a Vim interface where you can enter your commit message as detailed above. Then you can press `:wq` to commit your changes.

5. Hit `P` (or, *Pushing*). Type `u` to set the `remote` branch and push. Start typing `origin/fix/spacemacs` and it will finish the branch name for you -- `origin/fix/spacemacs-home-buffer-bookmarks-jump`. Hit `RETURN`.

    From the command line, type `git push origin fix/spacemacs-home-buffer-bookmarks` to push your changes to your fork.

### Making the pull request

You are now ready to submit your pull request. Switch back to your browser to the page for your fork.

1. Github will show your recently pushed branches near the top, in a yellow box. Refresh your page if you don't see this. Click the green *"Compare & pull request"* button.

2. In the next screen, Github will prompt you for a branch to compare against. By default it will show *base: master*. You want to select `develop` here. It may take a minute for the page to update after you have selected the `develop` branch.

3. If you've done everything right, it will be able to cleanly merge your commit and will indicate so. It will then pull from your commit message the subject and description. If you've written a good commit message, there should be no need to add anything here. Click *Create pull request*.

Congratulations! There you go, you have created your first pull request. Hopefully the project developers will like your pull request and merge it.

### Updating the pull request

As is often the case, the maintainers might offer feedback on your pull request that will require changes to be made.

1. Go to the `magit-status` buffer again: `SPC u SPC g s ~/.emacs.d RET`.

2. Check out your existing `fix/spacemacs-home-buffer-bookmarks-jump` branch (or whatever it is you named your branch when creating it) by typing `b b` and then `fix/spacemacs-home-buffer-bookmarks-jump`.

3. Make your changes as you did before.

4. Instead of creating a new commit with the changes, you can amend the existing commit. First type `c` from the `magit-status` buffer. Then type `a` which will open up the same commit buffer as before. Your existing commit message should already be pre-filled in the buffer. You can choose to modify it or commit with the same message by typing `C-c C-c` as before.

    From the command line, type `git commit --amend` to achieve the same result as above.

5. It is advised to ensure your `fix/spacemacs-home-buffer-bookmarks-jump` branch is up-to-date with `upstream/develop` before updating the pull request. This can be done by typing `F` in the `magit-status` buffer, then typing `r`, followed by `e`. In the popup that appears, type `upstream/develop` to rebase your current branch with changes from `upstream/develop`. This will pull all changes from `upstream/develop`, first apply them locally, and then apply your most recent commit from the above step on top of it.

    From the command line, type `git pull --rebase upstream develop` for the same result.

6. Push your changes to `origin`. Type `P`, followed by `-force`, followed by `u` to push our changes. Note: Since we've updated an existing commit, we need to force push our changes. Otherwise, they will be rejected by Github.

   From the command line, type `git push -f origin fix/spacemacs-home-buffer-bookmarks-jump` for the same result.

Happy Hacking!
