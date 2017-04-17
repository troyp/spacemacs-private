This page is dedicated to tips n' tricks. Feel free to add you own.

## Miscellaneous

### Ignoring whitespace changes in status diffs

<kbd>D -b g</kbd> toggles the `--ignore-space-change` switch in the
`magit-diff-refresh-popup`.

### Jump to status buffer while using ido to open a file

Similar to how it's possible to instead open a directory in dired while
completing a file name (using `C-d`) you can also instead bring up the
appropriate Magit status buffer.  This does require some setup, see
`ido-enter-magit-status`'s doc-string.

## Performance hints

Also see the performance hints [in the manual](http://magit.vc/manual/magit/Performance.html).

### Performance with virtualbox-mounted directories is bad

According to [#2108](https://github.com/magit/magit/issues/2108) Git
has the same issue and using ext2 helps

## Workflow tips

### History manipulation workflows

See [History Manipulation](https://github.com/magit/magit/wiki/History-Manipulation).
See [How to "Rewrite" in Magit 2.x](https://github.com/magit/magit/wiki/How-to-%22Rewrite%22-in-Magit-2.x)

## Useful code snippets

### Show staged and unstaged changes, but nothing else

To get a buffer with just the staged and unstaged changes, but not all
the other things displayed in the status buffer, use this:

```lisp
(define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
  "Mode for showing staged and unstaged changes."
  :group 'magit-status)

(defun magit-staging-refresh-buffer ()
  (magit-insert-section (status)
    (magit-insert-unstaged-changes)
    (magit-insert-staged-changes)))

(defun magit-staging ()
  (interactive)
  (magit-mode-setup #'magit-staging-mode))
```

Also see https://github.com/magit/magit/issues/2219.

### Ask for confirmation before pushing to origin/master

The convenient keybindings for magit actions can make it awfully easy
to unintentionally do something bad that you would never do if you had
to type out the git commands, and pushing to upstream -- typically
`origin/master` -- isn't always easy to recover from.

The following advice adds a `yes-or-no-p` query to the
`magit-push-current-to-upstream` command (i.e. the `P u` binding). If
you generally want to push to your push remote, and only occasionally
want to push to upstream, you may find this a convenient safety net.


```
;; Protect against accident pushes to upstream
(defadvice magit-push-current-to-upstream
    (around my-protect-accidental-magit-push-current-to-upstream)
  "Protect against accidental push to upstream.

Causes `magit-git-push' to ask the user for confirmation first."
  (let ((my-magit-ask-before-push t))
    ad-do-it))

(defadvice magit-git-push (around my-protect-accidental-magit-git-push)
  "Maybe ask the user for confirmation before pushing.

Advice to `magit-push-current-to-upstream' triggers this query."
  (if (bound-and-true-p my-magit-ask-before-push)
      ;; Arglist is (BRANCH TARGET ARGS)
      (if (yes-or-no-p (format "Push %s branch upstream to %s? "
                               (ad-get-arg 0) (ad-get-arg 1)))
          ad-do-it
        (error "Push to upstream aborted by user"))
    ad-do-it))

(ad-activate 'magit-push-current-to-upstream)
(ad-activate 'magit-git-push)
```

### Cycle margin visibility

```lisp
(defun magit-cycle-margin ()
  "Cycle visibility of the Magit margin.

,-> show with details --> show no details -- hide -.
`--------------------------------------------------'"
  (interactive)
  (if (not (magit-margin-option))
      (user-error "Magit margin isn't supported in this buffer")
    (pcase (list (nth 0 magit-buffer-margin)
                 (and (nth 3 magit-buffer-margin) t))
      (`(t t)
       (setf (nth 3 magit-buffer-margin) nil)
       (magit-set-buffer-margin nil t))
      (`(t nil)
       (setf (nth 0 magit-buffer-margin) nil)
       (magit-set-buffer-margin))
      (`(nil ,_)
       (setf (nth 0 magit-buffer-margin) t)
       (setf (nth 3 magit-buffer-margin) t)
       (magit-set-buffer-margin nil t)))))
```
