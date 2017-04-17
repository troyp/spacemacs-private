Magit no longer provides a hook that is run in each buffer that visits a file that is being tracked in the current repository whenever it refreshes the current Magit buffer. It used to provide such a hook and made use of it itself, but that was highly inefficient and so it was removed.

Such a hook could be implemented as follows. But I am not adding this to Magit because I do not want to commit to this particular implementation. This implementation is optimized to make it more efficient. As a result it doesn't run the hook for every buffer that matches the above description, only for those "for which it makes sense". But your use case might require running it for all buffers.

```lisp
(defvar magit--modified-files nil)

(defun magit-maybe-cache-modified-files ()
  "Maybe save a list of modified files.
That list is later used by `magit-update-uncommitted-buffers',
provided it is a member of `magit-post-refresh-hook'.  If it is
not, then don't save anything here."
  (when (memq 'magit-update-uncommitted-buffers magit-post-refresh-hook)
    (setq magit--modified-files (magit-modified-files t))))

(add-hook 'magit-pre-refresh-hook #'magit-maybe-cache-modified-files)
(add-hook 'magit-pre-call-git-hook #'magit-maybe-cache-modified-files)
(add-hook 'magit-pre-start-git-hook #'magit-maybe-cache-modified-files)

(defun magit-update-uncommitted-buffers ()
  "Update some file-visiting buffers belonging to the current repository.
Run `magit-update-uncommitted-buffer-hook' for each buffer
which visits a file inside the current repository that had
uncommitted changes before running the current Magit command
and/or that does so now."
  (let ((topdir (magit-toplevel)))
    (dolist (file (delete-consecutive-dups
                   (sort (nconc (magit-modified-files t)
                                magit--modified-files)
                         #'string<)))
      (--when-let (find-buffer-visiting (expand-file-name file topdir))
        (with-current-buffer it
          (run-hooks 'magit-update-uncommitted-buffer-hook))))))

(add-hook 'magit-post-refresh-hook #'magit-update-uncommitted-buffers)
```

This is the implementation I arrived at when asked to provide such a hook for the benefit of `diff-hl`. In [this comment](https://github.com/magit/magit/pull/2530#issuecomment-177052830) on [#2530](https://github.com/magit/magit/pull/2530) I communicate my decision to not include this in Magit. An earlier implementation was discussed in [#2523](https://github.com/magit/magit/pull/2523). The discussion started in [#2491](https://github.com/magit/magit/pull/2491).

If you need such a hook, then you can copy the above implementation to your init file. You are doing so at your own risk. This will impact performance if there are many buffers (including buffers belonging to other repositories) and/or files tracked in the current repository.

## Updating VC's mode-line information

Emacs' own Version Control package, also known as VC, displays something like `Git-master` in the mode-line. When using Magit (but also when using VC I believe) this information is not always up to date.

The Magit FAQ has an [entry on the subject](https://github.com/magit/magit/blob/master/Documentation/magit.org#the-mode-line-information-isnt-always-up-to-date). If after reading that and the Info node it links to, you still want to ensure that the VC mode-line information is up-to-date and have also concluded that `(setq auto-revert-check-vc-info t)` is [too expensive](https://github.com/syl20bnr/spacemacs/issues/2172#issuecomment-162698272), then add the above code instead and also the below.

This approach has the advantage that it doesn't create a constant load on the cpu. Instead you will likely get a noticeable spike every time you run a Magit command. 

```lisp
(add-hook 'magit-update-uncommitted-buffer-hook 'vc-find-file-hook)
```

Or if you use Emacs >= `25.1`, then use:

```lisp
(add-hook 'magit-update-uncommitted-buffer-hook 'vc-refresh-state)
```

Or you could use an alternative trimmed down implementation I wrote some time ago:

```lisp
(defun magit-refresh-vc-mode-line ()
  "Update the information displayed by `vc-mode' in the mode-line.
Like `vc-mode-line' but simpler, more efficient, and less buggy."
  (setq vc-mode
        (if vc-display-status
            (magit-with-toplevel
              (let* ((rev (or (magit-get-current-branch)
                              (magit-rev-parse "--short" "HEAD")))
                     (msg (cl-letf (((symbol-function #'vc-working-revision)
                                     (lambda (&rest _) rev)))
                            (vc-default-mode-line-string
                             'Git buffer-file-name))))
                (propertize
                 (concat " " msg)
                 'mouse-face 'mode-line-highlight
                 'help-echo (concat (get-text-property 0 'help-echo msg)
                                    "\nCurrent revision: " rev
                                    "\nmouse-1: Version Control menu")
                 'local-map vc-mode-line-map)))
          " Git"))
  (force-mode-line-update))

(add-hook 'magit-update-uncommitted-buffer-hook 'magit-refresh-vc-mode-line )
```

However I don't know whether that still works and whether the claims made in the doc-string are (still) correct. Also I think that `vc-refresh-state` now uses colors. The above snippet has not been adjusted accordingly.