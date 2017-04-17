Sometimes a user suggests that some infix argument or suffix command be added to a popup and the maintainer decides otherwise, usually because he feels that would not serve all users well. In other case he might just want some time to consider the proposal, or would like to delay adding the argument or command, until some preparations are done. **But fear not, you can usually just add it yourself.**

How that is done is described in the [Magit-Popup manual](https://magit.vc/manual/magit-popup) in the node [Customizing existing popups](https://magit.vc/manual/magit-popup/Customizing-existing-popups.html).

When a suggestion is rejected, then the user is usually encouraged to add an entry here. These entries serve as examples and you might even discover a suggested binding that you like but didn't think of yourself.

### `magit-log-popup`
#### add argument `--no-merges`

```lisp
(magit-define-popup-switch 'magit-log-popup
  ?m "Omit merge commits" "--no-merges")
```

Proposed in https://github.com/magit/magit/issues/2137.

#### add argument `--date-order`

```lisp
(magit-define-popup-switch
  'magit-log-popup
  ?s "Always sort by date" "--date-order")

;; (add-to-list 'magit-log-arguments "--date-order")
```

The commented line would add the flag to the list of default arguments; you can use the switch to turn it off then.

Proposed only here.

#### add arguments `--since` and `--until`

This prompts for dates to filter the log using [`org-read-date`](http://doc.endlessparentheses.com/Fun/org-read-date.html) calendar support.

```emacs-lisp
(autoload 'org-read-date "org")

(defun magit-org-read-date (prompt &optional _default)
  (org-read-date 'with-time nil nil prompt))

(magit-define-popup-option 'magit-log-popup
  ?s "Since date" "--since=" #'magit-org-read-date)

(magit-define-popup-option 'magit-log-popup
  ?u "Until date" "--until=" #'magit-org-read-date)
```

Proposed only here.

#### add argument `--first-parent`

```lisp
(magit-define-popup-switch
  'magit-log-popup
  ?1 "First parent" "--first-parent")
```

Proposed in https://github.com/magit/magit/pull/2538.

#### add an action to view the wip log

```lisp
(magit-define-popup-action 'magit-log-popup
  ?w "Wip" 'magit-wip-log-current)
```

This displays a log of wip saves created with one of the Magit wip modes.  Note that none of these modes is currently active by default, you must enable them explicitly in your config.

Proposed in https://github.com/magit/magit/issues/2151.

### `magit-commit-popup`
#### add argument `--date=`

```lisp
(magit-define-popup-option 'magit-commit-popup
  ?D "Override the author date" "--date=" #'read-from-minibuffer)
```

Proposed in https://github.com/magit/magit/issues/2307.

### `magit-dispatch-popup`
#### add command `magit-reset`

```lisp
(magit-define-popup-action 'magit-dispatch-popup
  ?x "Reset" 'magit-reset ?!)
```

Proposed in https://github.com/magit/magit/issues/2141.

### `magit-patch-popup`
#### add argument `--cover-letter`

```lisp
(magit-define-popup-switch 'magit-patch-popup
  ?c "Generate cover letter" "--cover-letter")
```

Proposed in https://github.com/magit/magit/pull/2233.

### `magit-branch-popup`
#### add command `magit-branch-orphan`

```lisp
(magit-define-popup-action 'magit-branch-popup
  ?o "Checkout new orphan branch" 'magit-branch-orphan)
```

Proposed in https://github.com/magit/magit/pull/2030.

### `magit-push-popup`
#### add argument --follow-tags

```lisp
(magit-define-popup-switch 'magit-push-popup
  ?t "Follow tags" "--follow-tags")
```

Proposed in https://github.com/magit/magit/pull/2919.
