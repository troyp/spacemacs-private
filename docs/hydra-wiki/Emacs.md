### Yank

Repeatable yank(-pop) command, with an option to switch to a list view using `helm` or
`browse-kill-ring`.

``` lisp
(defhydra hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev")
  ("l" helm-show-kill-ring "list" :color blue))   ; or browse-kill-ring
(global-set-key (kbd "M-y") #'hydra-yank-pop/yank-pop)
(global-set-key (kbd "C-y") #'hydra-yank-pop/yank)
```

### Movement

For many Emacs users, basic movement commands are [the most frequently used](http://ergoemacs.org/emacs/command-frequency.html)! Set up a movement group that means we don't need to hold the control key.

```lisp
(global-set-key
 (kbd "C-n")
 (defhydra hydra-move
   (:body-pre (next-line))
   "move"
   ("n" next-line)
   ("p" previous-line)
   ("f" forward-char)
   ("b" backward-char)
   ("a" beginning-of-line)
   ("e" move-end-of-line)
   ("v" scroll-up-command)
   ;; Converting M-v to V here by analogy.
   ("V" scroll-down-command)
   ("l" recenter-top-bottom)))
```

### Page Navigation

Repeatable page navigation commands, handy for large files with page
delimiters.

``` lisp
(defhydra hydra-page (ctl-x-map "" :pre (widen))
  "page"
  ("]" forward-page "next")
  ("[" backward-page "prev")
  ("n" narrow-to-page "narrow" :bind nil :exit t))
```

### Goto Line

Enhanced goto-line command:
 * Temporarily show line-numbers while the hydra is active
 * Use `m` to set the mark and `g` to jump to another line, i.e. to mark a range of lines by number.
 * Note that prefix arguments work, for example:
     *  `M-g 50 RET`  -- go to line 50
     *  `5 5 g` -- jump ahead to line 55
     *  `m` -- set the mark
     *  `6 5 g` -- extend the marked region to line 65
     *  `M-w` -- save the region to the kill-ring and exit the hydra

``` lisp
(defhydra hydra-goto-line (goto-map ""
                           :pre (linum-mode 1)
                           :post (linum-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))
```

### Move Text

Simple hydra to move current line (or region, if there is one) up and down. Requires
[`move-text.el`](http://www.emacswiki.org/emacs/move-text.el), which can be installed from MELPA.

``` lisp
(defhydra hydra-move-text ()
  "Move text"
  ("u" move-text-up "up")
  ("d" move-text-down "down"))
```

### Find file with <kbd>xf</kbd>
(Look ma, no modifiers!)

This example was proposed [here](http://emacs.stackexchange.com/a/9898/780) on Stack Exchange. It
binds the key sequence <kbd>xf</kbd> to `ido-find-file` while still allowing you to type "x"
normally for any other purpose. The hydra times out after half a second so if you really need to
type "xf" you can hit the <kbd>x</kbd>, wait a moment, and then proceed.

```lisp
(defun x-hydra-pre ()
  (insert "x")
  (let ((timer (timer-create)))
    (timer-set-time timer (timer-relative-time (current-time) 0.5))
    (timer-set-function timer 'hydra-keyboard-quit)
    (timer-activate timer)))

(defhydra x-hydra (:body-pre x-hydra-pre
                   :color blue
                   :hint nil)
  ("f" (progn (zap-to-char -1 ?x) (ido-find-file))))

(global-set-key "x" #'x-hydra/body)
```
### Outline minor mode

Outline minor mode keybingings are often repeated and hard to remember. This hydra solve it with the same keybindings. outline-minor-mode must be enabled.

```lisp
(defhydra hydra-outline (:color pink :hint nil)
  "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

"
  ;; Hide
  ("q" hide-sublevels)    ; Hide everything but the top-level headings
  ("t" hide-body)         ; Hide everything but headings (all body lines)
  ("o" hide-other)        ; Hide other branches
  ("c" hide-entry)        ; Hide this entry's body
  ("l" hide-leaves)       ; Hide body lines in this entry and sub-entries
  ("d" hide-subtree)      ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" show-all)          ; Show (expand) everything
  ("e" show-entry)        ; Show this heading's body
  ("i" show-children)     ; Show this heading's immediate child sub-headings
  ("k" show-branches)     ; Show all sub-headings under this heading
  ("s" show-subtree)      ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)                ; Up
  ("n" outline-next-visible-heading)      ; Next
  ("p" outline-previous-visible-heading)  ; Previous
  ("f" outline-forward-same-level)        ; Forward - same level
  ("b" outline-backward-same-level)       ; Backward - same level
  ("z" nil "leave"))

(global-set-key (kbd "C-c #") 'hydra-outline/body) ; by example
```

### Occur
This Hydra coupled by the saner occur defined [here](http://oremacs.com/2015/01/26/occur-dwim/) as well as some other customization allows:
* searching for the regexp
* navigating matches without leaving the occur buffer
* navigating using simple key strokes
* hiding the occur buffer
* re-attaching to the occur buffer in a split window

Keystrokes having meaning to occur are preserved and should work as per defaults. For example <kbd>e</kbd> should put you in ```occur-edit-mode```, <kbd>q</kbd> should quit the occur mode, etc.

Complete code is below:

```lisp
(defun occur-dwim ()
  "Call `occur' with a sane default, chosen as the thing under point or selected region"
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

;; Keeps focus on *Occur* window, even when when target is visited via RETURN key.
;; See hydra-occur-dwim for more options.
(defadvice occur-mode-goto-occurrence (after occur-mode-goto-occurrence-advice activate)
  (other-window 1)
  (hydra-occur-dwim/body))

;; Focus on *Occur* window right away.
(add-hook 'occur-hook (lambda () (other-window 1)))

(defun reattach-occur ()
  (if (get-buffer "*Occur*")
    (switch-to-buffer-other-window "*Occur*")
    (hydra-occur-dwim/body) ))

;; Used in conjunction with occur-mode-goto-occurrence-advice this helps keep
;; focus on the *Occur* window and hides upon request in case needed later.
(defhydra hydra-occur-dwim ()
  "Occur mode"
  ("o" occur-dwim "Start occur-dwim" :color red)
  ("j" occur-next "Next" :color red)
  ("k" occur-prev "Prev":color red)
  ("h" delete-window "Hide" :color blue)
  ("r" (reattach-occur) "Re-attach" :color red))

(global-set-key (kbd "C-x o") 'hydra-occur-dwim/body)
```

## Apropos

Emacs ships with many useful "Apropos" commands that [let you search for patterns](https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html). If you want to get into the habit of using these commands more often, the following Hydras might help:

```lisp
(defhydra hydra-apropos (:color blue)
  "Apropos"
  ("a" apropos "apropos")
  ("c" apropos-command "cmd")
  ("d" apropos-documentation "doc")
  ("e" apropos-value "val")
  ("l" apropos-library "lib")
  ("o" apropos-user-option "option")
  ("u" apropos-user-option "option")
  ("v" apropos-variable "var")
  ("i" info-apropos "info")
  ("t" tags-apropos "tags")
  ("z" hydra-customize-apropos/body "customize"))

(defhydra hydra-customize-apropos (:color blue)
  "Apropos (customize)"
  ("a" customize-apropos "apropos")
  ("f" customize-apropos-faces "faces")
  ("g" customize-apropos-groups "groups")
  ("o" customize-apropos-options "options"))
```
### Transpose
Many transpose options collected so they're easily accessible.
```lisp
    (global-set-key (kbd "C-c m")
    (defhydra hydra-transpose (:color red)
    "Transpose"
     ("c" transpose-chars "characters")
     ("w" transpose-words "words")
     ("o" org-transpose-words "Org mode words")
     ("l" transpose-lines "lines")
     ("s" transpose-sentences "sentences")
     ("e" org-transpose-elements "Org mode elements")
     ("p" transpose-paragraphs "paragraphs")
     ("t" org-table-transpose-table-at-point "Org mode table")
     ("q" nil "cancel" :color blue)))
```
