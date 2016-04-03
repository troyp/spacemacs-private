;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     ;; better-defaults
     emacs-lisp
     git
     markdown
     (org :variables
             org-enable-github-support t)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     haskell
     c-c++
     html
     javascript
     extra-langs
     lua
     python
     troyp
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place you code here."

  ;; ********************
  ;; *                  *
  ;; * SPACEMACS CONFIG *
  ;; *                  *
  ;; ********************

  ;; (setq-default tab-always-indent t)
  (global-linum-mode)
  (menu-bar-mode)

  ;; not working?
  (setq-default evil-esc-delay 0.00001)

  (global-set-key (kbd "<f1>") 'describe-prefix-bindings)
  (global-set-key (kbd "<C-f1>") 'describe-prefix-bindings)

  (global-set-key [C-tab] 'next-multiframe-window)
  (global-set-key [C-S-iso-lefttab] 'previous-multiframe-window)
  ;; change C-x - from 'shrink-window-if-larger-than-buffer to 'fit-window-to-buffer
  (global-set-key (kbd "\C-x -") 'fit-window-to-buffer)

  (global-set-key (kbd "M-n") 'evil-scroll-line-down)
  (global-set-key (kbd "M-p") 'evil-scroll-line-up)
  (global-set-key (kbd "C-S-j")
                  (lambda () (interactive) (scroll-other-window-down 1)))
  (global-set-key (kbd "C-S-k")
                  (lambda () (interactive) (scroll-other-window-down -1)))

  (global-set-key "\C-a" 'move-beginning-of-line-or-text)    ;; troyp/utils.el
  (global-set-key (kbd "<C-return>") 'open-line-below)       ;; troyp/utils.el
  (global-set-key (kbd "<C-S-return>") 'open-line-above)     ;; troyp/utils.el
  (global-set-key [\C-\S-up] 'move-text-up)
  (global-set-key [\C-\S-down] 'move-text-down)
  (global-set-key (kbd "M-S-SPC") 'just-one-space)

  (global-set-key [\M-f12] 'shell-pop)

  ;; -------------------------------------------------------------------------------
  ;; ,-------,
  ;; | Regex |
  ;; '-------'

  ;; modified emacs source: GPL3
  (defun pcre-occur (regexp &optional nlines)
    "Show all lines in the current buffer containing a match for REGEXP.
If a match spreads across multiple lines, all those lines are shown.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*Occur*'.
It serves as a menu to find any of the occurrences in this buffer.
\\<occur-mode-map>\\[describe-mode] in that buffer will explain how.

If REGEXP contains upper case characters (excluding those preceded by `\\')
and `search-upper-case' is non-nil, the matching is case-sensitive.

When NLINES is a string or when the function is called
interactively with prefix argument without a number (`C-u' alone
as prefix) the matching strings are collected into the `*Occur*'
buffer by using NLINES as a replacement regexp.  NLINES may
contain \\& and \\N which convention follows `replace-match'.
For example, providing \"defun\\s +\\(\\S +\\)\" for REGEXP and
\"\\1\" for NLINES collects all the function names in a lisp
program.  When there is no parenthesized subexpressions in REGEXP
the entire match is collected.  In any case the searched buffer
is not modified."
    (interactive (occur-read-primary-args))
    (occur-1 (pcre-to-elisp regexp) nlines (list (current-buffer))))

  ;; modified emacs source: GPL3
  (defun pcre-multi-occur (bufs regexp &optional nlines)
    "Show all lines in buffers BUFS containing a match for REGEXP.
This function acts on multiple buffers; otherwise, it is exactly like
`occur'.  When you invoke this command interactively, you must specify
the buffer names that you want, one by one.
See also `multi-occur-in-matching-buffers'."
    (interactive
     (cons
      (let* ((bufs (list (read-buffer "First buffer to search: "
                                      (current-buffer) t)))
             (buf nil)
             (ido-ignore-item-temp-list bufs))
        (while (not (string-equal
                     (setq buf (read-buffer
                                (if (eq read-buffer-function 'ido-read-buffer)
                                    "Next buffer to search (C-j to end): "
                                  "Next buffer to search (RET to end): ")
                                nil t))
                     ""))
          (add-to-list 'bufs buf)
          (setq ido-ignore-item-temp-list bufs))
        (nreverse (mapcar #'get-buffer bufs)))
      (occur-read-primary-args)))
    (occur-1 (pcre-to-elisp regexp) nlines bufs))

  ;; *************
  ;; *           *
  ;; * EVIL-MODE *
  ;; *           *
  ;; *************

  ;; (global-set-key [f9] 'evil-mode)

  ;; -----------------------
  ;; evil-symbol-word-search
  ;; -----------------------
  ;; Use symbols rather than words for * and # search
  ;; eg. in lisp modes, this will not stop at dashes, etc.
  ;; note: for symbol-based text-objects, use "io" ('evil-inner-symbol)
  ;;       rather than "iw" / "iW" ('evil-inner-word / 'evil-inner-WORD)
  (setq-default evil-symbol-word-search t)
  (defun toggle-evil-symbol-word-search ()
    (interactive)
    (setf evil-symbol-word-search (not evil-symbol-word-search)))
  (defalias 'evsw 'toggle-evil-symbol-word-search)
  (define-key evil-normal-state-map (kbd "C-*") #'toggle-evil-symbol-word-search)

  ;; -------------------------------------------------------------------------------
  ;; ,-------------------------,
  ;; | Evil State Key Bindings |
  ;; '-------------------------'
  ;;
  ;; ,--------------,
  ;; | NORMAL STATE |
  ;; '--------------'
  (define-key evil-normal-state-map [delete] #'kill-this-buffer)
  (defun insert-space () (interactive) (insert ? ))
  (define-key evil-normal-state-map (kbd "S-SPC") #'insert-space)
  ;; shift reverses C-d (-scroll-down) and C-o (-jump-backward)
  (define-key evil-normal-state-map (kbd "C-S-d") #'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-S-o") #'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "C-e") #'end-of-line)
  ;; remove C-y (use global M-p)
  (define-key evil-normal-state-map (kbd "C-y") nil)
  ;; reverse gu and gU
  (define-key evil-normal-state-map (kbd "gu") #'evil-upcase)
  (define-key evil-normal-state-map (kbd "gU") #'evil-downcase)
  ;; provide evil-repeat-find-char-reverse binding
  (define-key evil-normal-state-map (kbd "M-;") #'evil-repeat-find-char-reverse)
  ;; [r and ]r move to beginning and end of region
  (define-key evil-normal-state-map (kbd "[r") #'evil-visual-jump-to-region-beginning)
  (define-key evil-normal-state-map (kbd "]r") #'evil-visual-jump-to-region-end)
  (define-key evil-normal-state-map (kbd "M-RET RET") #'lisp-state-toggle-lisp-state)

  ;; ,--------------,
  ;; | VISUAL STATE |
  ;; '--------------'
  (defun insert-space-visual () (interactive) (execute-kbd-macro " ") (evil-visual-restore))
  (define-key evil-visual-state-map (kbd "S-SPC") #'insert-space-visual)
  (define-key evil-visual-state-map (kbd "C-SPC") #'evil-forward-char-or-extend)
  (define-key evil-visual-state-map (kbd "C-\\") #'shell-command-replace-region)
  (define-key evil-visual-state-map (kbd "M-u") #'evil-upcase)
  (define-key evil-visual-state-map (kbd "M-l") #'evil-downcase)

  ;; ,--------------,
  ;; | MOTION STATE |
  ;; '--------------'
  (define-key evil-motion-state-map (kbd "C-e") #'end-of-line)

  (define-key evil-motion-state-map (kbd "[") #'evil-motion-open-bracket-prefix-map)
  (define-key evil-motion-state-map (kbd "]") #'evil-motion-close-bracket-prefix-map)
  (define-prefix-command 'evil-motion-open-bracket-prefix-map)
  (define-prefix-command 'evil-motion-close-bracket-prefix-map)
  (define-key 'evil-motion-open-bracket-prefix-map "{" 'evil-previous-open-brace)
  (define-key 'evil-motion-open-bracket-prefix-map "(" 'evil-previous-open-paren)
  (define-key 'evil-motion-open-bracket-prefix-map "]" 'evil-backward-section-end)
  (define-key 'evil-motion-open-bracket-prefix-map "[" 'evil-backward-section-begin)
  (define-key 'evil-motion-open-bracket-prefix-map "b" "T(")
  (define-key 'evil-motion-open-bracket-prefix-map "B" "T)")
  (define-key 'evil-motion-close-bracket-prefix-map "{" 'evil-next-close-brace)
  (define-key 'evil-motion-close-bracket-prefix-map "(" 'evil-next-close-paren)
  (define-key 'evil-motion-close-bracket-prefix-map "[" 'evil-forward-section-end)
  (define-key 'evil-motion-close-bracket-prefix-map "]" 'evil-forward-section-begin)
  (define-key 'evil-motion-close-bracket-prefix-map "b" "t)")
  (define-key 'evil-motion-close-bracket-prefix-map "B" "t(")

;; ,--------------,
;; | INSERT STATE |
;; '--------------'
  (define-key evil-insert-state-map (kbd "C-S-a") #'evil-paste-last-insertion)
  (define-key evil-insert-state-map (kbd "C-a") #'beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") #'end-of-line)
  (define-key evil-insert-state-map (kbd "C-S-y") #'evil-copy-from-below)
  (define-key evil-insert-state-map (kbd "C-l") #'delete-char)
  (define-key evil-insert-state-map (kbd "C-S-l") #'backward-delete-char)
  (define-key evil-insert-state-map (kbd "C-S-k") #'kill-line)
  (define-key evil-insert-state-map (kbd "C-.") 'yas-expand)
  (define-key evil-insert-state-map (kbd "C-n") #'next-line)
  (define-key evil-insert-state-map (kbd "C-p") #'previous-line)
  ;; (define-key evil-insert-state-map (kbd "C-M-SPC") #'hippie-expand)

  ;; unicode insertion
  (define-key evil-insert-state-map (kbd "C-v") #'insert-char)
  (define-key evil-insert-state-map (kbd "M-v") #'iso-transl-ctl-x-8-map)
  (define-key evil-insert-state-map (kbd "C-k") #'evil-insert-digraph)

  ;; -------------------------------------------------------------------------------
  ;; ,----------------------,
  ;; | Evil Leader Bindings |
  ;; '----------------------'
  ;; can use bind-keys to define prefix maps (Leader map is 'spacemacs-cmds, see below)

  (evil-leader/set-key
    "b C-e"      'bury-buffer
    "b <insert>" 'buffer-major-mode
    "En"         'spacemacs/next-error
    "EN"         'spacemacs/previous-error
    "Ep"         'spacemacs/previous-error
    "<delete>"   'kill-buffer-and-window
    "RR"         'pcre-multi-occur
    "Rr"         'pcre-occur
    "oa"         'asciiheadings-prefix-key-map
    "oc"         'character-prefix-map
    "om"         'mode-ring-prefix-key-map
    "ov"         'variable-pitch-mode
    "<backtab>"  'switch-to-most-recent-buffer
    "<return>"   'helm-buffers-list
    )

  (bind-keys :map spacemacs-cmds
             :prefix-map character-prefix-map
             :prefix "o c"
             :prefix-docstring "Commands that act on the character at point."
             ("i" . insert-char)
             ("=" . describe-char)
             ("a" . what-cursor-position)
             ("p" . palette-foreground-at-point)  ;; palette.el (dadams)
             ("f" . get-char-face)
             )

  ;; -------------------------------------------------------------------------------
  ;; ,-------------------------,
  ;; | which-key Configuration |
  ;; '-------------------------'

  (which-key-add-key-based-replacements
    "C-x r"        "rectangle"
    "C-x 4"        "other window"
    "C-x 5"        "frame"
    "C-x 8"        "unicode"
    "C-x 8 SPC"    "no-break space"
    "C-x 8 \""     "äÄëËïÏöÖßüÜÿ"
    "C-x 8 '"      "áÁéÉíÍóÓúÚýÝ"
    "C-x 8 *"      "¡±­·«¯»¢©£µ°¶®§µ×¥¦"
    "C-x 8 ,"      "¸çÇ"
    "C-x 8 /"      "÷åÅæÆøØ"
    "C-x 8 1"      "½¼"
    "C-x 8 1 /"    "½¼"
    "C-x 8 3"      "¾"
    "C-x 8 3 /"    "¾"
    "C-x 8 ^"      "^¹²³âÂêÊîÎôÔûÛ"
    "C-x 8 `"      "`àÀèÈìÌòÒùÙ"
    "C-x 8 ~"      "~ãÃðÐñÑõÕþÞ¬"
    "C-x RET"      "coding system"
    "C-x ESC"      "repeat-complex-command"
    )


  ;; ***************
  ;; *             *
  ;; * MAJOR MODES *
  ;; *             *
  ;; ***************

  ;; -------------------------------------------------------------------------------
  ;; ,----------------------------,
  ;; | Major Mode Leader Bindings |
  ;; '----------------------------'

  ;; -------------------------------------------------------------------------------
  ;; ,---------------------,
  ;; | Major Mode Bindings |
  ;; '---------------------'

  ;; -------------------------------------------------------------------------------
  ;; ,------------,
  ;; | Emacs Lisp |
  ;; '------------'

  ;; --------------------------
  ;; Major Mode Leader Bindings
  ;; --------------------------

  (bind-keys :map spacemacs-emacs-lisp-mode-map
             ("j" . eval-print-last-sexp)
             ("M-RET" . eval-print-last-sexp)
             )


  ;; -------------------------------------------------------------------------------
  ;; ,---------,
  ;; | ISearch |
  ;; '---------'

  (define-key isearch-mode-map (kbd "C-'") 'avy-isearch)
  (define-key isearch-mode-map (kbd "C-\"") 'helm-swoop)

  ;; -------------------------------------------------------------------------------
  ;; ,---------,
  ;; | C-c C-v |
  ;; '---------'

  (eval-after-load "markdown-mode"
    '(define-key markdown-mode-map (kbd "C-c C-v") 'markdown-export-to-html-and-view))
  (eval-after-load "web-mode"
    '(define-key web-mode-map (kbd "C-c C-v") 'browse-buffer-file-with-external-browser))

  ;; -------------------------------------------------------------------------------
  ;; ,----------,
  ;; | Org-Mode |
  ;; '----------'

  ;; remove C-tab binding which shadows #'next-multiframe-window binding
  ;; replace with [, C-tab] binding
  (bind-key [C-tab] #'next-multiframe-window)
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map [C-tab] #'next-multiframe-window)))

  ;; -------------------------------------------------------------------------------
  ;; ,---------,
  ;; | Aliases |
  ;; '---------'

  (defalias 'init 'spacemacs/find-dotfile)
  (defalias 'string-to-symbol 'intern)
  (defalias 'symbol-to-string 'symbol-name)
  (defalias 'pr 'cl-prettyprint)
  ;; aliases to user-defined functions
  (defalias 'ppm 'message-prettyprint)

  ;; -------------------------------------------------------------------------------
  ;; ,-----------------,
  ;; | Keyboard Macros |
  ;; '-----------------'

  (fset 'switch-to-most-recent-buffer [?\C-x ?b return])

  ;; ;; MACROS
  ;; (defmacro define-keys (keymap &rest bindings)
  ;;   (loop for (key . fn) in bindings
  ;;         do (define-key keymap key fn)))

  ;; -------------------------------------------------------------------------------
  ;; ,-----------,
  ;; | Functions |
  ;; '-----------'

  (defun get-char-face (&optional pos)
    (interactive)
    (message "face: %s" (get-char-property (or pos (point)) 'face)))
  (defun lookup-key-in-current-state ()
    (interactive)
    (let* ((key-input (read-key-sequence "key: "))
           (current-state-map (eval (intern (format "evil-%s-state-map" evil-state))))
           (function-name (lookup-key current-state-map key-input)))
      (message (symbol-name function-name))))
  (defun lookup-key-interactive (keymap key)
    (interactive "sKeymap: \nsKey: ")
    (message
     (with-temp-buffer
       (cl-prettyprint (lookup-key (intern keymap) key))
       (buffer-string))))
  (defun string-prettyprint (FORM)
    (with-temp-buffer
      (cl-prettyprint FORM)
      (buffer-string)))
  (defun message-prettyprint (FORM)
    (interactive "XForm: ")
    (message
     (with-temp-buffer
       (cl-prettyprint FORM)
       (buffer-string))))
  (defun browse-file-with-external-browser (file)
      (if (browse-url-can-use-xdg-open)
          (browse-url-xdg-open file)
        (progn
          (require 'eww)
          (eww-browse-with-external-browser file))))
  (defun browse-buffer-file-with-external-browser ()
    (interactive)
    (browse-file-with-external-browser buffer-file-name))
  (defun markdown-export-to-html-and-view ()
    (interactive)
    (browse-file-with-external-browser (markdown-export)))

  ;; REGION-BEGINNING and REGION-END
  ;; TODO: make these into text motions
  (defun evil-visual-jump-to-region-beginning ()
    (interactive)
    (goto-char (region-beginning))
    (evil-insert-state t))
  (defun evil-visual-jump-to-region-end ()
    (interactive)
    (goto-char (region-end))
    (evil-insert-state t))
  (defun funboundp (symbol) (not (fboundp symbol)))
  (defun eval-replace-last-sexp ()
    (interactive)
    (let ((val (eval-last-sexp)))
      (kill-sexp -1)
      (cl-prettyprint val)))
  (defun buffer-major-mode (buffer)
    "Print the current major-mode in the echo area and copy to kill-ring. If called without an argument, it also copies to kill-ring."
    (interactive "i")
    (unless buffer (setq buffer (buffer-name)))
    (with-current-buffer buffer
      (let ((mm (format "%S" major-mode)))
        (message mm)
        (unless current-prefix-arg (kill-new mm)))))

  ;; -------------------------------------------------------------------------------
  ;; ,-----------------------,
  ;; | Temporary Workarounds |
  ;; '-----------------------'
  ;; fix deprecated #'avy--with-avy-keys
  (eval-after-load "avy"
   '(when (and (funboundp 'avy--with-avy-keys)
               (fboundp   'avy-with))
      (defalias 'avy--with-avy-keys 'avy-with)))

  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
