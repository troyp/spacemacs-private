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
     c-c++
     (clojure :variables
              ;; clojure-enable-fancify-symbols t
              )
     elfeed
     emacs-lisp
     extra-langs
     git
     go
     haskell
     html
     java
     javascript
     lua
     markdown
     ocaml
     octave
     (org :variables
          org-enable-github-support t
          )
     python
     racket
     (ruby :variables
           ruby-version-manager 'rvm
           inf-ruby-default-implementation "pry"
           )
     scala
     scheme
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            )
     shell-scripts
     smex
     ;; spell-checking
     (syntax-checking :variables
                      syntax-checking-enable-by-default t
                      )
     ;; version-control
     (vinegar :variables
              vinegar-reuse-dired-buffer t)
     vimscript
     troyp
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     dired-sort-menu
     (dired+ :variables
             diredp-hide-details-initially-flag t
             diredp-hide-details-propagate-flag t
             )
     lacarte
     ;; libraries
     dash
     diff-hl
     f
     names
     s
     tiny
     ;; Drew Adams Packages
     autofit-frame
     ;; bookmark+
     column-enforce-mode
     dired-sort-menu+
     doremi
     doremi-cmd
     doremi-frm
     doremi-mac
     eyedropper
     facemenu+
     faces+
     firefox-controller
     fit-frame
     flycheck-package
     font-lock+
     frame-cmds
     frame-fns
     helm-firefox
     help-fns+
     help-mode+
     help+
     hexrgb
     highlight
     isearch+
     isearch-prop
     frame-cmds
     frame-fns
     mozc
     move-dup
     naked
     palette
     replace+
     strings
     thingatpt+
     thumb-frm
     top-mode
     ucs-cmds
     wid-edit+
     zoom-frm
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
   dotspacemacs-elpa-https nil
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
   dotspacemacs-startup-banner 001
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode
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
   dotspacemacs-highlight-delimiters 'current
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

  ;; ============================================================================
  ;; =========================== ******************** ===========================
  ;; =========================== *                  * ===========================
  ;; =========================== * SPACEMACS CONFIG * ===========================
  ;; =========================== *                  * ===========================
  ;; =========================== ******************** ===========================
  ;; ============================================================================

  ;; (setq-default tab-always-indent t)
  (global-linum-mode)
  (menu-bar-mode)
  (minibuffer-depth-indicate-mode 1)

  ;; not working?
  (setq-default evil-esc-delay 0.00001)

  (setq auto-completion-enable-help-tooltip t)

  (setq scroll-preserve-screen-position 1)

  (defun concat-as-directory (&rest parts)
    "Concatenate a group of path components, adding trailing separators where needed."
    (cl-loop for part in parts concat (file-name-as-directory part)))
  (defun concat-as-file-path (&rest parts)
    "Concatenate a group of path components, with a final filename, adding trailing
 separators where needed."
    (cl-loop for partsleft on parts
             concat (let ((part (car partsleft)))
                      (if (cdr partsleft)
                          (file-name-as-directory part)
                        part))))

  (setq spacemacs-private-directory (concat-as-directory (getenv "HOME")
                                                         ".emacs.d/private/"))
  (setq recentf-save-file (concat-as-file-path spacemacs-private-directory
                                                   ".cache" "recentf"))
  (setq bookmark-default-file (concat-as-file-path spacemacs-private-directory
                                                   ".cache" "bookmarks"))

  ;; disable warnings about setting path in rc files (caused by nvm or rvm)
  (setq exec-path-from-shell-check-startup-files nil)

  ;; CUA RECTANGLE
  (setq cua-enable-cua-keys nil)
  (cua-mode t)


  ;; ==============================================================================
                                ;; ***************
                                ;; *             *
                                ;; * ENVIRONMENT *
                                ;; *             *
                                ;; ***************

  (setenv "PATH" (concat "/home/troy/.nvm/versions/node/v0.12.7/bin" ":" (getenv "PATH")))
  (add-to-list 'exec-path "/home/troy/.nvm/versions/node/v0.12.7/bin")

  ;; ==============================================================================
  ;;                       *****************************
  ;;                       *                           *
  ;;                       * ADDITIONAL LOCAL PACKAGES *
  ;;                       *                           *
  ;;                       *****************************

  (add-to-load-path "~/.emacs.d/private/local/")
  (add-to-load-path "~/.emacs.d/private/local/firefox-protocol")
  ;; my packages
  (add-to-load-path "~/.emacs.d/private/local/evil-visual-replace")
  (add-to-load-path "~/.emacs.d/private/local/evil-adjust")

  (defvar dotspacemacs-additional-local-packages)
  (setf dotspacemacs-additional-local-packages
    '(
      dired+
      evil-adjust
      evil-visual-replace
      find-func+
      firefox-protocol
      help-macro+
      ibuffer-hydra
      ))
  (loop for pkg in dotspacemacs-additional-local-packages do
        (require pkg))

  ;; ==============================================================================
  ;;                             *******************
  ;;                             *                 *
  ;;                             * AUTO-MODE-ALIST *
  ;;                             *                 *
  ;;                             *******************

  (add-to-list 'auto-mode-alist '("\\.jsm" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.zsh" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.keynavrc" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.pryrc" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ocamlinit" . tuareg-mode))


  ;; ==============================================================================
  ;;                                *************
  ;;                                *           *
  ;;                                * EVIL-MODE *
  ;;                                *           *
  ;;                                *************

  ;; prevent cursor from moving back a space at the end of a line
  (setq evil-move-cursor-back nil)

  ;; ,-------------------------,
  ;; | evil-symbol-word-search |
  ;; '-------------------------'

  ;; Use symbols rather than words for * and # search
  ;; eg. in lisp modes, this will not stop at dashes, etc.
  ;; note: for symbol-based text-objects, use "io" ('evil-inner-symbol)
  ;;       rather than "iw" / "iW" ('evil-inner-word / 'evil-inner-WORD)

  (setq-default evil-symbol-word-search t)

  (defun toggle-evil-symbol-word-search ()
    (interactive)
    (setf evil-symbol-word-search (not evil-symbol-word-search))
    (message (if evil-symbol-word-search "symbols" "words")))
  (defalias 'evsw 'toggle-evil-symbol-word-search)

  ;; ,----------,
  ;; | Digraphs |
  ;; '----------'

  (setq evil-digraphs-table-user
        '(
          ((?. ? ) . ?\x2024)    ;; one-dot leader
          ((?. ?/) . ?\x2026)    ;; (horizontal) ellipsis
          ((?. ?-) . ?\x30fb)    ;; CJK middle-dot
          ))
  (defalias 'digra 'evil-enter-digraphs)  ;; evil-utils

  ;; ,------------,
  ;; | Lisp State |
  ;; '------------'

  (define-key evil-lisp-state-map "," spacemacs-emacs-lisp-mode-map)
  ;; (define-key evil-lisp-state-map "." nil) ;; available

  ;; ,--------------,
  ;; | Text Objects |
  ;; '--------------'

  (spacemacs|define-text-object "." "dot" "." ".")
  (spacemacs|define-text-object "h" "helplink" "`" "'")

  (evil-define-text-object evil-inner-line (count &optional beg end type)
    (list (line-visible-beginning-position) (+ 1 (line-visible-end-position))))
  (evil-define-text-object evil-outer-line (count &optional beg end type)
    (list (line-beginning-position) (line-end-position)))
  (evil-define-text-object evil-inner-defun (count &optional beg end type)
    (save-excursion
      (mark-defun)
      (list (point) (mark))))

  (define-key evil-inner-text-objects-map "l" 'evil-inner-line)
  (define-key evil-outer-text-objects-map "l" 'evil-outer-line)
  (define-key evil-inner-text-objects-map "d" 'evil-inner-defun)

  ;; ,-----------,
  ;; | Undo Tree |
  ;; '-----------'

  (setq evil-want-fine-undo "No")

  (setq undo-tree-history-directory-alist
        (let ((undohistdir (concat spacemacs-private-directory ".undo-tree-history/")))
          `((".*" . ,undohistdir))))
  (setq undo-tree-auto-save-history t)

  ;; Attempt to prevent undo-tree history corruption...
  ;; https://github.com/syl20bnr/spacemacs/issues/774#issuecomment-194527210
  (defun my-save-undo-history ()
    (when (and (boundp 'undo-tree-mode)
               undo-tree-mode
               buffer-file-name
               (file-writable-p buffer-file-name)
               (not (eq buffer-undo-list t))
               (not revert-buffer-in-progress-p))
      (undo-tree-save-history nil t)))

  (defun my-save-all-undo-history ()
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (my-save-undo-history))))

  (add-hook 'kill-emacs-hook #'my-save-all-undo-history)
  (add-hook 'kill-buffer-hook #'my-save-undo-history)



;; ==============================================================================
;;                                 ****************
;;                                 *              *
;;                                 * KEY-BINDINGS *
;;                                 *              *
;;                                 ****************

  (defmacro def-variable-toggle (var)
    (let* ((fname `(concat "toggle-" (symbol-name ',var)))
           (fsym  (intern (eval fname))))
      `(defun ,fsym ()
         "Defined with `def-variable-toggle'."
         (interactive)
         (setq ,var (not ,var)))))

  ;; ,----------------------,
  ;; | Keybinding Functions |
  ;; '----------------------'
  ;; spacemacs macros:   evil-map evil-define-key evil-define-minor-mode-key
  ;;                     evil-define-keymap spacemacs|define-micro-state
  ;; bindkey fns/macros: bind-map bind-key bind-key* bind-keys bind-keys*
  ;;                     bind-keys-form bind-map-add-to-major-mode-list
  ;;                     bind-map-set-keys bind-map-kbd-keys unbind-key
  ;; other:   defhydra

  (defun define-keys (keymap &rest bindings)
    "Define multiple keys with `define-key'\nBINDINGS has the form KEY DEFN [KEY DEFN ...]"
    (loop for (key defn) on bindings by 'cddr do
          (define-key keymap key defn)))

  (defun kbd+ (keyrep &optional need-vector)
    (if (vectorp keyrep) keyrep (edmacro-parse-keys keyrep need-vector)))

  (defun gmap (keyrep defstr)
    "Vim-style global keybinding. Uses the `global-set-key' binding function."
    (global-set-key (kbd+ keyrep) (edmacro-parse-keys defstr t)))

  (defun fmap (keybind-fn keyrep defstr)
    "Vim-style keybinding using the key binding function KEYBIND-FN."
    (call keybind-fn (kbd+ keyrep) (edmacro-parse-keys defstr t)))

  (defun xmap (keymap keyrep defstr)
    "Vim-style keybinding in KEYMAP. Uses the `define-key' binding function."
    (define-key keymap (kbd+ keyrep) (edmacro-parse-keys defstr t)))

  (defun nmap (keyrep defstr) "Vim-style keybinding for `evil-normal-state'. Uses the `define-key' binding function."
         (xmap evil-normal-state-map keyrep defstr))
  (defun imap (keyrep defstr) "Vim-style keybinding for `evil-insert-state'. Uses the `define-key' binding function."
         (xmap evil-insert-state-map keyrep defstr))
  (defun vmap (keyrep defstr) "Vim-style keybinding for `evil-visual-state'. Uses the `define-key' binding function."
         (xmap evil-visual-state-map keyrep defstr))
  (defun mmap (keyrep defstr) "Vim-style keybinding for `evil-motion-state'. Uses the `define-key' binding function."
         (xmap evil-motion-state-map keyrep defstr))


  ;; ,-----------------,
  ;; | Global Bindings |
  ;; '-----------------'

  (global-set-key (kbd "M-0") 'universal-argument)
  ;; approximate global mapping (with higher priority)
  (define-key evil-normal-state-map (kbd "M-0") 'universal-argument)
  (define-key evil-visual-state-map (kbd "M-0") 'universal-argument)
  (define-key evil-insert-state-map (kbd "M-0") 'universal-argument)
  (define-key evil-emacs-state-map  (kbd "M-0") 'universal-argument)
  (define-key evil-hybrid-state-map (kbd "M-0") 'universal-argument)

  (global-set-key (kbd "M-S-x") 'execute-extended-command)

  (global-set-key [\M-f4] 'kill-buffer-and-window)

  (global-set-key [C-tab] 'next-multiframe-window)
  (global-set-key [C-S-iso-lefttab] 'previous-multiframe-window)
  ;; change C-x - from 'shrink-window-if-larger-than-buffer to 'fit-window-to-buffer
  (global-set-key (kbd "\C-x -") 'fit-window-to-buffer)

  (global-set-key (kbd "M-n") 'evil-scroll-line-down)
  (global-set-key (kbd "M-p") 'evil-scroll-line-up)
  (global-set-key (kbd "C-S-j")
                  (lambda () (interactive) (scroll-other-window 1)))
  (global-set-key (kbd "C-S-k")
                  (lambda () (interactive) (scroll-other-window-down 1)))

  (global-set-key (kbd "<C-return>") 'evil-cua-toggle)

  (global-set-key "\C-a" 'move-beginning-of-line-or-text)    ;; troyp/utils.el
  (global-set-key (kbd "<S-return>") 'open-line-below)       ;; troyp/utils.el
  (global-set-key (kbd "<C-S-return>") 'open-line-above)     ;; troyp/utils.el
  (global-set-key [\C-\S-down] 'spacemacs//move-text-move-text-down-J)
  (global-set-key [\C-\S-up] 'spacemacs//move-text-move-text-up-K)

  ;; remove C-S-SPC from cua-global-keymap and bind to just-one-space
  (define-key cua-global-keymap (kbd "C-S-SPC") nil)
  (global-set-key (kbd "C-S-SPC") 'just-one-space)

  (global-set-key (kbd "C-M-d") 'scroll-other-window)
  (global-set-key (kbd "C-M-u") 'scroll-other-window-down)
  (global-set-key (kbd "C-M-S-d") 'scroll-other-window-down)
  (global-set-key (kbd "M-J") 'scroll-up-line)
  (global-set-key (kbd "M-K") 'scroll-down-line)

  (global-set-key [\C-f10] 'menu-bar-mode)
  (global-set-key [\M-f12] 'shell-pop)
  (global-set-key (kbd "C-'") 'shell-pop)

  (global-set-key (kbd "C-x a C-'") 'abbrev-prefix-mark)

  (global-set-key (kbd "C-M-v") 'er/expand-region)
  (global-set-key (kbd "C-S-M-v") 'er/contract-region)

  (global-set-key [f1] 'help-map)
  (global-set-key (kbd "<C-f1>") 'describe-prefix-bindings)
  (global-set-key (kbd "<M-f1>") 'describe-key)
  (global-set-key [f5] 'spacemacs-cmds)
  (global-set-key [\C-f5] 'which-key-show-top-level)
  (global-set-key (kbd "<C-f9>") 'evil-normal-state)
  (global-set-key (kbd "<M-f9>") 'evil-evilified-state)
  (global-set-key (kbd "<S-f9>") 'current-mode-and-state)

  (global-set-key [f7] 'exchange-point-and-mark)
  (global-set-key [f8] 'er/contract-region)
  (global-set-key [f9] 'er/expand-region)

  (global-set-key (kbd "M-c") 'evil-upcase-first-letter)
  (global-set-key (kbd "M-C") 'capitalize-word)

  (global-set-key (kbd "C->") 'evil-repeat-pop-next)

  (global-set-key (kbd "<M-insert>") 'org-capture)

  ;; approximate global mapping (with higher priority)
  (define-key evil-normal-state-map (kbd "C-M-x") 'helm-eval-expression-with-eldoc)
  (define-key evil-visual-state-map (kbd "C-M-x") 'helm-eval-expression-with-eldoc)
  (define-key evil-insert-state-map (kbd "C-M-x") 'helm-eval-expression-with-eldoc)
  (define-key evil-emacs-state-map  (kbd "C-M-x") 'helm-eval-expression-with-eldoc)
  (define-key evil-hybrid-state-map (kbd "C-M-x") 'helm-eval-expression-with-eldoc)

  ;; -------------------------------------------------------------------------------
  ;; ,-------------------------,
  ;; | Evil State Key Bindings |
  ;; '-------------------------'
  ;;
  ;; ,--------------,
  ;; | NORMAL STATE |
  ;; '--------------'
  ;; note: evilified state map uses the bindings for keys:
  ;; / : h j k l n N v V gg G C-f C-b C-e C-y C-d C-u C-z

  ;; when rebinding them for normal-state, rebind for evilified-state also
  (define-key evil-normal-state-map [delete] 'kill-this-buffer)
  (defun insert-space () (interactive) (insert ? ))
  (define-key evil-normal-state-map (kbd "S-SPC") 'insert-space)
  ;; shift reverses C-d (-scroll-down) and C-o (-jump-backward)
  (define-key evil-normal-state-map (kbd "C-S-d") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-S-o") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
  ;; remove C-y (use global M-p)
  (define-key evil-normal-state-map (kbd "C-y") nil)
  ;; reverse gu and gU
  (define-key evil-normal-state-map (kbd "gu") 'evil-upcase)
  (define-key evil-normal-state-map (kbd "gU") 'evil-downcase)
  ;; provide evil-repeat-find-char-reverse binding
  (define-key evil-normal-state-map (kbd "M-;") 'evil-repeat-find-char-reverse)
  ;; [r and ]r move to beginning and end of region
  (define-key evil-normal-state-map (kbd "[r") 'evil-visual-jump-to-region-beginning)
  (define-key evil-normal-state-map (kbd "]r") 'evil-visual-jump-to-region-end)
  (define-key evil-normal-state-map (kbd "M-RET RET") 'lisp-state-toggle-lisp-state)
  ;; evil-symbol-word-search
  (define-key evil-normal-state-map (kbd "C-*") 'toggle-evil-symbol-word-search)

  ;; universal-argument
  (define-key evil-normal-state-map (kbd "C-S-u") 'universal-argument)
  ;; 'negative-argument is also on C-M--
  ;; M-- was bound to 'ahs-back-to-start but it doesn't seem to work
  (define-key evil-normal-state-map (kbd "M--") 'negative-argument)

  (define-key evil-normal-state-map (kbd "C--") 'spacemacs/evil-numbers-decrease)
  (define-key evil-normal-state-map (kbd "C-=") 'spacemacs/evil-numbers-increase)

  ;; evil-shift-up/down-line-or-block
  (define-key evil-normal-state-map [\M-\S-down] 'evil-shift-down-line-or-block)
  (define-key evil-normal-state-map [\M-\S-up] 'evil-shift-up-line-or-block)
  ;; insert at WORD beginning
  (define-key evil-normal-state-map (kbd "M-B") 'evil-insert-at-WORD-beginning)

  ;; ,-----------------,
  ;; | EVILIFIED STATE |
  ;; '-----------------'

  (defun evilified-state-init ()
    (define-keys evil-evilified-state-map
      (kbd "C-y") nil
      (kbd "C-e") 'end-of-line
      (kbd "C-v") 'evil-visual-block
      ))
  (add-hook 'evil-evilified-state-entry-hook 'evilified-state-init)

  ;; ,--------------,
  ;; | VISUAL STATE |
  ;; '--------------'
  (defun insert-space-visual () (interactive) (execute-kbd-macro " ") (evil-visual-restore))
  (define-key evil-visual-state-map (kbd "S-SPC") 'insert-space-visual)
  (define-key evil-visual-state-map (kbd "C-SPC") 'evil-forward-char-or-extend)
  (define-key evil-visual-state-map (kbd "C-\\") 'shell-command-replace-region)
  (define-key evil-visual-state-map (kbd "M-u") 'evil-upcase)
  (define-key evil-visual-state-map (kbd "M-l") 'evil-downcase)
  (define-key evil-visual-state-map (kbd "M-=") 'count-region)
  (define-key evil-visual-state-map (kbd ".") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "M-.") 'er/contract-region)
  ;; (define-key evil-visual-state-map (kbd "M-%") 'evil-visual-replace-query-replace)
  ;; (define-key evil-visual-state-map (kbd "C-M-%") 'evil-visual-replace-replace-regexp)
  (evil-visual-replace-visual-bindings)


  ;; ,--------------,
  ;; | MOTION STATE |
  ;; '--------------'
  (define-key evil-motion-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-motion-state-map (kbd "[") 'evil-motion-open-bracket-prefix-map)
  (define-key evil-motion-state-map (kbd "]") 'evil-motion-close-bracket-prefix-map)
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
  (define-key evil-insert-state-map (kbd "C-S-a") 'evil-paste-last-insertion)
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line-or-text)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-S-y") 'evil-copy-from-below)
  (define-key evil-insert-state-map (kbd "C-l") 'delete-char)
  (define-key evil-insert-state-map (kbd "C-S-l") 'backward-delete-char)
  (define-key evil-insert-state-map (kbd "C-S-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-.") 'yas-expand)
  (define-key evil-insert-state-map (kbd "C-n") 'next-line)
  (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  ;; (define-key evil-insert-state-map (kbd "C-M-SPC") 'hippie-expand)

  ;; unicode insertion
  (define-key evil-insert-state-map (kbd "C-v") 'insert-char)
  (define-key evil-insert-state-map (kbd "M-v") 'iso-transl-ctl-x-8-map)
  (define-key evil-insert-state-map (kbd "C-k") 'evil-insert-digraph)

  ;; -------------------------------------------------------------------------------
  ;; ,--------------------,
  ;; | Keymap Definitions |
  ;; '--------------------'
  ;; set function definition to value for keymaps defined only as values
  (fset 'help-map help-map)

  ;; -------------------------------------------------------------------------------
  ;; ,----------------------,
  ;; | Evil Leader Bindings |
  ;; '----------------------'
  ;; can use bind-keys to define prefix maps (Leader map is 'spacemacs-cmds, see below)

  (evil-leader/set-key
    "b M"          'switch-to-messages-buffer
    "b W"          'switch-to-warnings-buffer
    "b -"          'diff-buffer-with-file
    "b SPC"        'spacemacs/new-empty-buffer
    "b C-b"        'ibuffer
    "b C-e"        'bury-buffer
    "b C-u"        'undo-tree-clear
    "b <insert>"   'buffer-major-mode
    "b <f1>"       'about-emacs
    "f ."          'find-alternate-file
    "f >"          'find-alternate-file-other-window
    "f ' p"        'dired-spacemacs-private-directory
    "f / f"        'sudo-open-file
    "f / e"        'spacemacs/sudo-edit
    "f / b"        'sudo-edit-this-file
    ;; "h"            'help-prefix-map
    "h a"          'apropos
    "h f f"        'find-function
    "h f k"        'find-function-on-key
    "h f h"        'describe-function
    "h f w"        'find-function-other-window
    "h f W"        'find-function-other-window-noselect
    "h f 5"        'find-function-other-frame
    "h f ."        'find-function-at-point
    "h w"          'help-download-prefix-map
    "h C-m"        'lacarte-execute-menu-command
    "h C-/"        'evil-search-highlight-persist-remove-all
    "h C-?"        'evil-search-highlight-restore
    "h d C-b"      'describe-personal-keybindings
    "h 1"          'evil-goto-definition
    ;; "h <f1>"       'help-map
    "i -"          'tiny-expand
    "o a"          'asciiheadings-prefix-key-map
    "o c"          'character-prefix-map
    "o f"          'flycheck-command-map
    "o m"          'modes-prefix-key-map
    "o v"          'variable-pitch-mode
    "r b"          'bookmark-map
    "R R"          'pcre-multi-occur
    "R r"          'pcre-occur
    "t O"          (def-variable-toggle which-key-show-operator-state-maps)
    "t T"          (def-variable-toggle indent-tabs-mode)
    "t |"          'fci-mode
    "t C-/"        'evil-search-highlight-persist
    "w TAB"        'ace-swap-window
    "x a ."        'spacemacs/align-repeat-period
    "x a '"        'spacemacs/align-repeat-quote
    "x a \""       'spacemacs/align-repeat-double-quote
    "x a -"        'spacemacs/align-repeat-dash
    "x a C-;"      'spacemacs/align-repeat-semicolon-comment
    "x a SPC"      'quick-pcre-align-repeat
    "x l U"        'delete-duplicate-lines-nonblank
    "x C-l"        'quick-pcre-align-repeat
    "x N"          'rectangle-number-lines-interactive
    "3"            'spacemacs/enter-ahs-backward
    "8"            'spacemacs/enter-ahs-forward
    "."            'repeat-complex-command
    ","            'helm-mini
    ">"            'evil-shift-right-fine-dispatcher
    "<"            'evil-shift-left-fine-dispatcher
    "|"            'extend-to-column
    "<backtab>"    'switch-to-most-recent-buffer
    "<backspace>"  'kill-this-buffer
    "<delete>"     'kill-buffer-and-window
    "<return>"     'helm-buffers-list
    "<f3>"         'kmacro-keymap
    "<f5>"         'spacemacs/safe-revert-buffer
    "C-l"          'quick-pcre-align-repeat
    "C-v"          'evil-cua-toggle
    "C-w"          'delete-frame
    "C-."          'ido-switch-buffer
    "C-/"          'evil-search-highlight-persist-remove-all
    "C-?"          'evil-search-highlight-restore
    "C-SPC"        'cua-toggle-global-mark
    "C-S-SPC"      'just-one-blank-line
    "M-q"          'wrap-lines-in-region
    "M-x"          'helm-M-x
    "M-%"          'evil-visual-replace-query-replace
    "M-C-%"        'evil-visual-replace-replace-regexp
    )

  (bind-keys :map spacemacs-cmds
             :prefix-map help-download-prefix-map
             :prefix "h w"
             :prefix-docstring "Commands to download additional documentation."
             ("r" . github-download-README)
             ("w" . github-clone-wiki)
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

  (bind-keys :map spacemacs-cmds
             :prefix-map undo-prefix-map
             :prefix "o u"
             :prefix-docstring "Commands related to undo."
             ("t" . global-undo-tree-mode)
             ("v" . undo-tree-visualize)
             )


  (bind-keys :map spacemacs-cmds
             :prefix-map follow-prefix-map
             :prefix "w f"
             :prefix-docstring "Commands dealing with follow-mode."
             ("f"   . follow-delete-other-windows-and-split)
             ("SPC" . follow-mode)
             )

  (bind-keys :map spacemacs-cmds
             :prefix-map keymaps-prefix-map
             :menu-name "keys/keymaps"
             :prefix "K"
             :prefix-docstring "Commands dealing with keymaps."
             ("a" . which-key-show-keymap-at-point)
             ("f" . get-binding)
             ("i" . lookup-key-interactive)
             ("p" . prettyprint-keymap)
             ("r" . replace-ints-with-char)
             ("s" . which-key-show-current-state-map)
             ("u" . parent-keymap-at-point)
             ("w" . which-key-show)
             ("K" . which-key-show-top-level)
             )

  (bind-keys :map keymaps-prefix-map
             :prefix-map keymaps-describe-prefix-map
             :prefix "d"
             :prefix-docstring "Describe commands related to keymaps and key binding."
             ("b"   . describe-bindings)
             ("f"   . describe-function)
             ("k"   . describe-key)
             ("l"   . spacemacs/describe-last-keys)
             ("m"   . spacemacs/describe-mode)
             ("o"   . describe-option)
             ("K"   . describe-keymap)
             ("C-b" . describe-personal-keybindings)
             )

  (bind-keys :map spacemacs-cmds
             :prefix-map external-apps-prefix
             :menu-name "external apps"
             :prefix "!"
             :prefix-docstring "Commands dealing with external applications."
             ("f b" . helm-firefox-bookmarks)
             )

  (bind-keys :map spacemacs-cmds
             :prefix-map modes-prefix-key-map
             :prefix "o m"
             :prefix-docstring "Commands dealing with modes and states. Inherits from `mode-ring-prefix-key-map'"
             ("e" . evil-evilified-state)
             ("n" . evil-normal-state)
             )
  (set-keymap-parent modes-prefix-key-map mode-ring-prefix-key-map)

  (bind-keys :map spacemacs-cmds
             :prefix-map structured-text-prefix-map
             :menu-name "structured text"
             :prefix "X"
             :prefix-docstring "Commands dealing with structured text."
             ("sn" . sort-numeric-fields)
             )

  (bind-keys :map search-map
             ;; M-s map
             ("s" . dired-mark-files-regexp)
             )

  ;; -------------------------------------------------------------------------------
  ;; ,-------------------------,
  ;; | which-key Configuration |
  ;; '-------------------------'

  ;; ;; enable which-key for motions - breaks t/f
  ;; (setq which-key-show-operator-state-maps t)

  (bind-keys :map which-key-C-h-map
             ("C-h" . which-key-abort)
             )

  (which-key-add-key-based-replacements
    "C-x a"        "abbrev"
    "C-x n"        "narrow"
    "C-x r"        "rectangle/register/bookmark"
    "C-x X"        "edebug"
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
    "C-x @"        "event-apply--modifier"
    "M-g"          "goto-map"
    "M-s"          "search-map"
    "SPC b h"      "*spacemacs*"
    "SPC b s"      "*scratch*"
    "SPC b M"      "*messages*"
    "SPC b <f1>"   "*About GNU Emacs*"
    "SPC h f"      "find-function"
    "SPC o f"      "flycheck"
    "SPC K"        "keys/keymaps"
    "SPC X"        "structured text"
    )

  (dolist (cons '(("return"  . "RET")
                  ("delete"  . "Delete")
                  ("backtab" . "S-TAB")
                  ("escape"  . "ESC")
                  ))
    (add-to-list 'which-key-key-replacement-alist cons))


  ;; ,--------------------,
  ;; | Command Docstrings |
  ;; '--------------------'

  (defmacro set-docstring (fn docstr)
    (eval `(put ',fn 'function-documentation ,docstr)))

  (defun set-docstrings (&rest pairs)
    (eval `(loop for (fn docstr) on ',pairs by 'cddr do
                 (eval `(put ',fn 'function-documentation ,docstr)))))

  (set-docstrings
   'evil-search-highlight-persist-remove-all    "Remove all `evil-search' highlighting"
   )

  ;; ***************
  ;; *             *
  ;; * MINOR MODES *
  ;; *             *
  ;; ***************

  ;; -------------------------------------------------------------------------------
  ;; ,----------,
  ;; | Flycheck |
  ;; '----------'

  (eval-after-load "flycheck"
    '(progn
       (bind-keys
        :map spacemacs-cmds
        :prefix-map flycheck-prefix-map
        :prefix "o f"
        :prefix-docstring "flycheck commands."
        ("x"   . flycheck-disable-checker)
        ("v"   . flycheck-verify-setup)
        ("V"   . flycheck-version)
        ("i"   . flycheck-manual)
        ("H"   . display-local-help)
        ("h"   . flycheck-display-error-at-point)
        ("?"   . flycheck-describe-checker)
        ("e"   . flycheck-set-checker-executable)
        ("s"   . flycheck-select-checker)
        ("C-w" . flycheck-copy-errors-as-kill)
        ("l"   . flycheck-list-errors)
        ("p"   . flycheck-previous-error)
        ("n"   . flycheck-next-error)
        ("C-c" . flycheck-compile)
        ("C"   . flycheck-clear)
        ("c"   . flycheck-buffer)
        )
       ))

  ;; -------------------------------------------------------------------------------
  ;; ,------,
  ;; | Mozc |
  ;; '------'
  (require 'mozc)
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'overlay)


  ;; ***************
  ;; *             *
  ;; * MAJOR MODES *
  ;; *             *
  ;; ***************

  ;; -------------------------------------------------------------------------------
  ;; =====,------------------------,=====
  ;; =====| Terminal & Shell Modes |=====
  ;; ====='------------------------'=====

  (defun shell-pop-shell-type-set (shell-type)
    (cl-case shell-type
      ('multi (setq shell-pop-shell-type
                    ("multiterm" "*multiterm*"
                     (lambda nil (funcall 'multiterm nil)))))
      ('shell  (setq shell-pop-shell-type
                     ("shell" "*shell*"
                      (lambda nil (funcall 'shell nil)))))
      ('term   (setq shell-pop-shell-type
                     ("term" "*term*"
                      (lambda nil (funcall 'term shell-pop-term-shell)))))
      ('ansi   (setq shell-pop-shell-type
                     ("ansi-term" "*ansi-term*"
                      (lambda nil (funcall 'ansi-term shell-pop-term-shell)))
                     ))
      ('eshell (setq shell-pop-shell-type
                     ("eshell" "*eshell*"
                      (lambda nil (funcall 'eshell nil)))))))

  (defun shell-pop-shell-type-set-and-launch (shell-type)
    (shell-pop-shell-type-set shell-type)
    (shell-pop))

  (defhydra shell-pop-choose (global-map "C-x <f12>" :color blue :columns 6)
    "Terminal/Shell Mode"
    ( "a " (shell-pop-shell-type-set-and-launch 'ansi)   "ansi-term")
    ( "e " (shell-pop-shell-type-set-and-launch 'eshell) "eshell-mode")
    ( "s " (shell-pop-shell-type-set-and-launch 'shell)  "shell-mode")
    ( "t " (shell-pop-shell-type-set-and-launch 'term)   "term-mode")
    ( "m " (shell-pop-shell-type-set-and-launch 'multi)  "multi-term")
    ("ESC" nil "abort"))
  (global-set-key (kbd "C-x <f12>") 'shell-pop-choose/body)

  ;;           ,-----------,
  ;;           | ansi-term |
  ;;           '-----------'

  (evil-set-initial-state 'term-mode 'emacs)
  (eval-after-load "term"
    '(progn
       (define-key term-raw-map (kbd "C-p")      'term-send-up)
       (define-key term-raw-map (kbd "C-n")      'term-send-down)
       (define-key term-raw-map (kbd "C-c C-y")  'term-paste)
       ))


  ;;            ,-----------,
  ;;            | term-mode |
  ;;            '-----------'

  ;; Toggle between term-mode and shell-mode
  ;; https://www.emacswiki.org/emacs/ShellMode#toc12
  (eval-after-load 'term
    `(require 'shell)
    )

  (defun term-switch-to-shell-mode ()
    (interactive)
    (shell-mode)
    (set-process-filter  (get-buffer-process (current-buffer)) 'comint-output-filter )
    (local-set-key (kbd "C-j") 'term-switch-to-shell-mode)
    (compilation-shell-minor-mode 1)
    (comint-send-input))

  (defun term-switch-back-from-shell-mode ()
    (interactive)
    (compilation-shell-minor-mode -1)
    (font-lock-mode -1)
    (set-process-filter  (get-buffer-process (current-buffer)) 'term-emulate-terminal)
    (term-mode)
    (term-char-mode)
    (term-send-raw-string (kbd "C-l")))

  (defun term-switch-to-shell-mode ()
    (interactive)
    (if (equal major-mode 'term-mode)
        (term-switch-to-shell-mode)
      (term-switch-back-from-shell-mode)))

  ;; ,-----------------,
  ;; | multi-term-mode |
  ;; '-----------------'


  ;; -------------------------------------------------------------------------------
  ;; ,---------------------,
  ;; | bookmark-bmenu-mode |
  ;; '---------------------'

  ;; eval-after-load "bookmark.el" isn't working? (nor with 'bookmark)
  (defun bookmark-bmenu-mode-init ()
    (evilified-state-evilify-map bookmark-bmenu-mode-map
      :mode bookmark-bmenu-mode))
  (add-hook 'bookmark-bmenu-mode-hook 'bookmark-bmenu-mode-init)


  ;; -------------------------------------------------------------------------------
  ;; ,-------,
  ;; | Dired |
  ;; '-------'

  (defun dired-copy-file-path-as-kill ()
    (interactive)
    (dired-copy-filename-as-kill 0))
  (defun dired-copy-file-directory-as-kill ()
    (interactive)
    (file-name-directory (dired-copy-file-path-as-kill)))

  ;; INITIAL STATE:
  ;; dired-mode  :   (customized) evilified state
  ;; wdired-mode :                normal state

  (eval-after-load "dired"
   `(progn
      (require 'dired+)

      (define-key dired-mode-map (kbd "C-h") nil)

      ;; keys
      (evilified-state-evilify-map dired-mode-map
        :mode dired-mode
        :bindings
        (kbd "c")      'diredp-copy-this-file
        (kbd "gd")     'dired-hide-details-mode
        (kbd "gu")     'diredp-up-directory-reuse-dir-buffer
        (kbd "gw")     'dired-toggle-read-only
        (kbd "j")      'diredp-next-line
        (kbd "k")      'diredp-previous-line
        ;; copy name/path bindings: as in ranger
        (kbd "yd")     'dired-copy-file-directory-as-kill
        (kbd "yn")     'dired-copy-filename-as-kill
        (kbd "yp")     'dired-copy-file-path-as-kill
        ;; (kbd "{")      'evil-backward-paragraph
        ;; (kbd "}")      'evil-forward-paragraph
        (kbd "{")      'dired-prev-subdir
        (kbd "}")      'dired-next-subdir
        (kbd "C-n")    'ido-find-file
        (kbd "M-=")    'dired-create-directory
        (kbd "M-DEL")  'diredp-up-directory-reuse-dir-buffer
        [f2]           'dired-toggle-read-only
        (kbd "H")      'dired-do-hard-link
        (kbd "C-h")    nil
        )
      ;; T is the prefix key for the tags commands
      (which-key-add-major-mode-key-based-replacements 'dired-mode
        "T"      "tags"
        "M-+"    "diredp-recursive-map"
        "y"    "copy--as-kill"
        )
      ;; set function definition of 'dired-mode-map (same as value)
      (fset 'dired-mode-map dired-mode-map)
      ;; major-mode leader-key
      (spacemacs/set-leader-keys-for-major-mode 'dired-mode
        "c"     'dired-mode-map
        "tr"    'toggle-diredp-find-file-reuse-dir
        "v"     'dired-view-file    ;; for discovery - can just use \v
        "Y"     'diredp-relsymlink-this-file
        )
      (spacemacs/declare-prefix-for-mode 'dired-mode "mt" "toggles")

      ;; wdired-mode
      (evil-set-initial-state 'wdired-mode 'normal)
      )
   )

  (eval-after-load "dired+" `(diredp-toggle-find-file-reuse-dir 1))
  (eval-after-load "dired-sort-menu" `(require 'dired-sort-menu+))

  (defun wdired-init ()
    (define-keys wdired-mode-map
      (kbd "C-c <escape>") 'wdired-abort-changes
      [f2]                 'wdired-finish-edit
      ))
  (add-hook 'wdired-mode-hook 'wdired-init)

  (evil-define-key 'normal view-mode-map
    "q"   'View-quit
    )
  (defun view-mode-init ()
    (spacemacs/set-leader-keys-for-minor-mode 'view-mode
      "q"   'View-quit
      ))
  (add-hook 'view-mode-hook #'view-mode-init)


  ;; -------------------------------------------------------------------------------
  ;; ,--------,
  ;; | Elfeed |
  ;; '--------'

  ;; auto-evilification can't remap 'elfeed-search-fetch
  (eval-after-load "elfeed"
    `(progn
       (define-key elfeed-search-mode-map (kbd "C-x G") 'elfeed-search-fetch)
       ))

  ;; -------------------------------------------------------------------------------
  ;; ,------------,
  ;; | Emacs Lisp |
  ;; '------------'
  (define-key lisp-interaction-mode-map
    [remap eval-print-last-sexp] 'evil-eval-print-last-sexp)

  ;; -------------------------------------------------------------------------------
  ;; ,--------------,
  ;; | Haskell-Mode |
  ;; '--------------'

  (eval-after-load "haskell-mode"
    '(progn
       (bind-keys :map haskell-mode-map
                  ("C-c C-h" . nil)
                  ("C-c M-h" . haskell-hoogle)
                  ("C-c C-v" . browse-buffer-file-firefox)
                  )
       (which-key-add-major-mode-key-based-replacements 'haskell-mode
         "C-c @" "hiding"
         )
       ))

  ;; -------------------------------------------------------------------------------
  ;; ,-----------,
  ;; | helm-mode |
  ;; '-----------'

  (defun helm-switch-to-mini ()
    (interactive)
    (helm-run-after-exit #'helm-mini))

  (eval-after-load "helm-mode"
    `(progn
       (bind-keys :map helm-map
                  ("C-q"        . ace-jump-helm-line-and-select)  ;; was ace-jump-helm-line
                  ("C-S-q"      . ace-jump-helm-line) 
                  ("C-0"        . helm-select-action)
                  ("C-)"        . helm-execute-persistent-action)
                  ("C-S-O"      . helm-previous-source)
                  ("C-S-W"      . helm-yank-symbol-at-point)
                  ("M-m"        . spacemacs-cmds)
                  ("C-u"        . helm-delete-minibuffer-contents)
                  ("C-,"        . helm-switch-to-mini)
                  ("<f5>"       . nil)
                  ("<f9>"       . spacemacs/helm-navigation-micro-state)
                  ("<f11>"      . nil)
                  ("<escape>"   . evil-evilified-state)
                  ("<S-escape>" . evil-normal-state)
                  )

       (evilified-state-evilify-map helm-map
         :mode helm-mode
         :bindings
         "j"           'helm-next-line
         "k"           'helm-previous-line
         "i"           'evil-insert
         "a"           'evil-append
         [escape]      'keyboard-escape-quit
         [S-escape]    'evil-normal-state
         )))

  (spacemacs/set-leader-keys-for-major-mode 'helm-major-mode
    "tm"    'helm-toggle-all-marks
    )


; -------------------------------------------------------------------------------
  ;; ,--------------,
  ;; | helm-firefox |
  ;; '--------------'

  (defgroup firefox nil
    "Customization variables for interacting with the Firefox browser."
    :group 'environment)

  (defcustom firefox-profile-directory "~/.mozilla/firefox/"
    "The root directory for firefox profile config folders."
    :group 'firefox
    :type 'string)

  (defcustom firefox-default-user-profile "2xdr1tat.Troy"
    "The default firefox profile."
    :group 'firefox
    :type 'string)

  (defcustom firefox-default-user-path
    (concat-as-directory firefox-profile-directory firefox-default-user-profile)
    "The root directory for firefox profile config folders."
    :group 'firefox
    :type 'string)

  ;; requires wmctrl executable
  ;; firefox executable is "firefox" by default, otherwise $FIREFOXEXE
  (eval-after-load "helm-firefox"
    `(progn
       (defun helm-get-firefox-user-init-dir ()
         firefox-default-user-path)))

  ;; install firefox protocol ffbookmarks in about:config or user.js:
  ;; user_pref("network.protocol-handler.expose.ffbookmarks", false);
  (eval-after-load "firefox-protocol"
    `(progn
       (defun firefox-protocol--get-firefox-user-init-dir ()
         firefox-default-user-path)))

  ;; -------------------------------------------------------------------------------
  ;; ,-----------,
  ;; | help-mode |
  ;; '-----------'

  (eval-after-load "help-mode"
    `(progn
       (bind-keys :map help-mode-map
                  ("a" . help-previous)
                  ("d" . help-next)
                  )
       ))

  ;; -------------------------------------------------------------------------------
  ;; ,---------,
  ;; | ibuffer |
  ;; '---------'

  (evil-set-initial-state 'ibuffer-mode 'evilified)
  (eval-after-load 'ibuffer
    `(progn
      (require 'ibuffer-hydra)
      (define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
      (add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)
      ))

  ;; -------------------------------------------------------------------------------
  ;; ,----------,
  ;; | ido-mode |
  ;; '----------'

  (defun ido-init ()
    (bind-keys :map ido-completion-map
               ("M-+" . ido-make-directory)
               ("M-=" . ido-make-directory)
               ("M-m" . spacemacs-cmds)
               ))

  (add-hook 'ido-setup-hook 'ido-init)

  ;; (spacemacs/set-leader-keys-for-minor-mode 'ido-mode
  ;;   )

  ;; -------------------------------------------------------------------------------
  ;; ,-----------,
  ;; | Info-Mode |
  ;; '-----------'

  (bind-keys :map Info-mode-map
             ("M-h" . Info-history-back)
             ("M-l" . Info-history-forward)
             )

  (spacemacs/set-leader-keys-for-major-mode 'Info-mode
    "ci"      'info-index
    "cm"      'info-menu
    "cs"      'info-search
    "c RET"   'Info-follow-nearest-node
    "c SPC"   'Info-scroll-up
    "c+"      'Info-merge-subnodes
    "c,"      'Info-index-next
    "c-"      'negative-argument
    "c?"      'describe-mode
    "cG"      'Info-goto-node-web
    "cH"      'describe-mode
    "cL"      'Info-history
    "cT"      'Info-toc
    "c["      'Info-backward-node
    "c]"      'Info-forward-node
    "c^"      'Info-up
    "cb"      'beginning-of-buffer
    "ce"      'end-of-buffer
    "cf"      'Info-follow-reference
    "cg"      'Info-goto-node
    "ch"      'Info-help
    "cl"      'Info-history-back
    "cn"      'Info-next
    "ct"      'Info-top-node
    "cv"      'Info-virtual-book
    "cw"      'Info-copy-current-node-name
    )

  (spacemacs/declare-prefix-for-mode 'Info-mode "mc" "commands")

  (eval-after-load "Info-mode"
    '(progn
       (evil-define-key 'normal Info-mode-map
         (kbd ",")   'spacemacs-Info-mode-map
         (kbd "M-;") 'evil-repeat-find-char-reverse
         )
       (evil-define-key 'visual Info-mode-map
         (kbd ",")   'spacemacs-Info-mode-map
         (kbd "M-;") 'evil-repeat-find-char-reverse
         )
       (evil-define-key 'motion Info-mode-map
         (kbd ",")   'spacemacs-Info-mode-map
         (kbd "M-;") 'evil-repeat-find-char-reverse
         )
       (evil-define-key 'evilified-state Info-mode-map
         (kbd ",")   'spacemacs-Info-mode-map
         (kbd "M-;") 'evil-repeat-find-char-reverse
         )
       (define-key Info-mode-map
         (kbd "M-RET") 'spacemacs-Info-mode-map
         (kbd "M-;")   'evil-repeat-find-char-reverse
         )
       ))

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

  (global-set-key (kbd "C-c C-v") 'browse-buffer-file-firefox)
  (eval-after-load "markdown-mode"
    '(define-key markdown-mode-map (kbd "C-c C-v") 'markdown-export-to-html-and-view))
  (eval-after-load "web-mode"
    '(define-key web-mode-map (kbd "C-c C-v") 'browse-buffer-file-with-external-application))
  (eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-v") 'browse-buffer-file-firefox))

  ;; -------------------------------------------------------------------------------
  ;; ,------,
  ;; | Java |
  ;; '------'

  (defun java-init () (interactive)
         (define-key java-mode-map (kbd "M-c") 'evil-upcase-first-letter)
         (define-key java-mode-map (kbd "RET") 'c-indent-new-comment-line)
         )

  (add-hook 'java-mode-hook 'java-init)

  (setq eclim-eclipse-dirs "~/opt/eclipse"
        eclim-executable "~/opt/eclipse/eclim")

  ;; ================
  ;; Keyboard Macros.
  ;; ================

  (fset 'java-fn-from-spec
        (lambda (&optional arg)
          "Keyboard macro."
          (interactive "p")
          (kmacro-exec-ring-item
           (quote ([86 201326629 47 47 46 42 return return 102 58 120 119 104 167772192 1 102 41 108 11 1 101 112 65 32 123 125 escape 106 1] 0 "%d")) arg)))

  (fset 'java-constructor-from-spec
        (lambda (&optional arg)
          "Keyboard macro."
          (interactive "p")
          (kmacro-exec-ring-item
           (quote ([1 86 201326629 47 47 46 42 36 92 124 36 13 123 125 13 121 106 1] 0 "%d")) arg)))


  (fset 'java-field-from-spec
        (lambda
          (&optional arg)
          "Keyboard macro."
          (interactive "p")
          (kmacro-exec-ring-item
           (quote ([1 102 58 119 100 119 1 101 97 32 escape 112 102 58 114 59 86 134217848 100 101 108 101 116 101 45 116 114 97 105 108 105 110 103 45 119 104 105 116 101 115 112 97 99 101 13 65 escape 106 1] 0 "%d")) arg)))

  ;; -------------------------------------------------------------------------------
  ;; ,------,
  ;; | Lisp |
  ;; '------'

  (bind-keys :map spacemacs-emacs-lisp-mode-map
             ("e RET" . eval-replace-last-sexp)
             )
  (bind-keys :map spacemacs-lisp-interaction-mode-map
             ("e RET" . eval-replace-last-sexp)
             ("e j"   . eval-prettyprint-last-sexp)
             ("j"     . eval-prettyprint-last-sexp)
             ("x"     . prettyexpand-at-point)
             )

  ;; -------------------------------------------------------------------------------
  ;; ,-------,
  ;; | Ocaml |
  ;; '-------'

  (which-key-add-major-mode-key-based-replacements 'tuareg-mode
    ", c"     "compile"
    ", e"     "error"
    ", g"     "locate/alternate-file"
    ", g g"   "locate identifier at point (no new window)"
    ", g G"   "locate identifier at point in new window"
    ", h"     "documentation"
    ", r"     "case-analyze"
    ", s"     "utop"
    )

  ;; -------------------------------------------------------------------------------
  ;; ,------------,
  ;; | Occur-Mode |
  ;; '------------'

  (evilified-state-evilify-map occur-mode-map
    :mode occur-mode
    :bindings
    )

  ;; -------------------------------------------------------------------------------
  ;; ,----------,
  ;; | Org-Mode |
  ;; '----------'

  ;; remove C-tab binding which shadows #'next-multiframe-window binding
  ;; replace with [, C-tab] binding
  (bind-key [C-tab] 'next-multiframe-window)

  (defun org-init ()
    (define-key org-mode-map [C-tab] 'next-multiframe-window))

  (eval-after-load 'org
    `(progn
       (add-hook 'org-mode-hook 'org-init)
       (setq org-capture-templates
             '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
                "* TODO %?\n  %i\n  %a")
               ("j" "Journal" entry (file+datetree "~/org/journal.org")
                "* [%t] %^G\n%?")
               ))
       (setq org-directory (concat-as-directory (getenv "HOME") "org"))
       (setq org-default-notes-file (concat-as-file-path org-directory "notes.org"))
       ))

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "SPC"    'ace-link-org
    )


  ;; -------------------------------------------------------------------------------
  ;; ,-----------,
  ;; | Ruby-mode |
  ;; '-----------'

  (defun ruby-init ()
    (define-key spacemacs-ruby-mode-map "sb" 'ruby-send-buffer)
    (define-key spacemacs-ruby-mode-map "sI" 'inf-ruby))

  (add-hook 'ruby-mode-hook 'ruby-init)


  ;; -------------------------------------------------------------------------------
  ;; ,-----------------------,
  ;; | Spacemacs-Buffer-Mode |
  ;; '-----------------------'

  (which-key-add-major-mode-key-based-replacements 'spacemacs-buffer-mode
    "m"     "jump to menu"
    )

  ;; -------------------------------------------------------------------------------
  ;; ,---------------------------,
  ;; | Undo-Tree-Visualizer-Mode |
  ;; '---------------------------'

  (spacemacs/set-leader-keys-for-major-mode 'undo-tree-visualizer-mode
    "d"              'undo-tree-visualizer-toggle-diff
    "c?"             'describe-mode
    "cb"             'undo-tree-visualize-switch-branch-left
    "cd"             'undo-tree-visualizer-toggle-diff
    "cf"             'undo-tree-visualize-switch-branch-right
    "cg"             'revert-buffer
    "ch"             'describe-mode
    "cn"             'undo-tree-visualize-redo
    "cp"             'undo-tree-visualize-undo
    "cq"             'undo-tree-visualizer-quit
    "cs"             'undo-tree-visualizer-selection-mode
    "ct"             'undo-tree-visualizer-toggle-timestamps
    "c DEL"          'scroll-down-command
    "c S-SPC"        'scroll-down-command
    "c <C-down>"     'undo-tree-visualize-redo-to-x
    "c <C-up>"       'undo-tree-visualize-undo-to-x
    "c <down>"       'undo-tree-visualize-redo
    "c <left>"       'undo-tree-visualize-switch-branch-left
    "c <mouse-1>"    'undo-tree-visualizer-mouse-set
    "c <next>"       'undo-tree-visualizer-scroll-up
    "c <prior>"      'undo-tree-visualizer-scroll-down
    "c <right>"      'undo-tree-visualize-switch-branch-right
    "c <up>"         'undo-tree-visualize-undo
    )

  (spacemacs/declare-prefix-for-mode 'undo-tree-visualizer-mode "mc" "commands")

  (eval-after-load "undo-tree-visualizer-mode"
    '(progn
       (evil-define-key 'normal' undo-tree-visualizer-mode-map
         (kbd ",")   'spacemacs-undo-tree-visualizer-mode-map
         (kbd "M-;") 'evil-repeat-find-char-reverse
         )
       (evil-define-key 'visual' undo-tree-visualizer-mode-map
         (kbd ",")   'spacemacs-undo-tree-visualizer-mode-map
         (kbd "M-;") 'evil-repeat-find-char-reverse
         )
       (evil-define-key 'motion' undo-tree-visualizer-mode-map
         (kbd ",")   'spacemacs-undo-tree-visualizer-mode-map
         (kbd "M-;") 'evil-repeat-find-char-reverse
         )
       (evil-define-key 'evilified-state' undo-tree-visualizer-mode-map
         (kbd ",")   'spacemacs-undo-tree-visualizer-mode-map
         (kbd "M-;") 'evil-repeat-find-char-reverse
         )
       (define-key undo-tree-visualizer-mode-map
         (kbd "M-RET") 'spacemacs-undo-tree-visualizer-mode-map
         (kbd "M-;")   'evil-repeat-find-char-reverse
         )))

  ;; ---------------
  ;; Leader Bindings
  ;; ---------------
  (bind-keys :map spacemacs-emacs-lisp-mode-map
             ("m" . spacemacs-undo-tree-visualizer-mode-map)
             )

  ;; -------------------------------------------------------------------------------
  ;; ,----------,
  ;; | Web-Mode |
  ;; '----------'

  (spacemacs/set-leader-keys-for-major-mode 'web-mode
    ",gn"            'web-mode-element-next
    ",gN"            'web-mode-element-previous
    ",ge"            'web-mode-element-end
    )

  (which-key-add-major-mode-key-based-replacements 'web-mode
    ", g"       "navigate DOM tree"
    ", r"       "element operations"
    ", e"       "error"
    "C-c !"     "flycheck"
    "C-c @"     "highlight-symbol"
    "C-c C-a"   "attributes"
    "C-c C-b"   "blocks"
    "C-c C-d"   "DOM"
    "C-c C-e"   "entities"
    "C-c C-t"   "tags"
    )
  (eval-after-load "web-mode"
    '(progn
       (setq web-mode-tag-auto-close-style  1)
       (setq web-mode-enable-auto-expanding t)
       ))
  (eval-after-load "emmet-mode"
    '(define-keys emmet-mode-keymap
       (kbd "<C-return>")   'open-line-below
       (kbd "<C-S-return>") 'open-line-above
       ))

  ;; -------------------------------------------------------------------------------
  ;; ,-------,
  ;; | Align |
  ;; '-------'

  (spacemacs|create-align-repeat-x "period" "." nil t)
  (spacemacs|create-align-repeat-x "quote" "'")
  (spacemacs|create-align-repeat-x "double-quote" "\"")
  (spacemacs|create-align-repeat-x "dash" "-")
  (spacemacs|create-align-repeat-x "semicolon-comment" ";;?" )

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

  ;; -------------------------------------------------------------------------------
  ;; ,-----------------,
  ;; | Keyboard Macros |
  ;; '-----------------'

  (fset 'switch-to-most-recent-buffer [?\C-x ?b return])

  (fset 'comment-bar-heading-5=
        (lambda (&optional arg)
          "Keyboard macro."
          (interactive "p")
          (kmacro-exec-ring-item
           (quote ("gu$vils=vils=vils=vils=vils= ;;" 0 "%d")) arg)))

  ;; -------------------------------------------------------------------------------
  ;; ,--------------------------------,
  ;; | Key/Keymap Functions & Aliases |
  ;; '--------------------------------'
  ;; to show keymap with which-key:  (which-key--show-keymap keymap-name keymap)
  (defalias 'key-vector-to-readable-string 'key-description)
  (defalias 'key-readable-string-to-string 'kbd)  ;; or edmacro-parse-keys or read-kbd-macro
  (defalias 'key-input-to-vector 'read-key-sequence-vector)
  (defalias 'key-input-to-string 'read-key-sequence)
  (defun key-readable-string-to-vector (keystr) (edmacro-parse-keys keystr t))

  (defun lookup-key-interactive (keymap key)
    (interactive
     (list
      (read-string "Enter keymap: ")
      (read-key-sequence "Press key: " nil t)))
    (let* ((cmd (lookup-key (evalstr keymap) key)))
      (message "%s" (trim-multiline-string (string-prettyprint cmd)))))

  (defun which-key-show-current-state-map ()
    (interactive)
    (let ((current-state-map (format "evil-%s-state-map" evil-state)))
      (which-key-show (intern current-state-map))))

  (defun which-key-show (map)
    "Display the keymap MAP in a which-key pop-up."
    (interactive "SKeymap: ")
    (which-key--show-keymap (symbol-name map) (eval map)))

  ;; TODO: work out what 8-digit integers represent in a keymap.
  ;; Currently, they're left untouched.
  (defun replace-ints-with-char (beg end)
    "Replace the numbers in a keymap representation with a readable string
representation."
    (interactive "r")
    (unless (use-region-p)
      (setq beg (point-min)
            end (point-max)))
    (save-excursion
      (goto-char beg)
      (while (search-forward-regexp "\\((\\|\\[\\)\\([0-9]+\\)" end t)
        (if (string= (match-string 2) "92")
            (replace-match "\"\\\\\\\\\"" t nil)
          (let* ((match (match-string 2))
                 (num   (string-to-number match)))
            (if (characterp num)
                (replace-match (format "\"%c\"" num)
                               t nil)))))))

  ;; FIXME
  (defun prettyprint-keymap (kmap)
    (interactive "SKeymap: ")
    (set-mark-command)
    (cl-prettyprint (eval kmap))
    (evil-active-region 1)
    (replace-ints-with-char))

  (defun get-binding (cmd)
    (interactive "SCommand name: ")
    (let* ((cmdname       (symbol-name cmd))
           (cmdname-escd  (format "\\[%s]" cmdname))
           (cmdkey        (substitute-command-keys cmdname-escd))
           (cmdcons       (cons cmdname cmdkey)))
      (message "%S" cmdcons)
      cmdcons))

  (defun which-key-show-keymap-at-point (sym)
    (interactive (list (symbol-at-point)))
    (let ((kmap (cond
                 ((keymapp sym)        sym)
                 ((keymapp (eval sym))  (eval sym))
                 (t                     nil))))
      (which-key-show kmap)))

  (defun parent-keymap-at-point (sym)
    (interactive (list (symbol-at-point)))
    (let ((kmap (cond
                 ((keymapp sym)        sym)
                 ((keymapp (eval sym))  (eval sym))
                 (t                     nil))))
      (keymap-parent kmap)))

  (defun key-to-edmacro-format (key)
    "Converts a key to edmacro format (eg  -> C-x C-a).
The key should be entered using quoted-insert, or entered interactively.
See `edmacro-format-keys'."
    (interactive "kKey: ")
    (edmacro-format-keys key))

  ;; adapted from emacs source. GPL3.
  (defun read-key-sequence-and-related ()
    "This is the read-function used in `describe-key'.
It returns a list of (KEY UNTRANSLATED UP-EVENT).

KEY can be any kind of a key sequence; it can include keyboard events,
mouse events, and/or menu events.
UNTRANSLATED is a vector of the corresponding untranslated events.
UP-EVENT is the up-event that was discarded by reading KEY, or nil.

If KEY is a menu item or a tool-bar button that is disabled, this command
temporarily enables it to allow getting help on disabled items and buttons."
    (let ((enable-disabled-menus-and-buttons t)
          (cursor-in-echo-area t)
          saved-yank-menu)
      (unwind-protect
          ;; =====BODYFORMS====
          (let (key)
            ;; If yank-menu is empty, populate it temporarily, so that
            ;; "Select and Paste" menu can generate a complete event.
            (when (null (cdr yank-menu))
              (setq saved-yank-menu (copy-sequence yank-menu))
              (menu-bar-update-yank-menu "(any string)" nil))
            (setq key (read-key-sequence "Describe key (or click or menu item): "))
            (list
             key
             (prefix-numeric-value current-prefix-arg)
             ;; If KEY is a down-event, read and include the
             ;; corresponding up-event.  Note that there are also
             ;; down-events on scroll bars and mode lines: the actual
             ;; event then is in the second element of the vector.
             (and (vectorp key)
                  (let ((last-idx (1- (length key))))
                    (and (eventp (aref key last-idx))
                         (memq 'down (event-modifiers (aref key last-idx)))))
                  (or (and (eventp (aref key 0))
                           (memq 'down (event-modifiers (aref key 0)))
                           ;; However, for the C-down-mouse-2 popup
                           ;; menu, there is no subsequent up-event.  In
                           ;; this case, the up-event is the next
                           ;; element in the supplied vector.
                           (= (length key) 1))
                      (and (> (length key) 1)
                           (eventp (aref key 1))
                           (memq 'down (event-modifiers (aref key 1)))))
                  (read-event))))
        ;; =====UNWINDFORMS=====
        ;; Put yank-menu back as it was, if we changed it.
        (when saved-yank-menu
          (setq yank-menu (copy-sequence saved-yank-menu))
          (fset 'yank-menu (cons 'keymap yank-menu))))))

  ;; -------------------------------------------------------------------------------
  ;; ,---------,
  ;; | Aliases |
  ;; '---------'

  (defalias 'init 'spacemacs/find-dotfile)
  (defalias 'pr 'cl-prettyprint)
  (defalias 'copy-string-as-kill 'kill-new)
  (defalias 'reyas 'yas/reload-all)
  (defalias 'arv 'auto-revert-mode)
  (defalias 'revb 'revert-buffer)
  (defalias 'diffb 'diff-buffer-with-file)
  (defalias 'sim 'set-input-method)  ;; bound to C-x RET C-\
  (defalias 'repl 'ielm)
  (defalias 'lim 'lisp-interaction-mode)
  (defalias 'el 'emacs-lisp-mode)
  (defalias 'chmodx 'make-executable)
  (defalias 'unset 'makunbound)
  (defalias 'unfset 'fmakunbound)
  (defalias 'vll 'visual-line-mode)
  (defalias 'undefun 'fmakunbound)
  (defalias 'acoff 'auto-complete-mode-off)
  (defalias 'ali 'quick-pcre-align-repeat)
  (defalias 'replace-in-string 'dired-replace-in-string)
  ;; aliases for discoverability
  (defalias 'string-to-symbol 'intern)
  (defalias 'symbol-to-string 'symbol-name)
  ;; aliases to user-defined functions
  (defalias 'ppm 'message-prettyprint)
  (defalias 'boxcom 'box-heading-comment)
  (defalias 'reccom 'rect-heading-comment)
  (defalias 'sreccom 'short-rect-heading-comment)
  (defalias 'ppp 'insert-pp)

  ;; -------------------------------------------------------------------------------
  ;; ,-----------,
  ;; | Functions |
  ;; '-----------'

  (defun get-char-face (&optional pos)
    (interactive)
    (message "face: %s" (get-char-property (or pos (point)) 'face)))

  (defun evalstr(str) (eval (intern str)))

  (defun trim-multiline-string (str) (replace-regexp-in-string  "^\n+\\|\n+$" "" str))

  (defun string-prettyprint (FORM)
    "Return the result of printing FORM with `cl-prettyprint' as a string."
    (with-temp-buffer
      (cl-prettyprint FORM)
      (buffer-string)))

  (defun message-prettyprint (FORM)
    (interactive "XForm: ")
    (message "%s"
             (with-temp-buffer
               (cl-prettyprint FORM)
               (buffer-string))))

  (defun eval-string (s)
    "Evaluates a SEXP which is represented as a string."
    (eval (car (read-from-string s))))

  (defun browse-file-with-external-application (file)
    (if (browse-url-can-use-xdg-open)
        (browse-url-xdg-open file)
      (progn
        (require 'eww)
        (eww-browse-with-external-browser file))))

  (defun browse-buffer-file-firefox ()
    (interactive)
    (browse-url-firefox
     (replace-regexp-in-string
      " "
      "%20"
      (concat "file://"
              (expand-file-name (or buffer-file-name default-directory))))))

  (defun browse-buffer-file-with-external-application ()
    (interactive)
    (browse-file-with-external-application buffer-file-name))

  (defun markdown-export-to-html-and-view ()
    (interactive)
    (let ((fname (or (markdown-export-file-name)
                    (make-temp-file "markdown-output"))))
      (browse-file-with-external-application (markdown-export fname))))

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

  (defun buffer-major-mode (buffer)
    "Print the current major-mode in the echo area and copy to kill-ring. If called
 without an argument, it also copies to kill-ring."
    (interactive "i")
    (unless buffer (setq buffer (buffer-name)))
    (with-current-buffer buffer
      (let ((mm (format "%S" major-mode)))
        (message mm)
        (unless current-prefix-arg (kill-new mm)))))

  (defun eval-replace-last-sexp ()
    "Replace the preceding sexp with its value, formatted by `pp-to-string'.
With a prefix argument, formats the value using `(format \"%S\" val)' instead."
    (interactive)
    (if (boundp 'evil-state)
        (evil-save-state
          (call-interactively #'evil-append)
          (eval-replace-last-sexp-core))
      (eval-replace-last-sexp-core)))

  (defun eval-replace-last-sexp-core ()
    "Replace the preceding sexp with its value, formatted by pp-to-string. With a
 prefix argument, formats the value using `(format \"%S\" val)' instead."
    (let ((val (eval (preceding-sexp))))
      (kill-sexp -1)
      (if current-prefix-arg (insert (format "%S" val))
        (insert (replace-regexp-in-string "\n\\'" "" (pp-to-string val))))))

  (defun eval-prettyprint-last-sexp (eval-last-sexp-arg-internal)
    (interactive "P")
    (cl-prettyprint (eval-last-sexp eval-last-sexp-arg-internal)))

  (defalias 'move-visible-beginning-of-line 'back-to-indentation
    "Move to the first non-whitespace character on the line (or the end of line if
 no non-whitespace)")

  (defun move-visible-end-of-line ()
    "Move to the last non-whitespace character on the line (or the start of line if
 no non-whitespace)"
    (interactive)
    (end-of-line)
    (re-search-backward "[^ \t\n]" (line-beginning-position) 1))

  ;; TODO: maintain position if no matches
  (defun pcre-replace-regexp-in-string (PCRE REP STRING &optional FIXEDCASE LITERAL SUBEXP START)
    "Replace all matches for PCRE with REP in STRING, where PCRE is converted to
    an elisp regexp by the function `rxt-pcre-to-elisp'.\n

For the meaning of the optional arguments, see `replace-regexp-in-string'."
    (replace-regexp-in-string (pcre-to-elisp REGEXP)
                              REP STRING FIXEDCASE LITERAL SUBEXP START))

  ;; TODO: these github download functions require the home directory hard-coded
  ;; TODO: because ~ and $HOME aren't working -- investigate
  (defun github-download-README (repo-str)
    (interactive "sUSER/REPO or URL: ")
    (let* ((user/repo (replace-regexp-in-string
                       (pcre-to-elisp "^https://github.com/|/wiki$|(\.wiki)?\.git$")
                       ""
                       repo-str))
           (url       (concat "https://github.com/" user/repo ".git"))
           (userdir   (replace-regexp-in-string (pcre-to-elisp "^~/")
                                                (concat (getenv "HOME") "/")
                                                user-emacs-directory))
           (dir       (concat userdir "private/docs/"))
           (repo      (replace-regexp-in-string "[^/]+/" ""
                                                user/repo))
           (cmd       (concat "tempdir=`mktemp -d`;\n"
                              "cd $tempdir;\n"
                              "url=\"" url "\";\n"
                              "echo Creating Temp directory: $tempdir\n"
                              "git clone \"$url\";\n"
                              "cd *;\n"
                              "for f in *README*; do\n"
                              "  name=\"" repo "-$f\";"
                              "  cp \"$f\" \"" dir "$name\";"
                              "done;")))
      (message cmd)
      (shell-command (format "bash -c %s" (shell-quote-argument cmd)))))

  (defun github-download-docs (repo-str)
    (interactive "sUSER/REPO or URL: ")
    (let* ((user/repo (replace-regexp-in-string
                       (pcre-to-elisp "^https://github.com/|/wiki$|(\.wiki)?\.git$")
                       ""
                       repo-str))
           (url       (concat "https://github.com/" user/repo ".git"))
           (userdir   (replace-regexp-in-string (pcre-to-elisp "^~/")
                                                (concat (getenv "HOME") "/")
                                                user-emacs-directory))
           (dir       (concat userdir "private/docs/"))
           (repo      (replace-regexp-in-string "[^/]+/" ""
                                                user/repo))
           (cmd       (concat "tempdir=`mktemp -d`;\n"
                              "cd $tempdir;\n"
                              "url=\"" url "\";\n"
                              "echo Creating Temp directory: $tempdir\n"
                              "git clone \"$url\";\n"
                              "cd *;\n"
                              "for f in doc docs; do\n"
                              "  name=\"" repo "-$f\";"
                              "  cp -r -T \"$f\" \"" dir "$name\";"
                              "done;")))
      (message cmd)
      (shell-command (format "bash -c %s" (shell-quote-argument cmd)))))

  (defun github-clone-wiki (repo-str)
    (interactive "sUSER/REPO or URL: ")
    (let* ((user/repo (replace-regexp-in-string
                       (pcre-to-elisp "^https://github.com/|/wiki$|(\.wiki)?\.git$")
                       ""
                       repo-str))
           (url       (concat "https://github.com/" user/repo ".wiki.git"))
           (userdir   (replace-regexp-in-string
                       (pcre-to-elisp "^~/")
                       (concat (getenv "HOME") "/")
                       user-emacs-directory))
           (dir       (concat userdir "private/docs/")))
      (shell-command (format "cd '%s'; git clone %s" dir url))))

  (defun switch-to-messages-buffer ()
    (interactive)
    (switch-to-buffer (messages-buffer)))

  (defun switch-to-warnings-buffer ()
    (interactive)
    (switch-to-buffer "*Warnings*"))

  (defun help-previous ()
    (interactive)
    (help-xref-go-back (current-buffer)))
  (defun help-next ()
    (interactive)
    (help-xref-go-forward (current-buffer)))

  (defun help-buttons()
    "List of help buttons in current buffer"
    (loop for (text . pos) in (ace-link--help-collect)
          collect (cons text (button-at pos))))

  ;; TODO: work out how to test for search highlighting -> write toggle function
  (defun evil-search-highlight-restore ()
    (interactive)
    (hlt-highlight-regexp-region (buffer-end -1) (buffer-end 1) (car regexp-search-ring)))

  (defun evil-upcase-first-letter ()
    (interactive)
    ;; TODO: make work with regions and motions
    (evil-upcase (point) (+ 1 (point)))
    (evil-forward-word-begin))

  (evil-define-operator evil-upcase-first-letter (beg end type)
    "Convert text to upper case."
    (if (eq type 'block)
        (evil-apply-on-block #'evil-upcase-first-letter beg end nil)
      (while (< (point) end)
        (evil-upcase (point) (+ 1 (point)))
        (evil-forward-word-begin))))

  (defun undo-tree-clear ()
    (interactive)
    (setq buffer-undo-tree nil))

  (defun dired-spacemacs-private-directory ()
    (interactive)
    (dired spacemacs-private-directory))

  (defun insert-pp (object)
    "`insert' the pretty-printed representation of OBJECT into current buffer. See `pp'."
    (interactive)
    (insert (pp object)))

  ;; evil-shift-right/left while maintaining visual mode
  (defun evil-visual-shift-left ()
    (interactive)
    (call-interactively 'evil-shift-left)
    (evil-visual-restore))
  (defun evil-visual-shift-right ()
    (interactive)
    (call-interactively 'evil-shift-right)
    (evil-visual-restore))

  (defun evil-lisp-insert-function-application (pt mk)
    "Surround the region (or symbol-at-point if region is inactive) with parens and
 position point after the open-paren, with a space after it."
    (interactive "r")
    (unless (region-active-p)
      (setf pt (cadr (symbol-at-point-with-bounds)))
      (setf mk (- (cddr (symbol-at-point-with-bounds)) 1)))
    (evil-visual-make-selection pt mk)
    (evil-insert-state)
    (evil-goto-char (+ mk 1))
    (insert ")")
    (evil-goto-char pt)
    (insert "( ")
    (evil-backward-char))

  ;; ============================
  ;; functions dealing with lines
  ;; ============================

  (defun line-at-point-string ()
    "Return the line around point as a string.
Similar to (thing-at-point \'line t) except it does not return a trailing newline.
See also `thing-at-point'"
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (buffer-substring-no-properties beg end)))

  (defun line-at-point-blank-p ()
    "Returns a non-nil value if the current line contains only whitespace."
    (string-match-p "^[[:space:]]*$" (line-at-point-string)))
  (defun line-above-blank-p (&optional n)
    (save-excursion
      (forward-line (- (or n 1)))
      (line-at-point-blank-p)))
  (defun line-below-blank-p (&optional n)
    (save-excursion
      (forward-line (or n 1))
      (line-at-point-blank-p)))
  (defun adjacent-line-blank-p (&optional n)
    "Returns a non-nil value if the Nth line above and/or below point contains
only whitespace).
See `line-at-point-blank-p', `line-above-blank-p', `line-below-blank-p'"
    (or (line-above-blank-p n)
        (line-below-blank-p n)))


;; ======================
;; delete duplicate lines
;; ======================

  (defun delete-duplicate-lines-nonblank
      (beg end &optional reverse adjacent delete-blanks interactive)
    "Delete duplicate lines within region. This is the same as
`delete-duplicate-lines' except it keeps blank lines by default unless the
DELETE-BLANKS argument is non-nil.\n\nCan be called with the prefixes:

C-u          Keep the last instance of each line
C-u C-u      Delete blank line duplicates
C-u C-u C-u  Only delete adjacent duplicates
\nSee also `spacemacs/uniquify-lines', which deletes adjacent duplicate lines
within the region."
    (interactive
     (progn
       (list
        (region-beginning) (region-end)
        (equal current-prefix-arg '(4))
        (equal current-prefix-arg '(64))
        (equal current-prefix-arg '(16))
        t)))
    (delete-duplicate-lines beg end reverse adjacent (not delete-blanks) interactive))

  (defun just-one-blank-line ()
    (interactive)
    (if (and (line-at-point-blank-p)
             (adjacent-line-blank-p))
        (delete-blank-lines)))


  (defun remove-doubled-blank-lines ()
    (interactive)
    (replace-regexp "\n[[:space:]]*\n\\([[:space:]]*\n\\)+" "\n\n"))

  (defun delete-adjacent-repeated-lines ()
    (interactive)
    (destructuring-bind (beg . end) (evil-get-visual-region-or-buffer)
      (delete-duplicate-lines beg end nil t nil t)))


  (defun sudo-edit-this-file ()
    (interactive)
    (let ((f (concat "/sudo::" (expand-file-name buffer-file-name))))
      (find-file f)))


;; ================
;; shift left/right
;; ================
;; TODO: non-hackish versions

  (defun evil-shift-left-fine ()
    (interactive)
    (let ((evil-shift-width 1))
      (call-interactively 'evil-shift-left)))
  (defun evil-shift-right-fine ()
    (interactive)
    (let ((evil-shift-width 1))
      (call-interactively 'evil-shift-right)))
  (defun evil-visual-shift-left-fine ()
    (interactive)
    (let ((evil-shift-width 1))
      (evil-visual-shift-left))
    (execute-kbd-macro "gv"))
  (defun evil-visual-shift-right-fine ()
    (interactive)
    (let ((evil-shift-width 1))
      (evil-visual-shift-right))
    (execute-kbd-macro "gv"))
  (defun evil-shift-left-fine-dispatcher ()
    (interactive)
    (if (eq evil-state 'visual)
        (call-interactively 'evil-visual-shift-left-fine)
      (call-interactively 'evil-shift-left-fine)))
  (defun evil-shift-right-fine-dispatcher ()
    (interactive)
    (if (eq evil-state 'visual)
        (call-interactively 'evil-visual-shift-right-fine)
      (call-interactively 'evil-shift-right-fine)))


  (defun evil-cua-toggle ()
    (interactive)
    (if cua-rectangle-mark-mode
        (progn
          (cua-rectangle-mark-mode -1)
          (cua-cancel)
          (evil-exit-emacs-state))
      (evil-emacs-state)
      (cua-rectangle-mark-mode 1)))
  (global-set-key (kbd "<C-return>") 'evil-cua-toggle)

  (defmacro after-motion (fn)
    "Builds a function to map a point to its new position after the motion
command FN has been applied."
    `(lambda (pt)
       (save-excursion
         (goto-char pt)
         (funcall ,fn)
         (point))))

  (defun evil-insert-at-WORD-beginning (&optional count)
    (interactive "p")
    (evil-backward-WORD-begin count)
    (evil-insert-state))

  (defun wrap-lines-in-region (beg end)
    "An interactive function to split lines longer than `fill-column'.
Splits long lines in the region using `fill-paragraph', but never joins lines.
Ie., each line is treated as a distinct paragraph."
    (interactive "r")
    (when (numberp current-prefix-arg) (setq fill-column current-prefix-arg))
    (replace-string "\n" "\n\n" nil beg end)
    (evil-active-region 1)
    (fill-paragraph nil t)
    (replace-string "\n\n" "\n" nil beg end))

;; FIXME: splits the line after region (near start of line); irregular indentation.
(defun wrap-region-or-comment (beg end)
  "An interactive function to split lines longer than `fill-column'.
Splits long lines in the region using `fill-paragraph', but never joins lines.
Ie., each line is treated as a distinct paragraph.
Comments are first uncommented using `evilnc-comment-or-uncomment-region' before
wrapping and then re-commented."
  (interactive "r")
  (let ((comment? (evilnc--in-comment-p beg)))
    (when (numberp current-prefix-arg) (setq fill-column current-prefix-arg))
    (when comment? (evilnc--comment-or-uncomment-region beg end))
    (replace-string "\n" "\n\n" nil beg end)
    (evil-active-region 1)
    (fill-paragraph nil t)
    (replace-string "\n\n" "\n" nil beg end)
    (when comment? (evilnc--comment-or-uncomment-region beg end))))

(defun helm-yank-symbol-at-point ()
  "Yank the symbol at point in `helm-current-buffer' into minibuffer."
  (interactive)
  (with-helm-current-buffer
    (let ((sym (symbol-name (symbol-at-point))))
      (helm-set-pattern (concat helm-pattern sym)))))

(defun find-function-other-window-noselect (function)
  "Display the source of FUNCTION in other window."
  (interactive (find-function-read))
  (let ((thiswin (selected-window)))
    (find-function-other-window function)
    (select-window thiswin)
    ))

(defun rectangle-number-lines-interactive ()
  "Insert a column of incrementing numbers in the rectangular region.
The command prompts for an initial number and a format string.
The format string is interpreted as in the `format' function.
See also `rectangle-number-lines'."
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively #'rectangle-number-lines)))

(defun current-mode-and-state()
  "Display the current `evil-state' and `major-mode' in the minibuffer."
  (interactive)
  (let ((s (format "evil-state: %S || major-mode: %S"
                   evil-state
                   major-mode)))
    (message s)))

(defun sprint-keymap (map)
  (with-temp-buffer
    (cl-prettyprint map)
    (replace-ints-with-char (point-min) (point-max))
    (buffer-string)))

(defun symbol-or-function-near-point ()
  (or (when (fboundp 'symbol-nearest-point) (symbol-nearest-point))
      (function-called-at-point)))

(defun prettyprint-keymap (map)
  "Insert a pretty-printed representation of a keymap."
  (interactive (list
                (completing-read "keymap: " obarray
                                 nil nil
                                 (symbol-name (symbol-or-function-near-point)))))
  (when (stringp map) (setq map (intern map)))
  (when (symbolp map) (setq map (eval map)))
  (move-end-of-line 1)
  (newline)
  (insert (sprint-keymap map)))

(defun extend-to-column (&optional col set-fill-column)
  "Extend line to column COL by adding spaces, if necessary.

Interactively, COL is provided as a prefix argument. If COL is omitted, the
value of `fill-column' is used.

If SET-FILL-COLUMN is true, or if the prefix argument is negative, the (positive)
value of COL is additionally set as the new value of `fill-column'."
  (interactive "p")
  (when (or (null col)
            (and (called-interactively-p) (null current-prefix-arg)))
    (setq col fill-column))
  (when (and (called-interactively-p) (< col 0))
    (setq set-fill-column t)
    (setq col (abs col)))
  (when set-fill-column
    (setf fill-column col))
  (move-to-column col t))

(defun spacemacs-rgrep (regexp)
  (interactive "sREGEXP: ")
  (rgrep regexp
         ".spacemacs* *.el"
         (expand-file-name user-emacs-directory)))

(defun prettyexpand-at-point ()
  "Pretty print macro-expansion of sexp at point.

Inserts the expansion on a new line at the end of the sexp."
  (interactive)
  (let ((sexp (sexp-at-point)))
    (forward-sexp)
    (newline)
    (insert (cl-prettyexpand sexp))))


  ;; TODO: write replace-line function.

;; -------------------------------------------------------------------------------
;; ,-----------------------,
;; | Temporary Workarounds |
;; '-----------------------'

;; ==================================
;; fix deprecated 'avy--with-avy-keys
;; ==================================
(eval-after-load "avy"
  '(when (and (funboundp 'avy--with-avy-keys)
              (fboundp   'avy-with))
     (defalias 'avy--with-avy-keys 'avy-with)))


;; ==========================================================
;; describe-symbol command taken from emacs 25 sources (GPL3)
;; ==========================================================
(global-set-key (kbd "C-h y") 'describe-symbol)

;;;###autoload
(defun describe-symbol (symbol &optional buffer frame)
  "Display the full documentation of SYMBOL.
Will show the info of SYMBOL as a function, variable, and/or face."
  (interactive
   ;; FIXME: also let the user enter a face name.
   (let* ((v-or-f (variable-at-point))
          (found (symbolp v-or-f))
          (v-or-f (if found v-or-f (function-called-at-point)))
          (found (or found v-or-f))
          (enable-recursive-minibuffers t)
          val)
     (setq val (completing-read (if found (format "Describe symbol (default %s): " v-or-f)
                                  "Describe symbol: ")
                                obarray
                                (lambda (vv) (cl-some (lambda (x) (funcall (nth 1 x) vv))
                                                      describe-symbol-backends))
                                t
                                nil
                                nil
                                (if found (symbol-name v-or-f))))
     (list (if (equal val "")
               v-or-f (intern val)))))
  (if (not (symbolp symbol))
      (user-error "You didn't specify a function or variable"))
  (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
  (unless (frame-live-p frame) (setq frame (selected-frame)))
  (with-current-buffer (help-buffer)
    ;; Push the previous item on the stack before clobbering the output buffer.
    (help-setup-xref nil nil)
    (let* ((docs
            (nreverse
             (delq nil
                   (mapcar (pcase-lambda (`(,name ,testfn ,descfn))
                                         (when (funcall testfn symbol)
                                           ;; Don't record the current entry in the stack.
                                           (setq help-xref-stack-item nil)
                                           (cons name
                                                 (funcall descfn symbol buffer frame))))
                           describe-symbol-backends))))
           (single (null (cdr docs))))
      (while (cdr docs)
        (goto-char (point-min))
        (let ((inhibit-read-only t)
              (name (caar docs))        ;Name of doc currently at BOB.
              (doc (cdr (cadr docs))))  ;Doc to add at BOB.
          (insert doc)
          (delete-region (point) (progn (skip-chars-backward " \t\n") (point)))
          (insert "\n\n"
                  (eval-when-compile
                    (propertize "\n" 'face '(:height 0.1 :inverse-video t)))
                  "\n")
          (when name
            (insert (symbol-name symbol)
                    " is also a " name "." "\n\n")))
        (setq docs (cdr docs)))
      (unless single
        ;; Don't record the `describe-variable' item in the stack.
        (setq help-xref-stack-item nil)
        (help-setup-xref (list #'describe-symbol symbol) nil))
      (goto-char (point-min)))))

(defvar describe-symbol-backends
  `((nil ,#'fboundp ,(lambda (s _b _f) (describe-function s)))
    ("face" ,#'facep ,(lambda (s _b _f) (describe-face s)))
    (nil
     ,(lambda (symbol)
        (or (and (boundp symbol) (not (keywordp symbol)))
            (get symbol 'variable-documentation)))
     ,#'describe-variable)))

;;;###autoload
(defmacro pcase-lambda (lambda-list &rest body)
  "Like `lambda' but allow each argument to be a pattern.
I.e. accepts the usual &optional and &rest keywords, but every
formal argument can be any pattern accepted by `pcase' (a mere
variable name being but a special case of it)."
  (declare (doc-string 2) (indent defun)
           (debug ((&rest pcase-PAT) body)))
  (let* ((bindings ())
         (parsed-body (macroexp-parse-body body))
         (args (mapcar (lambda (pat)
                         (if (symbolp pat)
                             ;; Simple vars and &rest/&optional are just passed
                             ;; through unchanged.
                             pat
                           (let ((arg (make-symbol
                                       (format "arg%s" (length bindings)))))
                             (push `(,pat ,arg) bindings)
                             arg)))
                       lambda-list)))
    `(lambda ,args ,@(car parsed-body)
       (pcase-let* ,(nreverse bindings) ,@(cdr parsed-body)))))

;;;###autoload
(defun macroexp-parse-body (body)
  "Parse a function BODY into (DECLARATIONS . EXPS)."
  (let ((decls ()))
    (while (and (cdr body)
                (let ((e (car body)))
                  (or (stringp e)
                      (memq (car-safe e)
                            '(:documentation declare interactive cl-declare)))))
      (push (pop body) decls))
    (cons (nreverse decls) body)))

;; -------------------------------------------------------------------------------
;; ,-------------------,
;; | Load Private Data |
;; '-------------------'
(load "~/.emacs.d/private/private-data.el")

  )



;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/.cache/bookmarks")
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(paradox-automatically-star t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
