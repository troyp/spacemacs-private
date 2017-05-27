;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; **********
;; *        *
;; * LAYERS *
;; *        *
;; **********

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

   ;; dotspacemacs-configuration-layer-path '()

   ;; -------------------------------------------------------------------------------
   ;; ,----------------------,
   ;; | Configuration Layers |
   ;; '----------------------'
   ;;
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     php
     csv
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
     graphviz
     (haskell :variables
              haskell-enable-shm-support t
              )
     html
     ;; ivy
     java
     javascript
     lua
     markdown
     ocaml
     octave
     (org :variables
          org-enable-github-support t
          )
     nlinum
     python
     racket
     (ranger :variables
             ranger-show-preview t
             ranger-cleanup-eagerly t
             ranger-show-dotfiles t
             ranger-ignored-extensions '("mkv" "iso" "mp4" "avi")
             ranger-max-preview-size 10)
     (ruby :variables
           ruby-version-manager 'rvm
           inf-ruby-default-implementation "pry"
           )
     rust
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
     ;; LOCAL LAYERS
     drag-n-drop
     no-dots
     troyp
     )

   ;; -------------------------------------------------------------------------------
   ;; ,---------------------,
   ;; | Additional Packages |
   ;; '---------------------'
   ;;
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     ;; ---------
     ;; libraries
     ;; ---------
     cl-lib-highlight
     (dash            :location (recipe :fetcher github :repo "magnars/dash.el" :files ("dash.el")))
     (dash-functional :location (recipe :fetcher github :repo "magnars/dash.el" :files ("dash-functional.el")))
     diff-hl
     f
     general
     google-this
     ivy
     names
     s
     tiny
     ;; TODO: work out why local packages aren't working
     ;; ;; Local
     ;; (dired+ :location local)
     ;; (find-func+ :location local)
     ;; (firefox-protocol :location local)
     ;; ;; My Local
     ;; (ibuffer-hydra :location local)
     ;; (t :location local)
     ;; ----------------------------------------------------------------------
     ;; -----------
     ;; My Packages
     ;; -----------
     evil-visual-replace
     ;; (evil-visual-replace :local :location "~/.emacs.d/private/repos/evil-visual-replace/evil-visual-replace.el")
     ;; (fn :location (recipe :fetcher github :repo "troyp/fn.el" :files ("fn.el")))
     fn
     (ls :location (recipe :fetcher github :repo "troyp/ls.el" :files ("ls.el")))
     (evil-adjust :location (recipe :fetcher github :repo "troyp/evil-adjust"))
     ;; --------
     ;; My Forks
     ;; --------
     (undo-tree :location (recipe :fetcher github :repo "troyp/undo-tree.el"))
     ;; ----------------------------------------------------------------------
     ;; -------------------
     ;; Drew Adams Packages
     ;; -------------------
     autofit-frame
     ;; bookmark+
     (dired+ :variables
             diredp-hide-details-initially-flag t
             diredp-hide-details-propagate-flag t
             )
     dired-sort-menu+
     doremi doremi-cmd doremi-frm doremi-mac
     eyedropper
     facemenu+
     faces+
     fit-frame
     font-lock+
     frame-cmds frame-fns
     help-fns+ help-mode+ help+
     hexrgb
     highlight
     isearch+ isearch-prop
     lacarte
     naked
     nim-mode
     palette
     replace+ strings
     thingatpt+
     thumb-frm
     ucs-cmds
     wid-edit+
     zoom-frm
     ;; ----------------------------------------------------------------------
     ;; --------------
     ;; other packages
     ;; --------------
     ;; quelpa-use-package
     dired-sort-menu
     column-enforce-mode
     dirtree
     elmacro
     evil-textobj-column
     firefox-controller
     flycheck-package
     (goto-gem :location (recipe :fetcher github :repo "pidu/goto-gem"))
     helm-firefox
     jsfmt
     mozc
     move-dup
     nameless
     top-mode
     )

   ;; -------------------------------------------------------------------------------
   ;; ,-------------------,
   ;; | Excluded Packages |
   ;; '-------------------'
   ;;
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))


;; ********
;; *      *
;; * INIT *
;; *      *
;; ********
;;
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
   dotspacemacs-startup-banner "~/.emacs.d/core/banners/oo1-banner.txt"
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
                         zenburn
                         moe-light
                         soothe
                         afternoon
                         alect-black
                         alect-light
                         sanityinc-tomorrow-blue
                         sanityinc-tomorrow-eighties
                         )
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

   ;; -------------------------------------------------------------------------------
   ;; ,------------,
   ;; | User-Added |
   ;; '------------'
   ;; TODO: remove when possible
   ;; workaround for github.com/syl20bnr/spacemacs/issues/8091
   dotspacemacs-helm-use-fuzzy 'source
   ))


;; *************
;; *           *
;; * USER-INIT *
;; *           *
;; *************
;;
(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;;
  ;; Source directory
  (setq source-directory "/opt/emacs25/src")
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "private/themes/"))
  )


;; ============================================================================
;; =========================== ******************** ===========================
;; =========================== *                  * ===========================
;; =========================== * SPACEMACS CONFIG * ===========================
;; =========================== *                  * ===========================
;; =========================== ******************** ===========================
;; ============================================================================
;;
(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place you code here."

  ;; (setq-default tab-always-indent t)
  (global-nlinum-mode)
  (menu-bar-mode)
  (scroll-bar-mode)
  (minibuffer-depth-indicate-mode 1)
  (setq evil-search-highlight-persist nil)
  (setq left-fringe-width 16)    ;; displays diff-hl mode well

  ;; not working?
  (setq-default evil-esc-delay 0.00001)

  (setq auto-completion-enable-help-tooltip t)

  (setq scroll-preserve-screen-position 1)

  (defvar spacemacs-private-directory
    (expand-file-name (file-name-as-directory "private") user-emacs-directory)
    "Location of Spacemacs private/ directory.")

  (defvar recentf-save-file
    (expand-file-name ".cache/recentf" spacemacs-private-directory)
    "Location of the save-file used by `recentf-mode'.")

  (setq bookmark-default-file (concat-as-file-path spacemacs-private-directory
                                                   ".cache" "bookmarks"))

  (setq srecode-map-save-file
        (concat-as-file-path spacemacs-private-directory "srecode-map.el"))

  (setq-default evil-lookup-func
                (defun man-interactive ()
                  (call-interactively 'man)))

  ;; disable warnings about setting path in rc files (caused by nvm or rvm)
  (setq exec-path-from-shell-check-startup-files nil)

  ;; follow VC'ed symlinks
  (setq vc-follow-symlinks t)

  ;; undo
  (setq undo-outer-limit 50000000)

  ;; ,---------------,
  ;; | CUA Rectangle |
  ;; '---------------'

  (setq cua-enable-cua-keys nil)
  (cua-mode t)
  ;; stop cua breaking C-x C-x in visual-line selections
  (add-hook 'cua-mode-hook
            (fn: define-key cua-global-keymap [remap exchange-point-and-mark] nil))
  (define-key cua-global-keymap [remap exchange-point-and-mark] nil)

  ;; ,-----------------,
  ;; | CUA Global Mark |
  ;; '-----------------'

  (defun my/cua-global-mark-remove-nonregion-remappings ()
    (interactive)
    (define-key cua--global-mark-keymap [remap self-insert-command] nil)
    (define-key cua--global-mark-keymap [remap backward-delete-char] nil)
    (define-key cua--global-mark-keymap [remap backward-delete-char-untabify] nil)
    (define-key cua--global-mark-keymap [t]
      '(menu-item "sic" nil :filter cua--self-insert-char-p))
    (define-key cua--global-mark-keymap [remap newline] nil)
    (define-key cua--global-mark-keymap [remap newline-and-indent] nil)
    (define-key cua--global-mark-keymap "\r" nil)
    (define-key cua--global-mark-keymap "\t" nil))

  (use-package cua-gmrk
    :init (my/cua-global-mark-remove-nonregion-remappings)
    )

  ;; AVY KEYS
  (setq avy-keys (list 97 115 100 102 106 107 108 59))

  ;; global-hl-line
  (require 'color)
  (defun auto-set-hl-line-face ()
    "Set `hl-line' face to slightly darker than default background."
    (set-face-attribute 'hl-line
                        nil    ;; all frames
                        :foreground nil
                        :background (color-darken-name (face-background 'default nil 'default)
                                                       20)
                        ))
  (add-hook 'global-hl-line-mode-hook 'auto-set-hl-line-face)

  ;; ==============================================================================
  ;; ***************
  ;; *             *
  ;; * ENVIRONMENT *
  ;; *             *
  ;; ***************

  ;; (setenv "PATH" (concat "/home/troy/.nvm/versions/node/v0.12.7/bin" ":" (getenv "PATH")))
  ;; (add-to-list 'exec-path "/home/troy/.nvm/versions/node/v0.12.7/bin")

  ;; disable warning about setting PATH in .zshrc
  (setq exec-path-from-shell-check-startup-files nil)

  ;; C Source Directories
  (setq find-function-C-source-directory "/opt/emacs25/src")

  ;; Info directories
  (let ((extra-Info-dirs
         (list
          ;; "/usr/share/info/emacs-24"
          (concat source-directory "info")
          "/usr/share/info/"
          "/opt/info/"
          )))
    (dolist (dir extra-Info-dirs)
      (add-to-list 'Info-directory-list dir)))

  ;; Shell-command switches
  (setq shell-command-switch "-ic")

  ;; ===============================================================================
  ;;                                      ________
  ;;                                     |        |
  ;;                                     | THEMES |
  ;;                                     |________|

  (spacemacs|define-transient-state my/cycle-theme
    :title "Theme Cycling Transient State"
    :doc "\n[_n_] next [_p_] previous [_0_] reset [_q_] quit"
    :bindings
    ("n" spacemacs/cycle-spacemacs-theme)
    ("p" my/spacemacs/reverse-cycle-spacemacs-theme)
    ("0" my/spacemacs/reset-spacemacs-theme)
    ("q" nil :exit t))
  (spacemacs/set-leader-keys "Tn" 'spacemacs/my/cycle-theme-transient-state/body)
  (defun my/spacemacs/reverse-cycle-spacemacs-theme ()
    "Reverse cycle through themes defined in `dotspacemacs-themes.'"
    (interactive)
    (if spacemacs--cur-theme (disable-theme spacemacs--cur-theme)
      (setq spacemacs--cur-theme (car dotspacemacs-themes)))
    (add-to-list 'spacemacs--cycle-themes spacemacs--cur-theme)
    (setq spacemacs--cur-theme
          (let ((i (-elem-index spacemacs--cur-theme dotspacemacs-themes)))
            (if (> i 0)
                (nth (1- i) dotspacemacs-themes)
              (-last-item dotspacemacs-themes))))
    (message "Loading theme %s..." spacemacs--cur-theme)
    (spacemacs/load-theme spacemacs--cur-theme))
  (defun my/spacemacs/reset-spacemacs-theme ()
    (interactive)
    (when spacemacs--cur-theme (disable-theme spacemacs--cur-theme))
    (setq spacemacs--cur-theme 'spacemacs-dark)
    (setq spacemacs--cycle-themes nil)
    (message "Loading theme %s..." spacemacs--cur-theme)
    (spacemacs/load-theme spacemacs--cur-theme))

  ;; ==============================================================================
  ;;                       *****************************
  ;;                       *                           *
  ;;                       * ADDITIONAL LOCAL PACKAGES *
  ;;                       *                           *
  ;;                       *****************************

  (add-to-load-path "~/.emacs.d/private/local/")
  (add-to-load-path "~/.emacs.d/private/local/firefox-protocol")
  ;; MY PACKAGES
  (add-to-load-path "~/.emacs.d/private/local/t")

  (defvar dotspacemacs-additional-local-packages)
  (setf dotspacemacs-additional-local-packages
        '(
          find-func+
          firefox-protocol
          help-macro+
          ibuffer-hydra
          t
          ))
  (loop for pkg in dotspacemacs-additional-local-packages do
        (require pkg nil :noerror))

  (add-to-load-path "/home/troy/.emacs.d/private/repos/elisp-utils/")
  (require 'elisp-utils)
  (add-to-load-path "/home/troy/.emacs.d/private/repos/tempgit/")
  (require 'tempgit)
  (add-to-load-path "/home/troy/.emacs.d/private/repos/asoc.el/")
  (add-to-load-path "/home/troy/.emacs.d/private/repos/asciiboxes/")
  (add-to-load-path "/home/troy/.emacs.d/private/repos/tempgit/")

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
  (add-to-list 'auto-mode-alist '("\\.vimpagerrc" . vimrc-mode))
  (add-to-list 'auto-mode-alist '("\\.jshintrc" . json-mode))


  ;; ==============================================================================
  ;;                                *************
  ;;                                *           *
  ;;                                * EVIL-MODE *
  ;;                                *           *
  ;;                                *************

  ;; ;; prevent cursor from moving back a space at the end of a line
  ;; (setq evil-move-cursor-back nil)

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

  ;; ,--------------,
  ;; | Text Objects |
  ;; '--------------'

  (spacemacs|define-text-object "." "dot" "." ".")
  (spacemacs|define-text-object "h" "helplink" "`" "'")
  (spacemacs|define-text-object "q" "curlquote" "‘" "’")

  (evil-define-text-object evil-inner-defun (count &optional beg end type)
    "operates on the top-level sexp around point."
    (save-excursion
      (mark-defun)
      (list (+ (point) 2)
            (- (mark) 2))))
  (evil-define-text-object evil-outer-defun (count &optional beg end type)
    "operates on the top-level sexp around point."
    (save-excursion
      (mark-defun)
      (list (+ (point) 1)
            (- (mark) 1))))

  (evil-define-text-object evil-inner-filename (count &optional beg end type)
    "operates on the filename around point."
    (-cons-to-list (bounds-of-thing-at-point 'filename)))

  (evil-define-text-object evil-inner-line (count &optional beg end type)
    (list (my/line-visible-beginning-position) (my/line-visible-end-position)))
  (evil-define-text-object evil-outer-line (count &optional beg end type)
    (list (line-beginning-position) (line-end-position)))

  (evil-define-text-object evil-inner-sexp  (count &optional beg end type)
    (let ((sexp-bounds (cdr (sexp-at-point-with-bounds))))
      (list (1+ (car sexp-bounds)) (1- (cdr sexp-bounds)))))
  (evil-define-text-object evil-outer-sexp  (count &optional beg end type)
    (let ((sexp-bounds (cdr (sexp-at-point-with-bounds))))
      (list (car sexp-bounds) (cdr sexp-bounds))))

  ;; heredoc object: define properties for thingatpt
  (put 'heredoc 'beginning-op
       (lambda ()
         (search-backward-regexp "<< *EOF" nil t)
         (forward-line)))
  (put 'heredoc 'end-op
       (lambda ()
         (search-forward-regexp "^EOF$" nil t)
         (forward-line -1)
         (end-of-line)
         (forward-char)))
  (put 'heredoc 'forward-op
       (lambda (count)
         (search-forward-regexp "^EOF$" nil t count)
         (forward-line)))
  ;; define evil-inner-textobject in terms of thing at pt
  (evil-define-text-object evil-inner-heredoc (count &optional beg end type)
    "operates on the heredoc around point."
    (-cons-to-list (bounds-of-thing-at-point 'heredoc)))

  (define-key evil-inner-text-objects-map "d" 'evil-inner-defun)
  (define-key evil-outer-text-objects-map "d" 'evil-outer-defun)
  (define-key evil-inner-text-objects-map "f" 'evil-inner-filename)
  (define-key evil-inner-text-objects-map "l" 'evil-inner-line)
  (define-key evil-outer-text-objects-map "l" 'evil-outer-line)
  (define-key evil-inner-text-objects-map "<" 'evil-inner-heredoc)
  (define-key evil-inner-text-objects-map "x" 'evil-inner-sexp)
  (define-key evil-outer-text-objects-map "x" 'evil-outer-sexp)
  ;; evil-textobj-column
  (define-key evil-inner-text-objects-map "C" 'evil-textobj-column-WORD)

  ;; ,--------,
  ;; | cursor |
  ;; '--------'
  (defun my/evil-insert-overwrite-cursor ()
    (set-cursor-color "chartreuse3")
    (setq cursor-type (if overwrite-mode (cons 'hbar 2) (cons 'bar 2))))

  (setq evil-insert-state-cursor #'my/evil-insert-overwrite-cursor)

  (defun my/enter-overwrite-mode ()
    (interactive)
    (call-interactively #'overwrite-mode)
    (evil-refresh-cursor))

  (define-key evil-insert-state-map (kbd "<insert>") 'my/enter-overwrite-mode)


  ;; ===============================================================================
  ;;                                      _________
  ;;                                     |         |
  ;;                                     | UNICODE |
  ;;                                     |_________|

  ;; ,----------,
  ;; | Digraphs |
  ;; '----------'

  (setq evil-digraphs-table-user
        '(
          ((?. ? ) . ?\x2024)    ;; ․ 1-dot leader
          ((?. ?/) . ?\x2025)    ;; ‥ 2-dot leader
          ;; compose combinations
          ((?. ?.) . ?\x2026)    ;; (horizontal) ellipsis (replaces ‥)
          ((?0 ?-) . ?\x30fb)    ;; CJK middle-dot
          ((?. ?-) . ?\x00b7)    ;; middle-dot
          ((?i 34) . ?\x00ef)    ;; ï (also on i:)
          ((?t ?-) . ?\x0167)    ;; ŧ
          ;; kragen compose combinations
          ((?t ?,) . ?\x0288)    ;; ʈ (replaces ţ which is also available on ,t)
          ((?, ?t) . ?\x0163)    ;; ţ (default, but obscured by binding above)
          ((?, ?T) . ?\x0162)    ;; Ţ (default, but obscured by binding above)
          ((?n ?,) . ?\x0273)    ;; ɳ (replaces ţ which is also available on ,n)
          ((?, ?n) . ?\x0146)    ;; ņ (default, but obscured by binding above)
          ((?, ?N) . ?\x0145)    ;; Ņ (default, but obscured by binding above)
          ))

  (defun my/evil-enter-digraphs ()
    "Enter digraphs until `undo' is triggered."
    (interactive)
    (while t (evil-insert-digraph 1)))

  (defun my/evil-enter-digraphs-default ()
    "Enter default digraphs until `undo' is triggered."
    (interactive)
    (let ((evil-digraphs-table-user nil))
      (while t (evil-insert-digraph 1))))

  ;; TODO: generate digraphs table with unicode names and descriptions
  (defun helm-occur-digraphs ()
    (interactive)
    (evil-ex-show-digraphs)
    (helm-occur))


  ;; ,-------------,
  ;; | Compose Key |
  ;; '-------------'

  (define-helm-occur-function "compose"
    "/usr/share/X11/locale/en_US.UTF-8/Compose")



  ;; ==============================================================================
  ;;                                 ****************
  ;;                                 *              *
  ;;                                 * KEY-BINDINGS *
  ;;                                 *              *
  ;;                                 ****************

  (defmacro my/def-variable-toggle (var)
    (let* ((fname `(concat "my/toggle-" (symbol-name ',var)))
           (fsym  (intern (eval fname))))
      `(defun ,fsym ()
         "Defined with `my/def-variable-toggle'."
         (interactive)
         (setq ,var (not ,var))
         (message "%S is %s" ',var (if ,var "enabled" "disabled")))))

  ;; ,----------------------,
  ;; | Keybinding Functions |
  ;; '----------------------'
  ;; spacemacs macros:   evil-map evil-define-key evil-define-minor-mode-key
  ;;                     evil-define-keymap spacemacs|define-micro-state
  ;; bindkey fns/macros: bind-map bind-key bind-key* bind-keys bind-keys*
  ;;                     bind-keys-form bind-map-add-to-major-mode-list
  ;;                     bind-map-set-keys bind-map-kbd-keys unbind-key
  ;; builtin: global-key-binding local-set-key local-unset-key
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

  (defmacro fn! (&rest body) `(lambda () (interactive) ,@body))

  ;; ,-----------------,
  ;; | Global Bindings |
  ;; '-----------------'

  ;; ===== High Priority: override mode bindings =====
  (bind-key* "M-0" 'universal-argument)
  (bind-key* "<C-tab>" 'next-multiframe-window)
  (bind-key* "<C-S-iso-lefttab>" 'previous-multiframe-window)
  (bind-key* "<M-delete>" 'my/kill-buffer-quit-help)
  (bind-key* "<M-S-delete>" 'kill-buffer-and-window)
  (bind-key* "C-:" 'execute-extended-command)
  (bind-key* "M-X" 'spacemacs/smex)
  (bind-key* "M-x" 'helm-M-x)
  ;; =================================================

  (global-set-key (kbd "C-h y") 'describe-symbol)

  (global-set-key (kbd "M-S-x") 'execute-extended-command)

  (global-set-key [\S-f4] 'my/launch-standalone-terminal)

  (global-set-key [\M-f4] 'kill-buffer-and-window)

  ;; change C-x - from 'shrink-window-if-larger-than-buffer to 'fit-window-to-buffer
  (global-set-key (kbd "\C-x -") 'fit-window-to-buffer)

  (global-set-key (kbd "M-n") 'evil-scroll-line-down)
  (global-set-key (kbd "M-p") 'evil-scroll-line-up)
  (global-set-key (kbd "C-S-j")
                  (lambda () (interactive) (scroll-other-window 1)))
  (global-set-key (kbd "C-S-k")
                  (lambda () (interactive) (scroll-other-window-down 1)))
  (global-set-key (kbd "C-M-d") 'scroll-other-window)
  (global-set-key (kbd "C-M-u") 'scroll-other-window-down)
  (global-set-key (kbd "C-M-S-d") 'scroll-other-window-down)
  (global-set-key (kbd "M-J") 'scroll-up-line)
  (global-set-key (kbd "M-K") 'scroll-down-line)

  (global-set-key (kbd "M-[") (fn! (forward-symbol -1)))
  (global-set-key (kbd "M-]") 'forward-symbol)

  (global-set-key (kbd "<C-return>") 'evil-cua-toggle)

  (global-set-key "\C-a" 'move-beginning-of-line-or-text)    ;; troyp/utils.el
  (global-set-key (kbd "<S-return>") 'open-line-below)       ;; troyp/utils.el
  (global-set-key (kbd "<C-S-return>") 'open-line-above)     ;; troyp/utils.el
  (global-set-key [\C-\S-down] 'move-text-down)
  (global-set-key [\C-\S-up]   'move-text-up)


  ;; remove C-S-SPC from cua-global-keymap and bind to my/insert-space-after
  (define-key cua-global-keymap (kbd "C-S-SPC") nil)
  (global-set-key (kbd "C-S-SPC") 'my/insert-space-after)
  ;; This binding is intercepted by UIM

  ;; just-one-space
  ;; This is available by default, since it translates to M-SPC (which can't be used itself since it's intercepted by KWin)
  ;; This just makes it explicit
  (global-set-key (kbd "M-S-SPC") 'just-one-space)

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

  (global-set-key (kbd "C-x C-c")
                  (fn! (when (y-or-n-p "Exit? ")
                         (call-interactively #'save-buffers-kill-terminal))))

  (bind-key* "C-M-x" 'helm-eval-expression-with-eldoc)

  (use-package google-this :bind-keymap ("C-c /" . google-this-mode-submap))

  (bind-keys :map help-map
             ("C-k" . find-function-on-key)
             )

  (bind-keys :map global-map
             :prefix-map snippets-prefix-map
             :prefix "M-<f3>"
             :prefix-docstring "Snippets and templates commands"
             ("c"      . aya-create)
             ("x"      . aya-expand)
             ("s"      . aya-yank-snippet)
             ("M-<f3>" . aya-open-line)
             ("a"      . aya-create-one-line)
             ("n"      . aya-create-snippet-with-newline)
             ("<tab>"  . helm-yas-complete)
             ("y"      . yas-new-snippet)
             ("v"      . yas-visit-snippet-file)
             )
  (defun aya-create-snippet-with-newline ()
    (interactive)
    (let ((aya-create-with-newline t))
      (call-interactively 'aya-create)))


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
  (define-key evil-normal-state-map [S-delete] 'delete-window)
  (define-key evil-normal-state-map [C-delete] 'kill-buffer-and-window)
  ;; shift reverses C-d (-scroll-down) and C-o (-jump-backward)
  (define-key evil-normal-state-map (kbd "C-S-d") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-S-o") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  ;; remove C-y (use global M-p)
  (define-key evil-normal-state-map (kbd "C-y") nil)
  ;; reverse gu and gU
  (define-key evil-normal-state-map (kbd "gu") 'evil-upcase)
  (define-key evil-normal-state-map (kbd "gU") 'evil-downcase)
  ;; centre after n, N, '
  (define-key evil-normal-state-map (kbd "n") 'my/evil-search-next-and-center)
  (define-key evil-normal-state-map (kbd "N") 'my/evil-search-previous-and-center)
  (define-key evil-normal-state-map (kbd "'") 'my/evil-goto-mark-line)
  (define-key evil-normal-state-map (kbd "gi") 'my/evil-insert-resume)

  ;; PCRE search forward/backward
  (define-key evil-normal-state-map (kbd "/") 'my/evil-pcre-search-forward)
  (define-key evil-normal-state-map (kbd "?") 'my/evil-pcre-search-backward)

  ;; provide evil-repeat-find-char-reverse binding
  (define-key evil-normal-state-map (kbd "M-;") 'evil-repeat-find-char-reverse)
  ;; [r and ]r move to beginning and end of region
  (define-key evil-normal-state-map (kbd "[r") 'evil-visual-jump-to-region-beginning)
  (define-key evil-normal-state-map (kbd "]r") 'evil-visual-jump-to-region-end)
  ;; evil-symbol-word-search
  (define-key evil-normal-state-map (kbd "C-*") 'toggle-evil-symbol-word-search)

  ;; universal-argument
  (define-key evil-normal-state-map (kbd "C-S-u") 'universal-argument)
  ;; 'negative-argument is also on C-M--
  ;; M-- was bound to 'ahs-back-to-start but it doesn't seem to work
  (define-key evil-normal-state-map (kbd "M--") 'negative-argument)
  ;; fix SPC key after C-u in insert-state
  (define-key universal-argument-map (kbd " ") 'self-insert-and-exit)

  (define-key evil-normal-state-map (kbd "C--") 'spacemacs/evil-numbers-decrease)
  (define-key evil-normal-state-map (kbd "C-=") 'spacemacs/evil-numbers-increase)

  ;; evil-shift-up/down-line-or-block
  (define-key evil-normal-state-map [\M-\S-down] 'evil-shift-down-line-or-block)
  (define-key evil-normal-state-map [\M-\S-up] 'evil-shift-up-line-or-block)
  ;; insert at WORD beginning/end
  (define-key evil-normal-state-map (kbd "M-B") 'my/evil-insert-at-WORD-beginning)
  (define-key evil-normal-state-map (kbd "M-E") 'my/evil-append-at-WORD-end)
  ;; forward/backward sentence
  (define-key evil-normal-state-map (kbd "M-e") 'evil-forward-sentence-begin)
  (define-key evil-normal-state-map (kbd "M-a") 'evil-backward-sentence-begin)

  ;; evil-arg
  (define-key evil-normal-state-map (kbd "M-j") 'evil-forward-arg)
  (define-key evil-normal-state-map (kbd "M-k") 'evil-backward-arg)

  ;; evil-window: C-w C-w to swap windows
  (bind-key "C-w" #'my/window-swap-with-next evil-window-map)

  ;; save
  (define-key evil-normal-state-map "ZC" 'save-buffers-kill-terminal)

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
  (define-key evil-visual-state-map (kbd "C-\\") 'my/shell-command-replace-region)
  (define-key evil-visual-state-map (kbd "M-u") 'evil-upcase)
  (define-key evil-visual-state-map (kbd "M-l") 'evil-downcase)
  (define-key evil-visual-state-map (kbd "M-=") 'count-region)
  (define-key evil-visual-state-map (kbd ".") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "M-.") 'er/contract-region)
  (define-key evil-visual-state-map (kbd "O") 'evil-visual-rotate)
  (evil-visual-replace-visual-bindings :PCRE)


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
  ;; define bindings for shift-right/left-line: frees C-d, C-t:
  (define-key evil-insert-state-map (kbd "S-<tab>")       'evil-shift-right-line)
  ;; (define-key evil-insert-state-map (kbd "S-<backspace>") 'evil-shift-left-line)
  (define-key evil-insert-state-map (kbd "C-d")   'delete-forward-char)
  (define-key evil-insert-state-map (kbd "C-l")   'delete-forward-char)
  (define-key evil-insert-state-map (kbd "C-S-l") 'backward-delete-char)
  (define-key evil-insert-state-map (kbd "M-h")   'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-t")   'transpose-chars)
  (define-key evil-insert-state-map (kbd "C-S-a") 'evil-paste-last-insertion)
  (define-key evil-insert-state-map (kbd "C-a")   'move-beginning-of-line-or-text)
  (define-key evil-insert-state-map (kbd "C-e")   'end-of-line)
  (define-key evil-insert-state-map (kbd "C-S-y") 'evil-copy-from-below)
  (define-key evil-insert-state-map (kbd "C-S-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-.")   'tsp-yas-expand)
  (define-key evil-insert-state-map (kbd "M-?")   'dabbrev-expand)
  (define-key evil-insert-state-map (kbd "C-n")   'next-line)
  (define-key evil-insert-state-map (kbd "C-p")   'previous-line)

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

  (spacemacs/set-leader-keys
    "a ="          'calculator
    "a '"          'browse-url-firefox
    "b M"          'my/switch-to-messages-buffer
    "b S"          'my/switch-to-scratch-buffer-other-window
    "b W"          'my/switch-to-warnings-buffer
    "b -"          'diff-buffer-with-file
    "b +"          'my/switch-to-changelog-buffer
    "b SPC"        'spacemacs/new-empty-buffer
    "b C-b"        'ibuffer
    "b C-e"        'bury-buffer
    "b C-f"        'buffer-face-set
    "b C-u"        'undo-tree-clear
    "b <insert>"   'buffer-major-mode
    "b <f1>"       'about-emacs
    "e F"          'flycheck-mode
    "e TAB"        'flycheck-next-error
    "e S-TAB"      'flycheck-previous-error
    "f e s s"      'spacemacs-rgrep
    "f e s p"      'spacemacs-private-rgrep
    "f e s o"      'spacemacs-only-rgrep
    "f e s e"      'elpa-rgrep
    "f ."          'find-alternate-file
    "f >"          'find-alternate-file-other-window
    "f ' e"        'dired-spacemacs-directory
    "f ' p"        'dired-spacemacs-private-directory
    "f / f"        'sudo-open-file
    "f / e"        'spacemacs/sudo-edit
    "f / b"        'sudo-edit-this-file
    "f SPC"        'my/open-file-at-point
    "f <insert>"   'find-file-clipboard
    "f C-."        'find-file-at-point
    "f C-k"        'bookmark-jump
    "g C-x v"      'vc-prefix-map
    ;; "h"            'help-prefix-map
    "h a"          'apropos
    "h d C-b"      'describe-personal-keybindings
    "h w"          'help-download-prefix-map
    "h ."          'count-words
    "h /"          'find-function-prefix-map
    "h \\"         'shell-command-on-region
    "h 1"          'evil-goto-definition
    "h C-f"        'find-library-other-window
    "h C-m"        'lacarte-execute-menu-command
    "h C-y"        'tsp-info-goto-node
    "h C-/"        'evil-search-highlight-persist-remove-all
    "h C-?"        'evil-search-highlight-restore
    ;; "h <f1>"       'help-map
    "i -"          'tiny-expand
    "m <f10>"      'my/menu-execute/lambda-l-and-exit
    "o a"          'asciiheadings-prefix-key-map
    "o c"          'character-prefix-map
    "o f"          'flycheck-command-map
    "o m"          'modes-prefix-key-map
    "r b"          'bookmark-map
    "s R R"          'pcre-multi-occur
    "s R r"          'pcre-occur
    "t O"          (my/def-variable-toggle which-key-show-operator-state-maps)
    "t T"          (my/def-variable-toggle indent-tabs-mode)
    "t 3"          'my/toggle-evil-mc-mode
    "t C-n"        'my/toggle-evil-mc-mode-and-cursor
    "t SPC"        (my/def-variable-toggle show-trailing-whitespace)
    "t '"          'variable-pitch-mode
    "t |"          'fci-mode
    "t ?"          'helm-descbinds-mode  ;; reactivated by helm - TODO: investigate
    "t :"          'nameless-mode
    "t ["          'diff-hl-flydiff-mode
    "t C-s"        'my/undo-auto-save-make-local-and-toggle
    "t C-/"        'evil-search-highlight-persist
    "t C-'"        (my/def-variable-toggle fit-frame-inhibit-fitting-flag)
    "T |"          'scroll-bar-mode
    "w TAB"        'ace-swap-window
    "w DEL"        'my/delete-window-ace-move-buffer-quit-help
    "x a ."        'spacemacs/align-repeat-period
    "x a '"        'spacemacs/align-repeat-quote
    "x a \""       'spacemacs/align-repeat-double-quote
    "x a -"        'spacemacs/align-repeat-dash
    "x a #"        'spacemacs/align-repeat-hash
    "x a C-;"      'spacemacs/align-repeat-semicolon-comment
    "x a C-/"      'spacemacs/align-repeat-slash-comment
    "x a C-'"      'tsp-align-quoted-column
    "x a C-\""     'tsp-align-double-quoted-column
    "x a SPC"      'quick-pcre-align-repeat
    "x a S-SPC"    'quick-pcre-align
    "x a C-SPC"    'pcre-align-region
    "x l U"        'delete-duplicate-lines-nonblank
    "x N"          'rectangle-number-lines-interactive
    "x <insert>"   'region-swap
    "x t r"        'region-swap
    "x C-k"        'evil-insert-digraph
    "x C-l"        'quick-pcre-align-repeat
    "x \\"         'my/evil-shell-command-replace-region
    "x |"          'my/shell-command-process-region-as-file
    "z +"          'spacemacs/scale-font-transient-state/spacemacs/scale-up-font
    "z ="          'spacemacs/scale-font-transient-state/spacemacs/scale-up-font
    "z -"          'spacemacs/scale-font-transient-state/spacemacs/scale-down-font
    "3"            'spacemacs/enter-ahs-backward
    "8"            'spacemacs/enter-ahs-forward
    "."            'repeat-complex-command
    ","            'helm-mini
    ">"            'evil-shift-right-fine-dispatcher
    "<"            'evil-shift-left-fine-dispatcher
    "="            'quick-calc
    "-"            'my/dired-replace-buffer
    "S-SPC"        'avy-goto-char-timer
    "SPC"          'avy-goto-char
    "<backtab>"    'switch-to-most-recent-buffer
    "<return>"     'helm-buffers-list
    "<f3>"         'kmacro-keymap
    "<f5>"         'spacemacs/safe-revert-buffer
    "<f10>"        'my/menu-execute/lambda-a-and-exit
    "C-l"          'quick-pcre-align-repeat
    "C-v"          'evil-cua-toggle
    "C-w"          'delete-frame
    "C-y"          'my/paste-no-properties
    "C-."          'ido-switch-buffer
    "C-/"          'evil-search-highlight-persist-remove-all
    "C-?"          'evil-search-highlight-restore
    "C-\\"         'set-input-method
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
             :prefix-map find-function-prefix-map
             :menu-name "find-function-"
             :prefix "h /"
             ("f" . find-function)
             ("k" . find-function-on-key)
             ("h" . describe-function)
             ("w" . find-function-other-window)
             ("W" . find-function-other-window-noselect)
             ("5" . find-function-other-frame)
             ("." . find-function-at-point)
             )

  (bind-keys :map spacemacs-cmds
             :prefix-map keymaps-prefix-map
             :menu-name "keys/keymaps"
             :prefix "K"
             :prefix-docstring "Commands dealing with keymaps."
             ("a" . which-key-show-keymap-at-point)
             ("e" . edmacro-insert-key)
             ("f" . get-binding)
             ("i" . lookup-key-interactive)
             ("m" . which-key-show-minor-mode-keymap)
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

  ;; =====DIFF PREFIX MAP=====
  (bind-keys :map spacemacs-cmds
             :prefix-map diff-prefix
             :menu-name "diff"
             :prefix "&"
             :prefix-docstring "Diff commands."
             ("f"   . diff-buffer-with-file)
             ("b"   . ediff-buffers)
             ("3"   . ediff-buffers3)
             )
  (bind-keys :map diff-prefix
             :prefix-map merge-prefix
             :menu-name "merge"
             :prefix "m"
             :prefix-docstring "Merge commands."
             ("m" . ediff-merge-files)
             ("M" . ediff-merge-files-with-ancestor)
             ("b" . ediff-merge-buffers)
             ("B" . ediff-merge-buffers-with-ancestor)
             ("d" . ediff-merge-directories)
             ("D" . ediff-merge-directories-with-ancestor)
             )
  (which-key-add-key-based-replacements
    "SPC &"    "diff"
    "SPC & m"  "merge"
    )

  (bind-keys :map spacemacs-cmds
             :prefix-map column-prefix
             :menu-name "column"
             :prefix "|"
             :prefix-docstring "Column commands."
             ("e" . my/extend-to-column)
             ("|" . my/add-column-marker)
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

  (bind-keys :map spacemacs-cmds
             :prefix-map match-lines-map
             :menu-name "match lines"
             :prefix "s L"
             :prefix-docstring "Commands matching lines against a pattern."
             ("m" . keep-lines)
             ("n" . flush-lines)
             ("c" . how-many)
             ("h" . highlight-lines-matching-regexp)
             ("y" . my/copy-matching-lines)
             ("Y" . my/copy-non-matching-lines)
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
    "z RET"        "MACRO: z t ^"
    "z <left>"     "MACRO: z h"
    "z <right>"    "MACRO: z l"
    "z -"          "MACRO: z b ^"
    "z ."          "MACRO: z z ^"
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
    "C-x *"        "calc-dispatch-map"
    "M-g"          "goto-map"
    "M-s"          "search-map"
    "SPC b h"      "*spacemacs*"
    "SPC b s"      "*scratch*"
    "SPC b M"      "*messages*"
    "SPC b <f1>"   "*About GNU Emacs*"
    "SPC f /"      "sudo"
    "SPC f '"      "dired"
    "SPC h /"      "find-function"
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
    "Set a sequence of docstring functions

\(fn [FN1 DOCSTR1] ...)"
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
  ;; ,--------------,
  ;; | evil-mc-mode |
  ;; '--------------'

  (defun my/toggle-evil-mc-mode()
    (interactive)
    (if evil-mc-mode
        (progn (evil-mc-undo-all-cursors)
               (evil-mc-mode 0)
               (message "evil-mc-mode disabled"))
      (evil-mc-mode 1)
      (message "evil-mc-mode enabled")))

  (defun my/toggle-evil-mc-mode-and-cursor()
    (interactive)
    (if evil-mc-mode
        (progn (evil-mc-undo-all-cursors)
               (evil-mc-mode 0)
               (message "evil-mc-mode disabled"))
      (evil-mc-mode 1)
      (evil-mc-make-and-goto-next-match)
      (message "evil-mc-mode enabled")))

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
  ;; Note: requires mozc_emacs_helper program -- debian pkg emacs-mozc-bin
  (require 'mozc)
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'overlay)
  ;; If Emacs doesn't recognize IM, make s-SPC toggle mozc-mode
  (global-set-key (kbd "s-SPC") 'mozc-mode)
  (setcar (cdr (assoc 'mozc-mode minor-mode-alist))
          "日")

  ;; -------------------------------------------------------------------------------
  ;; ,----------,
  ;; | Nameless |
  ;; '----------'
  (use-package nameless
    :config
    (setq nameless-private-prefix t)
    )

  ;; -------------------------------------------------------------------------------
  ;; ,-----------------,
  ;; | Whitespace-Mode |
  ;; '-----------------'
  (setq whitespace-style-heavy
        '(face tabs spaces trailing lines-tail space-before-tab newline indentation empty
               space-after-tab space-mark tab-mark newline-mark))
  (setq whitespace-style-light
        '(face tabs spaces trailing space-before-tab indentation space-after-tab space-mark tab-mark))
  (setq-default whitespace-style whitespace-style-light)

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
  ;; ,------,
  ;; | Calc |
  ;; '------'
  ;; calc: bypass calc-dispatch and bind C-x * directly to calc-dispatch-map
  ;;       (allow introspection of keybindings)
  (require 'calc)
  (global-set-key (kbd "C-x *") calc-dispatch-map)

  ;; -------------------------------------------------------------------------------
  ;; ,------------,
  ;; | Calculator |
  ;; '------------'

  (evil-set-initial-state 'calculator-mode 'emacs)

  ;; -------------------------------------------------------------------------------
  ;; ,-------,
  ;; | Dired |
  ;; '-------'

  (use-package dired+
    :init
    (spacemacs|add-toggle diredp-wrap-around-flag
      :status diredp-wrap-around-flag
      :on  (setq diredp-wrap-around-flag t)
      :off (setq diredp-wrap-around-flag nil)
      )
    (setq diredp-wrap-around-flag nil)
    (diredp-toggle-find-file-reuse-dir 1)
    )

  (setq wdired-use-dired-vertical-movement 'sometimes)

  (defun dired-copy-file-path-as-kill ()
    (interactive)
    (dired-copy-filename-as-kill 0))
  (defun dired-copy-file-directory-as-kill ()
    (interactive)
    (let ((dir (file-name-directory (dired-copy-file-path-as-kill))))
      (if (eq last-command 'kill-region)
          (kill-append dir nil)
        (kill-new dir))
      (message "%s" dir)))

  ;; INITIAL STATE:
  ;; dired-mode  :   (customized) evilified state
  ;; wdired-mode :                normal state

  (eval-after-load "dired"
    `(progn
       (define-key dired-mode-map (kbd "C-h") nil)
       ;; keys: dired-mode-map
       (bind-keys
        :map dired-mode-map
        ("q" . tsp-quit-window-kill)
        ("<S-return>" . dired-find-file)
        )

       ;; keys: evilified-state
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
         (kbd "\"")     'evil-use-register
         )

       ;; T is the prefix key for the tags commands
       (which-key-add-major-mode-key-based-replacements 'dired-mode
         "T"    "tags"
         "M-+"  "diredp-recursive-map"
         "y"    "copy--as-kill"
         )
       ;; set function definition of 'dired-mode-map (same as value)
       (fset 'dired-mode-map dired-mode-map)
       ;; major-mode leader-key
       (spacemacs/set-leader-keys-for-major-mode 'dired-mode
         "c"     'dired-mode-map
         "to"    'dired-omit-mode
         "tl"    'dired-hide-details-mode
         "tr"    'toggle-diredp-find-file-reuse-dir
         "tw"    'spacemacs/toggle-diredp-wrap-around-flag
         "v"     'dired-view-file    ;; for discovery - can just use \v
         "Y"     'diredp-relsymlink-this-file
         "x"     'tsp-dired-cut-or-copy-files
         "p"     'tsp-dired-copy-files-here
         "m"     'tsp-dired-move-files-here
         )
       (spacemacs/declare-prefix-for-mode 'dired-mode "mt" "toggles")

       ;; wdired-mode
       (evil-set-initial-state 'wdired-mode 'normal)

       (spacemacs/set-leader-keys-for-major-mode 'wdired-mode
         "c"    'wdired-finish-edit
         "a"    'wdired-abort-changes
         "t"    'wdired-tidy-name-keep-square-brackets
         )

       )
    )

  (eval-after-load "dired-sort-menu" `(require 'dired-sort-menu+))

  (defun wdired-init ()
    (define-keys wdired-mode-map
      (kbd "C-c <escape>") 'wdired-abort-changes
      [f2]                 'wdired-finish-edit
      ))
  (add-hook 'wdired-mode-hook 'wdired-init)

  (defvar tsp-dired-files-to-move-or-copy '()
    "Stores a list of files to be moved or copied by tsp-dired-*-files-here
 commands.")

  (defun tsp-dired-cut-or-copy-files (append)
    (interactive "P")
    (let ((files (dired-get-marked-files)))
      (if append
          (dolist (f files)
            (add-to-list tsp-dired-files-to-move-or-copy f))
        (setq tsp-dired-files-to-move-or-copy files))))

  (defun tsp-dired-copy-files-here
      (&optional overwrite
                 keep-time preserve-uid-gid preserve-permissions)
    (interactive "P")
    (dolist (f tsp-dired-files-to-move-or-copy)
      (let ((newpath (concat (dired-current-directory)
                             (file-name-nondirectory f))))
        (copy-file f newpath overwrite
                   keep-time preserve-uid-gid preserve-permissions))))

  (defun tsp-dired-move-files-here (&optional overwrite)
    (interactive "P")
    (dolist (f tsp-dired-files-to-move-or-copy)
      (let ((newpath (concat (dired-current-directory)
                             (file-name-nondirectory f))))
        (rename-file f newpath overwrite))))

  (defun my/dired-replace-buffer ()
    (interactive)
    (let ((dir default-directory))
      (kill-buffer (current-buffer))
      (dired dir)))

  ;; -------------------------------------------------------------------------------
  ;; ,--------------,
  ;; | Edmacro-Mode |
  ;; '--------------'

  ;; FIXME: major-mode leader key not working
  (spacemacs/set-leader-keys-for-major-mode 'edmacro-mode
    "c"   'edmacro-finish-edit
    "a"   'spacemacs/kill-this-buffer
    )


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
  ;; ,--------------,
  ;; | Haskell-Mode |
  ;; '--------------'
  ;; structured-haskell-mode - issues with evil:
  ;;     https://github.com/chrisdone/structured-haskell-mode/issues/81

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

  (use-package helm-mode
    ;;
    :init
    (defun helm-switch-to-mini ()
      (interactive)
      (helm-run-after-exit #'helm-mini))
    ;;
    :bind (:map helm-map
                ("C-q"        . ace-jump-helm-line-and-select)  ;; was ace-jump-helm-line
                ("C-S-q"      . ace-jump-helm-line)
                ("C-0"        . helm-select-action)
                ("C-)"        . helm-execute-persistent-action)
                ("C-S-O"      . helm-previous-source)
                ("C-S-W"      . helm-yank-symbol-at-point)
                ("C-u"        . helm-delete-minibuffer-contents)
                ("C-,"        . helm-switch-to-mini)
                ("<f5>"       . nil)
                ("<f11>"      . nil)
                ("<escape>"   . evil-evilified-state)
                ("<S-escape>" . evil-normal-state)
                ("<f9>"       . spacemacs/helm-navigation-transient-state/body)
                )
    ;;
    :config
    (evilified-state-evilify-map helm-map
      :mode helm-mode
      :bindings
      "j"           'helm-next-line
      "k"           'helm-previous-line
      "i"           'evil-insert
      "a"           'evil-append
      [escape]      'keyboard-escape-quit
      [S-escape]    'evil-normal-state
      )
    (spacemacs/set-leader-keys-for-major-mode 'helm-major-mode
      "tm"    'helm-toggle-all-marks)
    (define-keys helm-map
      (kbd "M-RET")  spacemacs-helm-major-mode-map
      (kbd "M-m")    spacemacs-cmds
      (kbd "S-SPC")  spacemacs-cmds
      )
    )

  ;; -------------------------------------------------------------------------------
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
    (expand-file-name (file-name-as-directory firefox-default-user-profile)
                      firefox-profile-directory)
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
       (define-key button-map [remap push-button] #'my/push-button-and-center)
       ))

  ;; -------------------------------------------------------------------------------
  ;; ,---------,
  ;; | Hy-Mode |
  ;; '---------'

  (which-key-add-major-mode-key-based-replacements 'hy-mode
    "SPC m s"    "evaluate"
    ", s"        "evaluate"
    "SPC m V"    "pyvenv"
    ", V"        "pyvenv"
    )

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
             ("<mouse-4>" . mwheel-scroll)
             ("<mouse-5>" . mwheel-scroll)
             ("<mouse-6>" . Info-backward-node)
             ("<mouse-7>" . Info-forward-node)
             ("<mouse-8>" . Info-history-forward )
             ("<mouse-9>" . Info-history-back)
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
       (define-keys Info-mode-map
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
  ;; ,--------------,
  ;; | Indent-Guide |
  ;; '--------------'

  (setq indent-guide-recursive t)

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
  (define-key text-mode-map (kbd "C-c C-v") 'browse-buffer-file-firefox)

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

  (defun my/java-quick-run ()
    (interactive)
    (let ((name (replace-regexp-in-string
                 "\\.java$" ""
                 (file-name-nondirectory buffer-file-name))))
      (shell-command (format "javac %s.java && java %s" name name))))

  (spacemacs/set-leader-keys-for-major-mode 'java-mode
    "RET"           'my/java-quick-run
    )

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
  ;; ,----------,
  ;; | js2-mode |
  ;; '----------'

  (defun my/js-url-decode ()
    (interactive)
    (my/evil-shell-command-replace-region (region-beginning) (region-end) "inclusive" "sed 's/+/%2B/g' | xargs -0 urlencode -d"))

  (spacemacs/set-leader-keys-for-major-mode 'js2-mode
    "%"    'my/js-url-decode
    )


  ;; -------------------------------------------------------------------------------
  ;; ,-----------,
  ;; | JSON-Mode |
  ;; '-----------'

  (spacemacs/set-leader-keys-for-major-mode 'json-mode
    "ed"            'js-eval-defun
    "ee"            'js-eval
    "go"            'js-find-symbol
    "b"             'json-mode-beautify
    )

  (which-key-add-major-mode-key-based-replacements 'json-mode
    ", e"       "eval"
    ", g"       "find"
    ", h"       "json-snatcher"
    )

  ;; -------------------------------------------------------------------------------
  ;; ,-------,
  ;; | Magit |
  ;; '-------'


  (defun magit-init-fn ()
    (interactive)
    ;; remove C-tab binding which shadows #'next-multiframe-window binding
    ;; replace with C-` binding
    (bind-key "<C-tab>" nil magit-mode-map)
    (bind-keys
     :map magit-mode-map
     ("<C-tab>" . nil)
     ("<C-`>"   . magit-section-cycle)
     ("q"       . my/magit-mode-kill-buffer)
     )
    (add-hook 'magit-diff-visit-file-hook #'recenter)
    (setf magit-log-revision-headers-format "\
%+b
Author:    %aN <%aE>
Committer: %cN <%cE>"))

  (eval-after-load 'magit-mode
    `(progn
       (add-hook 'magit-mode-hook 'magit-init-fn)
       ))

  (defun my/magit-mode-kill-buffer (bury)
    (interactive "P")
    (magit-mode-bury-buffer (not bury)))

  (fset 'magit-diff-toggle-ignore-all-space
        (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([68 19 45 45 105 103 110 111 114 101 45 97 108 108 45 115 112 97 99 101 return return 103] 0 "%d")) arg)))
  (fset 'magit-diff-toggle-ignore-space-change
        (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([68 19 45 45 105 103 110 111 114 101 45 115 112 97 99 101 45 99 104 97 110 103 101 return return 103] 0 "%d")) arg)))

  (defun my/magit-diff-meld ()
    (interactive)
    (my/async-shell-command-no-window
     "GIT_EXTERNAL_DIFF='meld $2 $5 | cat' git diff --color-words"))
  (defun my/magit-diff-added-meld ()
    (interactive)
    (my/async-shell-command-no-window
     "GIT_EXTERNAL_DIFF='meld $2 $5 | cat' git diff --cached --color-words"))
  (defun my/magit-diff-committed-meld ()
    (interactive)
    (my/async-shell-command-no-window
     "GIT_EXTERNAL_DIFF='meld $2 $5 | cat' git diff HEAD --color-words"))
  (defun my/magit-undo-last-commit ()
    (interactive)
    (magit-reset "HEAD~"))

  (spacemacs/set-leader-keys-for-major-mode 'magit-diff-mode
    "s"      'magit-diff-toggle-ignore-all-space
    "S"      'magit-diff-toggle-ignore-space-change
    )
  (spacemacs/set-leader-keys-for-major-mode 'magit-status-mode
    "s"      'magit-diff-toggle-ignore-all-space
    "S"      'magit-diff-toggle-ignore-space-change
    "d g g"  'my/magit-diff-meld
    "d g a"  'my/magit-diff-added-meld
    "d g c"  'my/magit-diff-committed-meld
    "r"      'my/magit-undo-last-commit
    )

  ;; -------------------------------------------------------------------------------
  ;; ,----------,
  ;; | Markdown |
  ;; '----------'

  (defun my/markdown-gh-linkify-heading ()
    (interactive)
    (let* ((heading-line (my/get-current-line))
           (heading (replace-regexp-in-string "^#*[ \\t]*" "" heading-line))
           (heading-fragment (downcase
                              (concat "#" (replace-regexp-in-string " " "-" heading))))
           (link (format "[%s](%s)\n" heading heading-fragment)))
      (copy-string-as-kill link)))

  (bind-keys :map spacemacs-markdown-mode-map
             ("i C-l"  . my/markdown-gh-linkify-heading)
             )

  ;; -------------------------------------------------------------------------------
  ;; ,------,
  ;; | Lisp |
  ;; '------'

  (bind-keys :map spacemacs-emacs-lisp-mode-map
             ;; common
             ("e j"    . my/eval-print-last-sexp)
             ("e p"    . my/eval-prettyprint-last-sexp)
             ("e D"    . eval-instrument-defun)
             ("e RET"  . my/eval-replace-last-sexp)
             ("C-M-x"  . eval-defun)
             ("g g"    . my/spacemacs/jump-to-definition-and-center)
             ("t i"    . ert-run-tests-interactively)
             ("j"      . my/eval-print-last-sexp)
             ("v"      . my/evil-select-sexp-at-point)
             ("x"      . prettyexpand-at-point)
             ("; j"    . my/eval-print-last-sexp-as-comment)
             ("; p"    . my/eval-prettyprint-last-sexp-as-comment)
             ("<f3> n" . kmacro-name-last-macro)
             ("<f3> p" . insert-kbd-macro)
             (">"      . my/delete-function-application)
             ;; emacs-lisp-mode only
             ("M-w"    . elu-github-copy-md-sig-and-doc)
             ("M-t"    . elu-create-test)
             )
  (bind-keys :map spacemacs-lisp-interaction-mode-map
             ;; common
             ("e j"    . my/eval-print-last-sexp)
             ("e p"    . my/eval-prettyprint-last-sexp)
             ("e D"    . eval-instrument-defun)
             ("e RET"  . my/eval-replace-last-sexp)
             ("C-M-x"  . eval-defun)
             ("g g"    . my/spacemacs/jump-to-definition-and-center)
             ("j"      . my/eval-print-last-sexp)
             ("v"      . my/evil-select-sexp-at-point)
             ("x"      . prettyexpand-at-point)
             ("; j"    . my/eval-print-last-sexp-as-comment)
             ("; p"    . my/eval-prettyprint-last-sexp-as-comment)
             ("<f3> n" . kmacro-name-last-macro)
             ("<f3> p" . insert-kbd-macro)
             (">"      . my/delete-function-application)
             )
  (bind-keys :map lisp-interaction-mode-map
             ("C-j"  . my/eval-prettyprint-last-sexp)
             )

  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    ", d"         "macrostep"
    ", <f3>"      "kmacro"
    )

  (defun emacs-lisp-init-fn ()
    (interactive)
    ;; set tab-width to 8 to make GNU sources readable
    (setq tab-width 8)
    (make-local-variable 'evil-args-delimiters)
    (setq evil-args-delimiters (mapcar (fn: make-string <> ? ) (number-sequence 1 50)))
    )
  (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-init-fn)

  (defun my/delete-function-application ()
    "Remove the function under point and the parentheses wrapping it."
    (interactive)
    (apply #'evil-delete (evil-inner-symbol))
    (sp-unwrap-sexp -1)
    (evil-normal-state))

  ;; testing macro for my packages
  (defmacro should-equal
      (expr keyword result)
    (progn
      (unless (eq keyword :result) (error "expected :result"))
      `(should (equal ,expr ,result))))

  (defmacro should-not-equal
      (expr keyword result)
    (progn
      (unless (eq keyword :result) (error "expected :result"))
      `(should-not (equal ,expr ,result))))

  ;; ===== EVIL-ADJUST =====

  (require 'evil-adjust)
  (evil-adjust :noemacs25fix)

  ;; ===== SWITCH TO EVIL LISP STATE =====

  ;; (define-key evil-lisp-state-map "." nil) ;; available

  (bind-keys :map evil-lisp-state-map
             ("x"   . evil-delete-char)
             (","   . spacemacs-emacs-lisp-mode-map)
             ("C-o" . evil-execute-in-normal-state)
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

  (defun org-init ()
    (interactive)
    ;; remove C-tab binding which shadows #'next-multiframe-window binding
    ;; replace with [, C-tab] binding
    ;; (unbind-key [C-tab] org-mode-map)
    (unbind-key "<C-tab>" org-mode-map)
    (bind-keys
     :map org-mode-map
     ("<tab>" . org-cycle)
     ("<S-iso-lefttab>" . nil)
     ("<M-n>" . org-next-link)
     ("<M-p>" . org-previous-link)
     ("<C-c><C-v>" . tsp-org-view-as-html))
    (evil-define-key 'normal org-mode-map
      (kbd "-")    'dired-jump
      (kbd "g -")  'org-cycle-list-bullet)
    )

  ;; (evil-define-keymap my/org-map
  ;;   "My org-mode bindings."
  ;;   :mode org-mode
  ;;   (kbd "-")    'dired-jump
  ;;   (kbd "g -")  'org-cycle-list-bullet)

  (eval-after-load 'org
    `(progn
       (add-hook 'org-mode-hook 'org-init)
       (setq org-capture-templates
             '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
                "* TODO %?\n  %i\n  %a")
               ("j" "Journal" entry (file+datetree "~/org/journal.org")
                "* [%t] %^G\n%?")
               ))
       (setq org-directory (expand-file-name (file-name-as-directory "org")
                                             (getenv "HOME")))
       (setq org-default-notes-file
             (expand-file-name (file-name-as-directory "notes.org")
                               org-directory))
       ))

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "SPC"      'ace-link-org
    "<C-tab>"  'org-force-cycle-archived
    "<tab>"    'org-cycle
    "<S-tab>"  'org-shifttab
    "h h"      'helm-org-in-buffer-headings
    "h 1"      'my/helm-org-in-buffer-h1
    "h 2"      'my/helm-org-in-buffer-h2
    )

  (eval-after-load 'ob-keys
    `(progn
       (spacemacs/set-leader-keys-for-major-mode 'org-mode
         "B"        org-babel-map      ;; originally on <C-c><C-v> prefix
         )))

  (which-key-add-major-mode-key-based-replacements 'org-mode
    ", g"  "goto"
    ", i"  "insert"
    ", S"  "subtree"
    ", t"  "table"
    ", x"  "text style"
    ", B"  "org-babel"
    )

  ;; modified from helm-org.el source: GPL3
  (defun my/helm-org-in-buffer-h1 ()
    "Preconfigured helm for org buffer level 1 headings."
    (interactive)
    (let (helm-org-show-filename helm-org-format-outline-path)
      (helm :sources (helm-source-org-headings-for-files
                      (list (current-buffer)))
            :candidate-number-limit 99999
            :input "^\\*\\  "
            :preselect (helm-org-in-buffer-preselect)
            :truncate-lines helm-org-truncate-lines
            :buffer "*helm org inbuffer*")))
  (defun my/helm-org-in-buffer-h2 ()
    "Preconfigured helm for org buffer level 2 headings."
    (interactive)
    (let (helm-org-show-filename helm-org-format-outline-path)
      (helm :sources (helm-source-org-headings-for-files
                      (list (current-buffer)))
            :candidate-number-limit 99999
            :input "^\\*\\*\\  "
            :preselect (helm-org-in-buffer-preselect)
            :truncate-lines helm-org-truncate-lines
            :buffer "*helm org inbuffer*")))

  ;; -------------------------------------------------------------------------------
  ;; ,-----------,
  ;; | Ruby-mode |
  ;; '-----------'

  (defun ruby-init ()
    (define-key spacemacs-ruby-mode-map "sb" 'ruby-send-buffer)
    (define-key spacemacs-ruby-mode-map "sI" 'inf-ruby))

  (add-hook 'ruby-mode-hook 'ruby-init)

  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
    "g l"      'goto-gem
    "g /"      'goto-gem-grep-gem
    "g ?"      'goto-gem-grep-all-gems
    )

  ;; -------------------------------------------------------------------------------
  ;; ,-----------------------,
  ;; | Spacemacs-Buffer-Mode |
  ;; '-----------------------'

  (which-key-add-major-mode-key-based-replacements 'spacemacs-buffer-mode
    "m"     "jump to menu"
    )

  ;; -------------------------------------------------------------------------------
  ;; ,-----------,
  ;; | Undo Tree |
  ;; '-----------'
  (setq undo-tree-git-repo "http://www.dr-qubit.org/git/undo-tree.git")

  (setq evil-want-fine-undo "No")

  (setq undo-tree-history-directory-alist
        (let ((undohistdir (concat spacemacs-private-directory ".undo-tree-history/")))
          `(("/home/.*/.emacs.d.*/private/.*" . nil)
            ("/home/.*/code/.*"               . nil)
            ("."                              . ,undohistdir))))


  ;; bind-keys to undo-tree register functions even when undo-tree-mode is off
  (bind-keys :map global-map
             ("C-x r u" .undo-tree-save-state-to-register)
             ("C-x r U" .undo-tree-restore-state-from-register)
             )

  (defun undo-tree-clear ()
    (interactive)
    (setq buffer-undo-tree nil))

  ;; =====UNDO-TREE-AUTO-SAVE-HISTORY=====
  ;; Causes undo-tree corruption

  (setq undo-tree-auto-save-history t)

  ;; Attempt to prevent undo-tree history corruption...
  ;; https://github.com/syl20bnr/spacemacs/issues/774#issuecomment-194527210
  (defun my/save-undo-history ()
    (interactive)
    (when (and (boundp 'undo-tree-mode)
               undo-tree-mode
               buffer-file-name
               (file-writable-p buffer-file-name)
               (not (eq buffer-undo-list t))
               (not revert-buffer-in-progress-p))
      (undo-tree-save-history nil t)))
  (defun my/save-all-undo-history ()
    (interactive)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (my/save-undo-history))))
  ;; (add-hook 'kill-emacs-hook #'my/save-all-undo-history)
  ;; (add-hook 'kill-buffer-hook #'my/save-undo-history)


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
       (evil-define-key 'normal undo-tree-visualizer-mode-map
         (kbd ",")   'spacemacs-undo-tree-visualizer-mode-map
         (kbd "M-;") 'evil-repeat-find-char-reverse
         )
       (evil-define-key 'visual undo-tree-visualizer-mode-map
         (kbd ",")   'spacemacs-undo-tree-visualizer-mode-map
         (kbd "M-;") 'evil-repeat-find-char-reverse
         )
       (evil-define-key 'motion undo-tree-visualizer-mode-map
         (kbd ",")   'spacemacs-undo-tree-visualizer-mode-map
         (kbd "M-;") 'evil-repeat-find-char-reverse
         )
       (evil-define-key 'evilified-state undo-tree-visualizer-mode-map
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
  ;; ,-----------,
  ;; | View-Mode |
  ;; '-----------'

  (evil-define-key 'normal view-mode-map
    "q"   'View-quit
    )
  (defun view-mode-init ()
    (spacemacs/set-leader-keys-for-minor-mode 'view-mode
      "q"   'View-quit
      ))
  (add-hook 'view-mode-hook #'view-mode-init)

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

  (evil-define-text-object evil-inner-element (count &optional beg end type)
    (list (+ 1 (web-mode-tag-end-position (web-mode-element-beginning-position (point)))) (web-mode-tag-beginning-position (web-mode-element-end-position (- (point) 1)))))
  (evil-define-text-object evil-outer-element (count &optional beg end type)
    (list (web-mode-element-beginning-position (point)) (+ 1 (web-mode-element-end-position (point)))))
  (define-key evil-inner-text-objects-map "e" 'evil-inner-element)
  (define-key evil-outer-text-objects-map "e" 'evil-outer-element)

  ;; -------------------------------------------------------------------------------
  ;; ,-----------,
  ;; | Yasnippet |
  ;; '-----------'

  (defun tsp-yas-expand (&optional field)
    "Expand a snippet before point. Simplified version of `yas-expand'."
    (interactive)
    (setq yas--condition-cache-timestamp (current-time))
    (let ((templates-and-pos (yas--templates-for-key-at-point)))
      (if templates-and-pos
          (yas--expand-or-prompt-for-template
           (nth 0 templates-and-pos)
           ;; Delete snippet key and active region when expanding.
           (min (if (use-region-p) (region-beginning) most-positive-fixnum)
                (nth 1 templates-and-pos))
           (max (if (use-region-p) (region-end) most-negative-fixnum)
                (nth 2 templates-and-pos))))))

  ;; -------------------------------------------------------------------------------
  ;; ,-------,
  ;; | Align |
  ;; '-------'

  (spacemacs|create-align-repeat-x "period" "\\." t nil)
  (spacemacs|create-align-repeat-x "quote" "'")
  (spacemacs|create-align-repeat-x "double-quote" "\"")
  (spacemacs|create-align-repeat-x "dash" "-")
  (spacemacs|create-align-repeat-x "hash" "#" )
  (spacemacs|create-align-repeat-x "semicolon-comment" ";;?" )
  (spacemacs|create-align-repeat-x "slash-comment" "//" )

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

  (evil-define-motion my/evil-pcre-search-forward ()
    (format
     "Search forward incrementally for entered text or PCRE.

If `evil-regexp-search' is t, search for a PCRE.

ISEARCH DOCUMENTATION.
----------------------
%s" (documentation 'isearch-forward))
    :jump t
    :type exclusive
    :repeat evil-repeat-search
    (let ((init-pcre-mode (if pcre-mode 1 0)))
      (pcre-mode t)
      (evil-search-incrementally t evil-regexp-search)
      (pcre-mode init-pcre-mode)))

  (evil-define-motion my/evil-pcre-search-backward ()
    (format
     "Search backward incrementally for entered text or PCRE.

If `evil-regexp-search' is t, search for a PCRE.

ISEARCH DOCUMENTATION.
----------------------
%s" (documentation 'isearch-forward))
    :jump t
    :type exclusive
    :repeat evil-repeat-search
    (let ((init-pcre-mode (if pcre-mode 1 0)))
      (pcre-mode t)
      (evil-search-incrementally nil evil-regexp-search)
      (pcre-mode init-pcre-mode)))

  ;; -------------------------------------------------------------------------------
  ;; ,-----------------,
  ;; | Keyboard Macros |
  ;; '-----------------'

  (fset 'switch-to-most-recent-buffer [?\C-x ?b return])

  (fset 'just-one-space-before-open-brace
        (lambda (&optional arg)
          "Keyboard macro."
          (interactive "p")
          (kmacro-exec-ring-item
           (quote ([102 91 104 134217848 106 117 115 116 45 111 110 101 45 115 112 97 99 101 return 106 1] 0 "%d"))
           arg)))

  (fset 'wdired-tidy-name-keep-square-brackets
        (lambda (&optional arg)
          "Keyboard macro. In wdired-mode, remove everything between after the first )
except the the first [...] (preceded by a space) and everything after a dot.
Then move to the next line (column 3).

    KEEP) DELETE[KEEP]DELETE.KEEP"
          (interactive "p")
          (kmacro-exec-ring-item
           (quote ([102 41 97 32 escape 118 116 91 99 32 escape 102 93 108 100 116 46 106 48 108 108] 0 "%d"))
           arg)))

  (fset 'md-sig-to-list-item
        (fset 'md-sig-to-list-item
              (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([118 69 121 103 118 115 93 105 42 32 escape 69 108 120 97 35 escape 80 16 97 45 escape 118 36 104 201326629 38 63 32 return 45 return] 0 "%d")) arg))))

  (fset 'setq->push
        (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([99 119 112 117 115 104 escape 119 100 119 108 100 119 104 32 107 87 escape] 0 "%d")) arg)))

  (defun my/delete-inside-double-quotes ()
    "Delete contents of double quotes."
    ;; TODO: rewrite as function to handle escaped quotes
    (interactive)
    (command-execute "l?\"lvNhd"))
  (defun my/delete-double-quotes ()
    "Delete double quotes and contents."
    (interactive)
    (let (start end)
      (right-char)
      (cl-loop do
       (search-backward "\"")
       while (yas-inside-string))
      (setf start (point))
      (right-char)
      (cl-loop do
       (search-forward "\"")
       while (yas-inside-string))
      (setf end (point))
      (delete-region start end)))

  ;; -------------------------------------------------------------------------------
  ;; ,--------------------------------,
  ;; | Key/Keymap Functions & Aliases |
  ;; '--------------------------------'
  ;; to get the definition of a key sequence in a keymap: lookup-key
  ;; to show keymap with which-key:  (which-key--show-keymap keymap-name keymap)
  ;; to show keys sequence that invoked the current command: (this-command-keys)
  (defalias 'key-vector-to-readable-string 'key-description)
  ;; WARNING: key-description is described as an *approximate* inverse to kbd.
  (defalias 'key-readable-string-to-string 'kbd)  ;; or edmacro-parse-keys or read-kbd-macro
  (defalias 'key-input-to-vector 'read-key-sequence-vector)
  (defalias 'key-input-to-string 'read-key-sequence)
  (defun key-readable-string-to-vector (keystr) (edmacro-parse-keys keystr t))

  (defun read-key-insert-vector ()
    "Read a keystroke and insert as a vector."
    (interactive)
    (cl-prettyprint (read-key-sequence-vector "")))

  (defun read-key-insert-readable-string (arg)
    "Read a keystroke and insert as an edmacro-style string.

    If a prefix argument is supplied, the double quotes are omitted."
    (interactive "P")
    (if arg
        (insert (key-description (read-key-sequence-vector "")))
      (insert ?" (key-description (read-key-sequence-vector "")) ?")))

  (defun read-key-insert-string (arg)
    "Read a keystroke and insert as a string.

    If a prefix argument is supplied, the double quotes are omitted."
    (interactive "P")
    (if arg
        (insert (read-key-sequence ""))
      (insert ?" (read-key-sequence "") ?")))

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

  (defun read-kbd-event (start &optional end)
    "Read a string or the region as an event sequence.

The string should represent a sequence of keys in `edmacro-mode' format.
Interactively, acts on the region. Called from lisp, START may be a string."
    (interactive "r")
    (if (stringp start)
        (listify-key-sequence (edmacro-parse-keys start end))
      (listify-key-sequence (edmacro-parse-keys (buffer-substring start end)))))

  (defun simulate-keypress (keys)
    (interactive "sKeys: ")
    "Simulate a key press or key sequence.

Keys are specified using `edmacro-mode' key syntax.
Note: when the key sequence represents a completed action, `execute-kbd-macro'
may be used instead, eg.  (execute-kbd-macro (kbd "C-x o")).

Example: to enter the help prefix and await another keypress...
    (simulate-keypress "C-h")"
    (setq prefix-arg current-prefix-arg)
    (setq unread-command-events (read-kbd-event keys)))

  (defun define-simulated-keypress (keys)
    "Return a command executing a simulated keypress of KEY.

KEY is specified in `edmacro-mode' format."
    `(lambda ()
       (interactive)
       (setq prefix-arg current-prefix-arg)
       (setq unread-command-events (read-kbd-event ,keys))))

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

  (defun my/eval-replace-last-sexp ()
    "Replace the preceding sexp with its value, formatted by `pp-to-string'.
With a prefix argument, formats the value using `(format \"%S\" val)' instead."
    (interactive)
    (if (boundp 'evil-state)
        (evil-save-state
          (call-interactively #'evil-append)
          (my/eval-replace-last-sexp-core))
      (my/eval-replace-last-sexp-core)))

  (defun my/eval-replace-last-sexp-core ()
    "Replace the preceding sexp with its value, formatted by pp-to-string. With a
 prefix argument, formats the value using `(format \"%S\" val)' instead."
    (let ((val (eval (preceding-sexp))))
      (kill-sexp -1)
      (if current-prefix-arg (insert (format "%S" val))
        (insert (replace-regexp-in-string "\n\\'" "" (pp-to-string val))))))

  (defun my/evil-normal-or-visual-state? ()
    (and (boundp 'evil-state)
         (memq evil-state
               '(normal visual))))

  (defun my/evil-indent-line-hold-position ()
    (interactive)
    (let ((marker (make-marker)))
      (set-marker marker (point))
      (evil-indent-line (line-beginning-position) (line-end-position))
      (goto-char marker)))

  (defun my/eval-print-last-sexp (&optional same-line)
    "Print the value of the preceding sexp on a new line.

If SAME-LINE is non-nil, do not start a new line."
    (interactive "P")
    (let ((evil? (boundp 'evil-state))
          (evil-move-beyond-eol t))
      (when (my/evil-normal-or-visual-state?)
        (forward-char 1))
      (unless same-line
        (newline))
      (eval-last-sexp t)
      (if evil?
          (my/evil-indent-line-hold-position)
        (indent-according-to-mode))
      (unless same-line
        (newline))))

  (defun my/eval-prettyprint-last-sexp (&optional eval-last-sexp-arg-internal)
    (interactive "P")
    (cl-case (and (boundp 'evil-state)
                  evil-state)
      ('normal (progn
                 (evil-append 1)
                 (cl-prettyprint
                  (eval-last-sexp eval-last-sexp-arg-internal))
                 (evil-normal-state)
                 ))
      ('visual (progn
                 (evil-append 1)
                 (cl-prettyprint
                  (eval-last-sexp eval-last-sexp-arg-internal))
                 (evil-visual-restore)
                 ))
      (otherwise
       (cl-prettyprint
        (eval-last-sexp eval-last-sexp-arg-internal)))
      ))

  (defun my/eval-print-last-sexp-as-comment (&optional eval-last-sexp-arg-internal)
    (interactive "P")
    (let ((beg (line-beginning-position 2))  ;; start of next line
          end
          (printer (if (fboundp 'evil-adjust-eval-print-last-sexp)
                       #'evil-adjust-eval-print-last-sexp
                     #'eval-print-last-sexp)))
      (funcall printer eval-last-sexp-arg-internal)
      (setq end (line-end-position 0))       ;; end of previous line
      (comment-region beg end)))

  (defun my/eval-prettyprint-last-sexp-as-comment (&optional eval-last-sexp-arg-internal)
    (interactive "P")
    (let ((beg (line-beginning-position 2))  ;; start of next line
          end)
      (my/eval-prettyprint-last-sexp eval-last-sexp-arg-internal)
      (setq end (line-end-position 1))       ;; end of current line
      (comment-region beg end)))

  (defun my/eval-print-last-sexp-maybe-comment (&optional comment)
    (interactive "P")
    (if comment (my/eval-print-last-sexp-as-comment)
      (my/eval-print-last-sexp-as-comment)))

  (defun my/eval-prettyprint-last-sexp-maybe-comment (&optional comment)
    (interactive "P")
    (if comment (my/eval-print-last-sexp-as-comment)
      (my/eval-prettyprint-last-sexp-as-comment)))

  (defun eval-instrument-defun ()
    "Equivalent to `eval-defun' with a prefix argument."
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively 'eval-defun)))

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

  (defun my/switch-to-messages-buffer ()
    (interactive)
    (switch-to-buffer (messages-buffer)))

  (defun my/switch-to-warnings-buffer ()
    (interactive)
    (switch-to-buffer "*Warnings*"))

  (defun my/switch-to-changelog-buffer ()
    (interactive)
    (find-file (concat user-emacs-directory "CHANGELOG.org")))

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

  (defun my/copy-current-line ()
    (interactive)
    (when (my/line-visible-end-position)
      (kill-ring-save (my/line-visible-beginning-position) (my/line-visible-end-position))))

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

  (defun remove-trailing-space-and-blank-lines (&optional beg end)
    (interactive
     (cond ((use-region-p)   (list (region-beginning) (region-end)))
           (:else            (list (point) (point-max)))))
    (delete-trailing-whitespace beg end)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\n+" end t)
        (replace-match "\n" nil nil))))

  (defun remove-trailing-space-and-doubled-blank-lines (&optional beg end)
    (interactive
     (cond ((use-region-p)   (list (region-beginning) (region-end)))
           (:else            (list (point) (point-max)))))
    (delete-trailing-whitespace beg end)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\n\n+" end t)
        (replace-match "\n\n" nil nil))))

  (defun sudo-edit-this-file ()
    (interactive)
    (let ((f (concat "/sudo::" (expand-file-name buffer-file-name))))
      (find-file f)))

  (defun my/open-file-at-point ()
    "Open the file at point using xdg-open."
    (interactive)
    (shell-command (concat "xdg-open " (ffap-file-at-point))))

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

  (defun my/evil-insert-at-WORD-beginning (&optional count)
    (interactive "p")
    (evil-backward-WORD-begin count)
    (evil-insert-state))

  (defun my/evil-append-at-WORD-end (&optional count)
    (interactive "p")
    (evil-forward-WORD-end count)
    (evil-append 1))

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

  (defun my/extend-to-column (&optional col set-fill-column)
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

  (defun rgrep-with-ignored (regexp ignored files dir)
    (interactive "sREGEXP: \nsIGNORED: \nFILES: \nDIR: ")
    (let ((grep-find-ignored-directories (append ignored grep-find-ignored-directories)))
      (rgrep regexp files dir)))

  (defun spacemacs-rgrep (regexp)
    (interactive "sREGEXP: ")
    (let ((grep-find-ignored-directories (cons ".cache" grep-find-ignored-directories)))
      (rgrep regexp
             ".spacemacs* *.el"
             (directory-file-name (expand-file-name user-emacs-directory)))))

  (defun spacemacs-only-rgrep (regexp)
    (interactive "sREGEXP: ")
    (rgrep-with-ignored regexp
                        '(".cache" "elpa" "private")
                        ".spacemacs* *.el"
                        (directory-file-name (expand-file-name user-emacs-directory))))

  (defun spacemacs-private-rgrep (regexp)
    (interactive "sREGEXP: ")
    (rgrep-with-ignored regexp
                        '(".cache")
                        ".spacemacs* *.el"
                        (expand-file-name "private" user-emacs-directory)))

  (defun elpa-rgrep (regexp)
    (interactive "sREGEXP: ")
    (rgrep regexp
           ".spacemacs* *.el"
           (directory-file-name (expand-file-name "elpa/" user-emacs-directory))))

  (defun dired-spacemacs-private-directory ()
    "Open `dired' in `spacemacs-private-directory'."
    (interactive)
    (dired spacemacs-private-directory))

  (defun dired-spacemacs-directory ()
    "Open `dired' in the `user-emacs-directory'."
    (interactive)
    (dired user-emacs-directory))

  (defun prettyexpand-at-point ()
    "Pretty print macro-expansion of sexp at point.

Inserts the expansion on a new line at the end of the sexp."
    (interactive)
    (let ((sexp (sexp-at-point)))
      (forward-sexp)
      (newline)
      (insert (cl-prettyexpand sexp))))

  (defun my/evil-select-sexp-at-point ()
    (interactive)
    (let ((sexp-bounds (cdr (sexp-at-point-with-bounds))))
      (evil-visual-char (cdr sexp-bounds) (car sexp-bounds))))

  (defun describe-bindings-orig ()
    (interactive)
    (let ((helm-descbinds-mode? helm-descbinds-mode))
      (helm-descbinds-mode 0)
      (describe-bindings)
      (when helm-descbinds-mode? (helm-descbinds-mode 1))))

  (defun avy-goto-form-feed ()
    (interactive)
    (avy-goto-subword-0 1 (lambda () (= 12 (char-after)))))

  (defun tsp-align-quoted-column (beg end)
    (interactive "r")
    (quick-pcre-align-repeat beg end " (?:')")
    (evil-indent beg end))

  (defun tsp-align-double-quoted-column (beg end)
    (interactive "r")
    (quick-pcre-align-repeat beg end " (?:\")")
    (evil-indent beg end))

  (defun tsp-quit-window-kill (&optional bury window)
    (interactive "P")
    (quit-window (not bury) window))

  (defun tsp-info-goto-node (s)
    "Visit a node in a given info manual.

The manual and node are entered in the format:
    (manual) node
The node is chosen via `helm'. Optionally, a node pattern can be given alone."
    (interactive "s(manual) node: ")
    (let ((regexp "\\((\\(.*\\)) \\)?\\(.*\\)"))
      (string-match regexp s)
      (let ((manual (match-string-no-properties 2 s))
            (node   (match-string-no-properties 3 s)))
        (Info-directory)
        (when manual (Info-menu manual))
        (helm :sources helm-info-default-sources
              :input   node
              :buffer  "*helm info*"))))

  (defun my/variable-make-local-and-toggle (sym)
    "Make a variable buffer-local and toggle"
    (interactive "SVariable: ")
    (let ((new-value (not (and (boundp sym) (eval sym)))))
      (when (not (local-variable-p sym)) (make-local-variable sym))
      (set sym new-value)
      (message (format "%S: %S" sym new-value))))

  (defun my/undo-auto-save-make-local-and-toggle ()
    "Make `undo-tree-auto-save-history' buffer-local and toggle"
    (interactive)
    (my/variable-make-local-and-toggle 'undo-tree-auto-save-history))

  (my/def-variable-toggle company-quickhelp-mode)

  (defun my/shell-command-process-region-as-file
      (start end command &optional output-buffer error-buffer)
    "Process the region as a file with COMMAND and replace with output.

The command should use %s to represent the filename. When called interactively,
acts on the region if active, or else the entire buffer."
    (interactive
     (list (if (region-active-p) (region-beginning) (point-min))
           (if (region-active-p) (region-end) (point-max))
           (read-shell-command "run on current file: ")
           0
           shell-command-default-error-buffer))
    (let ((curbuf   (current-buffer))
          (tmpfile  (make-temp-file "process-region")))
      (append-to-file (buffer-substring start end) nil tmpfile)
      (when (and output-buffer (not (bufferp output-buffer)))
        (delete-region start end))
      ;; Since the shell command may contain multiple occurrences of "%s",
      ;; #'format may need multiple repetitions of TMPFILE to substitute.
      ;; We count the number of occurrences of "%s" in command and provide that
      ;; may repetitions of TMPFILE in the format argument list.
      ;; This may result in too many repetitions if COMMAND contains escaped %s,
      ;; but that's okay. The excess arguments to format are ignored.
      (let* ((n (s-count-matches "%s" command))
             (repeated-tmpfile (-repeat n tmpfile))
             (resolved-command (apply #'format command repeated-tmpfile)))
        (shell-command resolved-command output-buffer error-buffer))))

  (defun my/shell-command-replace-region
      (start end command &optional error-buffer display-error-buffer)
    "Process the region as input with COMMAND and replace with output.

If the region is not active, the entire buffer is processed."
    (interactive
     (list (if (region-active-p) (region-beginning) (point-min))
           (if (region-active-p) (region-end) (point-max))
           (read-shell-command "run shell command: ")
           shell-command-default-error-buffer
           t))
    (let ((curbuf  (current-buffer)))
      (shell-command-on-region start end command t t
                               error-buffer display-error-buffer)))

  (evil-define-command my/evil-shell-command-replace-region
    (start end type command &optional error-buffer display-error-buffer)
    "Process the region as input with COMMAND and replace with output.

If the region is not active, the entire buffer is processed."
    (interactive
     (let ((selection (evil-visual-range)))
       (list
        (if (evil-visual-state-p) (nth 0 selection) (point-min))
        (if (evil-visual-state-p) (nth 1 selection) (point-max))
        (if (evil-visual-state-p) (nth 2 selection) "inclusive")
        (read-shell-command "run shell command: ")
        shell-command-default-error-buffer
        t)))
    (let ((curbuf    (current-buffer))
          (tempbuf   (generate-new-buffer "*evil-shell-command-replace-region*")))
      (evil-yank start end type)
      (with-temp-buffer
        (evil-paste-before 1)
        (shell-command-on-region (point-min) (point-max) command t t
                                 error-buffer display-error-buffer)
        (evil-visual-select (point-min) (point-max) type)
        (evil-yank (point-min) (point-max) type))
      (evil-visual-restore)
      (evil-visual-paste 1)))

  (defun my/kill-buffer-quit-help ()
    "Kill the current buffer, close its window, and quit the help buffer."
    (interactive)
    (kill-buffer-and-window)
    (quit-window nil (get-buffer-window "*Help*")))

  ;; TODO: find out how to identify active window before minibuffer entry
  ;;       so this can be called with M-x
  (defun my/delete-window-ace-move-buffer ()
    (interactive)
    (require 'ace-window)
    (let ((b (current-buffer))
          (w (aw-select "move to window:")))
      (delete-window)
      (set-window-buffer w b)))

  (defun my/delete-window-ace-move-buffer-select ()
    (interactive)
    (require 'ace-window)
    (let ((b (current-buffer))
          (w (aw-select "move to window:")))
      (delete-window)
      (set-window-buffer w b)
      (select-window w)))

  (defun my/delete-window-ace-move-buffer-quit-help ()
    (interactive)
    (require 'ace-window)
    (let ((b (current-buffer))
          (w (aw-select "move to window:")))
      (quit-window nil (get-buffer-window "*Help*"))
      (delete-window)
      (set-window-buffer w b)))

  (defun my/web-mode-normalize-html ()
    (interactive)
    (my/shell-command-process-region-as-file "hxnormalize '%s'"))

  (defun my/underscore-to-camelcase()
    (interactive)
    (save-excursion (when (< (mark) (point))
                      (exchange-point-and-mark))
                    (while (search-forward-regexp (pcre-to-elisp/cached "([a-zA-Z0-9])_([a-zA-Z0-9])")
                                                  (when (region-active-p) (max (point) (mark)))
                                                  t)
                      (replace-match (concat (match-string 1) (upcase (match-string 2)))))))

  (defun my/dash-to-camelcase()
    (interactive)
    (save-excursion (when (< (mark) (point))
                      (exchange-point-and-mark))
                    (while (search-forward-regexp (pcre-to-elisp/cached "(\\w)-(\\w)")
                                                  (when (region-active-p) (max (point) (mark)))
                                                  t)
                      (replace-match (concat (match-string 1) (upcase (match-string 2)))))))

  ;; NOTE:
  ;; when terminal is opened *starting* in a git repo with changes, attempting to use
  ;; completion kills the shell (ZSH segfaults).
  ;; - This also happens with call-process and with emacs's ansi-term and multiterm
  ;; - changing ZSH theme doesn't fix this
  ;; - also happens when setting initial directory with
  ;;   (start-process "terminal" nil "roxterm" "-d" "DIR")
  ;; - also happens if you start a subshell from the initial shell
  (defun my/launch-standalone-terminal ()
    "Launch an external terminal emulator in the current directory.

   With a prefix-argument, launch in spacemacs private directory."
    (interactive)
    (if current-prefix-arg
        (start-process "terminal" nil "x-terminal-emulator"
                       "-d" (expand-file-name (file-name-as-directory "private")
                                              user-emacs-directory))
      (start-process "terminal" nil "x-terminal-emulator")))

  (defmacro no-helm-limit (&optional forms)
    "Execute FORMS without any `helm-candidate-number-limit' in effect."
    (declare (debug 'body))
    `(let ((helm-candidate-number-limit nil))
       ,forms))

  (defun my/execute-menu-command ()
    (interactive)
    (let ((helm-candidate-number-limit nil))
      (lacarte-execute-menu-command '(local minor global))))

  (defun my/execute-command ()
    (interactive)
    (let ((helm-candidate-number-limit nil))
      (lacarte-execute-command '(local minor global))))

  (defhydra my/menu-execute (global-map "C-x <f10>" :color blue :columns 1)
    "Choose from local minor or global commands"
    ("l" (no-helm-limit (lacarte-execute-menu-command '(local)))  "local")
    ("m" (no-helm-limit (lacarte-execute-menu-command '(minor)))  "minor")
    ("g" (no-helm-limit (lacarte-execute-menu-command '(global))) "global")
    ("a" (no-helm-limit (lacarte-execute-menu-command '(local minor global))) "all")
    ("ESC" nil "abort"))
  (global-set-key (kbd "C-x <f10>") 'my/menu-execute/body)

  (defun my/swap-windows (w1 w2)
    (let ((b1 (window-buffer w1))
          (b2 (window-buffer w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)))

  (defun my/frame-windows (&optional frame)
    (cl-remove
     nil
     (car
      (gethash
       (or frame (selected-frame))
       window-numbering-table))))

  (defun my/window-swap-with-next ()
    (interactive)
    (let* ((ws (my/frame-windows))
           (w1 (selected-window))
           (w2 (if (equal (aref ws 1) ws)
                   (aref ws 2)
                 (aref ws 1))))
      (message "(my/swap-windows %S %S)" w1 w2)
      (my/swap-windows w1 w2)))

  (defun my/copy-matching-lines (regexp &optional unique-buffer append-results)
    "Copy lines containing a match for REGEXP and display in a new buffer.

If UNIQUE-BUFFER is non-nil, a new *copy-matching* buffer is used, even if one
already exists. If APPEND-RESULTS is non-nil, the results are appended to the
results already in the *copy-matching* buffer. The combined results are then
displayed and copied to the kill-ring. Note that APPEND-RESULTS is redundant if
UNIQUE-BUFFER is non-nil."
    (interactive "sCopy lines containing a match for regexp: ")
    (let ((min (if (region-active-p) (region-beginning) (point-min)))
          (max (if (region-active-p) (region-end) nil))
          (resultbuf (get-buffer-create (if unique-buffer
                                            (gensym "*copy-matching*")
                                          "*copy-matching*"))))
      (with-current-buffer resultbuf
        (read-only-mode 0)
        (if append-results
            (goto-char (point-max))
          (erase-buffer)))
      (save-excursion
        (save-match-data
          (goto-char min)
          (while (re-search-forward regexp max t)
            (let ((line (buffer-substring (line-beginning-position 1)
                                          (line-beginning-position 2))))
              (with-current-buffer resultbuf (insert line))))
          (pop-to-buffer resultbuf)
          (kill-ring-save (point-min) (point-max))
          (help-mode)))))

  (defun my/copy-non-matching-lines (regexp &optional unique-buffer append-results)
    "Copy lines not containing a match for REGEXP and display in a new buffer.

If UNIQUE-BUFFER is non-nil, a new *copy-matching* buffer is used, even if one
already exists. If APPEND-RESULTS is non-nil, the results are appended to the
results already in the *copy-matching* buffer. The combined results are then
displayed and copied to the kill-ring. Note that APPEND-RESULTS is redundant if
UNIQUE-BUFFER is non-nil."
    (interactive "sCopy all lines except those containing a match for regexp: ")
    (let ((min (if (region-active-p) (region-beginning) (point-min)))
          (max (if (region-active-p) (region-end) (point-max)))
          (searchbuf (current-buffer))
          (resultbuf (get-buffer-create (if unique-buffer
                                            (gensym "*copy-matching*")
                                          "*copy-matching*"))))
      (with-current-buffer resultbuf
        (read-only-mode 0)
        (if append-results
            (goto-char (point-max))
          (erase-buffer)))
      (save-excursion
        (save-match-data
          (goto-char min)
          (unless (bolp) (forward-line))
          (while (< (point) max)
            (let ((line (buffer-substring (point) (line-end-position))))
              (when (not (string-match-p regexp line))
                (with-current-buffer resultbuf
                  (insert line)
                  (newline))))
            (forward-line))
          (pop-to-buffer resultbuf)
          (kill-ring-save (point-min) (point-max))
          (help-mode)))))

  ;; FIXME: macro was hanging when used in my/convert-kill-to-rectangle.
  ;;        The marker-position was much greater than maximum point.
  (defmacro dolines (spec &rest body)
    "Iterate through the (visible) lines of the current buffer.

SPEC is an optional list of loop variable names (NVAR LINEVAR). NVAR, if
specified, contains the number of the current line. LINEVAR, if specified,
contains the text of the current line. BODY is one or more sexps to execute for
each line."
    (declare (indent 1))
    (let* ((min      (if (region-active-p) (region-beginning) (point-min)))
           (max      (if (region-active-p) (region-end) (point-max)))
           (maxmark  (set-marker (make-marker) max))
           (nvar     (car spec))
           (linevar  (cadr spec))
           (vardefs  nil))
      (when linevar
        (push `(setf ,linevar (buffer-substring (line-beginning-position)
                                                (line-end-position)))
              vardefs))
      (when nvar
        (push `(setf ,nvar (line-number-at-pos (point)))
              vardefs))
      `(progn
         (goto-char ,min)
         (while (< (point) (marker-position ,maxmark))
           ;; set loop variables
           ,@vardefs
           ;; process
           ,@body
           ;; increment
           (forward-line 1)))))

  (defun insertf (string &rest objects)
    "Insert a string at point, formatted with a format-string and arguments."
    (insert (apply #'format string objects)))

  (defun my/insert-space-after (&rest arg)
    (interactive "p")
    (insert-char ?\  (car arg))
    (backward-char (car arg)))

  (defun my/install-bb-spacemacs-layers ()
    (interactive)
    (shell-command "cd ~/.emacs.d/private/layer-groups; git clone https://github.com/TheBB/spacemacs-layers.git bb-spacemacs-layers"))

  (defun my/add-column-marker (beg end &optional group spacing repeat)
    (interactive "r")
    (unless group (setq group 1))
    (let ((endm (save-excursion
                  (goto-char (1- end))
                  (end-of-line)
                  (point-marker))))
      (save-excursion
        (goto-char beg)
        (while (<= (point) (marker-position endm))
          (end-of-line)
          (insert " |")
          (beginning-of-line 2))
        (set-marker endm (line-end-position))
        (align-regexp beg (marker-position endm) "\\(\\s-*\\)|" group spacing repeat)
        (set-marker endm nil))))

  ;; modified version of evil-goto-mark-line
  (evil-define-command my/evil-goto-mark-line (char &optional noerror)
    "Go to the line of the marker specified by CHAR and recentre.
With prefix argument, do not recentre."
    :keep-visual t
    :repeat nil
    :type line
    (interactive (list (read-char)))
    (evil-goto-mark char noerror)
    (evil-first-non-blank)
    (unless current-prefix-arg
      (evil-scroll-line-to-center nil)))

  ;; TODO: scroll-to-center only if target is out of view
  (defun my/evil-insert-resume (count)
    (interactive "p")
    (evil-insert-resume count)
    (evil-scroll-line-to-center nil))

  (defun my/async-shell-command-no-window (command &optional output-buffer error-buffer)
    (interactive "sShell command: ")
    (let ((display-buffer-alist '(("*Async Shell Command*" . (display-buffer-no-window nil)))))
      (async-shell-command command)))

  (defun my/get-non-visible-buffer (&optional filter)
    "Return the first non-visible buffer.

Minibuffer buffers are ignored. If FILTER is supplied, it should be a boolean
function taking a buffer as its argument. Only buffers satisfying FILTER are
considered."
    (let* ((bs (buffer-list)))
      (while
          (let ((b (car bs)))
            (or (get-buffer-window b 'visible)
                (minibufferp b)
                (and filter (not (funcall filter b)))))
        (setq bs (cdr bs)))
      (car bs)))

  (evil-define-operator my/evil-replace-in-region (beg end)
    "Select text and replace in region [BEG, END].

Text is selected using `my/evil-select-region-operator'."
    :move-point nil
    (interactive "<r>")
    (let* ((from-str-range (evil-operator-range))
           (from-str (apply #'buffer-substring from-str-range))
           (to-str (read-from-minibuffer (format "Replace '%s' with: " from-str))))
      (perform-replace from-str to-str t
                       nil                   ;; regex
                       current-prefix-arg    ;; delimited
                       nil nil               ;; repeat-count map
                       beg end)))

  (defun my/line-length (&optional N trim)
    "Return the length of the Nth line.

If N is nil, the current line is measured.
if TRIM is non-nil, leading and trailing whitespace is ignored."
    (save-excursion
      (when N
        (goto-char (point-min))
        (forward-line (1- N)))
      (if trim
         (1+ (- (progn (end-of-line) (re-search-backward "[^ \t\n]" (line-beginning-position) t))
                (progn (back-to-indentation) (point))))
       (- (line-end-position)
          (line-beginning-position)))))

  (defun my/paste-no-properties ()
    "Paste the text at the top of the kill ring, but ignore properties.

In particular, rectangular selections are yanked as whole lines."
    (interactive)
    (insert (current-kill 0)))

  (defmacro my/resulting-position (&rest operations)
    "Return the position that results after OPERATIONS are performed.

All changes are reverted."
    `(save-mark-and-excursion
      ,@operations
      (point)))

  (defmacro my/define-evil-motion-and-center (func)
    "Define a version of an evil jump motion which centers the target line."
    (let* ((func-name (symbol-name func))
           (new-name  (concat "my/" func-name "-and-center"))
           (new-func  (intern new-name))
           (new-doc   (format "Perform `%s' and recenter." func)))
      `(evil-define-motion ,new-func (count)
         ,new-doc
         :jump t
         :type exclusive
         :keep-visual t
         (,func count)
         (evil-scroll-line-to-center nil))))

  (defmacro my/define-command-and-center (func)
    "Define a version of a command which centers the target line."
    (let* ((func-name (symbol-name func))
           (new-name  (concat "my/" func-name "-and-center"))
           (new-func  (intern new-name))
           (new-doc   (format "Perform `%s' and recenter." func)))
      `(defun ,new-func ()
         (interactive)
         (call-interactively #',func)
         (evil-scroll-line-to-center nil))))

  (my/define-command-and-center spacemacs/jump-to-definition)
  (my/define-command-and-center push-button)
  (my/define-evil-motion-and-center evil-search-previous)
  (my/define-evil-motion-and-center evil-search-next)

  ;; -------------------------------------------------------------------------------
  ;; ,-------------,
  ;; | Minor Modes |
  ;; '-------------'

  (defun current-minor-modes ()
    (--filter (and (boundp it) (eval it))
              minor-mode-list))

  (defun add-minor-mode-menu ()
    (interactive)
    (defvar minor-mode-menu (make-sparse-keymap "Minor Modes"))
    (define-key-after
      global-map
      [menu-bar minor-modes]
      (cons "Minor Modes" minor-mode-menu)
      'Perspectives)
    (dolist (mmode (current-minor-modes))
      (eval `(define-key minor-mode-menu [,mmode] '(menu-item ,(symbol-name mmode) ,mmode :help ,(format "Toggle %S" mmode))))))

  (defun helm-describe-minor-modes ()
    (interactive)
    (helm :sources `((name . "minor modes")
                     (candidates . ,(current-minor-modes))
                     (action . describe-function))))

  (defun helm-choose-minor-mode ()
    (interactive)
    (helm :sources `((name . "minor modes")
                     (candidates . ,(current-minor-modes))
                     (action . intern))))

  (defun minor-mode-set-lighter (mode lighter)
    (setcar (cdr (assoc 'mozc-mode minor-mode-alist))
            lighter))

  ;; -------------------------------------------------------------------------------
  ;; ,-----------------------,
  ;; | Temporary Workarounds |
  ;; '-----------------------'
  (defun workarounds()
    (interactive)
    (when (file-readable-p "~/.emacs.d/private/local/workarounds.el")
      (load "~/.emacs.d/private/local/workarounds.el")))
  (eval-after-load "move-text.el"
    `(progn
       (undefun 'move-text--last-line-is-just-newline)
       (workarounds)))

  ;; -------------------------------------------------------------------------------
  ;; ,-------------------,
  ;; | Load Private Data |
  ;; '-------------------'
  (when (file-readable-p "~/.emacs.d/private/private-data.el")
    (load "~/.emacs.d/private/private-data.el"))

  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (winum fuzzy evil-textobj-column nlinum-relative nlinum jsfmt flymake-phpcs ggtags helm-gtags goto-gem elmacro go-guru caml faceup pcre2el pcache alert log4e gntp commenter epc ctable concurrent deferred markdown-mode skewer-mode json-snatcher json-reformat js2-mode parent-mode projectile request haml-mode gitignore-mode frame-cmds pos-tip package-lint flycheck flx moz magit magit-popup git-commit with-editor smartparens iedit anzu evil goto-chg undo-tree sbt-mode scala-mode simple-httpd org ace-jump-mode noflet powerline popwin elfeed hexrgb frame-fns faces+ doremi windata tree-mode dired-sort-menu diminish web-completion-data dash-functional tern go-mode ghc haskell-mode eclim company hydra inflections edn multiple-cursors paredit peg eval-sexp-fu highlight cider seq spinner queue pkg-info clojure-mode epl rust-mode inf-ruby bind-map bind-key fit-frame yasnippet packed anaconda-mode pythonic f dash s helm avy helm-core async auto-complete popup package-build company-quickhelp phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode csv-mode zenburn-theme yapfify xterm-color ws-butler wolfram-mode window-numbering wid-edit+ which-key web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen utop use-package ucs-cmds tuareg top-mode toml-mode toc-org tiny thumb-frm thrift thingatpt+ tagedit strings stan-mode spacemacs-theme spaceline soothe-theme solarized-theme smex smeargle slim-mode shell-pop scss-mode scad-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs replace+ rbenv ranger rake rainbow-delimiters racket-mode racer quelpa qml-mode pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements persp-mode paradox palette ox-gfm orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file ocp-indent nim-mode neotree names nameless naked multi-term mozc move-text move-dup monokai-theme moe-theme mmm-mode minitest merlin matlab-mode markdown-toc magit-gitflow macrostep lua-mode ls lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode lacarte julia-mode json-mode js2-refactor js-doc ivy isearch-prop isearch+ intero insert-shebang info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-mode+ help-fns+ help+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-firefox helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets graphviz-dot-mode google-translate google-this golden-ratio go-eldoc gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md general geiser font-lock+ fn flycheck-rust flycheck-pos-tip flycheck-package flycheck-haskell flx-ido fish-mode firefox-controller fill-column-indicator fancy-battery facemenu+ eyedropper eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-replace evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu evil-adjust eshell-z eshell-prompt-extras esh-help ensime emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies dumb-jump doremi-mac doremi-frm doremi-cmd disaster dirtree dired-sort-menu+ dired+ diff-hl define-word dactyl-mode cython-mode company-web company-tern company-statistics company-shell company-go company-ghci company-ghc company-emacs-eclim company-cabal company-c-headers company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow coffee-mode cmm-mode cmake-mode clojure-snippets clj-refactor clean-aindent-mode clang-format cl-lib-highlight cider-eval-sexp-fu chruby cargo bundler autofit-frame auto-yasnippet auto-highlight-symbol auto-compile arduino-mode alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-automatically-star nil)
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
