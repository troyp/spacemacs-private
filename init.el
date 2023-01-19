;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.


;; ╭──────────╮
;; │          │
;; │  LAYERS  │
;; │          │
;; ╰──────────╯

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')

   dotspacemacs-configuration-layer-path '()

   ;; ───────────────────────────────────────────────────────────────────────────────
   ;; ╭──────────────────────╮
   ;; │ configuration layers │
   ;; ╰──────────────────────╯
   ;;
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     typescript
     yaml
     php
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     c-c++
     (clojure :variables
              ;; clojure-enable-fancify-symbols t
              )
     deft
     elfeed
     emacs-lisp
     ess
     ;; major-modes
     git
     go
     helm
     graphviz
     (haskell :variables
              haskell-enable-shm-support t
              )
     html
     java
     javascript
     lua
     markdown
     nim
     ocaml
     octave
     (org :variables
          org-enable-github-support t
          )
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
     (syntax-checking :variables
                      syntax-checking-enable-by-default t
                      )
     (vinegar :variables
              vinegar-reuse-dired-buffer t)
     vimscript
     ;; LOCAL LAYERS
     drag-n-drop
     no-dots
     perl5
     w3m
     search-engine
     troyp
     )

   ;; ───────────────────────────────────────────────────────────────────────────────
   ;; ╭─────────────────────╮
   ;; │ additional packages │
   ;; ╰─────────────────────╯
   ;;
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     ;; ───────────────────────────────────────────────────────────────────────────────
     ;; ╭───────────╮
     ;; │ libraries │
     ;; ╰───────────╯
     cl-lib-highlight
     (dash            :location (recipe :fetcher github :repo "magnars/dash.el" :files ("dash.el")))
     (dash-functional :location (recipe :fetcher github :repo "magnars/dash.el" :files ("dash-functional.el")))
     diff-hl
     f
     general
     google-this
     names
     s
     tiny
     ;; ───────────────────────────────────────────────────────────────────────────────
     ;; ╭─────────────╮
     ;; │ my packages │
     ;; ╰─────────────╯
     fn
     evil-visual-replace
     ;; ───────────────────────────────────────────────────────────────────────────────
     ;; ╭────────────────╮
     ;; │ other packages │
     ;; ╰────────────────╯
     quelpa-use-package
     column-enforce-mode
     company-quickhelp
     dired-filter
     dired-open
     (dired-subtree :after dired)
     dired-toggle-sudo
     dirtree
     easy-escape
     elmacro
     elnode
     jedi-direx
     evil-easymotion
     evil-snipe
     evil-textobj-column
     evil-vimish-fold
     expand-region
     flycheck-package
     helm-unicode
     jsfmt
     lispy
     lispyville
     loccur
     magit-todos
     mozc
     move-dup
     nameless
     org-web-tools
     origami
     vimish-fold
     visual-regexp-steroids
     wgrep
     wgrep-ack
     wgrep-ag
     wgrep-helm
     wgrep-pt
     yequake
     )

   ;; ───────────────────────────────────────────────────────────────────────────────
   ;; ╭───────────────────╮
   ;; │ excluded packages │
   ;; ╰───────────────────╯

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))


;; ╭────────╮
;; │        │
;; │  INIT  │
;; │        │
;; ╰────────╯

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
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
   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil)
  )

;; ╭────────────╮
;; │            │
;; │  USER-ENV  │
;; │            │
;; ╰────────────╯

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )
;; WARNING: if the spacemacs installation is messed up (eg shell commands won't work), the
;; .spacemacs.env file is a likely suspect.

;; ╭─────────────╮
;; │             │
;; │  USER-INIT  │
;; │             │
;; ╰─────────────╯

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;;
  ;; Source directory
  (setq source-directory "/opt/emacs25/src")
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "private/themes/"))

  ;; customize
  (setq custom-file (expand-file-name "private/.customize.el" user-emacs-directory))
  (load custom-file)
  )

;; ╭─────────────╮
;; │             │
;; │  USER-LOAD  │
;; │             │
;; ╰─────────────╯

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

;; ════════════════════════════════════════════════════════════════════════════
;; ═══════════════════════════ ╭────────────────────╮ ═════════════════════════
;; ═══════════════════════════ │                    │ ═════════════════════════
;; ═══════════════════════════ │  SPACEMACS CONFIG  │ ═════════════════════════
;; ═══════════════════════════ │                    │ ═════════════════════════
;; ═══════════════════════════ ╰────────────────────╯ ═════════════════════════
;; ════════════════════════════════════════════════════════════════════════════

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭────────────────────────────╮
  ;; │ function/macro indentation │
  ;; ╰────────────────────────────╯

  (setq lisp-indent-function 'common-lisp-indent-function)

  (defun my/toggle-indent-function ()
    (interactive)
    (if (eq lisp-indent-function 'lisp-indent-function)
        (setq lisp-indent-function 'common-lisp-indent-function)
      (setq lisp-indent-function 'lisp-indent-function))
    (message (format "Indent function: %S" lisp-indent-function)))

  (eval-after-load 'cl-indent
    `(progn
       (put 'if 'common-lisp-indent-function 2)
       (put 'use-package 'common-lisp-indent-function 1)
       ))

  (put 'setq 'lisp-indent-function 1)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────────────────────╮
  ;; │ custom definition macros │
  ;; ╰──────────────────────────╯

  (defmacro my/def-variable-toggle (var)
    (let* ((fname `(concat "my/toggle-" (symbol-name ',var)))
           (fsym  (intern (eval fname))))
      `(defun ,fsym ()
         "Defined with `my/def-variable-toggle'."
         (interactive)
         (setq ,var (not ,var))
         (message "%S is %s" ',var (if ,var "enabled" "disabled")))))

  (defmacro my/define-named-variable-toggle (name var)
    `(defun ,name ()
       "Defined with `my/def-variable-toggle'."
       (interactive)
       (setq ,var (not ,var))
       (message "%S is %s" ',var (if ,var "enabled" "disabled"))))

  (defmacro my/def-variable-cycle (var &rest values)
    (let* ((fname `(concat "my/cycle-" (symbol-name ',var)))
           (fsym  (intern (eval fname))))
      `(defun ,fsym ()
         "Defined with `my/def-variable-toggle'."
         (interactive)
         (let ((idx (-elem-index ,var ',values))
               (n   (length ',values)))
           (if (or (null idx)
                   (= idx (1- n)))
               (setq ,var (nth 0 ',values))
             (setq ,var (nth (1+ idx) ',values))))
         (message "%S is now: %s" ',var ,var))))

  (defmacro my/define-named-variable-cycle (name var values)
    (declare (indent 1))
    `(defun ,name ()
       "Defined with `my/def-variable-toggle'."
       (interactive)
       (let ((idx (-elem-index ,var ',values))
             (n   (length ',values)))
         (if (or (null idx)
                 (= idx (1- n)))
             (setq ,var (nth 0 ',values))
           (setq ,var (nth (1+ idx) ',values))))
       (message "%S is now: %s" ',var ,var)))

  (defmacro my/def-variable-local-cycle (var &rest values)
    (let* ((fname `(concat "my/cycle-" (symbol-name ',var)))
           (fsym  (intern (eval fname))))
      `(defun ,fsym ()
         "Defined with `my/def-variable-toggle'."
         (interactive)
         (let ((idx (-elem-index ,var ',values))
               (n   (length ',values)))
           (when (not (local-variable-p ',var)) (make-local-variable ',var))
           (if (or (null idx)
                   (= idx (1- n)))
               (setq ,var (nth 0 ',values))
             (setq ,var (nth (1+ idx) ',values))))
         (message "%S is now: %s" ',var ,var))))

;; ───────────────────────────────────────────────────────────────────────────────

  ;; (setq-default tab-always-indent t)
  (spacemacs/toggle-line-numbers-on)
  (menu-bar-mode)
  (scroll-bar-mode)
  (minibuffer-depth-indicate-mode 1)
  (setq evil-search-highlight-persist nil)
  (setq left-fringe-width 16)    ;; displays diff-hl mode well

  (setq switch-to-visible-buffer nil)

  ;; not working?
  (setq-default evil-esc-delay 0.00001)

  (setq auto-completion-enable-help-tooltip t)

  (setq scroll-preserve-screen-position 1)

  (setq scroll-margin 1)

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

  (setq browse-url-generic-program "/opt/firefox/firefox")
  (setq browse-url-browser-function 'browse-url-generic)
  (setq engine/browser-function 'browse-url-generic)

  ;; follow VC'ed symlinks
  (setq vc-follow-symlinks t)

  ;; undo
  (setq undo-outer-limit 50000000)

  ;; columns
  (defvar my/default-goto-column 80)
  (setq fci-rule-column 80)
  (setq fill-column 79)

  ;; ╭────────╮
  ;; │ Backup │
  ;; ╰────────╯
  (setq make-backup-files        t
        version-control          t
        kept-new-versions        10
        kept-old-versions        1
        delete-old-versions      t
        backup-by-copying        t
        vc-make-backup-files     t
        )
  (setq backup-directory-alist
        `(("" . ,(expand-file-name "backup/per-save" spacemacs-private-directory))))

  ;; modified from https://stackoverflow.com/a/20824625/1261964
  (defun so/force-backup-of-buffer ()
    "Make a special 'per session' backup at the first save of each emacs session."
    (when (not buffer-backed-up)
      ;; Override the default parameters for per-session backups.
      (let ((backup-directory-alist
             `(("" . ,(expand-file-name "backup/per-session" spacemacs-private-directory))))
            (kept-new-versions 3))
        (backup-buffer)))
    ;; Make a "per save" backup on each save.  The first save results in
    ;; both a per-session and a per-save backup, to keep the numbering
    ;; of per-save backups consistent.
    (let ((buffer-backed-up nil))
      (backup-buffer)))

  (add-hook 'before-save-hook  'so/force-backup-of-buffer)

  ;; ╭───────────╮
  ;; │ Find File │
  ;; ╰───────────╯
  ;; https://stackoverflow.com/a/18026067/1261964

  ;; Open files and go places like we see from error messages, i e: path:line:col
  ;; (to-do "make `find-file-line-number' check if the file exists")
  (defadvice find-file (around find-file-line-number
                               (path &optional wildcards)
                               activate)
    "Turn files like file.js:14:10 into file.js and going to line 14, col 10."
    (save-match-data
      (let* ((match (string-match "^\\(.*?\\):\\([0-9]+\\):?\\([0-9]*\\)$" path))
             (line-no (and match
                           (match-string 2 path)
                           (string-to-number (match-string 2 path))))
             (col-no (and match
                          (match-string 3 path)
                          (string-to-number (match-string 3 path))))
             (path (if match (match-string 1 path) path)))
        ad-do-it
        (when line-no
          ;; goto-line is for interactive use
          (goto-char (point-min))
          (forward-line (1- line-no))
          (when (> col-no 0)
            (forward-char (1- col-no)))))))

  ;; ╭──────────────────╮
  ;; │ Spacemacs Config │
  ;; ╰──────────────────╯

  (defun my/dotspacemacs/refresh-init ()
    (interactive)
    (dotspacemacs|call-func dotspacemacs/init))
  (defun my/dotspacemacs/refresh-user-init ()
    (interactive)
    (dotspacemacs|call-func dotspacemacs/user-init))
  (defun my/dotspacemacs/refresh-user-config ()
    (interactive)
    (dotspacemacs|call-func dotspacemacs/user-config))
  (defun my/configuration-layer/sync ()
    (interactive)
    (configuration-layer/sync))

  ;; ╭───────────────╮
  ;; │ CUA Rectangle │
  ;; ╰───────────────╯

  (setq cua-enable-cua-keys nil)
  (cua-mode t)
  ;; stop cua breaking C-x C-x in visual-line selections
  (add-hook 'cua-mode-hook
            (fn: define-key cua-global-keymap [remap exchange-point-and-mark] nil))
  (define-key cua-global-keymap [remap exchange-point-and-mark] nil)

  (defun my/cua-rectangle-toggle ()
    (interactive)
    (cond (cua-rectangle-mark-mode
           (cua-rectangle-mark-mode -1)
           (cua-cancel))
          (t
           (evil-emacs-state)
           (cua-rectangle-mark-mode 1))))

  (defun my/keyboard-quit ()
    (interactive)
    (if (equal last-command 'my/keyboard-quit)
        (evil-normal-state)
      (keyboard-quit)))

  ;; ╭─────────────────╮
  ;; │ CUA Global Mark │
  ;; ╰─────────────────╯

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

  ;; ╭──────────╮
  ;; │ avy-keys │
  ;; ╰──────────╯
  (setq avy-keys (list ?j ?f ?k ?d ?l ?s ?\; ?a ?i ?e ?o ?w ?n ?v))
  (setq avy-timeout-seconds 0.2)

  ;; ╭────────────────╮
  ;; │ global-hl-line │
  ;; ╰────────────────╯
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

  ;; ╭─────────╮
  ;; │ pos-tip │
  ;; ╰─────────╯
  (setq x-gtk-use-system-tooltips nil)
  (defun my/pos-tip-set-brown-on-yellow ()
    (interactive)
    (setq pos-tip-foreground-color "#3e3030")
    (setq pos-tip-background-color "#ffffe0"))
  (defun my/pos-tip-set-light-grey-on-purple ()
    (interactive)
    (setq pos-tip-foreground-color "#b2b2b2")
    (setq pos-tip-background-color "#5e5079"))
  (defun my/pos-tip-set-blue-grey-light-on-dark ()
    (interactive)
    (setq pos-tip-foreground-color "#9a9aba")
    (setq pos-tip-background-color "#34323e"))

  ;; ╭─────────────────╮
  ;; │ Keyboard Macros │
  ;; ╰─────────────────╯
  (define-key kmacro-keymap (kbd "<insert>") 'insert-kbd-macro)
  (define-key kmacro-keymap (kbd "M-l") 'helm-execute-kmacro)
  (define-key kmacro-keymap (kbd "M-e") 'edit-last-kbd-macro)
  (define-key kmacro-keymap (kbd "M-s") 'my/set-named-kbd-macro-as-last)

  (defun my/kmacro-p (sym)
    (or (get sym 'kmacro)
        (stringp (symbol-function sym))
        (vectorp (symbol-function sym))))

  (defun my/kmacro-call (kmac)
    (interactive)
    (kmacro-exec-ring-item (list kmac 0 "%d") nil))

  (defun my/describe-kmacro-command (cmd)
    (interactive
     (list
      (intern
       (completing-read
        "Kbd macro command: " obarray
        (lambda (fn)
          (and (commandp cmd)
               (symbol-name cmd)
               (my/kmacro-p cmd)))
        t))))
    (describe-function cmd t))

  (defun my/describe-kmacro-function (fn)
    (interactive
     (list
      (intern
       (completing-read
        "Kbd macro function: " obarray
        (lambda (fn)
          (and (fboundp fn)
               (symbol-name fn)
               (my/kmacro-p fn)))
        t))))
    (describe-function fn))

  (defmacro my/kmacro-fset (symbol arg2 &optional arg3)
    "Set SYMBOL's function definition and kmacro property.

Returns the function definition."
    (declare (indent 1))
    (let ((definition (or arg3 arg2))
          (docstring  (and arg3 arg2)))
      `(progn
         (fset ,symbol ,definition)
         (put ,symbol 'kmacro t)
         (when ,docstring
           (put ,symbol 'function-documentation ,docstring))
         ,definition)))

  (defun my/set-last-kbd-macro (defn)
    (setq last-kbd-macro defn))

  (defun my/set-named-kbd-macro-as-last (fn)
    (interactive
     (list
      (intern
       (completing-read
        "Kbd macro function: " obarray
        (lambda (fn)
          (and (fboundp fn)
               (symbol-name fn)
               (my/kmacro-p fn)))
        t))))
    (setq last-kbd-macro fn))

  (defun my/evil-substitute (beg end pcre replacement  &optional flags)
    "Substitute REPLACEMENT for PCRE within region (or current line if no region active).

PCRE is a PCRE-style regex. REPLACEMENT is a replacement string using \n to represent groups.
FLAGS is a list of characters, eg '(?g)"
    (interactive)
    (evil-ex-substitute
     beg end
     (evil-ex-make-substitute-pattern (pcre-to-elisp pcre) flags)
     `(replace-eval-replacement . ,replacement)))

  (defun my/evil-substitute-region (pcre replacement &optional flags)
    "Substitute REPLACEMENT for PCRE within region (or current line if no region active).

PCRE is a PCRE-style regex. REPLACEMENT is a replacement string using \n to represent groups.
FLAGS is a list of characters, eg '(?g)"
    (interactive)
    (let ((beg (if (region-active-p) (region-beginning) (line-beginning-position)))
          (end (if (region-active-p) (region-end) (line-end-position))))
      (my/evil-substitute beg end pcre replacement flags)))

  ;; ╭────────────────╮
  ;; │ thing-at-point │
  ;; ╰────────────────╯

  (defvar my/thing-at-point-qualifed-name-regexp "[a-zA-Z_][a-zA-Z_.]*")
  (put 'qualified-name 'bounds-of-thing-at-point
       (lambda ()
         (let ((thing (thing-at-point-looking-at
                       my/thing-at-point-js-qualifed-name-regexp 100)))
           (when thing
             (cons (match-beginning 0) (match-end 0))))))
  (put 'qualified-name 'thing-at-point
       (lambda ()
         (let ((bounds (bounds-of-thing-at-point 'qualified-name)))
           (when bounds
             (buffer-substring-no-properties (car bounds) (cdr bounds))))))
  (defun my/qualified-name-at-point ()
    (interactive)
    (thing-at-point 'qualified-name))

  ;; ╭──────╮
  ;; │ TAGS │
  ;; ╰──────╯

  (setq my/evil-tags-file (expand-file-name "repos/evil" spacemacs-private-directory))
  (setq my/spacemacs-tags-file user-emacs-directory)

  (defun view-tag-other-window (tagname &optional next-p regexp-p)
    "Same as `find-tag-other-window' but doesn't move the point"
    ;; https://www.emacswiki.org/emacs/EmacsTags#tags
    (interactive (find-tag-interactive "View tag other window: "))
    (let ((window (get-buffer-window)))
      (find-tag-other-window tagname next-p regexp-p)
      (recenter 0)
      (select-window window)))

  ;; ctags not working
  (defun my/dactyl-ctags-generate ()
    (interactive)
    (shell-command
     "ctags-exuberant -e -R .pentadactylrc *.js *.jsm *.penta *.cpp *.h *.idl *.awk *.sh *.py"))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────╮
  ;; │               │
  ;; │  ENVIRONMENT  │
  ;; │               │
  ;; ╰───────────────╯

  ;; (setenv "PATH" (concat "/home/troy/.nvm/versions/node/v0.12.7/bin" ":" (getenv "PATH")))
  ;; (add-to-list 'exec-path "/home/troy/.nvm/versions/node/v0.12.7/bin")

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

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────╮
  ;; │          │
  ;; │  THEMES  │
  ;; │          │
  ;; ╰──────────╯

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

  (defvar-local my/restore-mode-line-cookies nil)
  (defun my/face-restore-mode-line ()
    (interactive)
    (while my/restore-mode-line-cookies
      (face-remap-remove-relative (pop my/restore-mode-line-cookies))))
  (defun my/face-remap-mode-line (&rest specs)
    (interactive)
    (push (apply #'face-remap-add-relative 'mode-line specs)
          my/restore-mode-line-cookies))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────────────╮
  ;; │                       │
  ;; │  additional packages  │
  ;; │                       │
  ;; ╰───────────────────────╯

  ;; ;; install quelpa-use-package using quelpa
  ;; (quelpa
  ;;  '(quelpa-use-package
  ;;    :fetcher github
  ;;    :repo "quelpa/quelpa-use-package"))
  ;; (require 'quelpa-use-package)
  ;; (package-install 'quelpa-use-package)
  (require 'quelpa-use-package)
  ;; (setq use-package-ensure-function 'use-package-ensure-elpa)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────────────────╮
  ;; │ additional local packages │
  ;; ╰───────────────────────────╯

  (add-to-load-path "~/.emacs.d/private/local/")
  (add-to-load-path "~/.emacs.d/private/local/firefox-protocol")
  ;; MY PACKAGES
  (add-to-load-path "~/.emacs.d/private/local/t")

  (defvar my/dotspacemacs-additional-local-packages)
  (setf my/dotspacemacs-additional-local-packages
        '(
          csv-mode
          firefox-protocol
          highlight
          ibuffer-hydra
          ;; mode-compile
          ;; mode-compile-kill
          moz
          sort-group-lines
          top-mode
          tridactyl-mode
          ;; Drew Adams packages
          apu
          ascii-art-to-unicode
          autofit-frame
          dired+
          dired-sort-menu
          dired-sort-menu+
          doremi doremi-cmd doremi-frm doremi-mac
          eyedropper
          facemenu+
          faces+
          find-func+
          fit-frame
          font-lock+
          frame-cmds frame-fns
          help-fns+ help-mode+ help+ help-macro+
          hexrgb
          isearch+ isearch-prop
          lacarte
          naked
          nim-mode
          palette
          replace+ strings
          symbol-overlay
          thingatpt+
          thumb-frm
          ucs-cmds
          wid-edit+
          zoom-frm
          ;;
          t
          ))
  (loop for pkg in my/dotspacemacs-additional-local-packages do
        (require pkg nil :noerror))

  (add-to-load-path "/home/troy/.emacs.d/private/repos/elisp-utils/")
  (require 'elisp-utils)
  (add-to-load-path "/home/troy/.emacs.d/private/repos/tempgit/")
  (add-to-load-path "/home/troy/.emacs.d/private/repos/asoc.el/")
  (require 'asoc)
  (add-to-load-path "/home/troy/.emacs.d/private/repos/asciiboxes/")

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────────╮
  ;; │                   │
  ;; │  auto-mode alist  │
  ;; │                   │
  ;; ╰───────────────────╯

  (add-to-list 'auto-mode-alist '("\\.dired" . dired-virtual-mode))
  (add-to-list 'auto-mode-alist '("\\.jshintrc" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.jsm" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.keynavrc" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.maff" . archive-mode))
  (add-to-list 'auto-mode-alist '("\\.mlterm\.*" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.ocamlinit" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.penta\\(dactylrc\\)?" . dactyl-text-mode))
  (add-to-list 'auto-mode-alist '("\\.pryrc" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.sc" . scala-mode))
  (add-to-list 'auto-mode-alist '("\\.sh" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.vimpagerrc" . vimrc-mode))
  (add-to-list 'auto-mode-alist '("\\.zsh" . sh-mode))
  (add-to-list 'auto-mode-alist '("mimeapps\\.list" . conf-mode))
  (add-to-list 'auto-mode-alist '("tridactylrc" . tridactyl-mode))
  (add-to-list 'auto-mode-alist '("tridactyl.*\\.el" . emacs-lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.tri$" . tridactyl-mode))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────────╮
  ;; │             │
  ;; │  evil-mode  │
  ;; │             │
  ;; ╰─────────────╯

  ;; ;; prevent cursor from moving back a space at the end of a line
  ;; (setq evil-move-cursor-back nil)

  ;; ╭─────────────────────────╮
  ;; │ evil-symbol-word-search │
  ;; ╰─────────────────────────╯

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

  ;; ╭──────────────╮
  ;; │ Text Objects │
  ;; ╰──────────────╯

  (spacemacs|define-text-object "." "dot" "." ".")
  (spacemacs|define-text-object "h" "helplink" "`" "'")
  (spacemacs|define-text-object "q" "curlquote" "‘" "’")
  (spacemacs|define-text-object (kbd "C-]") "corner-bracket" "「" "」")
  (spacemacs|define-text-object "m" "comment" "/* " " */")

  (evil-define-text-object evil-inner-defun (count &optional beg end type)
    "operates on the top-level sexp around point."
    (save-excursion
      (mark-defun)
      (list (+ (point) 2)
            (- (mark) 2))))
  (evil-define-text-object evil-a-defun (count &optional beg end type)
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
  (evil-define-text-object evil-a-line (count &optional beg end type)
    (list (line-beginning-position) (line-end-position)))

  ;; FIXME
  (evil-define-text-object evil-inner-sexp  (count &optional beg end type)
    (let ((sexp-bounds (cdr (sexp-at-point-with-bounds))))
      (list (1+ (car sexp-bounds)) (1- (cdr sexp-bounds)))))
  (evil-define-text-object evil-a-sexp  (count &optional beg end type)
    (let ((sexp-bounds (cdr (sexp-at-point-with-bounds))))
      (list (car sexp-bounds) (cdr sexp-bounds))))

  (evil-define-text-object evil-inner-element (count &optional beg end type)
    (list (+ 1 (web-mode-tag-end-position (web-mode-element-beginning-position (point)))) (web-mode-tag-beginning-position (web-mode-element-end-position (- (point) 1)))))
  (evil-define-text-object evil-a-element (count &optional beg end type)
    (list (web-mode-element-beginning-position (point)) (+ 1 (web-mode-element-end-position (point)))))

  (evil-define-text-object evil-inner-simple-paragraph  (count &optional beg end type)
    (list (save-excursion (search-backward-regexp my/blank-line-regexp) (next-line) (point))
          (save-excursion
            (search-forward-regexp my/blank-line-regexp)
            ;; handle case of overshoot (when blank line at end contains whitespace)
            (when (my/line-at-point-blank-p) (previous-line) (end-of-line) (forward-char))
            (point))))
  (evil-define-text-object evil-a-simple-paragraph  (count &optional beg end type)
    (list (save-excursion (search-backward-regexp my/blank-line-regexp) (point))
          (save-excursion
            (search-forward-regexp my/blank-line-regexp)
            (next-line)
            ;; handle case of overshoot (when blank line at end contains whitespace)
            (unless (my/line-at-point-blank-p) (previous-line) (end-of-line) (forward-char))
            (point))))

  (setq my/dividerRegexp (pcre-to-elisp "^ *.?.? *(----------------------------------------------------------------------+|======================================================================|──────────────────────────────────────────────────────────────────────+)|^ "))
  (evil-define-text-object evil-inner-divider (count &optional beg end type)
      (list
       (save-excursion (search-backward-regexp my/dividerRegexp) (next-line) (point))
       (save-excursion (search-forward-regexp my/dividerRegexp) (beginning-of-line) (point))))
  (evil-define-text-object evil-a-divider (count &optional beg end type)
      (list
       (save-excursion (search-backward-regexp my/dividerRegexp) (next-line) (point))
       (save-excursion (search-forward-regexp my/dividerRegexp) (next-line) (beginning-of-line) (point))))

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

  (evil-define-text-object evil-inner-charrun (count &optional beg end type)
    (let* ((c (buffer-substring-no-properties (point) (1+ (point))))
           (charset (if (memq c '(?^ ?- ?\\)) (concat "\\" c) c))
           (right (save-excursion
                    (skip-chars-forward charset)
                    (point)))
           (left (save-excursion
                   (skip-chars-backward charset)
                   (let ((l (point)))
                     (if (< l (1- right)) (1+ l) l)))))
      (list left right)))
  (evil-define-text-object evil-a-charrun (count &optional beg end type)
    (let* ((c (buffer-substring-no-properties (point) (1+ (point))))
           (charset (if (memq c '(?^ ?- ?\\)) (concat "\\" c) c))
           (left (save-excursion
                   (skip-chars-backward charset)
                   (point)))
           (right (save-excursion
                    (skip-chars-forward charset)
                    (point))))
      (list left right)))

  (evil-define-text-object evil-textobj-column-a-WORD
      (count &optional beg end type)
    "Select a WORD column and preceding spaces.

COUNT, BEG, END, and TYPE have no effect."
    (let* ((WORD-col (evil-textobj-column--create-range #'evil-forward-WORD-begin
                                                        #'evil-backward-WORD-begin
                                                        #'evil-forward-WORD-end))
           (WORD-topleft (car WORD-col))
           (bottomright (cadr WORD-col))
           (topleft (save-excursion
                      (goto-char WORD-topleft)
                      (skip-chars-backward " \t")
                      (point))))
      (evil-range topleft bottomright 'rectangle)))

  (evil-define-text-object evil-textobj-column-a-word
      (count &optional beg end type)
    "Select a WORD column and the preceding character.

COUNT, BEG, END, and TYPE have no effect."
    (let* ((word-col (evil-textobj-column--create-range #'evil-forward-word-begin
                                                        #'evil-backward-word-begin
                                                        #'evil-forward-word-end))
           (topleft (car word-col))
           (bottomright (cadr word-col)))
      (evil-range (1- topleft) bottomright 'rectangle)))

  (define-key evil-inner-text-objects-map "d" 'evil-inner-defun)
  (define-key evil-outer-text-objects-map "d" 'evil-a-defun)
  (define-key evil-inner-text-objects-map "f" 'evil-inner-filename)
  (define-key evil-inner-text-objects-map "l" 'evil-inner-line)
  (define-key evil-outer-text-objects-map "l" 'evil-a-line)
  (define-key evil-inner-text-objects-map "<" 'evil-inner-heredoc)
  (define-key evil-inner-text-objects-map "x" 'evil-inner-sexp)
  (define-key evil-outer-text-objects-map "x" 'evil-a-sexp)
  (define-key evil-inner-text-objects-map "0" 'evil-inner-paren)
  (define-key evil-outer-text-objects-map "0" 'evil-a-paren)
  (define-key evil-inner-text-objects-map "9" 'evil-inner-paren)
  (define-key evil-outer-text-objects-map "9" 'evil-a-paren)
  (define-key evil-inner-text-objects-map "C" 'evil-textobj-column-WORD)
  (define-key evil-outer-text-objects-map "C" 'evil-textobj-column-a-WORD)
  (define-key evil-inner-text-objects-map "S" 'evil-textobj-column-word)
  (define-key evil-outer-text-objects-map "S" 'evil-textobj-column-a-word)
  (define-key evil-inner-text-objects-map "e" 'evil-inner-element)
  (define-key evil-outer-text-objects-map "e" 'evil-a-element)
  (define-key evil-inner-text-objects-map "z" 'evil-inner-charrun)
  (define-key evil-outer-text-objects-map "z" 'evil-a-charrun)
  (define-key evil-inner-text-objects-map "" 'evil-inner-simple-paragraph)
  (define-key evil-outer-text-objects-map "" 'evil-a-simple-paragraph)
  (define-key evil-inner-text-objects-map "" 'evil-inner-divider)
  (define-key evil-outer-text-objects-map "" 'evil-a-divider)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────────╮
  ;; │ evil markers │
  ;; ╰──────────────╯
  (load "~/.emacs.d/private/local/binchen-enhance-emacs-evil-global-markers.el")
  (global-set-key (kbd "C-M-'") 'counsel-evil-goto-global-marker)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭────────────╮
  ;; │ evil-snipe │
  ;; ╰────────────╯
  (require 'evil-snipe)
  (evil-snipe-mode +1)
  (setq evil-snipe-scope 'whole-visible)
  (setq evil-snipe-repeat-scope 'whole-buffer)
  (setq evil-snipe-spillover-scope 'whole-buffer)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────╮
  ;; │ evil-surround │
  ;; ╰───────────────╯

  (setq-default
   evil-surround-pairs-alist
   '(
     ( 40            "( " . " )"  )
     ( 91            "[ " . " ]"  )
     ( ?{            "{ " . " }"  )
     ( 41             "(" . ")"   )
     ( 93             "[" . "]"   )
     ( ?}             "{" . "}"   )
     ( ?#            "#{" . "}"   )
     ( ?b             "(" . ")"   )
     ( ?B             "{" . "}"   )
     ( ?>             "<" . ">"   )
     ( ?<            "< " . " >"  )
     ( ?$            "${" . "}"   )
     ( ?\C-'         "`"  . "'"   )
     ( ?\C-/        "/* " . " */" )
     ( ?\C-\\  "jsb -d¦ " . "¦"   )
     ( ?\C-`   "`"        . "`"   )    ;; so <C-`><C-`> can backquote symbol
     (?t . evil-surround-read-tag)
     (?, . evil-surround-read-tag)
     (?f . evil-surround-function)
     (?c . my/evil-surround-character)
     (?\C-m . my/evil-surround-ask)
     (?s . my/evil-surround-string)
     (?z . my/evil-surround-string-mirrored)
     ))

  (defun my/evil-surround-ask ()
    "Read surround pair(s) from minibuffer.
Enter `BEG END' or `BEG' if END delimiter is the same.
If one delimiter is empty, leave a space at beginning or end."
    (let* ((beg (evil-surround-read-from-minibuffer "BEG: " ""))
           (end_ (evil-surround-read-from-minibuffer "END: " ""))
           (end (if (string-empty-p end_) beg end)))
      (cons beg end)))

  ;; FIXME
  (defun my/evil-surround-function-defn ()
    "Read a functionname from the minibuffer and wrap selection in function call"
    ;; adapted from evil-surround-function
    (interactive)
    (let ((fname (evil-surround-read-from-minibuffer "" "")))
      (cond
        ((member major-mode evil-surround-lisp-modes) (cons (format "(defun %s()" (or fname "")) ")"))
        ((eq major-mode python-mode) (cons (format "def %s():" (or fname "")) ""))
        ( t (cons (format "function %s() {" (or fname "")) "}"))
        )))

  (defun my/evil-surround-character ()
    (interactive)
    (let ((c (read-char)))
      (cons (string c) (string c))))

  (defun my/evil-surround-string ()
    (interactive)
    (let ((s (read-string "String: ")))
      (cons s s)))

  (defun my/evil-surround-string-mirrored ()
    (interactive)
    (let ((s (read-string "String: ")))
      (cons s (reverse s))))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────────────╮
  ;; │ evil-visualstar │
  ;; ╰─────────────────╯
  ;; maintain selection, allowing repeated * #
  (setq-default evil-visualstar/persistent t)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭────────╮
  ;; │ cursor │
  ;; ╰────────╯
  (defun my/evil-insert-overwrite-cursor ()
    (set-cursor-color "chartreuse3")
    (setq cursor-type (if overwrite-mode (cons 'hbar 2) (cons 'bar 2))))

  (setq evil-insert-state-cursor #'my/evil-insert-overwrite-cursor)

  (defun my/enter-overwrite-mode ()
    (interactive)
    (call-interactively #'overwrite-mode)
    (evil-refresh-cursor))

  (define-key evil-insert-state-map (kbd "<insert>") 'my/enter-overwrite-mode)

  ;; ╭────────────────────────╮
  ;; │ evil interactive codes │
  ;; ╰────────────────────────╯
  (setq my/evil-interactive-alist-source
        `((name . "evil-interactive-alist")
          (candidates . ,(asoc--map (list (format "%s\t\t\t%s" key value)) evil-interactive-alist))))

  (defun my/evil-interactive-alist-view ()
    (interactive)
    (helm :sources '(my/evil-interactive-alist-source)))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────╮
  ;; │           │
  ;; │  unicode  │
  ;; │           │
  ;; ╰───────────╯

  ;; ╭──────────╮
  ;; │ Digraphs │
  ;; ╰──────────╯


  (setq evil-digraphs-table-user
        '(
          ((?1 ?/) . ?\x215f)    ;; ⅟
          ((?1 ?7) . ?\x2150)    ;; ⅐
          ((?1 ?9) . ?\x2151)    ;; ⅑
          ((?1 ?0) . ?\x2152)    ;; ⅒
          ((?. ? ) . ?\x2024)    ;; ․ 1-dot leader
          ((?. ?/) . ?\x2025)    ;; ‥ 2-dot leader
          ;; compose combinations
          ((?. ?.) . ?\x2026)    ;; (horizontal) ellipsis (replaces ‥)
          ((?0 ?-) . ?\x30fb)    ;; CJK middle-dot
          ((?0 ?9) . ?\x3000)    ;; CJK full-width space
          ((?. ?-) . ?\x00b7)    ;; middle-dot
          ((?| ?2) . ?\x2016)    ;; ‖ double vertical bar
          ((?{ ?+) . ?\x2295)    ;; ⊕ circled plus
          ((?i 34) . ?\x00ef)    ;; ï (also on i:)
          ((?\x22 ?<) . ?\x201c) ;; “
          ((?\x22 ?>) . ?\x201d) ;; ”
          ((?o ?o) . ?\x00b0)    ;; °
          ((?8 ?8) . ?\x221e)    ;; ∞
          ((?- ?0) . ?\x203f)    ;; ‿
          ((?- ?9) . ?\x2040)    ;; ⁀
          ((?| 91) . ?\x27e6)    ;; ⟦
          ((?| 93) . ?\x27e7)    ;; ⟧
          ;; kragen compose combinations
          ((?t ?,) . ?\x0288)    ;; ʈ (replaces ţ which is also available on ,t)
          ((?T ?,) . ?\x01ae)    ;; Ʈ (replaces Ţ which is also available on ,T) - for consistency
          ((?, ?t) . ?\x0163)    ;; ţ (default, but obscured by binding above)
          ((?, ?T) . ?\x0162)    ;; Ţ (default, but obscured by binding above)
          ((?n ?,) . ?\x0273)    ;; ɳ (replaces ţ which is also available on ,n)
          ((?, ?n) . ?\x0146)    ;; ņ (default, but obscured by binding above)
          ((?, ?N) . ?\x0145)    ;; Ņ (default, but obscured by binding above)
          ((?x ?x) . ?\x00d7)    ;; ×
          ((?< ?|) . ?\x21b5)    ;; ↵
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

  ;; ╭─────────────╮
  ;; │ Compose Key │
  ;; ╰─────────────╯

  (define-helm-occur-function "compose"
    "/usr/share/X11/locale/en_US.UTF-8/Compose")

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭────────────────╮
  ;; │                │
  ;; │  KEY BINDINGS  │
  ;; │                │
  ;; ╰────────────────╯

  ;; ╭──────────────────────╮
  ;; │ Keybinding Functions │
  ;; ╰──────────────────────╯
  ;; References:
  ;;    [1] https://www.masteringemacs.org/article/mastering-key-bindings-emacs
  ;; keymap lookup order (from [1] above):
  ;;     1. overriding-terminal-local-map for terminal-specific key binds.
  ;;     2. overriding-local-map for keys that should override all other local keymaps.
  ;;        Be VERY careful if you use this!
  ;;     3. Keymap char property at point for keymaps that are local to the character point is at.
  ;;        This is used for stuff like fields in yasnippet and the customize dialog.
  ;;     4. emulation-mode-map-alists for advanced multi-mode keymap management
  ;;     5. minor-mode-overriding-map-alist for overriding the keymaps used by minor modes in major modes.
  ;;     6. minor-mode-map-alist is exactly like the overriding version above,
  ;;        but the preferred means of specifying the keymaps for minor modes.
  ;;     7. Keymap text property at point is like the one above for char properties but is for text properties only.
  ;;     8. current-local-map for keymaps defined in the buffers’ current local map
  ;;     9. current-global-map is the last place Emacs will look for key binds and it is for the global ones.

  ;; spacemacs macros:   evil-map evil-define-key evil-define-minor-mode-key
  ;;                     evil-define-keymap spacemacs|define-micro-state
  ;; bindkey fns/macros: bind-map bind-key bind-key* bind-keys bind-keys*
  ;;                     bind-keys-form bind-map-add-to-major-mode-list
  ;;                     bind-map-set-keys bind-map-kbd-keys unbind-key
  ;; builtin: global-key-binding local-set-key local-unset-key
  ;; other packages:
  ;;   general <https://github.com/noctuid/general.el#simulating-keypresses>:
  ;;   defhydra
  ;;   hercules <https://gitlab.com/jjzmajic/hercules.el>:
  ;;       provides hydra functionality using which-key
  ;;   smartrep <https://github.com/myuhe/smartrep.el>:
  ;;       allows prefix key for repeating commands
  ;;       eg. C-c n n n ... rather than C-c n C-c n C-c n ...
  ;;   which-key <https://github.com/justbur/emacs-which-key>:
  ;;       display popup showing keybinding completions

  (defun my/define-keys (keymap &rest bindings)
    "Define multiple keys with `define-key'\nBINDINGS has the form KEY DEFN [KEY DEFN ...]"
    (declare (indent 1))
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
  (defmacro defun! (name doc &rest body) `(defun ,name () ,doc (interactive) ,@body))

  ;; ╭────────────────────────────────╮
  ;; │ Macros for generating commands │
  ;; ╰────────────────────────────────╯

  (defmacro my/make-insertion-around-point (before after)
    `(lambda ()
       (interactive)
       (insert ,before ,after)
       (backward-char (length ,after))))

  (defmacro my/define-insertion-around-point  (name before after &optional docstr)
    `(defun ,name ()
       ,docstr
       (interactive)
       (insert ,before ,after)
       (backward-char (length ,after))))

  ;; ╭─────────────────╮
  ;; │ Global Bindings │
  ;; ╰─────────────────╯

  ;; ===== High Priority: override mode bindings =====
  (bind-key* "<C-tab>" 'next-multiframe-window)
  (bind-key* "<C-S-iso-lefttab>" 'previous-multiframe-window)
  (bind-key* "<M-delete>" 'my/kill-buffer-and-window-quit-help)
  (bind-key* "<M-S-delete>" 'kill-buffer-and-window)
  (bind-key* "<S-menu>" 'execute-extended-command)
  (bind-key* "M-X" 'spacemacs/smex)
  (bind-key* "M-x" 'helm-M-x)

  ;; use M-` as universal-argument
  (bind-key* "M-`" 'universal-argument)
  (define-key universal-argument-map (kbd "M-`") 'universal-argument-more)
  ;; =================================================
  (global-set-key (kbd "C-g") 'my/keyboard-quit)

  (global-set-key (kbd "C-M-j") 'evil-avy-goto-char-timer)
  (global-set-key (kbd "C-M-y") 'avy-copy-line)

  (global-set-key (kbd "C-h y") 'describe-symbol)

  (global-set-key (kbd "M-S-x") 'execute-extended-command)

  (global-set-key [\S-f4] 'my/launch-standalone-terminal)
  (global-set-key [f12] 'my/launch-standalone-file-manager)

  (global-set-key [\M-f4] 'kill-buffer-and-window)

  ;; change C-x - from 'shrink-window-if-larger-than-buffer to 'fit-window-to-buffer
  (global-set-key (kbd "\C-x -") 'fit-window-to-buffer)

  ;; NOTE: M-n/M-p/M-N/M-P are now free
  (global-set-key (kbd "M-p") 'evil-scroll-line-down)
  (global-set-key (kbd "M-n") 'evil-scroll-line-up)
  (global-set-key (kbd "M-k") 'scroll-up-line)
  (global-set-key (kbd "M-j") 'scroll-down-line)
  (global-set-key (kbd "M-K")
                  (lambda ()
                    (interactive) (scroll-other-window 1)))
  (global-set-key (kbd "M-J")
                  (lambda ()
                    "Scroll other window down"
                    (interactive) (scroll-other-window-down 1)))

  (global-set-key (kbd "C-M-d") 'scroll-other-window)
  (global-set-key (kbd "C-M-u") 'scroll-other-window-down)
  (global-set-key (kbd "C-M-S-d") 'scroll-other-window-down)

  (global-set-key (kbd "M-[") (fn! (forward-symbol -1)))
  (global-set-key (kbd "M-]") 'forward-symbol)

  (global-set-key (kbd "C-M-a") 'my/beginning-of-defun)
  (global-set-key (kbd "C-M-e") 'my/end-of-defun)
  (global-set-key (kbd "C-M-n") 'my/evil-next-paragraph-beginning)
  (global-set-key (kbd "C-M-p") 'my/evil-previous-paragraph-beginning)

  (global-set-key (kbd "<C-return>") 'evil-cua-toggle)
  (global-set-key (kbd "<C-M-return>") 'my/split-line-and-open-line-above)

  (global-set-key "\C-a" 'move-beginning-of-line-or-text)       ;; troyp/utils.el
  (global-set-key (kbd "<S-return>") 'my/open-line-below)       ;; troyp/utils.el
  (global-set-key (kbd "<C-S-return>") 'my/open-line-above)     ;; troyp/utils.el
  (global-set-key (kbd "C-S-J") 'move-text-down)
  (global-set-key (kbd "C-S-K") 'move-text-up)

  ;; evil-arg
  (global-set-key (kbd "M-j") 'evil-forward-arg)
  (global-set-key (kbd "M-k") 'evil-backward-arg)


  ;; remove C-S-SPC from cua-global-keymap and bind to my/insert-space-after
  (define-key cua-global-keymap (kbd "C-S-SPC") nil)
  (global-set-key (kbd "C-S-SPC") 'my/insert-space-after)
  ;; This binding is intercepted by UIM

  ;; bind cycle-spacing in place of just-one-space
  (global-set-key (kbd "M-S-SPC") 'cycle-spacing)

  (global-set-key (kbd "C-`") 'my/surround-symbol)

  (global-set-key [\M-f12] 'shell-pop)
  (global-set-key (kbd "C-'") 'shell-pop)

  (global-set-key (kbd "C-x a C-'") 'abbrev-prefix-mark)

  (global-set-key (kbd "C-M-;") 'er/expand-region)
  (global-set-key (kbd "C-S-M-;") 'er/contract-region)

  (global-set-key [f1] 'help-map)
  (global-set-key (kbd "<C-f1>") 'describe-prefix-bindings)
  (global-set-key (kbd "<M-f1>") 'describe-key)
  (global-set-key [f5] 'spacemacs-cmds)
  (global-set-key [\C-f5] 'which-key-show-top-level)
  (global-set-key (kbd "<C-f9>") 'evil-normal-state)
  (global-set-key (kbd "<M-f9>") 'evil-evilified-state)
  (global-set-key (kbd "<S-f9>") 'my/current-mode-and-state)
  (global-set-key (kbd "<M-S-f9>") 'my/show-last-command)

  (global-set-key [f7] 'exchange-point-and-mark)
  (global-set-key [f8] 'er/contract-region)
  (global-set-key [f9] 'er/expand-region)

  (global-set-key (kbd "<C-menu>") 'evil-avy-goto-word-0)

  (global-set-key (kbd "M-c") 'evil-upcase-first-letter)
  (global-set-key (kbd "M-C") 'capitalize-word)

  (global-set-key (kbd "C->") 'evil-repeat-pop-next)

  (global-set-key (kbd "<M-insert>") 'org-capture)

  (global-set-key (kbd "C-x C-c")
                  (fn! (when (y-or-n-p "Exit? ")
                         (call-interactively #'save-buffers-kill-terminal))))

  (global-set-key (kbd "C-x r 0") 'rectangle-number-lines-interactive)

  (bind-key* "C-M-x" 'helm-eval-expression-with-eldoc)

  (use-package google-this :bind-keymap ("C-c /" . google-this-mode-submap))

  (bind-keys :map help-map
             ("C-k" . find-function-on-key)
             ("<backspace>" . my/quit-help)
             )

  ;; ╭────────────────╮
  ;; │ Mouse bindings │
  ;; ╰────────────────╯

  (global-set-key (kbd "<mouse-3>")   'my/mouse-toggle-fold)
  (global-set-key (kbd "<mouse-4>")   'mwheel-scroll)
  (global-set-key (kbd "<mouse-5>")   'mwheel-scroll)
  (global-set-key (kbd "<S-mouse-4>") 'my/text-scale-increase-under-mouse)
  (global-set-key (kbd "<S-mouse-5>") 'my/text-scale-decrease-under-mouse)
  (global-set-key (kbd "<C-mouse-4>") 'spacemacs/zoom-frm-in)
  (global-set-key (kbd "<C-mouse-5>") 'spacemacs/zoom-frm-out)
  (global-set-key (kbd "<double-mouse-1>") 'evil-toggle-fold)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────────────────╮
  ;; │                           │
  ;; │  EVIL STATE KEY BINDINGS  │
  ;; │                           │
  ;; ╰───────────────────────────╯

  ;; ╭──────────────╮
  ;; │ normal state │
  ;; ╰──────────────╯
  ;; note: evilified state map uses the bindings for keys:
  ;; / : h j k l n N v V gg G C-f C-b C-e C-y C-d C-u C-z
  ;; when rebinding them for normal-state, rebind for evilified-state also

  ;; note: (evil-define-key 'normal global-map ...) takes precedence over
  ;;       (define-key evil-normal-state-map ...)

  (when (commandp 'my/evil-normal-state-and-cancel)
    (define-key evil-normal-state-map [escape] 'my/evil-force-normal-state-and-cancel))
  (define-key evil-normal-state-map [delete] 'kill-this-buffer)
  (define-key evil-normal-state-map [S-delete] 'delete-window)
  (define-key evil-normal-state-map [C-delete] 'kill-buffer-and-window)
  ;; shift reverses C-d (-scroll-down) and C-o (-jump-backward)
  (define-key evil-normal-state-map (kbd "C-S-d") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-S-o") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  ;; remove C-y (use global M-p)
  (define-key evil-normal-state-map (kbd "C-y") 'evil-paste-before)
  ;; rebind gu to evil-upcase (rather than -downcase), use gl for -downcase:
  (define-key evil-normal-state-map (kbd "gu") 'evil-upcase)
  (define-key evil-normal-state-map (kbd "gl") 'evil-downcase)

  (define-key evil-normal-state-map (kbd "gi") 'my/evil-insert-resume)
  (define-key evil-normal-state-map (kbd "gf") 'link-hint-open-multiple-links)
  (define-key evil-normal-state-map (kbd "gp") 'my/paste-multi)
  ;; quit other window
  (define-key evil-normal-state-map (kbd "gq") 'my/other-window-quit)

  ;; centre after n, N, '
  (when (commandp 'my/evil-ex-search-next-and-center)
    (define-key evil-normal-state-map (kbd "n") 'my/evil-ex-search-next-and-center))
  (when (commandp 'my/evil-ex-search-previous-and-center)
      (define-key evil-normal-state-map (kbd "N") 'my/evil-ex-search-previous-and-center))

  (define-key evil-normal-state-map (kbd "'") 'my/evil-goto-mark-line)
  ;; make | use `fill-column' as a default
  (define-key evil-normal-state-map (kbd "|") 'my/evil-goto-column)

  ;; PCRE search forward/backward
  (when (commandp 'my/evil-pcre-search-forward)
    (define-key evil-normal-state-map (kbd "/") 'my/evil-pcre-search-forward))
  (when (commandp 'my/evil-pcre-search-backward)
    (define-key evil-normal-state-map (kbd "?") 'my/evil-pcre-search-backward))

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
  ;; forward/backward sentence
  (define-key evil-normal-state-map (kbd "M-e") 'evil-forward-sentence-begin)
  (define-key evil-normal-state-map (kbd "M-a") 'evil-backward-sentence-begin)

  ;; evil-window: C-w C-w to swap windows
  (bind-key "C-w" #'my/window-swap-with-next evil-window-map)

  ;; save
  (define-key evil-normal-state-map "ZC" 'save-buffers-kill-terminal)

  ;; ╭─────────────────╮
  ;; │ evilified state │
  ;; ╰─────────────────╯

  (defun evilified-state-init ()
    (my/define-keys evil-evilified-state-map
      (kbd "C-y") nil
      (kbd "C-e") 'end-of-line
      (kbd "C-v") 'evil-visual-block
      ))
  (add-hook 'evil-evilified-state-entry-hook 'evilified-state-init)

  ;; ╭──────────────╮
  ;; │ visual state │
  ;; ╰──────────────╯

  (defun my/evil-move-end-of-line (&optional arg)
    (interactive "^p")
    (evil-move-end-of-line arg))

  (defun insert-space-visual () (interactive) (execute-kbd-macro " ") (evil-visual-restore))
  (define-key evil-visual-state-map (kbd "C-e") 'my/evil-move-end-of-line)
  (define-key evil-visual-state-map (kbd "C-SPC") 'evil-forward-char-or-extend)
  (define-key evil-visual-state-map (kbd "C-\\") 'my/shell-command-replace-region)
  (define-key evil-visual-state-map (kbd "M-u") 'evil-upcase)
  (define-key evil-visual-state-map (kbd "M-l") 'evil-downcase)
  (define-key evil-visual-state-map (kbd "M-=") 'count-region)
  (define-key evil-visual-state-map (kbd ".") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "M-.") 'er/contract-region)
  (define-key evil-visual-state-map (kbd "O") 'evil-visual-rotate)
  (evil-visual-replace-visual-bindings :PCRE)
  ;; Frank Fischer: move region
  ;; Too slow for moving more than a few lines
  (define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map [\C-\S-down] (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map [\C-\S-up] (concat ":m '<-2" (kbd "RET") "gv=gv"))

  ;; ╭──────────────╮
  ;; │ motion state │
  ;; ╰──────────────╯

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

  ;; ╭──────────────╮
  ;; │ insert state │
  ;; ╰──────────────╯

  ;; define bindings for shift-right/left-line: frees C-d, C-t:
  (define-key evil-insert-state-map (kbd "S-<tab>")       'evil-shift-right-line)
  ;; (define-key evil-insert-state-map (kbd "S-<backspace>") 'evil-shift-left-line)
  (define-key evil-insert-state-map (kbd "C-d")   'delete-forward-char)
  (define-key evil-insert-state-map (kbd "C-l")   'delete-forward-char)
  (define-key evil-insert-state-map (kbd "C-M-l") 'backward-delete-char)
  (define-key evil-insert-state-map (kbd "M-h")   'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-t")   'transpose-chars)
  (define-key evil-insert-state-map (kbd "C-S-a") 'evil-paste-last-insertion)
  (define-key evil-insert-state-map (kbd "C-M-v") 'mark-sexp)
  (define-key evil-insert-state-map (kbd "C-a")   'move-beginning-of-line-or-text)
  (define-key evil-insert-state-map (kbd "C-e")   'end-of-line)
  (define-key evil-insert-state-map (kbd "C-M-y") 'evil-copy-from-below)
  (define-key evil-insert-state-map (kbd "C-S-y") 'evil-copy-from-below)
  (define-key evil-insert-state-map (kbd "C-S-o") 'evil-open-above)
  (define-key evil-insert-state-map (kbd "C-S-k") 'kill-line)    ;; or C-o C-k
  (define-key evil-insert-state-map (kbd "C-.")   'my/yas-expand)
  (define-key evil-insert-state-map (kbd "M-?")   'dabbrev-expand)
  (define-key evil-insert-state-map (kbd "C-n")   'next-line)
  (define-key evil-insert-state-map (kbd "C-p")   'previous-line)
  (define-key evil-insert-state-map (kbd "C-M-SPC") 'my/insert-spaces-around-point)

  ;; (define-key evil-insert-state-map (kbd "C-M-SPC") 'hippie-expand)

  ;; unicode insertion
  ;; (define-key evil-insert-state-map (kbd "C-v") 'insert-char)
  (define-key evil-insert-state-map (kbd "C-v") 'helm-unicode)
  (define-key evil-insert-state-map (kbd "M-v") 'iso-transl-ctl-x-8-map)
  (define-key evil-insert-state-map (kbd "C-k") 'evil-insert-digraph)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭────────────────────╮
  ;; │ keymap definitions │
  ;; ╰────────────────────╯

  ;; set function definition to value for keymaps defined only as values
  (fset 'help-map help-map)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────────────────╮
  ;; │ evil leader bindings │
  ;; ╰──────────────────────╯
  ;; can use bind-keys to define prefix maps (Leader map is 'spacemacs-cmds, see below)

  (progn
    (put 'spacemacs/set-leader-keys 'lisp-indent-function '(2))

    (defun my/set-my-leader-keys ()
      (interactive)
      (spacemacs/set-leader-keys
        "a d"          'dired
        "a p"          'my/list-processes
        "a ="          'calculator
        "a -"          'elnode-stop
        "a \""         'browse-url-elinks
        "a SPC"        'helm-run-external-command
        "a C-'"        'browse-url-firefox
        "b M"          'my/switch-to-messages-buffer
        "b S"          'my/switch-to-scratch-buffer-other-window
        "b W"          'my/switch-to-warnings-buffer
        "b -"          'diff-buffer-with-file
        "b +"          'my/switch-to-changelog-buffer
        "b SPC"        'spacemacs/new-empty-buffer
        "b C-b"        'ibuffer
        "b C-e"        'bury-buffer
        "b C-f"        'buffer-face-set
        "b C-n"        'spacemacs/new-empty-buffer
        "b C-u"        'undo-tree-clear
        "b <insert>"   'buffer-major-mode
        "b <f1>"       'about-emacs
        "b <backtab>"    'my/switch-to-most-recent-buffer-other-window
        "c SPC"        'mode-compile
        "c DEL"        'mode-compile-kill
        "e F"          'flycheck-mode
        "e TAB"        'flycheck-next-error
        "e S-TAB"      'flycheck-previous-error
        "f e s s"      'spacemacs-rgrep
        "f e s p"      'spacemacs-private-rgrep
        "f e s o"      'spacemacs-only-rgrep
        "f e s e"      'elpa-rgrep
        "f e SPC c"    'my/dotspacemacs/refresh-user-config
        "f e SPC i"    'my/dotspacemacs/refresh-init
        "f e SPC l"    'my/configuration-layer/sync
        "f e SPC t"    'dotspacemacs/test-dotfile
        "f e SPC u"    'my/dotspacemacs/refresh-user-init
        "f ."          'find-alternate-file
        "f >"          'find-alternate-file-other-window
        "f ' e"        'dired-spacemacs-directory
        "f ' p"        'dired-spacemacs-private-directory
        "f / f"        'sudo-open-file
        "f / e"        'spacemacs/sudo-edit
        "f / b"        'my/sudo-edit-this-file
        "f -"          'my/browse-buffer-directory-firefox
        "f ="          'my/browse-buffer-file-firefox
        "f SPC"        'my/open-file-at-point
        "f <insert>"   'find-file-clipboard
        "f C-."        'find-file-at-point
        "f C-k"        'bookmark-jump
        "f C-y"        'my/yank-filename
        "g g"          'magit-file-popup
        "g C-x v"      'vc-prefix-map
        ;; "h"            'help-prefix-map
        "h a"          'apropos
        "h d C-b"      'describe-personal-keybindings
        "h d C-i"      'my/evil-interactive-alist-view
        "h w"          'help-download-prefix-map
        "h ;"          (defun! my/info-elisp "Open Elisp manual" (info "elisp"))
        "h :"          (defun! my/info-eintr "Open Elisp intro" (info "eintr"))
        "h ."          'count-words
        "h /"          'find-function-prefix-map
        "h 1"          'evil-goto-definition
        "h C-f"        'find-library-other-window
        "h C-m"        'lacarte-execute-menu-command
        "h C-y"        'tsp-info-goto-node
        "h C-/"        'evil-search-highlight-persist-remove-all
        "h C-?"        'evil-search-highlight-restore
        ;; "h <f1>"       'help-map
        "i -"          'tiny-expand
        "j ,"          'evil-avy-goto-word-0
        "m <f10>"      'my/lacarte-menu-execute/lambda-l-and-exit
        "o a"          'asciiheadings-prefix-key-map
        "o a c u"      'my/short-rect-heading-comment-to-unibox
        "o c"          'character-prefix-map
        "o f"          'flycheck-command-map
        "o m"          'modes-prefix-key-map
        "r b"          'bookmark-map
        "s / m"        'my/pcre-multi-occur
        "s / o"        'my/pcre-occur
        "s / l"        'my/pcre-loccur
        "T |"          'scroll-bar-mode
        "w w"          'ace-swap-window
        "w TAB"        'other-window
        "w DEL"        'my/delete-window-ace-move-buffer-quit-help
        "x a ."        'spacemacs/align-repeat-period
        "x a '"        'spacemacs/align-repeat-quote
        "x a \""       'spacemacs/align-repeat-double-quote
        "x a -"        'spacemacs/align-repeat-dash
        "x a #"        'spacemacs/align-repeat-hash
        "x a *"        'spacemacs/align-repeat-star
        "x a C-."      'spacemacs/align-repeat-decimal
        "x a RET"      'my/pcre-align
        "x a C-;"      'spacemacs/align-repeat-semicolon-comment
        "x a C-:"      'my/align-after-colon
        "x a C-/"      'spacemacs/align-repeat-slash-comment
        "x a C-'"      'tsp-align-quoted-column
        "x a C-\""     'tsp-align-double-quoted-column
        "x a SPC"      'my/quick-pcre-align-repeat
        "x a S-SPC"    'my/quick-pcre-align
        "x a C-SPC"    'my/align-whitespace
        "x l g"        'sort-group-lines
        "x l U"        'my/delete-duplicate-lines-nonblank
        "x N"          'rectangle-number-lines-interactive
        "x <insert>"   'region-swap
        "x t r"        'region-swap
        "x /"          'shell-command-on-region
        "x \\"         'my/evil-shell-command-replace-region
        "x |"          'my/shell-command-process-region-as-file
        "x C-b"        'my/copy-to-empty-buffer
        "x C-k"        'evil-insert-digraph
        "x C-l"        'my/quick-pcre-align-repeat
        "x C-SPC"      'my/center-in-whitespace
        "z +"          'spacemacs/scale-font-transient-state/spacemacs/scale-up-font
        "z ="          'spacemacs/scale-font-transient-state/spacemacs/scale-up-font
        "z -"          'spacemacs/scale-font-transient-state/spacemacs/scale-down-font
        "C /"          'my/pick-color
        "C ."          'my/pick-insert-color
        "1"            'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-1-and-exit
        "2"            'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-2-and-exit
        "3"            'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-3-and-exit
        "4"            'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-4-and-exit
        "8"            'spacemacs/enter-ahs-forward
        "*"            'spacemacs/enter-ahs-backward
        "."            'repeat-complex-command
        ","            'helm-mini
        ">"            'my/evil-shift-right-fine-dispatcher
        "<"            'my/evil-shift-left-fine-dispatcher
        "="            'quick-calc
        "-"            'my/dired-jump-and-kill
        "("            'my/add-spacing-inside-parens
        ")"            'my/remove-spacing-inside-parens
        "["            'my/add-spacing-inside-brackets
        "]"            'my/remove-spacing-inside-brackets
        "{"            'my/add-spacing-inside-braces
        "}"            'my/remove-spacing-inside-braces
        "\""           'my/add-spacing-inside-double-quotes
        "C-\""         'my/remove-spacing-inside-double-quotes
        "SPC"          'avy-goto-char-timer
        "S-SPC"        'spacemacs/toggle-maximize-buffer
        "<backtab>"    'my/switch-to-most-recent-buffer
        "<return>"     'helm-buffers-list
        "<f3>"         'kmacro-keymap
        "<f5>"         'spacemacs/safe-revert-buffer
        "<f10>"        'my/lacarte-menu-execute/lambda-a-and-exit
        "C-k"          'my/yank-to-end-of-line
        "C-l"          'my/quick-pcre-align-repeat
        "C-p"          'my/evil-paste-after-as-block
        "C-P"          'my/evil-paste-before-as-block
        "C-r"          'my/replace-line
        "C-v"          'my/cua-rectangle-toggle
        "C-x C-o"      'my/remove-blank-lines
        "C-w"          'delete-frame
        "C-y"          'my/paste-no-properties
        "C-8"          'spacemacs/helm-project-smart-do-search-region-or-symbol
        "C-."          'ido-switch-buffer
        "C-/"          'spacemacs/evil-search-clear-highlight
        "C-?"          'evil-search-highlight-restore
        "C-'"          'my/quote-to-end-of-line
        "C-\\"         'set-input-method
        "C-SPC"        'cua-toggle-global-mark
        "C-TAB"        'ace-window
        "M-S-SPC"      'my/just-one-blank-line
        "M-p"          'my/evil-paste-after-column-kill-height
        "M-q"          'my/wrap-lines-in-region
        "M-x"          'helm-M-x
        "M-%"          'evil-visual-replace-query-replace
        "M-DEL"        'my/kill-other-buffer-and-window
        "M-C-%"        'evil-visual-replace-replace-regexp
        "ESC ESC"      'my/keyboard-escape-quit-and-clear-highlight
        )
      )
    (my/set-my-leader-keys))

  (bind-keys :map spacemacs-cmds
             :prefix-map character-prefix-map
             :prefix "o c"
             :prefix-docstring "Commands that act on the character at point."
             ("i" . insert-char)
             ("=" . describe-char)
             ("a" . what-cursor-position)
             ("p" . palette-foreground-at-point)  ;; palette.el (dadams)
             ("f" . get-char-face)
             ("u" . helm-ucs)
             ("U" . helm-unicode)
             )

  (bind-keys :map spacemacs-cmds
             :prefix-map undo-prefix-map
             :prefix "o u"
             :prefix-docstring "Commands related to undo."
             ("s" . undo-tree-switch-branch)
             ("t" . global-undo-tree-mode)
             ("v" . undo-tree-visualize)
             )

  (bind-keys :map spacemacs-cmds
             :prefix-map modes-prefix-key-map
             :prefix "o m"
             :prefix-docstring "Commands dealing with modes and states. Inherits from `mode-ring-prefix-key-map'"
             ("e" . evil-evilified-state)
             ("n" . evil-normal-state)
             )
  (set-keymap-parent modes-prefix-key-map mode-ring-prefix-key-map)

  (bind-keys :map search-map
             ;; M-s map
             ("s" . dired-mark-files-regexp)
             )

  (my/def-variable-toggle undo-tree-visualizer-diff)
  (my/def-variable-toggle undo-tree-visualizer-timestamps)
  (my/def-variable-toggle undo-tree-auto-save-history)
  (bind-keys :map spacemacs-cmds
             :prefix-map my/undo-prefix-map
             :prefix "C-u"
             :prefix-docstring "Commands related to undo."

             ("s" . undo-tree-switch-branch)
             ("t d" . my/toggle-undo-tree-visualizer-diff)
             ("t h" . my/toggle-undo-tree-auto-save-history)
             ("t t" . global-undo-tree-mode)
             ("t w" . my/toggle-undo-tree-visualizer-timestamps)
             ("v" . undo-tree-visualize)
             )

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────╮
  ;; │ user-cmds-map │
  ;; ╰───────────────╯

  (bind-keys :map global-map
             ;; S-SPC
             :prefix-map user-cmds-map
             :menu-name "user cmds"
             :prefix "S-SPC"
             :prefix-docstring "User commands."
             ("S-SPC"   . my/insert-space)
             ("SPC"     . my/insert-space-after)    ;; won't bind
             ("C-S-SPC" . my/insert-space-after)
             ("S-TAB"   . my/switch-to-most-recent-buffer)
             ("B"       . my/evil-insert-at-WORD-beginning)
             ("F"       . my/evil-append-at-WORD-end)
             )
  (define-key special-mode-map (kbd "S-SPC") 'user-cmds-map)

  (bind-keys :map user-cmds-map
             ;; S-SPC d
             :prefix-map my/delete-prefix-map
             :menu-name "delete"
             :prefix "d"
             :prefix-docstring "Deletion commands."
             ("'"   . my/delete-inside-double-quotes)
             ("a '" . my/delete-double-quotes)
             )

  (bind-keys :map user-cmds-map
             :prefix-map help-download-prefix-map
             :prefix "h w"
             :prefix-docstring "Commands to download additional documentation."
             ("r" . github-download-README)
             ("w" . github-clone-wiki)
             )

  (bind-keys :map user-cmds-map
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

  (bind-keys :map user-cmds-map
             ;; S-SPC k
             :prefix-map my/keymaps-prefix-map
             :menu-name "keys/keymaps"
             :prefix "k"
             :prefix-docstring "Commands dealing with keymaps."
             ("a" . my/which-key-show-keymap-at-point)
             ("e" . edmacro-insert-key)
             ("f" . my/get-binding)
             ("i" . my/lookup-key-interactive)
             ("m" . which-key-show-minor-mode-keymap)
             ("p" . my/prettyprint-keymap)
             ("r" . my/replace-ints-with-char)
             ("s" . my/which-key-show-current-state-map)
             ("u" . my/parent-keymap-at-point)
             ("w" . my/which-key-show)
             ("K" . which-key-show-top-level)
             )

  (bind-keys :map my/keymaps-prefix-map
             ;; S-SPC k d
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

  (bind-keys :map user-cmds-map
             ;; S-SPC j
             :prefix-map my/jump-prefix-map
             :menu-name "jump"
             :prefix "j"
             :prefix-docstring "Jump commands."
             ("l ." . my/find-file-or-browse-url-at-point)
             ("l a" . link-hint-open-all-links)
             ("l f" . link-hint-open-link)
             ("l F" . link-hint-open-multiple-links)
             ("."   . my/find-file-or-browse-url-at-point)
             )

  (bind-keys :map user-cmds-map
             :prefix-map match-lines-map
             :menu-name "match lines"
             :prefix "l"
             :prefix-docstring "Commands matching lines against a pattern."
             ("m" . keep-lines)
             ("n" . flush-lines)
             ("c" . how-many)
             ("g" . sort-group-lines)
             ("h" . highlight-lines-matching-regexp)
             ("y" . my/copy-matching-lines)
             ("Y" . my/copy-non-matching-lines)
             )

  (bind-keys :map match-lines-map
             :prefix-map delete-lines-map
             :menu-name "delete lines"
             :prefix "d"
             :prefix-docstring "Commands deleting lines against a pattern."
             ("u" . my/delete-duplicate-lines-nonblank)
             ("U" . delete-duplicate-lines)
             )

  (bind-keys :map user-cmds-map
             ;; S-SPC m
             :prefix-map my/move-prefix-map
             :menu-name "move"
             :prefix "m"
             :prefix-docstring "Move/copy code."
             ("m"  . avy-move-line)
             ("r"  . avy-move-region)
             )

  (bind-keys :map my/move-prefix-map
             ;; S-SPC mc
             :prefix-map my/copy-map
             :menu-name "copy"
             :prefix "c"
             :prefix-docstring "copy code."
             ("c" . avy-copy-line)
             ("r" . avy-copy-region)
             )

  (bind-keys :map match-lines-map
             :prefix-map occur-map
             :menu-name "occur"
             :prefix "o"
             :prefix-docstring "Show matching lines in occur buffer."
             ("m" . my/pcre-multi-occur)
             ("o" . my/pcre-occur)
             ("l" . my/pcre-loccur)
             )

  (bind-keys :map user-cmds-map
             ;; S-SPC p
             :prefix-map my/projects-prefix-map
             :menu-name "projects"
             :prefix "p"
             :prefix-docstring "Commands opening projects."
             ("c" . my/cvim-source-dir)
             ("d" . my/dactyl-source-dir)
             ("s" . my/scripts-dir)
             ("v" . my/vimperator-source-dir-orig)
             )

  (my/def-variable-toggle which-key-show-operator-state-maps)
  (my/def-variable-toggle show-trailing-whitespace)
  (my/def-variable-toggle indent-tabs-mode)
  (my/def-variable-toggle fit-frame-inhibit-fitting-flag)
  (bind-keys :map user-cmds-map
             ;; S-SPC t
             :prefix-map my/toggles-prefix-map
             :menu-name "toggles"
             :prefix "t"
             :prefix-docstring "Commands toggling options."
             ("l"     . lispyville-mode)
             ("m"     . evil-matchit-mode)
             ("M"     . global-evil-matchit-mode)
             ("O"     . my/toggle-which-key-show-operator-state-maps)
             ("s"     . evil-surround-mode)
             ("T"     . my/toggle-indent-tabs-mode)
             ("w"     . my/toggle-show-trailing-whitespace)
             ("3"     . my/toggle-evil-mc-mode)
             ("8"     . evil-visualstar-mode)
             ("C-n"   . my/toggle-evil-mc-mode-and-cursor)
             ("RET w" . subword-mode)
             ("'"     . evil-visual-mark-mode)
             ("`"     . evil-vimish-fold-mode)
             ("~"     . variable-pitch-mode)
             ("="     . my/toggle-indent-function)
             ("|"     . fci-mode)
             ("?"     . helm-descbinds-mode)  ;; reactivated by helm - TODO: investigate
             (":"     . nameless-mode)
             ("["     . diff-hl-flydiff-mode)
             ("<f4>"  . my/cycle-shell-command-switch)
             ("C-l"   . toggle-truncate-lines)
             ("C-s"   . my/undo-auto-save-make-local-and-toggle)
             ("C-/"   . evil-search-highlight-persist)
             ("C-'"   . my/toggle-fit-frame-inhibit-fitting-flag)
             )

  (bind-keys :map user-cmds-map
             :prefix-map follow-prefix-map
             :prefix "w f"
             :prefix-docstring "Commands dealing with follow-mode."
             ("f"   . follow-delete-other-windows-and-split)
             ("SPC" . follow-mode)
             )

  (bind-keys :map user-cmds-map
             ;; S-SPC x
             :prefix-map my/structured-text-prefix-map
             :menu-name "structured text"
             :prefix "x"
             :prefix-docstring "Commands dealing with structured text."
             ("sn" . sort-numeric-fields)
             )

  (bind-keys :map user-cmds-map
             ;; S-SPC x r
             :prefix-map my/rectangle-prefix-map
             :menu-name "rectangle"
             :prefix "x r"
             :prefix-docstring "Commands operating on rectangles."
             ("c" . clear-rectangle)
             ("n" . rectangle-number-lines-interactive)
             ("N" . rectangle-number-lines)
             ("o" . open-rectangle)
             ("t" . string-rectangle-history)
             )

  (bind-keys :map user-cmds-map
             ;; S-SPC y
             :prefix-map my/yank-map
             :menu-name "yank"
             :prefix "y"
             :prefix-docstring "Commands to copy text to clipboard and kill-ring"
             ("d"   . my/yank-directory)
             ("g g" . my/git-yank-commit-messages)
             ("i"   . my/yank-buffer-initial-integers)
             ("I"   . my/yank-buffer-first-integers)
             ("n"   . my/yank-filename)
             ("l a" . my/link-hint-copy-all-links)
             ("l f" . link-hint-copy-link)
             ("l F" . link-hint-copy-multiple-links)
             ("l ." . link-hint-copy-link-at-point)
             ("p"   . my/yank-path)
             (". l" . link-hint-copy-link-at-point)
             (". s" . my/yank-sexp-at-point)
             (". w" . my/yank-word-at-point)
             ("\""  . my/avy-yank-inner-quotes)
             )

  (bind-keys :map user-cmds-map
             ;; S-SPC -
             :prefix-map my/diff-prefix-map
             :menu-name "diff"
             :prefix "-"
             :prefix-docstring "Commands for diffing files or text."
             ("r r" . ediff-regions-linewise)
             ("r w" . ediff-regions-wordwise)
             )

  (bind-keys :map user-cmds-map
             ;; S-SPC !
             :prefix-map my/external-apps-prefix
             :menu-name "external apps"
             :prefix "!"
             :prefix-docstring "Commands dealing with external applications."
             ("f b" . helm-firefox-bookmarks)
             )

  (bind-keys :map user-cmds-map
             ;; S-SPC &
             :prefix-map my/diff-prefix-map
             :menu-name "diff"
             :prefix "&"
             :prefix-docstring "Diff commands."
             ("f"   . diff-buffer-with-file)
             ("b"   . ediff-buffers)
             ("3"   . ediff-buffers3)
             )
  (bind-keys :map my/diff-prefix-map
             ;; S-SPC & m
             :prefix-map my/merge-prefix-map
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

  (bind-keys :map user-cmds-map
             ;; S-SPC |
             :prefix-map my/column-prefix-map
             :menu-name "column"
             :prefix "|"
             :prefix-docstring "Column commands."
             ("e" . my/extend-to-column)
             ("|" . my/add-column-marker)
             )

  (bind-keys :map user-cmds-map
             ;; S-SPC <f3>
             :prefix-map my/snippets-prefix-map
             :prefix "<f3>"
             :prefix-docstring "Snippets and templates commands"
             ("a"      . aya-create-one-line)
             ("c"      . aya-create)
             ("n"      . my/aya-create-snippet-with-newline)
             ("r"      . my/yas-reload-all-no-jit)
             ("s"      . aya-yank-snippet)
             ("v"      . yas-visit-snippet-file)
             ("x"      . aya-expand)
             ("y"      . yas-new-snippet)
             ("<tab>"  . helm-yas-complete)
             ("<f3>"   . aya-open-line)
             ("M-<f3>" . aya-open-line)
             )
  (global-set-key (kbd "M-<f3>") 'my/snippets-prefix-map)

  (which-key-add-key-based-replacements
    "S-SPC K"    "keymaps"
    "S-SPC X"    "structured text"
    "S-SPC y g"  "git"
    "S-SPC y ."  "at-point"
    "S-SPC !"    "external apps"
    "S-SPC &"    "diff"
    "S-SPC & m"  "merge"
    "S-SPC |"    "column"
    )

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────────────────────╮
  ;; │ which-key Configuration │
  ;; ╰─────────────────────────╯

  ;; ;; enable which-key for motions - breaks t/f
  ;; (setq which-key-show-operator-state-maps t)

  (bind-keys
   :map which-key-C-h-map
   ("C-h" . which-key-abort))

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
    "SPC f e s"    "spacemacs search"
    "SPC f e SPC"  "spacemacs config"
    "SPC h /"      "find-function"
    "SPC o f"      "flycheck"
    "SPC s /"      "PCRE Regex"
    "SPC t RET"    "more toggles"
    "SPC K"        "keys/keymaps"
    "SPC X"        "structured text"
    )

  ;; TODO: update to use 'which-key-replacement-alist
  (dolist (pair '(("return"  . "RET")
                  ("delete"  . "Delete")
                  ("backtab" . "S-TAB")
                  ("escape"  . "ESC")
                  ))
    (add-to-list 'which-key-key-replacement-alist pair))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────╮
  ;; │ C-c C-v │
  ;; ╰─────────╯

  (global-set-key (kbd "C-c C-v") 'browse-buffer-file-firefox)
  (eval-after-load "markdown-mode"
    '(define-key markdown-mode-map (kbd "C-c C-v") 'my/markdown-view))
  (eval-after-load "web-mode"
    '(define-key web-mode-map (kbd "C-c C-v") 'browse-buffer-file-with-external-application))
  (eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-v") 'browse-buffer-file-firefox))
  (define-key text-mode-map (kbd "C-c C-v") 'browse-buffer-file-firefox)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭────────────╮
  ;; │ Minibuffer │
  ;; ╰────────────╯

  (defmacro my/minibuffer-yank-thing-command (type)
    "Yank thing at point of type TYPE into the minibuffer"
    `(lambda ()
       (interactive)
       (with-selected-window (minibuffer-selected-window)
         (let ((thing (thing-at-point ,type)))
           (with-selected-window (active-minibuffer-window)
             (insert thing))))))

  (define-key minibuffer-local-map "\C-w" (my/minibuffer-yank-thing-command 'symbol))

  ;; ╭────────────────────╮
  ;; │ Command Docstrings │
  ;; ╰────────────────────╯

  (defmacro my/set-docstring (fn docstr)
    (eval `(put ',fn 'function-documentation ,docstr)))

  (defun my/set-docstrings (&rest pairs)
    "Set a sequence of docstring functions

\(fn [FN1 DOCSTR1] ...)"
    (eval `(loop for (fn docstr) on ',pairs by 'cddr do
                 (eval `(put ',fn 'function-documentation ,docstr)))))

  (my/set-docstrings
   'evil-search-highlight-persist-remove-all    "Remove all `evil-search' highlighting"
   )

  ;; ╭────────╮
  ;; │ elnode │
  ;; ╰────────╯

  (setq elnode-webserver-docroot
        (expand-file-name "private/public_html" user-emacs-directory))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────╮
  ;; │               │
  ;; │  MINOR MODES  │
  ;; │               │
  ;; ╰───────────────╯

  ;; ╭──────────────╮
  ;; │ evil-mc-mode │
  ;; ╰──────────────╯

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
    (if (and (boundp 'evil-mc-mode) evil-mc-mode)
        (progn (evil-mc-undo-all-cursors)
               (evil-mc-mode 0)
               (message "evil-mc-mode disabled"))
      (evil-mc-mode 1)
      (evil-mc-make-and-goto-next-match)
      (message "evil-mc-mode enabled")))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────────────╮
  ;; │ evil-vimish-fold │
  ;; ╰──────────────────╯

  (defun my/evil-vimish-fold-mode-hook ()
    (interactive)
    (evil-define-key 'normal evil-vimish-fold-mode-map "z " 'vimish-fold-avy)
    (evil-define-key 'visual evil-vimish-fold-mode-map "z " 'vimish-fold-avy))
  (eval-after-load 'evil-vimish-fold
    (add-hook 'evil-vimish-fold-mode-hook #'my/evil-vimish-fold-mode-hook))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────╮
  ;; │ Flycheck │
  ;; ╰──────────╯

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

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────╮
  ;; │ MozREPL │
  ;; ╰─────────╯
  (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

  (defun my/moz-minor-mode-init ()
    (interactive)
    (moz-minor-mode 1))

  (add-hook 'js2-mode-hook 'my/moz-minor-mode-init)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────╮
  ;; │ Mozc │
  ;; ╰──────╯
  ;; Note: requires mozc_emacs_helper program -- debian pkg emacs-mozc-bin
  (require 'mozc)
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'overlay)
  ;; If Emacs doesn't recognize IM, make s-SPC toggle mozc-mode
  (global-set-key (kbd "s-SPC") 'mozc-mode)
  (setcar (cdr (assoc 'mozc-mode minor-mode-alist))
          "日")

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────╮
  ;; │ Nameless │
  ;; ╰──────────╯
  (use-package nameless
    :config
    (setq nameless-private-prefix t)
    )

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────────────╮
  ;; │ Whitespace-Mode │
  ;; ╰─────────────────╯
  (setq whitespace-style-heavy
        '(face tabs spaces trailing lines-tail space-before-tab newline indentation empty
               space-after-tab space-mark tab-mark newline-mark))
  (setq whitespace-style-light
        '(face tabs spaces trailing space-before-tab indentation space-after-tab space-mark tab-mark))
  (setq-default whitespace-style whitespace-style-light)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────╮
  ;; │               │
  ;; │  MAJOR MODES  │
  ;; │               │
  ;; ╰───────────────╯

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ═════╭──────────────────────────╮═════
  ;; ═════│ terminal and shell modes │═════
  ;; ═════╰──────────────────────────╯═════

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

  ;; ╭───────────╮
  ;; │ ansi-term │
  ;; ╰───────────╯

  (evil-set-initial-state 'term-mode 'emacs)
  (eval-after-load "term"
    '(progn
       (define-key term-raw-map (kbd "C-p")      'term-send-up)
       (define-key term-raw-map (kbd "C-n")      'term-send-down)
       (define-key term-raw-map (kbd "C-c C-y")  'term-paste)
       ))

  ;; ╭────────────╮
  ;; │ shell-mode │
  ;; ╰────────────╯

  (eval-after-load 'shell
    '(progn
      (evil-define-key 'insert shell-mode-map
        (kbd "C-d")    'comint-send-eof
        (kbd "C-u")    'comint-kill-input)
      (add-hook 'shell-mode-hook 'my/shell-mode-init)))

  (spacemacs/set-leader-keys-for-major-mode 'shell-mode
      "q"           'my/comint-quit-and-close
      "r"           'shell-resync-dirs
      "<backspace>" 'my/bury-buffer-and-delete-window)

  (defun my/shell-mode-init ()
    (interactive)
    (define-key shell-mode-map
      (kbd "M-RET")  nil))

  (defun my/comint-quit-and-close ()
    (interactive)
    (comint-send-eof)
    (sit-for 1)
    (kill-buffer-and-window))

  ;; ╭───────────╮
  ;; │ term-mode │
  ;; ╰───────────╯

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

  ;; ╭─────────────────╮
  ;; │ multi-term-mode │
  ;; ╰─────────────────╯

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────────────────╮
  ;; │ bookmark-bmenu-mode │
  ;; ╰─────────────────────╯

  ;; eval-after-load "bookmark.el" isn't working? (nor with 'bookmark)
  (defun bookmark-bmenu-mode-init ()
    (evilified-state-evilify-map bookmark-bmenu-mode-map
      :mode bookmark-bmenu-mode))
  (add-hook 'bookmark-bmenu-mode-hook 'bookmark-bmenu-mode-init)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────╮
  ;; │ Calc │
  ;; ╰──────╯
  ;; calc: bypass calc-dispatch and bind C-x * directly to calc-dispatch-map
  ;;       (allow introspection of keybindings)
  (require 'calc)
  (global-set-key (kbd "C-x *") calc-dispatch-map)

  ;; ╭────────────╮
  ;; │ calculator │
  ;; ╰────────────╯

  (evil-set-initial-state 'calculator-mode 'emacs)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────╮
  ;; │               │
  ;; │  dactyl-mode  │
  ;; │               │
  ;; ╰───────────────╯
  (defun my/dactyl-init ()
    (interactive)
    (setq-local imenu-generic-expression `((nil "^\" | .* |$" 0)))
    (setq-local tab-width 4)
    (setq-local comment-end "\"")
    (modify-syntax-entry ?_ "w")
    )

  (add-hook 'dactyl-mode-hook 'my/dactyl-init)

  (my/define-named-variable-cycle my/dactyl-cycle-fill-prefix
    fill-prefix (nil "\\" "\" " "    \\ "))

  (spacemacs/set-leader-keys-for-major-mode 'dactyl-mode
      ","     'my/dactyl-cycle-fill-prefix
      "'"     (my/make-insertion-around-point "\" " " \"")
      "/"     (my/make-insertion-around-point "/* " " */")
      "a a"   'my/dactyl-align-defs-repeat
      "a m"   'my/dactyl-align-defs-multiline
      "g b"   'my/dactyl-goto-binding
      "g c"   'my/dactyl-goto-command
      "g f"   'my/dactyl-goto-function
      "g g"   'my/dactyl-goto-group
      "g h"   'my/dactyl-goto-heading
      "g ;"   'my/dactyl-goto-hint-mode
      "m d"   'my/dactyl-make-defn-multiline
      "m 1"   'my/dactyl-make-defn-multiline-1
      "m a"   'my/dactyl-make-defn-multiline-align-1
      "o c"   'my/dactyl-command-occur
      "o f"   'my/dactyl-function-occur
      "o g"   'my/dactyl-show-groups
      "o s"   'my/dactyl-show-styles
      "o ;"   'my/dactyl-hint-mode-occur
      "o :"   'my/dactyl-hint-mode-occur-by-definition
      "o m"   'my/dactyl-mapping-occur
      "o \\"  'my/dactyl-show-sections
      "o /"   'my/dactyl-show-slash-star-sections
      "o '"   'my/dactyl-show-quote-sections
      "t"     'my/dactyl-toggle-text-mode
      "j"     'my/newline-indent-insert-fill-prefix
      "J"     'my/collapse-single-line-function
      ". c"   'my/dactyl-command-occur-at-point
      ". f"   'my/dactyl-function-occur-at-point
      ". m"   'my/dactyl-mapping-occur-at-point
      ". ;"   'my/dactyl-hint-mode-occur-at-point
      "SPC"   'helm-imenu
      "\\"    'my/dactyl-move-continuation-to-column-1
      )

  (which-key-add-major-mode-key-based-replacements 'dactyl-mode
      ", a"       "align"
      ", m"       "multiline"
      ", o"       "occur"
      "M-RET o"   "occur"
      ", ."       "at-point"
      "M-RET ."   "at-point"
      ", '"       "\" ⌶ \""
      "M-RET '"   "\" ⌶ \""
      ", /"       "/* ⌶ */"
      "M-RET /"   "/* ⌶ */"
      )

  (define-derived-mode
      dactyl-text-mode text-mode "Dactyl Text"
      "Text mode variant for editing .pentadactylrc. Use when dactyl-mode is too slow."
      (setq imenu-generic-expression `((nil "^\" | .* |$" 0)))
      (setq tab-width 4)
      (modify-syntax-entry ?_ "w")
      (setq-local comment-start "\"")
      (setq-local comment-end "\"")
      (yas-minor-mode 1))

  (spacemacs/set-leader-keys-for-major-mode 'dactyl-text-mode
      ","     'my/dactyl-cycle-fill-prefix
      "'"     (my/make-insertion-around-point "\" " " \"")
      "/"     (my/make-insertion-around-point "/* " " */")
      "a a"   'my/dactyl-align-defs-repeat
      "a m"   'my/dactyl-align-defs-multiline
      "g b"   'my/dactyl-goto-binding
      "g c"   'my/dactyl-goto-command
      "g f"   'my/dactyl-goto-function
      "g g"   'my/dactyl-goto-group
      "g h"   'my/dactyl-goto-heading
      "g ;"   'my/dactyl-goto-hint-mode
      "j"     'my/newline-indent-insert-fill-prefix
      "J"     'my/collapse-single-line-function
      "m d"   'my/dactyl-make-defn-multiline
      "m 1"   'my/dactyl-make-defn-multiline-1
      "m a"   'my/dactyl-make-defn-multiline-align-1
      "o c"   'my/dactyl-command-occur
      "o f"   'my/dactyl-function-occur
      "o g"   'my/dactyl-show-groups
      "o s"   'my/dactyl-show-styles
      "o m"   'my/dactyl-mapping-occur
      "o ;"   'my/dactyl-hint-mode-occur
      "o :"   'my/dactyl-hint-mode-occur-by-definition
      "o \\"  'my/dactyl-show-sections
      "o /"   'my/dactyl-show-slash-star-sections
      "o '"   'my/dactyl-show-quote-sections
      "t"     'my/dactyl-toggle-text-mode
      ". c"   'my/dactyl-command-occur-at-point
      ". f"   'my/dactyl-function-occur-at-point
      ". m"   'my/dactyl-mapping-occur-at-point
      ". ;"   'my/dactyl-hint-mode-occur-at-point
      "SPC"   'helm-imenu
      "\\"    'my/dactyl-move-continuation-to-column-1
      )

  (which-key-add-major-mode-key-based-replacements 'dactyl-text-mode
      ", a"       "align"
      ", m"       "multiline"
      ", o"       "occur"
      "M-RET o"   "occur"
      ", ."       "at-point"
      "M-RET ."   "at-point"
      ", '"       "\" ⌶ \""
      "M-RET '"   "\" ⌶ \""
      ", /"       "/* ⌶ */"
      "M-RET /"   "/* ⌶ */"
      )

  (defun my/dactyl-source-dir-orig()
    (interactive)
    (dired "/home/troy/source/git-repos/dactyl/common/")
    (dired-insert-subdir "/home/troy/source/git-repos/dactyl/common/components/")
    (dired-insert-subdir "/home/troy/source/git-repos/dactyl/common/content/")
    (dired-insert-subdir "/home/troy/source/git-repos/dactyl/common/modules/")
    (dired-insert-subdir "/home/troy/source/git-repos/dactyl/pentadactyl/content/")
    (dired-hide-details-mode 1)
    (setq-local my/dired-reuse-buffer nil))

  (defun my/dactyl-source-dir()
    (interactive)
    (dired "/home/troy/repos/dactyl/common/")
    (dired-insert-subdir "/home/troy/repos/dactyl/common/components/")
    (dired-insert-subdir "/home/troy/repos/dactyl/common/content/")
    (dired-insert-subdir "/home/troy/repos/dactyl/common/modules/")
    (dired-insert-subdir "/home/troy/repos/dactyl/pentadactyl/content/")
    (dired-hide-details-mode 1)
    (setq-local my/dired-reuse-buffer nil))

  (defun my/vimperator-source-dir-orig()
    (interactive)
    (dired "/home/troy/source/git-repos/dactyl/common/")
    (dired-insert-subdir "/home/troy/source/git-repos/vimperator-labs/common/components/")
    (dired-insert-subdir "/home/troy/source/git-repos/vimperator-labs/common/content/")
    (dired-insert-subdir "/home/troy/source/git-repos/vimperator-labs/common/modules/")
    (dired-insert-subdir "/home/troy/source/git-repos/vimperator-labs/vimperator/content/")
    (dired-hide-details-mode 1)
    (setq-local my/dired-reuse-buffer nil))

  (defun my/dactyl-make-defn-multiline (&optional arg)
    (interactive "p")
    (evil-ex-substitute
     (region-beginning)
     (region-end)
     (evil-ex-make-substitute-pattern
      " *\\( -desc\\| -nargs\\| -count\\| -bang\\| -modes\\| -complete\\| -ex\\| -js\\| -b\\b\\)"
      '(?g))
     "\n\\\\   \\1")
     (flush-lines "^ *\\\\? *$" (region-beginning) (region-end)))

  (defun my/dactyl-make-defn-multiline-1 (&optional arg)
    (interactive "p")
    (evil-ex-substitute
     (region-beginning)
     (region-end)
     (evil-ex-make-substitute-pattern
      " *\\( -nargs\\| -count\\| -bang\\| -modes\\| -complete\\| -ex\\| -js\\| -b\\b\\)"
      '(?g))
     "\n\\\\   \\1"))

  (defun my/dactyl-align-defs-multiline ()
    (interactive)
    (my/quick-pcre-align (region-beginning) (region-end) " -desc| :.| -[^b]"))

  (defun my/dactyl-align-defs-repeat ()
    (interactive)
    (my/pcre-align (region-beginning) (region-end) " :.| -" -1 1 t))

  (defun my/dactyl-make-defn-multiline-align-1 (&optional arg)
    (interactive "p")
    (my/dactyl-make-defn-multiline-1 arg)
    (evil-visual-restore)
    (my/dactyl-align-defs-multiline))

  (defun my/dactyl-toggle-text-mode ()
    (interactive)
    (if (eq major-mode 'dactyl-mode) (dactyl-text-mode) (dactyl-mode)))

  (defun my/dactyl-move-continuation-to-column-1 (&optional arg)
    (interactive "p")
    (evil-ex-substitute
     (region-beginning) (region-end)
     (evil-ex-make-substitute-pattern "^    \\\\ " nil)
     "\\\\    "))

  (defun my/dactyl-make-xpi ()
    (interactive)
    (async-shell-command "cd ~/repos/dactyl/pentadactyl && make xpi"))

  ;; ╭────────────────────╮
  ;; │ my/dactyl/goto-... │
  ;; ╰────────────────────╯

  (defun my/dactyl-goto-group (group)
    "Jump to specified group"
    (interactive "sGroup: ")
    (let ((start-pos (point)))
      (unless (string-empty-p group) (beginning-of-buffer))
      (if (re-search-forward (pcre-to-elisp (concat "^ *group " group)) nil t)
          (progn
            (recenter 4)
            (beginning-of-line))
        (goto-char start-pos))))

  (defun my/dactyl-goto-function (fn)
    "Jump to specified function"
    (interactive
     (list
      (let* ((fap1 (thing-at-point 'symbol))
             (fap  (if fap1 (replace-regexp-in-string "^\\(.*=> *\\)" "" fap1 nil nil 1) "")))
        (s-trim (read-string (format "Function [default %s]: " fap) nil nil fap)))))
    (let ((start-pos (point)))
      (unless (string-empty-p fn) (beginning-of-buffer))
      (if (re-search-forward (pcre-to-elisp (concat "^ *function " fn) "i") nil t)
          (progn
            (recenter 4)
            (beginning-of-line))
        (goto-char start-pos))))

  (defun my/dactyl-goto-command (cmd)
    "Jump to specified command"
    (interactive
     (list
      (let ((cap (or (s-chop-suffixes '("<Space>" "<SPACE>") (thing-at-point 'symbol)) "")))
        (s-trim (read-string (format "Command [default %s]: " cap) nil nil cap)))))
    (let ((start-pos (point)))
      (unless (string-empty-p cmd) (beginning-of-buffer))
      (if (re-search-forward (pcre-to-elisp (concat "^ *command! " cmd) "i") nil t)
          (progn
            (recenter 4)
            (beginning-of-line))
        (goto-char start-pos))))

  (defun my/dactyl-goto-binding (b)
    "Jump to specified binding"
    (interactive "sKeys: ")
    (let ((start-pos (point)))
      (unless (string-empty-p b) (beginning-of-buffer))
      (if (re-search-forward (pcre-to-elisp (concat "^ *map (-modes [-,a-z]+ +)?" b) "") nil t)
          (progn
            (recenter 4)
            (beginning-of-line))
        (goto-char start-pos))))

  (defun my/dactyl-goto-hint-mode (hint)
    "Jump to specified hint mode"
    (interactive
     (list
      (let ((cap (or (s-chop-suffix "<Space>" (thing-at-point 'char)) "")))
        (s-trim (read-string (format "Hint mode [default %s]: " cap) nil nil cap)))))
    (let ((start-pos (point)))
      (unless (string-empty-p hint) (beginning-of-buffer))
      (if (re-search-forward (pcre-to-elisp (concat "^( *js)? *hints.addMode\\(['\"]\\\\?" hint "['\"]")) nil t)
          (progn
            (recenter 4)
            (beginning-of-line))
        (goto-char start-pos))))

  (defun my/dactyl-goto-heading (h)
    "Jump to specified boxed heading"
    (interactive "sHeading: ")
    (let ((start-pos (point)))
      (unless (string-empty-p h) (beginning-of-buffer))
      (if (re-search-forward (pcre-to-elisp (concat "^ *\" *[|│] +" h ".* [|│]") "i") nil t)
          (progn
            (recenter 4)
            (beginning-of-line))
        (goto-char start-pos))))

  ;; ╭─────────────────────╮
  ;; │ my/dactyl-...-occur │
  ;; ╰─────────────────────╯

  (defun my/dactyl-mapping-occur (prefix)
    "Open an `occur' buffer with statements mapping keys matching PREFIX."
    (interactive (list (read-string "Mappings for key prefix: " nil t nil)))
    (let ((pcre (concat "map!? +(-\\w+ +)*" prefix)))
      (occur (pcre-to-elisp pcre)))
    (switch-to-buffer-other-window "*Occur*"))

  (defun my/dactyl-hint-mode-occur (pattern)
    "Open an `occur' buffer with hint mode definitions matching PATTERN."
    (interactive (list (read-string "Hint modes matching: " nil t nil)))
    (let ((pcre (concat "^( *js)? *hints.addMode\\(['\"]\\\\?" (if (string-empty-p pattern) ".*" pattern) "['\"]")))
      (occur (pcre-to-elisp pcre)))
    (switch-to-buffer-other-window "*Occur*"))

  (defun my/dactyl-hint-mode-occur-by-definition (pattern)
    "Open an `occur' buffer with hint modes matching PATTERN."
    (interactive (list (read-string "Hint modes matching: " nil t nil)))
    (let ((pcre (concat "^( *js)? *hints.addMode\\(.*" pattern ".*\\)")))
      (occur (pcre-to-elisp pcre)))
    (switch-to-buffer-other-window "*Occur*"))

  (defun my/dactyl-command-occur (pattern &optional case-sensitive)
    "Open an `occur' buffer with statements defining commands matching PATTERN."
    (interactive (list (read-string "Commands matching PATTERN: " nil t nil)
                       current-prefix-arg))
    (let ((pcre (concat "command!? +(-\\w+ +)*\w*" pattern "\w*"))
          (flags (if case-sensitive "" "i")))
      (occur (pcre-to-elisp pcre flags)))
    (switch-to-buffer-other-window "*Occur*"))

  (defun my/dactyl-function-occur (pattern &optional case-sensitive)
    "Open an `occur' buffer with statements defining functions matching PATTERN."
    (interactive (list (read-string "Functions matching PATTERN: " nil t nil))
                 current-prefix-arg)
    (let ((pcre (concat "function +\w*" pattern "\w* *\(\)"
                        "|" "\w*" pattern "\w* *= *function *\(\)"))
          (flags (if case-sensitive "" "i")))
      (occur (pcre-to-elisp pcre flags)))
    (switch-to-buffer-other-window "*Occur*"))

  ;; ╭────────────────────────────╮
  ;; │ my/dactyl-*-occur-at-point │
  ;; ╰────────────────────────────╯
  (defun my/dactyl-mapping-occur-at-point (prefix)
    "Open an `occur' buffer with statements mapping keys matching PREFIX."
    (interactive
     (list
      (read-string "Mappings for key prefix: "
                   (regexp-quote (apply 'buffer-substring (-take 2 (evil-inner-WORD))))
                   t)))
    (let ((pcre (concat "map!? +(-\\w+ +)*" prefix)))
      (occur (pcre-to-elisp pcre)))
    (switch-to-buffer-other-window "*Occur*"))

  (defun my/dactyl-hint-mode-occur-at-point (pattern)
    "Open an `occur' buffer with statements defining hint mode matching PATTERN."
    (interactive
     (list
      (read-string "Hint mode: "
                   (regexp-quote (apply 'buffer-substring (-take 2 (evil-inner-WORD))))
                   t)))
    (let ((pcre (concat "^( *js)? *hints.addMode\\(['\"]\\\\?" (if (string-empty-p pattern) ".*" pattern) "['\"]")))
      (occur (pcre-to-elisp pcre)))
    (switch-to-buffer-other-window "*Occur*"))

  (defun my/dactyl-command-occur-at-point (pattern &optional case-sensitive)
    "Open an `occur' buffer with statements defining commands matching PATTERN."
    (interactive (list (read-string "Commands matching PATTERN: " (regexp-quote (tap-thing-at-point 'word)) t)
                       current-prefix-arg))
    (let ((pcre (concat "command!? +(-\\w+ +)*\w*" pattern "\w*")))
      (occur (pcre-to-elisp pcre "i")))
    (switch-to-buffer-other-window "*Occur*"))

  (defun my/dactyl-function-occur-at-point (pattern &optional case-sensitive)
    "Open an `occur' buffer with statements defining functions matching PATTERN."
    (interactive (list (read-string "Functions matching PATTERN: " (regexp-quote (tap-thing-at-point 'word)) t))
                 current-prefix-arg)
    (let ((pcre (concat "function +\w*" pattern "\w* *\(\)"
                        "|" "\w*" pattern "\w* *= *function *\(\)")))
      (occur (pcre-to-elisp pcre "i")))
    (switch-to-buffer-other-window "*Occur*"))

  ;; ╭──────────────────╮
  ;; │ my/dactyl-show-* │
  ;; ╰──────────────────╯
  (defun my/dactyl-show-groups ()
    "Open an `occur' buffer with all group statements."
    (interactive)
    (occur (pcre-to-elisp "^group [a-zA-Z0-9_]+ -locations="))
    (switch-to-buffer-other-window "*Occur*"))

  (defun my/dactyl-show-slash-star-sections ()
    "Open an `occur' buffer with all section headings."
    (interactive)
    (occur (pcre-to-elisp "^ */\\* +[|│] .* [|│] +\\*/$"))
    (switch-to-buffer-other-window "*Occur*"))

  (defun my/dactyl-show-quote-sections ()
    "Open an `occur' buffer with all section headings."
    (interactive)
    (occur (pcre-to-elisp "^ *\" *[|│] .* [|│] *\"?$"))
    (switch-to-buffer-other-window "*Occur*"))

  (defun my/dactyl-show-sections ()
    "Open an `occur' buffer with all section headings."
    (interactive)
    (occur (pcre-to-elisp "^ *\" *[|│] .* [|│] *\"?$|^ */\\* +[|│] .* [|│] +\\*/$"))
    (switch-to-buffer-other-window "*Occur*"))

  (defun my/dactyl-show-styles ()
    "Open an `occur' buffer with all style commands."
    (interactive)
    (occur (pcre-to-elisp "^style -name="))
    (switch-to-buffer-other-window "*Occur*"))


  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────────────╮
  ;; │                  │
  ;; │  tridactyl-mode  │
  ;; │                  │
  ;; ╰──────────────────╯
  (add-to-load-path-if-exists "~/.tridactyl/emacs")
  (require 'tri)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────────╮
  ;; │ shell-script-mode │
  ;; ╰───────────────────╯
  (defun my/sh-alias-to-fn ()
    (interactive)
    (my/evil-substitute-region
     "^([# ]*)alias +([^ ]+)=[\"'](.*)[\"']$"
     "\\1\\2 () { \\3 }"))

  (defun my/sh-alias-and-comment-to-fn ()
    (interactive)
    (my/evil-substitute-region
     "^([# ]*)alias +([^ ]+)=[\"'](.*)[\"'] *(#.*)$"
     "\\4\n\\1\\2 () { \\3 }"))

  (spacemacs/set-leader-keys-for-major-mode 'sh-mode
      "ca"    'my/sh-alias-to-fn
      "cA"    'my/sh-alias-and-comment-to-fn
      )

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────╮
  ;; │ Dired │
  ;; ╰───────╯

  (make-local-variable 'my/dired-reuse-buffer)
  (setq-default my/dired-reuse-buffer t)
  (my/define-named-variable-toggle my/dired-toggle-reuse-buffer my/dired-reuse-buffer)

  (defun my/dired-find-file-maybe-alternate ()
    (interactive)
    (if my/dired-reuse-buffer
        (dired-find-alternate-file)
      (dired-find-file)))

  (use-package dired+
    :init
    (spacemacs|add-toggle diredp-wrap-around-flag
      :status diredp-wrap-around-flag
      :on  (setq diredp-wrap-around-flag t)
      :off (setq diredp-wrap-around-flag nil)
      )
    :config
    (setq diredp-hide-details-propagate-flag t)
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-wrap-around-flag nil)
    (diredp-toggle-find-file-reuse-dir -1)
    )

  (require 'dired-toggle-sudo)
  (eval-after-load 'tramp
    '(progn
      ;; Allow to use: /sudo:user@host:/path/to/file
      (add-to-list 'tramp-default-proxies-alist
       '(".*" "\\`.+\\'" "/ssh:%h:"))))

  (setq wdired-use-dired-vertical-movement 'sometimes)

  (defun my/dired-copy-file-path-as-kill ()
    (interactive)
    (dired-copy-filename-as-kill 0))
  (defun my/dired-copy-file-directory-as-kill ()
    (interactive)
    (let ((dir (file-name-directory (my/dired-copy-file-path-as-kill))))
      (if (eq last-command 'kill-region)
          (kill-append dir nil)
        (kill-new dir))
      (message "%s" dir)))

  ;; INITIAL STATE:
  ;; dired-mode  :   (customized) evilified state
  ;; wdired-mode :                normal state

  (defun my/dired-init ()
    (my/define-keys dired-mode-map
      (kbd "S-SPC")          nil
      (kbd "C-h")            nil
      (kbd "q")              'my/quit-window-kill
      (kbd "<return>")       'my/dired-find-file-maybe-alternate
      (kbd "<S-return>")     'dired-find-file
      (kbd "<mouse-3>")      'diredp-mouse-3-menu
      (kbd "<down-mouse-3>") 'diredp-mouse-3-menu
      ))
  (defun my/wdired-init ()
    (my/define-keys wdired-mode-map
      (kbd "C-c <escape>") 'wdired-abort-changes
      [f2]                 'wdired-finish-edit
      ))

  (eval-after-load "dired"
    `(progn
       (add-hook 'dired-mode-hook 'my/dired-init)
       (add-hook 'wdired-mode-hook 'my/wdired-init)

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
         (kbd "yd")     'my/dired-copy-file-directory-as-kill
         (kbd "yn")     'dired-copy-filename-as-kill
         (kbd "yp")     'my/dired-copy-file-path-as-kill
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
         [mouse-3]      'diredp-mouse-3-menu
         )

       ;; T is the prefix key for the tags commands
       (which-key-add-major-mode-key-based-replacements 'dired-mode
         "T"      "tags"
         "M-+"    "diredp-recursive-map"
         "y"      "copy--as-kill"
         ", t s"  "toggle/sort"
         )
       ;; set function definition of 'dired-mode-map (same as value)
       (fset 'dired-mode-map dired-mode-map)
       ;; major-mode leader-key
       (spacemacs/set-leader-keys-for-major-mode 'dired-mode
         "c"     'dired-mode-map
         "g"     'my/rgrep
         "h"     'dired-hide-subdir
         "k"     'diredp-kill-this-tree
         "K"     'dired-kill-tree
         "m"     'my/dired-move-files-here
         "o"     'my/git-browse-origin
         "p"     'my/dired-copy-files-here
         "s"     'dired-toggle-sudo
         "to"    'dired-omit-mode
         "tl"    'dired-hide-details-mode
         "tr"    'toggle-diredp-find-file-reuse-dir
         "tr"    'my/dired-toggle-reuse-buffer
         "tsf"   'dired-sort-menu-toggle-dirs-first
         "tw"    'spacemacs/toggle-diredp-wrap-around-flag
         "v"     'dired-view-file    ;; for discovery - can just use \v
         "x"     'my/dired-cut-or-copy-files
         "Y"     'diredp-relsymlink-this-file
         ","     'dired-subtree-toggle
         "."     'dired-subtree-cycle
         "RET"   'dired-open-xdg
         "<f2>"  'dired-do-rename
         )
       (spacemacs/declare-prefix-for-mode 'dired-mode "mt" "toggles")

       ;; wdired-mode
       (evil-set-initial-state 'wdired-mode 'normal)

       (spacemacs/set-leader-keys-for-major-mode 'wdired-mode
         "c"    'wdired-finish-edit
         "a"    'wdired-abort-changes
         "t"    'my/wdired-tidy-name-keep-square-brackets
         )

       )
    )

  (eval-after-load "dired-sort-menu" `(require 'dired-sort-menu+))

  (eval-after-load "ranger"
    `(progn
       (bind-keys
        :map ranger-mode-map
        ("C-h" . nil))
       ))

  (defvar my/dired-files-to-move-or-copy '()
    "Stores a list of files to be moved or copied by tsp-dired-*-files-here
 commands.")

  (defun my/dired-cut-or-copy-files (append)
    (interactive "P")
    (let ((files (dired-get-marked-files)))
      (if append
          (dolist (f files)
            (add-to-list my/dired-files-to-move-or-copy f))
        (setq my/dired-files-to-move-or-copy files))))

  (defun my/dired-copy-files-here
      (&optional overwrite
                 keep-time preserve-uid-gid preserve-permissions)
    (interactive "P")
    (dolist (f my/dired-files-to-move-or-copy)
      (let ((newpath (concat (dired-current-directory)
                             (file-name-nondirectory f))))
        (copy-file f newpath overwrite
                   keep-time preserve-uid-gid preserve-permissions))))

  (defun my/dired-move-files-here (&optional overwrite)
    (interactive "P")
    (dolist (f my/dired-files-to-move-or-copy)
      (let ((newpath (concat (dired-current-directory)
                             (file-name-nondirectory f))))
        (rename-file f newpath overwrite))))

  (defun my/dired-jump-and-kill ()
    (interactive)
    (let ((b (current-buffer)))
      (dired-jump)
      (kill-buffer b)))

  (defun my/diredp-create-files-union (union-buffer-name files)
    "Create a diredp union buffer from a list of files/directories."
    (diredp-dired-union union-buffer-name nil nil files))


  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────╮
  ;; │ Direx │
  ;; ╰───────╯

  (require 'direx)
  (push '(direx:direx-mode :position left :width 25 :dedicated t)
        popwin:special-display-config)
  (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)


  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────────╮
  ;; │ Edmacro-Mode │
  ;; ╰──────────────╯

  ;; FIXME: major-mode leader key not working
  (spacemacs/set-leader-keys-for-major-mode 'edmacro-mode
    "c"   'edmacro-finish-edit
    "a"   'spacemacs/kill-this-buffer
    )


  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭────────╮
  ;; │ Elfeed │
  ;; ╰────────╯

  ;; auto-evilification can't remap 'elfeed-search-fetch
  (eval-after-load "elfeed"
    `(progn
       (define-key elfeed-search-mode-map (kbd "C-x G") 'elfeed-search-fetch)
       ))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────────╮
  ;; │ finder-mode │
  ;; ╰─────────────╯

  (add-hook 'finder-mode-hook 'my/finder-mode-init)

  (defun my/finder-mode-init ()
    (define-key finder-mode-map (kbd "C-h") 'help-map)
    (evilified-state-evilify-map finder-mode-map
        :mode finder-mode
        ))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────╮
  ;; │ gitconfig │
  ;; ╰───────────╯

  (spacemacs/set-leader-keys-for-major-mode 'giconfig-mode
      "cm"    'my/gitconfig-convert-master
      )


  (my/kmacro-fset 'my/gitconfig-convert-master
    "Convert a .gitconfig alias involving master to detect the default branch name"
    [48 102 61 97 32 33 103 105 116 escape 86 58 115 47 109 97 115 116 101 114 47 36 40 115 101 100 32 39 115 124 46 42 92 47 124 124 39 32 46 103 105 116 92 47 114 101 102 115 92 47 114 101 109 111 116 101 115 92 47 111 114 105 103 105 110 92 47 72 69 65 68 41 47 103 return])

  ;; structured-haskell-mode - issues with evil:
  ;;     https://github.com/chrisdone/structured-haskell-mode/issues/81
  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────────╮
  ;; │ Haskell-Mode │
  ;; ╰──────────────╯

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
      (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
      (add-to-list 'evil-emacs-state-modes 'haskell-interactive-mode)
      ))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────╮
  ;; │ helm-mode │
  ;; ╰───────────╯

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
    (my/define-keys helm-map
      (kbd "M-RET")  spacemacs-helm-major-mode-map
      (kbd "M-m")    spacemacs-cmds
      (kbd "S-SPC")  spacemacs-cmds
      )
    )

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────────╮
  ;; │ helm-firefox │
  ;; ╰──────────────╯

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

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────╮
  ;; │ help-mode │
  ;; ╰───────────╯

  (eval-after-load "help-mode"
    `(progn
       (bind-keys :map help-mode-map
                  ("a" . help-previous)
                  ("d" . help-next)
                  )
       (define-key button-map [remap push-button] #'my/push-button-and-center)
       ))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────╮
  ;; │ Hy-Mode │
  ;; ╰─────────╯

  (which-key-add-major-mode-key-based-replacements 'hy-mode
    "SPC m s"    "evaluate"
    ", s"        "evaluate"
    "SPC m V"    "pyvenv"
    ", V"        "pyvenv"
    )

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────╮
  ;; │ ibuffer │
  ;; ╰─────────╯

  (evil-set-initial-state 'ibuffer-mode 'evilified)
  (eval-after-load 'ibuffer
    `(progn
       (require 'ibuffer-hydra)
       (define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
       (add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)
       ))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────╮
  ;; │ ido-mode │
  ;; ╰──────────╯

  (defun ido-init ()
    (bind-keys :map ido-completion-map
               ("M-+" . ido-make-directory)
               ("M-=" . ido-make-directory)
               ("M-m" . spacemacs-cmds)
               ))

  (add-hook 'ido-setup-hook 'ido-init)

  ;; (spacemacs/set-leader-keys-for-minor-mode 'ido-mode
  ;;   )

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────╮
  ;; │ Info-Mode │
  ;; ╰───────────╯

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
       (my/define-keys Info-mode-map
         (kbd "M-RET") 'spacemacs-Info-mode-map
         (kbd "M-;")   'evil-repeat-find-char-reverse
         )
       ))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────────────────────────────────────────────────╮
  ;; │ highlighting -- symbol-overlay and highlight-phrase │
  ;; ╰─────────────────────────────────────────────────────╯

  (evil-define-key '(normal visual) global-map (kbd "C-M-h") nil)
  (bind-keys
   :map symbol-overlay-map
   ("C-M-h"       . my/toggle-highlight-region)
   ("<backspace>" . hlt-unhighlight-regexp-groups-region)
   ("C-M-u"       . hlt-unhighlight-all-prop)
   ("M-h"         . symbol-overlay-mode)
   ("N"           . symbol-overlay-switch-forward)
   ("P"           . symbol-overlay-switch-backward)
   ("R"           . symbol-overlay-remove-all))

  (defun my/toggle-highlight-region (term)
    (interactive "sHighlight term:")
    (if (string-empty-p term)
        (hlt-unhighlight-regexp-groups-region)
      (let ((case-fold-search t))
        (hlt-highlight-regexp-groups-region nil nil term))))

  (defun my/hlt-unhighlight-last ()
    (interactive)
    (hlt-unhighlight-all-prop nil nil))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────╮
  ;; │ ISearch │
  ;; ╰─────────╯

  (define-key isearch-mode-map (kbd "C-'") 'avy-isearch)
  (define-key isearch-mode-map [67108898] 'helm-swoop) ;; C-doublequote

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────────╮
  ;; │ Indent-Guide │
  ;; ╰──────────────╯

  (setq indent-guide-recursive t)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───╮
  ;; │ J │
  ;; ╰───╯

  (add-to-load-path "~/.emacs.d/private/local/j-mode")
  (autoload 'j-mode "j-mode.el" "Major mode for editing J files" t)

  (eval-after-load 'j-mode
    '(progn
      ;; J Console named 'jcons' to avoid conflict with Java Monitoring & Management Console executable
      (setq j-console-cmd "ijconsole")
      (setq j-help-local-dictionary-url "/opt/j64-804/addons/docs/help/dictionary/")
      ))

  (add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────╮
  ;; │ Java │
  ;; ╰──────╯

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

  ;; Java Keyboard Macros.
  ;;
  (fset 'my/java-fn-from-spec
        [86 201326629 47 47 46 42 return return 102 58 120 119 104 167772192 1
            102 41 108 11 1 101 112 65 32 123 125 escape 106 1])
  (fset 'my/java-constructor-from-spec
        [1 86 201326629 47 47 46 42 36 92 124 36 13 123 125 13 121 106 1])
  (fset 'my/java-field-from-spec
        [1 102 58 119 100 119 1 101 97 32 escape 112 102 58 114 59 86 134217848
           100 101 108 101 116 101 45 116 114 97 105 108 105 110 103 45 119 104
           105 116 101 115 112 97 99 101 13 65 escape 106 1])

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────╮
  ;; │ js2-mode │
  ;; ╰──────────╯

  (defun my/js-show-methods ()
    (interactive)
    (my/pcre-occur "^ +[^ ]+:(.*) \\{")
    (switch-to-buffer-other-window "*Occur*"))

  (defun my/js-url-decode ()
    (interactive)
    (my/evil-shell-command-replace-region (region-beginning) (region-end) "inclusive" "sed 's/+/%2B/g' | xargs -0 urlencode -d"))

  (my/kmacro-fset 'my/js-fn-to-method
          "Convert a named function to an object property anonymous function"
          [58 115 47 94 92 40 32 42 92 125 92 41 36 47 92 49 44 47 return
              103 118 58 115 47 94 32 42 102 117 110 99 116 105 111 110 32 92 40 91 94 32 40 93 43 92 41 47 92 49 58 32 102 117 110 99 116 105 111 110 47 return
              103 118 tab])

  (my/kmacro-fset 'my/js-method-to-fn
    "Convert an object property anonymous function to a named function"
    [86 58 115 47 92 40 91 94 32 93 43 92 41 58 32 42 102 117 110 99 116 105 111 110 47 102 117 110 99 116 105 111 110 32 92 49 47 return 5 37 108 120])

  (defun my/quoted-string-to-delimited-comment ()
    (interactive)
    (evil-ex-substitute
     (region-beginning)
     (region-end)
     '("^\\( *\\)\"\\([^\"]*\\)\";? *$")
     '(replace-eval-replacement . "\\1/* \\2 */")))

  (defun my/lookup-chrome-webextension-api (s)
    (interactive
     (list
      (let ((name-at-point (my/qualified-name-at-point)))
        (read-string
         (format "Search for: %s" name-at-point)
         nil t
         name-at-point))))
    (browse-url-firefox
     (concat "https://www.google.com.au/search?hl=en&q="
             "chrome+%28webextension+OR+javascript%29+"
             s)))

  (defun my/lookup-firefox-webextension-api (s)
    (interactive
     (list
      (let ((name-at-point (my/qualified-name-at-point)))
        (read-string
         (format "Search for: %s" name-at-point)
         nil t
         name-at-point))))
    (browse-url-firefox
     (concat "https://www.google.com.au/search?hl=en&q="
             "firefox+%28webextension+OR+javascript%29+"
             s)))

  (defun my/js-method-to-arrow-function ()
    (interactive)
    (let ((p (evil-a-paragraph)))
      (goto-char (car p))
      (re-search-forward "\\([a-zA-Z0-9_!]+\\): function() {\n *\\(.*[^;]\\);?\n *}," (cadr p))
      (replace-match "\\1: () => \\2,")
      ))

  (defun my/double-slash-comment-to-delimited ()
    (interactive)
    (let ((beg (if (region-active-p) (region-beginning) (line-beginning-position)))
          (end (if (region-active-p) (region-end) (line-end-position))))
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "// ?\\(.*$\\)" end)
          (replace-match "/* \\1 */")))))

  (defun my/urldecode-bookmarklet (start end)
    (interactive "r")
    (my/shell-command-replace-region start end "sed 's/+/%2B/g' | xargs -0 urlencode -d"))

  (my/def-variable-local-cycle js-indent-level 4 2)
  (setq js-switch-indent-offset 2)

  (spacemacs/set-leader-keys-for-major-mode 'js2-mode
    "d"    'my/urldecode-bookmarklet
    "hwc"  'my/lookup-chrome-webextension-api
    "hwf"  'my/lookup-firefox-webextension-api
    "J"    'my/collapse-single-line-function
    "om"   'my/js-show-methods
    "ti"   'my/cycle-js-indent-level
    "va"   'my/js-method-to-arrow-function
    "vf"   'my/js-method-to-fn
    "vm"   'my/js-fn-to-method
    "vq"   'my/quoted-string-to-delimited-comment
    "v/"   'my/double-slash-comment-to-delimited
    "%"    'my/js-url-decode
    )

  (which-key-add-major-mode-key-based-replacements 'js2-mode
      ", t"    "toggles"
      ", h w"  "webextension docs"
      )

  ;; Greasemonkey script fix for js-mode
  (eval-after-load 'js
    '(progn
      (setq js--regexp-literal-fix
       "[^=][=(,:]\\(?:\\s-\\|\n\\)*\\(/\\)\\(?:\\\\.\\|[^/*\\]\\)\\(?:\\\\.\\|[^/\\]\\)*\\(/\\)")
      (setq js-font-lock-syntactic-keywords-fix
       ;; "|" means generic string fence
       `((,js--regexp-literal-fix (1 "|") (2 "|"))))
      (setq js-font-lock-syntactic-keywords js-font-lock-syntactic-keywords-fix)))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────╮
  ;; │ JSON-Mode │
  ;; ╰───────────╯

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

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────╮
  ;; │ Magit │
  ;; ╰───────╯


  (defun magit-init-fn ()
    (interactive)
    ;; remove C-tab binding which shadows #'next-multiframe-window binding
    ;; replace with C-` binding
    (bind-key "<C-tab>" nil magit-mode-map)
    (fset 'magit-status-mode-map magit-status-mode-map)
    (global-magit-file-mode t)
    (bind-keys
     :map magit-mode-map
     ("<C-tab>" . nil)
     ("<C-`>"   . magit-section-cycle)
     ("m"       . evil-set-marker)
     ("q"       . my/magit-mode-kill-buffer)
     ("R"       . magit-remote-popup)
     ("S"       . magit-submodule-popup)
     ("'"       . my/evil-goto-mark-line)
     )
    (evil-define-key 'normal magit-mode-map (kbd "m") 'evil-set-marker)
    (evil-define-key 'normal magit-mode-map (kbd "q") 'my/magit-mode-kill-buffer)
    (evil-define-key 'normal magit-mode-map (kbd "R") 'magit-remote-popup)
    (evil-define-key 'normal magit-mode-map (kbd "S") 'magit-submodule-popup)
    (evil-define-key 'normal magit-mode-map (kbd "'") 'my/evil-goto-mark-line)
    (evil-define-key 'normal magit-mode-map (kbd "-") 'dired-jump)
    (evil-define-key 'normal magit-mode-map (kbd "S-SPC") nil)
    (add-hook 'magit-diff-visit-file-hook #'recenter)
    (setf magit-log-revision-headers-format "\
%+b
Author:    %aN <%aE>
Committer: %cN <%cE>"))

  (eval-after-load 'magit-blame
    `(progn
       (define-key magit-blame-read-only-mode-map (kbd "C-b") 'my/magit-blame-and-center)
       ))

  (eval-after-load 'magit-mode
    `(progn
       (add-hook 'magit-mode-hook 'magit-init-fn)
       ))

  (eval-after-load 'magit-status
    `(progn
       (bind-keys
        :map magit-status-mode-map
        ("<S-return>" . magit-diff-visit-file-other-window)
        )
       ))

  (eval-after-load 'magit-diff
    `(progn
       (bind-keys
        :map magit-diff-mode-map
        ("<S-return>" . magit-diff-visit-file-other-window)
        )
       ))

  (spacemacs/set-leader-keys-for-major-mode 'magit-diff-mode
      "s"          'magit-diff-toggle-ignore-all-space
      "S"          'magit-diff-toggle-ignore-space-change
      )

  (spacemacs/set-leader-keys-for-major-mode 'magit-status-mode
      "c"      'magit-status-mode-map
      "d g g"  'my/magit-diff-meld
      "d g a"  'my/magit-diff-added-meld
      "d g c"  'my/magit-diff-committed-meld
      "d f"    'magit-diff-toggle-refine-hunk
      "o"      'my/git-browse-origin
      "r"      'my/magit-undo-last-commit
      "s"      'magit-diff-toggle-ignore-all-space
      "S"      'magit-diff-toggle-ignore-space-change
      "y c"    'my/git-yank-commit-messages
      "SPC"    'magit-diff-show-or-scroll-up
      "<F2>"   'magit-file-rename
      )

  (which-key-add-major-mode-key-based-replacements 'magit-status-mode
      ", c"      "magit-status-mode-map"
      ", d"      "diff"
      ", y"      "yank"
      )

  (defvar my/git-executable "/usr/local/bin/git")

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
    (magit-reset-soft "HEAD~"))

  (defun my/magit-stash-changes-and-checkout ()
    (interactive)
    (magit-stash "*temp*")
    (call-interactively 'magit-checkout))

  (defun my/magit-back-to-master-pop-changes ()
    (interactive)
    (magit-checkout "master")
    (magit-stash-pop "*temp*"))

  (defun my/git-browse-origin ()
    (interactive)
    (let* ((url (shell-command-to-string "git config --get remote.origin.url"))
           (https-url (if (s-starts-with? "git@" url)
                          (replace-regexp-in-string
                           "^git@" "https://"
                           (replace-regexp-in-string
                            ":" "/" url))
                        url)))
      (browse-url-firefox https-url)))

  (defun my/git-yank-commit-messages (n)
    (interactive "p")
    (let ((s (s-trim (shell-command-to-string (format "git log --format=%%B -n 1 HEAD~%d" (decf n))))))
       (kill-new s)
       (message s)))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────╮
  ;; │ Markdown │
  ;; ╰──────────╯

  (setq-default my/markdown-browser browse-url-generic-program)

  (bind-keys :map spacemacs-markdown-mode-map
             ("i C-l"  . my/markdown-gh-linkify-heading)
             ("n"      . my/markdown-next-link)
             ("x ="    . my/uppercase-double-underline)
             ("1"      . my/markdown-underline-heading)
             ("."      . markdown-follow-link-at-point)
             ("TAB"    . markdown-show-all)
             ("C-v"    . my/markdown-app-call)
             ("M-h"    . my/github-heading-to-readme-link)
             ("M-l"    . my/github-linkify-heading)
             )

  (defun markdown-init-fn ()
    (interactive)
    (define-key markdown-mode-mouse-map (kbd "<mouse-2>") 'my/mouse-set-point-and-browse-url)
    )

  (eval-after-load 'markdown-mode
    `(progn
       (add-hook 'markdown-mode-hook 'markdown-init-fn)
       ))

  (defun my/mouse-set-point-and-browse-url (event)
    (interactive "e")
    (let ((p (posn-point (event-start event))))
      (goto-char p)
      (my/browse-url-at-point p)))

  (defun my/markdown-gh-linkify-heading ()
    (interactive)
    (let* ((heading-line (my/get-current-line))
           (heading (replace-regexp-in-string "^#*[ \\t]*" "" heading-line))
           (heading-fragment (downcase
                              (concat "#" (replace-regexp-in-string " " "-" heading))))
           (link (format "[%s](%s)\n" heading heading-fragment)))
      (copy-string-as-kill link)))

  (defun my/markdown-export-to-html-and-view (&optional file)
    "Export markdown to html and view with XDG default browser."
    (interactive)
    (let ((fname (if file
                     (with-temp-buffer
                       (insert-file-contents file t)
                       (markdown-export (markdown-export-file-name)))
                   (unless (buffer-file-name)
                     (write-file (make-temp-file "markdown-output")))
                   (markdown-export (markdown-export-file-name)))))
      (browse-url-xdg-open fname)))

  (defun my/markdown-view-with-remarkable (file)
    "View FILE with remarkable (https://github.com/jamiemcg/remarkable)."
    (interactive (list (buffer-file-name)))
    (my/async-shell-command-no-window
     (format "remarkable '%s'" file)))

  (defun my/markdown-view (file &optional app)
    "View markdown FILE with APP.

APP is specified as a key from `my/markdown-apps'.

When called interactively, view the current file using an app specified by the
current prefix argument.

    nil  convert to html & view in default browser
    C-u  [ choose from completion menu ]
    1    grip server & view in firefox
    2    firefox markdown plugin
    3    remarkable"
    (interactive
     (list (buffer-file-name)
           (let ((arg current-prefix-arg))
             (cond ((null arg)        (intern (completing-read "app: " my/markdown-apps)))
                   ((equalp arg 1)    'browser/md)
                   ((equalp arg 2)    'grip/browser)
                   ((equalp arg 3)    'remarkable)
                   ((equalp arg '(4)) 'xdg/html)
                   (t                 'xdg/html)
                   ))))
    (my/markdown-app-call (or app 'remarkable) file))

  (defvar my/markdown-apps
    (list
     '(grip/browser . (progn (my/async-shell-command-no-window
                              (format "grip '%s'" (buffer-file-name)))
                             (run-with-timer
                              1.0 nil
                              (fn: start-process
                                   "localhost" nil my/markdown-browser "http://localhost:6419"))))
     '(xdg/html     . (my/markdown-export-to-html-and-view))
     '(browser/md   . (my/async-shell-command-no-window (format "%s 'file://%s'" my/markdown-browser file)))
     '(remarkable   . (my/async-shell-command-no-window (format "remarkable '%s'" file))))
    "List of (SYMBOL . FUNCTION) pairs specifying applications for opening
markdown files.")

  (defun my/markdown-app-call (app &optional file)
    "Lookup APP in `my/markdown-apps' and call with 'file bound to FILE.

If FILE is nil, the file associated with the current buffer is used."
    (interactive (list (intern (completing-read "app: " my/markdown-apps))
                       (buffer-file-name)))
    (let ((code (alist-get app my/markdown-apps)))
      (eval `(let ((file ,file)) ,code))))

  (defun my/github-heading-to-readme-link ()
    (interactive)
    (let* ((heading  (thing-at-point 'line t))
           (stripped (replace-regexp-in-string "^#* *\\| *$" "" heading))
           (link     (downcase
                      (replace-regexp-in-string
                       " " "-"
                       (replace-regexp-in-string
                        "[&()]" ""
                        stripped)))))
      (kill-new link)))

  (defun my/github-linkify-heading (&optional href)
    (interactive)
    (let* ((heading  (thing-at-point 'line t))
           (hashes   (replace-regexp-in-string
                      "\n" ""
                      (replace-regexp-in-string " .*" " " heading)))
           (stripnl  (replace-regexp-in-string "\n" "" heading))
           (stripped (replace-regexp-in-string "^#* *\\| *$" "" stripnl))
           (href     (or href
                         (replace-regexp-in-string
                          "\n" ""
                          (my/github-heading-to-readme-link))))
           (result   (format "%s[%s](#%s)" hashes stripped href)))
      (beginning-of-line)
      (kill-line)
      (insert result)))

  (defun my/markdown-next-link ()
    (interactive)
    (markdown-next-link)
    (search-forward-regexp "://"))

  (defun my/markdown-underline-heading (&optional remove-first-WORD)
    (interactive "P")
    (beginning-of-line 1)
    (when remove-first-WORD
      (delete-region (point)
                     (min
                      (save-excursion (evil-forward-WORD-begin) (point))
                      (line-end-position))))
    (kmacro-exec-ring-item '([ ?y ?y ?p ?v ?i ?l ?r ?= ] 0 "%d") 1))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────╮
  ;; │ Lisp │
  ;; ╰──────╯

  (bind-keys :map spacemacs-emacs-lisp-mode-map
             ;; common
             ("M-RET"  . lisp-state-toggle-lisp-state)
             ("e j"    . my/eval-print-last-sexp)
             ("e p"    . my/eval-prettyprint-last-sexp)
             ("e y"    . my/eval-yank-last-sexp)
             ("e D"    . eval-instrument-defun)
             ("e RET"  . my/eval-replace-last-sexp)
             ("C-M-x"  . eval-defun)
             ("g g"    . my/spacemacs/jump-to-definition-and-center)
             ("g h"    . my/emacs-lisp-goto-heading)
             ("t i"    . ert-run-tests-interactively)
             ("j"      . my/eval-print-last-sexp)
             ("v"      . my/evil-select-sexp-at-point)
             ("x"      . prettyexpand-at-point)
             ("; j"    . my/eval-print-last-sexp-as-comment)
             ("; p"    . my/eval-prettyprint-last-sexp-as-comment)
             ("<f3> n" . kmacro-name-last-macro)
             ("<f3> p" . insert-kbd-macro)
             (">"      . my/delete-function-application)
             ("C-l"    . reposition-window)
             ("C-m"    . spacemacs/macrostep-transient-state/body)
             ;; emacs-lisp-mode only
             ("M-w"    . elu-github-copy-md-sig-and-doc)
             ("M-t"    . elu-create-test)
             )
  (bind-keys :map spacemacs-lisp-interaction-mode-map
             ;; common
             ("M-RET"  . lisp-state-toggle-lisp-state)
             ("e j"    . my/eval-print-last-sexp)
             ("e p"    . my/eval-prettyprint-last-sexp)
             ("e y"    . my/eval-yank-last-sexp)
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
             ("C-l"    . reposition-window)
             ("C-m"    . spacemacs/macrostep-transient-state/body)
             )
  (bind-keys :map emacs-lisp-mode-map
             ("M-B"  . my/backward-evil-defun)
             ("M-F"  . my/forward-evil-defun)
             )
  (bind-keys :map lisp-interaction-mode-map
             ("C-j"  . my/eval-prettyprint-last-sexp)
             )

  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
      ", d"         "macrostep"
      ", <f3>"      "kmacro"
      )

  (eval-after-load 'evil-lisp-state
    '(progn
      (if evil-lisp-state-global
          (define-key evil-lisp-state-map "/" (evil-lisp-state-enter-command sp-split-sexp))
        (define-key evil-lisp-state-major-mode-map "/" (evil-lisp-state-enter-command sp-split-sexp)))))

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

  (defun my/emacs-lisp-goto-heading (h)
    "Jump to specified boxed heading"
    (interactive "sHeading: ")
    (let ((start-pos (point)))
      (beginning-of-buffer)
      (if (re-search-forward (pcre-to-elisp (concat "^ *;;? *\\| +" h ".* \\|") "i") nil t)
          (progn
            (recenter 4)
            (beginning-of-line))
        (goto-char start-pos))))

  (evil-define-motion my/forward-evil-defun (count)
    "Move to the end of the COUNT-th next paragraph."
    :jump t
    :type exclusive
    (evil-signal-at-bob-or-eob count)
    (evil-forward-beginning 'evil-defun count)
    (evil-first-non-blank))

  (evil-define-motion my/backward-evil-defun (count)
    "Move to the end of the COUNT-th next paragraph."
    :jump t
    :type exclusive
    (evil-signal-at-bob-or-eob count)
    (evil-backward-beginning 'evil-defun count)
    (evil-first-non-blank))

  ;; ===== TESTING MACROS FOR MY PACKAGES =====

  (cl-defmacro should-equal (expr &key result)
    `(should (equal ,expr ,result)))

  (cl-defmacro should-not-equal (expr &key result)
    `(should-not (equal ,expr ,result)))

  (cl-defmacro should-error-with (expr &key error)
    `(should (equal (should-error ,expr)
                    ,error)))

  (cl-defmacro should-error-with-type  (expr &key error)
    `(should (equal (car (should-error ,expr))
                    ,error)))

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

  ;; ╭──────────────────────────╮
  ;; │ CIDER Overlays for Elisp │
  ;; ╰──────────────────────────╯
  ;; http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html

  (autoload 'cider--make-result-overlay "cider-overlays")

  (defun endless/eval-overlay (value point)
    (cider--make-result-overlay (format "%S" value)
      :where point
      :duration 'command)
    ;; Preserve the return value.
    value)
  (defun endless/eval-region-advice (f beg end &rest r)
    (endless/eval-overlay (apply f beg end r) end))
  (defun endless/eval-last-sexp-advice (r) (endless/eval-overlay r (point)))
  (defun endless/eval-defun-advice (r)
    (endless/eval-overlay r (save-excursion (end-of-defun) (point))))
  (defun endless/eval-overlay-on ()
    (interactive)
    (advice-add 'eval-region :around #'endless/eval-region-advice)
    (advice-add 'eval-last-sexp :filter-return #'endless/eval-last-sexp-advice)
    (advice-add 'eval-defun :filter-return #'endless/eval-defun-advice))
  (defun endless/eval-overlay-off ()
    (interactive)
    (advice-remove 'eval-region #'endless/eval-region-advice)
    (advice-remove 'eval-last-sexp #'endless/eval-last-sexp-advice)
    (advice-remove 'eval-defun #'endless/eval-defun-advice))

  ;; ╭───────╮
  ;; │ lispy │
  ;; ╰───────╯

  (use-package lispyville
    :init
    (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
    :config
    (lispyville-set-key-theme '(operators c-w additional)))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────╮
  ;; │ Ocaml │
  ;; ╰───────╯

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

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭────────────╮
  ;; │ Occur-Mode │
  ;; ╰────────────╯

  (evilified-state-evilify-map occur-mode-map
    :mode occur-mode
    :bindings
    )

  ;; ╭─────────────╮
  ;; │ loccur-mode │
  ;; ╰─────────────╯
  (require 'loccur)

  ;; based on `loccur': GPL3
  (defun my/pcre-loccur (pcre)
    (interactive
     (list
      (cond
        ((region-active-p) (prog1
                               (buffer-substring (mark) (point))
                             (deactivate-mark)))
        (loccur-mode       "")
        (t                 (read-string "PCRE loccur: "
                                         (loccur-prompt)
                                         'loccur-history)))))
    (let ((regex (pcre-to-elisp pcre)))
      (if (or loccur-mode (string-empty-p pcre))
          (progn
            ;; Null current search and turn off loccur-mode
            (setf loccur-current-search nil)
            (loccur-mode 0))
        ;; otherwise: Check that regex is different than previous search
        (if (string-equal regex loccur-current-search)
            (message "regex equal to loccur-current-search")
          (cl-pushnew regex loccur-history)
          (setf loccur-current-search regex)
          (loccur-mode)
         (when loccur-jump-beginning-of-line (beginning-of-line))))))

  (set-face-attribute 'loccur-face nil :background "#C8E559" :foreground nil)


  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────╮
  ;; │ Org-Mode │
  ;; ╰──────────╯

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
     ("<C-S-return>" . nil)
     ("<S-return>" . nil)
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
    "SPC"          'ace-link-org
    "<C-tab>"      'org-force-cycle-archived
    "<S-return>"   'org-table-copy-down
    "<C-S-return>" 'org-insert-todo-heading-respect-content
    "C-M-h v"     'my/org-describe-variable
    "C-M-h f"     'my/org-describe-function
    "<tab>"        'org-cycle
    "<S-tab>"      'org-shifttab
    "h h"          'helm-org-in-buffer-headings
    "h 1"          'my/helm-org-in-buffer-h1
    "h 2"          'my/helm-org-in-buffer-h2
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

  (defun my/org-symbol-trim-= (sym)
    (let* ((str (symbol-name sym))
           (trimmed (replace-regexp-in-string "^=\\|=$" "" str)))
      (intern trimmed)))


  (defun my/org-describe-variable (variable)
    "Display the full documentation of VARIABLE (a symbol).
With a prefix argument, candidates are user variables (options) only.
Default candidate is the `symbol-nearest-point'.
Return the documentation, as a string.

VARIABLE names an Emacs Lisp variable, possibly a user option.
If VARIABLE has a buffer-local value in BUFFER or FRAME (default to
the current buffer and current frame) then it is displayed, along with
the global value."
    (interactive
     (let* ((symb0 (or (and (fboundp 'symbol-nearest-point)
                            (my/org-symbol-trim-= (symbol-nearest-point)))
                       (variable-at-point)))
            (symb (if (numberp symb0) nil symb0))
            (curbuf (current-buffer))
            val)
       (list
        (intern
         (completing-read
          (format "Describe variable%s: "
                  (if (and symb (boundp symb)) (format " (default %s)" symb) ""))
          obarray
          (if current-prefix-arg
              `(lambda (vv)
                 (with-current-buffer ',curbuf (user-variable-p vv)))
            `(lambda (vv)
               (with-current-buffer ',curbuf
                 (or (get vv 'variable-documentation)
                     (and (boundp vv) (not (keywordp vv)))))))
          t nil nil
          (and (symbolp symb) (boundp symb) (symbol-name symb))))
        )))
    (describe-variable variable))

  ;; based on 'describe-variable (GPL3)
  (defun my/org-describe-function (function &optional commandp)
    "Display the full documentation of FUNCTION (a symbol).
FUNCTION names an Emacs Lisp function, possibly a user command.
With a prefix argument, candidates are only commands (interactive).

Default candidate is: preferably the `symbol-nearest-point', or else
the innermost function call surrounding point
\(`function-called-at-point').
Return the description that was displayed, as a string."
    (interactive
     (let* ((symnpt                        (my/org-symbol-trim-= (symbol-nearest-point)))
            (fn                            (or (and (fboundp symnpt) symnpt)
                                               (function-called-at-point)))
            (enable-recursive-minibuffers  t)
            (completion-annotate-function  (lambda (fn) (and (commandp (intern-soft fn))  "  (command)")))
            (type                          (if current-prefix-arg 'command 'function))
            (prompt                        (format "Describe %s%s: " type
                                                   (if (if current-prefix-arg (commandp fn) (fboundp fn))
                                                       (format " (default %s)" fn)
                                                     "")))
            val)
       (setq val  (completing-read prompt obarray (if current-prefix-arg 'commandp 'fboundp) t nil nil
                                   (and (if current-prefix-arg (commandp fn) (fboundp fn))  (symbol-name fn))))
       (list (if (equal val "") fn (intern val)) current-prefix-arg)))
    (let* ((interactivep  (if (or (> emacs-major-version 23) ; Emacs 23.1 `called-interactively' accepts no arg.
                                  (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
                              (called-interactively-p 'interactive)
                            (interactive-p)))
           (err/msg-fn    (if interactivep #'message #'error))
           (fn/cmd-txt    (if commandp 'command 'function)))
      (if (and interactivep  (not function))
          (funcall err/msg-fn "You did not specify a function symbol") ; Avoid "Not a defined function: `nil'".
        (if (not (if commandp
                     (commandp function)
                   (or (functionp function) ; Allow anonymous functions (Emacs bug #24221).
                       (and function  (fboundp (intern-soft function)))))) ; Allow macros and special forms.
            (funcall err/msg-fn "Not a defined %s: `%S'" fn/cmd-txt function)
          (help-setup-xref (list #'describe-function function)
                           (if (or (> emacs-major-version 23) ; Emacs 23.1 `called-interactively' accepts no arg.
                                   (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
                               (called-interactively-p 'interactive)
                             (interactive-p)))
          (save-excursion
            (if (fboundp 'with-help-window)
                (with-help-window  (help-buffer) ; Emacs 24.4 needs this - see Emacs bug #17109.
                  (prin1 function)
                  ;; Use " is " instead of ": " so it is easier to get the function name using `forward-sexp'.
                  (princ " is ")
                  (describe-function-1 function)
                  (with-current-buffer standard-output (buffer-string))) ; Return help text.
              (with-output-to-temp-buffer (help-buffer)
                (prin1 function)
                ;; Use " is " instead of ": " so it is easier to get the function name using `forward-sexp'.
                (princ " is ")
                (describe-function-1 function)
                (print-help-return-message)
                (with-current-buffer standard-output (buffer-string)))))))))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────╮
  ;; │ PDF-View-Mode │
  ;; ╰───────────────╯

  (add-hook 'pdf-view-mode-hook 'my/pdf-view-mode-init)

  (defun my/pdf-view-mode-init ()
    (interactive)
    (spacemacs/toggle-line-numbers-on))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭────────╮
  ;; │ Proced │
  ;; ╰────────╯

  (add-hook 'proced-mode-hook 'my/proced-init)

  (defun my/proced-init ()
    (interactive)
    (fset 'proced-mode-map proced-mode-map))

  ;; set function definition of 'proced-mode-map (same as value)
  (spacemacs/set-leader-keys-for-major-mode 'proced-mode
      "<f10>"      'my/lacarte-execute-local-menu-command
      "q"          'quit-window
      "c"          'proced-mode-map
      )

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────╮
  ;; │ Ruby-mode │
  ;; ╰───────────╯

  (defun ruby-init ()
    (define-key spacemacs-ruby-mode-map "sb" 'ruby-send-buffer)
    (define-key spacemacs-ruby-mode-map "sI" 'inf-ruby))

  (add-hook 'ruby-mode-hook 'ruby-init)

  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
    "g l"      'goto-gem
    "g /"      'goto-gem-grep-gem
    "g ?"      'goto-gem-grep-all-gems
    )

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────────╮
  ;; │ Shell Script │
  ;; ╰──────────────╯

  (spacemacs/set-leader-keys-for-major-mode 'sh-mode
      "J"     'my/collapse-single-line-function
      "."     'find-file-at-point
      )

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────────────╮
  ;; │ Spacemacs-Buffer-Mode │
  ;; ╰───────────────────────╯

  (which-key-add-major-mode-key-based-replacements 'spacemacs-buffer-mode
    "m"     "jump to menu"
    )

  (setq undo-tree-git-repo "http://www.dr-qubit.org/git/undo-tree.git")
  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────╮
  ;; │ SVG │
  ;; ╰─────╯
  (setq image-file-name-extensions (remove "svg" image-file-name-extensions))
  (add-to-list 'auto-mode-alist '("\\.svg\\'" . xml-mode))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────╮
  ;; │ Undo Tree │
  ;; ╰───────────╯

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


  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────────────────╮
  ;; │ Undo-Tree-Visualizer-Mode │
  ;; ╰───────────────────────────╯

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

  ;; ───────────────
  ;; Leader Bindings
  ;; ───────────────
  (bind-keys :map spacemacs-emacs-lisp-mode-map
             ("m" . spacemacs-undo-tree-visualizer-mode-map)
             )

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────╮
  ;; │ View-Mode │
  ;; ╰───────────╯

  (evil-define-key 'normal view-mode-map
    "q"   'View-quit
    )
  (defun view-mode-init ()
    (spacemacs/set-leader-keys-for-minor-mode 'view-mode
      "q"   'View-quit
      ))
  (add-hook 'view-mode-hook #'view-mode-init)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────╮
  ;; │ Web-Mode │
  ;; ╰──────────╯

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
    '(my/define-keys emmet-mode-keymap
       (kbd "<C-return>")   'my/open-line-below
       (kbd "<C-S-return>") 'my/open-line-above
       ))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────╮
  ;; │ w3m │
  ;; ╰─────╯

  (add-hook 'w3m-mode-hook 'my/w3m-mode-init)

  (defun my/w3m-mode-init ()
    (interactive)
    (define-key w3m-mode-map (kbd "C-h") 'help-map)
    (evilified-state-evilify-map w3m-mode-map
        :mode w3m-mode
        :bindings
        (kbd "C-h")  'help-map)
    )

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭──────────╮
  ;; │ Snippets │
  ;; ╰──────────╯

  (defun my/yas-expand (&optional field)
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

  (defun my/yas-reload-all-no-jit () (interactive) (yas-reload-all t))

  (defun my/aya-create-snippet-with-newline ()
    (interactive)
    (let ((aya-create-with-newline t))
      (call-interactively 'aya-create)))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────╮
  ;; │ Align │
  ;; ╰───────╯

  (unless (boundp 'align-default-spacing) (setf align-default-spacing 1))

  (defmacro spacemacs|create-align-repeat-x (name regexp &optional justify-right default-after)
    (let* ((new-func (intern (concat "spacemacs/align-repeat-" name)))
           (new-func-defn
            `(defun ,new-func (start end switch)
               (interactive "r\nP")
               (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
                 (spacemacs/align-repeat start end ,regexp ,justify-right after)))))
      (put new-func 'function-documentation "Created by `spacemacs|create-align-repeat-x'.")
      new-func-defn))

  (spacemacs|create-align-repeat-x "period" "\\." t nil)
  (spacemacs|create-align-repeat-x "quote" "'")
  (spacemacs|create-align-repeat-x "double-quote" (string 34))
  (spacemacs|create-align-repeat-x "dash" "-")
  (spacemacs|create-align-repeat-x "hash" "#" )
  (spacemacs|create-align-repeat-x "star" "\\*")
  (spacemacs|create-align-repeat-x "semicolon-comment" ";;?" )
  (spacemacs|create-align-repeat-x "slash-comment" "//" )

  ;; Redefine spacemacs functions (to add link to help)
  (spacemacs|create-align-repeat-x "comma" "," nil t)
  (spacemacs|create-align-repeat-x "semicolon" ";" nil t)
  (spacemacs|create-align-repeat-x "colon" ":" nil t)
  (spacemacs|create-align-repeat-x "equal" "=")
  (spacemacs|create-align-repeat-x "math-oper" "[+\\-*/]")
  (spacemacs|create-align-repeat-x "ampersand" "&")
  (spacemacs|create-align-repeat-x "bar" "|")
  (spacemacs|create-align-repeat-x "left-paren" "(")
  (spacemacs|create-align-repeat-x "right-paren" ")" t)
  (spacemacs|create-align-repeat-x "backslash" "\\\\")

  (defun my/pcre-align (beg end pcre &optional group spacing repeat)
    "Align a region using a PCRE. Requires pcre2el.

Interactively, operates on the current region and prompts for the PCRE, GROUP,
SPACING and REPEAT.

GROUP is the number of the group to be modified (ie. spacing group). The entire
PCRE is group 1, the first subexpression group 2, etc. A negative group value
indicates that spaces should be added before the group, a positive value means
spaces should be added after it.

SPACING is the minimum number of spaces between columns.

See `align-regexp' for details."
    (interactive
     (list
      (region-beginning)
      (region-end)
      (pcre-to-elisp (rxt--read-pcre "PCRE: "))
      (string-to-number (read-string "Paren group (neg=insert before, pos=insert after) [-1]: " nil t "0"))
      (string-to-number (read-string "Number of spaces (or column if negative) [1]: " nil t "1"))
      (my/y-or-n-p "Repeat?")))
    (unless beg (setq beg (region-beginning)))
    (unless beg (setq beg (region-end)))
    (unless group (setq group 0))
    (unless spacing (setq spacing align-default-spacing))
    (align-regexp beg end pcre group spacing repeat))

  (defun my/quick-pcre-align (BEG END s &optional spacing repeat)
    "Align region using a PCRE. PCRE doesn't require the group for expansion. Requires pcre2el."
    (interactive "r\nsPCRE to align on: ")
    (unless BEG (setq BEG (region-beginning)))
    (unless BEG (setq BEG (region-end)))
    (unless spacing (setq spacing align-default-spacing))
    (let ((regexp (format "(\s*)(%s)" s)))
      (align-regexp BEG END (pcre-to-elisp regexp) 1 spacing repeat)))

  (defun my/quick-pcre-align-repeat (BEG END s &optional spacing)
    "Align region using repeated matches of a PCRE. Requires pcre2el."
    (interactive "r\nsPCRE to align on: ")
    (unless BEG (setq BEG (region-beginning)))
    (unless BEG (setq BEG (region-end)))
    (unless spacing (setq spacing align-default-spacing))
    (let ((regexp (format "(\s*)(%s)" s)))
      (align-regexp BEG END (pcre-to-elisp regexp) 1 spacing t)))

  (defun my/align-after-colon (BEG END spacing)
    "Align the first word after a colon in each line in the region.
The minimum spacing is given by the prefix argument, if given, or
otherwise is equal to 'align-default-spacing."
    (interactive "r\nP")
    (let ((padding (cond ((null spacing) align-default-spacing)
                         (t              (prefix-numeric-value spacing))))
          (regexp  "\\(?:[:]\\)\\(\\s-*\\).*"))
      (align-regexp (region-beginning) (region-end) regexp 1 padding nil)))

  (defun my/align-whitespace (BEG END spacing &optional repeat)
    "Align the first whitespace region on each line (or all if REPEAT is t)."
    (interactive "r\np")
    (unless spacing (setq spacing align-default-spacing))
    (align-regexp BEG END (pcre-to-elisp "\\s+") 0 spacing repeat))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────╮
  ;; │ Regex │
  ;; ╰───────╯

  ;; modified emacs source: GPL3
  (defun my/pcre-occur (regexp &optional nlines)
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
  (defun my/pcre-multi-occur (bufs regexp &optional nlines)
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

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────────────╮
  ;; │ Keyboard Macros │
  ;; ╰─────────────────╯

  (fset 'my/switch-to-most-recent-buffer [?\C-x ?b return])
  (fset 'my/other-window-quit [C-tab ?q])

  (defun my/switch-to-most-recent-buffer-other-window ()
    (interactive)
    (other-window 1)
    (my/kmacro-call #'my/switch-to-most-recent-buffer)
    (other-window 1))

  ;; note: (fset 'foo [...]) -- keyboard macros set this way only in bindings.
  ;;       Can't be called from lisp: use my/kmacro-call to call if necessary.
  (fset 'my/md-sig-to-list-item
        [118 69 121 103 118 115 93 105 42 32 escape 69 108 120 97 35 escape 80
             16 97 45 escape 118 36 104 201326629 38 63 32 return 45 return])
  (fset 'my/setq->push
        [99 119 112 117 115 104 escape
            119 100 119 108 100 119 104 32 107 87 escape])

  (fset 'my/just-one-space-before-open-brace
        [102 91 104 134217848 106 117 115 116 45 111 110 101 45 115 112 97 99
             101 return 106 1])
  (fset 'my/add-spacing-inside-parens [ ?c ?s 41 40 ])
  (fset 'my/remove-spacing-inside-parens [ ?c ?s 40 41 ])
  (fset 'my/add-spacing-inside-brackets [ ?c ?s 93 91 ])
  (fset 'my/remove-spacing-inside-brackets [ ?c ?s 91 93 ])
  (fset 'my/add-spacing-inside-braces [ ?c ?s 125 123 ])
  (fset 'my/remove-spacing-inside-braces [ ?c ?s 123 125 ])
  (fset 'my/add-spacing-inside-double-quotes [ ?c ?s 34 40 ?c ?s 41 34 ])
  (fset 'my/remove-spacing-inside-double-quotes [?c ?s 40 34 ])
  (fset 'my/collapse-single-line-function [escape ?J ?J ?h ?c ?s 125 123])
  (fset 'my/quote-to-end-of-line [escape ?v 5 left ?s 34])
  (fset 'my/uppercase-double-underline [escape ?v ?i ?l ?g ?u ?y ?y ?p ?v ?i ?l ?r ?=])
  (fset 'my/yank-inside-double-quotes [?y ?i 34])

  (fset 'my/surround-symbol
   (lambda (&optional arg)
     (interactive "p")
     (evil-normal-state)
     (kmacro-exec-ring-item '([ ?v ?i ?o ] 0 "%d") arg)
     (call-interactively #'evil-surround-region)))

  (my/kmacro-fset 'my/wdired-tidy-name-keep-square-brackets
    "Keyboard macro. In wdired-mode, remove everything between after the first )
except the the first [...] (preceded by a space) and everything after a dot.
Then move to the next line (column 3).

    KEEP) DELETE[KEEP]DELETE.KEEP"
    [102 41 97 32 escape 118 116 91 99 32 escape 102 93 108 100 116 46 106 48 108 108])

  (my/kmacro-fset 'my/anime-file-names-to-multiline
          "Kbd macro: convert name in format ENG・ROMAJI (JAP) to 3 lines."
        [escape ?V 134217848
                ?d ?e ?l ?e ?t ?e ?- ?t ?r ?a ?i ?l ?i ?n ?g
                ?- ?w ?h ?i ?t ?e ?s ?p ?a ?c ?e return
                ?0 ?f ?・ escape ?a backspace return 32 32 32 32 escape
                ?$ ?x ?F 40 ?x ?i backspace return escape ?j ?0])

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭────────────────────────────────╮
  ;; │ Key/Keymap Functions & Aliases │
  ;; ╰────────────────────────────────╯
  ;; to get the definition of a key sequence in a keymap: lookup-key
  ;; to show keymap with which-key:  (which-key--show-keymap keymap-name keymap)
  ;; to show keys sequence that invoked the current command: (this-command-keys)
  (defalias 'alias/key-vector-to-readable-string 'key-description)
  ;; WARNING: key-description is described as an *approximate* inverse to kbd.
  (defalias 'alias/key-readable-string-to-string 'kbd)  ;; or edmacro-parse-keys or read-kbd-macro
  (defalias 'alias/key-input-to-vector 'read-key-sequence-vector)
  (defalias 'alias/key-input-to-string 'read-key-sequence)
  (defun my/key-readable-string-to-vector (keystr) (edmacro-parse-keys keystr t))

  (defun my/read-key-insert-vector ()
    "Read a keystroke and insert as a vector."
    (interactive)
    (cl-prettyprint (read-key-sequence-vector "")))

  (defun my/read-key-insert-readable-string (arg)
    "Read a keystroke and insert as an edmacro-style string.

    If a prefix argument is supplied, the double quotes are omitted."
    (interactive "P")
    (if arg
        (insert (key-description (read-key-sequence-vector "")))
      (insert ?" (key-description (read-key-sequence-vector "")) ?")))

  (defun my/read-key-insert-string (arg)
    "Read a keystroke and insert as a string.

    If a prefix argument is supplied, the double quotes are omitted."
    (interactive "P")
    (if arg
        (insert (read-key-sequence ""))
      (insert ?" (read-key-sequence "") ?")))

  (defun my/lookup-key-interactive (keymap key)
    (interactive
     (list
      (read-string "Enter keymap: ")
      (read-key-sequence "Press key: " nil t)))
    (let* ((cmd (lookup-key (evalstr keymap) key)))
      (message "%s" (trim-multiline-string (string-prettyprint cmd)))))

  (defun my/which-key-show-current-state-map ()
    (interactive)
    (let ((current-state-map (format "evil-%s-state-map" evil-state)))
      (my/which-key-show (intern current-state-map))))

  (defun my/which-key-show (map)
    "Display the keymap MAP in a which-key pop-up."
    (interactive "SKeymap: ")
    (which-key--show-keymap (symbol-name map) (eval map)))

  ;; TODO: work out what 8-digit integers represent in a keymap.
  ;; Currently, they're left untouched.
  (defun my/replace-ints-with-char (beg end)
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
  (defun my/prettyprint-keymap (kmap)
    (interactive "SKeymap: ")
    (set-mark-command)
    (cl-prettyprint (eval kmap))
    (evil-active-region 1)
    (my/replace-ints-with-char))

  (defun my/get-binding (cmd)
    (interactive "SCommand name: ")
    (let* ((cmdname       (symbol-name cmd))
           (cmdname-escd  (format "\\[%s]" cmdname))
           (cmdkey        (substitute-command-keys cmdname-escd))
           (cmdcons       (cons cmdname cmdkey)))
      (message "%S" cmdcons)
      cmdcons))

  (defun my/which-key-show-keymap-at-point (sym)
    (interactive (list (symbol-at-point)))
    (let ((kmap (cond
                 ((keymapp sym)        sym)
                 ((keymapp (eval sym))  (eval sym))
                 (t                     nil))))
      (my/which-key-show kmap)))

  (defun my/parent-keymap-at-point (sym)
    (interactive (list (symbol-at-point)))
    (let ((kmap (cond
                 ((keymapp sym)        sym)
                 ((keymapp (eval sym))  (eval sym))
                 (t                     nil))))
      (keymap-parent kmap)))

  (defun my/read-kbd-event (start &optional end)
    "Read a string or the region as an event sequence.

The string should represent a sequence of keys in `edmacro-mode' format.
Interactively, acts on the region. Called from lisp, START may be a string."
    (interactive "r")
    (if (stringp start)
        (listify-key-sequence (edmacro-parse-keys start end))
      (listify-key-sequence (edmacro-parse-keys (buffer-substring start end)))))

  (defun my/simulate-keypress (keys)
    (interactive "sKeys: ")
    "Simulate a key press or key sequence.

Keys are specified using `edmacro-mode' key syntax.
Note: when the key sequence represents a completed action, `execute-kbd-macro'
may be used instead, eg.  (execute-kbd-macro (kbd "C-x o")).

Example: to enter the help prefix and await another keypress...
    (my/simulate-keypress "C-h")"
    (setq prefix-arg current-prefix-arg)
    (setq unread-command-events (my/read-kbd-event keys)))

  (defun my/define-simulated-keypress (keys)
    "Return a command executing a simulated keypress of KEY.

KEY is specified in `edmacro-mode' format."
    `(lambda ()
       (interactive)
       (setq prefix-arg current-prefix-arg)
       (setq unread-command-events (my/read-kbd-event ,keys))))

  (defun my/key-to-edmacro-format (key)
    "Converts a key to edmacro format (eg  -> C-x C-a).
The key should be entered using quoted-insert, or entered interactively.
See `edmacro-format-keys'."
    (interactive "kKey: ")
    (edmacro-format-keys key))

  ;; adapted from emacs source. GPL3.
  (defun my/read-key-sequence-and-related ()
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

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────╮
  ;; │ Aliases │
  ;; ╰─────────╯

  (defalias 'init 'spacemacs/find-dotfile)
  (defalias 'pr 'cl-prettyprint)
  (defalias 'reyas 'yas/reload-all)
  (defalias 'arv 'auto-revert-mode)
  (defalias 'revb 'revert-buffer)
  (defalias 'diffb 'diff-buffer-with-file)
  (defalias 'sim 'set-input-method)  ;; bound to C-x RET C-\
  (defalias 'repl 'ielm)
  (defalias 'lim 'lisp-interaction-mode)
  (defalias 'el 'emacs-lisp-mode)
  (defalias 'vll 'visual-line-mode)
  (defalias 'acoff 'auto-complete-mode-off)
  (defalias 'ali 'my/quick-pcre-align-repeat)
  ;; aliases for discoverability
  (defalias 'alias/unset 'makunbound)
  (defalias 'alias/unfset 'fmakunbound)
  (defalias 'alias/undefun 'fmakunbound)
  (defalias 'alias/string-to-symbol 'intern)
  (defalias 'alias/symbol-to-string 'symbol-name)
  (defalias 'alias/copy-string-as-kill 'kill-new)
  (defalias 'alias/match-regexp-at-point 'looking-at)
  ;; aliases to user-defined functions
  (defalias 'chmodx 'make-executable)
  (defalias 'ppm 'message-prettyprint)
  (defalias 'boxcom 'box-heading-comment)
  (defalias 'reccom 'rect-heading-comment)
  (defalias 'sreccom 'short-rect-heading-comment)
  (defalias 'ppp 'insert-pp)

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────╮
  ;; │ Functions │
  ;; ╰───────────╯

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

  (defun browse-buffer-file-firefox ()
    (interactive)
    (browse-url-firefox
     (replace-regexp-in-string
      " "
      "%20"
      (concat "file://"
              (expand-file-name (or buffer-file-name default-directory))))))

  (setq browse-url-firefox-program "firefox")

  (defun browse-buffer-file-with-external-application ()
    (interactive)
    (browse-url-xdg-open buffer-file-name))

  (defun my/browse-url-at-point (&optional point)
    (interactive)
    (let ((url (url-get-url-at-point point)))
      (browse-url-firefox url)))

  (defun my/find-file-or-browse-url-at-point ()
    (interactive)
    (let ((url (or (markdown-link-url) (url-get-url-at-point))))
      (if url
          (browse-url-firefox url)
        (find-file-at-point))))


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
    (let ((val (eval (elisp--preceding-sexp))))
      (kill-sexp -1)
      (if current-prefix-arg (insert (format "%S" val))
        (insert (replace-regexp-in-string "\n\\'" "" (pp-to-string val))))))

  (defun my/eval-yank-last-sexp ()
    "Copy the preceding sexp to the kill-ring."
    (interactive)
    (kill-new (format "%S" (eval (elisp--preceding-sexp)))))

  (defun my/evil-normal-or-visual-state? ()
    (and (boundp 'evil-state)
         (memq evil-state
               '(normal visual))))

  (defun my/toggle-evilified-state ()
    (interactive)
    (if (eq evil-state 'evilifed)
        (evil-normal-state)
      (evil-evilified-state)))

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

  (defun my/short-rect-heading-comment-to-unibox ()
    (interactive)
    (let ((beg (if (region-active-p) (region-beginning) (line-beginning-position)))
          (end (if (region-active-p) (region-end) (line-end-position))))
      (my/evil-substitute beg end "\\|"  "│" '(?g))
      (my/evil-substitute beg end  ",-" "╭─" '(?g))
      (my/evil-substitute beg end  "'-" "╰─" '(?g))
      (my/evil-substitute beg end  "-," "─╮" '(?g))
      (my/evil-substitute beg end  "-'" "─╯" '(?g))
      (my/evil-substitute beg end   "-"  "─" '(?g))))

  (defalias 'move-visible-beginning-of-line 'back-to-indentation
    "Move to the first non-whitespace character on the line (or the end of line if
 no non-whitespace)")

  (defun move-visible-end-of-line ()
    "Move to the last non-whitespace character on the line (or the start of line if
 no non-whitespace)"
    (interactive)
    (end-of-line)
    (re-search-backward "[^ \t\n]" (line-beginning-position) 1))

  (defun my/split-line-and-open-line-above ()
    (interactive)
    (comment-indent-new-line)
    (evil-open-above 1))

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
        (evil-forward-word-begin))
      (goto-char end)))

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

  (defvar my/blank-line-regexp "^[[:space:]]*$")
  (defvar my/blank-line-no-whitespace-regexp "^$")

  (defun my/line-at-point-string ()
    "Return the line around point as a string.
Similar to (thing-at-point \'line t) except it does not return a trailing newline.
See also `thing-at-point'"
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (buffer-substring-no-properties beg end)))

  (defun my/line-at-point-blank-p ()
    "Returns a non-nil value if the current line contains only whitespace."
    (string-match-p my/blank-line-regexp (my/line-at-point-string)))
  (defun my/line-above-blank-p (&optional n)
    (save-excursion
      (forward-line (- (or n 1)))
      (my/line-at-point-blank-p)))
  (defun my/line-below-blank-p (&optional n)
    (save-excursion
      (forward-line (or n 1))
      (my/line-at-point-blank-p)))
  (defun my/adjacent-line-blank-p (&optional n)
    "Returns a non-nil value if the Nth line above and/or below point contains
only whitespace).
See `my/line-at-point-blank-p', `my/line-above-blank-p', `my/line-below-blank-p'"
    (or (my/line-above-blank-p n)
        (my/line-below-blank-p n)))

  (defun my/copy-current-line ()
    (interactive)
    (when (my/line-visible-end-position)
      (kill-ring-save (my/line-visible-beginning-position) (my/line-visible-end-position))))

  (defun my/replace-line (count)
    (interactive "p")
    (let ((s (pop kill-ring)))
      (kill-whole-line count)
      (kill-new s)
      (evil-paste-before)))

  ;; ==============================
  ;; functions dealing with columns
  ;; ==============================

  (defun my/wrap-lines-in-region (beg end)
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
  (defun my/wrap-region-or-comment (beg end)
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

  ;; redefine | to use a default of `fill-column' rather than 0.
  (evil-define-motion my/evil-goto-column (count)
    "Move point to column COUNT.

Columns are indexed from zero. If COUNT is not supplied, use `fill-column'."
    :type exclusive
    (move-to-column (or count fill-column)))

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

  ;; ======================
  ;; delete duplicate lines
  ;; ======================

  (defun my/delete-duplicate-lines-nonblank
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

  (defun my/just-one-blank-line ()
    (interactive)
    (if (and (my/line-at-point-blank-p)
             (my/adjacent-line-blank-p))
        (delete-blank-lines)))

  (defun my/remove-blank-lines ()
    (interactive)
    (replace-regexp "\n\\([[:space:]]*\n\\)+" "\n\n"))

  (defun my/remove-doubled-blank-lines ()
    (interactive)
    (replace-regexp "\n[[:space:]]*\n\\([[:space:]]*\n\\)+" "\n\n"))

  (defun my/delete-adjacent-repeated-lines ()
    (interactive)
    (destructuring-bind (beg . end) (evil-get-visual-region-or-buffer)
      (delete-duplicate-lines beg end nil t nil t)))

  (defun my/remove-trailing-space-and-blank-lines (&optional beg end)
    (interactive
     (cond ((use-region-p)   (list (region-beginning) (region-end)))
           (:else            (list (point) (point-max)))))
    (delete-trailing-whitespace beg end)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\n+" end t)
        (replace-match "\n" nil nil))))

  (defun my/remove-trailing-space-and-doubled-blank-lines (&optional beg end)
    (interactive
     (cond ((use-region-p)   (list (region-beginning) (region-end)))
           (:else            (list (point) (point-max)))))
    (delete-trailing-whitespace beg end)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\n\n+" end t)
        (replace-match "\n\n" nil nil))))

  (defun my/sudo-edit-this-file ()
    (interactive)
    (let ((f (concat "/sudo::" (expand-file-name buffer-file-name))))
      (find-file f)))

  (defun my/open-file-at-point ()
    "Open the file at point using xdg-open."
    (interactive)
    (shell-command (concat "xdg-open " (ffap-guess-file-name-at-point))))

  (defun my/new-script (name &optional interpreter)
    "Create an empty executable script file in the current directory"
    (interactive "sName:\nsInterpreter:")
    (let ((cmd (concat "echo '#! " (or interpreter "") "' > " name)))
      (shell-command cmd)))

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
  (defun my/evil-shift-left-fine-dispatcher ()
    (interactive)
    (if (eq evil-state 'visual)
        (call-interactively 'evil-visual-shift-left-fine)
      (call-interactively 'evil-shift-left-fine)))
  (defun my/evil-shift-right-fine-dispatcher ()
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

  (defmacro my/after-motion (fn)
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

  (defun my/current-mode-and-state()
    "Display the current `evil-state' and `major-mode' in the minibuffer."
    (interactive)
    (let ((s (format "evil-state: %S || major-mode: %S"
                     evil-state
                     major-mode)))
      (message s)))

  (defun my/show-last-command ()
    (interactive)
    (message (format "%S" last-command)))

  (defun sprint-keymap (map)
    (with-temp-buffer
      (cl-prettyprint map)
      (my/replace-ints-with-char (point-min) (point-max))
      (buffer-string)))

  (defun symbol-or-function-near-point ()
    (or (when (fboundp 'symbol-nearest-point) (symbol-nearest-point))
        (function-called-at-point)))

  (defun my/prettyprint-keymap (map)
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
    (my/quick-pcre-align-repeat beg end " (?:')")
    (evil-indent beg end))

  (defun tsp-align-double-quoted-column (beg end)
    (interactive "r")
    (my/quick-pcre-align-repeat beg end " (?:\")")
    (evil-indent beg end))

  (defun my/quit-window-kill (&optional bury window)
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

  (defun my/kill-buffer-and-window-quit-help ()
    "Kill the current buffer, close its window, and quit the help buffer.

With a prefix argument, leaves any help buffer open."
    (interactive)
    (kill-buffer-and-window)
    (unless current-prefix-arg
      (let ((helpbuffer (get-buffer-window "*Help*")))
        (when helpbuffer
          (quit-window nil helpbuffer)))))

  (defun my/kill-other-buffer-and-window ()
    "Kill the other buffer and close its window."
    (interactive)
    (save-excursion
      (let ((orig-buffer (current-buffer)))
        (when (> (count-windows) 1)
          (other-window 1 nil)
          (unless (eq orig-buffer (current-buffer))
            (kill-buffer))
          (delete-window)))))

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

  (defun my/quit-help ()
    (interactive)
    (quit-window nil (get-buffer-window "*Help*")))

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

  (defvar my/external-file-manager "/usr/bin/dolphin")

  (defun my/launch-standalone-file-manager ()
    "Launch an external file manager in the current directory.

   With a prefix-argument, launch in spacemacs private directory. The file
manager is determined by the `my/external-file-manager' variable."
    (interactive)
    (let ((dir (replace-regexp-in-string
                "~/" "/home/troy/"
                (if current-prefix-arg
                    (expand-file-name (file-name-as-directory "private")
                                      user-emacs-directory)
                  default-directory))))
      (start-process "terminal" nil my/external-file-manager dir)))

  (defmacro no-helm-limit (&optional forms)
    "Execute FORMS without any `helm-candidate-number-limit' in effect."
    (declare (debug 'body))
    `(let ((helm-candidate-number-limit nil))
       ,forms))

  (defun my/lacarte-execute-menu-command ()
    (interactive)
    (let ((helm-candidate-number-limit nil))
      (lacarte-execute-menu-command '(local minor global))))

  (defun my/lacarte-execute-command ()
    (interactive)
    (let ((helm-candidate-number-limit nil))
      (lacarte-execute-command '(local minor global))))

  (defhydra my/lacarte-menu-execute (global-map "C-x <f10>" :color blue :columns 1)
    "Choose from local minor or global commands"
    ("l" (no-helm-limit (lacarte-execute-menu-command '(local)))  "local")
    ("m" (no-helm-limit (lacarte-execute-menu-command '(minor)))  "minor")
    ("g" (no-helm-limit (lacarte-execute-menu-command '(global))) "global")
    ("a" (no-helm-limit (lacarte-execute-menu-command '(local minor global))) "all")
    ("ESC" nil "abort"))
  (global-set-key (kbd "C-x <f10>") 'my/lacarte-menu-execute/body)

  (defun my/lacarte-execute-local-menu-command ()
    (interactive)
    (no-helm-limit (lacarte-execute-menu-command '(local))))

  (defun my/swap-windows (w1 w2)
    (let ((b1 (window-buffer w1))
          (b2 (window-buffer w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)))

  (defun my/frame-windows (&optional frame)
    (window-list (selected-frame)))

  (defun my/frame-parameter-names (&optional frame)
    (let ((frame (or frame (selected-frame))))
      (mapcar #'car (frame-parameters frame))))

  (defun my/window-swap-with-next ()
    (interactive)
    (let* ((ws (my/frame-windows))
           (w1 (selected-window))
           (w2 (if (equal (car ws) w1)
                   (cadr ws)
                 (car ws))))
      (message "(my/swap-windows %S %S)" w1 w2)
      (my/swap-windows w1 w2)))

  (defun my/bury-buffer-and-delete-window ()
    (interactive)
    (bury-buffer)
    (delete-window))

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

  (defun my/insert-space (&rest arg)
    (interactive "p")
    (insert-char ?\  (car arg)))

  (defun my/insert-space-after (&rest arg)
    (interactive "p")
    (insert-char ?\  (car arg))
    (backward-char (car arg)))

  (defun my/insert-spaces-around-point (n)
    (interactive (list (or current-prefix-arg 1)))
    (insert (make-string (* 2 n) ? ))
    (backward-char n)
    (evil-insert-state))

  (defun my/install-bb-spacemacs-layers ()
    (interactive)
    (shell-command "cd ~/.emacs.d/private/layer-groups; git clone https://github.com/TheBB/spacemacs-layers.git bb-spacemacs-layers"))

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

  ;; FIXME: always converts tabs->spaces, ignoring untabify
  (defun my/evil-convert-kill-to-block(&optional untabify)
    "Convert the latest selection to an evil block selection and push on kill-ring.

If UNTABIFY is non-nil, then tabs are replaced by spaces before conversion."
    (interactive)
    (let (lns)
      (with-temp-buffer
        (yank)
        ;; remove trailing newline
        (when (= (line-end-position) (line-beginning-position))
          (delete-backward-char 1))
        (when untabify (untabify (point-min) (point-max)))
        (beginning-of-buffer)
        (do ((n 1)) ((> n (evil-ex-last-line)))
          (push (my/line-length n) lns)
          (incf n))
        (end-of-buffer)
        (let ((longest-length (apply #'max lns))
              (last-length    (car lns))
              (space          32))
          (insert
           (make-string (- longest-length last-length) space))
          (evil-yank (point-min) (point-max) evil-visual-block)))))

  (defun my/evil-paste-after-as-block ()
    "Paste the latest selection after point after converting to a block.

If UNTABIFY is non-nil (or if no argument is given interactively), then tabs
are replaced by spaces before conversion."
    (interactive)
    (my/evil-convert-kill-to-block t)
    (call-interactively #'evil-paste-after))

  (defun my/evil-paste-before-as-block ()
    "Paste the latest selection before point after converting to a block.

If UNTABIFY is non-nil (or if no argument is given interactively), then tabs
are replaced by spaces before conversion."
    (interactive)
    (my/evil-convert-kill-to-block t)
    (call-interactively #'evil-paste-before))

  (defun my/number-of-lines-in-kill ()
    "Return the number of lines in the latest selection."
    (with-temp-buffer
      (yank)
      (evil-ex-last-line)))

  (defun my/evil-make-column-kill-height (char)
    "Create an evil visual-block out of a column of CHAR and push on kill-ring.

The height of the column is the same as that of the latest selection."
    (with-temp-buffer
      (insert
       (s-join "\n"
               (-repeat (my/number-of-lines-in-kill) (list char))))
      (evil-yank (point-min) (point-max) evil-visual-block)))
  (defun my/evil-paste-after-column-kill-height (char)
    "Paste a column of CHAR after point.

The height of the column is the same as that of the latest selection."
    (interactive "c")
    (my/evil-make-column-kill-height char)
    (evil-paste-after 1))
  (defun my/evil-paste-before-column-kill-height (char)
    "Paste a column of CHAR before point.

The height of the column is the same as that of the latest selection."
    (interactive "c")
    (my/evil-make-column-kill-height char)
    (evil-paste-before 1))
  (defun my/evil-select-column-kill-height ()
    "Select a column under point as an evil-block.

The height of the column is the same as that of the latest selection."
    (interactive)
    (evil-save-mark)
    (let ((beg (point))
          (end (my/resulting-position
                (evil-next-line (1- (my/number-of-lines-in-kill))))))
      (evil-visual-make-region beg end)
      (evil-visual-block)))

  (defmacro my/resulting-position (&rest operations)
    "Return the position that results after OPERATIONS are performed.

All changes are reverted."
    `(save-mark-and-excursion
      ,@operations
      (point)))

  (defun my/list-processes (&optional query-only buffer)
    (interactive)
    (list-processes query-only buffer)
    (select-window (display-buffer "*Process List*")))

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

  (my/define-command-and-center spacemacs/jump-to-definition)
  (my/define-command-and-center push-button)
  (my/define-command-and-center magit-blame)
  (my/define-evil-motion-and-center evil-ex-search-previous)
  (my/define-evil-motion-and-center evil-ex-search-next)

  (defun my/mouse-toggle-fold (ev)
    (interactive "e")
    (save-excursion
      (goto-char (posn-point (event-start ev)))
      (evil-toggle-fold)))

  (defun my/cycle-evil-visual-mark-mode ()
    (interactive)
    (spacemacs/toggle-evil-visual-mark-mode)
    (spacemacs/toggle-evil-visual-mark-mode))

  (defun my/remove-blank-lines (beg end)
    (interactive "r")
    (unless (region-active-p)
      (setq beg (point-min)
            end (point-max)))
    (flush-lines (pcre-to-elisp "^\\s*$") beg end))

  (defun my/perl-process-lines (cmdstr)
    (interactive "sPerl Command:")
    (my/shell-command-replace-region
     (if (region-active-p) (region-beginning) (point-min))
     (if (region-active-p) (region-end) (point-max))
     (format "perl -pe '%s'" cmdstr)))

  (defun my/text-scale-increase-under-mouse (x y &optional frame inc)
    "Increase the height of the default face in the buffer under the mouse."
    (interactive
     (let* ((mousepos (mouse-position))
            (frame    (car mousepos))
            (coords   (cdr mousepos))
            (x        (car coords))
            (y        (cdr coords)))
       (list x y frame 1)))
    (select-window (window-at x y frame))
    (text-scale-increase 1))

  (defun my/text-scale-decrease-under-mouse (x y &optional frame inc)
    (interactive
     (let* ((mousepos (mouse-position))
            (frame    (car mousepos))
            (coords   (cdr mousepos))
            (x        (car coords))
            (y        (cdr coords)))
       (list x y frame 1)))
    (select-window (window-at x y frame))
    (text-scale-decrease 1))

  (defun my/describe-function-string (func)
    (with-temp-buffer
      (help-mode)
      (read-only-mode 0)
      (insert (format "%S is " func))
      (let ((standard-output (current-buffer)))
        (describe-function-1 func))
      (buffer-string)))

  (defun my/describe-function-in-new-buffer (func)
    (interactive
     (list
      (completing-read "Function: " obarray 'fboundp t nil nil
                       (symbol-name (symbol-nearest-point)))))
    (switch-to-buffer (generate-new-buffer (format "*Documentation: %S*" func)))
    (insert (my/describe-function-string (symbol-function (intern func)))))

  (defun my/pos-tip-describe-function (func)
    (interactive
     (list
      (completing-read "Function: " obarray 'fboundp t
                       nil nil (symbol-name (symbol-nearest-point)))))
    (pos-tip-show (my/describe-function-string (symbol-function (intern func)))
                  nil nil nil -1))

  (defun my/evil-force-normal-state-and-cancel ()
    (interactive)
    (evil-force-normal-state)
    (if (fboundp 'pos-tip-hide) (pos-tip-hide)))

  (defun my/evil-normal-state-and-cancel ()
    (interactive)
    (evil-normal-state)
    (if (fboundp 'pos-tip-hide) (pos-tip-hide)))

  (defun my/faces-list-fg-bg-colors ()
    (interactive)
    (let ((new-buffer (generate-new-buffer "*Faces Colors*")))
      ;; (set-window-buffer nil new-buffer)
      (switch-to-buffer new-buffer)
      (mapcar (lambda (face)
                (let* ((fg (face-foreground face nil t))
                       (bg (face-background face nil t))
                       (fghex (if (stringp fg)
                                  (downcase (hexrgb-color-name-to-hex fg 2))
                                ""))
                       (bghex (if (stringp bg)
                                  (downcase (hexrgb-color-name-to-hex bg 2))
                                "")))
                  (insert (format "%S\t|\t%s\t|\t%s\n" face fghex bghex))))
              (face-list))
      (mark-whole-buffer)
      (spacemacs/align-repeat-bar (region-beginning) (region-end) nil)))

  (defmacro my/with-string-as-temp-buffer (s &rest code)
    "Run CODE on string S in a temp buffer and return the resulting contents
as a string."
    `(with-temp-buffer
       (insert ,s)
       (beginning-of-buffer)
       ,@code
       (buffer-string)))

  (defun my/substring-at-point (n)
    (buffer-substring (point) (+ (point) n)))

  (defun my/beginning-of-defun ()
    "Move point to the start of the enclosing definition.

Recognizes `defun', `defalias', `defmacro', `defvar', `defconst', `defmethod',
`defstruct', `defface' and `defmath'."
    (interactive)
    (while
        (and (not
              (looking-at-p
               "(def\\(un\\|alias\\|macro\\|var\\|const\\|method\\|struct\\|face\\|math\\)\\b"))
             (sp-backward-up-sexp))))

  (defun my/end-of-defun (&optional after)
    "Move point to the end of the enclosing definition.

Recognizes `defun', `defalias', `defmacro', `defvar', `defconst', `defmethod',
`defstruct', `defface' and `defmath'."
    (interactive)
    (my/beginning-of-defun)
    (sp-forward-sexp))

  (evil-define-motion my/evil-next-paragraph-beginning (count)
    "Move to the end of the COUNT-th next paragraph."
    :jump t
    :type exclusive
    (evil-signal-at-bob-or-eob count)
    (evil-forward-beginning 'evil-paragraph count)
    (evil-first-non-blank))

  (evil-define-motion my/evil-previous-paragraph-beginning (count)
    "Move to the end of the COUNT-th paragraph before point."
    :jump t
    :type exclusive
    (evil-signal-at-bob-or-eob count)
    (evil-backward-beginning 'evil-paragraph count)
    (evil-first-non-blank))

  (evil-define-motion my/evil-beginning-of-line-or-first-non-blank ()
    "Move to the the first non-blank character or the beginning of the line."
    :type exclusive
    (let ((start (point)))
      (evil-narrow-to-line (back-to-indentation))
      (when (= (point) start)
        (move-beginning-of-line nil))))

  (defun my/pick-color ()
    (interactive)
    (if current-prefix-arg
        (kill-new (s-trim (shell-command-to-string "grabc 1>/dev/null")))
      (kill-new (s-trim (shell-command-to-string "grabc 2>/dev/null")))))

  (defun my/pick-insert-color ()
    (interactive)
    (if current-prefix-arg
        (insert (s-trim (shell-command-to-string "grabc 1>/dev/null")))
      (insert (s-trim (shell-command-to-string "grabc 2>/dev/null")))))

  (defun my/copy-to-empty-buffer ()
    "Copy the current buffer's contents to an empty buffer and open mode dialog."
    (interactive)
    (let ((contents (buffer-string)))
      (spacemacs/new-empty-buffer)
      (insert contents)
      (helm-switch-major-mode)))

  ;; ╭──────────────────────────╮
  ;; │ Browser/Server Functions │
  ;; ╰──────────────────────────╯

  (defun my/serve-buffer-file ()
    (interactive)
    (elnode-make-webserver buffer-file-name 8009))

  (defun my/serve-buffer-directory ()
    (interactive)
    (my/async-shell-command-no-window "python2 -m SimpleHTTPServer 8009"))

  (defun my/browse-buffer-file-firefox ()
    (interactive)
    (my/serve-buffer-directory)
    (let ((name (file-name-nondirectory (buffer-file-name))))
      (browse-url-firefox (concat "localhost:8009/" name))))

  (defun my/browse-buffer-directory-firefox ()
    (interactive)
    (my/serve-buffer-directory)
    (browse-url-firefox "localhost:8009"))

  ;; ╭──────────────────────╮
  ;; │ Yank/Paste Functions │
  ;; ╰──────────────────────╯

  (defun my/kill-new-and-message (string &optional replace)
    (kill-new string replace)
    (message string))

  (defmacro my/define-yank-cmd (name expr &optional doc)
    (declare (indent 2))
    (let ((fnname (intern (concat "my/yank-" (symbol-name name)))))
      `(defun ,fnname (arg)
         ,doc
         (interactive "P")
         (let ((str (if arg (my/string-double-quote ,expr) ,expr)))
           (kill-new str)
           (message str)))))

  (defun my/yank-to-end-of-line (n)
    (interactive "p")
    (evil-yank (point) (line-end-position n)))

  ;; TODO: WIP - instead of quoting, universal argument is passed into expression
  (defmacro my/define-yank-cmd-pass-argument (name expr &optional interactive-code doc)
    "Define a command NAME which evaluates EXPR and yanks the value.

EXPR may contain the anaphoric variable N containing the universal argument.
INTERACTIVE-CODE describes how N is obtained. By default, the interactive code P is used
- nil if no argument, otherwise the numeric value. DOC is an optional documentation string."
    nil)

  ;; TODO: WIP
  (defmacro my/define-yank-at-point-cmd  (name expr &optional doc)
    nil)


  (my/define-yank-cmd filename (file-name-nondirectory (buffer-file-name)) "Yank name of current file")
  (my/define-yank-cmd path (buffer-file-name) "Yank full path of current file")
  (my/define-yank-cmd directory default-directory "Yank default directory")
  (my/define-yank-cmd sexp-at-point (sexp-at-point) "Yank SEXP at point")
  (my/define-yank-cmd word-at-point (word-at-point) "Yank word at point")

  ;; TODO: redefine using my/define-yank-cmd-pass-argument so universal arg -> signed
  (my/define-yank-cmd
      buffer-initial-integers
      (my/get-buffer-initial-integers)
    "Yank a space-separated list of the integers beginning each line")
  (my/define-yank-cmd
      buffer-first-integers
      (my/get-buffer-first-integers)
    "Yank a space-separated list of the first integers occurring on each line")

  (defun my/get-buffer-initial-integers (&optional signed)
    (let* ((lines (s-split "[\n]" (buffer-string)))
           (regex (if signed "^ *\\([-+]?[0-9]*\\).*"
                    "^ *\\([0-9]*\\).*"))
           (lines_ (-map (lambda (s) (s-replace-regexp regex "\\1" s)) lines))
           (s (s-join " " lines_)))
      s))

  (defun my/get-buffer-first-integers (&optional signed)
    (let* ((lines (s-split "[\n]" (buffer-string)))
           (regex (if signed "^\\(?:[^0-9]*[^0-9+-]\\)\\([-+]?[0-9]*\\).*"
                    "^[^0-9]*\\([0-9]*\\).*"))
           (lines_ (-map (lambda (s) (s-replace-regexp regex "\\1" s)) lines))
           (s (s-join " " lines_)))
      s))

  (defun my/avy-yank-inner-quotes ()
    (interactive)
    (avy-goto-char 34)
    (my/kmacro-call 'my/yank-inside-double-quotes))

  ;; TODO: improve
  (defun my/paste-multi (n)
    (interactive "p")
    (do ((i 1))
        ((>= i n))
      (yank i)
      (incf i)))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭─────────────╮
  ;; │ Minor Modes │
  ;; ╰─────────────╯

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

  ;; adapted from link-hint.el (Fox Kiester, GPL3)
  (defun my/link-hint-kill-append-link-at-point ()
    "Append a link of any supported type at point to the latest kill-ring entry.
See the default value of `link-hint-copy-ignore-types' for the unsupported
types."
    (interactive)
    (list
     nil
     "Copied"
     (link-hint--types-at-point-let-wrapper
      (cond (shr-url (kill-append (concat shr-url "\n") nil))
            (org-link (kill-append (concat (plist-get org-link :uri) "\n") nil))
            (text-url (kill-append (concat text-url "\n") nil))
            (file-link (kill-append (concat (ffap-guess-file-name-at-point) "\n") nil))
            (mu4e-url (kill-append (concat mu4e-url "\n") nil))
            (t (message "There is no supported link at the point."))))))

  ;; adapted from link-hint.el (Fox Kiester, GPL3)
  (defun my/link-hint-copy-all-links ()
    "Copy all visible links of a supported type.
See `link-hint-copy-link' for more information."
    (interactive)
    (let ((link-hint-ignore-types (append link-hint-ignore-types
                                          link-hint-copy-ignore-types)))
      (kill-new "")
      (link-hint--all-links-action #'my/link-hint-kill-append-link-at-point)))

  (evil-define-operator my/center-in-whitespace (beg end)
    "Center the text in [BEG, END] in the surrounding whitespace."
    :move-point nil
    (interactive "<r>")
    (let* ((l-ws-end (save-excursion
                       (goto-char beg)
                       (search-backward " " (line-beginning-position) t)
                       (point)))
           (l-ws-beg (save-excursion
                       (goto-char beg)
                       (search-backward " " (line-beginning-position) t)
                       (search-backward-regexp "[^ ]" (line-beginning-position) t)
                       (1+ (point))))
           (r-ws-beg (save-excursion
                       (goto-char end)
                       (search-forward " " (line-end-position) t)
                       (point)))
           (r-ws-end (save-excursion
                       (goto-char end)
                       (search-forward " " (line-end-position) t)
                       (search-forward-regexp "[^ ]" (line-end-position) t)
                       (1- (point))))
           (l-ws-length (1+ (- l-ws-end l-ws-beg)))
           (r-ws-length (1+ (- r-ws-end r-ws-beg)))
           (ws-length-diff (- r-ws-end r-ws-beg))
           (ws-length-adjust (/ ws-length-diff 2))
           )
      (cond
        ((> ws-length-diff 1)
         (delete-region r-ws-beg (+ r-ws-beg ws-length-adjust))
         (goto-char l-ws-beg)
         (insert (make-string ws-length-adjust ? )))
        ((< ws-length-diff -1)
         (delete-region l-ws-beg (- l-ws-beg ws-length-adjust))
         (goto-char r-ws-beg)
         (insert (make-string ws-length-adjust ? ))))))

  (evil-define-command my/foreach-line (cmd beg end &optional type ignore-short-lines)
    "Apply a command or lisp expression to each line of region.

The user is queried for a command which is then applied to each line of the
region. The begins in the goal column or the end of the line if it is too short.a
The goal column is determined by point for character and block selections, or
the start of the line for line selections.

If IGNORE-SHORT-LINES is non-nil, lines which do not reach the goal column are
omitted."
    :move-point nil
    (interactive
     (let ((c (intern
               (completing-read "Command: "
                                obarray
                                (lambda (s) (or (string-empty-p s) (commandp s)))
                                t)))
           (selection (evil-visual-range)))
       (list
        (if (eq c '##)  ;; result of interning ""
            (setq c (read--expression "Expression: "))
          c)
        (nth 0 selection)
        (nth 1 selection)
        (nth 2 selection)
        current-prefix-arg)))
    (save-mark-and-excursion
      (goto-char beg)
      (let ((goal-col (current-column))
            (end-row  (line-number-at-pos end))
            (end-col  (save-excursion (goto-char end) (current-column)))
            (row      (string-to-number (format-mode-line "%l"))))
        (unless (commandp cmd) (setq cmd `(lambda () (interactive ,cmd))))
        (save-mark-and-excursion
          (while (< row end-row)
            (evil-goto-column goal-col)
            (unless (and ignore-short-lines (< (current-column) goal-col))
              (call-interactively cmd))
            (next-line)
            (setq row (string-to-number (format-mode-line "%l"))))
          (when (or (eq type 'block)
                    (and (eq type 'inclusive)
                         (< (current-column) end-col)))
            (evil-goto-column goal-col)
            (unless (and ignore-short-lines (< (current-column) goal-col))
              (call-interactively cmd)))))))

  (defun my/read-function-or-expression-as-command ()
    "Read a command, function or lisp expression and return as a command.

If an atom is entered, returned it if the name of a command, or wrap it in
an interactive lambda if it is a non-interactive function. If a list is
entered, return a command which executes it."
    (interactive)
    (let ((expr (read--expression "Expression: ")))
      (cond
        ((and (symbolp expr) (commandp expr))   expr)
        ((and (symbolp expr) (functionp expr))  `(lambda () (interactive) (,expr)))
        ((listp expr)                           `(lambda () (interactive) ,expr)))))

  (defun my/current-line-number ()
    (string-to-number (format-mode-line "%l")))

  (defun my/string-double-quote (s) (concat "\"" s "\""))

  (defun my/delete-inside-double-quotes ()
    "Delete contents of double quotes."
    (interactive)
    (let* ((quote-regexp (pcre-to-elisp (concat "(^|[^\\\\])" (string 34))))
           (beg          (save-excursion
                           (search-backward-regexp quote-regexp)
                           (if (= (point) (line-beginning-position))
                               (+ (point) 1)
                             (+ (point) 2))))
           (end          (save-excursion
                           (search-forward-regexp quote-regexp)
                             (- (point) 1))))
      (kill-region beg end)))

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

  (defun my/cvim-source-dir()
    (interactive)
    (dired "/home/troy/repos/chromium-vim/")
    (dired-insert-subdir "/home/troy/repos/chromium-vim/background_scripts/")
    (dired-insert-subdir "/home/troy/repos/chromium-vim/content_scripts")
    (dired-insert-subdir "/home/troy/repos/chromium-vim/cvimrc_parser")
    (dired-insert-subdir "/home/troy/repos/chromium-vim/pages")
    (dired-insert-subdir "/home/troy/repos/chromium-vim/scripts")
    (beginning-of-buffer)
    (forward-paragraph)
    (diredp-next-line 2)
    (dired-hide-details-mode 1)
    (setq-local my/dired-reuse-buffer nil))

  (defun my/scripts-dir()
    (interactive)
    (dired "/home/troy/.scripts/")
    (let ((subdirs (directory-files "~/.scripts/script-directories" t "^\[^.\]")))
         (dolist (dir subdirs)
           (dired-insert-subdir dir)))
    (beginning-of-buffer)
    (forward-paragraph)
    (diredp-next-line 2)
    (dired-hide-details-mode 1)
    (setq-local my/dired-reuse-buffer nil))

  (defun my/rgrep (pattern &optional dir)
    (interactive "sPattern: \nsFiles/Base directories [default: .]: ")
    (when (string-empty-p (or dir "")) (setq dir "."))
    (let ((cmd (concat "grep -nrP --exclude-dir .git " pattern " " dir)))
      ;; (message cmd)
      ;; (async-shell-command cmd)
      (let* ((s (shell-command-to-string cmd))
             (buf (generate-new-buffer "*Grep results*"))
             (win (popwin:popup-buffer buf)))
        (select-window win)
        (insert s)
        (grep-mode)
        (beginning-of-buffer))))

  (defun my/rgrep-spacemacs (pattern)
    (interactive "spattern: ")
    (my/rgrep pattern spacemacs-start-directory))

  (defun my/y-or-n-p (&optional prompt)
    (let ((response (read-from-minibuffer (or (concat prompt " ") "y or n? "))))
      (cond ((member response (list "yes" "y" " ")) t)
            ((member response (list "no" "n" "")) nil)
            (t (my/y-or-n-p prompt)))))

  (defun my/sort-and-uniquify-lines ()
    (interactive)
    (call-interactively 'sort-lines)
    (evil-visual-refresh)
    (call-interactively 'spacemacs/uniquify-lines))

  (defun my/open-my-fork (&optional sourcebase forkbase)
    (interactive)
    (let* ((home         (getenv "HOME"))
           (sourcebase   (or sourcebase (format "%s/source/git-repos/" home)))
           (forkbase     (or forkbase (format "%s/repos/" home)))
           (sourcefile   buffer-file-name)
           (sourcefile-p (string-match sourcebase sourcefile))
           (forkfile     (replace-regexp-in-string sourcebase forkbase sourcefile))
           (line         (line-number-at-pos (point))))
      (when sourcefile-p
       (unless current-prefix-arg (kill-buffer (current-buffer)))
       (find-file forkfile)
       (goto-line line)
       (recenter-top-bottom '(4)))))

  (defun my/fork-jump (linum &optional sourcebase forkbase)
    (interactive
     (list (or current-prefix-arg (read-number "Line number: "))))
    (let* ((curlinepos (save-excursion (beginning-of-line) (point)))
           (home       (getenv "HOME"))
           (sourcebase (or sourcebase (format "%s/source/git-repos/" home)))
           (forkbase   (or forkbase (format "%s/repos/" home)))
           (forkfile   buffer-file-name)
           (canonfile  (replace-regexp-in-string forkbase sourcebase forkfile))
           (line       (s-trim
                        (shell-command-to-string
                         (format "tail -n+%d %s | head -n1" linum canonfile))))
           (jumppos    (save-excursion
                         (beginning-of-buffer)
                         (search-forward line)
                         (beginning-of-line)
                         (point))))
      (when (/= jumppos curlinepos)
        (goto-char jumppos))))

  (defun printvars (vars)
    (let* ((varnames (mapcar 'symbol-name vars))
           (varvalues (mapcar 'symbol-value vars))
           (pairs (-zip-pair varnames varvalues))
           (strs (mapcar (fn (format "%s:\t%S" (car <>) (cdr <>))) pairs)))
      (s-join "\n" strs)))
  (defmacro showvars (&rest vars)
    `(message "%s" (printvars ',vars)))

  (defun my/newline-indent-insert-fill-prefix (arg)
    (interactive "P")
    (newline)
    (let ((n (cond ((eq current-prefix-arg '(4)) (* 2 tab-width))
                   ((eq current-prefix-arg '-)   1)
                   (t                            tab-width)))
          (leading-spaces (save-excursion (back-to-indentation) (current-column))))
      (delete-char leading-spaces)
      (insert (make-string n ? ))
      (save-excursion
        (beginning-of-line)
        (insert fill-prefix))))

  (defun my/keyboard-escape-quit-and-clear-highlight()
    (interactive)
    (keyboard-escape-quit)
    (spacemacs/evil-search-clear-highlight))

  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────────────╮
  ;; │ Temporary Workarounds │
  ;; ╰───────────────────────╯
  (defun workarounds()
    (interactive)
    (when (file-readable-p "~/.emacs.d/private/local/workarounds.el")
      (load "~/.emacs.d/private/local/workarounds.el")))

  (when (string< emacs-version "26.1")
    (spacemacs/set-leader-keys "t n" 'linum-mode))

  (when (file-readable-p "~/.emacs.d/private/private-data.el")
    (load "~/.emacs.d/private/private-data.el"))
  ;; ───────────────────────────────────────────────────────────────────────────────
  ;; ╭───────────────────╮
  ;; │ Load Private Data │
  ;; ╰───────────────────╯
)
