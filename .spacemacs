;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
(defconst MIMAS-NAME "Mimas.local")
(defconst JUPITER-NAME "jupiter")
(defconst is-mimas (string-equal (system-name) MIMAS-NAME))
(defconst is-jupiter (string-equal (system-name) JUPITER-NAME))

(defun string-contains-p (needle haystack)
  (string-match-p (regexp-quote needle) haystack))

(defconst is-osx (string-contains-p "darwin" (symbol-name system-type)))
(defconst is-linux (string-contains-p "linux" (symbol-name system-type)))

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

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(python
     csv
     html
     emacs-lisp
     helm
     pdf

     (shell :variables shell-default-shell 'eshell)

     (treemacs :variables treemacs-use-follow-mode nil)
     theming
     themes-megapack

     (osx :variables
          osx-command-as 'meta
          osx-right-command-as 'none
          osx-option-as 'none)

     (lsp :variables
          lsp-navigation 'peek
          lsp-file-watch-threshold 3000 ; watch up to 3000 files
          lsp-clients-clangd-executable "/usr/local/Cellar/llvm/10.0.1_2/bin/clangd"
          lsp-clients-clangd-args '("--background-index" "--clang-tidy"))

     dap
     (auto-completion :variabes
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-use-company-box t
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete)

     (git :variables
          magit-repository-directories '(("~/.emacs.d" . 0))
          magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

     cmake
     yaml
     (c-c++ :variables
            c-c++-backend 'lsp-clangd
            c-c++-enable-google-style t
            c-c++-enable-clang-format-on-save t
            c-c++-adopt-subprojects t
            c-c++-default-mode-for-headers 'c++-mode)

     (org :variables
          org-capture-templates '(("c" "TODO" entry (file+headline "~/Dropbox/org/inbox.org" "Tasks") "* TODO  %?\n  %i\n")
                                  ("r" "Rezept" entry (file+headline "~/Dropbox/org/recipees.org" "Inbox") "* %?\n  %i\n")
                                  ("k" "Referenze" entry (file+headline "~/Dropbox/org/store.org" "reference") "* %?\n  %i\n")
                                  ("f" "TODO in file" entry (file+headline "~/Dropbox/org/inbox.org" "Tasks") "* TODO  %?\n  %i\n  %a"))
                                        ;org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇")
          org-agenda-files '("~/Dropbox/org/_today.org"
                             "~/Dropbox/org/inbox.org"
                             "~/dev/org/notebook.org")
          org-todo-keywords '(
                              (sequence "STARTED(s)" "TODO(t)" "WAITING(w@/!)" "REVIEW(r)" "BLOCKED(b)" "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c)")
                              (sequence "LEARN" "TRY" "|" "COMPLETE(x)")
                              (sequence "TODELEGATE(-)" "DELEGATED(d)" "|" "COMPLETE(x)"))
          org-want-todo-bindings 't
          org-hide-leading-stars t

          org-hide-emphasis-markers t
          org-todo-keyword-faces
          '(
            ("TODO" . org-todo)
            ("DONE" . (:foreground "gray" :weight bold ))
            ("STARTED" . (:foreground "LightSlateBlue" :weight bold))
            ("REVIEW" . (:foreground "red" :weight bold   ))
            ("BLOCKED" . (:foreground "DarkGoldenrod" :weight bold))
            ("WAITING" . (:foreground "red" :weight bold  ))
            ("SOMEDAY" . (:foreground "gray" :weight bold ))
            ("DELEGATED" . (:foreground "gray" :weight bold ))))

     spacemacs-org
     clojure
     )


   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      apache-mode ;; for htaccess syntax
                                      dockerfile-mode
                                      prism
                                      docker
                                      git-gutter+
                                      bazel-mode
                                      rainbow-mode
                                      transpose-frame
                                      vlf
                                      (evil-adjust :location (recipe :fetcher github :repo "troyp/evil-adjust"))
                                      (helm-icons :location (recipe :fetcher github :repo "yyoncho/helm-icons"))
                                      (tron-legacy-theme :location (recipe :fetcher github :repo "ianpan870102/tron-legacy-emacs-theme"))
                                      (piper :location (recipe :fetcher gitlab :repo "howardabrams/emacs-piper"))
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    evil-escape
                                    evil-exchange
                                    evil-goggles
                                    emmet-mode
                                    )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

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

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default spacemacs-27.1.pdmp)
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

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

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

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

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

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

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil


   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         doom-zenburn
                         doom-snazzy
                         doom-nord
                         spacemacs-dark
                         spacemacs-light
                         doom-gruvbox
                         doom-one
                         doom-one-light
                         tron-legacy
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(doom)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '(
                               "Fantasque Sans Mono"
                               :size 13.0
                               ;; "Source Code Pro"
                               ;; :size 10.0
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

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
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

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
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

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

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))

  (helm-icons-enable)

  (setq org-pretty-entities t)
  (add-to-list 'exec-path "~/.cargo/bin")
  (use-package org-tempo :after org)
  (setq projectile-enable-caching t)

  (setq c-basic-offset 4)

  (require 'evil-adjust)
  (evil-adjust)

  (use-package piper
    :config
    (spacemacs/declare-prefix "o |" "piper")
    (spacemacs/set-leader-keys
      "|"     '("piper-ui"        . piper-user-interface)
      "o | |" '("piper-locally"   . piper)
      "o | d" '("other-directory" . piper-other)
      "o | r" '("piper-remotely"  . piper-remote)))

  ;; (setq clang-format-executable "clang-format-9")

  ;; don't know why this is disabled by default...
  (setq undo-tree-enable-undo-in-region t)

  (defun mc/search-cpp-ref (search-term)
    (interactive "sSearch Term: ")
    (require 'url-util)
    (browse-url (string-join (list "https://en.cppreference.com/mwiki/index.php?title=Special:Search&search=" (url-hexify-string search-term)))))

  (spacemacs/set-leader-keys "o c" 'mc/search-cpp-ref)

  (defadvice compile (around use-bashrc activate)
    "Load .bashrc in any calls to bash (e.g. so we can use aliases)"
    (let ((shell-command-switch "-ic"))
      ad-do-it))
  (setq compilation-scroll-output t) ;; make compilation window scroll
  ;;(setq compilation-scroll-output 'first-error) ;; stop at first error

  (setq scroll-conservatively 101
        scroll-margin 6
        scroll-preserve-screen-position 't)

  ;; magit
  (defadvice magit-diff-visit-file-other-window (after fix-git-gutter activate) (make-window-fringes-smaller-for-git-gutter))
  (with-eval-after-load 'magit
    (define-key magit-hunk-section-map (kbd "M-<return>") 'magit-diff-visit-file-other-window)
    (define-key magit-hunk-section-map (kbd "C-<return>") 'magit-diff-visit-file-other-window)
    (define-key magit-file-section-map (kbd "M-<return>") 'magit-diff-visit-file-other-window)
    (define-key magit-file-section-map (kbd "C-<return>") 'magit-diff-visit-file-other-window))

  ;; custom bindings for treemacs
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map (kbd "o a -") 'treemacs-visit-node-ace-vertical-split)
    (define-key treemacs-mode-map (kbd "o a /") 'treemacs-visit-node-ace-horizontal-split)
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))


  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  (setq tramp-default-method "ssh")
  (setq tramp-verbose 3)
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

  (setq dired-listing-switches "-alh")

  ;;more overlay colors
  (defface symbol-overlay-face-9
    '((t (:background "orange" :foreground "black")))
    "Symbol Overlay default candidate 9"
    :group 'symbol-overlay)

  (defface symbol-overlay-face-10
    '((t (:background "medium purple" :foreground "black")))
    "Symbol Overlay default candidate 8"
    :group 'symbol-overlay)
  (add-to-list 'symbol-overlay-faces 'symbol-overlay-face-9)
  (add-to-list 'symbol-overlay-faces 'symbol-overlay-face-10)


  (setq doom-modeline-vcs-max-length 100
        doom-modeline-buffer-encoding nil)

  ;; no bell
  (setq ring-bell-function 'ignore)
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))



  (defun mc/toggle-notes ()
    (interactive)
    (let* ((all-prefs (window-prev-buffers))
           (nextpr (car all-prefs))
           (_buffer-name (buffer-name nil)))
      (if (string-equal _buffer-name "notebook.org")
          (switch-to-buffer (car nextpr))
        (mc/open-notebook))))


  (defun mc/open-inbox () (interactive) (find-file "~/Dropbox/org/inbox.org"))
  (defun mc/open-notebook () (interactive) (find-file "~/dev/org/notebook.org"))
  (defun mc/open-reference () (interactive) (find-file "~/Dropbox/org/store.org"))

  (spacemacs/declare-prefix "o" "custom")
  (spacemacs/set-leader-keys
    "oi" 'mc/open-inbox
    "oo" 'mc/toggle-notes
    "or" 'mc/open-reference)

  (spacemacs/set-leader-keys
    "aa" 'org-agenda-list)

  (spacemacs/set-leader-keys
    "gh" 'git-gutter+-show-hunk-inline-at-point)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "tu" 'org-dblock-update)
  (spacemacs/set-leader-keys "qq" 'spacemacs/frame-killer)


  (setq helm-buffer-max-length nil)

  ;; u was set to downcase which sucks
  (unbind-key (kbd "u") evil-visual-state-map)
  (bind-key (kbd "u") 'undo-tree-undo evil-visual-state-map)

  (defun make-window-fringes-smaller-for-git-gutter ()
    (if (bound-and-true-p git-gutter+-mode)
        (set-window-fringes nil 4 8)
      (set-window-fringes nil nil)))
  (add-hook 'git-gutter+-mode-hook 'make-window-fringes-smaller-for-git-gutter)


  ;; better grep:
  (setq grep-files-aliases
        '(("all" . "* .[!.]* ..?*")
          ("el" . "*.el")
          ("ch" . "*.[ch]")
          ("c" . "*.c")
          ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
          ("cch" . "*.h *.cc *.cpp")
          ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++")
          ("h" . "*.h")
          ("m" . "[Mm]akefile*")
          ("tex" . "*.tex")))

  ;; don't ask when visiting files under versioncontrol
  (setq vc-follow-symlinks t)

  (setq lsp-file-watch-ignored '(
                                 "[/\\\\]\\.git$"
                                 "[/\\\\]\\.hg$"
                                 "[/\\\\]\\.bzr$"
                                 "[/\\\\]_darcs$"
                                 "[/\\\\]\\.svn$"
                                 "[/\\\\]_FOSSIL_$"
                                 "[/\\\\]\\.idea$"
                                 "[/\\\\]\\.bitbucket$"
                                 "[/\\\\]\\.ensime_cache$"
                                 "[/\\\\]\\.clwb$"
                                 "[/\\\\]\\.vscode$"
                                 "[/\\\\]\\.eunit$"
                                 "[/\\\\]node_modules$"
                                 "[/\\\\]\\.fslckout$"
                                 "[/\\\\]\\.tox$"
                                 "[/\\\\]\\.stack-work$"
                                 "[/\\\\]\\.bloop$"
                                 "[/\\\\]\\.metals$"
                                 "[/\\\\]target$"
                                 "[/\\\\]\\.deps$"
                                 "[/\\\\]build-aux$"
                                 "[/\\\\]autom4te.cache$"
                                 ".cache/bazel"
                                 "[/\\\\]\\.ccls-cache$"
                                 "bazel-out"
                                 "bazel-werkstatt"
                                 "bazel-bin"
                                 "bazel-testlogs"
                                 "[/\\\\]\\.reference$"
                                 "[/\\\\]bazel-out$"
                                 "[/\\\\]bazel-werkstatt$"
                                 "[/\\\\]bazel-bin$"
                                 "[/\\\\]bazel-testlogs$"
                                 "/home/conradmi/.cache"
                                 "bazel-genfiles$"
                                 )
        lsp-idle-delay 0.5
        evil-want-Y-yank-to-eol nil
        writeroom-width 160
        )

  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(typing yapfify stickyfunc-enhance sphinx-doc pytest pyenv-mode py-isort pippel pipenv pyvenv pip-requirements lsp-python-ms lsp-pyright live-py-mode importmagic epc ctable concurrent helm-pydoc helm-gtags helm-cscope xcscope ggtags cython-mode counsel-gtags company-anaconda blacken anaconda-mode pythonic csv-mode php-mode dockerfile-mode speed-type org-super-agenda reveal-in-osx-finder osx-trash osx-dictionary osx-clipboard launchctl web-mode web-beautify tagedit slim-mode scss-mode sass-mode pug-mode prettier-js impatient-mode simple-httpd helm-css-scss haml-mode emmet-mode counsel-css counsel swiper ivy company-web web-completion-data add-node-modules-path apache-mode piper zenburn-theme zen-and-art-theme yasnippet-snippets yaml-mode xterm-color ws-butler writeroom-mode winum white-sand-theme which-key vterm volatile-highlights vlf vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tron-legacy-theme treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil transpose-frame toxi-theme toml-mode toc-org terminal-here tao-theme tangotango-theme tango-plus-theme tango-2-theme symon symbol-overlay sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle shell-pop seti-theme reverse-theme restart-emacs rebecca-theme rainbow-mode rainbow-delimiters railscasts-theme racer purple-haze-theme professional-theme prism popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pdf-tools pcre2el password-generator paradox overseer orgit organic-green-theme org-superstar org-projectile org-present org-pomodoro org-mime org-download org-cliplink org-brain open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme nameless mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme modus-vivendi-theme modus-operandi-theme minimal-theme material-theme majapahit-theme magit-svn magit-section magit-gitflow madhat2r-theme macrostep lush-theme lsp-ui lorem-ipsum link-hint light-soap-theme kaolin-themes jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide hybrid-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-rtags helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp helm-ls-git helm-icons helm-gitignore helm-git-grep helm-flx helm-descbinds helm-ctest helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate google-c-style golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter+ gandalf-theme fuzzy font-lock+ flycheck-ycmd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-package flycheck-elsa flx-ido flatui-theme flatland-theme farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-ediff evil-cleverparens evil-args evil-anzu evil-adjust espresso-theme eshell-z eshell-prompt-extras esh-help emr elisp-slime-nav editorconfig dumb-jump dracula-theme dotenv-mode doom-themes doom-modeline django-theme disaster diminish devdocs define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dap-mode dakrone-theme cyberpunk-theme cpp-auto-include company-ycmd company-rtags company-c-headers column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmake-mode cmake-ide clues-theme clojure-snippets clean-aindent-mode cider-eval-sexp-fu cider chocolate-theme cherry-blossom-theme centered-cursor-mode ccls cargo busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bazel-mode badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme ace-link ace-jump-helm-line ac-ispell))
 '(safe-local-variable-values
   '((dap-debug-template-configurations
      ("finance" :type "lldb" :target "/Users/michaelconrads/dev/lab/finance/build/finance")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)