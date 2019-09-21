;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;;; dotspacemacs/layers
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
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/")

   ;; List of configuration layers to load.
;;; layers config
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; +chat
     ;; erc
     ;; jabber
     ;; rcirc
     ;; slack

     ;; +checkers
     ;; spell-checking
     ;syntax-checking

     ;; +completion
     ;auto-completion
     ;helm
     ;; ivy
     ;; templates
     ;; (templates :variables
     ;;            templates-private-directory "~/.spacemacs.d/templates")

     ;; +distribution

     ;; +emacs
     ;better-defaults
     ;ibuffer
     ;org
     ;; semantic
     ;; smex                               ;提供更传统的补全方式,基于ido的
     ;; typography

     ;; +email
     ;; gnus
     ;; mu4e
     ;; notmuch

     ;; +filetree
     ;; neotree
     ;treemacs

     ;; +fonts
     ;; unicode-fonts

     ;; +frameworks
     ; django
     ;; emberjs ;; phoenix
     ;; react
     ;; ruby-on-rails

     ;; +fun
     ;; emoji
     ;; games
     ;; selectric
     ;; xkcd

     ;; +intl
     ;; chinese
     ;; japanese
     ;; keyboard-layout

     ;; +lang
     ;; agda
     ;; asciidoc
     ;; asm
     ;; autohotkey
     ;; bibtex
     ;; c-c++
     ;; clojure
     ;; coffeescript
     ;common-lisp
     ;; coq
     ;; crystal
     ;; csharp
     ;; csv
     ;; d
     ;; elixir
     ;; elm
     ;emacs-lisp
     ;; erlang
     ;; ess
     ;; factor
     ;; faust
     ;; forth
     ;; fsharp
     ;go
     ;; gpu
     ;; graphviz
     ;; groovy
     ;haskell
     ;; html
     ;; hy
     ;; idris
     ;; ipython-notebook
     ;; java
     ;; javascript
     ;; jr
     ;; json
     ;; jsonnet
     ;; julia
     ;; kotlin
     ;; latex
     ;; lua
     ;; major-modes
     ;; markdown
     ;; nim
     ;; ocaml
     ;; ocatave
     ;; pact
     ;; per15
     ;; per16
     ;; php
     ;; plantuml
     ;; protobuf
     ;; purescript
     ;; python
     ;; racket
     ;; restructuretext
     ;; ruby
     ;; rust
     ;; scala
     ;; scheme
     ;; semantic-web
     ;; shell-scripts
     ;; sml
     ;; sql
     ;; swift
     ;; typescript
     ;; vimscript
     ;; windows-scripts
     ;; yaml

     ;; +misc
     ;; copy-as-format
     ;; multiple-cursors
     ;; nlinum
     ;; parinfer

     ;; +music
     ;; spotify

     ;; +os
     ;; nixos
     ;; osx

     ;; +pari-programming
     ;; floobits

     ;; +readers
     ;; dash
     ;; deft
     ;; elfeed
     ;; epub
     ;; pdf
     ;; speed-reading

     ;; +source-control
     ;; git
     ;; github
     ;; perforce
     ;; version-control
     ;; (version-control :variables
     ;;                  version-control-diff-tool 'diff-hl
     ;;                  version-control-diff-side 'left
     ;;                  version-control-global-margin t
     ;;                  )

     ;; +spacemacs

     ;; +tags
     ;; cscope
     ;; gtags

     ;; +themes
     ;; colors
     ;; themes-megapack
     ;; theming

     ;; +tools
     ;; ansible
     ;; bm
     ;; cfenging
     ;; chrome
     ;; cmake-mode
     ;; command-log
     ;; dap
     ;; debug
     ;; docker
     ;; fasd
     ;; finance
     ;; geolocation
     ;imenu-list
     ;lsp
     ;; nginx
     ;; node
     ;; pandoc
     ;; pass
     ;; prettier
     ;; prodigy
     ;; puppet
     ;; ranger
     ;; rebox
     ;; restclient
     ;; salt
     ;; (shell :variables
             ;; shell-default-height 30
             ;; shell-default-position 'bottom)
     ;; sphinx
     ;; systemed                           ;不识别这个层
     ;; tern
     ;; terraform
     ;; tmux                               ;干扰太多快捷键了
     ;; transmission
     ;; vagrant
     ;; web-beautify
     ;; xclipboard
     ;; ycmd

     ;; +vim
      ;; evil-commentary
     ;; evil-snipe
     ;; vim-empty-lines
     ;; vinegar

     ;; +web-services
     ;; confluence
     ;; evernote
     ;; search-engine
     ;; twitter
     ;; wakatime

     ;; exwm
     ;; backup                             ;; +tools下的备份设置
     ;; dingyi
     ;; timestamp

;;; spacemacs layer
     syntax-checking
     auto-completion
     helm
     better-defaults
     ibuffer
     (org :variables
          org-src-tab-acts-natively nil)
     treemacs
     common-lisp
     emacs-lisp
     go
     haskell
     (java :variables
           ;; java-backend 'meghanada
           java-backend 'lsp
           )
     (c-c++ :variables
            ;; c-c++-backend 'lsp-cquery
            c-c++-adopt-subprojects t
            c-c++-backend 'lsp-ccls
            ;; c-c++-lsp-executable (file-truename "~/dev/cpp/ccls/Release/ccls")
            c-c++-lsp-executable "/usr/bin/ccls"
            c-c++-lsp-sem-highlight-rainbow t
            c-c++-lsp-cache-dir (concat spacemacs-cache-directory "lsp-ccls")
            )
     dap
     dart
     markdown
     (python :variables
             ;; python-backend 'anaconda
             python-backend 'lsp
             python-lsp-server 'mspyls
             ;; lsp-python-ms-dir
             lsp-python-ms-dir "/usr/lib/microsoft-python-language-server/"
             lsp-python-ms-executable nil
             ;; lsp-python-ms-executable "/usr/lib/microsoft-python-language-server/Microsoft.Python.LanguageServer"
             ;; python-lsp-git-root "~/dev/python/python-language-server"
             python-sort-imports-on-save t
             python-fill-column 99
             python-test-runner '(pytest nose)
             python-formatter 'yapf
             python-format-on-save t
             python-save-before-test nil
             )
     scheme
     shell-scripts
     multiple-cursors
     deft
     git
     version-control
     chrome
     imenu-list
     lsp
     ranger
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     search-engine
     vimscript
     latex

;;;; 前端 Web
     web-beautify
     (html :variables
           web-fmt-tool 'web-beautify
           css-enable-lsp t
           less-enable-lsp t
           scss-enable-lsp t)
     json
     javascript

;;; my layer configuration
;;;; my-core-layer
     my-fonts
     my-bootstrap
     my-minibuffer
     outshine
     chinese-IM
     my-org-db
     my-switch-buffer
     my-org
     my-org-themes
     my-describe
     my-org-babel
     my-org-image
     my-org-agenda
     my-helm
     my-ivy
     my-switch-buffer
     my-snippet
     my-dired
     my-pinyin
     my-editing
     my-system-packages                 ;安装程序
     ;; emms
     my-emms
     my-evil
     my-stumpwm                         ;执行stumpwm lisp
     my-web
     my-image
     ;; my-modeline
     my-message                         ;有些消息不要显示
     ;; my-autosave
     my-dict
     my-bookmarks
     my-themes

;;;; prog
     my-java
     my-haskell
;;;; 工具
     my-shell

;;;; 其他
     timestamp                          ;增加几个插入时间戳的函数

     my-search
     my-snippet
     my-english
     my-proxy                              ;提供函数，切换proxy状态
     my-download
     my-firefox
     my-prog
     ;; my-lsp
     ;; my-git
     ;; mmm

     my-default
;;;; my-test-layer
     ;; my-anki
     ;; my-snails
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
;;; additional/frozen/exclded packages
   dotspacemacs-additional-packages '(
                                      doom-themes
                                      ;;ends here
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

;;; dotspacemacs/init
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

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'org-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message "I love cxy!"

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         kaolin-ocean
                         doom-dracula
                         kaolin-dark
                         atom-one-dark
                         ;; doom-gruvbox
                         ;; spacemacs-dark
                         ;; spacemacs-light
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(doom :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '(
                               ;; "ConsolasWithYahei"
                               ;; :size 17
                               ;; :weight normal
                               ;; :width normal
                               )

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
   dotspacemacs-major-mode-leader-key "m"
   ;; dotspacemacs-major-mode-leader-key nil

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   ;; dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-major-mode-emacs-leader-key "<f12>"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; dotspacemacs-distinguish-gui-tab t
   ;; dotspacemacs-distinguish-gui-ret t
   ;; dotspacemacs-distinguish-gui-esc t

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
   ;; dotspacemacs-enable-server nil
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
   dotspacemacs-pretty-docs nil))

;;; dotspacemacs/user-env
(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

;;; dotspacemacs/user-init
(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;;;; dingyi/set melpa mirror
  (setq configuration-layer-elpa-archives
        '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
          ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))

;;;; 设置outline minor mode 的prefix,必须在outline加载之前就加载
  (defvar outline-minor-mode-prefix "\M-#") ;must be set before outline is loaded

;;;; maximize
  (setq frame-resize-pixelwise t)
  (setq x-frame-normalize-before-maximize t)
  (tool-bar-mode -1)
  (dotimes (n 3)
    (toggle-frame-maximized))

;;;; user-init ends
  )
;;; dotspacemacs/user-load
(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
;;;; user-load ends
  )
;;; dotspacemacs/user-config
(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
;;;; 杀死buffer
  (kill-buffer "*spacemacs*")
  ;; 有warnings 还是处理比较好。
  ;; (kill-buffer "*Warnings*")
  (delete-other-windows)

;;;; user-config ends
  )
;;; dotspacemacs/emacs-custom-settings
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
 '(ansi-color-names-vector
   ["#000000" "#880000" "#005500" "#663311" "#004488" "#770077" "#007777" "#eeeecc"])
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("7d4340a89c1f576d1b5dec57635ab93cdc006524bda486b66d01a6f70cffb08e" default))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#a3a1a1")
 '(highlight-changes-colors '("#ff8eff" "#ab7eff"))
 '(highlight-tail-colors
   '(("#323342" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#323342" . 100)))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")))
 '(iswitchb-mode t)
 '(jdee-db-active-breakpoint-face-colors (cons "#f2f2f2" "#4271ae"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f2f2f2" "#718c00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f2f2f2" "#a5a4a5"))
 '(kaolin-themes-hl-line-colored nil)
 '(linum-format " %5i ")
 '(magit-diff-use-overlays nil)
 '(objed-cursor-color "#c82829")
 '(org-drill-done-count-color "#663311")
 '(org-drill-failed-count-color "#880000")
 '(org-drill-mature-count-color "#005500")
 '(org-drill-new-count-color "#004488")
 '(package-selected-packages
   '(bf-mode all-the-icons-dired capture ansi package-build shut-up epl git commander f dash s cask magit elisp-format helpful elisp-demos gcmh srcery-theme lab-themes flucui-themes ess-smart-equals multiple-cursors dracula-theme zenburn-theme gruvbox-theme darkokai-theme benchmark-init esup atom-one-dark-theme atom-dark-theme dired-quick-sort pangu-spacing org-noter cdlatex org-edit-latex magit-todos netease-music pinyin-search purp-theme helm-file-preview naysayer-theme parchment-theme poly-org bongo evil-embrace camcorder helm-emms load-relative vterm "anki-editor" anki-editor twittering-mode weibo highlight-indent-guides eink-theme simple-bookmarks fm-bookmarks autobookmarks nord-theme el-get emaps helm-youtube leuven-theme org-babel-eval-in-repl use-package-ensure-system-package evil helm-rg pyim proxy-mode yatemplate yasnippet-snippets yapfify xterm-color ws-butler writeroom-mode winum which-key wgrep volatile-highlights visual-regexp-steroids vi-tilde-fringe uuidgen use-package unfill treemacs-projectile treemacs-evil toc-org system-packages symon symbol-overlay stumpwm-mode string-inflection spaceline-all-the-icons smex smeargle slime-company shell-pop restart-emacs ranger rainbow-delimiters pytest pyenv-mode py-isort prettier-js popwin pippel pipenv pip-requirements persp-mode pcre2el password-generator paradox overseer org-projectile org-present org-pomodoro org-mime org-download org-cliplink org-bullets open-junk-file nameless mwim mvn multi-term move-text mmm-mode meghanada maven-test-mode markdown-toc magit-svn magit-gitflow lsp-ui lsp-treemacs lsp-python-ms lsp-java lsp-haskell lorem-ipsum live-py-mode link-hint ivy-yasnippet ivy-xref ivy-purpose ivy-posframe ivy-hydra intero insert-shebang indent-guide importmagic iflipb ibuffer-projectile hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-systemd helm-system-packages helm-swoop helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-navi helm-mode-manager helm-make helm-lsp helm-hoogle helm-gitignore helm-git-grep helm-flx helm-dired-history helm-descbinds helm-company helm-c-yasnippet helm-ag haskell-snippets groovy-mode groovy-imports grep-dired gradle-mode google-translate golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gmail-message-mode gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md general geiser fuzzy font-lock+ flypy flymd flycheck-pos-tip flycheck-package flycheck-haskell flycheck-bashate flx-ido fish-mode find-file-in-project fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-find-char-pinyin evil-exchange evil-escape evil-ediff evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help ensime engine-mode emms emacsql-sqlite elisp-slime-nav editorconfig edit-server dumb-jump dotenv-mode doom-themes doom-modeline dired-toggle-sudo dired-toggle dired-ranger dired-rainbow dired-open dired-narrow dired-filter dired-collapse dired-avfs diminish diff-hl devdocs deft define-word dante cython-mode counsel-projectile company-statistics company-shell company-lsp company-go company-ghci company-ghc company-emacs-eclim company-cabal company-anaconda common-lisp-snippets column-enforce-mode cnfonts cmm-mode clean-aindent-mode centered-cursor-mode browse-at-remote blacken beacon auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent ace-pinyin ace-link ace-jump-helm-line ac-ispell))
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-color-map
   (list
    (cons 20 "#718c00")
    (cons 40 "#999a00")
    (cons 60 "#c1a800")
    (cons 80 "#eab700")
    (cons 100 "#eda70a")
    (cons 120 "#f19714")
    (cons 140 "#f5871f")
    (cons 160 "#e69659")
    (cons 180 "#d7a594")
    (cons 200 "#c9b4cf")
    (cons 220 "#c88597")
    (cons 240 "#c85660")
    (cons 260 "#c82829")
    (cons 280 "#bf4748")
    (cons 300 "#b66667")
    (cons 320 "#ad8586")
    (cons 340 "#a3a1a1")
    (cons 360 "#a3a1a1")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#242728" "#323342" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block-begin-line ((t (:background "black" :foreground "black")))))
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block-begin-line ((t (:background "black" :foreground "black"))))
)
