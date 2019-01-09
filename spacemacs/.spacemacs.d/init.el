;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

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

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (deft :variables
     	 deft-directory "~/Sync/notes")
     ;; ivy
     helm
     ;; better-defaults
     (auto-completion :variables
                      ;; default
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      ;; personal
                      auto-completion-private-snippets-directory "~/Sync/.yasnippets"
                      auto-completion-enable-snippets-in-popup t
                      ;; auto-completion-enable-help-tooltip 'manual ;; M-h
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     ;; --user importmagic epc service_factory autoflake json-rpc
     ;; pytest python-jedi hy yapf python-isort
     (python :variables
             python-test-runner 'pytest
             ;; python-test-runner '(pytest nose)
             python-sort-imports-on-save nil  ;; ", r I"
             python-enable-yapf-format-on-save nil ;; ", ="
             )
     emacs-lisp
     neotree
     ;; org
     (org :variables
          ;; org-enable-bootstrap-support t
          org-enable-org-journal-support t
          org-journal-dir "~/Sync/share/phone/box/journal/"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format ""
          org-journal-find-file 'find-file
          org-journal-enable-agenda-integration t
          ;; org-enable-github-support t
          org-enable-reveal-js-support t
          org-reveal-root "/home/dan/.pandoc/reveal.js"
          org-projectile-file "/home/dan/Sync/notes/TODOs.org")
     org-gcal
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     (mu4e :variables
           ;; mu4e-headers-sort-direction "ascending"
           mu4e-spacemacs-layout-name "@Mu4e"
           mu4e-spacemacs-layout-binding "m"
           mu4e-spacemacs-kill-layout-on-exit t
           mu4e-maildir "~/Sync/Maildir"
           mu4e-account-alist t
           ;; mu4e-enable-async-operations t ;; problem: do not send email!!!
           mu4e-use-maildirs-extension t
           ;; when mail is sent, automatically convert org body to HTML
           org-mu4e-convert-to-html t
           ;; set mu4e as default
           mail-user-agent 'mu4e-user-agent
           message-mail-user-agent 'mu4e-user-agent
           ;; mode-line notifications about new messages
           mu4e-enable-mode-line t)
     ;; hunspell-{it,en_US}
     (spell-checking :variables
                     spell-checking-enable-by-default nil  ;; =SPC t S=
                     enable-flyspell-auto-completion t)
     ;; "SPC e l"
     ;; for python: pacaur -S flake8 or python-pylint
     ;; (syntax-checking :variables syntax-checking-use-original-bitmaps t)
     syntax-checking
     ;; (git :variables git-gutter-use-fringe t)
     git
     bibtex
     (pdf :variables
          pdf-misc-print-programm "/usr/bin/lpr"
          pdf-misc-print-programm-args (quote ("-o media=a4" "-o fitplot"))
          pdf-view-resize-factor 1.1
          pdf-annot-activate-created-annotations t)
     markdown
     csv
     ;; ;; (elfeed :variables
     ;; ;;         rmh-elfeed-org-files (list "~/Sync/.elfeed/1.org"))
     ;; ;; ipython-notebook
     ;; ;; ess
     pandoc ;; need wkhtmltopdf
     graphviz
     html ;; also non sure I need it
     (plantuml :variables
               org-plantuml-jar-path "/opt/plantuml/plantuml.jar"
               plantuml-jar-path "/opt/plantuml/plantuml.jar")
     writing
     typography  ;; =SPC t T= to enable
     ;; themes-megapack
     themes-selected
     epub
     (twitter :variables
              twittering-use-master-password t)
     systemd
     common-lisp  ;; sbcl required; provide slime
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(ob-ipython
                                      org-noter
                                      visual-fill-column
                                      git-annex ;; C-x C-q in annexed buffer; in dired instead @e @a (@g @d);
                                      magit-annex
                                      fzf
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
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         sanityinc-solarized-dark
                         sanityinc-solarized-light
                         subatomic256
                         spacemacs-dark
                         spacemacs-light
                         spacegray
                         zen-and-art
                         material
                         smyx
                         granger
                         tangotango
                         tango-dark
                         tango-plus
                         flatui
                         zen-and-art
                         soft-stone
                         molokai
                         doom-molokai
                         afternoon
                         ample-zen
                         badwolf
                         soft-morning
                         sanityinc-tomorrow-day
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.0)
   ;; dotspacemacs-mode-line-theme '(vim-powerline)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '(;;
                               ;; "DejaVu Sans Mono"
                               ;; "Hack"
                               ;; "Source Code Pro"
                               ;; "Inconsolata-g"
                               ;; "Fira Code Retina"
                               "Fira Code"
                               ;; "Meslo LG S"
                               ;; "Fantasque Sans Mono"
                               ;; "Hermit"
                               ;; "Bitstream Vera Sans Mono"
                               ;; "Monofur"
                               ;; "Dina"
                               :size 14
                               :weight regular
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
   dotspacemacs-enable-server nil

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
  (setq-default git-magit-status-fullscreen t)
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
  (setq browse-url-browser-function 'browse-url-generic
        engine/browser-function 'browse-url-generic
        browse-url-generic-program "firefox")
  (set-language-environment "UTF-8")
  ;; ;; to debug
  (setq debug-on-error t)
  ;; ;; To use always only one frame fullscreen, simply.
  ;; (set-frame-parameter nil 'fullscreen 'fullboth)

  ;; proselint in org and mu4e modes
  (add-to-list 'flycheck-global-modes 'markdown-mode)
  (add-to-list 'flycheck-global-modes 'org-mode)
  (add-to-list 'flycheck-global-modes 'mu4e-compose-mode) ;; yet not working
  (with-eval-after-load 'flycheck
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
	            (id (one-or-more (not (any " "))))
	            (message) line-end))
    :modes (text-mode markdown-mode gfm-mode message-mode mu4e-modes org-mode))
  (add-to-list 'flycheck-checkers 'proselint)
  )

  ;; deft
  (setq deft-recursive t
        deft-extensions '("org" "md" "txt" "markdown"))

  ;; rescale text size
  ;; C-x C-0 restores the default font size
  (setq text-scale-mode-step 1.05)
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)

  ;; emulate ctags
  (global-set-key (kbd "<f9>") 'imenu-list)
  (global-set-key (kbd "C-<f9>") 'imenu-list-smart-toggle)

  ;; emulate i3wm
  (global-set-key (kbd "s-<left>") 'evil-window-left)
  (global-set-key (kbd "H-<left>") 'evil-window-left)
  (global-set-key (kbd "H-<right>") 'evil-window-right)
  (global-set-key (kbd "H-<up>") 'evil-window-up)
  (global-set-key (kbd "H-<down>") 'evil-window-down)
  (global-set-key (kbd "H-q") 'evil-window-delete)

  ;; evil-numbers =SPC n -=
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

  ;; gtranslate =SPC x g t= =SPC u SPC x g t=
  ;; (setq google-translate-translation-directions-alist '(("it" . "en") ))
  (setq google-translate-default-source-language "it"
        google-translate-default-target-language "en")

  ;; visual-fill-colums
  (setq visual-fill-column-center-text t
        ;; ;; set right curly arrow even when visual line mode is wrapping logical lines into visual ones.
        ;; visual-line-fringe-indicators '(arrow right-curly-arrow)
        visual-fill-column-fringes-outside-margins nil
        ;; allow splitting windows with wide margins
        split-window-preferred-function #'visual-fill-column-split-window-sensibly)
  ;; adjust margins upon text resize
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  (add-hook 'visual-fill-column-mode-hook #'visual-line-mode)
  ;; (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (define-key evil-normal-state-map (kbd "<SPC> o o") 'visual-fill-column-mode)
  ;; use variable-width font for some modes
  (defun xah-use-variable-width-font ()
    "Set current buffer to use variable-width font."
    (variable-pitch-mode 1)
    ;; (visual-fill-column-mode)
    (text-scale-increase 1.1))

  ;; bookmarks file
  (setq bookmark-save-flag 1
        bookmark-default-file "~/.spacemacs.d/bookmarks" )

  ;; org-noter
  (define-key evil-normal-state-map (kbd "<SPC> o n") 'org-noter)
  (setq org-noter-property-doc-file "INTERLEAVE_PDF"
        org-noter-property-note-location "INTERLEAVE_PAGE_NOTE"
        org-noter-default-notes-file-names "~/Sync/biblio/biblio.org"
        org-noter-always-create-frame nil  ;; do not create a new frame
        ;; org-noter-doc-property-in-notes t
        org-noter-doc-split-fraction '(0.7 . 0.5)
        )
  (with-eval-after-load 'org-noter
    (define-key org-noter-notes-mode-map (kbd "C-M-k") 'org-noter-create-skeleton)
    (define-key org-noter-doc-mode-map (kbd "C-M-k") 'org-noter-create-skeleton))
  (add-hook 'text-mode-hook 'evil-insert-state)


  ;; pdf
  (add-hook 'pdf-view-mode-hook 'pdf-view-set-slice-from-bounding-box)

  ;; spell-checking enabled in all text and derived (e.g. org latex composemail)
  (add-hook 'text-mode-hook 'flyspell-mode)
  ;; dictionary switch
  (define-key evil-normal-state-map (kbd "<SPC> S r") 'flyspell-region)
  (define-key evil-normal-state-map (kbd "<SPC> S a") (lambda () (interactive) (ispell-change-dictionary "american")))
  (define-key evil-normal-state-map (kbd "<SPC> S i") (lambda () (interactive) (ispell-change-dictionary "italian")))
  ;; DAN at the moment does not work
  ;; (with-eval-after-load "ispell"
  ;;   (setq ispell-program-name "hunspell")
  ;;   ;; ispell-set-spellchecker-params has to be called
  ;;   ;; before ispell-hunspell-add-multi-dic will work
  ;;   (ispell-set-spellchecker-params)
  ;;   (ispell-hunspell-add-multi-dic "it_IT,en_US")
  ;;   (setq ispell-dictionary "it_IT,en_US"))

  ;; magit
  (setq magit-repository-directories '("~/Sync/" "~/workspace/")) ;like projectile
  (require 'git-annex)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)


  ;; personal config
  (push "~/.spacemacs.d/config/" load-path)
  (with-eval-after-load 'org (require 'my-gtd)
                        (require 'my-org)
                        (require 'my-org-publish))
  ;; (with-eval-after-load 'elfeed
  ;;   (require 'elfeed-config))
  (with-eval-after-load 'bibtex (require 'bibtex-config))
  (with-eval-after-load 'mu4e (require 'mu4e-config))

  ;; TAB for za
  ;; (define-key evil-normal-state-map (kbd "<tab>") (kbd "za")) ;conflict magit
  (with-eval-after-load 'python
    (define-key python-mode-map (kbd "<tab>") (kbd "za")))
  (define-key emacs-lisp-mode-map (kbd "<tab>") (kbd "za"))

  (add-hook 'python-mode-hook (lambda nil (load-theme 'wombat )))
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
   (quote
    (slime-company slime common-lisp-snippets zen-and-art-theme yasnippet-snippets yapfify xresources-theme ws-butler writeroom-mode writegood-mode wordnut winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package typo twittering-mode twilight-bright-theme toc-org theme-changer tangotango-theme tango-plus-theme tagedit systemd symon sublime-themes subatomic256-theme string-inflection spaceline-all-the-icons spacegray-theme solarized-theme soft-stone-theme soft-morning-theme smyx-theme smeargle slim-mode sdcv scss-mode sass-mode restart-emacs rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode prettier-js popwin plantuml-mode pippel pipenv pip-requirements persp-mode pcre2el password-generator paradox pandoc-mode ox-reveal ox-pandoc overseer orgit org-ref org-projectile org-present org-pomodoro org-noter org-mime org-journal org-gcal org-download org-bullets org-brain open-junk-file ob-ipython nov neotree nameless mu4e-maildirs-extension mu4e-alert move-text monokai-theme molokai-theme mmm-mode material-theme markdown-toc magit-svn magit-gitflow magit-annex macrostep lorem-ipsum live-py-mode link-hint langtool intellij-theme indent-guide importmagic impatient-mode idea-darkula-theme hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-mu helm-mode-manager helm-make helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag graphviz-dot-mode google-translate golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-annex gh-md fzf fuzzy font-lock+ flyspell-popup flyspell-correct-helm flycheck-pos-tip flx-ido flatui-theme fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav editorconfig dumb-jump dotenv-mode doom-themes doom-modeline diminish dictcc deft define-word darcula-theme cython-mode csv-mode counsel-projectile company-web company-statistics company-quickhelp company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clean-aindent-mode centered-cursor-mode borland-blue-theme blackboard-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile atom-dark-theme artbollocks-mode ample-zen-theme aggressive-indent afternoon-theme ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
