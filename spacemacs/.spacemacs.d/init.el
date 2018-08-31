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
     ;; gtags
     csv
     (deft :variables
     	 deft-directory "~/Sync/notes")
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
     ;; helm
     ;; ivy
     (org :variables
          ;; org-enable-bootstrap-support t
          ;; org-enable-github-support t
          org-enable-reveal-js-support t
          org-reveal-root "/home/dan/.pandoc/reveal.js"
          org-projectile-file "/home/dan/Sync/notes/TODOs.org")
     (mu4e :variables
           mu4e-maildir "~/Sync/Maildir"
           mu4e-account-alist t
           ;;;; mu4e-enable-async-operations t
           ;; mode-line notifications about new messages
           mu4e-enable-mode-line t)
     bibtex
     ;; (elfeed :variables
     ;;         rmh-elfeed-org-files (list "~/Sync/.elfeed/1.org"))

     emacs-lisp
     ;; --user importmagic epc service_factory autoflake
     ;; pytest python-jedi python-json-rpc hy yapf python-isort
     (python :variables
             python-test-runner 'pytest
             ;; python-test-runner '(pytest nose)
             python-sort-imports-on-save nil  ;; ", r I"
             python-enable-yapf-format-on-save nil ;; ", ="
             )
     ;; version-control
     ;; (git :variables git-gutter-use-fringe t)
     git
     markdown
     (pdf :variables
          pdf-annot-activate-created-annotations t)
     (spell-checking :variables
                     spell-checking-enable-auto-dictionary t
                     ;; ispell-program-name "hunspell"  ;; not so useful
                     ;; ispell-local-dictionary "it_IT" ;; not so useful
                     ;; ispell-local-dictionary "italian" ;; not so useful
                     spell-checking-enable-by-default nil
                     enable-flyspell-auto-completion t)
     ;; "SPC e l"
     ;; for python: pacaur -S flake8 or python-pylint
     (syntax-checking :variables syntax-checking-use-original-bitmaps t)
     ipython-notebook
     ess
     org-gcal
     graphviz
     html
     (plantuml :variables
               org-plantuml-jar-path "/opt/plantuml/plantuml.jar"
               plantuml-jar-path "/opt/plantuml/plantuml.jar")
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(;; develop includes
                                      ;; helm-mu
                                      ;; ob-ipython 
                                      org-noter
                                      visual-fill-column
                                      git-annex ;; C-x C-q in annexed buffer; in dired instead @e @a (@g @d);
                                      magit-annex
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
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Dejavu Sans Mono"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 0.9)

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
   dotspacemacs-large-file-size 6
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
  "Configuration function for user code.
This function is called at the very end of Spacemacs
initialization after layers configuration. This is the place
where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a
package is loaded, you should place your code here."
  (global-set-key (kbd "<f9>") 'imenu-list)
  (global-set-key (kbd "C-<f9>") 'imenu-list-smart-toggle)
  ;; need to fix python interpreter completion
  ;; to debug
  ;; (setq debug-on-error t)
  ;; fix inferior ipython startup
  (setq python-shell-completion-native-enable nil)
  ;; ;; not sure I need this
  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args "-i --simple-prompt")
  ;; (setq python-shell-interpreter "jupyter-console"
  ;;       python-shell-interpreter-args "--existing")
  ;; ;; Fix an incompatibility between the ob-async and ob-ipython packages
  ;; (setq ob-async-no-async-languages-alist '("ipython"))
  ;; ein
  (setq ein:jupyter-default-server-command "jupyter-notebook"
        ein:jupyter-default-notebook-directory "~/Sync/"
        ein:use-auto-complete-superpack t)
        ;; ein:use-smartrep t)
  ;;Try for ob-ipython: pip install importmagic epc
  (setq org-src-window-setup 'current-window) ;;Try
  ;;Try (setq org-src-lang-modes '(("ipython" . python)))
  ;; (require 'color) ;; This is somehow obsolete.
  ;; (set-face-attribute 'org-block nil :background
  ;;                     (color-darken-name
  ;;                      (face-attribute 'default :background) 3))
  ;; (setq org-src-block-faces '(("emacs-lisp" (:background "#EEE2FF"))
  ;;                             ("ipython" (:background "#E5FFB8"))))
  (global-company-mode)
  (add-to-list 'company-backends 'company-ob-ipython)
  (spacemacs/set-leader-keys "h i" 'ob-ipython-inspect)

  ;; required by mu4e-send-delay for sending correctly formatted email
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")

  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (add-hook 'pdf-view-mode-hook 'pdf-view-set-slice-from-bounding-box)
  (define-key evil-normal-state-map (kbd "<SPC> o n") 'org-noter)
  (setq org-noter-property-doc-file "INTERLEAVE_PDF"
        org-noter-property-note-location "INTERLEAVE_PAGE_NOTE")
  ;; dictionary switch
  (define-key evil-normal-state-map (kbd "<SPC> S r") 'flyspell-region)
  (define-key evil-normal-state-map (kbd "<SPC> S a") (lambda () (interactive) (ispell-change-dictionary "american")))
  (define-key evil-normal-state-map (kbd "<SPC> S i") (lambda () (interactive) (ispell-change-dictionary "italian")))

  ;; gtranslate =SPC x g t=
  ;; (setq google-translate-translation-directions-alist '(("it" . "en") ))
  (setq google-translate-default-source-language "it"
        google-translate-default-target-language "en")

  ;; deft
  (setq deft-recursive t
        deft-extensions '("org" "md" "txt" "markdown"))
  ;; personal config
  (push "~/.spacemacs.d/config/" load-path)
  (with-eval-after-load 'org
    (require 'my-gtd)
    (require 'my-org)
    (require 'my-org-publish)
    )
  (with-eval-after-load 'elfeed
    (require 'elfeed-config))
  (with-eval-after-load 'bibtex
    (require 'bibtex-config))
  ;; set mu4e as default
  (setq mail-user-agent 'mu4e-user-agent)
  (with-eval-after-load 'mu4e
    (require 'mu4e-config))
  ;; C-x C-0 restores the default font size
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  ;; (define-key evil-normal-state-map (kbd "<tab>") (kbd "za")) ;conflict magit
  (with-eval-after-load 'python
    (define-key python-mode-map (kbd "<tab>") (kbd "za")))
  (define-key emacs-lisp-mode-map (kbd "<tab>") (kbd "za"))
  ;; magit 
  (setq magit-repository-directories '("~/Sync/" "~/workspace/")) ;like projectile
  (require 'git-annex)
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
  ;; 
  ;; use variable-width font for some modes
  (defun xah-use-variable-width-font ()
    "Set current buffer to use variable-width font."
    (variable-pitch-mode 1)
    (text-scale-increase 1.1)
    ;; (visual-fill-column-mode)
    )
  (add-hook 'elfeed-show-mode-hook 'xah-use-variable-width-font)
  (add-hook 'elfeed-show-mode-hook 'visual-fill-column-mode)
  ;; (add-hook 'mu4e-view-mode-hook 'xah-use-variable-width-font)
  ;; (add-hook 'mu4e-headers-mode-hook 'xah-use-variable-width-font)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yapfify web-mode visual-fill-column tagedit smeargle slim-mode scss-mode sass-mode pyvenv pytest pyenv-mode py-isort pug-mode plantuml-mode pip-requirements orgit org-noter markdown-toc markdown-mode magit-gitflow magit-annex live-py-mode hy-mode helm-pydoc helm-gitignore helm-css-scss haml-mode gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-annex gh-md flyspell-popup flyspell-correct-helm flyspell-correct flycheck-pos-tip flycheck evil-magit magit magit-popup git-commit ghub with-editor ess-smart-equals ess-R-data-view ctable ess emmet-mode cython-mode company-web web-completion-data company-anaconda auto-dictionary pythonic anaconda-mode helm-mu org-ref pdf-tools key-chord ivy tablist helm-bibtex parsebib elfeed-web elfeed-org elfeed-goodies ace-jump-mode noflet elfeed biblio biblio-core mu4e-maildirs-extension mu4e-alert ht ob-ipython dash-functional ein skewer-mode websocket js2-mode simple-httpd helm-company helm-c-yasnippet fuzzy deft company-statistics company-quickhelp pos-tip company auto-yasnippet yasnippet ac-ispell auto-complete ox-reveal org-gcal request-deferred deferred org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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
    (yasnippet-snippets web-beautify symon string-inflection spaceline-all-the-icons all-the-icons memoize prettier-js pippel pipenv password-generator overseer org-brain nameless mmm-mode magit-svn importmagic epc concurrent impatient-mode helm-xref helm-purpose window-purpose imenu-list graphviz-dot-mode gitignore-templates evil-org evil-lion evil-goggles evil-cleverparens paredit editorconfig csv-mode counsel-projectile counsel swiper centered-cursor-mode font-lock+ dotenv-mode yapfify web-mode visual-fill-column tagedit smeargle slim-mode scss-mode sass-mode pyvenv pytest pyenv-mode py-isort pug-mode plantuml-mode pip-requirements orgit org-noter markdown-toc markdown-mode magit-gitflow magit-annex live-py-mode hy-mode helm-pydoc helm-gitignore helm-css-scss haml-mode gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-annex gh-md flyspell-popup flyspell-correct-helm flyspell-correct flycheck-pos-tip flycheck evil-magit magit magit-popup git-commit ghub with-editor ess-smart-equals ess-R-data-view ctable ess emmet-mode cython-mode company-web web-completion-data company-anaconda auto-dictionary pythonic anaconda-mode helm-mu org-ref pdf-tools key-chord ivy tablist helm-bibtex parsebib elfeed-web elfeed-org elfeed-goodies ace-jump-mode noflet elfeed biblio biblio-core mu4e-maildirs-extension mu4e-alert ht ob-ipython dash-functional ein skewer-mode websocket js2-mode simple-httpd helm-company helm-c-yasnippet fuzzy deft company-statistics company-quickhelp pos-tip company auto-yasnippet yasnippet ac-ispell auto-complete ox-reveal org-gcal request-deferred deferred org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
