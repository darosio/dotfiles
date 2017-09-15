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
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-private-snippets-directory "~/Sync/.yasnippets")
     ;; better-defaults
     bibtex
     (colors :variables
             colors-enable-nyan-cat-progress-bar t
             colors-enable-rainbow-identifiers t)
     csv
     dash
     deft
     emacs-lisp
     (elfeed :variables rmh-elfeed-org-files (list "~/Sync/.elfeed/1.org"))
     fasd
     (git :variables
          git-gutter-use-fringe t)
     graphviz
     helm
     html
     (mu4e :variables mu4e-enable-mode-line t)
     markdown
     (org :variables
          org-enable-reveal-js-support t
          org-reveal-root "/home/examples/reveal.js"
          org-enable-bootstrap-support t)
     (plantuml :variables
               org-plantuml-jar-path "/opt/plantuml/plantuml.jar"
               plantuml-jar-path "/opt/plantuml/plantuml.jar")
     ;; pip install --user yapf autoflake isort hy
     (python :variables
             python-test-runner 'pytest)
             ;; python-test-runner '(pytest nose))
     ;; (shell :variables
     ;;        ;; shell-default-height 30
     ;;        ;; shell-default-position 'bottom
     ;;        shell-default-shell 'eshell)
     shell-scripts
     ;; -S flake8 python-pylint
     syntax-checking
     (spell-checking :variables
                     spell-checking-enable-auto-dictionary t
                     spell-checking-enable-by-default nil
                     enable-flyspell-auto-completion t)
     (typography :variables typography-enable-typographic-editing t)
     version-control
     (version-control :variables
                       version-control-diff-tool 'diff-hl
                       version-control-global-margin t)
     themes-megapack
     pdf-tools
     (ranger :variables
             ranger-show-preview t)
     pandoc
     ipython-notebook
     ess
     yaml
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(org-gcal
                                      visual-fill-column
                                      ;; org-webpage
                                      langtool
                                      writegood-mode
                                      artbollocks-mode
                                      ob-ipython
                                      helm-mu
                                      synonymous
                                      wordnut
                                      adaptive-wrap
                                      synosaurus
                                      outline-magic
                                      interleave)
                                      ;; zotxt)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

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
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
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
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '(;;(todos . 3)
                                (bookmarks .7)
                                (recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(farmhouse-dark
                         hemisu-dark
                         hemisu-light
                         alect-light
                         farmhouse-light
                         birds-of-paradise-plus
   ;;                       spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Dejavu Sans Mono"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
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
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
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
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
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
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
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
   ;; If non nil, advise quit functions to keep server open when quitting.
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
you should place your code here."

   (setq google-translate-translation-directions-alist '(("it" . "en") ))
   ;; my personal config
   (push "~/.spacemacs.d/config/" load-path)
   (require 'mu4e-config nil t)
   (require 'org-config nil t)
   (require 'elfeed-config nil t)
   (require 'bibtex-config nil t)
   (setq deft-directory "~/Sync/notes")
   (setq deft-extensions '("org" "md" "txt" "markdown"))
   (setq deft-recursive t)
   (setq org-confirm-babel-evaluate 'never)

   (require 'ob-ipython)
   ;; don't prompt me to confirm everytime I want to evaluate a block
   (setq org-confirm-babel-evaluate nil)
   ;; display/update images in the buffer after I evaluate
   (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

   ;; (require 'publish-config)
   ;; (require 'org-webpage)
   ;; (owp/add-project-config
   ;;  '("darosio.bitbucket.io"
   ;;    :repository-directory "~/workspace/arte/Web/darosio.bitbucket.io"
   ;;    :remote (git "ssh://git@bitbucket.org/darosio/darosio.bitbucket.io.git" "master")
   ;;    ;; you can use `rclone` with `:remote (rclone "remote-name" "/remote/path/location")` instead.
   ;;    :site-domain "https://darosio.bitbucket.io/"
   ;;    :site-main-title "Life, Matters and metaModels"
   ;;    :site-sub-title "(Musings and ramblings through the world of Python and beyond)"
   ;;    ;; :theme (worg)
   ;;    :source-browse-url ("Sources" "https://bitbucket.org/darosio/darosio.bitbucket.io")
   ;;    :personal-avatar "~/workspace/arte/Web/darosio.bitbucket.io/IMG_0614.JPG"
   ;;    ;; :personal-duoshuo-shortname "tumashu-website"
   ;;    :web-server-port 7654))

   ;; (setq op/site-domain "https://darosio.bitbucket.io")
   ;; ;; (setq op/personal-github-link "https://github.com/CodyReichert")
   ;; (setq op/site-main-title "The One True Blog")
   ;; (setq op/site-sub-title "Emacs, Programming, and Arch Linux")
   ;; ;; (setq op/personal-disqus-shortname "theonetrueblog")

   (org-babel-do-load-languages 'org-babel-load-languages '(
                                                                           (plantuml . t)
                                                                           (python . t)
                                                                           (ipython . t)
                                                                           (gnuplot . t)
                                                                           (R . t)
                                                                           (shell . t)))
   (setq visual-fill-column-center-text t
         ;; split-window-preferred-function 'visual-fill-column-split-window-sensibly
         visual-fill-column-fringes-outside-margins t)
   ;; (global-visual-fill-column-mode)
   ;; activate koma scripts export
   (eval-after-load 'ox '(require 'ox-koma-letter))
   ;; http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/
   ;; unset C- and M- digit keys
   (dotimes (n 10)
     (global-unset-key (kbd (format "C-%d" n)))
     (global-unset-key (kbd (format "M-%d" n)))
     )
   ;; set up my own map
   (define-prefix-command 'bjm-map)
   (global-set-key (kbd "C-1") 'bjm-map)
   ;; (define-key bjm-map (kbd "a") 'org-agenda)
   (define-key bjm-map (kbd "a") (lambda () (interactive) (org-agenda nil " ")))
   (define-key bjm-map (kbd "f") 'bjm/elfeed-load-db-and-open)
   (define-key bjm-map (kbd "m") 'mu4e)
   (define-key bjm-map (kbd "n") 'deft)
   (define-key bjm-map (kbd "t") 'org-capture)
   ;; (require 'langtool)
   ;; (setq langtool-language-tool-jar "/usr/share/java/languagetool/languagetool.jar")
   (setq langtool-java-classpath "/usr/share/java/languagetool/*")
   ;; (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")

   ;; ease insertion of INTERLEAVE_PDF property
   (add-to-list 'org-structure-template-alist '("I" ":INTERLEAVE_PDF: ?"))

   ;; Folding with tab similar to org-mode
   (add-hook 'bibtex-mode-hook 'outline-minor-mode)
   (add-hook 'python-mode-hook 'outline-minor-mode)
   (eval-after-load 'outline
     '(progn
        (require 'outline-magic)
        (define-key outline-minor-mode-map (kbd "<tab>") 'outline-cycle)))

   ;; Export from org to latex with bibliography


   (setq org-export-latex-hyperref-format "\\ref{%s}")


   (setq org-latex-pdf-process
         '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
   (require 'ox-latex)
   (add-to-list 'org-latex-classes
                '("koma-article"
                  "\\documentclass{scrartcl}
                \\usepackage{microtype}
                \\usepackage{tgtermes}
                \\usepackage[scale=.9]{tgheros}
                \\usepackage{tgcursor}
                \\usepackage{paralist}
                \\newcommand{\\rc}{$^{14}C$}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

   ;; (add-hook 'elfeed-show-mode-hook (lambda () (buffer-face-set 'variable-pitch)))
   (progn
     ;; use variable-width font for some modes
     (defun xah-use-variable-width-font ()
       "Set current buffer to use variable-width font."
       (variable-pitch-mode 1)
       ;; (text-scale-increase 0.5 )
       )
     (add-hook 'elfeed-show-mode-hook 'xah-use-variable-width-font)
     ;; (add-hook 'emacs-lisp-mode-hook 'xah-use-variable-width-font)
     )
   (add-to-list 'load-path "~/.spacemacs.d/")
   (require 'diction)

   (global-set-key [f11] 'synonymous-synonyms)
   (global-set-key [f12] 'wordnut-lookup-current-word)
   (global-set-key [(control f12)] 'wordnut-search)
   (require 'helm-bookmark)
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable star definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (s elfeed iedit deferred alert org-category-capture websocket avy git-commit async magit-popup synonymous wordnut helm-wordnet helm-bibtex packed artbollocks-mode writegood-mode langtool git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter diff-hl flyspell-popup pdf-tools ivy markdown-mode flycheck-pos-tip skewer-mode js2-mode ess smartparens evil yasnippet company helm helm-core projectile magit with-editor org-plus-contrib hydra dash flycheck xterm-color shell-pop multi-term insert-shebang fish-mode eshell-z eshell-prompt-extras esh-help company-shell company-quickhelp pos-tip ox-twbs fasd zeal-at-point helm-dash rainbow-mode rainbow-identifiers color-identifiers-mode synosaurus zonokai-theme zenburn-theme zen-and-art-theme yapfify yaml-mode ws-butler winum which-key web-mode volatile-highlights visual-fill-column vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme typo twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spray spaceline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode seti-theme scss-mode sass-mode reverse-theme restart-emacs ranger rainbow-delimiters railscasts-theme pyvenv pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme plantuml-mode planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox pandoc-mode ox-reveal ox-pandoc orgit organic-green-theme org-webpage org-ref org-projectile org-present org-pomodoro org-gcal org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-ipython noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme mu4e-maildirs-extension mu4e-alert move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme macrostep lush-theme lorem-ipsum live-py-mode linum-relative link-hint light-soap-theme less-css-mode jbeans-theme jazz-theme ir-black-theme interleave inkpot-theme info+ indent-guide hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mu helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme graphviz-dot-mode grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md gandalf-theme fuzzy flyspell-correct-helm flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu ess-smart-equals ess-R-object-popup ess-R-data-view espresso-theme engine-mode emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies ein dumb-jump dracula-theme django-theme deft define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode company-web company-statistics company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
