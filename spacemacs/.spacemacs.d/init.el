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
     python
     (python :variables python-test-runner 'pytest)
     ;(python :variables python-test-runner '(pytest nose))
     yaml
     csv
     html
     markdown
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     auto-completion
     ;; better-defaults
     emacs-lisp
     ;; git
     ;; markdown
     org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     spell-checking
     ;; syntax-checking
     ;; version-control
     mu4e
     (mu4e :variables mu4e-enable-mode-line t)
     speed-reading
     deft
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(org-gcal)
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
   dotspacemacs-check-for-update nil
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
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Hack"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
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
   ;; (setq org-agenda-include-diary t)
   ;; (setq org-agenda-include-all-todo t) ;;only in http://sachachua.com/blog/tag/gtd/#post-4543
   (setq-default org-directory "~/Sync/notes/")
   ;(setq org-agenda-files (quote ("~/Sync/notes"

   (setq org-agenda-files (quote ("~/Sync/share/phone/box/notes/todo.org"
   								  "~/Sync/share/phone/box/notes/someday.org"
								  "~/Sync/share/phone/box/notes/weekly_review.org"
                                  "~/Sync/notes/home"
                                  "~/Sync/notes/gcal"
                                  "~/Sync/notes/arch")))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; STATES - todo keywords - TYP_TODO
   (setq org-todo-keywords
         (quote ((sequence "TODO(t)" "NEXT(n)" "APPT(a)" "|" "DONE(d)")
                 (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
              ;; (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
   (setq org-todo-keyword-faces
         (quote (;("TODO" :foreground "red" :weight bold)
                 ("NEXT" :foreground "light blue" :weight bold)
                 ("APPT" :foreground "yellow" :weight bold)
                 ("DONE" :foreground "forest green" :weight bold)
                 ("WAITING" :foreground "orange" :weight bold)
                 ("HOLD" :foreground "magenta" :weight bold)
                 ("CANCELLED" :foreground "forest green" :weight bold)
                 ;; ("MEETING" :foreground "forest green" :weight bold)
                 ;; ("PHONE" :foreground "forest green" :weight bold)
                 )))
   ;; change todo states pressing "t" (default)
   (setq org-use-fast-todo-selection t)
   ;; S-left S-right skipping setting timespamps 
   (setq org-treat-S-cursor-todo-selection-as-state-change nil)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; TODO states tigger tags
   (setq org-todo-state-tags-triggers
         (quote (("CANCELLED" ("CANCELLED" . t))
                 ("WAITING" ("WAITING" . t))
                 ("HOLD" ("WAITING") ("HOLD" . t))
                 (done ("WAITING") ("HOLD"))
                 ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                 ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                 ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
   ;; Org Capture
(setq org-default-notes-file "~/org/refile.org")
;;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
   (setq org-capture-templates
         (quote (("t" "todo" entry (file+headline "~/Sync/share/phone/box/notes/todo.org" "Inbox")
                  "* TODO %? \n%U\n%a\n" :clock-in t :clock-resume t)
                 ;; "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))
                 ;; "* TODO [#A] %? :INBOX:")))
                 ("i" "idea" entry (file+headline "~/Sync/share/phone/box/notes/someday.org" "Ideas")
                  "* IDEA [#B] %? :IDEA:")
				 ("b" "Blank" entry (file org-default-notes-file) "* %?\n%u")
                 ("a" "Appointment" entry (file  "~/Sync/notes/gcal/dpa.org" )
	                  "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
                 ("r" "respond" entry (file+headline "~/Sync/notes/todo.org" "Reply")
                  "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
				 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
				  "** NEXT %? \nDEADLINE: %t")
                 ("w" "org-protocol" entry (file+headline "~/Sync/notes/todo.org" "Inbox")
                                        "* TODO Review %c\n%U\n" :immediate-finish t)
                 )))
              ;("n" "note" entry (file "~/git/org/refile.org")
               ;"* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ;("j" "Journal" entry (file+datetree "~/git/org/diary.org")
               ;"* %?\n%U\n" :clock-in t :clock-resume t)
              ;("w" "org-protocol" entry (file "~/git/org/refile.org")
               ;"* TODO Review %c\n%U\n" :immediate-finish t)
              ;("m" "Meeting" entry (file "~/git/org/refile.org")
               ;"* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ;("p" "Phone call" entry (file "~/git/org/refile.org")
               ;"* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ;("h" "Habit" entry (file "~/git/org/refile.org")
               ;"* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Refile setup
   ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
   (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                    (org-agenda-files :maxlevel . 9))))
    ;(setq org-refile-targets (quote (("~/Sync/notes/todo.org" :maxlevel . 1) 
                                 ;("~/Sync/notes/someday.org" :level . 2))))
   ; Use full outline paths for refile targets - we file directly with IDO
   (setq org-refile-use-outline-path t)
   ; Targets complete directly with IDO
   (setq org-outline-path-complete-in-steps nil)
   ; Allow refile to create parent tasks with confirmation
   (setq org-refile-allow-creating-parent-nodes (quote confirm))
   ;; ; Use IDO for both buffer and file completion and ido-everywhere to t
   ;; (setq org-completion-use-ido t)
   ;; (setq ido-everywhere t)
   ;; (setq ido-max-directory-size 100000)
   ;; (ido-mode (quote both))
   ;; ; Use the current window when visiting files and buffers with ido
   ;; (setq ido-default-file-method 'selected-window)
   ;; (setq ido-default-buffer-method 'selected-window)
   ;; ; Use the current window for indirect buffer display
   ;; (setq org-indirect-buffer-display 'current-window)
   ;; ; Exclude DONE state tasks from refile targets
   ;; (defun bh/verify-refile-target ()
   ;;   "Exclude todo keywords with a done state from refile targets"
   ;;   (not (member (nth 2 (org-heading-components)) org-done-keywords)))
   ;; (setq org-refile-target-verify-function 'bh/verify-refile-target)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Calendars
   ;; gcalendar
   (require 'org-gcal)
   (setq org-gcal-client-id "100447390762-1unqkpjv30do2uq0r5uetd8pkr9ha81s.apps.googleusercontent.com"
         org-gcal-client-secret "EhuKxnNmAoM2XMPjCFOpiLDK"
         org-gcal-file-alist '(("danielepietroarosio@gmail.com" .  "~/Sync/notes/gcal/dpa.org")
                               ("c87gevr5pc3191on8c7nh8b4nc@group.calendar.google.com" .
                                "~/Sync/notes/gcal/figli.org")
           ;                    ("cfaned8dou8gm2qciies0itso4@group.calendar.google.com" .
            ;                    "~/Sync/notes/gcal/deadlines.org")
             ;                  ("tq1af7efj4l9h8glgqi2g5vmsg@group.calendar.google.com" .
              ;                  "~/Sync/notes/gcal/IBF.org")
                               ;; ("i_217.77.81.46#sunrise@group.v.calendar.google.com" .
                               ;;  "~/Sync/notes/gcal/sunrise.org")
                               ;; ("it.italian#holiday@group.v.calendar.google.com" .
                               ;;  "~/Sync/notes/gcal/feste.org")
                               ))
   ;; syncs whenever I load the agenda. Since this happens in the background,
   ;; if I just added something to my calendar, I might have to reload the agenda by hitting r in the agenda view.
   ;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
   ;; syncs with my Google calendar when I capture.
   ;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
   (setq org-agenda-custom-commands
         '(("c" "Simple agenda view"
      	 ((agenda "")
      	  (alltodo "")))))
   ;; Sunrise and sunset
   (setq calendar-latitude 46.067270) ; Borino
   (setq calendar-longitude 11.166153)
   (setq calendar-location-name "Trento")
   (setq calendar-time-zone 60)
   ;; Holidays (setq holiday-general-holidays '(
   (setq holiday-other-holidays
         '((holiday-fixed 1 1 "Capodanno")
           (holiday-fixed 5 1 "1 Maggio")
           (holiday-fixed 4 25 "Liberazione")
           (holiday-fixed 6 2 "Festa Repubblica")
           (holiday-fixed 7 14 "Bastille Day")
           ))
   (setq holiday-bahai-holidays nil)
   (setq holiday-hebrew-holidays nil)
   ;; (setq holiday-islamic-holidays nil)
   (setq holiday-christian-holidays
         '((holiday-fixed 12 8 "Immacolata Concezione")
           (holiday-fixed 12 25 "Natale")
           (holiday-fixed 12 26 "Santo Stefano")
           (holiday-fixed 1 6 "Epifania")
           (holiday-easter-etc -52 "Giovedì grasso")
           (holiday-easter-etc -47 "Martedì grasso")
           (holiday-easter-etc  -2 "Venerdì Santo")
           (holiday-easter-etc   0 "Pasqua")
           (holiday-easter-etc  +1 "Lunedì Pasqua")
           (holiday-fixed 8 15 "Assunzione di Maria")
           (holiday-fixed 11 1 "Ognissanti")
           ))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Tags with fast selection keys
   (setq org-tag-alist (quote ((:startgroup)
                               ("@errand" . ?e)
                               ("@office" . ?o)
                               ("@home" . ?h)
                               (:endgroup)
                               ("@pc" . ?p)
                               ("WAITING" . ?w)
                               ("HOLD" . ?h)
                               ("PERSONAL" . ?P)
                               ("WORK" . ?W)
                               ("NOTE" . ?n)
                               ("CANCELLED" . ?c)
                               ("FLAGGED" . ??))))
   ;; Allow setting single tags without the menu
   (setq org-fast-tag-selection-single-key (quote expert))
   ;; For tag searches ignore tasks with scheduled and deadline dates
   ;; (setq org-agenda-tags-todo-honor-ignore-options t)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; maybe exist: ", a n"  and  "Esc SPC"
   ;; (global-set-key (kbd "C-c a H") 'org-capture)
   (global-set-key (kbd "<f7> <f8>") 'calendar)    ; F7 F8
   ;; (define-key evil-insert-state-map (kbd "SPC a o H") 'forward-char)
   ;; (with-eval-after-load 'org-agenda '((agenda)
   ;;                                    (tags-todo "@office")
   ;;                                    (tags-todo "@home")
   ;;                                    (tags-todo "@pc")
   ;;                                    (tags-todo "WORK")
   ;;                                    (tags-todo "emacs")))
     ;; (require 'org-projectile)
     ;; (push (org-projectile:todo-files) org-agenda-files))
   ;; (global-set-key (kbd "SPC a o H") '((agenda)
   ;;                                  (tags-todo "@office")
   ;;                                  (tags-todo "@home")
   ;;                                  (tags-todo "@pc")
   ;;                                  (tags-todo "WORK")
   ;;                                  (tags-todo "emacs")))
   ;; ("H" "Office and Home Lists"
   ;;  ((agenda)
   ;;   (tags-todo "@office")
   ;;   (tags-todo "@home")
   ;;   (tags-todo "@pc")
   ;;   (tags-todo "WORK")
   ;;   (tags-todo "emacs")))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; my personal config
   (push "~/.spacemacs.d/config/" load-path)
   (require 'mu4e-config nil t)
   (setq deft-directory "~/Sync/notes")
   (setq deft-extensions '("org" "md" "txt" "markdown"))
   (setq deft-recursive t)
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
    (org-gcal request-deferred deferred helm-company helm-c-yasnippet company-web web-completion-data company-statistics company-anaconda company auto-yasnippet yasnippet ac-ispell auto-complete deft yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode anaconda-mode pythonic yaml-mode csv-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode flyspell-correct-helm flyspell-correct auto-dictionary mmm-mode markdown-toc markdown-mode gh-md spray mu4e-alert ht org-projectile org-present org org-pomodoro alert log4e gntp org-download htmlize gnuplot ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe mu4e-maildirs-extension uuidgen use-package toc-org spacemacs-theme spaceline restart-emacs request rainbow-delimiters quelpa popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
