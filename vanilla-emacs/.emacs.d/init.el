;;; package --- Summary my configuration init.el
;;; Commentary:
;; https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; https://blog.jft.rocks/emacs/emacs-from-scratch.html
;; https://gist.github.com/huytd/6b785bdaeb595401d69adc7797e5c22c for lsp-mode
;;;  new important ref https://sriramkswamy.github.io/dotemacs/#orgheadline278

;; Binding keys reserved to user: "C-c <letter>" and <F5> to <F9>.
;; https://github.com/lccambiaghi/vanilla-emacs
;; 
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
;; We use Straight, we must prevent Emacs from doing early package initialization.
(setq package-enable-at-startup nil)
;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)
;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)
;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)
;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
;; (setq comp-deferred-compilation nil)	; XXX:
(declare-function doom-reset-file-handler-alist-h "init.el")
;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-
;; `file-name-handler-alist' is consulted on every `require', `load' and various
;; path/io functions. You get a minor speed up by nooping this. However, this
;; may cause problems on builds of Emacs where its site lisp files aren't
;; byte-compiled and we're forced to load the *.el.gz files (e.g. on Alpine)
(unless (daemonp)
  (defvar doom--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist' later, because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (defun doom-reset-file-handler-alist-h ()
    ;; Re-add rather than `setq', because changes to `file-name-handler-alist'
    ;; since startup ought to be preserved.
    (dolist (handler file-name-handler-alist)
      (add-to-list 'doom--initial-file-name-handler-alist handler))
    (setq file-name-handler-alist doom--initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h)
  (add-hook 'after-init-hook '(lambda ()
                                ;; restore after startup
                                (setq gc-cons-threshold 16777216
                                      gc-cons-percentage 0.1))))
;; Ensure Doom is running out of this file's directory
(setq user-emacs-directory (file-truename (file-name-directory load-file-name)))

;;; Code:
(setq debug-on-error t)
(setq debug-on-quit t)
(defconst emacs-start-time (current-time))

(progn                                  ; Base UI
  ;; (setq ad-redefinition-action 'accept) ; to silent a defadvice warning in
  ;; XXX:                                   ; pcre2el (required by magit-todos)
  (setq image-use-external-converter t) ;27.1 viewer don't display many png
  (setq-default cursor-in-non-selected-windows nil
                cursor-type '(bar . 3)
                echo-keystrokes 0.1
                enable-recursive-minibuffers t
                fill-column 76
                ;; gc-cons-threshold 2000000 ; (* 500 1024 1024)
                indent-tabs-mode t
                tab-always-indent 'complete ; see company completion
                indicate-empty-lines t
                inhibit-startup-screen t
                major-mode 'text-mode
                resize-mini-windows t
                ring-bell-function 'ignore
                truncate-lines t			; truncating lines
                scroll-margin 3
                scroll-step 1
                sentence-end-double-space nil
                tab-width 4
                show-paren-delay 0.05 		; Show matching parenthesis
                )
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8-auto-unix)
  ;; (setq coding-system-for-read 'utf-8   ; use utf-8 by default to read
  ;;       coding-system-for-write 'utf-8  ; use utf-8 by default to write
  (setq x-stretch-cursor t)
  (setq font-lock-maximum-decoration t)
  (tooltip-mode    0)
  (show-paren-mode 1)                   ; highlight parenthesis
  (save-place-mode 1)                   ; remember last position in file
  (blink-cursor-mode 1)                 ; Don't blink the cursor
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-hl-line-mode)
  ;; XXX: custom tramp gdb
  (put 'narrow-to-region 'disabled nil)	; narrow to region =C-x n n=
  )

(defmacro csetq (sym val)
  "Define variables: SYM VAL."
  `(funcall (or (get ',sym 'custom-set) 'set-default) ',sym ,val))

(progn                                  ; Package configuration
  ;; straight
  (csetq straight-vc-git-default-clone-depth 1)
  (csetq straight-recipes-gnu-elpa-use-mirror t)
  (defvar bootstrap-version)
  (let ((bootstrap-file
		 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
		(bootstrap-version 5))
	(unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
		(goto-char (point-max))
		(eval-print-last-sexp)))
	(load bootstrap-file nil 'nomessage))
  
  (declare-function straight-use-package "straight")
  (straight-use-package 'use-package)
  (eval-when-compile
    (require 'use-package))
  ;; list-load-path-shadows built-in org
  (straight-use-package 'org)

  (eval-and-compile
	;; This is a variable that has been renamed but straight still refers when
	;; doing :straight (:no-native-compile t)
	;; (setq comp-deferred-compilation-black-list nil) ; He need it only for jupyter
	(setq-local straight-use-package-by-default t)
	;; (setq straight-cache-autoloads t)		; XXX:
	(setq straight-check-for-modifications '(watch-files find-when-checking))
	)
  (if (daemonp)
	  (progn (csetq use-package-always-demand t)
			 (setenv "EDITOR" "emacsclient"))
	(csetq use-package-always-defer t))

  (require 'package)
  (setq package-archives
		'(("melpa-stable" . "https://stable.melpa.org/packages/")
		  ("melpa" . "https://melpa.org/packages/")
		  ("gnu"   . "http://elpa.gnu.org/packages/")
		  ("nongnu"   . "http://elpa.nongnu.org/nongnu/")))

  (use-package use-package
	:straight use-package
	:config
	(setq use-package-compute-statistics t)
	(setq use-package-verbose t)
	(setq use-package-hook-name-suffix nil)
	(setq use-package-enable-imenu-support t))
  (use-package use-package-ensure-system-package)
  (use-package diminish)
  (use-package async)
  (use-package paradox
	:config
	(setq paradox-github-token "abc5e1c6710cee61646f0952091bae7b825852f3"
          paradox-automatically-star t
          paradox-execute-asynchronously t))
  )
(progn                                  ; UI more setting
  (use-package bookmark                 ; persistent bookmarks
    :straight nil
	:init (setq bookmark-save-flag 2
				;; to avoid sync conflicts in ~/Sync/.emacs
				bookmark-default-file "~/.emacs.d/bookmarks"))
  (use-package ediff                    ; Fix diff behavior
    :config
    (use-package ediff-wind
      :straight ediff
      :functions (ediff-setup-windows-plain)
      :init (setq ediff-window-setup-function #'ediff-setup-windows-plain
                  ediff-split-window-function #'split-window-right
                  ediff-diff-options "-w")))
  (progn                                ; printing
    (setq lpr-command "gtklp")
    ;; (setq ps-lpr-command "gtklp")
    ;; (progn					; printing; need: gv, ghostscript
    ;;   (require 'printing)		; load printing package
    ;;   ;; (setq pr-path-alist
    ;;   ;; 	'((unix      "." "~/bin" ghostview mpage PATH)
    ;;   ;; 	  (ghostview "$HOME/bin/gsview-dir")
    ;;   ;; 	  (mpage     "$HOME/bin/mpage-dir")
    ;;   ;; 	  ))
    ;;   (setq pr-txt-name      'prt_06a)
    ;;   (setq pr-txt-printer-alist
    ;; 	'((cc "lpr" nil "cc")
    ;; 	  (prt_07c nil   nil "prt_07c")
    ;; 	  ))
    ;;   (setq pr-ps-name       'cc)
    ;;   (setq pr-ps-printer-alist
    ;; 	'((cc "lpr" nil "-P" "cc")
    ;; 	  (lps_07c "lpr" nil nil  "lps_07c")
    ;; 	  (lps_08c nil   nil nil  "lps_08c")
    ;; 	  ))
    ;;   (pr-update-menus t)		; update now printer and utility menus
    ;;   )
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun mk-set-font (font &optional height)
    "Set font FONT as main font for all frames.
HEIGHT, if supplied, specifies height of letters to use."
    (interactive
     (list (completing-read "Use font: " (font-family-list)) nil))
    (set-face-attribute 'default nil :family font)
    (when height
      (set-face-attribute 'default nil :height height))
    (set-face-attribute 'variable-pitch nil :family font))
  (global-set-key (kbd "<f14> o f") #'mk-set-font)
  (straight-use-package 'f)
  (require 'f)
  ;; (use-package f :demand t)
  ;; (declare-function f-expand "f")
  (use-package files
	:straight (:type built-in)
    :init
    (setq auto-save-default nil	       ; stop creating #autosave# files
          auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
          backup-by-copying t	       ; don't clobber symlinks
          ;; make-backup-files nil
          ;; create-lockfiles nil
          ;; backup-directory-alist '(("." . "~/.emacs.d/backup"))
          backup-directory-alist (list (cons "." (f-expand "backups" user-emacs-directory)))
          delete-old-versions t
          kept-new-versions 4
          kept-old-versions 2
          ;; large-file-warning-threshold (* 15 1024 1024) ; large file warning
          large-file-warning-threshold 10240000
          require-final-newline t
          vc-display-status t
          vc-follow-symlinks t	      ; follow symlinks without asking
          version-control t)
    :config
    (advice-add 'revert-buffer :filter-args (lambda (&rest _rest) (list nil t)))
    :bind
    ("<f14> B" . revert-buffer)
    :hook
    ;; (before-save . whitespace-cleanup) ; not compatible with deft
    (after-save-hook . executable-make-buffer-file-executable-if-script-p)
    ;; rend les scripts executable par défault si c'est UN script.
    ;;    (lambda () (executable-make-buffer-file-executable-if-script-p)))
    )
  (use-package simple
	:straight (:type built-in)
    :init
    (setq
     blink-matching-delay 0.5
     blink-matching-paren 'jump-offscreen
     kill-read-only-ok t
     suggest-key-bindings nil)
    :preface
    (defun mk-auto-fill-mode ()
      "Enable ‘auto-fill-mode’ limiting it to comments."
      (setq-local comment-auto-fill-only-comments t)
      (auto-fill-mode 1))
    :config
    (column-number-mode 1)
    :bind
    ("C-z" . undo)
    ;; ("C-j" . newline)
    ("C-c q" . auto-fill-mode)
    ("M-h" . mark-word)
    ("M-S-h" . mark-paragraph)
    ;; ("<up>" . beginning-of-buffer)
    ;; ("<down>" . end-of-buffer)
    ;; ("<home>" . find-file)
    ;; ("<end>" . save-buffer)
    ;; ("<next> a f" . auto-fill-mode)
    :hook
    ((gitignore-mode-hook . mk-auto-fill-mode)
     (haskell-cabal-mode-hook . mk-auto-fill-mode)
     (prog-mode-hook . mk-auto-fill-mode)
     (proof-mode-hook . mk-auto-fill-mode)
     ;; (text-mode . auto-fill-mode)
     (yaml-mode-hook . mk-auto-fill-mode)))
  (use-package unfill
    :bind
    ("M-Q" . unfill-toggle))
  (use-package isearch
	:straight (:type built-in)
    :config
    (setq isearch-allow-scroll t))
  (use-package delsel
    :config
    (delete-selection-mode 1))
  (use-package fix-word
    :bind
    ("M-c" . fix-word-capitalize)
    ("M-l" . fix-word-downcase)
    ("M-u" . fix-word-upcase))
  (use-package rainbow-delimiters
    :hook
    ((emacs-lisp-mode-hook . rainbow-delimiters-mode)))
  (use-package electric
    :config
    (electric-indent-mode 0)
    :hook (python-mode-hook . electric-indent-mode))
  ;; python is excluded by aggressive indent because of not absolute indentation
  (use-package aggressive-indent
    :bind
    ("<f14> t i" . aggressive-indent-mode)
    :hook
    ((emacs-lisp-mode-hook . aggressive-indent-mode)
     (html-mode-hook . aggressive-indent-mode)))
  (use-package which-key                ; needed here by which-key replacements
	:commands (which-key-mode
			   which-key-add-key-based-replacements)
    :diminish
    :init (which-key-mode 1)
    :config (setq which-key-idle-delay 0.05))
  (use-package visual-fill-column
	:commands (visual-fill-column-split-window-sensibly
			   visual-fill-column-adjust) ;; although are functions
    :bind
    ("<f14> v" . visual-fill-column-mode)
    ("<f12>" . no-distraction-enable)
    ("<C-f12>" . no-distraction-disable)
    :preface
    (defun no-distraction-enable ()
      "Switch to no distraction env"
      (interactive)
      (visual-fill-column-mode)
      (text-scale-increase 2)
      ;; (wc-mode)
      )
    (defun no-distraction-disable ()
      "Switch off from no distraction env"
      (interactive)
      (visual-fill-column-mode -1)
      (text-scale-set 0)
      ;; (wc-mode -1)
      )
    :init
    (which-key-add-key-based-replacements "<f14> v" "Visual-fill-column")
    (setq visual-fill-column-center-text t
          visual-fill-column-width 98
          visual-fill-column-fringes-outside-margins nil
          ;; set right curly arrow even when visual line mode is wrapping logical lines into visual ones.
          visual-line-fringe-indicators '(bottom-left-angle top-right-angle)
          ;; allow splitting windows with wide margins
          split-window-preferred-function #'visual-fill-column-split-window-sensibly)
    ;; :config
    ;; adjust margins upon text resize
    (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
    :hook
    (visual-fill-column-mode-hook . visual-line-mode))
  (use-package window
	:straight (:type built-in)
    :defer t
    :preface
    (defvar prot/window-configuration nil
      "Current window-monocle configuration.")
    (declare-function one-window-p "window")
    (defun prot/window-single-toggle ()
      "Monocle toggle. Substitute zygospore."
      (interactive)
      (if (one-window-p)
          (when prot/window-configuration
            (set-window-configuration prot/window-configuration))
        (setq prot/window-configuration (current-window-configuration))
        (delete-other-windows)))
    :init
    (setq display-buffer-alist
          '(;; top side window
            ("\\*\\(Flymake\\|Package-Lint\\|vc-git :\\).*"
             (display-buffer-in-side-window)
             (window-height . 0.16)
             (side . top)
             (slot . 0)
             (window-parameters . ((no-other-window . t))))
            ("\\*Messages.*"
             (display-buffer-in-side-window)
             (window-height . 0.16)
             (side . top)
             (slot . 1)
             (window-parameters . ((no-other-window . t))))
            ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
             (display-buffer-in-side-window)
             (window-height . 0.16)
             (side . top)
             (slot . 2)
             (window-parameters . ((no-other-window . t))))
            ;; bottom side window
            ("\\*\\(Output\\|Register Preview\\).*"
             (display-buffer-in-side-window)
             (window-width . 0.16)       ; See the :hook
             (side . bottom)
             (slot . -1)
             (window-parameters . ((no-other-window . t))))
            (".*\\*\\(Completions\\|Embark Live Occur\\).*"
             (display-buffer-in-side-window)
             (window-height . 0.16)
             (side . bottom)
             (slot . 0)
             (window-parameters . ((no-other-window . t))))
            ("^\\(\\*e?shell\\|vterm\\).*"
             (display-buffer-in-side-window)
             (window-height . 0.16)
             (side . bottom)
             (slot . 1))
            ;; left side window
            ("\\*Help.*"
             (display-buffer-in-side-window)
             (window-width . 0.30)       ; See the :hook
             (side . left)
             (slot . 0))
            ;; right side window
            ("\\*Faces\\*"
             (display-buffer-in-side-window)
             (window-width . 0.25)
             (side . right)
             (slot . 0)
             (window-parameters . ((no-other-window . t)
                                   (mode-line-format . (" "
                                                        mode-line-buffer-identification)))))
            ("\\*Custom.*"
             (display-buffer-in-side-window)
             (window-width . 0.25)
             (side . right)
             (slot . 1))
            ;; bottom buffer (NOT side window)
            ("\\*\\vc-\\(incoming\\|outgoing\\).*"
             (display-buffer-at-bottom))
            ("\\*Embark Occur.*"
             (display-buffer-at-bottom))))
    (setq window-combination-resize t)
    (setq even-window-sizes 'height-only)
    (setq window-sides-vertical nil)
    ;; Note that the the syntax for `use-package' hooks is controlled by
    ;; the `use-package-hook-name-suffix' variable.  The "-hook" suffix is
    ;; not an error of mine.
    :hook ((help-mode-hook . visual-line-mode)
           (Custom-mode-hook . visual-line-mode))
    :bind (("H-n" . next-buffer)
           ("H-p" . previous-buffer)
           ("H-o" . other-window)
           ("H-2" . split-window-below)
           ("H-3" . split-window-right)
           ("H-0" . delete-window)
           ("H-1" . delete-other-windows)
           ("H-5" . delete-frame)
           ("H-{" . shrink-window-horizontally)
           ("H-}" . enlarge-window-horizontally)
           ("H-[" . shrink-window)
           ("H-]" . enlarge-window)
           ("H-=" . balance-windows-area)
           ("H-m" . prot/window-single-toggle)
           ("H-s" . window-toggle-side-windows)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package frame                   ;window split more consistent? ;;
	:straight (:type built-in)
    :config                                                            ;;
    (setq window-divider-default-right-width 1)                        ;;
    (setq window-divider-default-bottom-width 1)                       ;;
    (setq window-divider-default-places 'right-only)                   ;;
    :hook (after-init-hook . window-divider-mode))                     ;;
  (use-package browse-url
	:straight (:type built-in)
	:config
	(setq browse-url-browser-function 'browse-url-generic)
	(setq browse-url-generic-program "firefox")
	)
  )
(progn                                  ; single packages
  (use-package crux
	:bind
	("H-o" . crux-smart-open-line)
	("H-O" . crux-smart-open-line-above)
	("C-c M-d" . crux-duplicate-and-comment-current-line-or-region))
  (use-package recentf 					; enable recent files mode.
	:config
	(recentf-mode t)
	(setq recentf-exclude
		  `(,(expand-file-name "straight/build/" user-emacs-directory)
			,(expand-file-name "eln-cache/" user-emacs-directory)
			,(expand-file-name "etc/" user-emacs-directory)
			,(expand-file-name "var/" user-emacs-directory))))
  (use-package key-chord
	:commands (key-chord-mode
			   key-chord-define-global)
    :config
    (key-chord-mode 1))
  (use-package avy                      ; Move around
	;; :init
    ;; (defvar mk-avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s) "Home row Dvorak keys.")
    :bind (("M-g w" . avy-goto-word-1)
           ("M-g g" . avy-goto-line)
           ("M-g c" . avy-goto-char-2)))
  (use-package ace-window
	:demand t
	:bind
	("H-w" . ace-window)
	("C-'" . ace-window)
    ("C-c w" . ace-select-window)
	("C-c W" . ace-swap-window)
	:config
	(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
	(setq aw-scope 'global)				; 'frame
	(setq aw-dispatch-always nil))
  (use-package transpose-frame
	:bind
	("H-e" . transpose-frame))
  (use-package imenu-list               ; F9
    :bind
    ("<f9>" . imenu-list)
    ("<C-f9>" . imenu-list-smart-toggle))
  ;; (setq byte-compile-warnings '(cl-functions)) ; FIXME: when deft update
  (use-package deft                     ; Notes
    :bind
    ("<f14> a n" . deft)
    :config
    (setq deft-directory "~/Sync/notes"
          deft-recursive t
          deft-use-filename-as-title t
          deft-file-naming-rules '((nospace . "_") (case-fn . downcase)) ;; to preserve slashes
          deft-extensions '("org" "md" "markdown")  ;;"txt"
          deft-default-extension "org"
          deft-use-filter-string-for-filename t))
  (use-package flycheck                 ; Syntax checking
    :diminish t
    :bind (
           ("<f14> e l" . flycheck-list-errors)
           ("<f14> e e" . flycheck-mode)
           ("<f14> e b" . flycheck-buffer)
           ("<f14> e d" . flycheck-clear)
           ("<f14> e c" . counsel-flycheck)
           ("<f14> e h" . flycheck-describe-checker)
           ("<f14> e n" . flycheck-next-error)
           ("<f14> e p" . flycheck-previous-error)
           ("<f14> e s" . flycheck-select-checker)
           ("<f14> e S" . flycheck-set-checker-executable)
           ("<f14> e v" . flycheck-verify-setup)
           ("<f14> e y" . flycheck-copy-errors-as-kill)
           ("<f14> e x" . flycheck-explain-error-at-point))
    :init
    (which-key-add-key-based-replacements "<f14> e" "Errors check")
    :config
    ;; https://www.flycheck.org/en/latest/languages.html
	(setq-default flycheck-disabled-checkers '(proselint)) ;will use vale
    (setq flycheck-python-flake8-executable "flake8") ;yay -S flake8
    (setq-default
     flycheck-emacs-lisp-initialize-packages t
     flycheck-emacs-lisp-load-path 'inherit
     flycheck-temp-prefix ".flycheck")
    :hook (
           (gitignore-mode-hook . flycheck-mode)
           (markdown-mode-hook . flycheck-mode)
           (prog-mode-hook . flycheck-mode)
           (yaml-mode-hook . flycheck-mode))
    ;; :custom-face
    ;; (flycheck-fringe-error ((t (:background "#6C3333" :weight bold))))
    )
  ;; (use-package flycheck-pycheckers
  ;;   :config
  ;;   (setq flycheck-pycheckers-venv-root "~/.pyenv/versions")
  ;;   :hook
  ;;   (flycheck-mode-hook . flycheck-pycheckers-setup))
  (use-package doom-modeline
	:demand t
	:commands (doom-modeline-mode)
	:custom (doom-modeline-minor-modes t)
	:config (doom-modeline-mode 1))
  (use-package minions
	:commands (minions-mode)
	:init
	(minions-mode 1))
  (use-package char-menu
    :init
    (setq-default
     char-menu
     '("—" "‘’" "“”" "…" "«»"
       ("Typography"
        "–" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
       ("Math"
        "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√" "∇")
       ("Arrows"
        "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
       ("Greek"
        "α" "β" "Δ" "δ" "ε" "ζ" "η" "θ" "λ" "μ" "ν" "ξ"
        "Ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω" "Ω")))
    :bind
    ("<f14> DEL" . char-menu))
  (use-package smartparens
	:commands (smartparens-global-mode)
    :init
    (add-hook 'prog-mode-hook #'smartparens-mode)
    (setq sp-highlight-pair-overlay nil)
	(setq sp-highlight-wrap-overlay nil)
    (setq sp-highlight-wrap-tag-overlay nil)
	:bind
	(:map smartparens-mode-map
          ("<C-backspace>" . sp-backward-kill-sexp)
          ("H-b" . sp-backward-sexp)
          ("H-d" . sp-kill-sexp)
          ("H-f" . sp-forward-sexp)
          ("H-h" . sp-select-next-thing)
          ("H-k" . sp-kill-hybrid-sexp)
          ("H-t" . sp-add-to-previous-sexp)
		  ("H-)" . sp-forward-slurp-sexp)
          ("H-}" . sp-forward-barf-sexp)
          ("H-(" . sp-backward-slurp-sexp)
          ("H-{" . sp-backward-barf-sexp))
	:hook
	(inferior-python-mode-hook . smartparens-mode)
	(jupyter-repl-mode-hook . smartparens-mode)
	:config
	(smartparens-global-mode 1)
	(advice-add 'sp-add-to-previous-sexp :after (lambda () (just-one-space)))
	(advice-add 'sp-add-to-previous-sexp :after (lambda () (sp-forward-sexp)))
	(add-to-list 'sp-no-reindent-after-kill-modes 'haskell-cabal-mode)
	(add-to-list 'sp-no-reindent-after-kill-modes 'haskell-mode))
  (use-package hl-todo
    :bind (("<f14> 2 2" . hydra-2DO/body)
		   ("<f14> 2 n" . hl-todo-next)
		   ("<f14> 2 p" . hl-todo-previous)
		   ("<f14> 2 i" . hl-todo-insert)
		   ("<f14> 2 o" . hl-todo-occur)
		   )
	:init
	(defhydra hydra-2DO (:color pink :hint nil :foreign-keys warn)
      "A hydra for hl-todo."
      ("p" hl-todo-previous "previous")
      ("n" hl-todo-next "next")
      ("o" hl-todo-occur "occur")
      ("i" hl-todo-insert "insert")
      ("q" nil "cancel" :color blue))
	:config
	(add-to-list 'hl-todo-keyword-faces '("XXX:" . "#ff8c00"))
	(add-to-list 'hl-todo-keyword-faces '("TODO:" . "#dc143c"))
	(add-to-list 'hl-todo-keyword-faces '("FIXME:" . "#4e9393")))
  (use-package visual-regexp
    :bind ("<f14> 3" . vr/query-replace))
  (use-package typit
    :bind ("<f14> 5" . typit-advanced-test))
  (use-package expand-region
    :bind
    ("C-=" . er/expand-region))

  ;; (global-set-key (kbd "<tab>") #'hs-toggle-hiding)
  (use-package hideshow
    :diminish (hs-minor-mode . " ⒣")
    :bind
    ("<f14> t f" . hs-minor-mode)
    (:map prog-mode-map
          ("<tab>" . hs-toggle-hiding)
          ("H-z" . hs-hide-all)         ;FIXME: <backtab> did not work here.
          ("C-<tab>" . hs-show-all))
    :hook
    (prog-mode-hook . hs-minor-mode))
  (use-package calc
    :bind
    ("<f14> a c" . calc))
  )
(progn                                  ; Themes, Fonts and mode-line
  (set-fontset-font "fontset-default" nil
					(font-spec :size 20 :name "Symbola"))

  (use-package face-remap               ; Fonts
    :init
    (set-face-attribute 'default nil :family "IBM Plex mono" :height 116 :weight 'normal :width 'normal)
    (set-face-attribute 'fixed-pitch nil :family "IBM Plex mono" :height 120 :weight 'normal :width 'normal)
    (set-face-attribute 'variable-pitch nil :family "IBM Plex Sans" :height 130 :weight 'normal :width 'normal)
    :config
    (setq text-scale-mode-step 1.05))
  ;; (load-theme 'leuven t)
  ;; base16-tomorrow 'base16-woodland 'base16-material
  (use-package fantom-theme)
  (use-package spacemacs-theme)
  ;; :defer t :init (load-theme 'spacemacs-dark t))
  (if (daemonp) (load-theme 'spacemacs-dark t) (load-theme 'spacemacs-light t))
  (use-package espresso-theme)
  (use-package plan9-theme)
  (use-package anti-zenburn-theme)
  (use-package flatui-theme)
  (use-package modus-themes)
  (use-package poet-theme)
  ;; :defer t
  (use-package solarized-theme)
  (use-package doom-themes)
  ;; (setq-default mode-line-format
  ;;               '("%e" ; print error message about full memory.
  ;;                 mode-line-front-space
  ;;                 ;; mode-line-mule-info
  ;;                 ;; mode-line-client
  ;;                 ;; mode-line-modified
  ;;                 ;; mode-line-remote
  ;;                 ;; mode-line-frame-identification
  ;;                 mode-line-buffer-identification
  ;;                 "   "
  ;;                 ;; mode-line-position
  ;;                 (vc-mode vc-mode)
  ;;                 ;; "  "
  ;;                 ;; mode-line-modes
  ;;                 "   "
  ;;                 ;; mode-line-misc-info
  ;;                 ;; battery-mode-line-string
  ;;                 mode-line-end-spaces))
  )
(progn                                  ; Keybindings
  (use-package hydra
    :commands (hydra-default-pre
               hydra-keyboard-quit
               hydra--call-interactively-remap-maybe
               hydra-show-hint
               hydra-set-transient-map)
    :config
    (setq hydra-look-for-remap t)       ; XXX: do I need this?
    )
  ;; (straight-use-package '(mk :local-repo "~/.emacs.d/mk/" :branch "vanilla" :includes(mk-text mk-utils)))
  (use-package mk-utils
	:demand t
	:straight nil
	:load-path "~/.emacs.d/mk")
  (use-package mk-text                  ; XXX: composable or objed
	;;https://github.com/paldepind/composable.el
	;;https://github.com/clemera/objed
	:demand t
	:straight nil
	:load-path "~/.emacs.d/mk/"
	:commands
	(mk-transpose-line-down
	 mk-transpose-line-up
	 mk-duplicate-line
	 mk-mark-command
	 mk-smart-indent
	 mk-eat-indent
	 mk-join-lines
	 mk-copy-rest-of-line
	 mk-copy-buffer
	 mk-yark-primary
	 mk-narrow-to-region
	 mk-add-to-end-of-lines
	 mk-sort-lines-dwim)
	:bind
	("C-SPC" . mk-mark-command)
	("C-r" . mk-smart-indent)
	("<f15> D" . mk-copy-rest-of-line)
	("M-S" . mk-eat-indent)
	("M-j" . mk-join-lines)
	("M-n" . mk-transpose-line-down)
	("M-p" . mk-transpose-line-up)
	("M-r" . mk-duplicate-line)
	)
  (global-unset-key (kbd "<menu>"))
  (use-package modalka
	:commands (modalka-define-kbd)
	:init
	(which-key-add-key-based-replacements "<f14> t" "Toggle")
	(which-key-add-key-based-replacements "<f14> <tab>" "prev-buffer")
	(which-key-add-key-based-replacements "<f14> C-<tab>" "next-buffer")
	(setq-default modalka-cursor-type '(hbar . 3))
	(set-cursor-color "red")
	:bind
	(("<menu>" . modalka-mode)
	 ("<insert>" . modalka-mode)
	 ("M-/" . hippie-expand)
	 ("H-;" . comment-box)
	 ("H-<f1>" . which-key-show-top-level)
	 ("H-q" . delete-window)            ; emulate i3wm
	 ("H-<up>" . windmove-up)
	 ("H-<left>" . windmove-left)
	 ("H-<down>" . windmove-down)
	 ("H-<right>" . windmove-right)
	 ("H-<backspace>" . kill-whole-line)
	 ("H-\\" . indent-region)
	 ("<f14> t a" . abbrev-mode)
	 ("<f14> t A" . auto-revert-mode)
	 ("<f14> t e" . org-toggle-pretty-entities)
	 ("<f14> t d" . toggle-debug-on-error)
	 ("<f14> t l" . display-line-numbers-mode)
	 ("<f14> t n" . org-num-mode)
	 ("<f14> t c" . conf-mode)
	 ("<f14> t o o" . org-mode)
	 ("<f14> t t" . text-mode)
	 ("<f14> t v" . variable-pitch-mode)
	 ("<f14> t w" . whitespace-mode)
	 ("<f14> a d" . dired)
	 ("<f14> a k" . paradox-list-packages)
	 ("<f14> <tab>" . switch-to-prev-buffer)
	 ("<f14> C-<tab>" . switch-to-next-buffer)
	 ("<f14> Q" . save-buffers-kill-emacs)
	 ("C-c Q" . save-buffers-kill-emacs)
	 ("<f15> Q" . quit-window)
	 :map modalka-mode-map
	 ("G" . end-of-buffer)
	 ("s" . swiper)
	 ("S" . swiper-isearch-thing-at-point)
	 ("J" . avy-goto-line)
	 ("L" . avy-goto-line)
	 ("x q" . delete-window)
	 ("Q" . mk-sort-lines-dwim)
	 ("X" . mk-open-default-dir)
	 ;; simple
	 ("<SPC> s w" . count-words)
	 ("<SPC> s ;" . eval-expression)
	 ("<SPC> s l" . list-processes)
	 ;; mk-text
	 ("<SPC> t e" . mk-add-to-end-of-lines)
	 ("<SPC> t n" . mk-narrow-to-region)
	 ("<SPC> t w" . widen)
	 ("<SPC> t y" . mk-yank-primary))
	:hook
	((compilation-mode-hook . modalka-mode)
	 (conf-toml-mode-hook . modalka-mode)
	 (conf-unix-mode-hook . modalka-mode)
	 (gitignore-mode-hook . modalka-mode)
	 (haskell-cabal-mode-hook . modalka-mode)
	 (help-mode-hook . modalka-mode)
	 (info-mode-hook . modalka-mode)
	 (mustache-mode-hook . modalka-mode)
	 (prog-mode-hook . modalka-mode)
	 (proof-mode-hook . modalka-mode)
	 (bibtex-mode-hook . modalka-mode)
	 ;; (text-mode-hook . mk-modalka-mode-no-git-commit)
	 (yaml-mode-hook . modalka-mode)
	 (ztree-mode-hook . modalka-mode))
	:preface
	(defun mk-modalka-mode-no-git-commit ()
      "Enable ‘modalka-mode’ unless get edit git commit message."
      (unless (string-equal (buffer-name) "COMMIT_EDITMSG")
		(modalka-mode 1)))
	(defun mk-open-default-dir ()
      "Open default directory."
      (interactive)
      (find-file default-directory))
	:config
	;; (modalka-define-kbd "SPC" "C-SPC")
	;; few options: spc like spacemacs or f14 which works globally
	;; ' (handy as self-inserting)
	;; " (handy as self-inserting)
	(modalka-define-kbd "," "C-,")
	;; - (handy as self-inserting)
	(modalka-define-kbd "%" "M-%")
	(modalka-define-kbd "/" "M-.")
	(modalka-define-kbd "." "C-.")
	(modalka-define-kbd ":" "M-;")
	(modalka-define-kbd ";" "C-;")
	(modalka-define-kbd "?" "M-,")
	(modalka-define-kbd "a" "C-a")
	(modalka-define-kbd "b" "C-b")
	(modalka-define-kbd "c b" "C-c C-b")
	(modalka-define-kbd "c c" "C-c C-c")
	(modalka-define-kbd "c k" "C-c C-k")
	(modalka-define-kbd "c l" "C-c C-l")
	(modalka-define-kbd "c n" "C-c C-n")
	(modalka-define-kbd "c s" "C-c C-s")
	(modalka-define-kbd "c t" "C-c C-t")
	(modalka-define-kbd "c u" "C-c C-u")
	(modalka-define-kbd "c v" "C-c C-v")
	(modalka-define-kbd "c x" "C-c C-x")
	(modalka-define-kbd "c w" "C-c C-w")
	(modalka-define-kbd "c ," "C-c C-,")
	(modalka-define-kbd "d" "C-d")
	(modalka-define-kbd "e" "C-e")
	(modalka-define-kbd "f" "C-f")
	;; (modalka-define-kbd "g" "C-g")
	(modalka-define-kbd "g g" "M-<")
	(modalka-define-kbd "G" "M->")
	(modalka-define-kbd "h" "M-h")
	(modalka-define-kbd "H" "M-H")
	(modalka-define-kbd "i" "C-i")
	(modalka-define-kbd "j" "M-j")
	(modalka-define-kbd "k" "C-k")
	(modalka-define-kbd "l" "C-l")
	(modalka-define-kbd "m" "C-m")
	(modalka-define-kbd "n" "C-n")
	(modalka-define-kbd "o" "C-o")
	(modalka-define-kbd "p" "C-p")
	(modalka-define-kbd "q" "<f15> Q")
	(modalka-define-kbd "r" "C-r")
	;; s for swiper
	(modalka-define-kbd "s" "C-s")
	(modalka-define-kbd "S" "C-S")
	(modalka-define-kbd "t" "C-t")
	(modalka-define-kbd "u" "C-u")
	(modalka-define-kbd "v" "C-v")
	(modalka-define-kbd "w" "C-w")
	(modalka-define-kbd "x 0" "C-x C-0")
	(modalka-define-kbd "x 1" "C-x 1")
	(modalka-define-kbd "x 2" "C-x 2")
	(modalka-define-kbd "x 3" "C-x 3")
	(modalka-define-kbd "x ;" "C-x C-;")
	(modalka-define-kbd "x e" "C-x C-e")
	(modalka-define-kbd "x o" "C-x C-o")
	(modalka-define-kbd "x w" "C-x C-w")
	(modalka-define-kbd "x s" "C-x C-s")
	(modalka-define-kbd "x S" "C-x S")
	(modalka-define-kbd "x x" "C-x C-x")
	(modalka-define-kbd "x SPC" "C-x SPC") ;rectangles
	(modalka-define-kbd "x r t" "C-x r t")
	(modalka-define-kbd "x r c" "C-x r c")
	(modalka-define-kbd "x r d" "C-x r d")
	(modalka-define-kbd "x r r" "C-x r r") ;rectangles to register
	(modalka-define-kbd "x r x" "C-x r x") ;register
	(modalka-define-kbd "x r g" "C-x r g")
	(modalka-define-kbd "x b" "C-x b")	;ivy-switch-buffer
	(modalka-define-kbd "x m" "C-x m")	;mu4e-compose-new
	(modalka-define-kbd "y" "C-y")
	(modalka-define-kbd "z" "C-z")
	(modalka-define-kbd "c e" "C-c C-e")
	(modalka-define-kbd "W" "<f15> D") 	; trick F15
	)
  ;; (use-package evil-numbers
  ;;   :bind
  ;;   ("H-a" . evil-numbers/inc-at-pt)
  ;;   ("H-x" . evil-numbers/dec-at-pt))
  )
(progn                                  ; Ivy Counsel and Swiper
  (use-package smex                     ; Remember past actions
    :bind ("<f14> <f14>" . smex-major-mode-commands))
  (use-package counsel
    :bind (
           ("M-x" . counsel-M-x)
           ("<f14> o t" . counsel-load-theme)
           ("<f14> ," . counsel-switch-buffer)
           ("<f14> R" . counsel-bookmark)
           ("<f14> y" . counsel-yank-pop)
           ("<f14> f f" . counsel-find-file)
           ("<f14> f z" . (lambda () (interactive "") (cd "~/")(counsel-fzf)))
           ("<f14> f Z" . counsel-fzf)
           ("<f14> f l" . counsel-locate)
           ("<f14> f r" . counsel-recentf)
           ("<f14> / r" . counsel-rg)
           ("<f14> / s" . counsel-ag)
           ("<f14> / w" . counsel-search)
           ("<f14> / l" . counsel-recoll)
           ("<f1> a" . counsel-apropos)
           ("<f1> k" . counsel-descbinds)
           ("<f1> f" . counsel-describe-function)
           ("<f1> v" . counsel-describe-variable)
           ("<f14> c <f9>" . counsel-semantic-or-imenu)
           ("<f14> c u" . counsel-unicode-char)
           ("<f14> c h" . counsel-command-history)
           ("<f14> c i" . counsel-info-lookup-symbol)
           ("<f14> c l" . counsel-find-library)
           ("<f14> c L" . counsel-load-library)
           ("<f14> c m" . counsel-tmm)
           ("<f14> c M" . counsel-minor)
           ("<f14> c w" . counsel-wmctrl)
           ("<f14> g c c" . counsel-git-checkout)
           ("<f14> g c g" . counsel-git-grep)  ; find string in git project
           ("<f14> g c l" . counsel-git-log)
           ("<f14> g c s" . counsel-git-stash)
           ("<f14> c c" . counsel-colors-web)
           ("<f14> c C" . counsel-colors-emacs)
           ("<f14> c r" . counsel-mark-ring)
           ("<f14> c e" . counsel-org-entity)  ; orgmode
           ("<f14> c o a" . counsel-org-agenda-headlines)
           ("<f14> c o A" . counsel-org-goto-all))
    :init
    (which-key-add-key-based-replacements "<f14> f" "Files")
    (which-key-add-key-based-replacements "<f14> g" "Git")
    (which-key-add-key-based-replacements "<f14> f z" "fzf $HOME")
    (which-key-add-key-based-replacements "<f14> /" "Search")
    (which-key-add-key-based-replacements "<f14> a" "Apps")
    (which-key-add-key-based-replacements "<f14> c" "Counsels")
    :config
    (setq ivy-initial-inputs-alist nil ; ivy always guesses wrong "^"
          counsel-ag-base-command
          "ag --vimgrep --hidden -S %s"
          counsel-rg-base-command
          "rg --color never --no-heading --hidden -S %s"))
  (use-package ivy
    :demand t
    :diminish (ivy-mode . "Iv")
	:commands (ivy-mode)
	:functions (ivy-add-actions
                ivy-format-function-line)
    :bind
	(("<f14> r" . ivy-resume)
	 ("C-;" . ivy-resume)
	 )
    :init
    (ivy-mode 1)                        ; enable ivy globally at startup
    :config
    (setq ivy-use-virtual-buffers nil ; include 'bookmarks 'recentf or both t
          enable-recursive-minibuffers t ; enable this if you want `swiper' to use it
          ivy-height 20                  ; set height of the ivy window
          ivy-count-format "(%d/%d) "    ; count format, from the ivy help page
          ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                  (t      . ivy--regex-ignore-order)))
    ;; for ivy-rich
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
  (use-package ivy-rich
    :after ivy
	:commands (ivy-rich-mode)
    :init (ivy-rich-mode 1))
  (use-package ivy-hydra
    :after ivy)
  (use-package fzf
    :bind ("C-x x" . fzf))
  (use-package swiper
    :bind
    ("C-s" . swiper)
    ("C-S-s" . swiper-isearch-thing-at-point))
  )
(progn                                  ; Company completion
  (use-package company
    :diminish (company-mode . " ⓐ")
    :defines (company-ispell-dictionary
			  ;; company-show-quick-access
			  )
    :commands (company-complete-common-or-cycle
			   global-company-mode)
	:init
    (which-key-add-key-based-replacements "<f14> t N" "Ngram")
    (global-company-mode)
    :bind
	(;("<f14> t S" . toggle-company-ispell) ;; to complete words
     :map company-active-map
     ("C-n" . company-select-next-or-abort)
     ("C-p" . company-select-previous-or-abort)
     ("<tab>" . company-select-next-if-tooltip-visible-or-complete-selection)
     ("<backtab>" . (lambda () (interactive "") (company-complete-common-or-cycle -1)))
     ("C-/" . company-search-candidates)
     ("C-f" . company-filter-candidates)
     ("C-h" . company-show-doc-buffer)
     ("C-s" . counsel-company))
	:preface
	(defun toggle-company-ispell ()
      (interactive)
      (cond
       ((memq 'company-ispell company-backends)
        (setq company-backends (delete 'company-ispell company-backends))
        (message "company-ispell disabled"))
       (t
        (add-to-list 'company-backends 'company-ispell)
        (message "company-ispell enabled!"))))
    :config
    (setq company-tooltip-align-annotations t
          company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-show-quick-access t
          ;; company-require-match nil;company-require-match 'never
          )
    (setq company-backends              ;;XXX: to be tested
          '((company-files          ; files & directory
             company-keywords       ; keywords
             company-capf)  ; completion-at-point-functions
            (company-abbrev company-dabbrev)
            ))
    )
  (use-package company-box
	:hook (company-mode-hook . company-box-mode))
  (use-package company-quickhelp
    ;; :disabled
    :after (company)
    :init (company-quickhelp-mode)
    :bind
    ("<M-f1>" . company-quickhelp-mode))
  (use-package company-ngram
    :after company
	:commands (company-ngram-init)
	:bind
	("<f14> t N" . (lambda () (interactive) (company-ngram-init)))
    :config
    (setq company-ngram-data-dir "~/Sync/ngram")
    ;; company-ngram supports python 3 or newer
    (setq company-ngram-python "python3")
    ;; or use `M-x turn-on-company-ngram' and `M-x turn-off-company-ngram' on individual buffers
    (cons 'company-ngram-backend company-backends)
    (declare-function company-ngram-command "company-ngram")
    (run-with-idle-timer 7200 t (lambda () ; save the cache of candidates
                                  (company-ngram-command "save_cache"))))
  (use-package company-statistics
    :after (company)
	:commands (company-statistics-mode)
    :config (company-statistics-mode))
  )
(progn                                  ; Yasnippet
  (use-package yasnippet
    :preface
    (defun company-mode/backend-with-yas (backend)
      "Add yasnippet support for all company backends."
      (if (and (listp backend) (member 'company-yasnippet backend))
    	  backend
    	(append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
    :bind
    ("<f14> Y a" . yas-reload-all)
    ("<f14> Y n" . yas-new-snippet)
    ("<f14> Y v" . yas-visit-snippet-file)
    ("<f14> t y" . yas-minor-mode)
    ("<f14> i y" . yas-insert-snippet)
    ;; ;; disable yas minor mode map ;; use hippie-expand instead [sp]
    ;; (setq yas-minor-mode-map (make-sparse-keymap))
    :init
    (which-key-add-key-based-replacements "<f14> Y" "Yasnippet")
    (which-key-add-key-based-replacements "<f14> i" "Insert")
    :hook
    (prog-mode-hook . yas-minor-mode)
    (org-mode-hook . yas-minor-mode)
    (message-mode-hook . yas-minor-mode)
    (markdown-mode-hook . yas-minor-mode)
    :config
    (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (add-to-list 'yas-snippet-dirs "~/Sync/.emacs/yasnippets")
    (setq yas-triggers-in-field t
          yas-wrap-around-region t)     ;or [a-z] register
    (use-package yasnippet-snippets)
    (use-package ivy-yasnippet
      :bind
      ("<f14> i s" . ivy-yasnippet))
    ;; https://github.com/syl20bnr/spacemacs/pull/179
    ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names/10520
    (setq company-backends
          (mapcar #'company-mode/backend-with-yas company-backends))
    )
  ;; auto-yasnippet
  ;; yatemplate
  )
(progn                                  ; Spell checking
  (use-package ispell
    :config
    (setq ispell-program-name (executable-find "hunspell")
          ispell-really-hunspell t
		  ispell-dictionary "en_US-large")
    (add-to-list 'ispell-skip-region-alist
                 '(;("^#+BEGIN_SRC" . "^#+END_SRC")
                   ("^From:" . "line--$"))))
  (use-package flyspell
	:bind
    (("<f14> t s" . flyspell-mode)
     ("<f14> t S" . flyspell-correct-auto-mode)
     :map flyspell-mode-map
     ("C-." . flyspell-correct-wrapper)
     ("C-<" . flyspell-auto-correct-previous-word))
	:hook
	(flyspell-mode-hook . (lambda ()
							(unbind-key "C-;" flyspell-mode-map)))
	(text-mode-hook . flyspell-mode)
	(prog-mode-hook . flyspell-prog-mode)
	(change-log-mode-hook . (lambda () (flyspell-mode -1)))
	(log-edit-mode-hook . (lambda () (flyspell-mode -1))))
  (use-package flyspell-lazy
    :after (flyspell)
	:commands (flyspell-lazy-mode)
    :init
    (setq-default flyspell-lazy-disallow-buffers nil)
	(setq-default flyspell-lazy-idle-seconds 1)
    :config
    (flyspell-lazy-mode 1))
  (use-package flyspell-correct-ivy	; play better with darkroom and the like
    :after flyspell
	:commands flyspell-correct-ivy
    :init
    (setq flyspell-issue-message-flag nil)
    (setq flyspell-correct-interface #'flyspell-correct-ivy))
  (use-package guess-language
    ;; Its advantage is multi language in the same doc.
    ;; :diminish (guess-language-mode . "G")
    :bind
    ("<f14> t g" . guess-language-mode)
    ("<f14> s e" . (lambda () (interactive)
                     (ispell-change-dictionary "en_US-large")
                     (setq company-ispell-dictionary "/usr/share/dict/usa")
                     (flyspell-buffer)))
    ("<f14> s i" . (lambda () (interactive)
                     (ispell-change-dictionary "it_IT")
                     (setq company-ispell-dictionary "/usr/share/dict/italian")
                     (flyspell-buffer)))
    ;; :hook
    ;; (flyspell-mode . guess-language-mode)
    ;; ;; (flyspell-mode-prog . guess-language-mode)
    ;; (text-mode . guess-language-mode)
    :functions guess-language-switch-function
    :init
    (which-key-add-key-based-replacements "<f14> s" "Spell")
    (which-key-add-key-based-replacements "<f14> s e" "english")
    (which-key-add-key-based-replacements "<f14> s i" "italian")
    :config
    (setq guess-language-langcodes '((en . ("en_US-large" "English"))
                                     (it . ("it_IT" "Italian")))
          guess-language-min-paragraph-length 35
          guess-language-languages '(en it))
    (defun guess-language-switch-function (lang beginning end)
      "Switch additional dictionaries. LANG is the ISO 639-1 code of the language
       (as a symbol). BEGINNING and END are the endpoints of the region in which
       LANG was detected but these are ignored."
      ;; (when (and (featurep 'festival)
      ;; 		 (festivalp))
      ;; 	(pcase lang
      ;; 	  ('en (festival-voice-english-female))
      ;; 	  ('it (festival-voice-italian-female))))
      (pcase lang
        ('en (setq company-ispell-dictionary "/usr/share/dict/usa"))
        ('it (setq company-ispell-dictionary "/usr/share/dict/italian"))))
    (add-hook 'guess-language-after-detection-functions #'guess-language-switch-function))
  )
(progn                                  ; mu4e
  ;; XXX: email in org html
  ;; https://vxlabs.com/2015/01/28/sending-emails-with-math-and-source-code/
  ;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-04.org
  ;; ;; http://pragmaticemacs.com/emacs/email-templates-in-mu4e-with-yasnippet/
  ;; (add-hook 'mu4e-compose-mode-hook 'spacemacs/load-yasnippet)
  (use-package mu4e
    :straight (:type built-in)			; in AUR/mu
	:commands (mu4e-compose-new
			   mu4e-context-current)
	:hook
	((mu4e-view-mode-hook . variable-pitch-mode)
	 ;; (message-mode-hook . ; http://www.djcbsoftware.nl/code/mu/mu4e/Compose-hooks.html
	 (mu4e-compose-mode-hook .
							 (lambda()
							   "My settings for message composition."
							   (let* ((ctx (mu4e-context-current))
									  (name (if ctx (mu4e-context-name ctx))))
								 (when name
								   (cond
									((string= name "cnr")
									 (save-excursion (message-add-header "Bcc: daniele.arosio@cnr.it\n")))
									((string= name "pec")
									 (save-excursion (message-remove-header "Bcc:*")))
									)))
							   (set-fill-column 80)
							   (visual-fill-column-mode)
							   ;; (ispell-minor-mode)
							   ;; (flyspell-mode)
							   ;; (org~mu4e-mime-switch-headers-or-body)
							   ;; (turn-on-obligor)   ;; this prevented flyspell correction
							   ;; (turn-on-orgstruct)  ;; probably included in ++
							   ;; (turn-on-orgstruct++)  ;; =,he= to send bibtex email
							   ;; (typo-mode)
							   (footnote-mode)))
	 ;; (message-mode-hook . mu4e-compose-mode)
	 ;; (mu4e-headers-mode . variable-pitch-mode)
	 )
	:bind
	(("<f14> a m" . mu4e)
	 ("C-x m" . mu4e)
	 :map
	 mu4e-compose-mode-map
	 ("C-c o a" . mu4e-compose-attach-captured-message)
	 ("C-c o b" . message-goto-bcc)
	 ("C-c o c" . message-goto-cc)
	 ("C-c o m" . message-goto-body)
	 ("C-c o s" . message-goto-subject)
	 ("C-c o t" . message-goto-to)
	 :map
	 mu4e-view-mode-map
	 ("f" . mu4e-headers-mark-for-flag)
	 ("<tab>"     . org-next-link)	; 'shr-next-link
	 ("<backtab>" . org-previous-link)	; 'shr-previous-link
	 ("G"         . end-of-buffer)
	 ("V"         . mu4e-view-verify-msg-popup)
	 ("v"         . visual-fill-column-mode)
	 ("C-c l" . org-store-link)         ; requires ol.el
	 :map
	 mu4e-headers-mode-map
	 ("G"         . end-of-buffer)
	 ("D" . "T d")
	 ("f"         . mu4e-headers-mark-for-flag)
	 )
	;; :custom-face
	;; (mu4e-header-marks-face ((t (:height 180 ))))
	;; (mu4e-header-value-face ((t (:height 150))))
	;; (mu4e-view-body-face ((t (:height 140))))
	:init
	(set-default mail-user-agent 'mu4e-user-agent)
	;; (defun my-mu4e () ; trying to have themes bound to major modes
	;;   (interactive)
	;;   (let ((color-theme-is-global nil))
	;;     (select-frame (make-frame))
	;;     (color-theme-tango)
	;;     (mu4e)))
	:config
	;; (when (fboundp 'imagemagick-register-types) (imagemagick-register-types))
	(setq mu4e-attachment-dir "~/")
	(setq mu4e-change-filenames-when-moving t) ; rename files when moving (Needed for mbsync)
	(setq mu4e-completing-read-function 'completing-read) ; use convenient completion for navigation =j o=
	(setq mu4e-compose-keep-self-cc nil) ;default
	(setq mu4e-context-policy 'pick-first) ; start with the first (default) context; 
	(setq mu4e-get-mail-command "mbsync -a")
	(setq mu4e-headers-include-related nil) ; =W= to view threads
	(setq mu4e-headers-skip-duplicates nil) ; =V= to skip duplicates
	(setq mu4e-view-show-images t)	; enable inline images and VIEW
	(setq mu4e-confirm-quit nil)
	(setq mu4e-update-interval 30)
	(setq mu4e-hide-index-messages t)  ; hide updating messages
	(setq mu4e-use-fancy-chars t)
    (setq mu4e-maildir-shortcuts '(("/cnr/INBOX"         . ?i)
								   ("/gmail/Inbox"       . ?j)
								   ("/gmail/archive"     . ?g)
								   ("/cnr/Sent"          . ?s)
								   ("/cnr/Templates"     . ?t)
								   ("/archive"           . ?a)
								   ("/archives/personal" . ?p)
								   ("/cnr/refs"          . ?r)
								   ("/cnr/keepup"        . ?k)
								   ("/cnr/Drafts"        . ?d)))
    (setq mu4e-bookmarks		; Bookmarks
		  '((:query "flag:unread AND NOT flag:trashed AND NOT maildir:/feeds"
                    :name "Unread messages"
                    :key ?u)
            (:query "maildir:/feeds" :name "Feeds" :key ?f)
            (:query "date:today..now" :name "Today's messages" :key ?t)
            (:query "date:7d..now" :name "Last 7 days" :key ?w)
            (:query "mime:image/*" :name "Messages with images" :key ?p)
            (:query "size:5M..500M" :name "Big messages" :key ?b)
            (:query "flag:unread NOT flag:trashed AND (flag:list OR from:trac@sagemath.org)"
                    :name "Unread bulk messages" :key ?l)
            (:query ,(mapconcat 'identity
                                (mapcar
                                 (lambda (maildir)
								   (concat "maildir:" (car maildir)))
                                 mu4e-maildir-shortcuts) " OR ")
                    :name "All inboxes" :key ?i)))
	(setq mu4e-compose-forward-as-attachment nil)
	;; https://jherrlin.github.io/posts/emacs-mu4e/
	;; (setq mu4e-compose-complete-addresses t)
	;; (setq mu4e-compose-dont-reply-to-self t)
	;; (setq smtpmail-debug-info t)
	;; (setq smtpmail-stream-type 'starttls)
	;; (setq mm-sign-option 'guided)
	;;
	(setq mu4e-view-show-addresses t)	; show full addresses
	(setq mu4e-headers-leave-behavior 'apply) ; leaving headers view apply all marks
	(setq mu4e-save-multiple-attachments-without-asking t)
	(setq mu4e-html2text-command "html2text -utf8 -width 78") ;'w3m 'mu4e-shr2text
	(setq fill-flowed-encode-column 998) ; https://www.ietf.org/rfc/rfc2822.txt
	;; (set-face-attribute 'mu4e-view-body-face nil :inherit 'variable-pitch :height 150)
	;; (set-face-attribute 'mu4e-unread-face nil :inherit '(fixed-pitch shadow bold) :height 130)
	;; (set-face-attribute 'mu4e-header-face nil :inherit 'fixed-pitch :height 130)
	(use-package mu4e-context :straight mu4e
	  :config
	  (setq mu4e-contexts
			`( ,(make-mu4e-context
				 :name "cnr"
				 :enter-func (lambda () (mu4e-message "Entering Cnr context"))
				 :leave-func (lambda () (mu4e-message "Leaving Cnr context"))
				 ;; we match based on the maildir folder
				 ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
				 :match-func (lambda (msg)
                               (when msg
								 (string-prefix-p "/cnr" (mu4e-message-field msg :maildir))))
				 :vars '( ( user-mail-address      . "daniele.arosio@cnr.it"  )
                          ( user-full-name         . "Daniele Arosio" )
                          ( mu4e-sent-folder   . "/cnr/Sent" )
                          ( mu4e-drafts-folder . "/cnr/Drafts" )
                          ( mu4e-trash-folder  . "/cnr/Trash" )
                          ( mu4e-refile-folder . "/archive" )
                          ( mu4e-compose-signature .
                                                   (concat
													"Daniele Arosio\n"
													"Consiglio Nazionale delle Ricerche (CNR)\n"
													"Istituto di Biofisica\n"
													"Via Sommarive 18\n"
													"38123 Trento, Italy\n"
													"tel +39 0461 314607\n"))))
               ,(make-mu4e-context
				 :name "gmail"
				 :enter-func (lambda () (mu4e-message "Switch to the gmail context"))
				 ;; no leave-func
				 :match-func (lambda (msg)
                               (when msg
								 (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
				 :vars '( ( user-mail-address       . "danielepietroarosio@gmail.com" )
                          ( user-full-name          . "daniele arosio" )
                          (mu4e-drafts-folder . "/gmail/draft")
                          (mu4e-trash-folder . "/gmail/trash")
                          ( mu4e-compose-signature  .
													(concat
													 "daniele arosio\n"
													 "38123 Trento\n"))))
               ,(make-mu4e-context
				 :name "pec"
				 :match-func (lambda (msg)
                               (when msg
								 (string-prefix-p "/pec" (mu4e-message-field msg :maildir))))
				 :vars '( (user-mail-address  . "daniele.arosio@postecert.it" )
                          (user-full-name     . "Daniele Arosio" )
                          (mu4e-drafts-folder . "/pec/Drafts")
                          (mu4e-trash-folder  . "/pec/trash")
                          (mu4e-sent-folder   . "/pec/Sent Items")
                          (mu4e-compose-signature .
                                                  (concat
                                                   "daniele arosio\n"
                                                   "38123 Trento\n"))))
               ))
	  )
	(use-package mu4e-compose :straight mu4e
	  :config
	  (setq mu4e-sent-messages-behavior	; Save msg into sent folder only for pec
			(lambda ()
			  (if (string= (message-sendmail-envelope-from) "daniele.arosio@postecert.it")
				  'sent
				'delete)))
	  ;; default is to ask-if-none (ask when there's no context yet, and none match)
	  (setq mu4e-compose-context-policy 'ask-if-none)
	  (setq mu4e-compose-format-flowed t)  ; Set format=flowed
	  )
	(use-package mu4e-draft :straight mu4e
	  :config
	  (setq mu4e-compose-signature-auto-include nil)
	  (setq mu4e-compose-in-new-frame t) ; every new email composition gets its own frame
	  )
	(use-package mu4e-headers :straight mu4e
      :functions (mu4e-headers-mark-and-next
                  mu4e~headers-goto-docid)
	  :config
	  (setq mu4e-headers-date-format "%y/%m/%d")
	  (setq mu4e-headers-fields	'((:human-date     .  12)
								  (:flags          .   6)
								  (:size           .   7)
								  (:thread-subject . nil)
								  (:from           .  22)))
	  (setq mu4e-headers-auto-update t)	; default
	  (setq mu4e-headers-visible-lines 10)
	  )
	(use-package message
	  :straight (:type built-in)
      :functions (message-sendmail-envelope-from
                  message-add-header
                  message-remove-header)
	  :hook
	  (message-send-hook . org-mime-confirm-when-no-multipart)
	  :config
	  (setq message-kill-buffer-on-exit t) ; don't keep message buffers around
	  ;; (setq message-send-mail-function 'smtpmail-send-it)
	  (setq message-send-mail-function 'message-send-mail-with-sendmail)
	  (setq message-sendmail-envelope-from 'header)
	  (setq message-sendmail-f-is-evil nil)
	  (setq message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n")
	  (setq message-citation-line-function 'message-insert-formatted-citation-line)
	  )
	(use-package sendmail
	  :straight (:type built-in)
	  :config
	  (setq mail-specify-envelope-from t)
	  (setq mail-envelope-from 'header)
	  (setq mail-interactive t)
      )
	(use-package org-mu4e :straight mu4e
	  :bind (:map mu4e-compose-mode-map
				  ("C-c o O" . org~mu4e-mime-switch-headers-or-body))
      :config
      ;;store link to message/query if in header view (t/nil)
      (setq mu4e-org-link-query-in-headers-mode t)
	  (setq org-mu4e-convert-to-html t)
	  )
	(use-package org-mime
	  ;; :commands (org-mime-edit-mail-in-org-mode ;XXX
	  ;; 			 org-mime-change-element-style)
	  :bind (
			 :map mu4e-compose-mode-map
			 ("C-c o o" . org-mime-edit-mail-in-org-mode)
			 ("C-c o h" . org-mime-htmlize)
			 :map org-mode-map
			 ("C-c m" . org-mime-org-subtree-htmlize)
			 ("C-c M" . org-mime-org-buffer-htmlize))
	  :config
	  (setq org-mime-export-options '(
									  :section-numbers nil
									  :with-author nil
									  :with-toc nil))
	  :hook
	  (org-mime-html-hook . (lambda ()
							  (org-mime-change-element-style
							   "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
											 "darkred" "burlywood"))))
	  (mu4e-compose-mode-hook . (lambda ()(require 'org-mime))) ; work w/out server
	  )
	(use-package mu4e-view-common :straight mu4e
	  :preface
	  (defun mu4e-action-save-to-pdf (msg)
		(let* ((date (mu4e-message-field msg :date))
			   (infile (mu4e~write-body-to-html msg))
			   (outfile (format-time-string "%Y-%m-%d%H%M%S.pdf" date)))
		  (with-temp-buffer
			(shell-command
			 (format "wkhtmltopdf %s ~/%s" infile outfile) t))))
	  (defun jcs-view-in-eww (msg)
		(eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))
	  ;; view-attachment-actions  https://vxlabs.com/tag/mu4e/
	  (defun my-remove-attachment (msg num)
        "Remove attachment."
        (let* ((attach (mu4e~view-get-attach msg num))
			   (path (mu4e-msg-field msg :path))
			   (filename (and attach (plist-get attach :name)))
			   (cmd (format "touch /tmp/%s-removed; altermime --input=%s --replace='%s' --with='/tmp/%s-removed'"
							filename path filename filename)))
		  (when (and filename
                     (yes-or-no-p
					  (format "Are you sure you want to remove '%s'?" filename)))
            (shell-command cmd)
            (message cmd))))
	  :config
	  (setq mu4e-view-actions
			'(("capture message" . mu4e-action-capture-message)
              ("show this thread" . mu4e-action-show-thread)))
	  (add-to-list 'mu4e-view-actions
				   '("view in browser" . mu4e-action-view-in-browser) t)
	  (add-to-list 'mu4e-view-actions
				   '("print to pdf" . mu4e-action-save-to-pdf) t)
	  (add-to-list 'mu4e-view-actions
				   '("eww view" . jcs-view-in-eww) t)
	  (add-to-list 'mu4e-view-attachment-actions
				   '("remove-attachment" . my-remove-attachment))
	  )
	(use-package mu4e-mark :straight mu4e ; Tags and personal archive
	  :bind (:map
			 mu4e-headers-mode-map
			 ("z" . mu4e-headers-mark-for-tag)
			 ("P" . mu4e-headers-mark-for-personal)
			 :map
			 mu4e-view-mode-map
			 ("z" . mu4e-view-mark-for-tag)
			 ("P" . mu4e-view-mark-for-personal))
	  :config
	  (add-to-list 'mu4e-marks
				   ;; https://gist.github.com/lgatto/7091552
				   '(tag
                     :char       "z"
                     :prompt     "gtag"
                     :ask-target (lambda () (read-string "What tag do you want to add?"))
                     :action      (lambda (docid msg target)
                                    (mu4e-action-retag-message msg (concat "+" target)))))
	  (add-to-list 'mu4e-marks
				   '(personal
                     :char       "P"
                     :prompt     "personal"
                     :show-target (lambda (target) "personal")
                     :action      (lambda (docid msg target)
                                    ;; must come before proc-move since retag runs
                                    ;; 'sed' on the file
                                    (mu4e-action-retag-message msg "-\\Inbox")
                                    (mu4e~proc-move docid "/archives/personal" "+S-u-N"))))
	  (mu4e~headers-defun-mark-for tag)
	  (mu4e~headers-defun-mark-for personal)
	  (mu4e~view-defun-mark-for tag)
	  (mu4e~view-defun-mark-for personal)
	  )
	;; FIXME
	(setq 								; smtp queue
	 smtpmail-queue-mail  t
	 smtpmail-queue-dir  (expand-file-name "~/Sync/Maildir/queue/cur") ; Remember to "mu mkdir" and "touch .noindex"
	 ;; (set-language-environment "UTF-8")	; required by mu4e-send-delay for sending correctly formatted email
	 ;; (progn                    ; send delay
	 ;;   (add-to-list 'load-path "~/.spacemacs.d/mu4e-send-delay")
	 ;;   (require 'mu4e-send-delay)
	 ;;   (mu4e-send-delay-setup)
	 ;;   (add-hook 'mu4e-main-mode-hook 'mu4e-send-delay-initialize-send-queue-timer)
	 ;;   (add-hook 'mu4e-main-mode-hook
	 ;;   		(lambda ()
	 ;;   		  (define-key mu4e-compose-mode-map (kbd "C-c C-c") 'mu4e-send-delay-send-and-exit)))
	 ;;   (setq mu4e-send-delay-default-delay "5m" mu4e-send-delay-timer 120)
	 ;;   )
	 )
	)
  (use-package mu4e-jump-to-list
    :after mu4e)
  (progn                ; multiple attachments
    ;;  and dired: turn-on-gnus-dired-mode C-c RET C-a
    ;;  https://emacs.stackexchange.com/questions/14652/attach-multiple-files-from-the-same-directory-to-an-email-message
    (ivy-add-actions
     'counsel-locate
     '(("a" (lambda (x)
			  (unless (memq major-mode '(mu4e-compose-mode message-mode))
                (compose-mail))
			  (mml-attach-file x)) "Attach to email")))
    (ivy-add-actions
     'counsel-fzf
     '(("a" (lambda (x)
			  (unless (memq major-mode '(mu4e-compose-mode message-mode))
                (compose-mail))
			  (mail-add-attachment x)) "Attach to email")))
    )
  )
(progn                                  ; org
  (use-package org-capture :straight org
    :preface
    (defun nemacs-org-capture-review-daily ()
      (interactive)
      (progn
        (org-capture nil "rd")
        (org-capture-finalize t)
        (org-speed-move-safe 'outline-up-heading)
        (org-narrow-to-subtree)
        (fetch-calendar)
        (org-clock-in)))
    (defun my-new-weekly-review ()
      (interactive)
      (progn
        (org-capture nil "rw")
        (org-capture-finalize t)
        (org-speed-move-safe 'outline-up-heading)
        (org-narrow-to-subtree)
        (fetch-calendar)
        (org-clock-in)))
    ;; (defun nemacs-org-capture-add-basic-properties ()
    ;;   (interactive)
    ;;   (org-id-get-create))
    ;; :hook (org-capture-before-finalize . nemacs-org-capture-add-basic-properties)
    :hook
    ((org-capture-mode-hook . (lambda () (modalka-mode 0)))
     (org-after-refile-insert-hook . save-buffer))
    :config
	(setq org-default-notes-file "~/Sync/box/org/inbox.org"
          org-capture-templates
          '(
            ("t" "Todo simple entry" entry (file org-default-notes-file)
             "* TODO %?\n%[~/Sync/.emacs/templates/da-property-string]\n")
            ("f" "Fast capture and exit" entry (file org-default-notes-file)
             "* TODO %^{Title}\n%[~/Sync/.emacs/templates/da-property-string]\n" :immediate-finish t)
            ("T" "Tasks in gtd" entry (file+headline da-gtd "Tasks")
             "* %^{State|TODO|NEXT|WAIT|PASS|MAYB} %? \t%^{Tag|:WORK:|:PERSONAL:}\n%[~/Sync/.emacs/templates/da-property-string]\n" :empty-lines 1)
            ("e" "File email" entry (file org-default-notes-file)
             "* \"%:subject\"\n%[~/Sync/.emacs/templates/da-property-string-email]%i%?\n")
			("P" "new Project" entry (file "~/Sync/box/org/projects.org")
			 "* %? \t%^{Tag|:WORK:proj:|:PERSONAL:proj:}\n%[~/Sync/.emacs/templates/da-property-string]\n%^{CATEGORY}p" :empty-lines 1 :prepend t)
			("n" "Next urgent task" entry (file+headline da-gtd "Tasks")
			 "* NEXT [#A] %? \t%^{Tag|:WORK:|:PERSONAL:}\nDEADLINE: %t\n%[~/Sync/.emacs/templates/da-property-string]\n")
			("s" "Study item" entry (file+headline da-gtd "Study")
			 "* TODO %?\n%[~/Sync/.emacs/templates/da-property-string]\n")

			("w" "Weight" table-line (file+headline da-gtd "Weight")
			 "|%t|%?|")

			("a" "Experimental appointments" entry (file+headline da-gtd "Appointments") "* %? %:subject\n SCHEDULED:%^T--%^T\n %a\n")
			
			("h" "new Habit" entry (file+headline da-gtd "Habits")
			 "* TODO %? \nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:REPEAT_TO_STATE: TODO\n:END:\n%a")
			("i" "Idea" entry (file "~/Sync/box/org/ideas.org") "* %^{Idea} \n%u\n%a\n%?" :empty-lines 1)
			("j" "Journal" entry (file+olp+datetree "~/Sync/box/org/journal.org") "* %? %^g\n%t\n%a\n%i" ) ; prefix C-1 alternative to time-prompt t
			("m" "Meeting" entry (file+olp+datetree "~/Sync/box/org/journal.org") "* MEETING %? :MEETING:\n%T" :clock-in t :clock-resume t)
			("k" "supermarKet" entry (file+headline "~/Sync/box/org/shopping.org" "Supermarket") "* %? \t:buy:\n" :unnarrowed t :kill-buffer t)
			;; XXX: captures for: (1) project [entry and file template],
			;; (2) peso [table-line]
			("g" "Gcals")              ; gcals
			("gg" "Gcal dpa" entry (file  "~/Sync/box/org/gcal/dpa.org")
			 "* %? %:subject\n :PROPERTIES:\n :calendar-id: danielepietroarosio@gmail.com\n :END:\n:org-gcal:\n%^T--%^T\n%a\n:END:" :empty-lines 1)
			("gf" "Gcal figli" entry (file  "~/Sync/box/org/gcal/figli.org")
			 "* %? %:subject\n :PROPERTIES:\n :calendar-id: c87gevr5pc3191on8c7nh8b4nc@group.calendar.google.com\n :END:\n:org-gcal:\n%^T--%^T\n%a\n:END:" :empty-lines 1)
			("r" "Review")              ;reviews
			("rd" "Review: Daily" entry (file+olp+datetree "/tmp/daily-reviews.org")
			 (file "~/Sync/.emacs/templates/my_dailyreviewtemplate.org"))
			("rw" "Review: Weekly Review" entry (file+olp+datetree "/tmp/weekly-reviews.org")
			 (file "~/Sync/.emacs/templates/my_weeklyreviewtemplate.org"))
			;; Only in mu4e
			("R" "Reply to" entry
			 (file+headline da-gtd "E-mail")
			 "* TODO Reply \"%:subject\"\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n%[~/Sync/.emacs/templates/da-property-string-email]%i%?\n")
			("W" "Wait for Reply" entry
			 (file+headline da-gtd "E-mail")
			 "* WAIT for reply \"%:subject\"\n%[~/Sync/.emacs/templates/da-property-string-email]%i%?\n")
			)
		  org-capture-templates-contexts '(("R" ((in-mode . "mu4e-view-mode")))
										   ("W" ((in-mode . "mu4e-view-mode")))
										   ("R" ((in-mode . "mu4e-headers-mode")))
										   ("W" ((in-mode . "mu4e-headers-mode"))))
		  )
	;; (add-to-list 'org-capture-templates  ;; comma ,(format) needs to be
	;; added later
	;;              `("X" "Wait for Reply" entry
	;;                (file+headline da-gtd "E-mail")
	;;                ,(format "%s\n%s\n%s" "* WAIT for reply %:subject" da-property-string "%i%?")))
	:bind
	(("C-c c" . org-capture)
	 ("C-c t" . (lambda () (interactive "") (org-capture nil "t")))
	 ("C-c T" . (lambda () (interactive "") (org-capture nil "T")))
	 ("C-c R d" . nemacs-org-capture-review-daily)
	 ("C-c R w" . my-new-weekly-review))
	)
  (use-package org-super-agenda)        ;groups query demanded to org-agenda org-ql
  (use-package org-agenda :straight org
    :bind
	(("<f14> a a" . (lambda () (interactive "") (org-agenda nil "a")))
     :map
	 org-agenda-mode-map
     ("Z" . counsel-org-tag-agenda)
     ;; ("P" . "< \t")
     ("C-a" . org-agenda))
    :hook
    (org-agenda-mode-hook . (lambda () (hl-line-mode) (setq line-spacing 0.0)))
    :config
    (setq org-agenda-confirm-kill 1)
    (setq org-agenda-show-all-dates t)
    (setq org-agenda-show-outline-path nil)
    ;; All the "skip" need to be reviewed
    (setq org-agenda-skip-additional-timestamps-same-entry t)
    ;; (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
    (setq org-agenda-skip-deadline-prewarning-if-scheduled nil)
    (setq org-agenda-skip-scheduled-delay-if-deadline t)
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
    (setq org-agenda-skip-timestamp-if-done t)
    (setq org-habit-show-habits-only-for-today nil)
    (setq org-agenda-search-headline-for-time nil)
    (setq org-agenda-start-on-weekday 1)  ; Monday
    (setq org-agenda-start-with-follow-mode nil)
    (setq org-agenda-timegrid-use-ampm nil)
    (setq org-agenda-time-grid
          '((daily today require-timed)
            (0800 1000 1200 1400 1600 1800 2000)
            "      " "················"))
    (setq org-agenda-use-time-grid t)
    (setq org-agenda-window-setup 'current-window)
    (setq org-agenda-restore-windows-after-quit t)
    (setq org-agenda-todo-list-sublevels t)
    (setq                               ; Agenda display
     org-agenda-dim-blocked-tasks t     ; Dim blocked tasks
     org-agenda-show-future-repeats nil ; 'next to view this and the next.
     org-agenda-search-view-always-boolean t ; Lazy boolean search =C-c a s=
     org-agenda-sticky t)
    ;; XXX: custom agenda view http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
    ;; https://github.com/mwfogleman/.emacs.d/blob/master/michael.org
    ;; https://gist.github.com/mwfogleman/267b6bc7e512826a2c36cb57f0e3d854
    (progn                                          ; == Agenda ==
      ;; For tag searches ignore tasks with scheduled and deadline dates FIXME better control this in each agenda custom view
      (setq org-agenda-tags-todo-honor-ignore-options t) ; needed to
      ;; avoid seeing missed tasks in my unscheduled view; next tasks in
      ;; daily review view; in w but W is ok; 
      ;; all properties are inherited
      ;; (setq org-use-property-inheritance t) ;; XXX:  to be used with STYLE, e.g. habit not scheduled
      ;; (defun nemacs-org-agenda-startup ()
      ;;   (interactive)
      ;;   (org-agenda :keys "gtd"))
      ;;        (:map org-agenda-mode-map
      ;;              ("g" . org-gcal-fetch)))
      ;; (org-agenda-skip-deadline-if-done nil)
      ;; (org-agenda-skip-scheduled-if-done nil)
      (defvar in-7days (org-read-date nil nil "+7d"))
      (defvar in-14days (org-read-date nil nil "+14d"))
      ;; (setq org-agenda-follow-indirect t)
      ;; FIXME: could help following projects individually
      (advice-add 'org-agenda-goto :after
                  (lambda (&rest args)
                    (org-narrow-to-subtree)))
      (defvar da-super-agenda-groups
        '((:name "Overdue (WORK)"
                 :and (:tag "WORK" :deadline past)
                 :and (:tag "WORK" :scheduled past)
                 :order 1)
          (:name "Overdue (PERSONAL)"
                 :and (:tag "PERSONAL" :deadline past)
                 :and (:tag "PERSONAL" :scheduled past)
                 :order 21)
          (:name "Overdue (Unassigned)"
                 :deadline past
                 :scheduled past
                 :order 11)
          (:name "Scheduled tasks and future deadlines (WORK)"
                 :and (:tag "WORK" :deadline today)
                 :and (:tag "WORK" :deadline future)
                 :and (:tag "WORK" :scheduled today)
                 :and (:tag "WORK" :scheduled future)
                 :order 3)
          (:name "Scheduled tasks and future deadlines (PERSONAL)"
                 :and (:tag "PERSONAL" :deadline today)
                 :and (:tag "PERSONAL" :deadline future)
                 :and (:tag "PERSONAL" :scheduled today)
                 :and (:tag "PERSONAL" :scheduled future)
                 :order 23)
          (:name "Scheduled tasks and future deadlines (Unassigned)"
                 :deadline today
                 :deadline future
                 :scheduled today
                 :scheduled future
                 :order 13)
          (:name "Unscheduled (WORK)"
                 :tag "WORK"
                 :order 2)
          (:name "Unscheduled (PERSONAL)"
                 :tag "PERSONAL"
                 :order 22)
          (:name "Unscheduled (Unassigned)"
                 :anything t
                 :order 12)
          ))
      (setq org-agenda-custom-commands
            '(
              ("a" "Actions list [today]"
               ((org-super-agenda-mode)
                (agenda "" ((org-super-agenda-groups
                             '((:name "Overdue"
                                      :scheduled past
                                      :deadline past)
                               (:name "Today schedule"
                                      :time-grid t
                                      :deadline today
                                      :scheduled today
                                      :order 1)
                               (:name "Due Tomorrow"
                                      :deadline future
                                      :order 2)))))
                (alltodo "Important and Further picks"
                         ((org-agenda-overriding-header "")
                          (org-agenda-todo-ignore-scheduled t) ;like 'all
                          (org-agenda-todo-ignore-deadlines 'near)
                          (org-super-agenda-groups
                           '((:name "Important" :priority>="A")
                             (:discard (:scheduled t :deadline t))
                             (:name "Quick Picks" :and
                                    (:todo ("TODO" "NEXT") :effort< "0:15"))
                             (:discard (:anything t)))))))
               ((org-agenda-span 'day)
                (org-agenda-sorting-strategy '(todo-state-down priority-down category-keep habit-down))
                (org-agenda-skip-deadline-if-done nil) ;;XXX: those need to
                ;;be reviewed globally
                (org-agenda-skip-scheduled-if-done nil)
                (org-deadline-warning-days 1)
                (org-agenda-compact-blocks t)))
              ("d" "Daily review"
               ((org-super-agenda-mode)
                (agenda "Today" ((org-agenda-span 1)
                                 (org-deadline-warning-days 14)
                                 (org-super-agenda-groups
                                  `((:log t)
                                    (:discard (:todo ("DONE" "CANC")))
                                    (:name "Overdue deadlines" :deadline past)
                                    (:name "Overdue schedule" :scheduled past)
                                    (:name "Agenda" :time-grid t
                                           :deadline today
                                           :scheduled today :order 2)
                                    ;; works in agenda using \` instead of \'
                                    (:discard (:deadline (before ,in-7days)))
                                    (:name "Due soon" :deadline future)
                                    ))))
                (agenda "" ((org-agenda-start-day "+1d")
                            (org-agenda-span 6)
                            (org-agenda-time-grid
                             '((weekly) (0800 1300 1800)
                               "      " "················"))
                            (org-super-agenda-groups
                             '((:discard (:todo ("DONE" "CANC")))
                               (:name none :time-grid t
                                      :scheduled future
                                      :deadline future
                                      )))))
                (tags "-WAITING-PASSED-HOLDING-MAYBE" ;do not use -todo for refile archive
                      ((org-agenda-overriding-header "")
                       (org-agenda-todo-ignore-scheduled t)
                       (org-tags-match-list-sublevels 'indented)
                       (org-agenda-sorting-strategy '(todo-state-down priority-down category-keep habit-down))
                       (org-super-agenda-groups
                        `(
                          (:name "Tasks to refile" :tag "REFILE")
                          (:name "Tasks to archive"
                                 :and (:not (:tag ("NOTE")) :todo ("DONE" "CANC")))
                          (:discard (:and (:tag ("NOTE") :todo ("DONE" "CANC"))))
                          (:discard (:not (:todo t))) ;;XXX: not needed
                          (:discard (:deadline t :scheduled t))
                          ;; (:name "Future deadlines" :deadline (after ,in-14days) :order 1)
                          ;; (:name "Next-week deadlines" :deadline (after ,in-7days))
                          ;; (:name "Due within next 7 days" :deadline future)
                          (:discard (:not (:priority "A" :todo ("PASS" "WAIT" "NEXT"))))
                          (:name "Standalone tasks" :category "gtd" :order 0)
                          (:auto-outline-path t)
                          ;; (:auto-parent t)
                          (:auto-category t))))))
               ((org-agenda-include-diary nil)
                (org-agenda-sorting-strategy '(time-up habit-up tag-down category-keep priority-down))
                (org-agenda-compact-blocks t)))
              
              ("L" "Journal"
               tags "*"
               ((org-agenda-files '("~/Sync/box/org/journal.org")))
               ) ; XXX: finish this and review to
              ("b" "Backwards calendar loops"
               agenda ""
               ((org-agenda-overriding-header "Backwards calendar loops")
                ;; (org-agenda-overriding-columns-format "%20ITEM %DEADLINE")
                ;; (org-agenda-view-columns-initially t)
                (org-agenda-span 10)
                (org-agenda-start-day "-10d")
                (org-agenda-start-with-log-mode t)
                (org-agenda-include-diary nil)
                (org-agenda-use-time-grid t)))
              ("f" "Forwards loops, habits and recurring tasks"
               (
                (org-super-agenda-mode)
                (agenda "" ((org-agenda-files da-agenda-and-refile-files)
                            (org-super-agenda-groups
                             `((:discard (:habit t))
                               (:discard (:tag "recurring"))
                               (:name "Deadlines" :deadline future :order 0)
                               (:discard (:anything t))))
                            (org-agenda-overriding-header "")
                            (org-habit-show-habits-only-for-today nil)
                            (org-agenda-span 1)
                            (org-agenda-show-all-dates nil)
                            (org-agenda-show-future-repeats 'next)
                            (org-agenda-include-diary nil)
                            (org-agenda-todo-ignore-scheduled nil)
                            (org-agenda-tags-todo-honor-ignore-options nil)
                            (org-deadline-warning-days 730)
                            ))
                (tags "*" ((org-agenda-files da-agenda-and-refile-files)
                           (org-agenda-overriding-header "")
                           (org-super-agenda-groups
                            `((:discard (:tag "recurring"))
                              (:discard (:and (:habit t :scheduled (after ,in-14days))))
                              (:discard (:scheduled past))
                              (:discard (:date nil)) ; (:discard (:not (:scheduled t)))
                              (:auto-planning t)
                              ))))
                (agenda "" ((org-agenda-files da-agenda-and-refile-files)
                            (org-super-agenda-groups
                             `((:discard (:habit t))
                               (:name none :tag "recurring")
                               (:discard (:anything t))))
                            (org-agenda-overriding-header "Recurring deadlines")
                            (org-agenda-span 0)
                            (org-agenda-include-diary nil)
                            (org-deadline-warning-days 730)))
                (org-ql-block '(and (habit) (scheduled :from +15))
                              ((org-ql-block-header "Habits (+2w)")))
                )
               (
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-todo-ignore-deadlines nil)
                ;; (org-agenda-window-setup 'other-frame);XXX: i3
                (org-agenda-todo-ignore-scheduled 'past)
                (org-agenda-sorting-strategy '(scheduled-up deadline-up ts-up))))
              ("0" "Tasks to refile or archive"
               ((tags "REFILE" ((org-agenda-overriding-header "Tasks to Refile")))
                (tags "-NOTE-REFILE/DONE|CANC"
                      ((org-agenda-overriding-header "Tasks to Archive")))
                )
               (        (org-agenda-todo-ignore-scheduled nil)
                        (org-agenda-todo-ignore-deadlines nil)
                        ))
              ("l" "Standalone action list"
               ((org-super-agenda-mode)
                (tags "-proj-recurring-STYLE=\"habit\"/TODO|NEXT")) ; -STYLE=\"habit\" is not inherited
               ((org-super-agenda-groups da-super-agenda-groups)
                (org-agenda-sorting-strategy '(priority-down todo-state-down category-keep habit-down))
                (org-tags-match-list-sublevels 'indented)))
              ("w" "Follow-up list"
               ((org-super-agenda-mode)
                (tags "-proj/!WAIT|PASS"
                      ((org-agenda-overriding-header "Follow-up tasks list")))
                (tags "+proj/!WAIT|PASS"
                      ((org-agenda-overriding-header "Follow-up projects list")
                       (org-tags-exclude-from-inheritance '("proj"))))
                )
               ((org-super-agenda-groups da-super-agenda-groups)
                (org-tags-match-list-sublevels 'indented)))
              ("j" "Projects list"
               ((org-super-agenda-mode)
                ;; (tags "+proj/-HOLD-MAYB-PASS-WAIT-CANC-DONE"; -Proj=\"ignore\"
                (tags "+proj"
                      ((org-agenda-overriding-header "Active projects")
                       (org-tags-exclude-from-inheritance '("proj")) ; (org-use-tag-inheritance nil)
                       (org-super-agenda-groups da-super-agenda-groups)
                       ;; (:name "Stuck work projects" :and (:tag "WORK" :not (:children "NEXT")))
                       (org-agenda-skip-function '(org-agenda-skip-subtree-if 'nottodo '("NEXT")))))
                (tags "+proj"
                      ((org-agenda-overriding-header "Stuck projects")
                       (org-tags-exclude-from-inheritance '("proj")) ; (org-use-tag-inheritance nil)
                       ;; (org-agenda-skip-function '(org-agenda-skip-subtree-if 'todo '("NEXT")))
                       (org-super-agenda-groups (append '((:discard (:children "NEXT")))
                                                        da-super-agenda-groups))))
                (tags "+proj"
                      ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:discard (:regexp ":proj:"))
                          (:name "DONE" :todo "DONE")
                          (:name "NEXT" :todo "NEXT")
                          (:name "To follow-up" :todo ("PASS" "WAIT"))
                          (:name "TODO" :todo "TODO")
                          (:name "To reconsider" :todo ("HOLD" "MAYB"))
                          (:name "CANC" :todo "CANC")
                          (:discard (:anything t)))))))
               ((org-tags-match-list-sublevels 'indented)
                (org-agenda-sorting-strategy '(priority-down))
                ;; (org-use-property-inheritance t)
                (org-agenda-tag-filter-preset '("-WAITING" "-PASSED" "-HOLDING" "-MAYBE" "-ENDED"))
                ))
              ("h" "Tasks and projects on hold"
               ((org-super-agenda-mode)
                (tags "-proj/+HOLD"
                      ((org-agenda-overriding-header "Tasks on hold")))
                (tags "+proj/+HOLD"
                      ((org-agenda-overriding-header "Projects on hold")
                       (org-tags-exclude-from-inheritance '("proj"))))
                )
               ((org-super-agenda-groups da-super-agenda-groups)
                (org-tags-match-list-sublevels 'indented)))
              ("i" "Idea and hold, maybe, someday tasks and-or projects"
               ((org-super-agenda-mode)
                (tags "-proj/+MAYB"
                      ((org-agenda-overriding-header "Tasks for someday")
                       (org-super-agenda-groups da-super-agenda-groups)))
                (tags "+proj/+MAYB"
                      ((org-agenda-overriding-header "Projects for someday")
                       (org-tags-exclude-from-inheritance '("proj"))
                       (org-super-agenda-groups da-super-agenda-groups)))
                (tags "+idea"
                      ((org-agenda-overriding-header "Ideas")
                       (org-tags-match-list-sublevels 'indented)
                       (org-agenda-sorting-strategy '(todo-state-up priority-down category-keep tag-down))))
                ))
              ("c" . "Contexts")
              ("ce" "@errand" tags-todo "@errand")
              ("cf" "@fbk" tags-todo "@fbk")
              ("ch" "@home" tags-todo "@home")
              ("cd" "@dati" tags-todo "@dati")
              ("ct" "@telephone" tags-todo "@telephone")
              ("E" "Export agenda"
               ((agenda "" ((org-agenda-ndays 7)                      ;; overview of appointments
                            (org-agenda-start-on-weekday nil)         ;; calendar begins today
                            (org-agenda-repeating-timestamp-show-all t)
                            (org-agenda-entry-types '(:timestamp :sexp))))
                (agenda "" ((org-agenda-ndays 1)                      ;; daily agenda
                            (org-deadline-warning-days 700)             ;; 7 day advanced warning for deadlines
                            (org-agenda-todo-keyword-format "[ ]")
                            (org-agenda-scheduled-leaders '("" ""))
                            (org-agenda-prefix-format "%t%s")))
                (todo "TODO"                                          ;; todos sorted by context
                      ((org-agenda-prefix-format "[ ] %T: ")
                       (org-agenda-sorting-strategy '(tag-up priority-down))
                       (org-agenda-todo-keyword-format "")
                       (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
               ((org-agenda-with-colors nil)
                (org-agenda-remove-tags t)
                (htmlize-output-type 'css)
                (ps-number-of-columns 2)
                (ps-landscape-mode t))
               ("~/agenda.pdf" "~/agenda.html"))
              ;; other commands go here
              )))
    )
  (use-package jupyter
    :after (org)
	:init (eval-after-load 'org-babel (require 'jupyter))
    :defines org-babel-default-header-args:jupyter-python
    :bind (:map org-mode-map
                ("C-c s" . org-babel-jupyter-scratch-buffer)
                ("C-c <DEL>" . jupyter-org-clear-all-results)
                ("H-<tab>" . jupyter-org-hydra/body))
    :config
    (setq jupyter-repl-prompt-margin-width 4)
    (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                         (:session . "py01")
                                                         (:kernel . "py39"))))
  (use-package org
    :commands (org-capture-finalize
			   org-speed-move-safe
			   org-narrow-to-subtree
			   org-clock-in)
    :functions (org-read-date
				org-get-tags
				org-entry-delete
				org-entry-put
				org-toggle-tag)
    :defines (org-state)
    :preface
    (defun dpa-proj-state-change-hook()
      ;; WAIT PASS HOLD MAYB state changes trigger tag changes only for
      ;; projects. (in place of org-todo-state-tags-triggers)
      ;; Do not handle ARCHIVE and ENDED tags.
      (if (member  "proj" (org-get-tags nil t))
          (cond ((equal org-state "DONE")
                 (org-toggle-tag "WAITING" 'off) (org-toggle-tag "PASSED" 'off) (org-toggle-tag "HOLDING" 'off) (org-toggle-tag "MAYBE" 'off))
                ((equal (length org-state) 0)
                 (org-toggle-tag "WAITING" 'off) (org-toggle-tag "PASSED" 'off) (org-toggle-tag "HOLDING" 'off) (org-toggle-tag "MAYBE" 'off))
                ((equal org-state "TODO")
                 (org-toggle-tag "WAITING" 'off) (org-toggle-tag "PASSED" 'off) (org-toggle-tag "HOLDING" 'off) (org-toggle-tag "MAYBE" 'off))
                ((equal org-state "NEXT")
                 (org-toggle-tag "WAITING" 'off) (org-toggle-tag "PASSED" 'off) (org-toggle-tag "HOLDING" 'off) (org-toggle-tag "MAYBE" 'off))
                ((equal org-state "CANC")
                 (org-toggle-tag "WAITING" 'off) (org-toggle-tag "PASSED" 'off) (org-toggle-tag "HOLDING" 'off) (org-toggle-tag "MAYBE" 'off))
                ((equal org-state "WAIT")
                 (org-toggle-tag "WAITING" 'on) (org-toggle-tag "PASSED" 'off) (org-toggle-tag "HOLDING" 'off) (org-toggle-tag "MAYBE" 'off))
                ((equal org-state "PASS")
                 (org-toggle-tag "WAITING" 'off) (org-toggle-tag "PASSED" 'on) (org-toggle-tag "HOLDING" 'off) (org-toggle-tag "MAYBE" 'off))
                ((equal org-state "HOLD")
                 (org-toggle-tag "WAITING" 'off) (org-toggle-tag "PASSED" 'off) (org-toggle-tag "HOLDING" 'on) (org-toggle-tag "MAYBE" 'off))
                ((equal org-state "MAYB")
                 (org-toggle-tag "WAITING" 'off) (org-toggle-tag "PASSED" 'off) (org-toggle-tag "HOLDING" 'off) (org-toggle-tag "MAYBE" 'on))
                )
        ;; Remove project dedicated tags at the first state change.
        (progn (org-toggle-tag "WAITING" 'off) (org-toggle-tag "PASSED" 'off) (org-toggle-tag "HOLDING" 'off) (org-toggle-tag "MAYBE" 'off))))
    (defun internet-up-p (&optional host)
      "Check internet connectivity. Default HOST is google."
      (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                         (if host host "www.google.com"))))
    (defun fetch-calendar ()
      "Fetch gcals."
      (interactive)
      (when (internet-up-p) (org-gcal-fetch)))
    (defun my-babelsrc-org-mode-hook ()
      "Custom `org-mode' `typo-mode' behavior."
      (typo-mode 1)
      (add-hook 'typo-disable-electricity-functions 'org-in-src-block-p nil :local))
    :hook
    ((org-mode-hook . visual-line-mode)
     (org-mode-hook . flyspell-mode)
     (org-mode-hook . variable-pitch-mode)
     (org-mode-hook . my-babelsrc-org-mode-hook)
     (org-mode-hook . prettify-symbols-mode)
     (org-after-todo-state-change-hook . dpa-proj-state-change-hook))
    :bind
    (("C-c a" . org-agenda)
     ("M-h" . mark-word)
     ("M-S-h" . org-mark-element)
     :map
	 org-mode-map
	 ("H-<return>" . org-next-link)
	 ("H-S-<return>" . org-previous-link)
	 ("<f14> t o i" . org-indent-mode)
     ("<f14> c o o" . counsel-outline)
     ("<f14> c o f" . counsel-org-file)
     ("<f14> c o l" . counsel-org-link)
     ("<f14> c o t" . counsel-org-tag))
    :config
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch);; :background "burlywood")
	(set-face-attribute 'org-block nil :inherit '(fixed-pitch shadow))
	(set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch
						:background "burlywood" :foreground "darkred" :underline "darkred")
	(set-face-attribute 'org-block-end-line nil :inherit 'fixed-pitch
						:background "burlywood" :foreground "darkred" :overline "yellow")
	(set-face-attribute 'org-code nil :inherit 'fixed-pitch)
	(set-face-attribute 'org-link nil :inherit 'fixed-pitch)
	(set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch)
	(set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch)
	;; (font-lock-add-keywords 'org-mode
	;;                         '(("^ *\\([-]\\) "
	;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
	(setq-default prettify-symbols-alist '(("#+begin_src" . "➙")
                                           ("#+end_src" . "⇚")
                                           ("#+BEGIN_SRC" . "⤐")
                                           ("#+END_SRC" . "⌗")
                                           ("#+RESULTS:" . "⚡")
                                           ("=>" . "⇨")))
	(setq prettify-symbols-unprettify-at-point 'right-edge)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq-default org-startup-indented t)
	(setq org-refile-use-cache t)                 ;;
	(setq org-reverse-note-order nil)   ; default ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(use-package org-lint :straight org
	  :bind (:map
			 org-mode-map
			 ("<f14> e o" . org-lint))
	  )
    (use-package org-compat :straight org
	  :config
	  (org-add-link-type "mpv" (lambda (path) (browse-url-xdg-open path)))
	  )
	(use-package org-attach :straight org
	  :config
	  (setq org-attach-use-inheritance t)
	  )
	(use-package ol
	  :straight org
      :bind
	  (:map
	   org-mode-map
       ("C-c l" . org-store-link)
       ("C-c S-l" . org-toggle-link-display)
       ("C-c C-S-l" . org-insert-last-stored-link))
	  :config
      (setq org-link-keep-stored-after-insertion t)
      )
	(use-package org-indent :straight org
      :config
      (setq org-indent-indentation-per-level 1)
      )
	(setq-default org-image-actual-width 620) ;; nil ; so you can specify :width
	;; org-image-actual-width (/ (display-pixel-width) 4)
	(setq                               ; org buffers with babel
	 org-return-follows-link t
	 org-hide-emphasis-markers t              ; a better word processor
	 org-cycle-separator-lines 2              ; default=2
	 org-highlight-latex-and-related '(latex) ; Change color of inline latex $y=mx+c$
	 org-src-fontify-natively t               ; font in src blocks
	 org-src-window-setup 'current-window
	 org-src-preserve-indentation t           ; indentation in src blocks
	 org-edit-src-content-indentation 2         ; default
	 org-src-tab-acts-natively t                ; tab in src blocks
	 ;; org-hide-leading-stars t
	 org-pretty-entities t
	 ;; org-odd-levels-only t
	 org-confirm-babel-evaluate nil ; don't prompt to confirm evaluation every time
	 org-columns-default-format
	 "%48ITEM(Task) %TODO(todo) %ALLTAGS %SCHEDULED %6Effort(Effort){:} %6CLOCKSUM{:} %DEADLINE"
	 org-M-RET-may-split-line '((default . t)
								(headline . nil)
								(item . nil)
								(table . nil)))
	(org-babel-do-load-languages 'org-babel-load-languages
								 '((plantuml . t)
                                   (emacs-lisp . t)
                                   (awk . t)
                                   (python . t)
                                   (gnuplot . t)
                                   (latex . t)
                                   (R . t)
                                   (ditaa . t)
                                   (makefile . t)
                                   (dot . t)
                                   (shell . t)
                                   (perl . t)
                                   (jupyter . t) ;should be the last one
                                   ))
	(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
	(use-package ob-jupyter :straight jupyter
      :config
      ;; Overrides python and must be after org-babel-do-load-languages
      (org-babel-jupyter-override-src-block "python")
	  )
	(use-package ob-ditaa :straight org
      :config
      (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
	  )
	(use-package ox-latex :straight org
	  ;; XXX: https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-LaTeX.html
	  :config
	  ;; (add-to-list 'org-latex-logfiles-extensions '("lof" "lot" "tex="
	  ;; "dvi" "fdb_latexmk" "brf" "entoc" "ps" "spl" "bbl"); FIXME:
	  (setq org-latex-remove-logfiles t)
	  ;; (add-to-list 'org-latex-logfiles-extensions '("tex" "bbl"))
	  ;; (setq org-latex-pdf-process
	  ;; 		'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  ;; 		  "bibtex %b"
	  ;; 		  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
	  (setq org-latex-pdf-process
			'("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -bibtex -f %f"
			  "latexmk -c"))
	  (setq org-latex-listings 'minted)	; 'listings
	  (setq org-latex-packages-alist '(("" "minted")))
	  (setq org-latex-minted-options
			'(("breaklines" "true")
			  ("breakanywhere" "true")
			  ("tabsize" "4")
			  ("autogobble")
			  ("linenos")
			  ("numbersep" "0.5cm")
			  ("xleftmargin" "1cm")
			  ("frame" "single")))
	  (add-to-list 'org-latex-classes	; letter
				   '("letter"
					 "\\documentclass{letter}"
					 ("\\section{%s}" . "\\section*{%s}")
					 ("\\subsection{%s}" . "\\subsection*{%s}")
					 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
	  (add-to-list 'org-latex-classes	; mnras
				   '("mnras" "\\documentclass{mnras}"
					 ("\\section{%s}" . "\\section*{%s}")
					 ("\\subsection{%s}" . "\\subsection*{%s}")
					 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
					 ))
	  (add-to-list 'org-latex-classes	; koma-article
				   '("koma-article" "\\documentclass{scrartcl}
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
	  (add-to-list 'org-latex-classes	; IEEEtran
				   '("IEEEtran" "\\documentclass[11pt]{IEEEtran}"
					 ("\\section{%s}" . "\\section*{%s}")
					 ("\\subsection{%s}" . "\\subsection*{%s}")
					 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
					 ("\\paragraph{%s}" . "\\paragraph*{%s}")
					 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
	  (add-to-list 'org-latex-classes
				   '("apa6"
					 "\\documentclass{apa6}"
					 ("\\section{%s}" . "\\section*{%s}")
					 ("\\subsection{%s}" . "\\subsection*{%s}")
					 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
					 ("\\paragraph{%s}" . "\\paragraph*{%s}")
					 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
	  (add-to-list 'org-latex-classes	; extarticle without default packages
				   '("org-plain-extarticle"
					 "\\documentclass{extarticle}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
					 ("\\section{%s}" . "\\section*{%s}")
					 ("\\subsection{%s}" . "\\subsection*{%s}")
					 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
					 ("\\paragraph{%s}" . "\\paragraph*{%s}")
					 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
	  )
	(use-package ox-beamer :straight org
	  :init (eval-after-load 'ox '(require 'ox-beamer))
	  )
	(use-package ox-md :straight org
	  :init (eval-after-load 'ox '(require 'ox-md))
	  )
	(use-package ox-koma-letter :straight org
	  :init (eval-after-load 'ox '(require 'ox-koma-letter))
	  )
	(use-package ox-reveal				; FIXME
	  :config
	  (setq org-reveal-root "/home/dan/.pandoc/reveal.js")
	  (eval-after-load 'ox '(require 'ox-reveal))
	  )
	(use-package ox-rst
	  :init (eval-after-load 'ox '(require 'ox-rst))
	  )
	(use-package ox-pandoc
	  :init (eval-after-load 'ox '(require 'ox-pandoc))
	  )
	(use-package ox-twbs
	  :init (eval-after-load 'ox '(require 'ox-twbs))
	  )
	(use-package auctex :defer t)
	(use-package tex :straight auctex
	  :config
	  (setq TeX-auto-save t)
	  (setq TeX-parse-self t)
	  ;; (setq TeX-PDF-mode t))
	  (setq-default TeX-master nil)
	  )
	(use-package tex-buf :straight auctex
	  :config
	  (setq TeX-save-query nil))
	(use-package cdlatex)
	;; (use-package ob-exp
	;; 	:straight nil
	;; 	:config (setq org-export-use-babel nil)) ; Same of :eval never-export in header.
	(add-to-list 'org-structure-template-alist '("sl" . "src emacs-lisp"))
	(add-to-list 'org-structure-template-alist '("sb" . "src sh :results output :exports both"))
	(add-to-list 'org-structure-template-alist '("sB" . "src sh :session bash :results output :exports both"))
	(add-to-list 'org-structure-template-alist '("sj" . "src jupyter-python"))
	;; org-babel-default-header-args:sh    '((:results . "output replace"))
	;; org-babel-default-header-args:bash  '((:results . "output replace"))
	;; org-babel-default-header-args:shell '((:results . "output replace"))
	(setq org-fontify-done-headline t)
	(setq org-fontify-quote-and-verse-blocks t)
	(setq org-fontify-whole-heading-line t)
	(setq org-enforce-todo-dependencies t)                                 ;;
	(setq org-enforce-todo-checkbox-dependencies t)                        ;;
	(setq org-track-ordered-property-with-tag t)                           ;;
	;; general                                                             ;;
	(setq org-special-ctrl-a/e t)                                          ;;
	(setq org-special-ctrl-k t)                                            ;;
	(setq org-catch-invisible-edits 'show)                                 ;;
	(setq org-loop-over-headlines-in-active-region nil)                    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; init of my-gtd.conf
	;; A bare minimum simple starting to personalizing org for gtd.
	;; https://orgmode.org/worg/org-configs/org-customization-guide.html
	(progn                              ; Define variables
	  ;; (load-library "find-lisp")
	  ;; (declare-function find-lisp-find-files "find-lisp")
	  (defvar da-gtd "~/Sync/box/org/gtd.org")
	  (defvar da-agenda-and-refile-files
		(append '("~/Sync/box/org/gtd.org"
				  "~/Sync/box/org/ideas.org"
				  "~/Sync/box/org/inbox.org"
				  "~/Sync/box/org/journal.org"
				  "~/Sync/box/org/projects.org")
				;; "~/Sync/box/org/TODOs.org" ;; target for org-projectile REVIEW:
				;; "~/Sync/box/org/shopping.org")
				(directory-files "~/Sync/proj/" t "\\.org$")
				(directory-files "~/Sync/notes/home/" t "\\.org$")
				(directory-files-recursively "~/Sync/notes/arch/" "\\.org$") ; org files in all sub folders
				)))
	(setq                               ; (1) Agenda files
	 org-directory "~/Sync/box/org"
	 org-agenda-files (append da-agenda-and-refile-files
							  '("~/Sync/box/org/inbox.box.org"
								"~/Sync/box/org/gcal/"))
	 org-agenda-diary-file "~/Sync/box/org/journal.org"
	 org-agenda-include-diary t)        ; to display holidays in org-agenda
	(use-package org-archive :straight org
	  :config
	  (setq                             ; (2) Archives
	   org-archive-location "~/Sync/box/org/archives/%s_archive::"
	   org-archive-file-header-format "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n"))
	(setq org-agenda-text-search-extra-files `(agenda-archives) ; Search also in archives
		  org-deadline-warning-days 7                           ; FIXME: n
		  )
	(setq                               ; (3) Refile
	 org-refile-use-outline-path 'file      ; Full path preceded by filename
	 org-outline-path-complete-in-steps nil ; Complete directly with counsel
	 org-refile-allow-creating-parent-nodes 'confirm ; Ask confirmation when creating parent tasks
	 org-refile-targets '(
						  (nil :maxlevel . 9)
						  ("~/Sync/box/org/shopping.org" :maxlevel . 5)
						  (da-agenda-and-refile-files :maxlevel . 5)))
	;; (da-agenda-and-refile-files :maxlevel . 5)))
	;; (setq                               ; (4) Stuck project
	;;  org-stuck-projects '("+proj/-DONE-HOLD-MAYB-PASS-WAIT" ("NEXT") nil ""))
	(setq                               ; (5) Todo states
	 org-todo-keywords
	 ;; tracking state changes @: note !:date entering/leaving
	 '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	   (sequence "WAIT(w@/!)" "PASS(p!/!)" "HOLD(h@/@)" "MAYB(m/@)" "|" "CANC(c@/@)"))
	 org-todo-keyword-faces      ; Faces
	 '(("TODO" :foreground "crimson" :weight bold :box (:line-width 2 :style released-button))
	   ("NEXT" :foreground "light blue" :weight bold :box (:line-width 2 :style released-button))
	   ;; ("APPT" :foreground "yellow" :weight bold)
	   ("WAIT" (:foreground "orange" :weight bold))
	   ("PASS" :foreground "SpringGreen" :weight bold)
	   ("HOLD" :foreground "SaddleBrown" :weight bold )
	   ("MAYB" :foreground "MediumAquamarine" :weight bold)
	   ("DONE" :foreground "forest green" :weight bold)
	   ("CANC" (:foreground "forest green" :weight bold :strike-through t))
	   )
	 org-use-fast-todo-selection t
	 org-treat-S-cursor-todo-selection-as-state-change nil ; no log here
	 org-log-into-drawer t
	 org-log-done 'time
	 org-log-note-clock-out nil
	 org-log-redeadline nil
	 org-log-reschedule nil
	 org-read-date-prefer-future 'time)
	(setq                               ; (5) Tags for contexts
	 org-tag-alist nil                  ; default
	 org-tag-persistent-alist '((:startgroup . nil) ; mutually exclusive
								("@errand" . ?e)
								("@fbk" . ?f)
								("@home" . ?h)
								(:endgroup . nil)
								("@dati" . ?d)
								("@telephone" . ?t)
								("PERSONAL" . ?p)
								("WORK" . ?w)
								("idea" . ?i)
								("proj" . ?j)
								("buy" . ?b)
								("study" . ?s)
								)
	 org-tag-faces '(("WORK" :foreground "green")
					 ("PERSONAL" :foreground "orange")
				   	 ("proj" :weight bold)
					 ("@fbk" :weight italic))
	 org-fast-tag-selection-single-key t ; 'expert does't show
	 org-fast-tag-selection-include-todo nil
	 org-tags-column -82
	 org-support-shift-select t)
	;; Enable auto clock resolution for finding open clocks
	(use-package org-clock :straight org
	  :config
	  (setq org-clock-out-remove-zero-time-clocks t ; Removes clocked tasks with 0:00 duration
			org-clock-auto-clock-resolution (quote when-no-clock-is-running))
	  )
	;; Resume clocking task when emacs is restarted
	(org-clock-persistence-insinuate)
	;; XXX:  global Effort estimate values  ;http://doc.norang.ca/org-mode.html
	;; global STYLE property values for completion
	(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 6:00 0:00")
										("STYLE_ALL" . "habit"))))
	)
  (use-package sphinx-mode)
  (use-package ob-async
    :after ob
    :config
    (setq ob-async-no-async-languages-alist
		  '("jupyter-python" "jupyter-julia" "jupyter-R"))
	)
  (use-package org-ql
    :bind ("<f14> / q" . org-ql-search)
	)
  (use-package org-autolist
    :after (org)
    :hook (org-mode-hook . org-autolist-mode)
	)
  (use-package org-bullets
    :after (org)
	:commands org-cycle-list-bullet
    :custom
    (org-bullets-bullet-list '("◉" "○" "▶" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "✸"))
    (org-ellipsis "⤵")
    :preface
    (defhydra hydra-bullets (:color pink :hint nil :exit nil)
      "A hydra for org-bullets."
      ("b" org-cycle-list-bullet "cycle list bullets")
      ("B" org-bullets-mode "toggle" :color blue)
      ("q" nil "cancel" :color blue))
    :bind
    ("<f14> t b" . hydra-bullets/body)
    :hook (org-mode-hook . org-bullets-mode)
	)
  (use-package plantuml-mode
	:after (org)
    :defines org-plantuml-jar-path
    :init
    (setq plantuml-default-exec-mode 'jar)
    (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
    (setq org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
	)
  (use-package graphviz-dot-mode
    :config
	(setq graphviz-dot-indent-width 4)
    (add-to-list 'company-backends 'company-graphviz-dot-backend)
	)
  (use-package gnuplot)
  (use-package org-download
    :after org
    :bind (:map org-mode-map
                ("<f14> d c" . org-download-clipboard)
                ("<f14> d i" . org-download-image)
                ("<f14> d y" . org-download-yank)
                ("<f14> d e" . org-download-edit)
                ("<f14> d k" . org-download-delete)
                ("<f14> d r" . org-download-rename-at-point)
                ("<f14> d R" . org-download-rename-last-file)
                ("<f14> d s" . org-download-screenshot))
    :hook (dired-mode-hook . org-download-enable)
    (org-mode-hook . org-download-enable)
    :config
    (setq org-download-screenshot-method "maim -o -u -s %s")
    )
  (use-package org-cliplink
    :bind ("<f14> i c" . org-cliplink)
    )
  (use-package org-preview-html)
  )
(progn                                  ; calendars
  (use-package calendar
	:straight (:type built-in)
    :hook
    (calendar-today-visible-hook . calendar-mark-today)
	)
  (use-package solar                    ; (2) sunrise and sunset
    :straight nil
    ;; :load-path "/usr/share/emacs/"
    :config
    (setq calendar-latitude 46.067270 ; Borino
          calendar-longitude 11.166153
          calendar-location-name "Trento"
          calendar-time-zone 60))
  (setq holiday-general-holidays          ; (3) Holidays
        '((holiday-fixed 1 1 "Capodanno")
          (holiday-fixed 5 1 "1 Maggio")
          (holiday-fixed 4 25 "Liberazione")
          (holiday-fixed 6 2 "Festa Repubblica")
          (holiday-fixed 7 14 "Bastille Day"))
        holiday-christian-holidays
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
          (holiday-fixed 11 1 "Ognissanti"))
        holiday-bahai-holidays nil
        holiday-hebrew-holidays nil
        holiday-islamic-holidays nil)
  (use-package org-gcal
    :bind
    ("<f14> o g p" . org-gcal-post-at-point) ; (add-hook 'org-capture-before-finalize-hook)
    ("<f14> o g d" . org-gcal-delete-at-point)
    ("<f14> o g g" . org-gcal-sync)
    ("<f14> o g G" . org-gcal-fetch)
    :init
    (which-key-add-key-based-replacements "<f14> o g" "Gcal")
    :config
	(setq org-gcal-auto-archive nil)
    (setq org-gcal-token-file "~/Sync/.emacs/org-gcal/.org-gcal-token"
          org-gcal-client-id "1086004898054-uhp29b0kek41obv1dma52rpog8pr44gu.apps.googleusercontent.com"
          org-gcal-client-secret "sP2Jupy5GKtdDAAgupQrSzc2"
          org-gcal-file-alist '(("danielepietroarosio@gmail.com" .
                                 "~/Sync/box/org/gcal/dpa.org")
                                ("c87gevr5pc3191on8c7nh8b4nc@group.calendar.google.com" .
                                 "~/Sync/box/org/gcal/figli.org")
                                ("tq1af7efj4l9h8glgqi2g5vmsg@group.calendar.google.com" .
                                 "~/Sync/box/org/gcal/IBF.org"))))
  (use-package calfw                    ; apparently need by calfw-org
    :bind
    ("<f14> o g W" . cfw:open-calendar-buffer))
  (use-package calfw-org
    :bind
    (("<f14> o g w" . cfw:open-org-calendar)
     :map org-agenda-mode-map
     ("W" . cfw:open-org-calendar)))
  )
(progn                                  ; Writing
  ;; XXX: abbrev mode C-x a +/g
  ;; XXX: translate-shell =M-|= shell-command-on region
  (use-package wc-mode
    :disabled
    :bind (("C-c w" . wc-mode)
           :map wc-mode-map
           ("C-c C-w" . org-refile)       ; even if it was undefined
           ("C-c w w" . wc-set-word-goal)
           ("C-c w l" . wc-set-line-goal)
           ("C-c w a" . wc-set-char-goal)
           ("C-c w c" . wc-count)
           ("C-c w q" . wc-mode))
    ;; 	"xw"  #'(wc-mode :which-key "word count")
    :config
    (setq wc-modeline-format "Wd.%tw  Ch.%tc"))
  ;; (use-package lsp-ltex
  ;; 	:hook (text-mode-hook . (lambda ()
  ;; 							  (require 'lsp-ltex)
  ;; 							  (lsp))))  ; or lsp-deferred
  ;; (use-package flycheck-languagetool
  ;; 	:hook (text-mode . (lambda ()
  ;; 						 (require 'flycheck-languagetool)))
  ;; 	:init
  ;; 	(setq flycheck-languagetool-server-jar "/usr/share/java/languagetool/languagetool-server.jar"))
  (use-package flycheck-vale
	:commands (flycheck-vale-setup)
	:config (flycheck-vale-setup))
  (use-package langtool
	:commands (langtool-goto-previous-error
               langtool-goto-next-error
               langtool-check
               langtool-correct-buffer
               langtool-check-done
			   langtool-switch-default-language)
	:bind ("<f14> x l" . hydra-LT/body)
	:init
	(which-key-add-key-based-replacements "<f14> x" "teXt writing")
	(defhydra hydra-LT (:color pink :hint nil)
      "A hydra for Langtool."
      ("p" langtool-goto-previous-error "previous")
      ("n" langtool-goto-next-error "next")
      ("c" langtool-check "check")
      ("b" langtool-correct-buffer "correct buffer")
      ("d" langtool-check-done "done")
      ("l" langtool-switch-default-language "switch language")
      ("q" nil "cancel" :color blue))
	(setq langtool-java-classpath
          "/usr/share/languagetool:/usr/share/java/languagetool/*"
          langtool-java-bin "/usr/bin/java"
          langtool-disabled-rules '("WHITESPACE_RULE"
									"EN_UNPAIRED_BRACKETS"
									"COMMA_PARENTHESIS_WHITESPACE"
									"EN_QUOTES")
          langtool-mother-tongue "it"
          langtool-default-language "en-US"))
  (use-package wordnut
	:bind
	("<f14> x w" . wordnut-lookup-current-word)
	("<f14> x W" . wordnut-search))
  (use-package sdcv
	:bind
	("<f14> x s" . sdcv-search-pointer)
	:bind (:map sdcv-mode-map
				("n" . sdcv-next-dictionary)
				("p" . sdcv-previous-dictionary)))
  (use-package dictionary
	:bind
	("<f14> x d" . dictionary-search))
  (use-package synosaurus
	:bind ("<f14> x k" . synosaurus-choose-and-replace))
  (use-package powerthesaurus
	:bind
	("<f14> x p 0" . powerthesaurus-lookup-dwim)
	("<f14> x p p" . powerthesaurus-lookup-synonyms-dwim)
	("<f14> x p a" . powerthesaurus-lookup-antonyms-dwim)
	("<f14> x p d" . powerthesaurus-lookup-definitions-dwim)
	("<f14> x p r" . powerthesaurus-lookup-related-dwim)
	("<f14> x p s" . powerthesaurus-lookup-sentences-dwim))
  (use-package writegood-mode
	:bind
	("<f14> x g" . writegood-mode)
	("<f14> x Gl" . writegood-grade-level)
	("<f14> x Gr" . writegood-reading-ease))
  (use-package artbollocks-mode
	:bind
	("<f14> x a" . artbollocks-mode)
	("<f14> x Ar" . artbollocks-reading-ease)
	("<f14> x AR" . artbollocks-readability-index)
	("<f14> x Al" . artbollocks-grade-level)
	("<f14> x Aw" . artbollocks-word-count)
	("<f14> x As" . artbollocks-sentence-count)) ; XXX: maybe write-good is enough
  (use-package academic-phrases
	:bind
	("<f14> x i" . academic-phrases-by-section)
	("<f14> x I" . academic-phrases))
  (use-package goldendict
	:bind
	("<f14> x c" . goldendict-dwim))
  (use-package google-translate
	:defines google-translate-translation-directions-alist
	:commands (google-translate-at-point
			   google-translate-at-point-reverse)
	:bind
	("<f14> x t" . google-translate-smooth-translate)
	:init
	(setq google-translate-translation-directions-alist
          '(("it" . "en") ("en" . "it") ("it" . "de") ("it" . "fr"))
          google-translate-output-destination kill-ring
          google-translate-enable-ido-completion t
          google-translate-show-phonetic t
		  ;; google-translate-listen-program
          google-translate-pop-up-buffer-set-focus t)
	(defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
	(defun my-google-translate-at-point()
      "reverse translate if prefix"
      (interactive)
      (if current-prefix-arg
          (google-translate-at-point)
		(google-translate-at-point-reverse)))
	(setq google-translate-backend-method 'curl)) ; API changed
  (use-package markdown-mode
	:bind (:map markdown-mode-map
				("<return>" . nil)
				("M-n" . mk-transpose-line-down)
				("M-p" . mk-transpose-line-up))
	:init
	(setq markdown-url-compose-char ?…)
	;; (setq markdown-command "multimarkdown")
	:mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.mkd\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode)))
  (use-package cm-mode                  ;critic markup
	;; :hook (text-mode . cm-mode)
	:bind
	("<f14> x M" . cm-mode)
	("<f14> x m" . cm-prefix-map))
  (use-package typo
	:bind
	("<f14> t T" . typo-global-mode)
	:hook
	(text-mode-hook . typo-mode))
  )
(progn                                  ; Bibliography
  ;; (use-package org-bibtex
  ;;   :ensure org-plus-contrib
  ;;   :hook
  ;;   (org-mode-hook . (lambda () (require 'ox-bibtex)))
  ;;   (org-mode-hook . (lambda () (require 'ol-bibtex)))
  ;;   :defines org-bibtex-file
  ;;   :config
  ;;   (setq org-bibtex-file "~/Sync/biblio/papers.org"))
  (use-package bibtex
    :bind (:map bibtex-mode-map
                ("<tab>" . hs-toggle-hiding)
                ("<backtab>" . hs-hide-all)
                ("C-<tab>" . hs-show-all))
    :hook
    (bibtex-mode-hook . hs-minor-mode)
    :config
	;; Change fields and format
	(setq bibtex-user-optional-fields '(("keywords" "Keywords to describe the entry" "")
										("file" "Link to document file." ":")))
    ;; (setq bibtex-align-at-equal-sign t)
	;; (setq bibtex-text-indentation 28)
    (setq                               ; Bibtex key format
     bibtex-autokey-name-case-convert-function 'capitalize
     bibtex-autokey-name-year-separator ""
     bibtex-autokey-titleword-separator ""
     bibtex-autokey-year-title-separator "_"
     bibtex-autokey-titlewords 3
     bibtex-autokey-titlewords-stretch 0
     bibtex-autokey-titleword-case-convert-function 'capitalize
     bibtex-autokey-titleword-length 9)
	(setq bibtex-dialect 'biblatex))
  (use-package ivy-bibtex
    :bind
    ("<f14> a b" . ivy-bibtex)
    :hook
    (org-mode-hook . (lambda () (require 'ivy-bibtex)))
    (bibtex-mode-hook . (lambda () (require 'ivy-bibtex)))
    ;; :preface
    ;; (defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
    ;;   (let ((bibtex-completion-pdf-open-function
    ;;          (lambda (fpath) (start-process "evince" "*helm-bibtex-evince*" "/usr/bin/evince" fpath))))
    ;;     (bibtex-completion-open-pdf keys fallback-action)))
    :config
    ;; ;; what is the default citation style?
    ;; (setq bibtex-completion-cite-default-command "cite")
    ;; (setq bibtex-completion-cite-default-as-initial-input t)
    ;; ;; open PDFs with our favourite PDF reader
    ;; (setq bibtex-completion-pdf-open-function
    ;;       (lambda (fpath)
    ;;         (call-process do.minimal/pdf-reader nil 0 nil fpath)))
    ;; (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-ignore-order))
    ;; (ivy-bibtex-ivify-action bibtex-completion-open-pdf-external ivy-bibtex-open-pdf-external)
    ;; (ivy-add-actions
    ;;  'ivy-bibtex
    ;;  '(("P" ivy-bibtex-open-pdf-external "Open PDF file in external viewer (if present)")))
    )
  (use-package org-roam-bibtex
    :after org-roam
	:commands (org-roam-bibtex-mode)
    :bind (:map org-mode-map
                ("<f14> n m" . orb-note-actions)
                ("<f14> n b" . orb-insert-link))
	:init (org-roam-bibtex-mode)
	:config
	;; (require 'org-ref)
	(setq orb-insert-interface 'ivy-bibtex)
	(setq orb-insert-link-description "citation")
	(setq orb-preformat-keywords
          '("citekey" "title" "url" "author-or-editor" "keywords" "file" "year" "date" "doi")
		  ;; orb-file-field-extensions '("pdf") ;XXX: can add mp4 dju
	      orb-process-file-keyword t)
	(add-to-list 'org-roam-capture-templates
				 '("b" "bibliography reference" plain
				   "\n
  - tags ::
  - keywords :: %^{keywords}

%?

* main
:PROPERTIES:
:Custom_ID: %^{citekey}
:DOI: %^{doi}
:URL: %^{url}
:AUTHOR: %^{author-or-editor}
:Year: %^{year} %^{date}
:NOTER_DOCUMENT: %^{file}
:NOTER_PAGE:
:END:"
				   :target (file+head "biblio/${citekey}.org"
									  "#+title: ${title}\n")
				   ;; :immediate-finish t
				   :unnarrowed t)))
  (use-package bibtex-completion
	;; :init (eval-after-load 'bibtex-completion (require 'org-roam-bibtex))
	:config
	(require 'org-roam-bibtex)	;; without roam notes now has only title
    (setq bibtex-completion-bibliography '("~/Sync/biblio/MY.bib"
                                           "~/Sync/biblio/mine.bib"
                                           "~/Sync/biblio/former.bib"
                                           "~/Sync/biblio/books.bib"
                                           "~/Sync/biblio/main.bib")
		  ;; bibtex-completion-notes-path "~/Sync/biblio/biblio.org"
		  bibtex-completion-notes-path "~/Sync/notes/org-roam/biblio"
		  bibtex-completion-library-path '("~/Sync/biblio/main/"
										   "~/Sync/biblio/MY/"
										   "~/Sync/biblio/mine/"
										   "~/Sync/biblio/former/"
										   "~/Sync/biblio/books/")
		  ;; ;; helm-bibtex-notes-template-one-file
		  ;; bibtex-completion-notes-template-one-file
		  ;; "** ${=key=}; ${title}\n \ :PROPERTIES:\n \  :Custom_ID: ${=key=}\n \  :INTERLEAVE_PDF: \
		  ;;    ./pdfs/${=key=}.pdf\n \ :END:\ncite:${=key=}\n"
		  bibtex-completion-display-formats
		  '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${=type=:3} ${journal:32}")
			(inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${=type=:3} Chapter ${chapter:32}")
			(incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${=type=:3} ${booktitle:32}")
			(inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${=type=:3} ${booktitle:32}")
			(t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${=type=:3} ${keywords:32}"))
		  bibtex-completion-find-additional-pdfs t ; find also additional pdfs
		  bibtex-completion-pdf-extension '(".pdf" ".avi" ".mp4" ".ppt" ".odp" ".odt" ".doc" ".docx" ".txt" ".rtf" ".djvu" ".mov" ".epub" ".zip" ".tif" ".jpg" ".xls" ".xlsx"))
	;; (setq bibtex-completion-format-citation-functions
	;;       '((org-mode      . bibtex-completion-format-citation-org-title-link-to-PDF)
	;;         (latex-mode    . bibtex-completion-format-citation-cite)
	;;         (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
	;;         (default       . bibtex-completion-format-citation-default)))
	(setq bibtex-completion-additional-search-fields '(tags keywords journal booktitle) ; search also in tags and keywords fields
          bibtex-completion-pdf-field "file" ; Zotero
          ))
  (use-package engine-mode
    :commands (engine/set-keymap-prefix
               engine/get-query
               engine/execute-search
			   engine-mode)
    :init
    (engine-mode t)
    (engine/set-keymap-prefix (kbd "<f14> / /"))
    :config
    (defengine amazon
      "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
      :keybinding "z"
      )
    (defengine libgen
      "https://libgen.gs/search.php?req=%s"
      :keybinding "L")
    (defengine libgen-scimag
      "https://libgen.gs/scimag/?s=%s"
      :keybinding "l")
    (defengine duckduckgo
      "https://duckduckgo.com/?q=%s"
      :browser 'eww-browse-url
      :keybinding "d")
    (defengine github
      "https://github.com/search?ref=simplesearch&q=%s"
      :keybinding "h")
    (defengine google
      "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
      :keybinding "g")
    (defengine google-images
      "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
      :keybinding "i")
    (defengine google-maps
      "http://maps.google.com/maps?q=%s"
      :keybinding "m"
      :docstring "Mappin' it up.")
    (defengine project-gutenberg
      "http://www.gutenberg.org/ebooks/search/?query=%s"
      :keybinding "u")
    (defengine qwant
      "https://www.qwant.com/?q=%s"
      :keybinding "q")
    (defengine rfcs
      "http://pretty-rfc.herokuapp.com/search?q=%s"
      :keybinding "r")
    (defengine stack-overflow
      "https://stackoverflow.com/search?q=%s"
      :keybinding "o")
    (defengine twitter
      "https://twitter.com/search?q=%s"
      :keybinding "t")
    (defengine wikipedia
      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
      :keybinding "w"
      :docstring "Searchin' the wikis.")
    (defengine wiktionary
      "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s"
      :keybinding "k")
    (defengine wolfram-alpha
      "http://www.wolframalpha.com/input/?i=%s"
      :keybinding "a")
    (defengine Cambridge-dict-pronunciation
      "https://dictionary.cambridge.org/us/pronunciation/english/%s"
      :keybinding "p")
    (defengine youtube
      "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
      :keybinding "y"))
  (use-package citeproc)
  (use-package org-ref
	;; XXX: 2 open supplementary like arhel works from =a b= and not from cite:
	:bind (("<f14> <menu> la" . arxiv-add-bibtex-entry) ; lookup utilities
           ("<f14> <menu> lA" . arxiv-get-pdf-add-bibtex-entry)
           ("<f14> <menu> ld" . doi-utils-add-bibtex-entry-from-doi)
           ("<f14> <menu> li" . isbn-to-bibtex)
           ("<f14> <menu> lp" . pubmed-insert-bibtex-from-pmid)
           ("<f14> <menu> c" . org-ref-insert-link)
           :map bibtex-mode-map               ; modalka-mode-map
           ("<f14> <menu> h" . org-ref-bibtex-hydra/body)
           ("<f14> <menu> b" . org-ref-open-in-browser)
           ("<f14> <menu> n" . org-ref-open-bibtex-notes)
           ("<f14> <menu> p" . org-ref-open-bibtex-pdf)
           ("<f14> <menu> h" . org-ref-bibtex-hydra/body) ; misc
           ("<f14> <menu> i" . org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit)
           ("<f14> <menu> S" . org-ref-sort-bibtex-entry)
           ("C-c l" . org-ref-store-bibtex-entry-link)
		   ("<f14> b" . org-ref-bibtex-hydra/body)
           ;; :map latex-mode-map
		   ;; ("<f14> i i" . org-ref-insert-link)
		   :map org-mode-map
		   ;; ("<f14> i i" . org-ref-insert-link)
		   ("C-c ]" . org-ref-insert-link)
		   ("H-]" . org-ref-insert-link-hydra/body)
		   ;; :map markdown-mode-map
		   ;; ("<f14> i i" . org-ref-insert-link)
		   )
	:hook
	(org-mode-hook . (lambda () (require 'org-ref)))
	(bibtex-mode-hook . (lambda () (require 'org-ref)))
	(tex-mode-hook . (lambda () (require 'org-ref)))
	(markdown-mode-hook . (lambda () (require 'org-ref)))
	:config
	(require 'org-ref-ivy)
	;; v3.0 (setq org-ref-completion-library 'org-ref-ivy-cite
    ;;       org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex ;to open using file= attribute
    ;; v3.0 org-ref-default-bibliography '(;;"~/Sync/biblio/biblio.bib"
	;; 								 "~/Sync/biblio/main.bib"
	;; 								 "~/Sync/biblio/books.bib"
	;; 								 "~/Sync/biblio/mine.bib"
	;; 								 "~/Sync/biblio/former.bib"
	;; 								 "~/Sync/biblio/MY.bib")
    ;; trailing / affects ,hA associate Pdf to entry
    ;; org-ref-pdf-directory "~/Sync/biblio/pdfs/"
    ;; org-ref-bibliography-notes "~/Sync/biblio/biblio.org"
    ;; v3.0 reftex-default-bibliography '("~/Sync/biblio/biblio.bib")
    ;; Notes template, compatible with interleave
    ;; org-ref-notes-directory "~/Sync/biblio/Notes/"
    ;; org-ref-notes-function 'orb-edit-notes
    ;; org-ref-note-title-format
    ;; "** %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
    ;; "** %k; %t\n \ :PROPERTIES:\n \  :Custom_ID: %k\n \  :INTERLEAVE_PDF: \
    ;;    ./pdfs/%k.pdf\n \ :END:\n"
    ;; )
	;; v3.0 (org-ref-ivy-cite-completion)
	)
  ;; https://github.com/jkitchin/org-ref/blob/master/org-ref.org
  (use-package org-ref-ivy
	:straight nil
	:after org-ref
	:init (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
				org-ref-insert-cite-function 'org-ref-cite-insert-ivy
				org-ref-insert-label-function 'org-ref-insert-label-link
				org-ref-insert-ref-function 'org-ref-insert-ref-link
				org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))
  
  (use-package org-noter
	:commands  (org-noter-insert-note
				org-noter--valid-session
				org-noter--parse-root
				org-noter--get-precise-info
				org-noter--doc-approx-location
				org-noter--pretty-print-location)
	:bind (("<f14> n n" . org-noter)
           :map org-noter-notes-mode-map
           ("H-k" . org-noter-create-skeleton)
           ("H-n" . org-noter-sync-next-note)
           ("H-p" . org-noter-sync-prev-note)
           :map org-noter-doc-mode-map
           ("H-k" . org-noter-create-skeleton)
           ("H-n" . org-noter-sync-next-note)
           ("H-p" . org-noter-sync-prev-note)
           )
	:config
	(setq ;; org-noter-default-notes-file-names '("noter-othernotes.org" "biblio.org")
	 org-noter-hide-other nil
     org-noter-separate-notes-from-heading t
     ;; org-noter-notes-search-path '("~/Sync/biblio" "~/Sync/notes/org-roam/biblio")
     org-noter-notes-search-path '("~/Sync/notes/org-roam/biblio")
     org-noter-always-create-frame nil  ;; do not create a new frame
     org-noter-doc-split-fraction '(0.67 . 0.75)
     org-noter-notes-window-location 'other-frame)
	;; (require 'org-noter-pdftools) ; XXX: XX
	)
  (use-package pdf-tools
	:functions pdf-loader-install
    :bind (:map pdf-view-mode-map
                ("t" . org-ref-pdf-to-bibtex)
                ("C-s" . isearch-forward)
                ("/" . pdf-occur)
                ("C-?" . pdf-isearch-occur))
    :hook
    (pdf-view-mode-hook . (lambda () (require 'org-ref)))
    ;; (pdf-view-mode-hook . pdf-view-set-slice-from-bounding-box)
    :init
	(pdf-loader-install)
	;; (pdf-tools-install)
    :config
    (setq pdf-view-resize-factor 1.1    ; more fine-grained zooming
          ;; pdf-misc-print-program "/usr/bin/gtklp"
		  )
	(use-package pdf-misc
	  :straight pdf-tools
	  :config
	  (setq pdf-misc-print-program "/usr/bin/gtklp"))
	(use-package pdf-annot
	  :straight pdf-tools
      :bind (:map pdf-view-mode-map
                  ("h" . pdf-annot-add-highlight-markup-annotation))
      :config
      ;; automatically annotate highlights
      (setq pdf-annot-activate-created-annotations nil)))
  (use-package org-pdftools
    :hook (org-mode-hook . org-pdftools-setup-link))
  (use-package org-noter-pdftools
    :after org-noter
    :commands (org-noter-pdftools-jump-to-note)
    :hook
    (org-noter-doc-mode-hook . (lambda () (require 'org-noter-pdftools)))
    (org-noter-notes-mode-hook . (lambda () (require 'org-noter-pdftools)))
    :bind (:map org-noter-notes-mode-map
                ("C-H-k" . org-noter-pdftools-create-skeleton)
                :map org-noter-doc-mode-map
                ("C-H-k" . org-noter-pdftools-create-skeleton))
    :config
	;; Add a function to ensure precise note is inserted
	(defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
      (interactive "P")
      (org-noter--with-valid-session
       (let ((org-noter-insert-note-no-questions (if toggle-no-questions
													 (not org-noter-insert-note-no-questions)
                                                   org-noter-insert-note-no-questions))
			 (org-pdftools-use-isearch-link t)
			 (org-pdftools-use-freestyle-annot t))
		 (org-noter-insert-note (org-noter--get-precise-info)))))
	;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
	(defun org-noter-set-start-location (&optional arg)
      "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
      (interactive "P")
      (org-noter--with-valid-session
       (let ((inhibit-read-only t)
			 (ast (org-noter--parse-root))
			 (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
		 (with-current-buffer (org-noter--session-notes-buffer session)
           (org-with-wide-buffer
			(goto-char (org-element-property :begin ast))
			(if arg
				(org-entry-delete nil org-noter-property-note-location)
              (org-entry-put nil org-noter-property-note-location
							 (org-noter--pretty-print-location location))))))))
    (with-eval-after-load 'pdf-annot
      (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
  (use-package gscholar-bibtex
    :config
    (setq gscholar-bibtex-default-source "Google Scholar")
    (setq gscholar-bibtex-database-file "/home/dan/Sync/biblio/biblio.bib")
    )
  ;; https://ianjones.us/own-your-second-brain#org59f47bf2 XXX:
  ;; (use-package org-journal
  ;;   :bind
  ;;   ("C-c n j" . org-journal-new-entry)
  ;;   :custom
  ;;   (org-journal-dir "~/Desktop/03-resources/org-roam/")
  ;;   (org-journal-date-prefix "#+TITLE: ")
  ;;   (org-journal-file-format "%Y-%m-%d.org")
  ;;   (org-journal-date-format "%A, %d %B %Y"))
  ;; (setq org-journal-enable-agenda-integration t)
  (use-package org-protocol
	;; remember  xdg-mime default org-protocol.desktop
	;; x-scheme-handler/org-protocol
	:straight nil
	)
  (use-package org-roam
	:after org
	:defines org-roam-v2-ack
	;; :hook
	;; maybe restore into config to speed up stand alone emacs
	;; (after-init-hook . org-roam-db-autosync-enable)
	:commands (org-roam-db-autosync-enable)
	:init
	(setq org-roam-v2-ack t)
	:custom
	(org-roam-directory "~/Sync/notes/org-roam/")
	:bind (("<f14> n l" . org-roam-buffer-toggle)
		   ("<f14> n f" . org-roam-node-find)
		   ("<f14> n r" . org-roam-node-random)
		   ;; "SPC n r" 'org-roam-ref-add
		   ("<f14> n j" . org-roam-dailies-goto-today)
		   ("<f14> n J" . org-roam-dailies-goto-date)
		   :map org-roam-mode-map
		   ("<f14> n c" . org-roam-capture)
		   :map org-mode-map
		   ("<f14> n a" . org-roam-alias-add)
		   ("<f14> n g" . org-roam-graph)
		   ("<f14> n t" . org-roam-tag-add)
		   ("<f14> n o" . org-id-get-create)
		   ("<f14> n i" . org-roam-node-insert))
	;; :init
	;; (eval-after-load 'org-roam '(require 'org-roam-protocol)) ; XXX: XX
	:config
	;; (org-roam-setup) is now alias for
	(org-roam-db-autosync-enable)
	(add-to-list 'display-buffer-alist
				 '(("*org-roam*"
					(display-buffer-in-direction)
					(direction . right)
					(window-width . 0.33)
					(window-height . fit-window-to-buffer))))
	(use-package org-roam-graph
	  :straight org-roam
	  :config
	  (setq org-roam-graph-extra-config '(("concentrate" . "true")
										  ("overlap" . "prism100")
										  ("overlap_scaling" . "-8")
										  ;; ("pack" . "true")
										  ("sep" . "20.0")
										  ("esep" . "0.0")
										  ("splines" . "polyline")
										  ))
	  (setq org-roam-graph-node-extra-config
			'(("id"
			   ("shape" . "rectangle")
			   ("style" . "bold,rounded,filled")
			   ("fillcolor" . "#EEEEEE")
			   ("color" . "#C9C9C9")
			   ("fontcolor" . "#111111"))
			  ("http"
			   ("style" . "rounded,filled")
			   ("fillcolor" . "#EEEEEE")
			   ("color" . "#C9C9C9")
			   ("fontcolor" . "#0A97A6"))
			  ("https"
			   ("shape" . "rounded,filled")
			   ("fillcolor" . "#EEEEEE")
			   ("color" . "#C9C9C9")
			   ("fontcolor" . "#0A97A6"))))
	  (setq org-roam-graph-edge-extra-config nil))
	;; (setq org-roam-dailies-capture-templates
	;; 	  '(("l" "lab" entry
	;; 		 #'org-roam-capture--get-point
	;; 		 "* %?"
	;; 		 :file-name "~/Sync/box/org/daily/%<%Y-%m-%d>"
	;; 		 :head "#+title: %<%Y-%m-%d>\n"
	;; 		 :olp ("Lab notes"))
	;; 		("j" "journal" entry
	;; 		 #'org-roam-capture--get-point
	;; 		 "* %?"
	;; 		 :file-name "~/Sync/box/org/daily/%<%Y-%m-%d>"
	;; 		 :head "#+title: %<%Y-%m-%d>\n"
	;; 		 :olp ("Journal"))))
	(use-package org-roam-protocol
	  :straight org-roam
	  :demand t
	  :config
	  (setq org-roam-capture-ref-templates
			'(("r" "ref" plain "%?"
			   :target (file+head "websites/${slug}.org" "#+title: ${title}")
			   :unnarrowed t))))
	(setq org-roam-completion-everywhere t))

  )
(progn                                  ; Magit
  (use-package magit
    :bind
    ("C-x g" . magit-status)
    ("<f14> g s" . magit-status)
    ("<f14> g f" . magit-find-file-other-window)
    ("<f14> g x" . magit-checkout)
    ("<f14> g e" . magit-ediff-resolve)
    ("<f14> g C" . magit-clone)          ; g c is for counsel commands
    ("<f14> g i" . magit-init)
    (:map git-commit-mode-map
          ("M-n" . mk-transpose-line-down)
          ("M-p" . mk-transpose-line-up))
    :init
    (setq magit-repository-directories '(("/home/dan/workspace" . 4)
										 ("/home/dati" . 2)
                                         ("~/Sync" . 9)))
    :config
    (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))
  (use-package magit-todos
    :after magit
    :bind ("<f14> g 2" . magit-todos-list))
  (use-package magit-annex)
  ;; (use-package gitignore-mode)
  ;; (use-package gitconfig-mode)
  ;; (use-package gitattributes-mode)
  (use-package git-modes)
  (use-package gitignore-templates)
  (use-package browse-at-remote
    :bind ("<f14> g g" . bar-browse))
  (use-package git-messenger
    :bind ("<f14> g m" . git-messenger:popup-message))
  (use-package git-timemachine
    :bind
    ("<f14> g t" . git-timemachine)
    ("<f14> g T" . git-timemachine-toggle))
  (use-package diff-hl
	:commands global-diff-hl-mode
	:bind
	(("<f14> g n" . diff-hl-next-hunk)	; consider removing
	 ("<f14> g p" . diff-hl-previous-hunk))
	:hook
	((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
	 (magit-post-refresh-hook . diff-hl-magit-post-refresh))
	:init
	;; (setq diff-hl-draw-borders nil)
	;; (setq diff-hl-global-modes '(not org-mode))
	;; (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
	;; (setq diff-hl-global-modes (not '(image-mode org-mode)))
	:config
	(global-diff-hl-mode))
  )
(progn                                  ; Projectile
  (use-package projectile
    :diminish
    :commands (projectile-mode)
	:init
    (which-key-add-key-based-replacements "<f14> p" "Projectile")
    (projectile-mode)
    :bind
    ("<f14> p" . projectile-command-map)
    ("C-x p" . projectile-command-map)
    :config
    (setq projectile-project-search-path '("~/workspace"
										   "/home/dati"
                                           ;; "~/workspace/platereaders/"
                                           ;; "~/workspace/arte";; slowed down 0.5s
                                           "~/Sync"))
    ;; "/home/examples"))
    (setq projectile-completion-system 'ivy))
  (use-package counsel-projectile
    :after (projectile)
	:commands (counsel-projectile-mode)
    :init (counsel-projectile-mode))
  (use-package org-projectile
    :after (projectile)
    :bind
    ("<f14> p n" . org-projectile-project-todo-completing-read)
    :config
    (setq org-projectile-projects-file "~/Sync/box/org/projects.org")
    ;; org-agenda-files (append org-agenda-files (org-projectile-todo-files))
    (setq org-projectile-capture-template
          (format "%s" "* TODO %?\n%[~/Sync/.emacs/templates/da-property-string]\n"))
    (declare-function org-projectile-project-todo-entry "org-projectile")
    ;; (push (org-projectile-project-todo-entry) org-capture-templates)
    (add-to-list 'org-capture-templates
                 (org-projectile-project-todo-entry
                  :capture-character "p"
                  :capture-heading "Projectile TODO"))
    (setq org-link-elisp-confirm-function nil)))
;; ;; XXX: possible updates
;; ;; consult to rm swiper, fzf; beawre counsel-recoll
;; ;; inspire at https://github.com/lccambiaghi/vanilla-emacs
;; (use-package crux)
;; (use-package ivy-prescient)
;; (use-package selectrum)
;; (use-package selectrum-prescient)
;; vertico
;; https://www.omarpolo.com/dots/emacs.html
;; (use-package orderless)
;; (use-package embark)
;; (use-package marginalia)
(use-package consult)
(use-package consult-recoll)
;; (use-package citar
;;   :bind (("C-c b" . citar-insert-citation)
;;          :map minibuffer-local-map
;;          ("M-b" . citar-insert-preset))
;;   :custom
;;   (citar-bibliography '("~/Sync/biblio/main.bib")))


;; XXX: mrkkrp ...  wdired
;; https://github.com/mrkkrp/dot-emacs

(use-package dired
  :straight nil
  ;; :load-path "/usr/share/emacs/"
  :functions (dired-get-filename)
  :init
  (setq
   delete-by-moving-to-trash t
   dired-auto-revert-buffer t
   dired-dwim-target t
   dired-keep-marker-copy nil
   dired-listing-switches "-GAlh --group-directories-first"
   dired-recursive-copies 'always
   dired-recursive-deletes 'always)
  :config
  (defun mk-dired-open-external (file)
    "Open specified FILE with application determined by the OS."
    (interactive (list (dired-get-filename)))
    (call-process "xdg-open" nil 0 nil file))
  :bind
  (:map
   dired-mode-map
   ("b" . dired-up-directory)
   ("e" . mk-dired-open-external)
   ("w" . wdired-change-to-wdired-mode))
  :hook
  (dired-mode-hook . toggle-truncate-lines)
  (dired-mode-hook . turn-on-gnus-dired-mode))
(use-package dired-x
  :straight nil
  ;; :load-path "/usr/share/emacs/"
  :init
  (setq
   dired-clean-up-buffers-too t))
(use-package wdired
  :after (dired)
  :init
  (setq wdired-allow-to-change-permissions t))

(use-package exec-path-from-shell       ;demanded when daemonp
  :commands (exec-path-from-shell-initialize)
  :config (exec-path-from-shell-initialize))
(progn                                  ; python
  (defhydra hydra-for-py (:color blue :hint nil :exit nil)
    "
   ^Send^         ^Tests^       ^Navigate^         ^Virtualenv^       ^Pyenv^            ^Format^
  ^^^^^^^^----------------------------------------------------------------------------------------------
  ^ ^             _t_: tests    _d_: definition    _v_: activate      _e_: activate      _b_: black
  ^ ^             ^ ^           _D_: go back       _V_: deactivate    _E_: deactivate    _c_: create-doc
  ^ ^             _q_: quit     _k_: doc           _w_: workon        _p_: poetry        _n_: numpydoc
  "
    ;; ("s" run-python :color red)
    ;; ("r" python-shell-send-region :color red)
    ;; ("f" python-shell-send-defun :color red)
    ("t" python-pytest-dispatch)
    ("d" jedi:goto-definition :color red)
    ("D" jedi:goto-definition-pop-marker :color red)
    ("k" jedi:show-doc :color red)
    ("v" pyvenv-activate)
    ("V" pyvenv-deactivate)
    ("w" pyvenv-workon)
    ("e" pyenv-mode-set :color red)
    ("E" pyenv-mode-unset)
    ("p" poetry)
    ("b" blacken-buffer)
    ("c" sphinx-doc)
    ("n" numpydoc-generate)
    ("q" nil :color blue))
  (use-package python
    :load-path "/usr/share/emacs/"
    :bind
    ("<f14> t p" . python-mode)
    :bind (:map python-mode-map
                ("C-c C-S-p" . jupyter-run-repl)
				("<backtab>" . hs-hide-all)         ;FIXME: <backtab> did not work here.
				)
    (:map python-mode-map
          ("H-<tab>" . hydra-for-py/body))
    :config
    (setq-default python-fill-docstring-style 'pep-257-nn
                  python-indent 4)
    (setq python-shell-interpreter "jupyter"
          python-shell-interpreter-args "console --simple-prompt"
          python-shell-prompt-detect-failure-warning nil)
    (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))
  (use-package company-jedi
    :commands (jedi:goto-definition-pop-marker)
    :bind (:map python-mode-map
                ("C-c H-d" . jedi:goto-definition)
                ("C-c H-k" . jedi:show-doc))
    :hook (python-mode-hook . (lambda () (add-to-list 'company-backends 'company-jedi))))
  (use-package python-pytest
    :after python
    :bind (:map python-mode-map
                ("C-c H-t" . python-pytest-dispatch)))
  (use-package numpydoc
    :after python
	:commands (numpydoc-generate)
    :config
    (setq numpydoc-insertion-style 'yas)) ;'prompt|nil
  (use-package python-docstring
    :config (setq python-docstring-sentence-end-double-space nil)
    :hook (python-mode-hook . python-docstring-mode))
  (use-package sphinx-doc
    :commands (sphinx-doc)
    :hook (python-mode-hook . sphinx-doc-mode))
  (use-package blacken
    :bind (:map python-mode-map
                ("C-c =" . blacken-buffer))
    :hook (python-mode-hook . blacken-mode))
  (use-package eval-in-repl
    :hook (python-mode-hook . (lambda () (require 'eval-in-repl-python) ))
    ;; :config
    ;; (setq eir-jump-after-eval nil)
    :bind (:map python-mode-map
                ("<C-return>" . eir-eval-in-python)))
  (use-package pip-requirements)
  (use-package bind-key
    :functions bind-key--remove)
  (use-package pyenv-mode
    :after python
	:commands (pyenv-mode
			   pyenv-mode-set
			   pyenv-mode-unset)
    :init
    (add-to-list 'exec-path "~/.pyenv/shims")
    (setenv "WORKON_HOME" "~/.pyenv/versions/")
    :config
    (unbind-key "C-c C-s" pyenv-mode-map)
    (unbind-key "C-c C-u" pyenv-mode-map)
    (pyenv-mode)
    ;; :hook
    ;; (python-mode-hook . pyenv-mode)
    )
  (use-package yaml-mode				; yay -S yamllint
	:mode "\\.yml\\'"
	:hook ((yaml-mode . turn-off-flyspell))
	)
  (use-package toml-mode
	:mode "\\.toml\\'")
  (use-package pyvenv
	:commands (pyvenv-activate
               pyvenv-workon)
	:bind (:map python-mode-map
				("C-c H-a" . pyvenv-activate)
				("C-c H-A" . pyvenv-deactivate)
				("C-c H-w" . pyvenv-workon)))
  (use-package poetry
	:after python
	:commands (poetry))
  (use-package py-isort                 ;yay -S python-isort
	:after python
	;; :hook (before-save-hook . py-isort-before-save)
	:bind (:map python-mode-map
				("C-c s" . py-isort-buffer)
				("C-c S" . py-isort-region)))
  )
(use-package csv-mode
  :mode (("\\.csv\\'" . csv-mode)))
(use-package zeal-at-point
  ;; :defer t
  :bind
  ("<f14> z z" . zeal-at-point)
  ("<f14> z Z" . zeal-at-point-set-docset)
  :init
  (which-key-add-key-based-replacements "<f14> z" "Zeal")
  :config
  (add-to-list 'zeal-at-point-mode-alist
               '(python-mode . ("python3"
                                "pandas"
                                "numpy"
                                "matplotlib"
                                "scipy"))))
(use-package ess)
(use-package emojify)
(use-package slack
  :defer t                              ; avoid halting daemon startup
                                        ; asking for passwords
  :commands (slack-channel-select
             slack-im-select
             slack-group-select
             slack-select-rooms
             slack-select-unread-rooms
             slack-all-threads
             slack-thread-show-or-create
             slack-message-add-reaction
             slack-message-remove-reaction
             slack-message-show-reaction-users
             slack-message-pins-add
             slack-message-pins-remove
             slack-message-delete
             slack-room-pins-list
             slack-message-edit
             slack-message-write-another-buffer
             slack-clipboard-image-upload
             slack-ws-close
			 slack-register-team
			 slack-start)
  :hook (slack-mode-hook . emojify-mode)
  :preface
  (defhydra hydra-slack (:color pink :hint nil :exit t)
    "
    ^Select^             ^Reaction^         ^Pins^          ^Message
    ^^^^^^^^-----------------------------------------------------------------
    _c_: channel         _ra_: add          _pa_: add       _md_: delete
    _d_: direct IM       _rr_: remove       _pr_: remove    _me_: edit
    _g_: group           _rs_: show users   _pl_: list      _mo_: other buffer
    _R_: room            ^ ^
    _u_: unread rooms    _a_: all threads   _t_: thread or create
    _i_: image from clipboard
    "
    ("c" slack-channel-select)
    ("d" slack-im-select)
    ("g" slack-group-select)
    ("R" slack-select-rooms)
    ("u" slack-select-unread-rooms)
    ("a" slack-all-threads :exit nil)
    ("t" slack-thread-show-or-create)
    ("ra" slack-message-add-reaction)
    ("rr" slack-message-remove-reaction)
    ("rs" slack-message-show-reaction-users)
    ("pa" slack-message-pins-add)
    ("pr" slack-message-pins-remove)
    ("pl" slack-room-pins-list)
    ("md" slack-message-delete :exit nil)
    ("me" slack-message-edit)
    ("mo" slack-message-write-another-buffer)
    ("i" slack-clipboard-image-upload)
    ("x" slack-ws-close "Close Slack")
    ("q" nil "cancel" :color blue))
  :bind (("<f14> a s" . (lambda () (interactive "") (slack-start) (hydra-slack/body)))
         ;; ("<f14> a s s" . slack-start)
         :map slack-mode-map
         ("\C-n" . slack-buffer-goto-next-message)
         ("\C-p" . slack-buffer-goto-prev-message)
         ("H-<tab>" . hydra-slack/body)
         ("@" . slack-message-embed-mention)
         ("#" . slack-message-embed-channel))
  ;; :chords (("hh" . hydra-slack/body))
  :init
  (which-key-add-key-based-replacements
    "<f14> a s" "Slack")
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (auth-source-pass-enable)
  (slack-register-team
   :name "molecularvirologylab"
   :default t
   :token (auth-source-pick-first-password
           :host "molecularvirologylab.slack.com"
           :user "daniele.arosio@cnr.it")
   :subscribed-channels '(cftr papers labmeeting)
   :full-and-display-names t)
  (use-package alert
    :commands (alert)
    :init
    (setq alert-default-style 'libnotify))
  )
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  ;; :custom-face (variable-pitch ((t (:family "URW Bookman" :height 1.1))))
  :config
  (setq nov-text-width t)
  :hook (nov-mode-hook . visual-fill-column-mode)
  )
(use-package keyfreq
  :commands (keyfreq-mode
			 keyfreq-autosave-mode)
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
(use-package pocket-reader)
(use-package calibredb
  :commands calibredb
  ;; ripgrep-all (rga)
  :config
  (setq calibredb-root-dir "~/Sync/media/books/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist
		'(("~/Sync/media/books/")))
  (setq calibredb-size-show nil)
  (setq calibredb-comment-width 0)
  ;; (setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
  (setq calibredb-ref-default-bibliography "~/Sync/media/ebooks.bib")
  (add-to-list 'bibtex-completion-bibliography calibredb-ref-default-bibliography)
  )
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(flycheck-fringe-error ((t (:background "#6C3333" :weight bold))))
;;  '(font-lock-function-name-face ((t (:inherit fixed-pitch))))
;;  '(header-line ((t (:background "#fbf8ef"))))
;;  '(hl-line ((t (:extend t :height 140 :background "dark violet"))))
;;  '(markup-meta-face ((t (:height 140 :family "Inconsolata"))))
;;  '(org-block ((t (:inherit fixed-pitch :foreground "#655370" :background "gainsboro"))))
;;  '(org-block-begin-line ((t (:inherit fixed-pitch :background "#ddd8eb" :foreground "#9380b2"))))
;;  '(org-block-end-line ((t (:inherit fixed-pitch :background "#ddd8eb" :foreground "#9380b2"))))
;;  '(org-code ((t (:inherit (shadow fixed-pitch)))))
;;  '(org-document-info ((t)))
;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;;  '(org-done ((t (:strike-through t))))
;;  '(org-headline-done ((t (:inherit default :weight bold :strike-through t))))
;;  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
;;  '(org-link ((t (:underline t))))
;;  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;  '(org-property-value ((t (:inherit fixed-pitch))) t)
;;  '(org-scheduled-previously ((t (:weight bold :underline nil))))
;;  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;  '(org-table ((t (:inherit fixed-pitch))))
;;  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.95))))
;;  '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
;;  '(slack-all-thread-buffer-thread-header-face ((t (:weight bold :height 1.8)))))
;; Telega
(use-package pass
  ;; :custom ((pass-show-keybindings nil))
  :bind (("C-x P" . pass))
  )
(use-package tzc
  :demand t
  :config
  (setq tzc-favourite-time-zones '("Europe/Rome")))

(setq debug-on-error nil)
(setq debug-on-quit nil)
(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))
;; ;; Garbage collector - decrease threshold to 5 MB
;; (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
;; (setq gc-cons-threshold (* 32 1024 1024)
;;       gc-cons-percentage 0.1
;;       garbage-collection-messages nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(git-modes ox-rst org-compat org-roam-graph company-box dream-theme numpydoc yaml-mode pdf-annot org-autolist ox-twbs ox-twtb org-archive counsel-web mu4e-actions mu4e-view mu4e-headers mu4e-utils transpose-frame exwm-config ob-jupyter auctex cdlatex ox sdcv zeal-at-point yasnippet-snippets yapfify writegood-mode wordnut which-key wc-mode visual-fill-column use-package-ensure-system-package synosaurus smex smartparens slack ranger pyvenv python-docstring pytest powerthesaurus plantuml-mode pip-requirements paradox ox-reveal org-super-agenda org-ref org-projectile org-plus-contrib org-noter org-gcal org-bullets olivetti ob-async mu4e-maildirs-extension mu4e-jump-to-list moe-theme markdown-mode magit-todos magit-annex langtool jupyter ivy-yasnippet ivy-rich ivy-bibtex imenu-list highlight-indent-guides guess-language gscholar-bibtex graphviz-dot-mode google-translate goldendict gnuplot gitignore-templates gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter general flyspell-correct-ivy flycheck fantom-theme expand-region evil-numbers evil-nerd-commenter evil ess doom-themes diminish dictionary dictcc deft counsel-projectile company-statistics company-quickhelp company-ngram company-jedi cm-mode calfw-org calfw browse-at-remote base16-theme avy artbollocks-mode academic-phrases))
 '(safe-local-variable-values
   '((org-ref-pdf-directory . "~/Sync/biblio/books/")
	 (org-ref-pdf-directory . "~/Sync/biblio/MY/")
	 (bibtex-completion-notes-path . "~/Sync/biblio/MY/MY.org")
	 (org-ref-bibliography-notes . "~/Sync/biblio/MY/MY.org")
	 (org-download-image-dir . "./WORK/")))
 '(send-mail-function 'sendmail-send-it))

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
