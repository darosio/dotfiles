;;; package --- Summary my configuration init.el
;;; Commentary:
;; https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; https://blog.jft.rocks/emacs/emacs-from-scratch.html
;; https://gist.github.com/huytd/6b785bdaeb595401d69adc7797e5c22c for lsp-mode
;;;  new important ref https://sriramkswamy.github.io/dotemacs/#orgheadline278
;;; Code:
(setq debug-on-error t)
(setq debug-on-quit t)
;; to silent a defadvice warning in pcre2el (required by magit-todos)
(setq ad-redefinition-action 'accept)

(setq gc-cons-threshold (* 500 1024 1024))
(defconst emacs-start-time (current-time))
(progn					; Base UI
  ;; Increase the garbage collection threshold to 500 MB to ease startup
  (scroll-bar-mode 0)
  (tool-bar-mode   0)
  (tooltip-mode    0)
  (menu-bar-mode   0)
  (show-paren-mode)		      ; highlight parenthesis
  (save-place-mode)		      ; remember last position in file
  (prefer-coding-system 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8-auto-unix)
  ;; (setq-default truncate-lines t) 	; truncating lines
  (setq inhibit-startup-screen t ; inhibit useless and old-school startup screen
        ring-bell-function 'ignore ; silent bell when you make a mistake
        coding-system-for-read 'utf-8  ; use utf-8 by default to read
        coding-system-for-write 'utf-8 ; use utf-8 by default to write
        sentence-end-double-space nil ; sentence SHOULD end with only a point.
        ;; default-fill-column 80		; toggle wrapping text at the 80th character
	column-number-mode t
        ;; tab-width 4			; tab are 4 spaces large
        ;; show-paren-delay 0		; Show matching parenthesis
	version-control t	      ; use versioned backups
	vc-follow-symlinks t	      ; follow symlinks without asking
	large-file-warning-threshold (* 15 1024 1024) ; large file warning
        ;; auto-save-default nil		; CHECKME stop creating #autosave# files
	auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	create-lockfiles nil
        ;; make-backup-files nil		; stop creating backup~ files
	backup-directory-alist '(("." . "~/.emacs.d/backup"))
	backup-by-copying t             ; don't clobber symlinks
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	)
  (setq x-stretch-cursor t)
  (blink-cursor-mode -1)		; Don't blink the cursor
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-hl-line-mode)
  ;; TODO: custom tramp gdb
  (put 'narrow-to-region 'disabled nil)	; narrow to region =C-x n n=
  )
(progn					; Hooks
  ;; rend les scripts executable par défault si c'est un script.
  (add-hook 'after-save-hook
            (lambda () (executable-make-buffer-file-executable-if-script-p)))
  ;; supprime les caractères en trop en sauvegardant.
  ;; not compatible with deft
  ;; (add-hook 'before-save-hook
  ;;           (lambda () (delete-trailing-whitespace)))
  ;; enable folding
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  )
(progn					; Package configuration
  ;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (require 'package)
  (setq package-enable-at-startup nil)	; tells emacs not to load any packages before starting up
  (setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
			   ("org"   . "http://orgmode.org/elpa/")
                           ("gnu"   . "http://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("elpy" . "https://jorgenschaefer.github.io/packages/")))
	;; package-archive-priorities '(("melpa-stable" . 10)
	;; 			     ("org"     . 5)
	;; 			     ("gnu"     . 5)
	;; 			     ("melpa"        . 5)))
  (package-initialize)
  ;; Bootstrap `use-package`
  (unless (package-installed-p 'use-package)
    (package-refresh-contents); updage packages archive
    (package-install 'use-package)); and install the most recent version of use-package
  ;; This is only needed once, near the top of the file
  (eval-when-compile
    (require 'use-package))
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)
  ;; (defvar is-defer (if (getenv "EMACS_DEFER") t))
  ;; mar2020 1.7 vs 6.7 s defer all vs none
  (if (daemonp)
      (setq use-package-always-demand t)
    (setq use-package-always-defer t))
  (use-package use-package-ensure-system-package)
  (use-package diminish)
  (use-package async)
  (use-package paradox	       ; general is below, def =SPC a k= there
    :config
    (setq paradox-github-token "abc5e1c6710cee61646f0952091bae7b825852f3"
	  paradox-automatically-star t
	  paradox-execute-asynchronously t))
  (use-package undo-tree
    :diminish " ⽊"			; ttf-code2000 ttf-symbola
    :config
    (setq undo-tree-enable-undo-in-region nil)
    (global-undo-tree-mode -1)))
(progn					; UI more setting
  (use-package bookmark			; persistent bookmarks
    :init (setq bookmark-save-flag t
		bookmark-default-file "~/Sync/.emacs/bookmarks"))
  (use-package ediff			; Fix diff behavior
    :load-path "/usr/share/emacs/"
    :commands ediff-setup-windows-plain
    :init (setq ediff-window-setup-function #'ediff-setup-windows-plain
		ediff-split-window-function #'split-window-right
		ediff-diff-options "-w"))
  (progn				; printing
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
  )
(progn					; Themes, Fonts and mode-line
  (use-package fantom-theme)
  (use-package doom-themes)
  (use-package moe-theme)
  (use-package base16-theme
    ;; 'base16-solarized-light 'base16-tomorrow 'base16-default-dark 'base16-gruvbox-light-medium
    ;; 'base16-cupertino 'base16-woodland base16-unikitty-dark 'base16-materia
    :config
    (load-theme 'base16-gruvbox-dark-hard t))
  (set-face-attribute 'default nil
		      :family "IBM Plex mono"
		      :height 107
		      :weight 'normal
		      :width 'normal)
  (use-package face-remap
    :config
    (setq text-scale-mode-step 1.05))
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
(progn					; Keybindings and Evil
  (use-package general
    :demand t
    :config
    (general-def				; global
      "M-/"    #'hippie-expand
      "M-1"    #'delete-other-windows
      "H-;"    #'comment-box
      "H-<f1>" #'which-key-show-top-level
      ;; org-mode bindings
      "C-c l"  #'org-store-link
      "C-c a"  #'org-agenda
      "C-c c"  #'org-capture
      "C-c q"  #'auto-fill-mode)
    ;; (general-auto-unbind-keys t) ; disable to check for overlapping keys
    (general-evil-setup t))
  (general-create-definer lead0-def
    :states '(normal insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer lead1-def
    ;; :keymaps '(normal emacs) XXX: documentation was wrong
    :states '(normal emacs)
    :prefix ","
    :non-normal-prefix "M-,")
  (lead0-def
    ;; simple command
    "<tab>"   #'(switch-to-prev-buffer :wk "previous buffer")
    "C-<tab>" #'(switch-to-next-buffer :wk "next buffer")
    "q"   #'(save-buffers-kill-emacs :wk "Quit Emacs")
    ;; bind to double key press
    "w"  #'(:ignore t :wk "Windows")
    "wl"  #'(windmove-right :wk "move right")
    "wh"  #'(windmove-left :wk "move left")
    "wk"  #'(windmove-up :wk "move up")
    "wj"  #'(windmove-down :wk "move bottom")
    "w/"  #'(split-window-right :wk "split right")
    "w-"  #'(split-window-below :wk "split bottom")
    "wq"  #'(delete-window :wk "delete window")
    "wm"  #'(maximize-window :wk "maximize window")
    "t"  #'(:ignore t :wk "Toggle")
    "ta"  #'abbrev-mode
    "tA"  #'auto-revert-mode
    "td"  #'toggle-debug-on-error
    "tl"  #'display-line-numbers-mode
    "to"  #'org-mode
    "tt"  #'text-mode
    "tw"  #'whitespace-mode
    ;; Applications
    "a" #'(:ignore t :wk "Applications")
    "ad" #'dired
    "ak" #'(paradox-list-packages :wk "packages")
    )
  (use-package evil
    :demand t
    :commands evil-close-folds
    :general (;;"H-f"    (lambda ()(interactive)(hs-minor-mode)(evil-close-folds)) ; folding
	      ;; emulate i3wm
	      "H-<left>"  #'evil-window-left :which-key "go to left window"
	      "H-<right>" #'evil-window-right :which-key "go to right window"
	      "H-<up>"    #'evil-window-up :which-key "go to up window"
	      "H-<down>"  #'evil-window-down :which-key "go to down window"
	      "H-q"       #'evil-window-delete :which-key "quit window")
    (general-def 'normal prog-mode-map	; TAB to fold
      "TAB" #'evil-toggle-fold)
    :init
    ;; (setq evil-want-keybinding nil)
    (evil-mode 1)
    (global-undo-tree-mode -1)
    :config
    ;; (eval-after-load "evil-maps"	;; unset q for record-macro
    ;;   (define-key evil-normal-state-map "q" nil))
    ;; (use- package evil-collection
    ;;  
    ;;   :config (evil-collection-init))
    )
  (general-swap-key nil 'motion ";" ":")
  (lead1-def			      ; XXX+: simulate-key evi-nerd-commenter
    "w" '("; w" :wk ":w⮒") 		;like C-x C-s save-buffer
    "q" '("; q" :wk ":q⮒")		;like SPC w q delete-window
    ;; "q" '((general-key-dispatch #'delete-window
    ;; 	   :timeout 0.5
    ;; 	   "w" #'evil-delete-buffer) :wk ":q⮒")
    "d"  'evil-delete-buffer)
  (use-package key-chord
    :demand t
    :config
    (setq key-chord-one-key-delay 0.6)
    (setq key-chord-two-keys-delay 0.3)
    (key-chord-mode 1)
    (key-chord-define evil-normal-state-map "qq" "; q")
    (key-chord-define evil-normal-state-map "ww" "; w")
    (key-chord-define evil-normal-state-map "wq" "; wq")
    (key-chord-define evil-normal-state-map "qb" 'evil-delete-buffer)
    )
  (use-package which-key
    :diminish (which-key-mode . " 🔑")
    :init (which-key-mode 1)
    :config (setq which-key-idle-delay 0.05))
  (use-package evil-nerd-commenter	;; TODO: 2
    :init (evilnc-default-hotkeys nil t)) ; only evil default keybinding
  (use-package evil-numbers
    :general (general-def
	       "H-a" #'evil-numbers/inc-at-pt
	       "H-x" #'evil-numbers/dec-at-pt))
  )
(progn					; Ivy Counsel and Swiper
  (use-package smex)            ; Remember past actions
  (use-package counsel
    :general
    ("M-x" #'(counsel-M-x :wk "M-x"))
    (lead0-def :infix "o"
      "t" #'counsel-load-theme)
    (lead0-def
      "SPC" #'(counsel-M-x :wk "M-x")
      "b"   #'(counsel-switch-buffer :wk "change buffer")
      "R"   #'(counsel-bookmark :wk "Open bookmarks")
      "y"   #'(counsel-yank-pop :wk "yank pop")
      ;; bind to double key press
      "f"  #'(:ignore t :wk "files")	; files
      "ff"  #'counsel-find-file
      "fz"  #'counsel-fzf
      "fZ"  (lambda () (interactive "") (cd "~/")(counsel-fzf))
      "fl"  #'counsel-locate
      "fr"  #'counsel-recentf
      "i"  #'(:ignore t :wk "insert")	; insert
      "/"  #'(:ignore t :wk "search")	; searches
      "/r" #'(counsel-rg :wk "ripgrep")
      "/s" #'(counsel-ag :wk "ag")
      "/w" #'(counsel-search :wk "www")
      "//" #'(counsel-recoll :wk "recoll"))
    (general-def :prefix "<f1>"
      "a" #'counsel-apropos
      "k" #'counsel-descbinds
      "f" #'counsel-describe-function
      "v" #'counsel-describe-variable)
    (general-def :prefix "<f2>"
      "<f9>" #'counsel-semantic-or-imenu
      "c" #'counsel-unicode-char
      "h" #'counsel-command-history
      "i" #'counsel-info-lookup-symbol
      "l" #'counsel-find-library
      "L" #'counsel-load-library
      "m" #'counsel-tmm
      "M" #'counsel-minor
      "w" #'counsel-wmctrl)
    :config
    (setq ivy-initial-inputs-alist nil	; ivy always guesses wrong "^"
	  counsel-ag-base-command "ag --vimgrep --hidden -S %s"
	  counsel-rg-base-command
	  "rg --color never --no-heading --hidden -S %s"))
  (use-package ivy
    :demand t
    :commands ivy-add-actions ivy-format-function-line
    :diminish (ivy-mode . "Iv")	; does not display ivy in the modeline
    :general (lead0-def
	      "r" #'ivy-resume)
    :init
    (ivy-mode 1)		      ; enable ivy globally at startup
    :config
    (setq ;ivy-initial-inputs-alist nil	; ivy always guesses wrong "^" XXX: remove
	  ivy-use-virtual-buffers nil ; include 'bookmarks 'recentf or both t
	  enable-recursive-minibuffers t ; enable this if you want `swiper' to use it
	  ivy-height 20		   ; set height of the ivy window
	  ivy-count-format "(%d) " ; count format, from the ivy help page
	  ivy-re-builders-alist '((swiper . ivy--regex-plus)
				  (t      . ivy--regex-ignore-order)))
    ;; for ivy-rich
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    )
  (use-package ivy-rich
    :after ivy
    :init
    (ivy-rich-mode 1))
  (use-package swiper
    :general (
	      "C-s"   #'swiper	                ; search for string in current buffer
	      "C-S-s" #'swiper-isearch-thing-at-point))
  )
(use-package avy			; Move around
  :general (lead0-def
	     "m"   '(:ignore t :which-key "Move avy")
	     "mc" #'avy-goto-char
	     "mi" #'ivy-avy
	     "mw" #'avy-goto-word-1))
(use-package imenu-list			; F9
  :general
  ("<f9>"   #'imenu-list)
  ("<C-f9>" #'imenu-list-smart-toggle))
(use-package deft			; Notes
  :general (lead0-def
	     "an" (lambda () (interactive) (deft) (evil-insert-state nil)))
  (declare-function evil-insert-state "evil")
  :config
  (setq deft-directory "~/Sync/notes"
	deft-recursive t
	deft-use-filename-as-title t
	deft-file-naming-rules '((nospace . "_") (case-fn . downcase)) ;; to preserve slashes
	deft-extensions '("org" "md" "markdown")  ;;"txt"
	deft-use-filter-string-for-filename t))
(use-package ranger
  :general (lead0-def "ar" #'ranger)
  :config (setq ranger-cleanup-eagerly t))
(use-package highlight-indent-guides	; indent guides
  :hook
  ((prog-mode . highlight-indent-guides-mode))
  :init
  (setq highlight-indent-guides-method 'column)
  :config
  (highlight-indent-guides-mode))
(use-package flycheck			; Syntax checking
  :general (lead0-def
	     "e"   '(:ignore t :which-key "Flycheck")
	     "ee" #'flycheck-mode
	     "el" #'flycheck-list-errors
	     "eb" #'flycheck-buffer
	     "ec" #'flycheck-clear
	     "ef" #'counsel-flycheck
	     "eh" #'flycheck-describe-checker
	     "en" #'flycheck-next-error
	     "eN" #'flycheck-previous-error
	     "ep" #'flycheck-previous-error
	     "es" #'flycheck-select-checker
	     "eS" #'flycheck-set-checker-executable
	     "ev" #'flycheck-verify-setup
	     "ey" #'flycheck-copy-errors-as-kill
	     "ex" #'flycheck-explain-error-at-point))
(progn					; Company completion
  (use-package company
    :diminish (company-mode . " ⓐ")
    :defines company-ispell-dictionary
    :commands (company-complete-common-or-cycle)
    :general
    (lead0-def
      "tN" '((lambda () (interactive) (company-ngram-init)) :which-key "Ngram")
      "tS" #'toggle-company-ispell) ;; to complete words
    (general-define-key
     :keymaps 'company-active-map
     "C-n"   #'company-select-next-or-abort
     "C-p"   #'company-select-previous-or-abort
     ;; "TAB"  #'company-complete-selection
     "<tab>" #'company-select-next-if-tooltip-visible-or-complete-selection
     "<S-tab>" (lambda () (interactive "") (company-complete-common-or-cycle -1))
     "<backtab>" (lambda () (interactive "") (company-complete-common-or-cycle -1))
     "C-/"   #'company-search-candidates
     "C-M-/" #'company-filter-candidates
     "C-h"   #'company-show-doc-buffer
     "C-s"   #'counsel-company)
    :init
    (defun toggle-company-ispell ()
      (interactive)
      (cond
       ((memq 'company-ispell company-backends)
	(setq company-backends (delete 'company-ispell company-backends))
	(message "company-ispell disabled"))
       (t
	(add-to-list 'company-backends 'company-ispell)
	(message "company-ispell enabled!"))))
    (setq tab-always-indent 'complete)	;company-require-match 'never
    (global-company-mode)
    :config
    (setq company-tooltip-align-annotations t
	  company-idle-delay 0.2
	  company-minimum-prefix-length 2
	  company-show-numbers t
	  company-require-match nil)
    )
  (use-package company-quickhelp
    :after (company)
    :init (company-quickhelp-mode)
    :general ("<M-f1>"   #'company-quickhelp-mode
	      "<M-S-f1>" #'company-quickhelp-local-mode))
  (use-package company-ngram
    :after company
    :commands company-ngram-command
    :config
    (setq company-ngram-data-dir "~/Sync/ngram")
    ;; company-ngram supports python 3 or newer
    (setq company-ngram-python "python3")
    ;; or use `M-x turn-on-company-ngram' and `M-x turn-off-company-ngram' on individual buffers
    (cons 'company-ngram-backend company-backends)
    (run-with-idle-timer 7200 t (lambda () ; save the cache of candidates
				  (company-ngram-command "save_cache"))))
  (use-package company-statistics
    :after (company)			;works like :hook
    :init (company-statistics-mode))
  (use-package yasnippet
    :diminish (yas-minor-mode " y")
    :general
    (lead0-def
      "Y"  #'(:ignore t :which-key "Yasnippet")
      "Yn" #'yas-new-snippet
      "Yv" #'yas-visit-snippet-file
      "ty" #'yas-minor-mode
      "iy" #'yas-insert-snippet)
    ;; ;; disable yas minor mode map ;; use hippie-expand instead [sp]
    ;; (setq yas-minor-mode-map (make-sparse-keymap))
    :commands (yas-reload-all
    	       company-mode/backend-with-yas)
    :config
    (yas-global-mode 0)
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    (add-hook 'org-mode-hook #'yas-minor-mode)
    (add-hook 'message-mode-hook #'yas-minor-mode)
    (add-hook 'markdown-mode-hook #'yas-minor-mode)
    (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (add-to-list 'yas-snippet-dirs "~/Sync/.emacs/yasnippets")
    (yas-reload-all)
    (setq yas-triggers-in-field t
    	  yas-wrap-around-region t)	;or [a-z] register
    ;; Add yasnippet support for all company backends
    ;; https://github.com/syl20bnr/spacemacs/pull/179
    ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names/10520
    (defun company-mode/backend-with-yas (backend)
      (if (and (listp backend) (member 'company-yasnippet backend))
    	  backend
    	(append (if (consp backend) backend (list backend))
    		'(:with company-yasnippet))))
    (setq company-backends
    	  (mapcar #'company-mode/backend-with-yas company-backends))
    )
  (use-package yasnippet-snippets)
  (use-package ivy-yasnippet
    :after (yasnippet)
    :general (lead0-def
	       "is" #'ivy-yasnippet)
    :init
    ;; (setq ivy-yasnippet-expand-keys nil)
    )
  ;; auto-yasnippet
  ;; yatemplate
  )
(progn 					; Spell checking
  (setq ispell-program-name (executable-find "hunspell")
        ispell-really-hunspell t
	ispell-dictionary "en_US-large")
  (add-to-list 'ispell-skip-region-alist
	       '(("^#+BEGIN_SRC" . "^#+END_SRC")
		 ("^From:" . "line--$")))
  (use-package flyspell
    :hook
    (text-mode . flyspell-mode)
    (prog-mode . flyspell-prog-mode)
    (change-log-mode-hook . (lambda () (flyspell-mode -1)))
    (log-edit-mode-hook . (lambda () (flyspell-mode -1))))
  (use-package flyspell-correct-ivy	; play better with darkroom and the like
  :general (general-define-key
	    :states 'normal
	    :keymaps 'flyspell-mode-map
	    "]z" #'flyspell-correct-at-point)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))
  (use-package guess-language
    :diminish (guess-language-mode . "gL")
    :general (lead0-def
	       "tg" #'guess-language-mode
	       "ts" #'flyspell-mode ; flyspell-correct-auto-mode))
	       "s"   '(:ignore t :which-key "Spell")
	       "se"  '((lambda () (interactive)
		       (ispell-change-dictionary "en_US-large")
		       (setq company-ispell-dictionary "/usr/share/dict/usa")
		       (flyspell-buffer)) :which-key "en_US")
	       "si"  '((lambda () (interactive)
		       (ispell-change-dictionary "it_IT")
		       (setq company-ispell-dictionary "/usr/share/dict/italian")
		       (flyspell-buffer)) :which-key "it_IT"))
    ;; :hook
    ;; (flyspell-mode . guess-language-mode)
    ;; (flyspell-mode-prog . guess-language-mode)
    ;; (text-mode . guess-language-mode)
    :commands guess-language-switch-function
    :config
    (setq guess-language-langcodes '((en . ("en_US-large" "American"))
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
(progn 					; calendars
  (use-package solar			; (2) sunrise and sunset
    :load-path "/usr/share/emacs/"
    :config
    (setq calendar-latitude 46.067270 ; Borino
	  calendar-longitude 11.166153
	  calendar-location-name "Trento"
	  calendar-time-zone 60))
  (setq holiday-other-holidays		; (3) Holidays
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
    :general (lead0-def :infix "o"
	       "p" #'org-gcal-post-at-point ; (add-hook 'org-capture-before-finalize-hook)
	       "d" #'org-gcal-delete-at-point
	       "g" #'org-gcal-sync
	       "G" #'org-gcal-fetch)
    :config
    (setq org-gcal-token-file "~/Sync/.emacs/org-gcal/.org-gcal-token"
	  org-gcal-client-id "1086004898054-uhp29b0kek41obv1dma52rpog8pr44gu.apps.googleusercontent.com"
	  org-gcal-client-secret "sP2Jupy5GKtdDAAgupQrSzc2"
	  org-gcal-file-alist '(("danielepietroarosio@gmail.com" .
				 "~/Sync/box/org/gcal/dpa.org")
				("c87gevr5pc3191on8c7nh8b4nc@group.calendar.google.com" .
				 "~/Sync/box/org/gcal/figli.org")
				("tq1af7efj4l9h8glgqi2g5vmsg@group.calendar.google.com" .
				 "~/Sync/box/org/gcal/IBF.org")))
    )
  (use-package calfw
    :general (lead0-def "oW" #'cfw:open-calendar-buffer))
  (use-package calfw-org
    :general (lead0-def "ow" #'cfw:open-org-calendar))
  )
(use-package visual-fill-column
  :general
  ("C-c v" #'visual-fill-column-mode
   "<f12>" #'no-distraction-enable
   "<C-f12>" #'no-distraction-disable)
  ;; (general-swap-key nil 'motion
  ;;   ;; swap evil-next-line evil-next-visual-line
  ;;   "k" "gk"
  ;;   "j" "gj")
  :commands (visual-fill-column-adjust)
  :init
  (setq visual-fill-column-center-text t
	visual-fill-column-width 88
	visual-fill-column-fringes-outside-margins nil
	;; set right curly arrow even when visual line mode is wrapping logical lines into visual ones.
	;; visual-line-fringe-indicators '(right-arrow right-curly-arrow)
	visual-line-fringe-indicators '(bottom-left-angle top-right-angle)
	;; allow splitting windows with wide margins
	split-window-preferred-function #'visual-fill-column-split-window-sensibly)
  (defun no-distraction-enable ()
    "Switch to no distraction env"
    (interactive)
    (visual-fill-column-mode)
    (text-scale-increase 2)
    (wc-mode))
  (defun no-distraction-disable ()
    "Switch off from no distraction env"
    (interactive)
    (visual-fill-column-mode -1)
    (text-scale-set 0)
    (wc-mode -1))
  :config
  ;; adjust margins upon text resize
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  (add-hook 'visual-fill-column-mode-hook #'visual-line-mode)
  ;; (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  )
(use-package evil-better-visual-line
  :init
  (evil-better-visual-line-on))
(use-package wc-mode
  :general
  ;; (general-unbind 'normal wc-mode-map
  ;; :with 'ignore
  ;; [remap wc-set-word-goal]
  ;; [remap wc-set-line-goal]
  ;; [remap wc-set-char-goal]
  ;; [remap wc-count])
  (lead0-def
	"xw"  #'(wc-mode :which-key "word count")
    )
  (lead0-def
   :keymaps 'wc-count-map
   "xww" #'wc-set-word-goal
   "xwl" #'wc-set-line-goal
   "xwa" #'wc-set-char-goal
   "xwc" #'wc-count)
  ("C-c C-w" #'org-refile)
  ;; :general
  ;; (general-define-key
  ;; 		      "x n" '(:keymap wc-mode-map :package wc-mode))
  :config
  (setq wc-modeline-format "Wd.%tw  Ch.%tc"))
(progn					; mu4e
  ;; TODO: https://vxlabs.com/2015/01/28/sending-emails-with-math-and-source-code/
  (setq mail-user-agent 'mu4e-user-agent)
  (when (fboundp 'imagemagick-register-types) (imagemagick-register-types))
  ;; (use-package evil-mu4e)
  (use-package mu4e
    :load-path "/usr/share/emacs/site-lisp/mu4e/"
    :commands (mu4e-message
	       mu4e-context-current
	       mu4e-headers-mark-and-next
	       mu4e-get-headers-buffer
	       mu4e-error)
    :general
    ("C-x m" #'mu4e-compose-new)
    (lead0-def "am" #'mu4e)
    :config
    ;; keep byte-compiler happy
    (declare-function mu4e~headers-goto-docid "mu4e-headers")
    (declare-function mu4e~view-get-attach "mu4e-headers")
    (declare-function mu4e~write-body-to-html "mu4e-headers")
    (use-package message
      :load-path "/usr/share/emacs/"
      :commands (message-sendmail-envelope-from
		 message-add-header
		 message-remove-header))
    (setq mu4e-maildir "~/Sync/Maildir")
    (progn				; config
      (use-package org-mu4e
	:load-path "/usr/share/emacs/site-lisp/mu4e/"
	:config
	(setq org-mu4e-link-query-in-headers-mode t))
      ;;store link to message if in header view, not to header query; or viceversa
      (setq mu4e-confirm-quit nil
	    mu4e-update-interval 120
	    mu4e-auto-retrieve-keys t
	    mu4e-headers-leave-behavior 'apply ; leaving headers view apply all marks
	    mu4e-headers-visible-lines 18
	    mu4e-headers-auto-update t	; this is the default
	    mu4e-hide-index-messages t  ; hide updating messages
	    mu4e-view-show-addresses t	; show full addresses
	    mu4e-get-mail-command "mbsync -a"
	    mu4e-change-filenames-when-moving t ; rename files when moving (Needed for mbsync)
	    message-kill-buffer-on-exit t	; don't keep message buffers around
	    mu4e-view-show-images t          ; enable inline images and VIEW
	    mu4e-use-fancy-chars t
	    mu4e-html2text-command 'mu4e-shr2text
	    mu4e-headers-include-related nil ; =W= to view threads
	    mu4e-headers-skip-duplicates nil ; =V= to skip duplicates
	    mu4e-headers-fields		; headers list (nil means unlimited)
	    '( (:human-date     .  18)
	       (:flags          .   5)
	       (:from           .  22)
	       (:mailing-list   .  12)
	       (:size           .   7)
	       (:thread-subject . nil))
	    mu4e-attachment-dir  "~/"
	    mu4e-save-multiple-attachments-without-asking t
	    ;; mu4e-headers-date-format "%Y-%m-%d %H:%M" ; give me ISO(ish) format date-time stamps in the header list
	    mu4e-completing-read-function 'completing-read ; use convenient completion for navigation =j o=
	    )
      (setq mu4e-contexts		; Contexts
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
	       )
	    ;; start with the first (default) context; 
	    mu4e-context-policy 'pick-first
	    ;; default is to ask-if-none (ask when there's no context yet, and none match)
	    mu4e-compose-context-policy 'ask-if-none
	    )
      (setq mu4e-sent-messages-behavior	; Save msg into sent folder only for pec
	    (lambda ()
	      (if (string= (message-sendmail-envelope-from) "daniele.arosio@postecert.it")
		  'sent
		'delete)))
      (setq mu4e-maildir-shortcuts	; Shortcuts
	    '( ("/cnr/INBOX"         . ?i)
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
	    `(("flag:unread AND NOT flag:trashed AND NOT maildir:/feeds"
	       "Unread messages" ?u)
	      ("maildir:/feeds"
	       "Feeds" ?f)
	      ("date:today..now"
	       "Today's messages" ?t)
	      ("date:7d..now"
	       "Last 7 days" ?w)
	      ("mime:image/*"
	       "Messages with images" ?p)
	      ("size:5M..500M"
	       "Big messages"     ?b)
	      ("flag:unread NOT flag:trashed AND (flag:list OR from:trac@sagemath.org)"
	       "Unread bulk messages" ?l)
	      (,(mapconcat 'identity
			   (mapcar
			    (lambda (maildir)
			      (concat "maildir:" (car maildir)))
			    mu4e-maildir-shortcuts) " OR ")
	       "All inboxes" ?i))
	    )
      (progn				; Tags and personal archive
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
		       :char       "p"
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
	(define-key mu4e-headers-mode-map (kbd "z") 'mu4e-headers-mark-for-tag)
	(define-key mu4e-headers-mode-map (kbd "p") 'mu4e-headers-mark-for-personal)
	(define-key mu4e-view-mode-map (kbd "z") 'mu4e-view-mark-for-tag)
	(define-key mu4e-view-mode-map (kbd "p") 'mu4e-view-mark-for-personal)
	)
      (progn				; view-actions
	(setq mu4e-view-actions '(("capture message" . mu4e-action-capture-message)
				  ("show this thread" . mu4e-action-show-thread)))
	(add-to-list 'mu4e-view-actions
		     '("view in browser" . mu4e-action-view-in-browser) t)
	(defun mu4e-action-save-to-pdf (msg)
	  (let* ((date (mu4e-message-field msg :date))
		 (infile (mu4e~write-body-to-html msg))
		 (outfile (format-time-string "%Y-%m-%d%H%M%S.pdf" date)))
	    (with-temp-buffer
	      (shell-command
	       (format "wkhtmltopdf %s ~/%s" infile outfile) t))))
	(add-to-list 'mu4e-view-actions '("print to pdf" . mu4e-action-save-to-pdf) t)
	(defun jcs-view-in-eww (msg)
	  (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))
	(add-to-list 'mu4e-view-actions '("eww view" . jcs-view-in-eww) t)
	)
      (progn				; view-attachment-actions  https://vxlabs.com/tag/mu4e/
	(defun my-remove-attachment (msg num)
	  "Remove attachment."
	  (let* ((attach (mu4e~view-get-attach msg num))
		 (path (mu4e-msg-field msg :path))
		 (filename (and attach (plist-get attach :name)))
		 (cmd (format "touch /tmp/%s-removed; altermime --input=%s --replace='%s' --with='/tmp/%s-removed'"  filename path filename filename)))
	    (when (and filename
		       (yes-or-no-p
			(format "Are you sure you want to remove '%s'?" filename)))
	      (shell-command cmd)
	      (message cmd))))
	(add-to-list 'mu4e-view-attachment-actions
		     '("remove-attachment" . my-remove-attachment))
	)
      )
    (progn				; Composing and Sending
      (setq message-send-mail-function 'message-send-mail-with-sendmail
	    ;; message-send-mail-function 'smtpmail-send-it
	    ;; send-mail-function 'sendmail-send-it ; send
	    ;; sendmail-program "msmtp-enqueue.sh"
	    mail-specify-envelope-from t ;'header
	    mail-envelope-from 'header
	    message-sendmail-envelope-from 'header
	    message-sendmail-f-is-evil nil
	    mail-interactive t
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
      (setq mu4e-compose-signature-auto-include nil
	    mu4e-compose-forward-as-attachment nil
	    message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n"
	    message-citation-line-function 'message-insert-formatted-citation-line
	    mu4e-compose-in-new-frame t	  ; every new email composition gets its own frame
            org-mu4e-convert-to-html t
	    mu4e-compose-format-flowed t  ; Set format=flowed
	    fill-flowed-encode-column 998 ; https://www.ietf.org/rfc/rfc2822.txt
	    )
      (add-hook 'message-mode-hook ; http://www.djcbsoftware.nl/code/mu/mu4e/Compose-hooks.html
		;; (add-hook 'mu4e-compose-mode-hook
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
		  ;; (turn-on-orgtbl)   ;; this prevented flyspell correction
		  ;; (turn-on-orgstruct)  ;; probably included in ++
		  ;; (turn-on-orgstruct++)  ;; =,he= to send bibtex email
		  ;; (typo-mode)
		  (footnote-mode)
		  ))
      )
    (progn				; Keybindings
      ;; (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
      ;;   "t" 'message-goto-to
      ;;   "m" 'message-goto-body
      ;;   "b" 'message-goto-bcc
      ;;   "c" 'message-goto-cc
      ;;   "s" 'message-goto-subject)

      ;; (evilified-state-evilify-map mu4e-view-mode-map
      ;;   :mode mu4e-view-mode
      ;;   :bindings
      ;;   (kbd "f") 'mu4e-headers-mark-for-flag)

      (general-define-key :keymaps 'mu4e-compose-mode-map
			  "C-c o" 'org~mu4e-mime-switch-headers-or-body
			  )
      (general-define-key :keymaps 'mu4e-view-mode-map
			  "<tab>"     'org-next-link	; 'shr-next-link
			  "<backtab>" 'org-previous-link	; 'shr-previous-link
			  ","         'mu4e-view-headers-prev
			  "N"         'mu4e-view-headers-prev
			  "G"         'end-of-buffer
			  "V"         'visual-fill-column-mode
			  )
      (general-define-key :keymaps 'mu4e-headers-mode-map
			  ","	   'mu4e-headers-prev
			  "N"	   'mu4e-headers-prev
			  "G"         'end-of-buffer
			  "f"         'mu4e-headers-mark-for-flag
			  )
      )
    (progn				; TODO: multiattachment
					; Multiattchments ============================================================
      ;; (eval-when-compile (require 'dired))
      ;; (defun iqbal-mu4e-file-attach-marked-files ()
      ;;   (gnus-dired-attach (dired-map-over-marks (dired-get-file-for-visit) nil)))
      ;; (defun iqbal-mu4e-attach-files-from-dired ()
      ;;   (interactive)
      ;;   (if (region-active-p)
      ;;       (iqbal-mu4e-file-attach-files-from-region)
      ;;     (iqbal-mu4e-file-attach-marked-files)))
      ;; (with-eval-after-load 'dired
      ;;   (define-key dired-mode-map (kbd "a") #'iqbal-mu4e-attach-files-from-dired))
      ;; (with-eval-after-load 'helm-files
      ;;   (add-to-list 'helm-find-files-actions
      ;;                '("Attach files for mu4e" . iqbal-helm-mu4e-attach) t)
      ;;   (defun iqbal-helm-mu4e-attach (_file)
      ;;     (gnus-dired-attach (helm-marked-candidates))))

      ;; ;; Attachment from locate "SPC f L" ... "C-z"
      ;; (helm-add-action-to-source "Attach to Email" #'mml-attach-file helm-source-locate)
      ;; Attach multiple files from helm-ind-files-actions (and dired)
      (ivy-add-actions
       'counsel-locate
       '(("a" (lambda (x)
		(unless (memq major-mode '(mu4e-compose-mode message-mode))
		  (compose-mail))
		(mml-attach-file x)) "Attach to email")))


      ;; ;; http://pragmaticemacs.com/emacs/email-templates-in-mu4e-with-yasnippet/
      ;; (add-hook 'mu4e-compose-mode-hook 'spacemacs/load-yasnippet)

      )
    )
  (use-package mu4e-jump-to-list
    :after mu4e)
  (use-package mu4e-maildirs-extension
    :after mu4e				; very slow to load
    :defines mu4e-maildirs-extension-default-collapse-level
    ;; :commands (mu4e-maildirs-extension-load)
    :general
    (general-def :keymaps 'mu4e-main-mode-map
      "TAB" 'mu4e-maildirs-extension-toggle-maildir-at-point)
    :init
    (mu4e-maildirs-extension-load)
    :config
    ;; mu4e-maildirs-extension-maildir-collapsed-prefix "archives"
    (setq mu4e-maildirs-extension-default-collapse-level 0))
  ;; (use-package mu4e-conversation
  ;;   :after mu4e
  ;;   :config (global-mu4e-conversation-mode))
  )
(progn					; org
  ;; (use-package beginend)
  (use-package org
    :ensure org-plus-contrib
    :commands (org-entry-get
	       ;; org-babel-do-load-languages
	       org-capture-finalize
	       org-speed-move-safe
	       org-narrow-to-subtree
	       org-clock-in
	       org-read-date)
    :hook ((org-mode . visual-line-mode)
	   (org-mode . flyspell-mode))
    :general
    (lead0-def
      "o'" #'counsel-evil-marks
      "oa" #'counsel-org-agenda-headlines
      "oA" #'counsel-org-goto-all
      "oc" #'counsel-colors-web
      "oC" #'counsel-colors-emacs
      "or" #'counsel-evil-registers
      )
    (lead0-def
      :keymaps 'org-mode-map
      "ie" #'counsel-org-entity
      "oo" #'counsel-outline
      "of" #'counsel-org-file
      "om" #'counsel-mark-ring
      "ol" #'counsel-org-link
      "oz" #'counsel-org-tag)
    (general-define-key
     :keymaps 'org-agenda-mode-map
     "Z" #'counsel-org-tag-agenda)
    (lead1-def
      :keymaps 'org-mode-map
      :states '(normal emacs)
      "," #'org-ctrl-c-ctrl-c)
    (general-define-key :states '(normal)
			"RET" #'org-return)
    :config
    (use-package ob-async
      :after (org))
    (use-package org-super-agenda)
    (setq org-hide-emphasis-markers t)	; a better word processor
    ;; (setq org-export-latex-hyperref-format "\\ref{%s}") ; for org mode < 8.0
    ;; https://orgmode.org/worg/org-tutorials/org-latex-export.html
    (setq org-return-follows-link t)
    (setq org-highlight-latex-and-related '(latex)) ; Then inline latex like $y=mx+c$ will appear in a different color
    ;; (setq org-startup-truncated nil)		    ; fill wrap long line, needed for org headings? FIXME
    ;; split line for new heading, item or row?
    ;; (setq org-M-RET-may-split-line '((default . t)
    ;; 				     (headline . nil)
    ;; 				     (item . nil)
    ;; 				     (table . nil)))
    (add-to-list 'org-structure-template-alist '("sl" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("sb" . "src sh :results output :exports both"))
    (add-to-list 'org-structure-template-alist '("sB" . "src sh :session bash :results output :exports both"))
    (add-to-list 'org-structure-template-alist '("sj" . "src jupyter-python"))

    ;; init of my-gtd.conf
    (defun internet-up-p (&optional host)
      "Check internet connectivity. Default HOST is google."
      (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
			 (if host host "www.google.com"))))
    (defun fetch-calendar ()
      "Fetch gcals."
      (interactive)
      (when (internet-up-p) (org-gcal-fetch)))
    (declare-function internet-up-p .)
    (declare-function fetch-calendar .)
    ;; A bare minimum simple starting to personalizing org for gtd.
    ;; https://orgmode.org/worg/org-configs/org-customization-guide.html
    (progn					; Define variables
      (load-library "find-lisp")
      (declare-function find-lisp-find-files "find-lisp")
      (defvar da-agenda-and-refile-files
	(append '("~/Sync/box/org/gtd.org"
		  "~/Sync/box/org/ideas.org"
		  "~/Sync/box/org/inbox.org"
		  "~/Sync/box/org/projects.org"
		  "~/Sync/box/org/someday.org"
		  "~/Sync/box/org/TODOs.org" ;; target for org-projectile
		  "~/Sync/box/org/spesa.org")
		(find-lisp-find-files "~/Sync/notes/arch/" "\.org$")
		(find-lisp-find-files "~/Sync/notes/home/" "\.org$")
		(find-lisp-find-files "~/Sync/proj/" "\.org$")))
      (defvar da-gtd "~/Sync/box/org/gtd.org")
      (defvar da-wrtemplate "~/.spacemacs.d/templates/my_weeklyreviewtemplate.org")
      (defvar da-templates "~/.spacemacs.d/templates")
      )
    (progn					; (1) Agenda files; (2) Archives; and (3) Refile
      (setq-default org-directory "~/Sync/box/org")
      (setq org-agenda-files (append da-agenda-and-refile-files
				     '("~/Sync/box/org/diary.org"
				       "~/Sync/box/org/gcal/")))
      (setq org-agenda-diary-file "~/Sync/box/org/diary.org"
	    org-agenda-include-diary t)
      ;; ARCHIVE
      (setq org-archive-location "~/Sync/box/org/archives/%s_archive::")
      (defvar org-archive-file-header-format "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n")
      ;; REFILE
      (setq org-refile-targets '((da-agenda-and-refile-files :maxlevel . 5)))
      ;; Be sure to use the full path preceded by filename to insert at top level
      (setq org-refile-use-outline-path 'file)
      ;; Targets complete directly with helm/ido
      (setq org-outline-path-complete-in-steps nil)
      ;; Allow refile to create parent tasks with confirmation
      (setq org-refile-allow-creating-parent-nodes 'confirm)
      )
    (progn					; (4) Stuck project; and (5) TODOs and tags
      (setq org-stuck-projects
	    '("+proj/-DONE-HOLD" ("NEXT") nil ""))
      (setq org-todo-keywords
	    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	      (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")) )
      (setq org-todo-keyword-faces
	    '(("TODO" :foreground "red" :weight bold)
	      ("NEXT" :foreground "light blue" :weight bold)
	      ;; ("APPT" :foreground "yellow" :weight bold)
	      ("DONE" :foreground "forest green" :weight bold)
	      ("WAITING" :foreground "orange" :weight bold)
	      ("HOLD" :foreground "magenta" :weight bold)
	      ("CANCELLED" :foreground "forest green" :weight bold)
	      ("MEETING" :foreground "forest green" :weight bold)
	      ))
      (setq org-todo-state-tags-triggers
	    '(("CANCELLED" ("CANCELLED" . t))
	      ("WAITING" ("WAITING" . t))
	      ("HOLD" ("WAITING") ("HOLD" . t))
	      (done ("WAITING") ("HOLD"))
	      ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
	      ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
	      ("DONE" ("WAITING") ("CANCELLED") ("HOLD")) ))
      (setq org-use-fast-todo-selection t)
      ;; S-left S-right skipping setting timespamps  DEF
      (setq org-treat-S-cursor-todo-selection-as-state-change nil)
      (setq org-log-into-drawer t)
      (setq org-log-done 'time)
      ;; for when I set a task as e.g. Canceled open the buffer in insert state
      (add-hook 'org-log-buffer-setup-hook 'evil-insert-state)
      ;; CONTEXTS
      (setq org-tag-persistent-alist '((:startgroup)  ;; mutually exclusive
				       ("Contexts")
				       (:grouptags)
				       ("@errand" . ?e)
				       ("@fbk" . ?f)
				       ("@home" . ?h)
				       ("@telephone" . ?t)
				       (:endgroup)
				       ("@net" . ?n)  ;; I doubt it is usefull
				       ("PERSONAL" . ?p)
				       ("WORK" . ?w)))
      (setq org-tag-alist (quote (("@dati" . ?d)
				  ("@mail" . ?m)
				  ("idea" . ?i)
				  ("proj" . ?j)
				  )))
      ;; Allow setting single tags without the menu
      (setq org-fast-tag-selection-single-key 'expert)
      ;; Include the todo keywords
      (setq org-fast-tag-selection-include-todo nil)
      )
    (progn					; (6) Captures
      (setq org-default-notes-file "~/Sync/box/org/inbox.org")
      ;; defconst (concat da-templates ".org")
      (define-key global-map "\C-ct" (lambda () (interactive) (org-capture nil "t")))
      (setq org-capture-templates
	    '(("r" "Reply to" entry (file+headline da-gtd "Reply")
	       "* TODO %a to %:from \nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n %?" :unnarrowed t)
	      ("w" "Wait for Reply" entry (file+headline da-gtd "Reply")
	       "* WAITING %a from %:from" :immediate-finish t)
	      ("t" "todo" entry (file org-default-notes-file)
	       "* TODO %? %:subject\n%U\n%a\n" :kill-buffer t)
	      ("a" "Experimental appointments" entry (file+headline da-gtd "Appointments")
	       "* %? %:subject\n SCHEDULED:%^T--%^T\n %a\n")
	      ("g" "Gcal dpa" entry (file  "~/Sync/box/org/gcal/dpa.org")
	       "* %? %:subject\n :PROPERTIES:\n :calendar-id: danielepietroarosio@gmail.com\n :END:\n:org-gcal:\n%^T--%^T\n%a\n:END:" :empty-lines 1)
	      ("f" "Gcal figli" entry (file  "~/Sync/box/org/gcal/figli.org")
	       "* %? %:subject\n :PROPERTIES:\n :calendar-id: c87gevr5pc3191on8c7nh8b4nc@group.calendar.google.com\n :END:\n:org-gcal:\n%^T--%^T\n%a\n:END:" :empty-lines 1)
	      ("h" "new Habit" entry (file+headline da-gtd "Habits")
	       "* TODO %? \nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:REPEAT_TO_STATE: TODO\n:END:\n%a")
	      ("u" "Urgent task" entry (file+headline da-gtd "Tasks") "** NEXT %? \nDEADLINE: %t")
	      ("i" "new Idea" entry (file "~/Sync/box/org/ideas.org") "* %^{Idea} \n%u\n%a\n%?" :empty-lines 1)
	      ;; diary.org
	      ("j" "Journal" entry (file+olp+datetree "~/Sync/box/org/diary.org") "* %?\n%t\n" ) ; prefix C-1 alternative to time-prompt t
	      ("m" "Meeting" entry (file+olp+datetree "~/Sync/box/org/diary.org") "* MEETING %? :MEETING:\n%T" :clock-in t :clock-resume t)
	      ;; reading
	      ("R" "to read" entry (file+headline da-gtd "Reading")
	       "* TODO Read %a \n%U\n" :unnarrowed t :kill-buffer t)
	      ;; spesa.org
	      ("s" "Spesa" entry (file+headline "~/Sync/box/org/spesa.org" "Supermarket") ;; TODO: try checkitem
	       "* TODO %? \n")
	      ("rd" "Review: Daily" entry (file+olp+datetree "/tmp/daily-reviews.org")
	       (file "~/.spacemacs.d/templates/my_dailyreviewtemplate.org"))
	      ("rw" "Review: Weekly Review" entry (file+olp+datetree "/tmp/weekly-reviews.org")
	       (file "~/.spacemacs.d/templates/my_weeklyreviewtemplate.org"))
	      ))
      (setq org-capture-templates-contexts
	    '(("r" ((in-mode . "mu4e-view-mode")))
	      ("w" ((in-mode . "mu4e-view-mode")))))
      (add-hook 'org-capture-mode-hook 'evil-insert-state)
      )
    ;; Display properties (org files and agenda views)
    (defun gs/org-agenda-add-location-string ()
      "Gets the value of the LOCATION property."
      (let ((loc (org-entry-get (point) "LOCATION")))
	(if (> (length loc) 0)
	    (concat "{" loc "} ") "")))
    (setq org-cycle-separator-lines 2 ;; default
	  org-tags-column -82
	  ;; org-agenda-tags-column -82
	  ;; org-agenda-sticky t
	  org-agenda-dim-blocked-tasks t  ;; Dim blocked tasks
	  org-enforce-todo-dependencies t
	  org-enforce-todo-checkbox-dependencies t
	  org-agenda-show-future-repeats nil  ;; 'next to view this and the next.
	  org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
				     (timeline . "  % s")
				     (todo . "  %-12:c  ")
				     (tags . "  %-12:c  ")
				     (search . "  %i %-12:c"))
	  org-columns-default-format
	  "%48ITEM(Task) %4TODO(todo) %ALLTAGS %SCHEDULED %6Effort(Effort){:} %6CLOCKSUM{:} %DEADLINE")
    ;; TODO: custom agenda view http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
    ;; https://github.com/mwfogleman/.emacs.d/blob/master/michael.org
    ;; https://gist.github.com/mwfogleman/267b6bc7e512826a2c36cb57f0e3d854
    (progn					; == Agenda ==
      (setq org-agenda-search-view-always-boolean t ;; lazy boolean search =C-c a s=
	    org-agenda-text-search-extra-files `(agenda-archives) ;; Include agenda archives when searching
	    org-deadline-warning-days 7)
      ;; For tag searches ignore tasks with scheduled and deadline dates FIXME better control this in each agenda custom view
      (setq org-agenda-tags-todo-honor-ignore-options t)
      ;; all properties are inherited
      (setq org-use-property-inheritance t) ;; @2DO to be used with STYLE, e.g. habit not scheduled
      ;; (defun nemacs-org-agenda-startup ()
      ;;   (interactive)
      ;;   (org-agenda :keys "gtd"))
      ;; (defun nemacs-org-agenda-mark-as-done (&optional arg)
      ;;   (interactive "P")
      ;;   (org-agenda-todo "DONE"))
      ;; (defun nemacs-org-agenda-mark-as-done-capture-follow-up (&optional arg)
      ;;   (interactive "P")
      ;;   (org-agenda-todo "DONE")
      ;;   (org-agenda-switch-to)
      ;;   (org-capture 0 "t"))
      ;; :hook (org-agenda-mode . nemacs-org-agenda-hook)
      ;; :bind (("C-c a" . org-agenda)
      ;;        ("C-c d" . nemacs-org-agenda-startup)
      ;;        (:map org-agenda-mode-map
      ;;              ("g" . org-gcal-fetch)
      ;;              ("x" . nemacs-org-agenda-mark-as-done)
      ;;              ("X" . nemacs-org-agenda-mark-as-done-capture-follow-up)))
      ;; (org-agenda-skip-deadline-if-done nil)
      ;; (org-agenda-skip-scheduled-if-done nil)
      ;; (defun nemacs-org-capture-add-basic-properties ()
      ;;   (interactive)
      ;;   (org-id-get-create))
      (defun nemacs-org-capture-review-daily ()
	(interactive)
	(progn
	  (org-capture nil "rd")
	  (org-capture-finalize t)
	  (org-speed-move-safe 'outline-up-heading)
	  (org-narrow-to-subtree)
	  (fetch-calendar)
	  (org-clock-in)))
      (define-key global-map "\C-crd" 'nemacs-org-capture-review-daily)
      (defun my-new-weekly-review ()
	(interactive)
	(progn
	  (org-capture nil "rw")
	  (org-capture-finalize t)
	  (org-speed-move-safe 'outline-up-heading)
	  (org-narrow-to-subtree)
	  (fetch-calendar)
	  (org-clock-in)))
      (define-key global-map "\C-crw" 'my-new-weekly-review)
      ;; (defun nemacs-org-capture-review-weekly ()
      ;;   (interactive)
      ;;   (progn
      ;;     (org-capture nil "rw")
      ;;     (org-capture-finalize t)
      ;;     (org-speed-move-safe 'outline-up-heading)
      ;;     (org-narrow-to-subtree)
      ;;     (org-gcal-fetch)
      ;;     (org-clock-in)))
      ;; :hook (org-capture-before-finalize . nemacs-org-capture-add-basic-properties)
      ;; :bind (("M-m"     . nemacs-org-capture)
      ;;        ("C-c c"   . org-capture)
      ;;        ("C-c r d" . nemacs-org-capture-review-daily)
      ;;        ("C-c r w" . nemacs-org-capture-review-weekly))
      ;; :custom
      ;; (org-capture-templates `(("t" "Add TODO Task" entry (file ,org-default-notes-file)
      ;;                           ,nemacs-org-capture-basic-template
      ;;                           :empty-lines 1 :immediate-finish t)
      ;;                          ("T" "Add Linked TODO Task" entry (file ,org-default-notes-file)
      ;;                           ,nemacs-org-capture-link-template
      ;;                           :empty-lines 1 :immediate-finish t)
      ;;                          ("c" "Add Contact" entry (file "~/Dropbox/orgfiles/contacts.org")
      ;;                           ,nemacs-org-capture-contact-template
      ;;                           :empty-lines 1)
      ;;                          ("rw" "Review: Weekly" entry (file+olp+datetree "/tmp/reviews.org")
      ;;                           (file "~/Dropbox/orgfiles/templates/weekly-review.template.org")))))
      (defun my-org-agenda-recent-open-loops ()
	(interactive)
	(let ((org-agenda-start-with-log-mode t)
	      (org-agenda-use-time-grid nil))
	  (fetch-calendar)
	  (org-agenda-list nil (org-read-date nil nil "-2d") 4)
	  ;; (beginend-org-agenda-mode-goto-beginning)
	  ))
      (defvar target-date (org-read-date nil nil "+7d"))
      (setq org-agenda-custom-commands
	    '(
	      ("u" "Unscheduled TODOs" ((tags-todo "-proj-CANCELLED/-WAITING-HOLD"
						   ;; ("u" "Unscheduled TODOs" ((todo "TODO"
						   ((org-agenda-overriding-header "Unscheduled inactive Tasks")
						    (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
						    (org-agenda-todo-ignore-scheduled 'all)))))
	      ("a" "Today actions list"
	       ((org-super-agenda-mode)
		(agenda "" ((org-deadline-warning-days 1)
			    (org-super-agenda-groups
			     ;; :tag ("PERSONAL" "@home")  ?maybe
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
		(alltodo "Further picks"
			 ((org-agenda-overriding-header "")
			  (org-agenda-todo-ignore-scheduled t)
			  (org-super-agenda-groups
			   '((:name "Important" :priority>="A")
			     (:name "Quick Picks"
				    :and (:not (:scheduled t :deadline t)
					       :effort< "0:15"))
			     (:discard (:anything t)))))))
	       ((org-agenda-span 'day)
		(org-agenda-compact-blocks t)))
	      ("r" "Daily review"
	       ((org-super-agenda-mode)
		(agenda "Today" ((org-deadline-warning-days 0)
				 (org-super-agenda-groups
				  '((:name "Overdue"
					   :scheduled past
					   :deadline past)
				    (:name "Today schedule"
					   :time-grid t
					   :deadline today
					   :scheduled today
					   :order 1)))))
		(agenda "Tomorrow" ((org-agenda-start-day "+1d")
				    (org-super-agenda-groups
				     '((:name "Tomorrow schedule"
					      :time-grid t
					      :scheduled future)))))
		(alltodo "Further picks"
			 ((org-agenda-overriding-header "")
			  (org-agenda-todo-ignore-scheduled t)
			  (org-super-agenda-groups
			   `((:name "Important" :priority>="A")
			     (:discard (:deadline (after ,target-date)))
			     (:name "Due within next 7 days" :deadline future)
			     (:discard (:anything t))))))
		(tags "REFILE"
		      ((org-agenda-overriding-header "Tasks to Refile")))
		(tags "-NOTE-REFILE-ARCHIVE/DONE|CANCELLED"
		      ((org-agenda-overriding-header "Tasks to Archive")))
		(tags-todo "-CANCELLED/NEXT"
			   ((org-agenda-overriding-header "Next Tasks:")
			    (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-sorting-strategy '(habit-up category-keep priority-down))
			    (org-tags-match-list-sublevels 'indented))))
	       ((org-agenda-span 'day)
		(org-agenda-compact-blocks t)))
	      ("b" "Backwards calendar loops"
	       ;; (,(my-org-agenda-longer-open-loops)))
	       ((org-super-agenda-mode)
		(agenda ""
			((org-agenda-overriding-header "Backwards calendar loops")
			 (org-agenda-span 10)
			 (org-agenda-start-day "-10d")
			 (org-agenda-start-with-log-mode t)  ;; FIXME is not working
			 (org-agenda-use-time-grid t)))))
	      ("x" "Tasks to refile or archive"
	       ((tags "REFILE"
		      ((org-agenda-overriding-header "Tasks to Refile")))
		(tags "-NOTE-REFILE-ARCHIVE/DONE|CANCELLED"
		      ((org-agenda-overriding-header "Tasks to Archive")))))
	      ("h" "Habits and Recurring Tasks"
	       ((org-super-agenda-mode)
		(alltodo "" ((org-super-agenda-groups '(
							(:name "Habits" :habit t :order 100)
							(:auto-group t :order 90)
							(:discard (:anything t))
							))
			     (org-agenda-overriding-header "Habits and Recurring Tasks")))))
	      ("H" "Habits and Recurring Tasks"
	       ((org-super-agenda-mode)
		(agenda "deadlines" ((org-super-agenda-groups '((:discard (:not (:deadline t)))
								(:name "Habits" :habit t)
								(:auto-group t)
								(:discard (:anything t))))
				     (org-agenda-overriding-header "Recurring deadlines")
				     (org-agenda-span 'day)
				     (org-deadline-warning-days 730)
				     (org-agenda-compact-blocks nil)))
		(alltodo "scheduled" ((org-super-agenda-groups '((:discard (:deadline t))
								 (:name "Scheduled Habits" :habit t)
								 (:auto-group t)
								 (:discard (:anything t))))
				      (org-agenda-overriding-header "Scheduled Habits and Recurring Tasks")
				      (org-agenda-compact-blocks nil)))))
	      ("l" "Stadalone Tasks"
	       ((org-super-agenda-mode)
		(tags-todo "-recurring" ((org-super-agenda-groups '((:discard (:tag "proj"))
								    (:discard (:habit t))
								    ;; (:discard (:auto-group t)) ;; unable to discard recurring
								    (:name "High-priority tasks"
									   :priority>= "A")
								    (:name "Transforming into projects?"
									   :children todo :order 1)
								    (:name "With subtasks"
									   :children t :order 2)
								    (:name "Personal Next"
									   :and (:tag ("PERSONAL" "@home") :todo "NEXT")
									   :order 20)
								    (:name "Personal"
									   :and (:tag ("PERSONAL" "@home") :todo "TODO")
									   :order 21)
								    ;; (:name "Personal"
								    ;;        (:tag ("PERSONAL" "@home") :auto-todo))
								    (:name "Work Next"
									   :and (:tag "WORK" :todo "NEXT")
									   :order 5)
								    (:name "Work"
									   :and (:tag "WORK" :todo "TODO")
									   :order 10)
								    (:name "Other next actions"
									   :todo "NEXT"
									   :order 20)
								    (:name "Other todo actions"
									   :todo "TODO"
									   :order 30)
								    (:discard (:anything t))))
					 (org-agenda-overriding-header "Standalone Tasks")
					 (org-agenda-sorting-strategy '(deadline-up category-keep priority-down))
					 (org-tags-match-list-sublevels 'indented)))))
	      ("f" "Upcoming week and future deadlines"
	       ((org-super-agenda-mode)
		(agenda "next week"
			((org-agenda-span 8)
			 (org-agenda-start-on-weekday nil)
			 (org-agenda-time-grid nil)
			 (org-agenda-overriding-header "Next week")
			 (org-deadline-warning-days 0)
			 (org-agenda-skip-deadline-prewarning-if-scheduled t)
			 (org-agenda-skip-scheduled-delay-if-deadline t)
			 ;; (org-agenda-skip-scheduled-if-deadline-is-shown t)
			 ))
		(alltodo "Important picks"
			 ((org-agenda-overriding-header "")
			  (org-agenda-todo-ignore-scheduled t)
			  (org-super-agenda-groups
			   `((:name "Important Tasks" :priority>="A")
			     (:discard (:anything t))))))
		(agenda "" ((org-super-agenda-groups `((:discard (:not (:deadline (after ,target-date))))
						       ;; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org#concrete-dates
						       (:name "PERSONAL 2-year deadlines"
							      :tag ("PERSONAL" "@home") :order 22)
						       (:name "WORK 2-year deadlines"
							      :tag "WORK" :order 1)))
			    (org-agenda-span 'day)
			    ;; (org-agenda-start-day "+7d")  ;; future deadlines are not shown
			    (org-agenda-overriding-header "All 2-year deadlines")
			    (org-agenda-show-all-dates nil)
			    (org-deadline-warning-days 730))))
	       ((org-agenda-compact-blocks t)))
	      ("w" "Action list excluding PERSONAL"
	       ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
			    (org-agenda-span 2)))
		(tags-todo "-CANCELLED/NEXT"
			   ((org-agenda-overriding-header "Next Tasks:")
			    (org-agenda-sorting-strategy '(habit-up category-keep priority-down))
			    (org-tags-match-list-sublevels 'indented)))
		(tags "REFILE"
		      ((org-agenda-overriding-header "Tasks to Refile")
		       (org-agenda-todo-ignore-scheduled nil)
		       (org-agenda-todo-ignore-deadlines nil)
		       (org-tags-match-list-sublevels nil)))
		(tags "-NOTE-REFILE-ARCHIVE/DONE|CANCELLED"
		      ((org-agenda-overriding-header "Tasks to Archive")
		       (org-agenda-todo-ignore-scheduled nil)
		       (org-agenda-todo-ignore-deadlines nil)
		       ))
		(tags-todo "-proj-HOLD-CANCELLED-REFILE-STYLE=\"habit\"/!-NEXT-WAITING-HOLD-CANCELLED"
			   ((org-agenda-overriding-header "Standalone Tasks")
			    (org-tags-match-list-sublevels 'indented)
			    (org-agenda-sorting-strategy '(habit-up category-keep priority-down))))
		(tags-todo "-proj/!WAITING"
			   ((org-agenda-overriding-header "Standalone Waiting Tasks")
			    (org-tags-match-list-sublevels 'indented)
			    (org-agenda-sorting-strategy '(category-keep)))))
	       ((org-agenda-tag-filter-preset '("-PERSONAL"))
		(org-agenda-todo-ignore-scheduled t)
		(org-agenda-todo-ignore-deadlines t)
		))
	      ("W" "Daily review"
	       ((org-super-agenda-mode)
		(agenda "" ((org-agenda-overriding-header "Today's Schedule:")
			    (org-agenda-span 2)
			    (org-super-agenda-groups
			     '((:name "Personal"
				      :tag ("PERSONAL" "@home")
				      :order 22)
			       (:name "Overdue"
				      :deadline past)
			       (:name "Due today"
				      :deadline (today past))
			       (:name "Today"
				      :time-grid t
				      :order 1)))
			    ))
		(tags-todo "*" ((org-agenda-overriding-header "")
				(org-super-agenda-groups
				 '((:name "Next tasks"
					  :todo "NEXT"
					  :order 1)
				   (:name "Tasks to archive"
					  ;; :todo "DONE"
					  :tag "CANCELLED"
					  :todo "CANCELLED")
				   (:name "Tasks to refile"
					  :tag "REFILE")
				   (:name "Waiting standalone tasks"
					  :tag "WAITING")
				   ;; (:discard (:tag ("NOTE" "ARCHIVE")))
				   (:discard (:tag "proj"))
				   ;; (:discard (:todo ""))
				   ))))
		(tags "-NOTE-REFILE-ARCHIVE/DONE|CANCELLED"
		      ((org-agenda-overriding-header "Tasks to Archive")))
		))
	      ("p" "Action list only PERSONAL"
	       ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
			    (org-agenda-span 'day)))
		(tags-todo "+PERSONAL-CANCELLED/NEXT"
			   ((org-agenda-overriding-header "Next Tasks:")
			    (org-agenda-sorting-strategy '(habit-up category-keep priority-down))
			    (org-tags-match-list-sublevels 'indented)))
		(tags "REFILE"
		      ((org-agenda-overriding-header "Tasks to Refile")
		       (org-tags-match-list-sublevels nil)))
		(tags "-proj-NOTE-REFILE-ARCHIVE/DONE|CANCELLED"
		      ((org-agenda-overriding-header "Tasks to Archive")))
		(tags-todo "-proj-HOLD-CANCELLED-REFILE-STYLE=\"habit\"/!-NEXT-WAITING-HOLD-CANCELLED"
			   ((org-agenda-overriding-header "Standalone Tasks")
			    (org-tags-match-list-sublevels 'indented)
			    (org-agenda-sorting-strategy '(category-keep))))
		(tags-todo "-proj/!WAITING"
			   ((org-agenda-overriding-header "Standalone Waiting Tasks")
			    (org-tags-match-list-sublevels 'indented)
			    (org-agenda-sorting-strategy '(habit-up category-keep priority-down)))))
	       ((org-agenda-tag-filter-preset '("+PERSONAL"))
		(org-agenda-todo-ignore-scheduled t)
		(org-agenda-todo-ignore-deadlines t)
		))

	      ("J" "proJects"
	       ((org-super-agenda-mode)
		(tags "-HOLD-CANCELLED+proj/-DONE"
		      (
		       (org-agenda-overriding-header "")
		       (org-super-agenda-groups
			'((:auto-category t
					  )))
		       ))))

	      ("j" "proJects"
	       ((tags "-HOLD-CANCELLED+proj"
		      ((org-agenda-overriding-header "active projects")
		       (org-use-tag-inheritance nil)  ;; only headlines tagged:proj
		       (org-tags-match-list-sublevels 'indented)
		       (org-agenda-skip-function '(org-agenda-skip-subtree-if 'nottodo '("NEXT")))
		       (org-agenda-sorting-strategy '(category-keep))))
		(stuck ""
		       ((org-agenda-overriding-header "stuck projects")
			(org-use-tag-inheritance nil)
			(org-agenda-sorting-strategy '(category-keep))))
		(tags-todo "+proj/!NEXT"
			   ((org-agenda-overriding-header "next project tasks")
			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":proj:"))
			    (org-tags-match-list-sublevels 'indented)
			    (org-agenda-sorting-strategy '(category-keep))))
		(tags-todo "+proj/!WAITING"
			   ((org-agenda-overriding-header "waiting project tasks")
			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":proj:"))
			    (org-tags-match-list-sublevels 'indented)
			    (org-agenda-sorting-strategy '(category-keep))))
		(tags-todo "+proj/!TODO"
			   ((org-agenda-overriding-header "todo project tasks")
			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":proj:"))
			    (org-tags-match-list-sublevels 'indented)
			    (org-agenda-sorting-strategy '(category-keep))))
		(tags "+proj/-WAITING-TODO-NEXT"
		      ((org-agenda-overriding-header "other project headings")
		       (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":proj:"))
		       (org-tags-match-list-sublevels 'indented)
		       (org-agenda-sorting-strategy '(category-keep))))
		)
	       (
		;; ;; (org-agenda-tag-filter-preset '("-linux" "+PERSONAL"))
		;; ;; (org-agenda-start-with-log-mode t)
		;; ;; (org-agenda-log-mode-items 'clock)
		;; ;; (org-agenda-todo-ignore-deadlines 'near)
		;; (org-agenda-todo-ignore-scheduled t)
		;; (org-agenda-todo-ignore-deadlines t)

		(ps-number-of-columns 2)
		(ps-landscape-mode t)
		(ps-print-color-p 'black-white)
		)
	       ("~/theagenda.pdf")
	       )
	      ("i" "Idea and hold, maybe, someday tasks and-or projects"
	       ((tags "+idea"
		      ((org-agenda-overriding-header "Ideas")
		       (org-tags-match-list-sublevels 'indented)
		       (org-agenda-sorting-strategy '(category-keep priority-down))))
		(tags-todo "HOLD|MAYBE"
			   ((org-agenda-overriding-header "HOLD tags")
			    (org-tags-match-list-sublevels 'indented)
			    (org-agenda-sorting-strategy '(category-keep))))))
	      ("c" . "Contexts")
	      ("ce" "@errand" tags-todo "@errand")
	      ("cf" "@fbk" tags-todo "@fbk")
	      ("ch" "@home" tags-todo "@home")
      ("ct" "@telephone" tags-todo "@telephone")
	      ("C" "Contexts"
	       ((tags "@office")
		(tags "@home")
		(tags "@dati")
		(tags "@internet")
		(tags "@phone")
		(tags "@email")
		(tags "@errands"))
	       ((ps-number-of-columns 2)
		(ps-landscape-mode t)
		(ps-print-color-p 'black-white)
		(htmlize-output-type 'css))
	       ("~/context-lists.pdf" "~/context-lists.html"))

	      ("P" "Printed agenda"
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
    ;; Enable auto clock resolution for finding open clocks
    (use-package org-clock
      :ensure org-plus-contrib
      ;; :commands org-clock-persistence-insinuate
      :config
      (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running)))
    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)
    ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)
    ;; global Effort estimate values  ;http://doc.norang.ca/org-mode.html
    ;; global STYLE property values for completion
    (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 6:00 0:00")
					("STYLE_ALL" . "habit"))))

    ;; (require 'my-gtd)
    (setq org-confirm-babel-evaluate nil) ; don't prompt to confirm evaluation every time
    (use-package ox-latex
      :ensure org-plus-contrib
      :config
      (setq org-latex-pdf-process
	    '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f")))
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
				   ;; (sh . t) FIXME:
				   (shell . t) ;; works only with the provided 'org layer
				   (jupyter . t) ;should be the last one
				   ))
    (use-package ob-ditaa
      :ensure org-plus-contrib
      :config
      (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"))
    (declare-function org-babel-jupyter-override-src-block "jupyter")
    (org-babel-jupyter-override-src-block "python") ; Jupyter overrides Python
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
    (setq org-src-fontify-natively t	 ; font in src blocks
	  org-src-preserve-indentation t ; indentation in src blocks
	  org-src-tab-acts-natively t)	 ; tab in src blocks
    )
  (use-package org-bullets
    :general (lead0-def
	       "tb" #'org-bullets-mode
	       "tB" #'org-cycle-list-bullet)
    :hook (org-mode . org-bullets-mode)
    :after org)
  (use-package jupyter
    :after (org)
    :defines (org-babel-default-header-args:jupyter-python
	      ob-async-no-async-languages-alist)
    :config
    (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
							 (:session . "py01")
							 (:kernel . "python3")))
    (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia"))
    ;; (setq jupyter-org-define-key ", h"  'jupyter-current-server)
  ;; (use-package ob-ipython
  ;;   ;;in virtualenv install importmagic epc FIXME
  ;;   :after (org)
  ;;   :config ;;   (add-to-list 'company-backends 'company-ob-ipython))
    )
  (use-package plantuml-mode
    :defines org-plantuml-jar-path
    :config
    (setq plantuml-default-exec-mode 'jar
	  plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
	  org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"))
  (use-package graphviz-dot-mode
    :config (setq graphviz-dot-indent-width 4)
    (add-to-list 'company-backends 'company-graphviz-dot-backend))
  (use-package ox-koma-letter
    :ensure org-plus-contrib
    :init
    (eval-after-load 'ox '(require 'ox-koma-letter))
    ;; :config
    ;; ;;   "xelatex -interaction nonstopmode %f"
    ;; ;; or as suggested by org-ref
    ;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
    ;; (require 'ox-latex)
    ;; (add-to-list 'org-latex-classes
    ;;              '("koma-article"
    ;;                "\\documentclass{scrartcl}
    ;;                 \\usepackage{microtype}
    ;;                 \\usepackage{tgtermes}
    ;;                 \\usepackage[scale=.9]{tgheros}
    ;;                 \\usepackage{tgcursor}
    ;;                 \\usepackage{paralist}
    ;;                 \\newcommand{\\rc}{$^{14}C$}"
    ;;                ("\\section{%s}" . "\\section*{%s}")
    ;;                ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    ;; (require 'ox-beamer)
    )
  (use-package gnuplot)
  )
(progn					; writing
  (lead0-def :infix "x"
    "l"  'langtool-check
    "L"  'langtool-correct-buffer
    "x"  'langtool-check-done
    "n"  'wordnut-lookup-current-word
    "N"  'wordnut-search
    "s"  'sdcv-search-pointer
    "c"  'dictcc-at-point
    "d"  'dictionary-search
    "D"  'goldendict-dwim
    "p"  'powerthesaurus-lookup-word-at-point
    "P"  'powerthesaurus-lookup-word
    "g"  'writegood-mode
    "Gl" 'writegood-grade-level
    "Gr" 'writegood-reading-ease
    "a"  'artbollocks-mode
    "Ar" 'artbollocks-reading-ease
    "AR" 'artbollocks-readability-index
    "Al" 'artbollocks-grade-level
    "Aw" 'artbollocks-word-count
    "As" 'artbollocks-sentence-count
    "i"  'academic-phrases-by-section
    "I"  'academic-phrases
    "t"  'google-translate-smooth-translate)
  (use-package langtool
    :commands langtool-details-error-message
    :general (general-define-key :states 'normal
				 "[a" 'langtool-goto-previous-error
				 "]a" 'langtool-goto-next-error)
    :config
    (use-package popup
      :commands (popup-tip)
      :config
    (defun langtool-autoshow-detail-popup (overlays)
      (when (require 'popup nil t)
	;; Do not interrupt current popup
	(unless (or popup-instances
		    ;; suppress popup after type `C-g` .
		    (memq last-command '(keyboard-quit)))
	  (let ((msg (langtool-details-error-message overlays)))
	    (popup-tip msg)))))
    )
    (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"
	  langtool-java-bin "/usr/bin/java"
	  langtool-mother-tongue "it"
	  langtool-autoshow-message-function 'langtool-autoshow-detail-popup
	  langtool-disabled-rules '("WHITESPACE_RULE"
				    "EN_UNPAIRED_BRACKETS"
				    "COMMA_PARENTHESIS_WHITESPACE"
				    "EN_QUOTES")))
  (use-package wordnut
    :config
    (add-hook 'wordnut-mode-hook 'evil-emacs-state))
  (use-package sdcv
    :general (general-define-key :states 'emacs
				 :keymaps 'sdcv-mode-map
				 "j"  'sdcv-next-dictionary
				 "k"  'sdcv-previous-dictionary)
    :config
    (add-hook 'sdcv-mode-hook 'evil-emacs-state))
  (use-package dictcc
    :config (setq dictcc-destination-lang "it"
		  dictcc-source-lang "en"))
  (use-package dictionary
    :config
    (add-hook 'dictionary-mode-hook 'evil-emacs-state))
  (use-package synosaurus)
  (use-package powerthesaurus)
  (use-package writegood-mode)
  (use-package artbollocks-mode)
  (use-package academic-phrases)
  (use-package goldendict)
  (use-package google-translate
    :defines google-translate-translation-directions-alist
    :init
    (setq google-translate-translation-directions-alist
	  '(("it" . "en") ("en" . "it") ("it" . "de") ("it" . "fr"))
	  google-translate-output-destination kill-ring
	  google-translate-enable-ido-completion t
	  google-translate-show-phonetic t
	  ;; google-translate-listen-program
	  google-translate-pop-up-buffer-set-focus t))
  (use-package ox-reveal
    :after org
    :custom
    (org-reveal-root "/home/dan/.pandoc/reveal.js"))
  (use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
	   ("\\.md\\'" . markdown-mode)
	   ("\\.mkd\\'" . markdown-mode)
	   ("\\.markdown\\'" . markdown-mode))
    ;; :init (setq markdown-command "multimarkdown")
    )
  (use-package cm-mode			;critic markup
    ;; :hook (text-mode . cm-mode)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; ;; Using vacuous schema [2 times]			  ;;
    ;; ;; Can't guess python-indent-offset, using defaults: 4	  ;;
    ;; ;; File mode specification error: (args-out-of-range 0 69) ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    :general (lead0-def
	       "xM" #'cm-mode
	       "xm" #'(cm-prefix-map :which-key "Criticmarkup")))
  )
(use-package pdf-tools
  :general (general-define-key :keymaps 'pdf-view-mode-map
			       "C-s" 'isearch-forward
			       "/" 'pdf-isearch-occur)
  :init (pdf-loader-install)
  :defines pdf-misc-print-programm
  :config
  (setq pdf-misc-print-programm "/usr/bin/gtklp"))
(progn					; Biblio
  (add-hook 'bibtex-mode-hook 'hs-minor-mode)
  (general-define-key :keymaps 'bibtex-mode-map
		      :states 'normal
		      "TAB" 'evil-toggle-fold
		      "C-j" 'org-ref-bibtex-next-entry
		      "C-k" 'org-ref-bibtex-previous-entry)
  (use-package ivy-bibtex
    :general (lead0-def
	       "Bh" #'helm-bibtex
	       "Bb" #'ivy-bibtex))
  (use-package gscholar-bibtex)
  (use-package org-ref
    ;; :defines (bibtex-completion-find-additional-pdfs
    ;; 	      bibtex-completion-pdf-extension)
    :commands (;;org-ref-pdf-to-bibtex  did not activate org-ref; so using a hook see below
	       org-ref-open-in-browser
               org-ref-open-bibtex-notes
               org-ref-open-bibtex-pdf
               org-ref-bibtex-hydra/body
               org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
               org-ref-sort-bibtex-entry
               arxiv-add-bibtex-entry
               arxiv-get-pdf-add-bibtex-entry
               doi-utils-add-bibtex-entry-from-doi
               isbn-to-bibtex
               pubmed-insert-bibtex-from-pmid)
    :general
    (general-define-key :keymaps 'bibtex-mode-map
			:states 'normal
			:prefix ","
			"j" 'org-ref-bibtex-next-entry
			"k" 'org-ref-bibtex-previous-entry
			;; Open
			"b" 'org-ref-open-in-browser
			"n" 'org-ref-open-bibtex-notes
			"p" 'org-ref-open-bibtex-pdf
			;; Misc
			"h" 'org-ref-bibtex-hydra/body
			"i" 'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
			"s" 'org-ref-sort-bibtex-entry
			;; Lookup utilities
			"la" 'arxiv-add-bibtex-entry
			"lA" 'arxiv-get-pdf-add-bibtex-entry
			"ld" 'doi-utils-add-bibtex-entry-from-doi
			"li" 'isbn-to-bibtex
			"lp" 'pubmed-insert-bibtex-from-pmid)
    (general-define-key :keymaps 'org-mode-map
			:states 'normal
			:prefix ","
			"ic" 'org-ref-helm-insert-cite-link)
    (general-define-key :keymaps 'markdown-mode-map
			:states 'normal
			:prefix ","
			"ic" 'org-ref-helm-insert-cite-link)
    (general-define-key :keymaps 'pdf-view-mode-map
    			:states 'emacs
    			"t" 'org-ref-pdf-to-bibtex)
    :init
    (add-hook 'org-mode-hook (lambda () (require 'org-ref)))
    (add-hook 'pdf-view-mode-hook (lambda () (require 'org-ref)))
    (add-hook 'bibtex-mode-hook (lambda () (require 'org-ref)(require 'helm-bibtex)))
    (setq bibtex-completion-bibliography '("~/Sync/biblio/MY/MY.org" "~/Sync/biblio/biblio.org")
  	  bibtex-completion-notes-path "~/Sync/biblio/biblio.org"
  	  bibtex-completion-library-path '("~/Sync/biblio/pdfs/"
  					   "~/Sync/biblio/MY/"
  					   "~/Sync/biblio/books/"))
    (setq org-ref-default-bibliography '("~/Sync/biblio/biblio.bib" "~/Sync/biblio/MY/MY.bib")
	  org-ref-bibliography-files '("~/Sync/biblio/MY/MY.bib" "~/Sync/biblio/biblio.bib")
	  ;; trailing / affects ,hA associate pdf to entry
	  org-ref-pdf-directory "~/Sync/biblio/pdfs/"
	  org-ref-bibliography-notes "~/Sync/biblio/biblio.org"
	  reftex-default-bibliography '("~/Sync/biblio/biblio.bib")
	  ;; Bibtex key format
	  bibtex-autokey-name-case-convert-function 'capitalize
	  bibtex-autokey-name-year-separator ""
	  ;; bibtex-autokey-year-length 2
	  ;; bibtex-autokey-year-title-separator "_"
	  bibtex-autokey-titleword-separator ""
	  bibtex-autokey-titlewords 3
	  bibtex-autokey-titleword-case-convert-function 'capitalize
	  bibtex-autokey-titleword-length 5
	  ;; Notes template, compatible with interleave
	  org-ref-note-title-format
	  "** %k; %t\n \ :PROPERTIES:\n \  :Custom_ID: %k\n \  :INTERLEAVE_PDF: \
             ./pdfs/%k.pdf\n \ :END:\n"
	  ;; helm-bibtex-notes-template-one-file
	  bibtex-completion-notes-template-one-file
	  "** ${=key=}; ${title}\n \ :PROPERTIES:\n \  :Custom_ID: ${=key=}\n \  :INTERLEAVE_PDF: \
             ./pdfs/${=key=}.pdf\n \ :END:\ncite:${=key=}\n"
	  bibtex-completion-additional-search-fields '(tags keywords) ; search also in tags and keywords fields
	  bibtex-completion-pdf-field "file" ; Zotero
	  bibtex-completion-find-additional-pdfs t ; find also additional pdfs
	  ;; works only from helm-bibtex. less common e.g. ".md" can go into file={...}
	  bibtex-completion-pdf-extension '(".pdf" ".avi" ".ppt" ".odp" ".odt" ".doc" ".docx")))
  (use-package helm-bibtex
    :after org-ref			; It is required by org-ref
    :commands (bibtex-completion-open-pdf
	       helm-marked-candidates
	       helm-add-action-to-source
	       bibtex-completion-open-pdf-external)
    :config
    ;; rifle for helm (can open avi, ppt ...)
    (defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
      (let ((bibtex-completion-pdf-open-function
	     (lambda (fpath) (start-process "rifle" "*helm-bibtex-external*" "/usr/bin/rifle" (expand-file-name fpath)))))
	(bibtex-completion-open-pdf keys fallback-action)))
    (helm-bibtex-helmify-action bibtex-completion-open-pdf-external helm-bibtex-open-pdf-external)
    (helm-add-action-to-source "Open PDF using rifle" 'helm-bibtex-open-pdf-external helm-source-bibtex 2))
  (use-package org-noter
    :general ("C-c n" #'org-noter)
    :config
    (setq org-noter-property-doc-file "INTERLEAVE_PDF"
	  org-noter-property-note-location "INTERLEAVE_PAGE_NOTE"
	  org-noter-default-notes-file-names '("noter-othernotes.org" "biblio.org")
	  org-noter-notes-search-path '("~/Sync/biblio")
	  org-noter-always-create-frame nil  ;; do not create a new frame
	  ;; org-noter-doc-property-in-notes t
	  org-noter-doc-split-fraction '(0.7 . 0.5)
	  org-noter-separate-notes-from-heading t)
    (define-key org-noter-notes-mode-map (kbd "C-M-k") 'org-noter-create-skeleton)
    (define-key org-noter-doc-mode-map (kbd "C-M-k") 'org-noter-create-skeleton))
  )
(progn 					; magit
  (lead0-def
    "g"   '(:ignore t :which-key "Git")
    "gcc" #'counsel-git-checkout
    "gcg" #'counsel-git-grep		; find string in git project
    "gcl" #'counsel-git-log
    "gcs" #'counsel-git-stash
    "gs"  '(magit :which-key "Status")
    "gl"  '(magit-todos-list :which-key "todo List")
    "gg"  '(bar-browse :which-key "Browse at remote")
    "gm"  '(git-messenger:popup-message :which-key "Message popup")
    "gf"  '(magit-find-file-other-window :which-key "Find file")
    "gt"  '(git-timemachine :which-key "Time machine")
    "gT"  '(git-timemachine-toggle :which-key "Time machine")
    "gx" 'magit-checkout
    "ge" 'magit-ediff-resolve)
  (use-package magit
    :init
    (setq magit-repository-directories '(("/home/dan/workspace/" . 4)
					 ("~/Sync" . 9)))
    :config
    (add-hook 'git-commit-mode-hook 'evil-insert-state)
    (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))
  ;; (use-package evil-magit)
  ;; (require 'evil-magit)
  (use-package hl-todo
    :general (lead1-def
	       :infix "t"
	       "p" #'hl-todo-previous
	       "n" #'hl-todo-next
	       "o" #'hl-todo-occur
	       "i" #'hl-todo-insert)
    :init
    (global-hl-todo-mode)
    :config
    (add-to-list 'hl-todo-keyword-faces '("XXX" . "#cc9393"))
    )
  (use-package magit-todos :after magit)
  (use-package magit-annex)
  (use-package gitignore-mode)
  (use-package gitconfig-mode)
  (use-package gitattributes-mode)
  (use-package gitignore-templates)
  (use-package browse-at-remote)
  (use-package git-messenger)
  (use-package git-timemachine)
  (use-package git-gutter
    :diminish ""
    :init (global-git-gutter-mode +1))
  )
(progn					; projectile
  (use-package projectile
    :general (lead0-def
	       "p" '(projectile-command-map :which-key "Projectile map"))
    :init
    (projectile-mode)
    :config
    ;; (setq projectile-project-search-path '("~/workspace"
    ;; 					   "~/Sync"
    ;; 					   "/home/examples"
    ;; 					   "~/workspace/arte")) ;; slow down 0.5s
    (setq projectile-completion-system 'ivy))
  (use-package counsel-projectile
    :after (projectile)
    :init
    (counsel-projectile-mode)
    )
  (use-package org-projectile
    :after (projectile)
    :commands (org-projectile-project-todo-entry)
    :general
    (nmap "SPC p n" '(org-projectile-project-todo-completing-read :which-key "New project TODO"))
    :config
    (setq org-projectile-projects-file "~/Sync/box/org/TODOs.org")
    ;; org-agenda-files (append org-agenda-files (org-projectile-todo-files)) ;also in refile targets in my-gtd
    (push (org-projectile-project-todo-entry) org-capture-templates)))

(progn					; python
  (use-package smartparens
    :init
    ;; Always start smartparens mode in js-mode.
    (add-hook 'prog-mode-hook #'smartparens-mode)
    :config
    (add-hook 'inferior-python-mode-hook 'smartparens-mode)
    )
  ;; (use-package elpy
  ;;   :init
  ;;   (advice-add 'python-mode :before 'elpy-enable)
  ;;   :config
  ;;   ;; Enable Flycheck
  ;;   (when (require 'flycheck nil t)
  ;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;;     (add-hook 'elpy-mode-hook 'flycheck-mode))
    ;; Use IPython for REPL

    (use-package python
      :load-path "/usr/share/emacs/"
      :config
      (setq python-shell-interpreter "jupyter"
	    python-shell-interpreter-args "console --simple-prompt"
	    python-shell-prompt-detect-failure-warning nil))
    ;; (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  ;; )
  (use-package company-jedi
    :init
    (defun my/python-mode-hook ()
      (jedi:setup)
      (add-to-list 'company-backends 'company-jedi))
    (add-hook 'python-mode-hook 'my/python-mode-hook))
;; (use-package jedi
;;   :init
;;   (add-to-list 'company-backends 'company-jedi)
;;   :config
;;   (use-package company-jedi
;;     :init
;;     (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
;;     (setq company-jedi-python-bin "python")))
;; (use-package anaconda-mode
;;   :init (add-hook 'python-mode-hook 'anaconda-mode)
;;         (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;;   :config (use-package company-anaconda
;;             :init (add-hook 'python-mode-hook 'anaconda-mode)
;;             (eval-after-load "company"
;;               '(add-to-list 'company-backends '(company-anaconda :with company-capf)))))

  ;; (use-package lsp-mode)
  ;; (use-package lsp-python-ms
  ;;   :hook (python-mode . (lambda ()
  ;; 			   (require 'lsp-python-ms)
  ;; 			   (lsp))))  ; or lsp-deferred
  ;;; TODO:  XXX: http://wikemacs.org/wiki/Python
  ;; (use-package eglot)

  (use-package yapfify
    :init
    (add-hook 'python-mode-hook 'yapf-mode)
    (general-define-key :keymaps 'python-mode-map
    			:states 'normal
    			:prefix "C-c"
			"=" '(yapfify-buffer :which-key "format buffer"))
    (general-define-key :keymaps 'python-mode-map
    			:states 'visual
    			:prefix "C-c"
			"=" '(yapfify-region :which-key "format region")))
  (use-package expand-region
    :general
    ("C-=" '(er/expand-region :which-key "expand region")))
  (use-package pyvenv
    :commands (pyvenv-activate
	       pyvenv-workon)
    :init
    (general-define-key :keymaps 'python-mode-map
			:states 'normal
			:prefix ","
          "va" 'pyvenv-activate
          "vd" 'pyvenv-deactivate
          "vw" 'pyvenv-workon))
  (use-package python-docstring)
  (use-package pytest
    :commands (pytest-one
               pytest-pdb-one
               pytest-all
               pytest-pdb-all
               pytest-module
               pytest-pdb-module)
    :init
    (general-define-key :keymaps 'python-mode-map
			:states 'normal
			:prefix ","
    "tA" '(pytest-pdb-all :which-key "test all debug")
    "ta" '(pytest-all :which-key "test all")
    "tD" '(pytest-pdb-directory :which-key "test directory debug")
    "td" '(pytest-directory :which-key "test directory")
    "tM" '(pytest-pdb-module :which-key "test module debug")
    "tm" '(pytest-module :which-key "test module")
    "tT" '(pytest-pdb-one :which-key "test one debug")
    "tt" '(pytest-one :which-key "test one")
    "tq" '(pytest-failed :which-key "quit on first failed")
    "tr" '(pytest-run :which-key "run py.test")
    )
    :config (add-to-list 'pytest-project-root-files "setup.cfg")
    )
  (use-package pip-requirements
   )
  )
(use-package zeal-at-point
  :defer t
  :general (lead0-def
            "hz" 'zeal-at-point
            "hZ" 'zeal-at-point-set-docset)
  :config
  (add-to-list 'zeal-at-point-mode-alist
	       '(python-mode . ("python3"
				"pandas"
				"numpy"
				"matplotlib"
				"scipy"))))
(use-package ess)
;; (with-eval-after-load 'elfeed (require 'elfeed-config))
;; (with-eval-after-load 'bibtex (require 'bibtex-config))
(use-package emojify)
(use-package slack
  :defer t
  :general
  (progn
    (lead0-def
      "as" #'(:ignore t :which-key "Slack")
      "ass" #'slack-start
      "asj" #'slack-channel-select
      "asg" #'slack-group-select
      "ask" #'slack-select-rooms
      "asd" #'slack-im-select
      "asq" #'slack-ws-close)
    (general-define-key
     :keymaps '(slack-message-buffer-mode-map
		slack-search-result-buffer-mode-map
		slack-mode-map)
     "C-j" #'slack-buffer-goto-next-message
     "C-k" #'slack-buffer-goto-prev-message)
    (lead1-def
      :keymaps '(slack-message-buffer-mode-map
		 slack-pinned-items-buffer-mode-map
		 slack-edit-message-mode-map
		 slack-mode-map)
      "j" #'slack-channel-select
      "g" #'slack-group-select
      "k" #'slack-select-rooms
      "d" #'slack-im-select
      "ra" 'slack-message-add-reaction
      "rr" 'slack-message-remove-reaction
      "rs" 'slack-message-show-reaction-users
      "pl" 'slack-room-pins-list
      "pa" 'slack-message-pins-add
      "pr" 'slack-message-pins-remove
      "mm" 'slack-message-write-another-buffer
      "me" 'slack-message-edit
      "md" 'slack-message-delete
      "t" 'slack-thread-show-or-create
      "q" 'slack-ws-close
      "@" 'slack-message-embed-mention
      "#" 'slack-message-embed-channel
      )
    (general-def 'insert 'slack-mode-map
      "@" 'slack-message-embed-mention
      "#" 'slack-message-embed-channel))
  :commands (slack-start)
  :init
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

(use-package imenu-anywhere
  :general ("C-<" #'imenu-anywhere))

(use-package typo
  :general (lead0-def
	     "tT" #'typo-global-mode)
  :hook
  (text-mode . typo-mode)
  )
;; ,,,(use-package undo-fu)

(setq debug-on-error nil)
(setq debug-on-quit nil)
(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))

;; ;; Garbage collector - decrease threshold to 5 MB
;; (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
(setq gc-cons-threshold (* 32 1024 1024)
      gc-cons-percentage 0.1
      garbage-collection-messages nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zeal-at-point yasnippet-snippets yapfify writegood-mode wordnut which-key wc-mode visual-fill-column use-package-ensure-system-package synosaurus smex smartparens slack sdcv ranger pyvenv python-docstring pytest powerthesaurus plantuml-mode pip-requirements paradox ox-reveal org-super-agenda org-ref org-projectile org-plus-contrib org-noter org-gcal org-bullets olivetti ob-async mu4e-maildirs-extension mu4e-jump-to-list moe-theme markdown-mode magit-todos magit-annex langtool jupyter ivy-yasnippet ivy-rich ivy-bibtex imenu-list highlight-indent-guides guess-language gscholar-bibtex graphviz-dot-mode google-translate goldendict gnuplot gitignore-templates gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter general flyspell-correct-ivy flycheck fantom-theme expand-region evil-numbers evil-nerd-commenter evil ess doom-themes diminish dictionary dictcc deft counsel-projectile company-statistics company-quickhelp company-ngram company-jedi cm-mode calfw-org calfw browse-at-remote base16-theme avy artbollocks-mode academic-phrases))))

(provide 'init)
;;; init.el ends here
