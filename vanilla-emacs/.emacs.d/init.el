;;; package --- Summary my configuration init.el
;;; Commentary:
;; https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; https://blog.jft.rocks/emacs/emacs-from-scratch.html
;; https://gist.github.com/huytd/6b785bdaeb595401d69adc7797e5c22c for lsp-mode
;;;  new important ref https://sriramkswamy.github.io/dotemacs/#orgheadline278
;;; Code:
(setq debug-on-error t)
(setq debug-on-quit t)
(defconst emacs-start-time (current-time))
(progn                                  ; Base UI
  (setq ad-redefinition-action 'accept) ; to silent a defadvice warning in
                                        ; pcre2el (required by magit-todos)
  (setq-default cursor-in-non-selected-windows nil
                cursor-type '(bar . 3)
                echo-keystrokes 0.1
                enable-recursive-minibuffers t
                fill-column 76
                gc-cons-threshold 2000000 ; (* 500 1024 1024)
                indent-tabs-mode nil
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
  (setq-default buffer-file-coding-system 'utf-8-auto-unix)
  (setq coding-system-for-read 'utf-8   ; use utf-8 by default to read
        coding-system-for-write 'utf-8  ; use utf-8 by default to write
        x-stretch-cursor t)
  (scroll-bar-mode 0)
  (tool-bar-mode   0)
  (tooltip-mode    0)
  (menu-bar-mode   0)
  (show-paren-mode 1)                   ; highlight parenthesis
  (save-place-mode)                     ; remember last position in file
  (blink-cursor-mode 0)                 ; Don't blink the cursor
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; (global-hl-line-mode)
  ;; TODO: custom tramp gdb
  (put 'narrow-to-region 'disabled nil)	; narrow to region =C-x n n=
  )
(progn                                  ; Package configuration
  (require 'package)
  ;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
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
    (package-refresh-contents)      ; updage packages archive
    (package-install 'use-package)) ; install the most recent version
  ;; Configure `use-package' prior to loading it.
  (eval-and-compile
    (setq use-package-always-ensure t)
    (setq use-package-verbose t)
    ;; (setq use-package-always-defer nil)
    ;; (setq use-package-always-demand nil)
    ;; (setq use-package-expand-minimally nil)
    (setq use-package-hook-name-suffix nil)
    (setq use-package-enable-imenu-support t))
  ;; This is only needed once, near the top of the file
  (eval-when-compile
    (require 'use-package))
  (require 'use-package-ensure)
  ;; (defvar is-defer (if (getenv "EMACS_DEFER") t))
  ;; mar2020 1.7 vs 6.7 s defer all vs none
  (if (daemonp)
      (setq use-package-always-demand t)
    (setq use-package-always-defer t))
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
    :init (setq bookmark-save-flag t
                bookmark-default-file "~/Sync/.emacs/bookmarks"))
  (use-package ediff                    ; Fix diff behavior
    :config
    (use-package ediff-wind
      :ensure nil
      ;; :load-path "/usr/share/emacs/"
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
  (use-package f :demand t)
  (declare-function f-expand "f")
  (use-package files
    :ensure nil
    ;; :load-path "/usr/share/emacs"
    :init
    (setq auto-save-default nil	       ; top creating #autosave# files
          ;; auto-save-default nil		; CHECKME stop creating #autosave# files
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
          vc-display-status nil
          vc-follow-symlinks t	      ; follow symlinks without asking
          version-control t)
    :config
    (advice-add 'revert-buffer :filter-args (lambda (&rest _rest) (list nil t)))
    :bind
    ("<f14> R" . revert-buffer)
    :hook
    ;; (before-save . whitespace-cleanup) ; not compatible with deft
    (after-save-hook . executable-make-buffer-file-executable-if-script-p)
    ;; rend les scripts executable par défault si c'est UN script.
    ;;    (lambda () (executable-make-buffer-file-executable-if-script-p)))
    )
  (use-package simple
    :ensure nil
    ;; :load-path "/usr/share/emacs"
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
    (electric-indent-mode 0))
  (use-package aggressive-indent
    :bind
    ("<f14> t i" . aggressive-indent-mode)
    :hook
    ((emacs-lisp-mode-hook . aggressive-indent-mode)
     (html-mode-hook . aggressive-indent-mode)))
  (use-package which-key                ; needed here by which-key replacements
    :diminish
    :init (which-key-mode 1)
    :config (setq which-key-idle-delay 0.05))
  (use-package visual-fill-column
    :commands visual-fill-column-adjust ; although is a function
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
      (wc-mode))
    (defun no-distraction-disable ()
      "Switch off from no distraction env"
      (interactive)
      (visual-fill-column-mode -1)
      (text-scale-set 0)
      (wc-mode -1))
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
    :ensure nil
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
            ("\\*\\(Flycheck\\|Flymake\\|Package-Lint\\|vc-git :\\).*"
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
           (custom-mode-hook . visual-line-mode))
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
           ("H-=" . balance-windows-area)
           ("H-m" . prot/window-single-toggle)
           ("H-s" . window-toggle-side-windows)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package emacs                    ;window split more consistent? ;;
    :config                                                            ;;
    (setq window-divider-default-right-width 1)                        ;;
    (setq window-divider-default-bottom-width 1)                       ;;
    (setq window-divider-default-places 'right-only)                   ;;
    :hook (after-init-hook . window-divider-mode))                     ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )
(progn                                  ; single packages
  (use-package key-chord
    :config
    (key-chord-mode 1))
  (use-package avy                      ; Move around
    :init
    (defvar mk-avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s) "Home row Dvorak keys.")
    ;; (set-default avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
    :bind
    ("<f14> m l" . avy-goto-line)
    ("<f14> m c" . avy-goto-char)
    ("<f14> m w" . avy-goto-word-1)
    :init
    (which-key-add-key-based-replacements "<f14> m" "Move to"))
  (use-package ace-window
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
          aw-scope 'frame ;'global
          aw-dispatch-always t)
    (use-package transpose-frame)
    :init
    (key-chord-define-global "ww" #'ace-window)
    :bind
    ("H-w" . ace-window))
  (use-package imenu-list               ; F9
    :bind
    ("<f9>" . imenu-list)
    ("<C-f9>" . imenu-list-smart-toggle))
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
           ("<f14> e c" . flycheck-clear)
           ("<f14> e f" . counsel-flycheck)
           ("<f14> e h" . flycheck-describe-checker)
           ("<f14> e n" . flycheck-next-error)
           ("<f14> e N" . flycheck-previous-error)
           ("<f14> e p" . flycheck-previous-error)
           ("<f14> e s" . flycheck-select-checker)
           ("<f14> e S" . flycheck-set-checker-executable)
           ("<f14> e v" . flycheck-verify-setup)
           ("<f14> e y" . flycheck-copy-errors-as-kill)
           ("<f14> e x" . flycheck-explain-error-at-point))
    :init
    (which-key-add-key-based-replacements "<f14> e" "Errors check")
    :config
    (setq-default
     flycheck-emacs-lisp-initialize-packages t
     flycheck-emacs-lisp-load-path 'inherit
     flycheck-temp-prefix ".flycheck")
    :hook (
           (gitignore-mode-hook . flycheck-mode)
           (markdown-mode-hook . flycheck-mode)
           (prog-mode-hook . flycheck-mode)
           (yaml-mode-hook . flycheck-mode))
    :custom-face
    (flycheck-fringe-error ((t (:background "#6C3333" :weight bold)))))
  (use-package flycheck-color-mode-line
    :after (flycheck)
    :hook
    ((flycheck-mode-hook . flycheck-color-mode-line-mode)))
  (use-package smart-mode-line
    :demand t
    :config
    (let ((sml/no-confirm-load-theme t))
      (sml/setup)))
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
    :init
    ;; Always start smartparens mode in js-mode.
    (add-hook 'prog-mode-hook #'smartparens-mode)
    (setq sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil)
    :config
    ;; (add-hook 'inferior-python-mode-hook 'smartparens-mode)
    (smartparens-global-mode 1)
    (advice-add 'sp-add-to-previous-sexp :after (lambda () (just-one-space)))
    (advice-add 'sp-add-to-previous-sexp :after (lambda () (sp-forward-sexp)))
    (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-cabal-mode)
    (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-mode)
    :bind (:map smartparens-mode-map
                ("<C-backspace>" . sp-backward-kill-sexp)
                ("H-b" . sp-backward-sexp)
                ("H-d" . sp-kill-sexp)
                ("H-f" . sp-forward-sexp)
                ("H-h" . sp-select-next-thing)
                ("H-k" . sp-kill-hybrid-sexp)
                ("H-t" . sp-add-to-previous-sexp)))
  (use-package hl-todo
    :bind ("<f14> 2" . hydra-2DO/body)
    :init
    (global-hl-todo-mode)
    (defhydra hydra-2DO (:color pink :hint nil)
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
          ("<backtab>" . hs-hide-all)
          ("<H-tab>" . hs-show-all))
    :hook
    (prog-mode-hook . hs-minor-mode))
  (use-package calc
    :bind
    ("<f14> a c" . calc))
  )
(progn                                  ; Themes, Fonts and mode-line
  (use-package face-remap               ; Fonts
    :init
    (set-face-attribute 'default nil :family "IBM Plex mono" :height 107 :weight 'normal :width 'normal)
    (set-face-attribute 'fixed-pitch nil :family "IBM Plex mono" :height 107 :weight 'normal :width 'normal)
    (set-face-attribute 'variable-pitch nil :family "IBM Plex Sans" :height 130 :weight 'normal :width 'normal)
    :config
    (setq text-scale-mode-step 1.05))
  (use-package fantom-theme)
  (use-package doom-themes)
  (use-package moe-theme)
  (use-package faff-theme)
  (use-package modus-operandi-theme)
  (use-package modus-vivendi-theme)
  (use-package poet-theme :defer t)
  (use-package base16-theme
    ;; 'base16-solarized-light 'base16-tomorrow 'base16-default-dark 'base16-gruvbox-light-medium
    ;; 'base16-cupertino 'base16-woodland base16-unikitty-dark 'base16-materia
    :config
    (load-theme 'base16-gruvbox-dark-hard t))
  (use-package cyphejor                 ; minor modes modeline
    :init
    (setq
     cyphejor-rules
     '(:upcase
       ("bookmark"    "→")
       ("buffer"      "β")
       ("diff"        "Δ")
       ("dired"       "δ")
       ("emacs"       "ε")
       ("eshell"      "εsh")
       ("inferior"    "i" :prefix)
       ("interaction" "i" :prefix)
       ("interactive" "i" :prefix)
       ("lisp"        "λ" :postfix)
       ("menu"        "▤" :postfix)
       ("mode"        "")
       ("package"     "↓")
       ("python"      "π")
       ("shell"       "sh" :postfix)
       ("text"        "ξ")
       ("wdired"      "↯δ")))
    (cyphejor-mode 1))
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
  (use-package mk-utils
    :demand t
    :load-path "~/.emacs.d/mk/")
  (use-package mk-text                  ; TODO: composable or objed
    ;;https://github.com/paldepind/composable.el
    ;;https://github.com/clemera/objed
    :load-path "~/.emacs.d/mk/"
    :demand t
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
    :after (mk-text mk-utils)
    :bind (("<menu>" . modalka-mode)
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
           ("<f14> t d" . toggle-debug-on-error)
           ("<f14> t l" . display-line-numbers-mode)
           ("<f14> t o" . org-mode)
           ("<f14> t t" . text-mode)
           ("<f14> t v" . variable-pitch-mode)
           ("<f14> t w" . whitespace-mode)
           ("<f14> a d" . dired)
           ("<f14> a k" . paradox-list-packages)
           ("<f14> <tab>" . switch-to-prev-buffer)
           ("<f14> C-<tab>" . switch-to-next-buffer)
           ("<f14> Q" . save-buffers-kill-emacs)
           ("<f14> B" . ibuffer-list-buffers)
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
    :hook ((compilation-mode-hook . modalka-mode)
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
           (text-mode-hook . mk-modalka-mode-no-git-commit)
           (yaml-mode-hook . modalka-mode)
           (ztree-mode-hook . modalka-mode))
    :init
    (which-key-add-key-based-replacements "<f14> t" "Toggle")
    (which-key-add-key-based-replacements "<f14> <tab>" "prev-buffer")
    (which-key-add-key-based-replacements "<f14> C-<tab>" "next-buffer")
    (setq-default modalka-cursor-type '(hbar . 3))
    (set-cursor-color "red")
    :config
    (defun mk-modalka-mode-no-git-commit ()
      "Enable ‘modalka-mode’ unless get edit git commit message."
      (unless (string-equal (buffer-name) "COMMIT_EDITMSG")
        (modalka-mode 1)))

    (defun mk-open-default-dir ()
      "Open default directory."
      (interactive)
      (find-file default-directory))

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
  (use-package evil-numbers
    :bind
    ("H-a" . evil-numbers/inc-at-pt)
    ("H-x" . evil-numbers/dec-at-pt))
  )
(progn                                  ; Ivy Counsel and Swiper
  (use-package smex                     ; Remember past actions
    :bind ("<f14> <f14>" . smex-major-mode-commands))
  (use-package counsel
    :bind (
           ("M-x" . counsel-M-x)
           ("<f14> o t" . counsel-load-theme)
           ("<f14> b" . counsel-switch-buffer)
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
           ("<f14> / /" . counsel-recoll)
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
    :functions (ivy-add-actions
                ivy-format-function-line)
    :bind ("<f14> r" . ivy-resume)
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
    :init
    (ivy-rich-mode 1))
  (use-package swiper
    :bind
    ("C-s" . swiper)
    ("C-S-s" . swiper-isearch-thing-at-point))
  )
(progn                                  ; Company completion
  (use-package company
    :diminish (company-mode . " ⓐ")
    :defines company-ispell-dictionary
    :commands (company-complete-common-or-cycle)
    :preface (defun toggle-company-ispell ()
               (interactive)
               (cond
                ((memq 'company-ispell company-backends)
                 (setq company-backends (delete 'company-ispell company-backends))
                 (message "company-ispell disabled"))
                (t
                 (add-to-list 'company-backends 'company-ispell)
                 (message "company-ispell enabled!"))))
    :bind (
           ("<f14> t N" . (lambda () (interactive) (company-ngram-init)))
           ("<f14> t S" . toggle-company-ispell) ;; to complete words
           :map company-active-map
           ("C-n" . company-select-next-or-abort)
           ("C-p" . company-select-previous-or-abort)
           ("<tab>" . company-select-next-if-tooltip-visible-or-complete-selection)
           ("<backtab>" . (lambda () (interactive "") (company-complete-common-or-cycle -1)))
           ("C-/" . company-search-candidates)
           ("C-f" . company-filter-candidates)
           ("C-h" . company-show-doc-buffer)
           ("C-s" . counsel-company))
    :init
    (which-key-add-key-based-replacements "<f14> t N" "Ngram")
    (setq tab-always-indent 'complete)
    (global-company-mode)
    :config
    (setq company-tooltip-align-annotations t
          company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-show-numbers t
          ;; company-require-match nil;company-require-match 'never
          ))
  (use-package company-quickhelp
    :after (company)
    :init (company-quickhelp-mode)
    :bind
    ("<M-f1>" . company-quickhelp-mode))
  (use-package company-ngram
    :after company
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
    :after (company)			;works like :hook
    :init (company-statistics-mode))
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
    (use-package yasnippet-snippets)
    (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (add-to-list 'yas-snippet-dirs "~/Sync/.emacs/yasnippets")
    ;; (yas-reload-all)
    (setq yas-triggers-in-field t
          yas-wrap-around-region t)     ;or [a-z] register
    ;; https://github.com/syl20bnr/spacemacs/pull/179
    ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names/10520
    (setq company-backends
          (mapcar #'company-mode/backend-with-yas company-backends))
    )
  (use-package ivy-yasnippet
    :after (yasnippet)
    :bind
    ("<f14> i s" . ivy-yasnippet)
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
                 '(("^#+BEGIN_SRC" . "^#+END_SRC")
                   ("^From:" . "line--$"))))
  (use-package flyspell
    :hook
    (text-mode-hook . flyspell-mode)
    (prog-mode-hook . flyspell-prog-mode)
    (change-log-mode-hook . (lambda () (flyspell-mode -1)))
    (log-edit-mode-hook . (lambda () (flyspell-mode -1))))
  (use-package flyspell-lazy
    :after (flyspell)
    :init
    (setq-default
     flyspell-lazy-disallow-buffers nil
     flyspell-lazy-idle-seconds 1)
    :config
    (flyspell-lazy-mode 1))
  (use-package flyspell-correct-ivy	; play better with darkroom and the like
    :after flyspell
    :bind
    (:map flyspell-mode-map
          ("C-." . flyspell-correct-at-point))
    :init
    (setq flyspell-issue-message-flag nil)
    (setq flyspell-correct-interface #'flyspell-correct-ivy))
  (use-package guess-language
    ;; Its advantage is multi language in the same doc.
    :diminish (guess-language-mode . "gL")
    :bind
    ("<f14> t g" . guess-language-mode)
    ("<f14> t s" . flyspell-mode) ; flyspell-correct-auto-mode
    ("<f14> t H-s" . flyspell-correct-auto-mode)
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
(progn                                  ; calendars
  (use-package calendar
    :hook
    (calendar-today-visible-hook . calendar-mark-today))
  (use-package solar                    ; (2) sunrise and sunset
    :ensure nil
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
    (setq org-gcal-token-file "~/Sync/.emacs/org-gcal/.org-gcal-token"
          org-gcal-client-id "1086004898054-uhp29b0kek41obv1dma52rpog8pr44gu.apps.googleusercontent.com"
          org-gcal-client-secret "sP2Jupy5GKtdDAAgupQrSzc2"
          org-gcal-file-alist '(("danielepietroarosio@gmail.com" .
                                 "~/Sync/box/org/gcal/dpa.org")
                                ("c87gevr5pc3191on8c7nh8b4nc@group.calendar.google.com" .
                                 "~/Sync/box/org/gcal/figli.org")
                                ("tq1af7efj4l9h8glgqi2g5vmsg@group.calendar.google.com" .
                                 "~/Sync/box/org/gcal/IBF.org"))))
  ;; (use-package calfw
  ;;   :bind
  ;;   ("<f14> o g W" . cfw:open-calendar-buffer))
  (use-package calfw-org
    :bind
    (("<f14> o g w" . cfw:open-org-calendar)
     :map org-agenda-mode-map
     ("W" . cfw:open-org-calendar)))
  )
(progn                                  ; mu4e
  ;; TODO: email in org html
  ;; https://vxlabs.com/2015/01/28/sending-emails-with-math-and-source-code/
  ;; ;; http://pragmaticemacs.com/emacs/email-templates-in-mu4e-with-yasnippet/
  ;; (add-hook 'mu4e-compose-mode-hook 'spacemacs/load-yasnippet)
  (when (fboundp 'imagemagick-register-types) (imagemagick-register-types))
  (use-package mu4e
    :ensure nil
    ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
    :commands
    (mu4e-compose-new
     mu4e-context-current)
    :hook
    (mu4e-view-mode-hook . variable-pitch-mode)
    ;; (mu4e-headers-mode . variable-pitch-mode)
    :bind
    (("C-x m" . mu4e-compose-new)
     ("<f14> a m" . mu4e)
     :map mu4e-compose-mode-map
     ("<f9> t" . message-goto-to)
     ("<f9> m" . message-goto-body)
     ("<f9> b" . message-goto-bcc)
     ("<f9> c" . message-goto-cc)
     ("<f9> s" . message-goto-subject)
     :map mu4e-view-mode-map
     ("f" . mu4e-headers-mark-for-flag)
     ("<tab>"     . org-next-link)	; 'shr-next-link
     ("<backtab>" . org-previous-link)	; 'shr-previous-link
     ("G"         . end-of-buffer)
     ("V"         . mu4e-view-verify-msg-popup)
     ("v"         . visual-fill-column-mode)
     :map mu4e-headers-mode-map
     ("G"         . end-of-buffer)
     ("f"         . mu4e-headers-mark-for-flag))
    :init
    (set-default mail-user-agent 'mu4e-user-agent)
    ;; (defun my-mu4e () ; trying to have themes bound to major modes
    ;;   (interactive)
    ;;   (let ((color-theme-is-global nil))
    ;;     (select-frame (make-frame))
    ;;     (color-theme-tango)
    ;;     (mu4e)))
    :config
    (use-package mu4e-utils
      :ensure nil
      ;; :load-path "/usr/share/emacs/"
      :functions (mu4e-error
                  mu4e-message
                  mu4e-get-headers-buffer))
    (use-package mu4e-headers
      :ensure nil
      ;; :load-path "/usr/share/emacs/"
      :functions (mu4e-headers-mark-and-next
                  mu4e~headers-goto-docid))
    (use-package mu4e-view
      :ensure nil
      ;; :load-path "/usr/share/emacs/"
      :functions (mu4e~view-get-attach))
    (use-package mu4e-actions
      :ensure nil
      ;; :load-path "/usr/share/emacs/"
      :functions (mu4e~write-body-to-html))
    (use-package message
      :ensure nil
      ;; :load-path "/usr/share/emacs/"
      :functions (message-sendmail-envelope-from
                  message-add-header
                  message-remove-header))
    (use-package org-mu4e
      :ensure nil
      ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
      :bind (:map mu4e-compose-mode-map
                  ("<f9> O" . org~mu4e-mime-switch-headers-or-body))
      :config
      ;;store link to message/query if in header view (t/nil)
      (setq mu4e-org-link-query-in-headers-mode t))
    (progn                              ; config
      (setq mu4e-confirm-quit nil
            mu4e-update-interval 120
            mu4e-headers-leave-behavior 'apply ; leaving headers view apply all marks
            mu4e-headers-visible-lines 10
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
        (define-key mu4e-headers-mode-map (kbd "z") 'mu4e-headers-mark-for-tag)
        (define-key mu4e-headers-mode-map (kbd "P") 'mu4e-headers-mark-for-personal)
        (define-key mu4e-view-mode-map (kbd "z") 'mu4e-view-mark-for-tag)
        (define-key mu4e-view-mode-map (kbd "P") 'mu4e-view-mark-for-personal)
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
    (progn                              ; Composing and Sending
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
  (use-package mu4e-jump-to-list
    :after mu4e)
  (use-package mu4e-maildirs-extension
    :after mu4e                         ; very slow to load
    :defines mu4e-maildirs-extension-default-collapse-level
    :bind (:map mu4e-main-mode-map
                ("<tab>" . mu4e-maildirs-extension-toggle-maildir-at-point))
    :init
    (mu4e-maildirs-extension-load)
    :config
    ;; mu4e-maildirs-extension-maildir-collapsed-prefix "archives"
    (setq mu4e-maildirs-extension-default-collapse-level 0))
  ;; (use-package mu4e-conversation
  ;;   :after mu4e
  ;;   :config (global-mu4e-conversation-mode))
  )
(progn                                  ; org
  ;; (use-package beginend)
  (use-package org-capture                                                                                                                                        ;;
    :ensure org-plus-contrib
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
    :hook (org-capture-mode-hook . (lambda () (modalka-mode 0)))
    :config                                                                                                                                                       ;;
    (setq org-default-notes-file "~/Sync/box/org/inbox.org"
          org-capture-templates
          '(("b"
             "Basic note to refile later" entry (file org-default-notes-file)
             "* %? %:subject\n%U\n%a\n")
            ("c"
             "Capture concise actionable item and exit" entry (file org-default-notes-file)
             "* TODO %^{Title}\n %U\n %i %l" :immediate-finish t)
            ("T" "Task of importance with a tag, deadline, and further editable space" entry
             (file+headline "tasks.org" "Task list with a date")
             "* %^{Scope of task||TODO [#A]|STUDY [#A]|MEET with} %^{Title} %^g\n DEADLINE: %^t\n :PROPERTIES:\n :CONTEXT: %a\n :CAPTURED: %U\n :END:\n\n %i %?")
            ("E" "Reply to an email" entry
             (file+headline "tasks.org" "Mail correspondence")
             "* TODO [#B] %:subject\n SCHEDULED: %t\n :PROPERTIES:\n :CONTEXT: %a\n :END:\n\n %i %?")
            ("I" "Idea")
            ("Ia" "Activity or event" entry
             (file+headline "i2deas.org" "Activities or events")
             "* ACT %^{Act about what}%? :private:\n :PROPERTIES:\n :CAPTURED: %U\n :END:\n\n %i")
            ("Ie" "Essay or publication" entry
             (file+headline "i2deas.org" "Essays or publications")
             "* STUDY %^{Expound on which thesis}%? :private:\n :PROPERTIES:\n :CAPTURED: %U\n :END:\n\n %i")
            ("Iv" "Video blog or screen cast" entry
             (file+headline "i2deas.org" "Screen casts or vlogs")
             "* RECORD %^{Record on what topic}%? :private:\n :PROPERTIES:\n :CAPTURED: %U\n :END:\n\n %i")
            
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
            ;; journal.org
            ("j" "Journal" entry (file+olp+datetree "~/Sync/box/org/journal.org") "* %?\n%t\n" ) ; prefix C-1 alternative to time-prompt t
            ("m" "Meeting" entry (file+olp+datetree "~/Sync/box/org/journal.org") "* MEETING %? :MEETING:\n%T" :clock-in t :clock-resume t)
            ;; reading
            ("R" "to read" entry (file+headline da-gtd "Reading")
             "* TODO Read %a \n%U\n" :unnarrowed t :kill-buffer t)
            ;; spesa.org
            ("s" "Spesa" entry (file+headline "~/Sync/box/org/spesa.org" "Supermarket") ;; TODO: try checkitem
             "* TODO %? \n")
            ("rd" "Review: Daily" entry (file+olp+datetree "/tmp/daily-reviews.org")
             (file "~/Sync/.emacs/templates/my_dailyreviewtemplate.org"))
            ("rw" "Review: Weekly Review" entry (file+olp+datetree "/tmp/weekly-reviews.org")
             (file "~/Sync/.emacs/templates/my_weeklyreviewtemplate.org"))
            
            ("r" "Reply to" entry (file+headline da-gtd "Reply")
             "* TODO %a to %:from \nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n %?" :unnarrowed t)
            ("w" "Wait for Reply" entry (file+headline da-gtd "Reply")
             "* WAITING %a from %:from" :immediate-finish t))
          org-capture-templates-contexts '(("r" ((in-mode . "mu4e-view-mode")))
                                           ("w" ((in-mode . "mu4e-view-mode"))))
          )
    :bind (("C-c c" . org-capture)
           ("C-c r d" . nemacs-org-capture-review-daily)
           ("C-c r w" . my-new-weekly-review))
    )
  (use-package org
    :ensure org-plus-contrib
    :commands
    (org-capture-finalize
     org-speed-move-safe
     org-narrow-to-subtree
     org-clock-in)
    :functions (org-read-date)
    :preface
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
    ;; https://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
    :hook
    ((org-mode-hook . visual-line-mode)
     (org-mode-hook . flyspell-mode)
     (org-mode-hook . variable-pitch-mode)
     (org-mode-hook . my-babelsrc-org-mode-hook))
    :bind
    (
     ("C-c a" . org-agenda)
     ("M-h" . mark-word)
     ("M-S-h" . org-mark-element)
     :map org-mode-map
     ("<f14> c o o" . counsel-outline)
     ("<f14> c o f" . counsel-org-file)
     ("<f14> c o l" . counsel-org-link)
     ("<f14> c o t" . counsel-org-tag))
    :config
    ;; The manual: https://orgmode.org/org.html
    ;; (let*
    ;;     (
    ;;      (variable-tuple '(:font "Overpass"))
    ;;      ;;  (cond ((x-list-fonts "Overpass") '(:font "Overpass"))
    ;;      ;;        ((x-list-fonts "IBM Plex") '(:font "IBM Plex Sans"))))
    ;;      (base-font-color     (face-foreground 'default nil 'default))
    ;;      ;; (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
    ;;      (headline           `(:weight bold)))
    (custom-theme-set-faces
     'user
     ;;    `(org-level-8 ((t (,@headline ,@variable-tuple))))
     ;;    `(org-level-7 ((t (,@headline ,@variable-tuple))))
     ;;    `(org-level-6 ((t (,@headline ,@variable-tuple))))
     ;;    `(org-level-5 ((t (,@headline ,@variable-tuple))))
     ;;    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.04))))
     ;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.08))))
     ;;    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.12))))
     ;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.15))))
     ;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 0.85 :underline nil))))
     ;;    ;; (custom-theme-set-faces
     ;;    ;;  'user
     '(org-block ((t (:inherit fixed-pitch))))
     '(org-code ((t (:inherit (shadow fixed-pitch)))))
     ;;    '(org-document-info ((t (:foreground "dark orange"))))
     ;;    '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
     ;;    '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
     ;;    '(org-link ((t (:foreground "royal blue" :underline t))))
     ;;    '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     ;;    '(org-property-value ((t (:inherit fixed-pitch))) t)
     ;;    '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
     )
    ;;    '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
    ;;    '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

    ;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    ;; (font-lock-add-keywords 'org-mode
    ;;                         '(("^ *\\([-]\\) "
    ;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq org-refile-use-cache t)                 ;;
    (setq org-reverse-note-order nil)   ; default ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (use-package ol
      :ensure org-plus-contrib
      :config
      (setq org-link-keep-stored-after-insertion t)
      :bind (:map org-mode-map
                  ("C-c l" . org-store-link)
                  ("C-c S-l" . org-toggle-link-display)
                  ("C-c C-S-l" . org-insert-last-stored-link)))

    (setq                               ; org buffers with babel
     org-return-follows-link t
     org-hide-emphasis-markers t              ; a better word processor
     ;; org-image-actual-width nil               ; so you can specify :width
     org-image-actual-width (/ (display-pixel-width) 4)
     org-cycle-separator-lines 2              ; default=2
     org-highlight-latex-and-related '(latex) ; Change color of inline latex $y=mx+c$
     org-src-fontify-natively t               ; font in src blocks

     org-src-window-setup 'current-window
     org-src-preserve-indentation t           ; indentation in src blocks
     org-edit-src-content-indentation 0
     org-src-tab-acts-natively t              ; tab in src blocks

     org-confirm-babel-evaluate nil ; don't prompt to confirm evaluation every time
     org-columns-default-format
     "%48ITEM(Task) %TODO(todo) %ALLTAGS %SCHEDULED %6Effort(Effort){:} %6CLOCKSUM{:} %DEADLINE"
     org-M-RET-may-split-line '((default . t)
                                (headline . nil)
                                (item . nil)
                                (table . nil)))
    (use-package jupyter
      :functions org-babel-jupyter-override-src-block
      :defines org-babel-default-header-args:jupyter-python
      :init
      (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                           (:session . "py01")
                                                           (:kernel . "python3")))
      ;; (setq jupyter-org-define-key ", h"  'jupyter-current-server)
      ;; (use-package ob-ipython
      ;;   ;;in virtualenv install importmagic epc FIXME
      ;;   :after (org)
      ;;   :config ;;   (add-to-list 'company-backends 'company-ob-ipython))
      )
    (use-package ob-async
      :config
      (setq
       ob-async-no-async-languages-alist '("jupyter-python" "jupyter-Julia")))
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
                                   (jupyter . t) ;should be the last one
                                   ))
    (org-babel-jupyter-override-src-block "python") ; Overrides python and
                                        ; must be after org-babel-do-load-lang
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
    (use-package ob-ditaa
      :ensure org-plus-contrib
      :config
      (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"))
    (use-package ox-latex
      :ensure org-plus-contrib
      :config
      (setq org-latex-pdf-process
            '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
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
      )
    (use-package ox-beamer
      :ensure org-plus-contrib
      :init
      (eval-after-load 'ox '(require 'ox-beamer)))
    (use-package ox-md
      :ensure org-plus-contrib
      :init (eval-after-load 'ox '(require 'ox-md)))
    (use-package ox-koma-letter
      :ensure org-plus-contrib
      ;; :after ox
      :init
      (eval-after-load 'ox '(require 'ox-koma-letter)))
    (use-package ox-reveal
      :init
      (setq org-reveal-root "/home/dan/.pandoc/reveal.js")
      (eval-after-load 'ox '(require 'ox-reveal)))
    ;; (use-package auctex)
    ;; (use-package cdlatex)
    (add-to-list 'org-structure-template-alist '("sl" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("sb" . "src sh :results output :exports both"))
    (add-to-list 'org-structure-template-alist '("sB" . "src sh :session bash :results output :exports both"))
    (add-to-list 'org-structure-template-alist '("sj" . "src jupyter-python"))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq org-fontify-done-headline t)                                     ;;
    (setq org-fontify-quote-and-verse-blocks t)                            ;;
    (setq org-fontify-whole-heading-line nil)                              ;;
    (setq org-enforce-todo-dependencies t)                                 ;;
    (setq org-enforce-todo-checkbox-dependencies t)                        ;;
    (setq org-track-ordered-property-with-tag t)                           ;;
    ;; general                                                             ;;
    (setq org-special-ctrl-a/e t)                                          ;;
    (setq org-special-ctrl-k t)                                          ;;
    (setq org-hide-emphasis-markers t)                                     ;;
    (setq org-catch-invisible-edits 'show)                                 ;;
    (setq org-loop-over-headlines-in-active-region nil)           ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; init of my-gtd.conf
    ;; defconst (concat da-templates ".org")
    ;; A bare minimum simple starting to personalizing org for gtd.
    ;; https://orgmode.org/worg/org-configs/org-customization-guide.html
    (progn                              ; Define variables
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
      (defvar da-templates "~/.spacemacs.d/templates"))
    (setq                               ; (1) Agenda files
     org-directory "~/Sync/box/org"
     org-agenda-files (append da-agenda-and-refile-files
                              '("~/Sync/box/org/journal.org"
                                "~/Sync/box/org/gcal/"))
     org-agenda-diary-file "~/Sync/box/org/journal.org"
     org-agenda-include-diary t)        ; to display holidays in org-agenda
    (setq                               ; (2) Archives
     org-archive-location "~/Sync/box/org/archives/%s_archive::"
     org-archive-file-header-format "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n")
    (setq                               ; (3) Refile
     org-refile-use-outline-path 'file      ; Full path preceded by filename
     org-outline-path-complete-in-steps nil ; Complete directly with counsel
     org-refile-allow-creating-parent-nodes 'confirm ; Ask confirmation when creating parent tasks
     org-refile-targets '((da-agenda-and-refile-files :maxlevel . 5)))
    (setq                               ; (4) Stuck project
     org-stuck-projects '("+proj/-DONE-HOLD" ("NEXT") nil ""))
    (setq org-todo-keywords
          '((sequence "TODO(t)" "|" "DONE(D)" "CANCELLED(C)")
            ))
    (setq                               ; (5) Todo states
     org-todo-keywords
     '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
       (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (sequence "ACT(a)" "|" "ACTED(A)")               ;;
       (sequence "BUY(b)" "|" "BOUGHT(B)")              ;;
       (sequence "MEET(m)" "|" "MET(M)" "POSTPONED(P)") ;;
       (sequence "STUDY(s)" "|" "STUDIED(S)")           ;;
       (sequence "RECORD(r)" "|" "RECORDED(R)"))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     org-todo-keyword-faces      ; Faces
     '(("TODO" :foreground "red" :weight bold)
       ("NEXT" :foreground "light blue" :weight bold)
       ;; ("APPT" :foreground "yellow" :weight bold)
       ("DONE" :foreground "forest green" :weight bold)
       ("WAITING" :foreground "orange" :weight bold)
       ("HOLD" :foreground "magenta" :weight bold)
       ("CANCELLED" :foreground "forest green" :weight bold)
       ("MEETING" :foreground "forest green" :weight bold))
     org-todo-state-tags-triggers ; Change tags upon state change
     '(("CANCELLED" ("CANCELLED" . t))
       ("WAITING" ("WAITING" . t))
       ("HOLD" ("WAITING") ("HOLD" . t))
       (done ("WAITING") ("HOLD"))
       ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
       ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
       ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))
     org-use-fast-todo-selection t
     org-treat-S-cursor-todo-selection-as-state-change nil ; no log here
     org-log-into-drawer t
     org-log-done 'time
     org-log-note-clock-out nil
     org-log-redeadline nil
     org-log-reschedule nil
     org-read-date-prefer-future 'time)
    (setq                               ; (5) Tags for contexts
     org-tag-persistent-alist '((:startgroup)
                                ("Contexts")
                                (:grouptags) ; mutually exclusive
                                ("@errand" . ?e)
                                ("@fbk" . ?f)
                                ("@home" . ?h)
                                ("@telephone" . ?t)
                                (:endgroup)
                                ("@net" . ?n) ; I doubt it is useful
                                ("PERSONAL" . ?p)
                                ("WORK" . ?w))
     org-tag-alist  '(("@dati" . ?d)
                      ("@mail" . ?m)
                      ("idea" . ?i)
                      ("proj" . ?j))
     org-tag-faces '(("WORK" :foreground "green")
                     ("PERSONAL" :foreground "orange")
                     ("proj" :weight bold)
                     ("@fbk" :weight italic))
     org-fast-tag-selection-single-key t ; 'expert does't show
     org-fast-tag-selection-include-todo nil
     org-tags-column -82
     org-support-shift-select t)


    ;; Enable auto clock resolution for finding open clocks
    (use-package org-clock
      :ensure org-plus-contrib
      :config
      (setq org-clock-out-remove-zero-time-clocks t ; Removes clocked tasks with 0:00 duration
            org-clock-auto-clock-resolution (quote when-no-clock-is-running)))
    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)

    ;; TODO:  global Effort estimate values  ;http://doc.norang.ca/org-mode.html
    ;; global STYLE property values for completion
    (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 6:00 0:00")
                                        ("STYLE_ALL" . "habit"))))
    )
  (use-package org-agenda
    :ensure org-plus-contrib
    :bind (:map org-agenda-mode-map
                ("Z" . counsel-org-tag-agenda))
    :config
    (setq org-agenda-confirm-kill 1)
    
    (setq org-agenda-show-all-dates t)
    (setq org-agenda-show-outline-path nil)
    ;; All the "skip" need to be reviewed
    (setq org-agenda-skip-additional-timestamps-same-entry t)
    
    (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
    (setq org-agenda-skip-scheduled-delay-if-deadline t)
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
    (setq org-agenda-skip-timestamp-if-done t)

    (setq org-agenda-search-headline-for-time nil)
    (setq org-agenda-span 3)
    (setq org-agenda-start-on-weekday 1)  ; Monday
    (setq org-agenda-start-with-follow-mode nil)
    (setq org-agenda-timegrid-use-ampm nil)
    (setq org-agenda-time-grid
          '((daily today require-timed)
            (0800 1000 1200 1400 1600 1800 2000)
            "      " "················"))
    (setq org-agenda-use-time-grid t)
    (setq org-agenda-window-setup 'current-window)
    (setq org-agenda-todo-list-sublevels t)
    (setq                               ; Agenda display
     org-agenda-sticky t
     org-agenda-dim-blocked-tasks t     ; Dim blocked tasks
     org-agenda-show-future-repeats nil ; 'next to view this and the next.
     org-agenda-search-view-always-boolean t ; Lazy boolean search =C-c a s=
     org-agenda-text-search-extra-files `(agenda-archives) ; Search also in archives
     org-deadline-warning-days 7)

    ;; TODO: custom agenda view http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
    ;; https://github.com/mwfogleman/.emacs.d/blob/master/michael.org
    ;; https://gist.github.com/mwfogleman/267b6bc7e512826a2c36cb57f0e3d854

    (use-package org-super-agenda)

    (progn                                          ; == Agenda ==
      ;; For tag searches ignore tasks with scheduled and deadline dates FIXME better control this in each agenda custom view
      (setq org-agenda-tags-todo-honor-ignore-options t) ; needed to
      ;; avoid seeing missed tasks in my unscheduled view; next tasks in
      ;; daily review view; in w but W is ok; 
      ;; all properties are inherited
      ;; (setq org-use-property-inheritance t) ;; TODO:  to be used with STYLE, e.g. habit not scheduled
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
    )
  (use-package org-bullets
    :after (org)
    :bind
    ("<f14> t b" . org-bullets-mode)
    ("<f14> t B" . org-cycle-list-bullet)
    :hook (org-mode-hook . org-bullets-mode))
  (use-package plantuml-mode
    :defines org-plantuml-jar-path
    :config
    (setq plantuml-default-exec-mode 'jar
          plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
          org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"))
  (use-package graphviz-dot-mode
    :config (setq graphviz-dot-indent-width 4)
    (add-to-list 'company-backends 'company-graphviz-dot-backend))
  (use-package gnuplot)                 ; TODO: read docs
  )
(progn                                  ; writing
  ;; XXX: abbrev mode C-x a +/g
  ;; TODO: translate-shell =M-|= shell-command-on region
  (use-package wc-mode
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
  (use-package langtool
    :commands (langtool-goto-previous-error
               langtool-goto-next-error
               langtool-check
               langtool-correct-buffer
               langtool-check-done)
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
    ("<f14> x p" . powerthesaurus-lookup-word-at-point)
    ("<f14> x P" . powerthesaurus-lookup-word)) ; API changed
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
    :bind
    ("<f14> x t" . google-translate-smooth-translate)
    :init
    (setq google-translate-translation-directions-alist
          '(("it" . "en") ("en" . "it") ("it" . "de") ("it" . "fr"))
          google-translate-output-destination kill-ring
          google-translate-enable-ido-completion t
          google-translate-show-phonetic t
          ;; google-translate-listen-program
          google-translate-pop-up-buffer-set-focus t
          ))                            ; API changed
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
(progn                                  ; Biblio
  ;; TODO: ;; https://rebeja.eu/posts/managing-bibliography-using-emacs-org-mode-and-org-ref/
  (use-package bibtex
    :bind (:map bibtex-mode-map
                ("<tab>" . hs-toggle-hiding)
                ("<backtab>" . hs-hide-all)
                ("<H-tab>" . hs-show-all))
    :hook
    (bibtex-mode-hook . hs-minor-mode)
    (bibtex-mode-hook . (lambda () (require 'org-ref)))
    (bibtex-mode-hook . (lambda () (require 'ivy-bibtex)))
    :config
    (setq                               ; Bibtex key format
     bibtex-autokey-name-case-convert-function 'capitalize
     bibtex-autokey-name-year-separator ""
     bibtex-autokey-titleword-separator ""
     bibtex-autokey-titlewords 3
     bibtex-autokey-titleword-case-convert-function 'capitalize
     bibtex-autokey-titleword-length 5))
  (use-package ivy-bibtex
    :bind
    ("<f14> a B" . helm-bibtex)
    ("<f14> a b" . ivy-bibtex)
    :config
    (setq bibtex-completion-bibliography '("~/Sync/biblio/MY/MY.org"
                                           "~/Sync/biblio/biblio.org")
          bibtex-completion-notes-path "~/Sync/biblio/biblio.org"
          bibtex-completion-library-path '("~/Sync/biblio/pdfs/"
                                           "~/Sync/biblio/MY/"
                                           "~/Sync/biblio/books/")
          ;; helm-bibtex-notes-template-one-file
          bibtex-completion-notes-template-one-file
          "** ${=key=}; ${title}\n \ :PROPERTIES:\n \  :Custom_ID: ${=key=}\n \  :INTERLEAVE_PDF: \
             ./pdfs/${=key=}.pdf\n \ :END:\ncite:${=key=}\n"
          bibtex-completion-additional-search-fields '(tags keywords) ; search also in tags and keywords fields
          bibtex-completion-pdf-field "file" ; Zotero
          bibtex-completion-find-additional-pdfs t ; find also additional pdfs
          ;; works only from helm-bibtex. less common e.g. ".md" can go into file={...}
          bibtex-completion-pdf-extension '(".pdf" ".avi" ".ppt" ".odp" ".odt" ".doc" ".docx")))
  (use-package org-ref
    :bind
    (:map bibtex-mode-map               ; modalka-mode-map
          ("SPC h" . org-ref-bibtex-hydra/body)
          ("SPC b" . org-ref-open-in-browser)
          ("SPC n" . org-ref-open-bibtex-notes)
          ("SPC p" . org-ref-open-bibtex-pdf)
          ("SPC h" . org-ref-bibtex-hydra/body) ; misc
          ("SPC i" . org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit)
          ("SPC S" . org-ref-sort-bibtex-entry)
          ("SPC la" . arxiv-add-bibtex-entry) ; lookup utilities
          ("SPC lA" . arxiv-get-pdf-add-bibtex-entry)
          ("SPC ld" . doi-utils-add-bibtex-entry-from-doi)
          ("SPC li" . isbn-to-bibtex)
          ("SPC lp" . pubmed-insert-bibtex-from-pmid))
    :hook
    (org-mode-hook . (lambda () (require 'org-ref)))
    ;; (bibtex-mode-hook (lambda () (require 'org-ref)(require 'helm-bibtex)))
    :init
    (setq org-ref-completion-library 'org-ref-ivy-cite
          org-ref-default-bibliography '("~/Sync/biblio/biblio.bib"
                                         "~/Sync/biblio/MY/MY.bib")
          org-ref-bibliography-files '("~/Sync/biblio/MY/MY.bib"
                                       "~/Sync/biblio/biblio.bib")
          ;; trailing / affects ,hA associate Pdf to entry
          org-ref-pdf-directory "~/Sync/biblio/pdfs/" ;
          org-ref-bibliography-notes "~/Sync/biblio/biblio.org"
          reftex-default-bibliography '("~/Sync/biblio/biblio.bib")
          ;; Notes template, compatible with interleave
          org-ref-note-title-format
          "** %k; %t\n \ :PROPERTIES:\n \  :Custom_ID: %k\n \  :INTERLEAVE_PDF: \
             ./pdfs/%k.pdf\n \ :END:\n"))
  (use-package helm-bibtex
    :functions (bibtex-completion-open-pdf
                helm-marked-candidates
                helm-add-action-to-source
                bibtex-completion-open-pdf-external)
    :config
    (setq bibtex-completion-format-citation-functions
          '((org-mode      . bibtex-completion-format-citation-org-title-link-to-PDF)
            (latex-mode    . bibtex-completion-format-citation-cite)
            (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
            (default       . bibtex-completion-format-citation-default)))
    ;; rifle for helm (can open avi, ppt ...)
    (defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
      (let ((bibtex-completion-pdf-open-function
             (lambda (fpath) (start-process "rifle" "*helm-bibtex-external*" "/usr/bin/rifle" (expand-file-name fpath)))))
        (bibtex-completion-open-pdf keys fallback-action)))
    (helm-bibtex-helmify-action bibtex-completion-open-pdf-external helm-bibtex-open-pdf-external)
    (helm-add-action-to-source "Open PDF using rifle" 'helm-bibtex-open-pdf-external helm-source-bibtex 2))
  (use-package org-noter                ; TODO: bindings here and pdf
    :bind
    ("<f14> n" . org-noter)
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
  (use-package pdf-tools
    :bind (:map pdf-view-mode-map
                ("t" . org-ref-pdf-to-bibtex))
    ;; 			       "C-s" 'isearch-forward
    ;; 			       "/" 'pdf-isearch-occur)
    :hook
    (pdf-view-mode-hook . (lambda () (require 'org-ref)))
    :init (pdf-loader-install)
    :defines pdf-misc-print-programm
    :config
    (setq pdf-misc-print-programm "/usr/bin/gtklp"))
  )
(progn                                  ; Magit
  (use-package magit
    :bind
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
    (setq magit-repository-directories '(("/home/dan/workspace/" . 4)
                                         ("~/Sync" . 9)))
    :config
    (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))
  (use-package magit-todos
    :after magit
    :bind ("<f14> g 2" . magit-todos-list))
  (use-package magit-annex)
  (use-package gitignore-mode)
  (use-package gitconfig-mode)
  (use-package gitattributes-mode)
  (use-package gitignore-templates)
  (use-package browse-at-remote
    :bind ("<f14> g g" . bar-browse))
  (use-package git-messenger
    :bind ("<f14> g m" . git-messenger:popup-message))
  (use-package git-timemachine
    :bind
    ("<f14> g t" . git-timemachine)
    ("<f14> g T" . git-timemachine-toggle))
  (use-package git-gutter
    :diminish " ~"
    :commands (git-gutter:next-hunk
               git-gutter:previous-hunk
               git-gutter:popup-hunk
               git-gutter:stage-hunk
               git-gutter:revert-hunk
               git-gutter:mark-hunk)
    :init
    (global-git-gutter-mode +1)
    (defhydra git-gutter (:color yellow :hint nil)
      "A hydra for git gutter!"
      ("g" magit-status "magit" :color blue)
      ("n" git-gutter:next-hunk "next")
      ("p" git-gutter:previous-hunk "previous")
      ("d" git-gutter:popup-hunk "diff")
      ("s" git-gutter:stage-hunk "stage")
      ("r" git-gutter:revert-hunk "revert")
      ("m" git-gutter:mark-hunk "mark")
      ("q" nil "cancel" :color blue))
    :bind
    ("<f14> g h" . git-gutter/body))
  )
(progn                                  ; projectile
  (use-package projectile
    :bind
    ("<f14> p" . projectile-command-map)
    :diminish
    :init
    (which-key-add-key-based-replacements "<f14> p" "Projectile")
    (projectile-mode)
    :config
    ;; (setq projectile-project-search-path '("~/workspace"
    ;; 					   "~/Sync"
    ;; 					   "/home/examples"
    ;; 					   "~/workspace/arte")) ;; slow down 0.5s
    (setq projectile-completion-system 'ivy))
  (use-package counsel-projectile
    :after (projectile)
    :init (counsel-projectile-mode))
  (use-package org-projectile
    :after (projectile)
    :bind
    ("<f14> p n" . org-projectile-project-todo-completing-read)
    :config
    (setq org-projectile-projects-file "~/Sync/box/org/TODOs.org")
    ;; org-agenda-files (append org-agenda-files (org-projectile-todo-files)) ;also in refile targets in my-gtd
    (declare-function org-projectile-project-todo-entry "org-projectile")
    (push (org-projectile-project-todo-entry) org-capture-templates)))

;; TODO: mrkkrp ...  wdired
(use-package dired
  :ensure nil
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
  ((dired-mode-hook . toggle-truncate-lines)))
(use-package dired-x
  :ensure nil
  ;; :load-path "/usr/share/emacs/"
  :init
  (setq
   dired-clean-up-buffers-too t))
(use-package wdired
  :after (dired)
  :init
  (setq wdired-allow-to-change-permissions t))

(progn   ; python
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
    :bind
    ("<f14> t p" . python-mode)
    :init
    (setq-default
     python-fill-docstring-style 'pep-257-nn
     python-indent-offset 2)
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
  ;;; TODO:  http://wikemacs.org/wiki/Python
  ;; (use-package eglot)

  (use-package yapfify
    :init
    (add-hook 'python-mode-hook 'yapf-mode)
    ;; (general-define-key :keymaps 'python-mode-map
    ;; 			:states 'normal
    ;; 			:prefix "C-c"
    ;; 			"=" '(yapfify-buffer :which-key "format buffer"))
    ;; (general-define-key :keymaps 'python-mode-map
    ;; 			:states 'visual
    ;; 			:prefix "C-c"
    ;; 			"=" '(yapfify-region :which-key "format region"))
    )
  
  (use-package pyvenv
    :commands (pyvenv-activate
               pyvenv-workon)
    :init
    ;; (general-define-key :keymaps 'python-mode-map
    ;; 			:states 'normal
    ;; 			:prefix ","
    ;;       "va" 'pyvenv-activate
    ;;       "vd" 'pyvenv-deactivate
    ;;       "vw" 'pyvenv-workon)
    )
  (use-package python-docstring)
  (use-package pytest
    :commands (pytest-one
               pytest-pdb-one
               pytest-all
               pytest-pdb-all
               pytest-module
               pytest-pdb-module)
    :init
    ;; (general-define-key :keymaps 'python-mode-map
    ;; 			:states 'normal
    ;; 			:prefix ","
    ;; "tA" '(pytest-pdb-all :which-key "test all debug")
    ;; "ta" '(pytest-all :which-key "test all")
    ;; "tD" '(pytest-pdb-directory :which-key "test directory debug")
    ;; "td" '(pytest-directory :which-key "test directory")
    ;; "tM" '(pytest-pdb-module :which-key "test module debug")
    ;; "tm" '(pytest-module :which-key "test module")
    ;; "tT" '(pytest-pdb-one :which-key "test one debug")
    ;; "tt" '(pytest-one :which-key "test one")
    ;; "tq" '(pytest-failed :which-key "quit on first failed")
    ;; "tr" '(pytest-run :which-key "run py.test")
    ;; )
    :config (add-to-list 'pytest-project-root-files "setup.cfg")
    )
  (use-package pip-requirements
    )
  )
(use-package zeal-at-point
  :defer t
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
;; (with-eval-after-load 'elfeed (require 'elfeed-config))
;; (with-eval-after-load 'bibtex (require 'bibtex-config))
(use-package emojify)
;; (use-package use-package-chords
;;   :ensure t
;;   :config (key-chord-mode 1))
(use-package slack
  :defer t                              ; avoid halting daemon startup
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
             slack-ws-close)
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
         ("@" . slack-message-embed-mention)
         ("#" . slack-message-embed-channel))
  ;; :chords (("hh" . hydra-slack/body))
  :init
  (which-key-add-key-based-replacements
    "<f14> a s" "Slack")
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (key-chord-define slack-mode-map ",," #'hydra-slack/body)
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

(use-package eyebrowse
  :diminish eyebrowse-mode
  :init
  (dotimes (n 10)
    (global-unset-key (kbd (format "C-%d" n)))
    (global-unset-key (kbd (format "M-%d" n))))
  (eyebrowse-mode t)
  (setq eyebrowse-switch-back-and-forth t
        eyebrowse-mode-line-separator " ")
  :config (progn
            (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
            (setq eyebrowse-new-workspace t)))
(use-package desktop
  :init
  (setq desktop-restore-eager 15))
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


(setq debug-on-error nil)
(setq debug-on-quit nil)
(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))

;; ;; Garbage collector - decrease threshold to 5 MB
;; (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
;; (setq gc-cons-threshold (* 32 1024 1024)
;;       gc-cons-percentage 0.1
;;       garbage-collection-messages nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-fringe-error ((t (:background "#6C3333" :weight bold))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(slack-all-thread-buffer-thread-header-face ((t (:weight bold :height 1.8)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (mu4e-actions mu4e-view mu4e-headers mu4e-utils transpose-frame exwm-config ob-jupyter auctex cdlatex ox sdcv zeal-at-point yasnippet-snippets yapfify writegood-mode wordnut which-key wc-mode visual-fill-column use-package-ensure-system-package synosaurus smex smartparens slack ranger pyvenv python-docstring pytest powerthesaurus plantuml-mode pip-requirements paradox ox-reveal org-super-agenda org-ref org-projectile org-plus-contrib org-noter org-gcal org-bullets olivetti ob-async mu4e-maildirs-extension mu4e-jump-to-list moe-theme markdown-mode magit-todos magit-annex langtool jupyter ivy-yasnippet ivy-rich ivy-bibtex imenu-list highlight-indent-guides guess-language gscholar-bibtex graphviz-dot-mode google-translate goldendict gnuplot gitignore-templates gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter general flyspell-correct-ivy flycheck fantom-theme expand-region evil-numbers evil-nerd-commenter evil ess doom-themes diminish dictionary dictcc deft counsel-projectile company-statistics company-quickhelp company-ngram company-jedi cm-mode calfw-org calfw browse-at-remote base16-theme avy artbollocks-mode academic-phrases)))
 '(send-mail-function (quote sendmail-send-it)))

(provide 'init)
;;; init.el ends here
