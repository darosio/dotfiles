;;; init.el --- Personal Emacs configuration file -*- lexical-binding: t; -*-
;;
;; Author: DanieleArosio <daniele.arosio@cnr.it>
;; Version: 3.19.0
;;
;; This file contains my personal Emacs configuration.
;;

;;; Commentary:
;; Binding keys reserved to user are: "C-c <letter>" and <F5> to <F9>.
;; C-M-s is Hyper key

;;; Code:

;; --- Startup & Core Emacs Settings ---
;; Disable GUI elements
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; Speed up startup
(setq gc-cons-threshold (* 50 1000 1000)
      ;;frame-inhibit-implied-resize t
      inhibit-splash-screen t
      use-file-dialog nil)
;; Global debug settings (consider making these conditional for normal use)
(setq debug-on-error t
      debug-on-quit t)
;; Set user-emacs-directory to the directory of this file
(setq user-emacs-directory (file-truename (file-name-directory load-file-name)))
;; Define a constant for Emacs start time (good for profiling)
(defconst emacs-start-time (current-time))
;; Configure Emacs as a daemon if applicable
(require 'server)
(defvar is-daemon nil "T if Emacs is running as a daemon.")
(if (daemonp)
    (progn
      (setq server-raise-frame t
            is-daemon t)
      (setenv "EDITOR" "emacsclient -c -a=''")))

(add-to-list 'load-path (expand-file-name "my-config" user-emacs-directory))
;; Set Flymake's load-path to match load-path
(setq elisp-flymake-byte-compile-load-path load-path)

(setenv "GPG_TTY" (or (getenv "GPG_TTY") "/dev/tty")) ;store gpg for gcal

;; --- Package Management (straight.el and use-package) ---
;; Use 'setq-default' instead of custom-set or setq to set variables
(setq-default straight-vc-git-default-clone-depth '1) ;full
(setq-default straight-recipes-gnu-elpa-use-mirror t)

;; *** 1. Straight Bootstrap ***
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; *** 2. Straight Configuration ***
;; Declare straight.el variables to silence Flymake warnings
(eval-when-compile
  (defvar straight-use-package-by-default)
  (defvar straight-cache-autoloads)
  (defvar straight-check-for-modifications)
  (declare-function straight-use-package "straight.el"))
;; Enable straight.el for all use-package calls by default
(setq straight-use-package-by-default t)
;; Enable autoload caching and modification checks for straight.el
(setq straight-cache-autoloads t
      straight-check-for-modifications '(check-on-startup find-when-checking))

;; *** 3. Crucial fix for the recognition of Use-Package Keywords ***
;; = nil t = as arguments means: not generating error if not found, and not recharging if already loaded.
(eval-when-compile
  (require 'use-package nil t))

;; *** 4. Use-package configuration ***
(use-package use-package
  :straight t
  :init
  (setq use-package-always-defer (not is-daemon)
        use-package-compute-statistics t
        use-package-verbose t
        use-package-enable-imenu-support t))

;; *** 5. Load first packages ***
;; ;; Install org built-in via straight to shadow Emacs's version (good practice)
(use-package org :ensure nil :straight (:type built-in))
;; This tells straight.el to always treat org as built-in, even if a package requests it.
(add-to-list 'straight-built-in-pseudo-packages 'org)
;; Prevent using the built-in transient https://github.com/magit/magit/discussions/4997
;; This often gets fixed by `:straight t` which fetches the latest version.
(use-package magit)

(progn                                  ; UI base setting

  (use-package bookmark
    :straight (:type built-in)
    :custom
    (bookmark-default-file "~/.emacs.d/bookmarks")
    (bookmark-save-flag 2))

  (use-package browse-url
    :straight (:type built-in)
    :custom
    (browse-url-browser-function 'browse-url-generic)
    (browse-url-generic-program "firefox"))

  (use-package comint
    :straight (:type built-in)
    :bind ("C-c <tab>" . comint-dynamic-complete-filename))

  (use-package ediff
    :straight (:type built-in)
    :custom
    (ediff-window-setup-function 'ediff-setup-windows-plain)
    (ediff-split-window-function (if (> (frame-width) 150)
                                     'split-window-horizontally
                                   'split-window-vertically))
    (ediff-diff-options "-w"))

  (use-package emacs
    :preface
    ;; Go to change fonts
    (defun mk-set-font (font &optional height)
      "Set font FONT as main font for all frames.
      HEIGHT, if supplied, specifies height of letters to use."
      (interactive
       (list (completing-read "Use font: " (font-family-list)) nil))
      (set-face-attribute 'default nil :family font)
      (when height
        (set-face-attribute 'default nil :height height))
      (set-face-attribute 'variable-pitch nil :family font))
    ;; http://xahlee.info/emacs/emacs/emacs_toggle_line_spacing.html
    (defun xah-toggle-line-spacing ()
      "Toggle line spacing between no extra space to extra half line height."
      (interactive)
      (setq-local line-spacing (if line-spacing nil 0.5))
      (redraw-frame))
    :init
    (which-key-add-key-based-replacements "C-c f" "Files")
    (which-key-add-key-based-replacements "C-c l" "Libraries")
    (which-key-add-key-based-replacements "C-c t" "Toggle")
    :config
    (setq-default blink-cursor-mode 0       ; Don't blink the cursor
                  buffer-file-coding-system 'utf-8-auto
                  cursor-in-non-selected-windows nil
                  cursor-type '(bar . 3)
                  echo-keystrokes 0.1
                  enable-recursive-minibuffers t ; Enable recursive minibuffers
                  fill-column 80
                  font-lock-maximum-decoration t
                  global-mark-ring-max 1024
                  image-use-external-converter t ; 27.1 viewer don't display many png
                  indent-tabs-mode nil  ; use spaces instead of tabs for indentation
                  indicate-empty-lines t
                  mark-ring-max 64
                  read-process-output-max (* 1024 1024)
                  resize-mini-windows t
                  ring-bell-function 'ignore
                  save-interprogram-paste-before-kill t
                  save-place-mode t     ; remember last position in file
                  scroll-margin 3
                  scroll-step 1
                  select-enable-clipboard t
                  sentence-end-double-space nil
                  show-paren-delay 0.05
                  tab-always-indent 'complete
                  tab-width 4
                  truncate-lines t
                  x-stretch-cursor t
                  yank-pop-change-selection t)
    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (tooltip-mode 0)
    (show-paren-mode 1)         ; highlight parenthesis
    (global-hl-line-mode 1)
    (put 'narrow-to-region 'disabled nil) ; narrow to region =C-x n n=
    (fset 'yes-or-no-p 'y-or-n-p)
    :bind
    (
     ("C-M-;" . comment-box)
     ("C-c t A" . auto-revert-mode)
     ("C-c t o e" . org-toggle-pretty-entities)
     ("C-c t d" . toggle-debug-on-error)
     ("C-c t l" . display-line-numbers-mode)
     ("C-c t o n" . org-num-mode)
     ("C-c t m c" . conf-mode)
     ("C-c t m o" . org-mode)
     ("C-c t m t" . text-mode)
     ("C-c t v" . variable-pitch-mode)
     ("C-c t w" . whitespace-mode)
     ("C-c t 5" . xah-toggle-line-spacing)
     ("M-g F" . mk-set-font)
     ("C-c Q" . save-buffers-kill-emacs)
     ("C-c l f" . find-library) ;; tmm =M-\`= and Mind mark, bookmark and register
     ("C-c l a" . apropos-library)
     ("C-c l l" . load-library))
    )

  (use-package files
    :straight (:type built-in)
    :preface
    ;; Revert buffer without prompting
    (defun my-revert-buffer (&rest _)
      "Revert buffer without prompting."
      (revert-buffer t t))
    :custom
    ;; Disable automatic saving of buffers
    (auto-save-default nil)
    ;; Save backup files in a temporary directory
    (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
    ;; Copy backup files instead of renaming them
    (copy-backup-files t)
    ;; Store backup files in a temporary directory
    (backup-directory-alist `((".*" . ,temporary-file-directory)))
    ;; Delete old backup files
    (delete-old-versions t)
    ;; Keep the 4 newest versions of a file
    (kept-new-versions 4)
    ;; Keep the 2 oldest versions of a file
    (kept-old-versions 2)
    ;; Warn when opening large files
    (large-file-warning-threshold 10240000)
    ;; Require final newline in files
    (require-final-newline t)
    ;; Display version control status in the mode line
    (vc-display-status t)
    ;; Follow symbolic links in version control operations
    (vc-follow-symlinks t)
    ;; Enable version control for files
    (version-control t)
    :bind
    ("C-x B" . revert-buffer)
    :hook
    ;; After saving make scripts executable
    (after-save . executable-make-buffer-file-executable-if-script-p))

  (use-package isearch
    :straight (:type built-in)
    :config
    (setq isearch-allow-scroll t))

  (use-package simple
    :straight (:type built-in)
    :init
    (setq blink-matching-delay 0.5
          blink-matching-paren 'jump-offscreen
          kill-read-only-ok t
          suggest-key-bindings t)
    ;; :preface
    ;; (defun mk-auto-fill-mode ()
    ;;   "Enable ‘auto-fill-mode’ limiting it to comments."
    ;;   (setq-local comment-auto-fill-only-comments t)
    ;;   (auto-fill-mode 1))
    :config
    (column-number-mode 1)
    :bind
    ("C-z" . undo)
    ("C-c q" . auto-fill-mode)
    ("M-h" . mark-word)
    ("M-S-h" . mark-paragraph)
    ("<f7> c" . count-words)
    ;; :hook
    ;; ((gitignore-mode . mk-auto-fill-mode)
    ;;  (haskell-cabal-mode . mk-auto-fill-mode)
    ;;  (prog-mode . mk-auto-fill-mode)
    ;;  (proof-mode . mk-auto-fill-mode))
    ;; (text-mode . auto-fill-mode)
    ;; (yaml-mode . mk-auto-fill-mode) ;TODO: check and remove
    )

  (use-package window
    :straight (:type built-in)
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
    :config
    ;; Set splitting behavior: horizontal splits preferred over vertical splits
    (setq window-sides-vertical 120) ;nil
    (setq split-height-threshold 100)
    :hook ((help-mode-hook . visual-line-mode)    ;; Wrap lines nicely in help buffers
           (Custom-mode-hook . visual-line-mode)) ;; Wrap lines in customize buffers
    :bind (("C-M-s-n" . next-buffer)
           ("C-M-s-p" . previous-buffer)
           ("C-M-s-o" . other-window)
           ("C-'" . other-window)
           ("C-M-s-'" . window-swap-states)
           ("C-M-s-2" . split-window-below)
           ("C-M-s-3" . split-window-right)
           ("C-M-s-0" . delete-window)
           ("C-M-s-1" . delete-other-windows)
           ("C-M-s-5" . delete-frame)
           ("C-M-s-{" . shrink-window-horizontally)
           ("C-M-s-}" . enlarge-window-horizontally)
           ("C-M-s-[" . shrink-window)
           ("C-M-s-]" . enlarge-window)
           ("C-M-s-=" . balance-windows-area)
           ("C-M-s-m" . prot/window-single-toggle) ;; toggle monocle mode
           ("C-M-s-s" . window-toggle-side-windows)
           ("C-M-s-q" . delete-window)              ;; emulate i3 window manager 'close window'
           ("C-M-s-<up>" . windmove-up)
           ("C-M-s-<left>" . windmove-left)
           ("C-M-s-<down>" . windmove-down)
           ("C-M-s-<right>" . windmove-right)))

  (use-package electric
    :straight (:type built-in)
    :config
    (electric-indent-mode 0)
    ;; python is excluded by aggressive indent because of not absolute indentation
    :hook (python-mode . electric-indent-mode))

  (use-package lpr
    :straight (:type built-in)
    :custom
    (lpr-command "lpr")
    (printer-name "HP_LaserJet_CM1415fn"))
  (use-package ps-print
    :straight (:type built-in)
    :custom
    (ps-print-header nil)
    (ps-print-footer nil)
    (ps-print-color-p t)
    (ps-print-header-frame nil)
    (ps-print-banner nil)
    (ps-print-scale 1.0)
    (ps-print-duplex nil)
    )
  ;; (setq lpr-command "gtklp")
  ;; (setq ps-lpr-command "gtklp")
  ;;            ; printing; need: gv, ghostscript
  ;;   (require 'printing)      ; load printing package
  ;;   ;; (setq pr-path-alist
  ;;   ;;   '((unix      "." "~/bin" ghostview mpage PATH)
  ;;   ;;     (ghostview "$HOME/bin/gsview-dir")
  ;;   ;;     (mpage     "$HOME/bin/mpage-dir")
  ;;   ;;     ))
  ;;   (setq pr-txt-name      'prt_06a)
  ;;   (setq pr-txt-printer-alist
  ;;    '((cc "lpr" nil "cc")
  ;;      (prt_07c nil   nil "prt_07c")
  ;;      ))
  ;;   (setq pr-ps-name       'cc)
  ;;   (setq pr-ps-printer-alist
  ;;    '((cc "lpr" nil "-P" "cc")
  ;;      (lps_07c "lpr" nil nil  "lps_07c")
  ;;      (lps_08c nil   nil nil  "lps_08c")
  ;;      ))
  ;;   (pr-update-menus t)      ; update now printer and utility menus

  ;; Themes and fonts
  (use-package faces
    :straight (:type built-in)
    :config
    (set-face-attribute 'default nil
                        :family "IBM Plex mono"
                        :height 116
                        :weight 'normal
                        :width 'normal)
    (set-face-attribute 'fixed-pitch nil
                        :family "IBM Plex mono"
                        :height 120
                        :weight 'normal
                        :width 'normal)
    (set-face-attribute 'variable-pitch nil
                        :family "IBM Plex Sans"
                        :height 130
                        :weight 'normal
                        :width 'normal))
  (use-package face-remap
    :straight (:type built-in)
    :config
    (setq text-scale-mode-step 1.05))
  (use-package spacemacs-theme
    :defer t
    :init
    (unless (daemonp)
      (load-theme 'spacemacs-light t)))

  (use-package poet-theme
    :defer t) ; Defer loading until theme is explicitly loaded

  (use-package solarized-theme
    :init
    (when (daemonp)
      (load-theme 'solarized-selenized-dark t)))
  )
(progn ;; single packages

  (use-package unfill
    :ensure t
    :bind ("C-c M-q" . unfill-toggle))

  (use-package aggressive-indent
    :bind
    ("C-c t i" . aggressive-indent-mode)
    :hook
    (emacs-lisp-mode . aggressive-indent-mode)
    (html-mode . aggressive-indent-mode))

  (use-package delsel
    :init (delete-selection-mode 1))

  (use-package fix-word
    :bind
    ("M-c" . fix-word-capitalize)
    ("M-l" . fix-word-downcase)
    ("M-u" . fix-word-upcase)
    ("M-C" . capitalize-word)
    ("M-L" . downcase-word)
    ("M-U" . upcase-word))

  (use-package which-key
    :commands (which-key-mode
               which-key-add-key-based-replacements)
    :bind ("C-M-s-<f1>" . which-key-show-top-level)
    :init
    (which-key-mode 1)
    (which-key-add-key-based-replacements "C-c t m" "Toggle mode")
    (which-key-add-key-based-replacements "C-c t o" "Toggle org")
    :config
    (setq which-key-idle-delay 0.05))

  (use-package visual-fill-column
    :commands (visual-fill-column-split-window-sensibly
               visual-fill-column-adjust)
    :defines (visual-fill-column-center-text
              visual-fill-column-width
              visual-fill-column-fringes-outside-margins)
    :bind (("C-c v" . visual-fill-column-mode)
           ("<f12>" . no-distraction-enable)
           ("<C-f12>" . no-distraction-disable))
    :config
    (setq visual-fill-column-center-text t
          visual-fill-column-width 98
          visual-fill-column-fringes-outside-margins nil
          ;; set right curly arrow even when visual line mode is wrapping logical lines into visual ones.
          visual-line-fringe-indicators '(bottom-left-angle top-right-angle)
          ;; allow splitting windows with wide margins
          split-window-preferred-function #'visual-fill-column-split-window-sensibly)
    ;; adjust margins upon text resize
    (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
    :hook
    (visual-fill-column-mode . visual-line-mode)
    :preface
    (defun no-distraction-enable ()
      "Switch to no distraction env"
      (interactive)
      (visual-fill-column-mode)
      (text-scale-increase 2))
    (defun no-distraction-disable ()
      "Switch off from no distraction env"
      (interactive)
      (visual-fill-column-mode -1)
      (text-scale-set 0)))

  (use-package crux
    :bind
    ("M-r" . crux-duplicate-current-line-or-region)
    ("C-c d" . crux-duplicate-current-line-or-region)
    ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
    ("C-a" . crux-move-beginning-of-line)
    ("C-S-j" . crux-top-join-line)
    ;; mk-mark use C-x SPC
    ("C-o" . crux-smart-open-line)
    ("C-S-o" . open-line)
    ("C-k" . crux-smart-kill-line)
    ("C-S-k" . kill-whole-line)
    ("C-$" . (lambda () (interactive) (move-end-of-line 1) (yank)))
    ("C-c s" . sort-lines))

  (use-package move-text
    :demand t
    :commands move-text-default-bindings
    :config
    (move-text-default-bindings))  ;; Binds M-<up>/<down> automatically

  (use-package recentf
    :init (recentf-mode t))

  (use-package transpose-frame
    :bind ("C-M-s-\"" . transpose-frame))

  (use-package imenu-list               ; F9
    :bind (("<f9>" . imenu-list)
           ("<C-f9>" . imenu-list-smart-toggle)))


  (use-package password-store :defer nil)
  (use-package pass
    :bind ("C-x P" . pass))

  (use-package tzc
    :demand t
    :defines tzc-favourite-time-zones
    :bind (("C-x T c" . tzc-convert-current-time)
           ("C-x T t" . tzc-convert-time-at-mark)
           ("C-x T w" . tzc-world-clock))
    :config
    (setq tzc-favourite-time-zones '("Europe/Rome" "America/New_York" "Asia/Tokyo"))) ; Added more for example

  (use-package nerd-icons
    :commands nerd-icons-install-fonts
    :config
    ;; Check for font existence more robustly
    (unless (or (find-font (font-spec :family "Symbols Nerd Font Mono"))
                (file-exists-p "~/.local/share/fonts/NFM.ttf"))
      (nerd-icons-install-fonts))
    :custom
    (nerd-icons-font-family "Symbols Nerd Font Mono"))

  (use-package doom-modeline
    :demand t
    ;; :after nerd-icons
    :commands (doom-modeline-mode)
    :custom (doom-modeline-minor-modes t)
    :config (doom-modeline-mode 1))

  (use-package minions
    :demand t
    :commands minions-mode
    :init (minions-mode 1))

  (use-package smartparens
    :commands smartparens-global-mode
    :defines (sp-no-reindent-after-kill-modes smartparens-mode-map)
    ;; :init
    ;; (setq sp-highlight-pair-overlay nil
    ;;       sp-highlight-wrap-overlay nil
    ;;       sp-highlight-wrap-tag-overlay nil)
    :bind (:map smartparens-mode-map
                ("<C-backspace>" . sp-backward-kill-sexp)
                ("C-M-s-b" . sp-backward-sexp)
                ("C-M-s-d" . sp-kill-sexp)
                ("C-M-s-f" . sp-forward-sexp)
                ("C-M-s-h" . sp-select-next-thing)
                ("C-M-s-k" . sp-kill-hybrid-sexp)
                ("C-M-s-t" . sp-add-to-previous-sexp)
                ("C-M-s-)" . sp-forward-slurp-sexp)
                ("C-M-s-}" . sp-forward-barf-sexp)
                ("C-M-s-(" . sp-backward-slurp-sexp)
                ("C-M-s-{" . sp-backward-barf-sexp))
    :hook
    ((prog-mode inferior-python-mode jupyter-repl-mode) . smartparens-mode)
    :config
    (smartparens-global-mode 1)
    (advice-add 'sp-add-to-previous-sexp :after (lambda () (just-one-space)))
    (advice-add 'sp-add-to-previous-sexp :after (lambda () (sp-forward-sexp)))
    ;; Use `add-to-list` for these
    (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-cabal-mode)
    (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-mode))

  (use-package expand-region
    :bind ("C-=" . er/expand-region))

  (use-package hideshow ;; for folding
    :straight (:type built-in)
    :bind (("C-c t f" . hs-minor-mode)
           (:map prog-mode-map
                 ("M-<tab>" . hs-toggle-hiding)
                 ("C-M-s-z" . hs-hide-all)
                 ("C-M-s-a" . hs-show-all)))
    :config
    (setq hs-hide-comments-when-hiding-all nil)
    :hook (prog-mode . hs-minor-mode))

  (use-package calc
    :bind ("M-g M-a c" . calc))

  (use-package dired
    :straight (:type built-in)
    :functions (dired-get-filename
                dired-next-line
                dired-previous-line)
    :init (setq delete-by-moving-to-trash t
                dired-auto-revert-buffer t
                dired-dwim-target t
                dired-keep-marker-copy nil
                dired-listing-switches "-GAlh --group-directories-first"
                dired-recursive-copies 'always
                dired-recursive-deletes 'always)
    :preface
    (defun mk-dired-open-external (file)
      "Open specified FILE with application determined by the OS."
      (interactive (list (dired-get-filename)))
      (call-process "xdg-open" nil 0 nil file))
    (defun mk-dired-first-file ()
      "Jump to the first file in current directory."
      (interactive)
      (goto-char (point-min))
      (dired-next-line 2))
    (defun mk-dired-last-file ()
      "Jump to the last file in current directory."
      (interactive)
      (goto-char (point-max))
      (dired-previous-line 1))
    :bind (:map
           dired-mode-map
           ("<next>" . mk-dired-last-file)
           ("<prior>" . mk-dired-first-file)
           ("b" . dired-up-directory)
           ("M-<up>" . dired-up-directory)
           ("e" . mk-dired-open-external)))

  (use-package dired-x
    :straight nil
    :init (setq dired-clean-up-buffers-too t))

  (use-package wdired
    :after dired
    :init (setq wdired-allow-to-change-permissions t)
    :bind (:map
           dired-mode-map
           ("w" . wdired-change-to-wdired-mode)
           :map wdired-mode-map
           ("<next>" . mk-dired-last-file)
           ("<prior>" . mk-dired-first-file)))

  (use-package visual-regexp
    :bind ("C-c %" . vr/query-replace))
  )
(progn ;; hl-todo

  (use-package hl-todo
    :defines hl-todo-keyword-faces
    :functions flymake-hl-todo
    :init
    (add-hook 'flymake-diagnostic-functions #'flymake-hl-todo nil 'local)
    :config
    (add-to-list 'hl-todo-keyword-faces '("MAYBE:" . "#80CCCC"))
    (add-to-list 'hl-todo-keyword-faces '("XXX:" . "#ff8c00"))
    (add-to-list 'hl-todo-keyword-faces '("TODO:" . "#dc143c"))
    (add-to-list 'hl-todo-keyword-faces '("FIXME:" . "#4e9393"))
    :bind (("M-g 2" . hl-todo-insert)))

  (use-package consult-todo
    :bind
    ("M-s t" . consult-todo-all)
    ("M-s T" . consult-todo-dir))
  )
(progn                                  ; Completion: vertico.

  ;; Vertico: Minimalistic vertical completion UI
  (use-package vertico
    :commands vertico-mode
    :defines vertico-map
    :init
    (vertico-mode)
    :custom
    (vertico-count 20)
    (vertico-resize t)
    (vertico-cycle t)
    :bind (:map vertico-map
                ("<next>" . vertico-last)
                ("<prior>" . vertico-first)
                ("C-S-n" . vertico-next-group)
                ("C-S-p" . vertico-previous-group))
    :config
    (setq completion-in-region-function #'consult-completion-in-region))

  ;; Vertico Repeat: repeat last minibuffer session
  (use-package vertico-repeat
    :straight vertico
    :after vertico
    :hook (minibuffer-setup . vertico-repeat-save)
    :bind (("C-;" . vertico-repeat)
           ("C-:" . vertico-repeat-select)))

  ;; Vertico Multiform: per-command/category layout configuration
  (use-package vertico-multiform
    :straight vertico
    :after vertico
    :init (vertico-multiform-mode)
    :custom
    (vertico-multiform-commands
     '((consult-imenu buffer indexed)))
    (vertico-multiform-categories
     '((file indexed)
       (consult-grep buffer)
       (symbol (vertico-sort-function . vertico-sort-alpha))
       ;; (embark-keybinding grid)
       )))

  ;; Save minibuffer history across sessions
  (use-package savehist
    :init (savehist-mode))

  ;; Emacs minibuffer tuning
  (use-package emacs
    :custom
    (enable-recursive-minibuffers t)
    (read-extended-command-predicate #'command-completion-default-include-p) ;only commands working in current mode
    (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)) ;turn off cursor
    (read-file-name-completion-ignore-case t)
    (read-buffer-completion-ignore-case t)
    (completion-ignore-case t))

  ;; Orderless: flexible matching style
  (use-package orderless
    :custom
    (completion-styles '(orderless partial-completion basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles orderless partial-completion))
                                     (buffer (styles orderless partial-completion))
                                     (command (styles orderless partial-completion)))))

  ;; CRM: Configure completing-read-multiple prompt
  (use-package crm
    :straight (:type built-in)
    :init
    (when (< emacs-major-version 31)
      (advice-add #'completing-read-multiple :filter-args
                  (lambda (args)
                    (cons (format "[CRM%s] %s"
                                  (string-replace "[ \t]*" "" crm-separator)
                                  (car args))
                          (cdr args)))))
    )

  ;; Marginalia: annotations in minibuffer
  (use-package marginalia
    :commands (marginalia-mode)
    :defines marginalia-annotators
    :init (marginalia-mode)
    ;; :custom FIXME: is empty
    ;; (marginalia-annotators
    ;;  '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :bind (:map minibuffer-local-map
                ("M-A" . marginalia-cycle)))

  ;; Consult: powerful commands for navigation and search
  (use-package consult
    :defines (xref-show-definitions-function
              xref-show-xrefs-function
              consult-theme
              consult-ripgrep
              consult-grep
              consult-git-grep
              consult-org-agenda
              consult-bookmark
              consult-recent-file
              consult-xref
              consult--source-recent-file
              consult--source-project-recent-file
              consult--source-bookmark
              consult-ripgrep-args
              consult-narrow-key)
    :commands (consult--customize-put
               consult--customize-set
               consult-customize
               consult-completion-in-region
               consult-register-window
               consult-xref)
    :functions consult-completing-read-multiple
    :bind (("C-/" . consult-line)
           ("M-X" . consult-mode-command)
           ("C-c f f" . consult-find)
           ("C-c f F" . consult-locate)
           ("C-c f z" . (lambda () (interactive)(cd "~/")(consult-find)))
           ("C-c f Z" . (lambda () (interactive)(cd "~/")(consult-fd))) ;;TODO: test
           ("C-c f r" . consult-recent-file)
           ("C-c f d" . consult-fd)
           ("M-g t" . consult-theme)
           ("M-g M" . consult-minor-mode-menu)
           ("C-c m" . consult-man)
           ("C-c k" . consult-kmacro)
           ;; Buffers
           ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
           ("M-g b" . consult-buffer)
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
           ;; Bookmarks and registers
           ("C-x r b" . consult-bookmark)    ;; orig. bookmark-jump
           ("M-#" . consult-register-load)
           ("M-\"" . consult-register-store) ;; M-' orig. abbrev-prefix-mark (unrelated)
           ("H-M-'" . consult-register)
           ;; Other custom bindingst
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ("C-x :" . consult-complex-command)       ;; C-x M-: repeat-complex-command
           ;; Goto
           ("M-g g" . consult-goto-line)
           ("M-g o" . consult-outline)
           ("M-g i" . consult-imenu)
           ("M-g M-i" . consult-imenu-multi)
           ("M-g m" . consult-mark)
           ("M-g M-m" . consult-global-mark)
           ("M-g e e" . consult-compile-error)
           ("M-g f" . consult-flymake)
           ("M-g M-o" . consult-org-heading)
           ;; Search
           ("M-s a" . consult-org-agenda)
           ("M-s g" . consult-grep)
           ("M-s M-g" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s M-l" . consult-line-multi)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; History and Isearch integration
           ("M-s e" . consult-isearch-history)
           (:map minibuffer-local-map
                 ("C-r" . consult-history))
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
           ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
           ("M-s M-l" . consult-line-multi)           ;; needed by consult-line to detect isearch
           )
    :init
    ;; This improves the register formatting, adds thin separator lines,
    ;; register sorting and hides the window mode line.
    (advice-add #'register-preview :override #'consult-register-window)
    (setq register-preview-delay 0.5
          xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
    :custom
    (consult-narrow-key "C-+")
    (consult-ripgrep-args
     "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --search-zip --no-heading --with-filename --line-number --hidden --glob=!.git/ --sortr=accessed")
    :config
    (consult-customize
     consult-theme :preview-key '(:debounce 0.4 any)
     consult-ripgrep consult-git-grep consult-org-agenda consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
     :preview-key "<right>")
    )

  ;; Embark: context-sensitive minibuffer actions
  (use-package embark
    :defines embark-quit-after-action
    :preface
    (defun embark-act-noquit ()
      "Run action but don't quit the minibuffer afterwards."
      (interactive)
      (let ((embark-quit-after-action nil))
        (embark-act)))
    :config
    ;; Quit minibuffer after actions that kill buffers, but not others
    (setq embark-quit-after-action '((kill-buffer . t) (t . nil)))
    :bind (("C-." . embark-act); Main action key
           ("C->" . embark-dwim); Do What I Mean
           ("C-h B" . embark-bindings); Alternative to describe-bindings
           :map minibuffer-local-completion-map
           ("C-M-s-e" . embark-export); Export current candidates
           ("C-M-s-b" . embark-become); Become a different command
           :map minibuffer-local-map
           ("C-M-s-e" . embark-export)
           ("C-M-s-b" . embark-become)))

  ;; Optional: Consult previews in embark-collect buffer
  (use-package embark-consult
    :after (embark consult)
    :hook (embark-collect-mode . consult-preview-at-point-mode))

  (use-package wgrep :demand t)
  (use-package consult-recoll
    :bind
    ("M-s /" . consult-recoll))

  ;; Use Dabbrev with Corfu!
  (use-package abbrev
    :straight (:type built-in)
    :config
    (setq abbrev-file-name "~/.emacs.d/abbrev_defs"
          save-abbrevs 'silent)
    (setq-default abbrev-mode t)
    :bind
    ("C-c t a" . abbrev-mode)
    ("M-/" . hippie-expand)        ;; in place of `dabbrev-expand`
    )
  (use-package dabbrev
    ;; Swap M-/ and C-M-/
    :bind (("C-M-/" . dabbrev-completion)
           ("M-/" . dabbrev-expand))
    ;; Other useful Dabbrev configurations.
    :custom
    (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

  (use-package corfu
    :straight (corfu :files (:defaults "extensions/*")
                     :includes (corfu-info corfu-history))
    :config
    (setq corfu-popupinfo-delay 0)
    :custom
    (text-mode-ispell-word-completion nil)
    :init
    (global-corfu-mode)
    (corfu-popupinfo-mode))

  (use-package cape
    :commands (cape-dabbrev
               cape-file
               cape-elisp-block
               cape-dict
               cape-keyword
               cape-capf-super)
    ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
    ;; Press C-c p ? to for help.
    :bind ("C-M-s-<tab>" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
    :init
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-file)
    (add-hook 'completion-at-point-functions #'cape-elisp-block)
    ;; (add-hook 'completion-at-point-functions #'cape-history)
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'cape-dabbrev #'cape-dict #'cape-file #'cape-keyword)))
    )
  )

(use-package yasnippet
  :defines (yas-snippet-dirs
            yas-triggers-in-field
            yas-wrap-around-region
            yas-minor-mode-map)
  :init (which-key-add-key-based-replacements "M-g Y" "Yasnippet")
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippets")
        yas-triggers-in-field t
        yas-wrap-around-region t)
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  :bind (("M-g Y a" . yas-reload-all)
         ("M-g Y n" . yas-new-snippet)
         ("M-g Y v" . yas-visit-snippet-file)
         ("C-c t y" . yas-minor-mode)
         :map yas-minor-mode-map
         ("<tab>" . nil)
         ("TAB" . nil)
         ("<backtab>" . yas-expand))
  :hook ((prog-mode org-mode message-mode markdown-mode) . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package consult-yasnippet
  :after yasnippet
  :bind ("M-s y" . consult-yasnippet))

;; --- Spell and Translate ---
(straight-use-package 'ispell)          ;;TODO: remove
(straight-use-package 'flyspell)
(straight-use-package 'flyspell-correct) ;;TODO: remove
(straight-use-package 'consult-flyspell)
(straight-use-package 'guess-language)
(straight-use-package 'sdcv)
(straight-use-package 'wordnut)
(straight-use-package 'powerthesaurus)
(require 'my-spell)

;; --- Prose ---
(straight-use-package 'cm-mode) ;; critic markup
(straight-use-package 'langtool)
(straight-use-package 'academic-phrases)
(straight-use-package 'writegood-mode)
(require 'my-prose)

;; --- Mail ---
(straight-use-package 'org-msg)
(straight-use-package 'org-mime)
(straight-use-package 'mu4e-jump-to-list)
(require 'my-email)

;; --- Org ---
(straight-use-package 'jupyter)
(straight-use-package 'ob-async)
(straight-use-package 'ox-rst)
(straight-use-package 'ox-pandoc)
(straight-use-package 'ox-twbs)
(straight-use-package 'auctex)
(straight-use-package 'cdlatex)
(straight-use-package 'ox-reveal)
(require 'my-ob)
;; (straight-use-package 'org)
(straight-use-package 'org-autolist)
(straight-use-package 'org-download)
(straight-use-package 'org-cliplink)
(straight-use-package 'org-modern)
(straight-use-package 'spacious-padding)
(require 'my-org)
(straight-use-package 'org-gcal)
(require 'my-org-cal)

(progn                                  ; org-roam and notes

  (use-package org-roam
    :after org
    :commands (org-roam-db-autosync-mode)
    :defines (org-roam-mode-map
              org-roam-node-display-template
              org-roam-completion-everywhere)
    :init
    ;; Help keep the `org-roam-buffer', toggled via `org-roam-buffer-toggle', sticky.
    (add-to-list 'display-buffer-alist
                 '("\\*org-roam\\*"
                   (display-buffer-in-side-window)
                   (side . left)
                   (slot . 0)
                   (window-width . 0.40)
                   (window-parameters . ((no-other-window . t)
                                         (no-delete-other-windows . t)))))
    :custom
    (org-roam-directory "~/Sync/notes/org-roam/")
    :bind (("M-s s" . org-roam-node-find)
           ("M-s M-s" . org-roam-ref-find)
           ("C-c n R" . org-roam-node-random)
           ("C-c n j" . org-roam-dailies-goto-today)
           ("C-c n J" . org-roam-dailies-goto-date)
           ("C-c n s" . org-roam-db-sync)
           :map org-roam-mode-map
           ("C-c n c" . org-roam-capture)
           :map org-mode-map
           ("<f11>" . org-roam-buffer-toggle)
           ("C-c n c" . org-roam-buffer-toggle)
           ("C-c n g" . org-roam-graph)
           ("C-c n a" . org-roam-alias-add)
           ("C-c n t" . org-roam-tag-add)
           ("C-c n o" . org-id-get-create)
           ("C-c n i" . org-roam-node-insert))
    :config
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (setq org-roam-completion-everywhere t)
    (org-roam-db-autosync-mode)
    (use-package org-roam-protocol
      :straight org-roam
      ;; as possible alternative consider https://github.com/alphapapa/org-protocol-capture-html
      ;; REMEMBER to execute:
      ;; xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol
      :config
      (setq org-roam-capture-ref-templates
            '(("r" "ref" plain "%?"
               :target (file+head "websites/${slug}.org" "#+title: ${title}")
               :unnarrowed t)))
      ))

  (use-package org-roam-ui
    :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after (org-roam)
    ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
    ;;         a hookable mode anymore, you're advised to pick something yourself
    ;;         if you don't care about startup time, use
    ;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

  (use-package deft
    :defines (deft-directory
              deft-recursive
              deft-use-filename-as-title
              deft-file-naming-rules
              deft-extensions
              deft-default-extension
              deft-use-filter-string-for-filename)
    :bind
    ("M-s C-n" . deft)
    :config
    (setq deft-directory "~/Sync/notes"
          deft-recursive t
          deft-use-filename-as-title t
          deft-file-naming-rules '((nospace . "_") (case-fn . downcase)) ;; to preserve slashes
          deft-extensions '("org" "md" "markdown")  ;;"txt"
          deft-default-extension "org"
          deft-use-filter-string-for-filename t))

  (use-package consult-notes
    :straight (:type git :host github :repo "mclear-tools/consult-notes")
    :commands (consult-notes
               consult-notes-search-in-all-notes
               consult-notes-org-roam-find-node
               consult-notes-org-roam-find-node-relation)
    :bind (("M-s n" . consult-notes-search-in-all-notes)
           ("M-s M-n" . consult-notes)
           ("M-s N" . consult-notes-org-roam-find-node))
    :config
    (setq consult-notes-file-dir-sources '(("Notes" ?n "~/Sync/notes/")
                                           ("Proj" ?p "~/Sync/proj/")
                                           ("Org" ?o "~/Sync/box/org/"))
          consult-notes-org-headings-files '("~/Sync/proj/lab.org"
                                             "~/Sync/box/org/journal.org"
                                             "~/Sync/box/org/projects.org")
          )
    (consult-notes-org-headings-mode)   ; org heading
    (consult-notes-org-roam-mode)       ; org roam
    ;; XXX: MAYBE: embark and citar support https://github.com/mclear-tools/consult-notes
    )

  (use-package consult-org-roam
    :after org-roam
    :commands consult-org-roam-mode
    :defines consult-org-roam-forward-links
    :init
    (require 'consult-org-roam)
    ;; Activate the minor mode
    (consult-org-roam-mode 1)
    :config
    ;; Eventually suppress previewing for certain functions
    (consult-customize consult-org-roam-forward-links :preview-key (kbd "M-."))
    :bind
    ;; Define some convenient keybindings as an addition
    ("C-c n R" . consult-org-roam-file-find)
    ("C-c n b" . consult-org-roam-backlinks)
    ("C-c n l" . consult-org-roam-forward-links)
    ("C-c n r" . consult-org-roam-search)
    :custom
    ;; Use `ripgrep' for searching with `consult-org-roam-search'
    (consult-org-roam-grep-func #'consult-ripgrep)
    ;; Configure a custom narrow key for `consult-buffer'
    (consult-org-roam-buffer-narrow-key ?r)
    ;; Display org-roam buffers right after non-org-roam buffers
    ;; in consult-buffer (and not down at the bottom)
    (consult-org-roam-buffer-after-buffers t))

  )
(progn ;; Bibliography

  (use-package bibtex
    :bind (:map bibtex-mode-map
                ("M-<tab>" . hs-toggle-hiding)
                ("C-M-s-z" . hs-hide-all)
                ("C-c t m h" . hs-minor-mode)
                ("C-M-s-a" . hs-show-all))
    :config
    (setq bibtex-dialect 'biblatex)
    (setq ;; Fields from Zotero
     bibtex-user-optional-fields '(("keywords" "Keywords to describe the entry" "")
                                   ("note" "Zotero notes" "")
                                   ("annotation" "Zotero annotation" "")
                                   ("file" "Link to document file." ":")))
    (setq bibtex-autokey-name-case-convert-function 'capitalize
          bibtex-autokey-name-year-separator ""
          bibtex-autokey-titleword-separator ""
          bibtex-autokey-year-title-separator "_"
          bibtex-autokey-titlewords 3
          bibtex-autokey-titlewords-stretch 0
          bibtex-autokey-titleword-case-convert-function 'capitalize
          bibtex-autokey-titleword-length 9) ;; Bibtex key format
    )

  (defvar completion-bibliography
    '("~/Sync/biblio/main.bib"
      "~/Sync/biblio/MY.bib"
      "~/Sync/biblio/former.bib"
      "~/Sync/biblio/books.bib") "List of bib files.")
  (defvar completion-library-path
    '("~/Sync/biblio/main/"
      "~/Sync/biblio/MY/"
      "~/Sync/biblio/former/"
      "~/Sync/biblio/books/") "List of folders containing pdf and other documents.")
  (defvar completion-notes-path
    "~/Sync/notes/org-roam/biblio" "Folder (or file) for notes.")

  (use-package citar
    :bind (("M-s b" . citar-open)
           ("M-s c" . citar-open-note)
           ("M-s M-b" . citar-insert-citation)
           :map minibuffer-local-map
           ("M-b" . citar-insert-preset))
    ;; ;; https://github.com/bdarcus/citar/wiki/Notes-configuration
    :defines (citar-templates
              citar-notes-source
              citar-symbol-separator)
    :custom
    (org-cite-global-bibliography completion-bibliography)
    (citar-bibliography completion-bibliography)
    (citar-library-paths completion-library-path)
    (citar-notes-paths '("~/Sync/notes/org-roam/biblio"))
    (org-cite-insert-processor 'citar)
    (org-cite-follow-processor 'citar)
    (org-cite-activate-processor 'citar)
    :defines citar-open-note-functions
    :init
    (setq citar-templates
          `((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
            (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords annotation:*}")
            (preview . "${author editor} (${year issued date}) ${title}, \
    ${journal journaltitle publisher container-title collection-title}.\n")
            (note . ,(concat "${title}\n\n"
                             "- tags ::\n" ;XXX
                             "- keywords :: ${keywords}\n"
                             "- extra :: ${annotation}\n"
                             "- note :: ${note}\n\n"
                             "* TODO main\n"
                             ":PROPERTIES:\n"
                             ":Custom_ID: ${=key=}\n"
                             ":NOTER_DOCUMENT: ${file}\n"
                             ":NOTER_PAGE:\n"
                             ":AUTHOR: ${author}\n"
                             ":JOURNAL: ${journaltitle}\n"
                             ":YEAR: ${year} ${date}\n"
                             ":DOI: ${doi}\n"
                             ":URL: ${url}\n"
                             ":END:\n\n"
                             "* suppl\n"
                             ":PROPERTIES:\n"
                             ":NOTER_DOCUMENT: ${file}\n"
                             ":END:"))))
    :hook
    (LaTeX-mode . citar-capf-setup)
    (org-mode . citar-capf-setup)
    (markdown-mode . citar-capf-setup)
    :config
    (setq citar-notes-source 'citar-file)
    (setq citar-symbol-separator "  ")
    )
  ;; https://github.com/bdarcus/citar/wiki/Notes-configuration

  (use-package citar-org-roam
    :after (citar org-roam)
    :commands (citar-org-roam-mode)
    :config (citar-org-roam-mode))

  (use-package citar-embark
    :after (citar embark)
    :commands (citar-embark-mode)
    :init (citar-embark-mode))

  (use-package oc
    :straight org
    :after (org)
    :config
    (setq org-cite-global-bibliography completion-bibliography)
    (setq org-cite-export-processors
          '(;(beamer csl)
            (latex biblatex)
            (t csl)))
    )

  (use-package oc-biblatex :straight org :after oc)

  (use-package oc-csl :straight org :after (oc)
    :init (setq org-cite-csl-styles-dir "~/Zotero/styles"))

  (use-package oc-natbib :straight org :after oc)


  (declare-function keymap-set "compat-29")
  (use-package engine-mode
    :commands (engine/set-keymap-prefix
               engine-mode)
    :functions (engine--execute-search engine--get-query defengine)
    :defines (1-amazon
              2-duckduckgo
              3-github
              4-google
              google-images
              google-maps
              project-gutenberg
              5-qwant
              stack-overflow
              6-twitter
              7-wikipedia
              8-wiktionary
              wolfram-alpha
              Cambridge-dict-pronunciation
              9-youtube)
    :init
    (engine-mode t)
    (engine/set-keymap-prefix (kbd "M-s M-/"))
    :config
    (defengine 1-amazon
      "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
      :keybinding "z"
      )
    (defengine 2-duckduckgo
      "https://duckduckgo.com/?q=%s"
      ;; :browser 'eww-browse-url
      :keybinding "d")
    (defengine 3-github
      "https://github.com/search?ref=simplesearch&q=%s"
      :keybinding "h")
    (defengine 4-google
      "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
      :keybinding "g")
    (defengine google-images
      "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
      :keybinding "i")
    (defengine google-maps
      "http://maps.google.com/maps?q=%s"
      :keybinding "m"
      :docstring "Mapping' it up.")
    (defengine project-gutenberg
      "http://www.gutenberg.org/ebooks/search/?query=%s"
      :keybinding "u")
    (defengine 5-qwant
      "https://www.qwant.com/?q=%s"
      :keybinding "q")
    (defengine stack-overflow
      "https://stackoverflow.com/search?q=%s"
      :keybinding "o")
    (defengine 6-twitter
      "https://twitter.com/search?q=%s"
      :keybinding "t")
    (defengine 7-wikipedia
      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
      :keybinding "w"
      :docstring "Searching' the wikis.")
    (defengine 8-wiktionary
      "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s"
      :keybinding "k")
    (defengine wolfram-alpha
      "http://www.wolframalpha.com/input/?i=%s"
      :keybinding "a")
    (defengine Cambridge-dict-pronunciation
      "https://dictionary.cambridge.org/us/pronunciation/english/%s"
      :keybinding "p")
    (defengine 9-youtube
      "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
      :keybinding "y"))

  (use-package gscholar-bibtex
    :defines (gscholar-bibtex-default-source
              gscholar-bibtex-database-file)
    :bind ("M-s C-b" . gscholar-bibtex)
    :config
    (setq gscholar-bibtex-default-source "Google Scholar"
          gscholar-bibtex-database-file "/home/dan/Sync/biblio/biblio.bib"))

  )
(progn                                  ; Magit

  (use-package magit
    :defines (magit-repository-directories
              magit-display-buffer-function
              magit-display-buffer-function)
    :bind
    ("C-c g g" . magit-status)
    ("C-c g f" . magit-find-file-other-window)
    ("C-c g x" . magit-checkout)
    ("C-c g e" . magit-ediff-resolve)
    ("C-c g c" . magit-clone)
    ("C-c g i" . magit-init)
    :init
    (which-key-add-key-based-replacements "C-c g" "Git")
    (setq magit-repository-directories '(("/home/dan/workspace" . 4)
                                         ("/home/dati" . 2)
                                         ("~/Sync" . 9)))
    :config
    (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

  (use-package magit-todos
    :after (magit)
    :defines magit-todos-keywords-list
    :bind
    ("C-c g 2" . magit-todos-list)
    ("C-c g 3" . magit-todos-mode)
    :init
    (magit-todos-mode)
    :config
    (add-to-list 'magit-todos-keywords-list "MAYBE")
    (add-to-list 'magit-todos-keywords-list "XXX"))

  ;; (use-package magit-annex)

  (use-package git-modes)

  (use-package gitignore-templates)

  (use-package browse-at-remote
    :bind ("C-c g b" . bar-browse))

  (use-package git-messenger
    :bind ("C-c g m" . git-messenger:popup-message))

  (use-package git-timemachine
    ;; :straight (:type git :repo "https://codeberg.org/pidu/git-timemachine")
    :bind
    ("C-c g t" . git-timemachine)
    ("C-c g T" . git-timemachine-toggle))

  (use-package diff-hl
    :commands global-diff-hl-mode
    :hook (magit-post-refresh . diff-hl-magit-post-refresh)
    :config (global-diff-hl-mode))

  )

;; --- Project ---
(use-package project
  :straight (:type built-in)
  :config
  (setq project-switch-commands #'project-find-file))
(use-package consult-project-extra
  :bind
  ("C-c p" . consult-project-extra-find))
(use-package apheleia
  :demand t
  :commands apheleia-global-mode
  :defines (apheleia-mode-alist
            apheleia-formatters
            apheleia-inhibit-functions)
  :preface
  ;; https://blog.chmouel.com/2016/09/07/dealing-with-yaml-in-emacs/
  (defun aj-toggle-fold ()
    "Toggle fold all lines larger than indentation on current line"
    (interactive)
    (let ((col 1))
      (save-excursion
        (back-to-indentation)
        (setq col (+ 1 (current-column)))
        (set-selective-display
         (if selective-display nil (or col 1))))))
  :bind ("C-<tab>" . aj-toggle-fold)
  :init (apheleia-global-mode +1)
  :config
  ;; Use json.tool for JSON files [python] (lsp: json-ls) extra/vscode-json-languageserver
  (add-to-list 'apheleia-mode-alist '(json-mode . python3-json))
  ;; Use yamlfmt for YAML files [yamlfmt] (lsp: yamlls) extra/yaml-language-server
  (add-to-list 'apheleia-formatters '(yamlfmt "yamlfmt" "-in"))
  (add-to-list 'apheleia-mode-alist '(yaml-mode . yamlfmt))
  ;; Use mdformat for MARKDOWN files [mdformat] (lsp: marksman) can be a dev dep
  (add-to-list 'apheleia-formatters '(mdformat "mdformat" "-"))
  (add-to-list 'apheleia-mode-alist '(markdown-mode . mdformat))
  ;; Add the taplo for TOML files [taplo-cli] taplo-cli include lsp
  (add-to-list 'apheleia-formatters '(taplo "taplo" "fmt")) ; Call 'taplo fmt' executable
  (add-to-list 'apheleia-mode-alist '(toml-mode . taplo)) ; Link toml-mode to taplo formatter
  ;; Add the shfmt for sh files [shfmt] (lsp: bash-language-server)
  (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt)) ; Link toml-mode to taplo formatter

  ;; Replace default (black) to use ruff for sorting import and formatting.
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (add-to-list 'apheleia-inhibit-functions
               (lambda ()
                 (and buffer-file-name
                      (string-match-p "\\.ipynb\\'" buffer-file-name)))) ; This function checks the file extension
  )

;; (use-package flycheck
;;   :init (global-flycheck-mode))

;; Treat files with a shell shebang but no extension as shell scripts
(add-to-list 'auto-mode-alist '("\\(ba\\|z\\)?sh\\'" . sh-mode))

(use-package eglot
  :straight (:type built-in)
  :hook
  (prog-mode . (lambda () (unless (eq major-mode 'emacs-lisp-mode) (eglot-ensure))))
  (yaml-mode . eglot-ensure)
  (markdown-mode . eglot-ensure)
  (toml-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.2)
  :config
  ;; (add-to-list 'eglot-server-programs '(python-mode . ("ruff" "server")))
  (add-to-list 'eglot-server-programs '((toml-mode) "taplo" "lsp" "stdio"))
  :bind (:map
         eglot-mode-map
         ("C-c r a" . eglot-code-actions)
         ("C-c r r" . eglot-rename)
         ("C-c r f" . eglot-format)))

(use-package flymake
  :straight (:type built-in)
  :bind (("M-g e l" . flymake-show-buffer-diagnostics)
         ("M-g e p" . flymake-show-project-diagnostics))
  :hook ((gitignore-mode . flymake-mode)
         (markdown-mode . flymake-mode)
         ;; (org-mode . flymake-mode) ; Uncomment if you want flymake in Org-mode
         ;; (text-mode . flymake-mode) ; Uncomment if you want flymake in text-mode
         ;; (yaml-mode . flymake-mode) ; If you want flymake for YAML without LSP
         (prog-mode . flymake-mode)))

;; --- Additional modes ---
(straight-use-package 'markdown-mode)
(straight-use-package 'sphinx-mode)
(straight-use-package 'plantuml-mode)
(straight-use-package 'graphviz-dot-mode)
(straight-use-package 'gnuplot)
(straight-use-package 'ess)
(straight-use-package 'json-mode)
(straight-use-package 'ssh-config-mode)
(straight-use-package 'pkgbuild-mode)
(straight-use-package 'web-mode)
(straight-use-package 'vimrc-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'toml-mode)
(straight-use-package 'csv-mode)
(straight-use-package 'dna-mode)
(require 'my-modes)

(progn                                  ; python

  (use-package python
    :straight (:type built-in)
    :bind   (("C-c t m p" . python-mode)
             (:map python-mode-map
                   ;; ("<backtab>" . hs-toggle-hiding) ; orig. python-indent-dedent-line
                   ("C-c C-P" . jupyter-run-repl)))
    :config
    (setq-default python-fill-docstring-style 'pep-257-nn
                  python-indent 4)
    (setq python-shell-interpreter "jupyter"
          python-shell-interpreter-args "console --simple-prompt"
          python-shell-prompt-detect-failure-warning nil)
    (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))

  (use-package devdocs
    :after python
    :bind ("C-c D" . devdocs-lookup))

  (use-package envrc
    :commands envrc-global-mode
    :init
    (envrc-global-mode))

  (use-package numpydoc
    :defines (python-mode-map numpydoc-insertion-style)
    :bind (:map python-mode-map
                ("C-c C-N" . numpydoc-generate))
    :init
    (setq numpydoc-insertion-style 'yas)) ;'prompt|nil

  (use-package python-pytest
    :bind (:map python-mode-map
                ("C-c T" . python-pytest-dispatch)
                ("<f8>"  . python-pytest-dispatch)))

  ;; Optionally, for REPL-driven workflows
  ;; (use-package eval-in-repl
  ;;   :after (python)
  ;;   :hook (python-mode . (lambda () (require 'eval-in-repl-python) ))
  ;;   :custom (eir-jump-after-eval nil)
  ;;   :bind (:map python-mode-map
  ;;               ("<C-return>" . eir-eval-in-python))
  ;;   )
  )

(use-package nov
  :defines nov-text-width
  :mode ("\\.epub\\'" . nov-mode)
  ;; :custom-face (variable-pitch ((t (:family "URW Bookman" :height 1.1))))
  :hook (nov-mode . visual-fill-column-mode)
  :config (setq nov-text-width t))

(use-package keyfreq
  :commands (keyfreq-mode
             keyfreq-autosave-mode)
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package calibredb
  :commands calibredb
  :defines (calibredb-root-dir
            calibredb-db-dir
            calibredb-library-alist
            calibredb-size-show
            calibredb-comment-width
            calibredb-ref-default-bibliography)
  ;; ripgrep-all (rga)
  :config
  (setq calibredb-root-dir "~/Sync/media/books/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Sync/media/books/")))
  (setq calibredb-size-show nil)
  (setq calibredb-comment-width 0)
  ;; (setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
  (setq calibredb-ref-default-bibliography "~/Sync/media/ebooks.bib")
  ;; (add-to-list 'bibtex-completion-bibliography calibredb-ref-default-bibliography)
  )

(straight-use-package
 '(seqel :type git :host github :repo "RNAer/seqel"))

(use-package emacs
  :straight nil
  :preface
  (defun my-org-zotero-open (path _)
    (call-process "xdg-open" nil nil nil (concat "zotero:" path)))
  :config
  (org-link-set-parameters "zotero" :follow #'my-org-zotero-open)
  )

;; --- AI LLM ---
(straight-use-package 'ellama)
(straight-use-package 'gptel)
;; (straight-use-package 'chatgpt-shell)
(require 'my-ai)

(use-package eee
  :straight (:type git :host github :repo "eval-exec/eee.el"
                   :files (:defaults "*.el" "*.sh"))
  :bind-keymap
  ("C-M-s-e" . ee-keymap)
  :config
  (setq ee-terminal-command "foot")
  (define-key ee-keymap (kbd "R") 'ee-rga)
  )

(setq debug-on-error nil)
(setq debug-on-quit nil)
(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))

(provide 'init)
;;; init.el ends here
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
 '(safe-local-variable-values '((org-download-image-dir . "./WORK/"))))


;; Here is a revision that simplifies your init.el by removing redundant or less-used packages, consolidates similar functionality, and transitions to some better-maintained or recommended alternatives. The goal is to keep your core workflow intact but reduce complexity and maintenance effort:

;; ---


;; - Removed `hungry-delete` and some commented out printing settings that aren't in use.
;; - Removed `gscholar-bibtex` (outdated) in favor of `citar` for bibliography, which you already use.
;; - Removed duplicative or lesser-used packages like `transpose-frame`, `tzc`, `numpydoc` (the latter can be reconsidered if you want but adds complexity).
;; - Removed `engine-mode`: mostly replaced by minibuffer search with Consult + Embark which you have configured.
;; - Consolidated built-in package uses to fewer `use-package` blocks with `:straight (:type built-in)` as appropriate.
;; - For spell/translation: keep only essential `flyspell`, `consult-flyspell`, `guess-language`.
;; - For completion, rely on `vertico`, `orderless`, `consult`, `embark` — already well set up.

;; ### Recommended transitions

;; - For snippet support: continue with `yasnippet` and `yasnippet-snippets` (well maintained).
;; - For LSP: use `eglot` (which you have) but add minimal configuration only.
;; - Use `apheleia` for formatters, keep it tuned to your existing setup.
;; - Use `doom-modeline` + `nerd-icons` + `minions` as your modeline stack.

;; ### Example of cleaned and simplified snippet for core packages:

;; ```elisp

;; ;; Completion & minibuffer: vertico + orderless + consult + embark
;; (use-package vertico
;;   :init (vertico-mode)
;;   :custom (vertico-count 20) (vertico-cycle t))

;; (use-package orderless
;;   :custom (completion-styles '(orderless partial-completion basic)))

;; (use-package consult
;;   :bind (("C-/" . consult-line)
;;          ("C-x b" . consult-buffer)
;;          ;; Add other preferred consult bindings
;;          ))

;; (use-package embark
;;   :bind (("C-." . embark-act)
;;          ("C-|" . embark-dwim)))

;; (use-package marginalia
;;   :init (marginalia-mode))

;; ;; Snippets
;; (use-package yasnippet
;;   :init (yas-global-mode 1)
;;   :config
;;   (setq yas-snippet-dirs '("~/.emacs.d/yasnippets")))

;; (use-package yasnippet-snippets
;;   :after yasnippet)

;; ;; Modeline and icons
;; (use-package nerd-icons
;;   :config
;;   (unless (find-font (font-spec :family "Symbols Nerd Font Mono"))
;;     (nerd-icons-install-fonts))
;;   (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

;; (use-package doom-modeline
;;   :hook (after-init . doom-modeline-mode))

;; (use-package minions
;;   :hook (after-init . minions-mode))

;; ;; Programming editing
;; (use-package smartparens
;;   :hook (prog-mode . smartparens-mode)
;;   :config (smartparens-global-mode 1))

;; (use-package hideshow
;;   :hook (prog-mode . hs-minor-mode)
;;   :bind (:map prog-mode-map
;;               ("M-<tab>" . hs-toggle-hiding)))

;; ;; Git
;; (use-package magit
;;   :bind (("C-c g g" . magit-status))
;;   :config
;;   (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; (use-package git-modes)
;; (use-package browse-at-remote)

;; ;; Org Roam / Notes stack remains, but consider trimming users to only needed packages.

;; ;; Spell check minimal
;; (use-package flyspell)
;; (use-package consult-flyspell)
;; (use-package guess-language)

;; ;; Formatter
;; (use-package apheleia
;;   :config (apheleia-global-mode +1))

;; ;; LSP
;; (use-package eglot
;;   :hook (prog-mode . eglot-ensure)
;;   :custom (eglot-autoshutdown t))

;; ;; Misc essentials only; remove obsolete/unnecessary packages for simplicity.
;; ```

;; ---

;; If you want, I can prepare a fully annotated cleaned init.el based on this or help identify specific packages to remove or replace with alternatives that have better support or simpler configs. The above approach reduces package count, duplicates, and unused legacy configs while keeping your experience familiar and efficient.
