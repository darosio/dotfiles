;;; init.el --- Personal Emacs configuration file
;;
;; Author: DanieleArosio <daniele.arosio@cnr.it>
;; Version: 3.19.0
;;
;; This file contains my personal Emacs configuration.
;;

;;; Commentary:
;; Binding keys reserved to user are: "C-c <letter>" and <F5> to <F9>.

;;; Code:

;; --- Startup & Core Emacs Settings ---

;; Disable GUI elements early
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Speed up startup
(setq frame-inhibit-implied-resize t
      inhibit-splash-screen t
      use-file-dialog nil)

;; Global debug settings (consider making these conditional for normal use)
(setq debug-on-error t
      debug-on-quit t)

;; Set user-emacs-directory to the directory of this file
(setq user-emacs-directory (file-truename (file-name-directory load-file-name)))

;; Define a constant for Emacs start time (good for profiling)
(defconst emacs-start-time (current-time))

;; --- Package Management (straight.el and use-package) ---

;; Disable file-name-handler-alist during startup for speed, restore later.
;; This is a very Doom-specific optimization, keep if it still gives you a noticeable speedup.
(defvar doom--initial-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (dolist (handler file-name-handler-alist)
              (add-to-list 'doom--initial-file-name-handler-alist handler))
            (setq file-name-handler-alist doom--initial-file-name-handler-alist)))

;; Configure straight.el defaults
(setq straight-vc-git-default-clone-depth 1
      straight-recipes-gnu-elpa-use-mirror t)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Require straight and use-package
(require 'straight)

;; Install org built-in via straight to shadow Emacs's version (good practice)
(straight-use-package 'org)

;; Enable straight.el for all use-package calls by default
(setq straight-use-package-by-default t)

;; Enable autoload caching and modification checks for straight.el
(setq straight-cache-autoloads t
      straight-check-for-modifications '(watch-files find-when-checking))

;; Configure Emacs as a daemon if applicable
(require 'server)
(defvar is-daemon nil "T if Emacs is running as a daemon.")
(if (daemonp)
    (progn
      (setq server-raise-frame t
            is-daemon t)
      (setenv "EDITOR" "emacsclient -c -a='')))

;; Use-package configuration
(use-package use-package
  :straight t ; Ensure use-package itself is managed by straight
  :init
  ;; Defer package loading based on daemon status
  (if is-daemon
      (setq use-package-always-demand nil)
    (setq use-package-always-defer t))
  :config
  (setq use-package-compute-statistics t
        use-package-verbose t
        use-package-hook-name-suffix nil
        use-package-enable-imenu-support t))

;; Ensure Magit uses its own transient, not the built-in Emacs one.
;; This often gets fixed by `:straight t` which fetches the latest version.
(use-package magit
  :straight t)

(use-package async
  :straight t)

;; --- UI Base Settings (built-in packages) ---

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
  (ediff-split-window-function
   (if (> (frame-width) 150)
       'split-window-horizontally
     'split-window-vertically))
  (ediff-diff-options "-w"))

(use-package emacs
  :straight (:type built-in)
  :preface
  (defun mk-set-font (font &optional height)
    "Set font FONT as main font for all frames.
    HEIGHT, if supplied, specifies height of letters to use."
    (interactive
     (list (completing-read "Use font: " (font-family-list)) nil))
    (set-face-attribute 'default nil :family font)
    (when height
      (set-face-attribute 'default nil :height height))
    (set-face-attribute 'variable-pitch nil :family font))

  (defun xah-toggle-line-spacing ()
    "Toggle line spacing between no extra space to extra half line height."
    (interactive)
    (setq-local line-spacing (if line-spacing nil 0.5))
    (redraw-frame))
  :config
  (setq-default
   blink-cursor-mode 0               ; Don't blink the cursor
   buffer-file-coding-system 'utf-8-auto
   cursor-in-non-selected-windows nil
   cursor-type '(bar . 3)
   echo-keystrokes 0.1
   enable-recursive-minibuffers t    ; Enable recursive minibuffers
   fill-column 80
   font-lock-maximum-decoration t
   gc-cons-threshold (* 50 1000 1000)
   global-mark-ring-max 1024
   image-use-external-converter t    ; 27.1 viewer don't display many png
   indent-tabs-mode nil              ; use spaces instead of tabs for indentation
   indicate-empty-lines t
   inhibit-startup-screen t          ; Redundant with `inhibit-splash-screen` above
   kept-new-versions 6               ; Redundant with `files` package below
   kept-old-versions 2               ; Redundant with `files` package below
   major-mode 'text-mode             ; This sets default major mode for new buffers. Consider if truly desired.
   mark-ring-max 64
   read-process-output-max (* 1024 1024)
   resize-mini-windows t
   ring-bell-function 'ignore
   save-interprogram-paste-before-kill t
   save-place-mode t                 ; remember last position in file
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
  (show-paren-mode 1)
  (global-hl-line-mode 1)
  (put 'narrow-to-region 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  :bind
  (("C-M-s-/" . hippie-expand)
   ("C-M-s-;" . comment-box)
    ("C-M-s-<backspace>" . kill-whole-line)
    ("C-M-s-\\" . indent-region)
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
    ("C-c Q" . save-buffers-kill-emacs)))

  (use-package files
    :straight (:type built-in)
    :preface
    (defun my-revert-buffer (&rest _)
      "Revert buffer without prompting."
      (revert-buffer t t))
    :custom
    (auto-save-default nil)
    (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
    (copy-backup-files t)
    (backup-directory-alist `((".*" . ,temporary-file-directory)))
    (delete-old-versions t)
    (kept-new-versions 4)
    (kept-old-versions 2)
    (large-file-warning-threshold 10240000)
    (require-final-newline t)
    (vc-display-status t)
    (vc-follow-symlinks t)
    (version-control t)
    :bind
    ("C-x B" . revert-buffer)
    :hook
    (after-save-hook . executable-make-buffer-file-executable-if-script-p))

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
          suggest-key-bindings nil)
    :preface
    (defun mk-auto-fill-mode ()
      "Enable `auto-fill-mode` limiting it to comments."
      (setq-local comment-auto-fill-only-comments t)
      (auto-fill-mode 1))
    :config
    (column-number-mode 1)
    :bind
    ("C-z" . undo)
    ("C-c q" . auto-fill-mode)
    ("M-h" . mark-word)
    ("M-S-h" . mark-paragraph)
    ("<f7> c" . count-words)
    :hook
    ((gitignore-mode-hook . mk-auto-fill-mode)
     (haskell-cabal-mode-hook . mk-auto-fill-mode)
     (prog-mode-hook . mk-auto-fill-mode)
     (proof-mode-hook . mk-auto-fill-mode)
     ;; (text-mode . auto-fill-mode) ; Potentially redundant if already using mk-auto-fill-mode
     ;; (yaml-mode-hook . mk-auto-fill-mode) ; TODO: check and remove, often not needed
     ))

  (use-package window
    :straight (:type built-in)
    :preface
    (defvar prot/window-configuration nil
      "Current window-monocle configuration.")
    ;; `declare-function` is useful but use-package `:commands` or `:functions` often handle this
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
    (setq window-sides-vertical nil) ; You might want `split-height-threshold` too
    :hook ((help-mode-hook . visual-line-mode)
           (Custom-mode-hook . visual-line-mode))
    :bind (("C-M-s-n" . next-buffer)
           ("C-M-s-p" . previous-buffer)
           ("C-M-s-o" . other-window)
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
           ("C-M-s-m" . prot/window-single-toggle)
           ("C-M-s-s" . window-toggle-side-windows)
           ("C-M-s-q" . delete-window) ; emulate i3wm
           ("C-M-s-<up>" . windmove-up)
           ("C-M-s-<left>" . windmove-left)
           ("C-M-s-<down>" . windmove-down)
           ("C-M-s-<right>" . windmove-right)))

  (use-package electric
    :straight (:type built-in)
    :init
    (electric-indent-mode 0) ; Disable globally, then enable selectively
    :hook (python-mode-hook . electric-indent-mode))

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
    (ps-print-duplex nil))

  ;; Themes and fonts
  (use-package faces
    :straight (:type built-in)
    :config
    ;; Use `set-face-attribute` in `:config` for clarity
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
    :defer t
    :init
    (when (daemonp)
      (load-theme 'solarized-selenized-dark t)))

  ;; --- Single Packages ---

  (use-package unfill
    :straight t
    :bind ("C-c M-q" . unfill-toggle))

  (use-package aggressive-indent
    :straight t
    :bind ("C-c t i" . aggressive-indent-mode)
    :hook ((emacs-lisp-mode-hook . aggressive-indent-mode)
           (html-mode-hook . aggressive-indent-mode)))

  (use-package delsel
    :straight t
    :init (delete-selection-mode 1))

  (use-package fix-word
    :straight t
    :bind
    ("M-c" . fix-word-capitalize)
    ("M-l" . fix-word-downcase)
    ("M-u" . fix-word-upcase))

  (use-package which-key
    :straight t
    :commands (which-key-mode which-key-add-key-based-replacements)
    :bind ("C-M-s-<f1>" . which-key-show-top-level)
    :init
    (which-key-mode 1)
    (which-key-add-key-based-replacements "C-c t m" "Toggle mode")
    (which-key-add-key-based-replacements "C-c t o" "Toggle org")
    :config
    (setq which-key-idle-delay 0.05))

  (use-package visual-fill-column
    :straight t
    :commands (visual-fill-column-split-window-sensibly
               visual-fill-column-adjust
               no-distraction-enable
               no-distraction-disable) ; Declare user functions here
    :bind (("C-c v" . visual-fill-column-mode)
           ("<f12>" . no-distraction-enable)
           ("<C-f12>" . no-distraction-disable))
    :init
    (setq visual-fill-column-center-text t
          visual-fill-column-width 98
          visual-fill-column-fringes-outside-margins nil
          visual-line-fringe-indicators '(bottom-left-angle top-right-angle)
          split-window-preferred-function #'visual-fill-column-split-window-sensibly)
    :config
    (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
    :hook
    (visual-fill-column-mode-hook . visual-line-mode)
    :preface ; Helper functions for no-distraction
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
    :straight t
    :bind
    ("C-a" . crux-move-beginning-of-line)
    ("C-o" . crux-smart-open-line)
    ("C-S-o" . crux-smart-open-line-above)
    ("C-H-o" . crux-duplicate-and-comment-current-line-or-region))

  (use-package recentf
    :straight (:type built-in)
    :init (recentf-mode t)
    :custom
    (recentf-exclude `(,(expand-file-name "straight/build/" user-emacs-directory)
                       ,(expand-file-name "eln-cache/" user-emacs-directory)
                       ,(expand-file-name "var/" user-emacs-directory)
                       ;; Add other paths if needed
                       "COMMIT_EDITMSG\\'" ; Exclude Git commit messages
                       "\\.\\(?:elc\\|ln\\)$" ; Exclude compiled Lisp files
                       "\\(?:[0-9]+\\.[a-z]+\\|\\.git\\)/" ; Exclude git directories and certain files
                       )))

  (use-package ace-window
    :straight t
    :demand t
    :bind (("C-M-s-w" . ace-window)
           ("C-'" . ace-select-window)
           ("C-\"" . ace-swap-window))
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
          aw-scope 'global
          aw-dispatch-always nil))

  (use-package transpose-frame
    :straight t
    :bind ("C-M-s-e" . transpose-frame))

  (use-package imenu-list
    :straight t
    :bind (("<f9>" . imenu-list)
           ("<C-f9>" . imenu-list-smart-toggle)))

  (use-package pass
    :straight t
    :bind ("C-x P" . pass))

  (use-package tzc
    :straight t
    :demand t
    :defines tzc-favourite-time-zones
    :bind (("C-x T c" . tzc-convert-current-time)
           ("C-x T t" . tzc-convert-time-at-mark)
           ("C-x T w" . tzc-world-clock))
    :config
    (setq tzc-favourite-time-zones '("Europe/Rome" "America/New_York" "Asia/Tokyo"))) ; Added more for example

  (use-package doom-modeline
    :straight t
    :demand t
    :commands (doom-modeline-mode)
    :custom (doom-modeline-minor-modes t)
    :config (doom-modeline-mode 1))

  (use-package nerd-icons
    :straight t
    :commands (nerd-icons-install-fonts)
    :init
    ;; Check for font existence more robustly
    (unless (find-font (font-spec :family "Symbols Nerd Font Mono"))
      (nerd-icons-install-fonts))
    :custom
    (nerd-icons-font-family "Symbols Nerd Font Mono"))

  (use-package minions
    :straight t
    :commands (minions-mode)
    :init (minions-mode 1))

  (use-package smartparens
    :straight t
    :commands smartparens-global-mode
    :init
    (add-hook 'prog-mode-hook #'smartparens-mode)
    (setq sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil)
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
    ((inferior-python-mode-hook . smartparens-mode)
     (jupyter-repl-mode-hook . smartparens-mode))
    :config
    (smartparens-global-mode 1)
    (advice-add 'sp-add-to-previous-sexp :after (lambda () (just-one-space)))
    (advice-add 'sp-add-to-previous-sexp :after (lambda () (sp-forward-sexp)))
    ;; Use `add-to-list` for these
    (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-cabal-mode)
    (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-mode))

  (use-package expand-region
    :straight t
    :bind ("C-=" . er/expand-region))

  (use-package hideshow
    :straight (:type built-in)
    :bind (("C-c t f" . hs-minor-mode)
           (:map prog-mode-map
                 ("<backtab>" . hs-toggle-hiding)
                 ("C-M-s-z" . hs-hide-all)
                 ("C-M-s-a" . hs-show-all)))
    :config
    (setq hs-hide-comments-when-hiding-all nil)
    :hook (prog-mode-hook . hs-minor-mode))

  (use-package calc
    :straight (:type built-in)
    :bind ("M-g M-a c" . calc))

  (use-package dired
    :straight (:type built-in)
    ;; Commands often need to be listed for autoloading, especially if called directly
    :commands (dired dired-get-filename dired-next-line dired-previous-line)
    :init
    (setq delete-by-moving-to-trash t
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
    :straight (:type built-in) ; dired-x is typically built-in or part of dired
    :after dired
    :init (setq dired-clean-up-buffers-too t))

  (use-package wdired
    :straight (:type built-in)
    :after dired
    :init (setq wdired-allow-to-change-permissions t)
    :bind (:map
           dired-mode-map
           ("w" . wdired-change-to-wdired-mode)
           :map wdired-mode-map
           ("<next>" . mk-dired-last-file)
           ("<prior>" . mk-dired-first-file)))

  (use-package visual-regexp
    :straight t
    :bind ("C-c %" . vr/query-replace))

  ;; --- Hydra, hl-todo, and Local Packages ---

  (use-package hydra
    :straight t
    :bind (("C-x C-b" . hydra-buffer/body))
    :commands (hydra-default-pre hydra-keyboard-quit hydra--call-interactively-remap-maybe
                                 hydra-show-hint hydra-set-transient-map) ; These are often internal, only list if directly needed
    :config
    (defhydra hydra-buffer (:color red :hint nil)
      ("k" kill-this-buffer "kill")
      ("K" kill-buffer-and-window "kill all")
      ("x" delete-window "delete")
      ("s" save-buffer "save")
      ("b" consult-buffer "switch") ; Assuming consult is used for buffer switching
      ("e" eval-buffer "evaluate")
      ("<" previous-buffer "previous")
      (">" next-buffer "next")))

  (use-package hl-todo
    :straight t
    :bind (("C-c 2 2" . hydra-2DO/body)
           ("C-c 2 n" . hl-todo-next)
           ("C-c 2 p" . hl-todo-previous)
           ("C-c 2 i" . hl-todo-insert)
           ("C-c 2 o" . hl-todo-occur))
    :config
    (defhydra hydra-2DO (:color pink :hint nil :foreign-keys warn)
      "A hydra for hl-todo."
      ("p" hl-todo-previous "previous")
      ("n" hl-todo-next "next")
      ("o" hl-todo-occur "occur")
      ("i" hl-todo-insert "insert")
      ("q" nil "cancel" :color blue))
    ;; Use `add-to-list` for keyword faces
    (add-to-list 'hl-todo-keyword-faces '("MAYBE:" . "#80CCCC"))
    (add-to-list 'hl-todo-keyword-faces '("XXX:" . "#ff8c00"))
    (add-to-list 'hl-todo-keyword-faces '("TODO:" . "#dc143c"))
    (add-to-list 'hl-todo-keyword-faces '("FIXME:" . "#4e9393")))

  ;; Local packages: mk-utils and mk-text
  ;; Consider placing these in a dedicated 'lisp/' or 'local/' subdirectory
  ;; and using `add-to-list 'load-path` at the top of your init file once.
  ;; This makes `use-package` syntax cleaner without `:load-path` on each.
  (add-to-list 'load-path (expand-file-name "mk" user-emacs-directory))

  (use-package mk-utils
    :defer 0 ; Defer 0 means load as soon as possible after dependencies
    :straight nil) ; Not managed by straight, assumed local

  (use-package mk-text
    :defer 0
    :straight nil
    :commands (mk-transpose-line-down
               mk-transpose-line-up
               mk-duplicate-line
               mk-mark-command
               mk-smart-indent
               mk-eat-indent
               mk-join-lines
               mk-copy-rest-of-line
               mk-copy-buffer ; Make sure this function exists in mk-text
               mk-yank-primary
               mk-narrow-to-region ; Make sure this function exists
               mk-add-to-end-of-lines
               mk-sort-lines-dwim)
    :bind
    ("C-SPC" . mk-mark-command)
    ("C-S-r" . mk-smart-indent)
    ("M-S" . mk-eat-indent)
    ("M-C-j" . mk-join-lines)
    ("M-n" . mk-transpose-line-down)
    ("M-p" . mk-transpose-line-up)
    ("M-r" . mk-duplicate-line)
    ("C-H-w" . mk-copy-rest-of-line)
    ("C-c E e" . mk-add-to-end-of-lines)
    ("C-S-y" . mk-yank-primary)
    ("C-c E s" . mk-sort-lines-dwim)
    ("C-$" . (lambda () (interactive) (move-end-of-line 1) (yank))))

  ;; --- Completion (Vertico, Orderless, Marginalia, Consult) ---

  (use-package vertico
    :straight (vertico :includes vertico-repeat :files (:defaults "extensions/vertico-repeat.el"))
    :bind
    (("C-;" . vertico-repeat)
     ("C-:" . vertico-repeat-select)
     :map vertico-map
     ("<next>" . vertico-last)
     ("<prior>" . vertico-first)
     ("C-S-n" . vertico-next-group)
     ("C-S-p" . vertico-previous-group)
     ("M-RET" . minibuffer-force-complete-and-exit)
     ("M-<tab>" . minibuffer-complete))
    :hook
    (minibuffer-setup-hook . vertico-repeat-save)
    :init
    (vertico-mode t)
    :custom
    (vertico-scroll-margin 0)
    (vertico-count 20)
    (vertico-resize t)
    (vertico-cycle t))

  (use-package vertico-repeat
    :straight t ; Ensure this extension is also managed by straight
    :after (vertico))

  (use-package orderless
    :straight t
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles basic partial-completion)))))

  (use-package savehist
    :straight (:type built-in)
    :after (vertico) ; Ensure vertico is loaded if its history is managed
    :init
    (savehist-mode)
    :config
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

  (use-package emacs
    :straight (:type built-in)
    :bind
    (("C-c l f" . find-library)
     ("C-c l a" . apropos-library)
     ("C-c l l" . load-library))
    :init
    (which-key-add-key-based-replacements "C-c f" "Files")
    (which-key-add-key-based-replacements "C-c l" "Libraries")
    :preface
    (defun crm-indicator (args)
      "Prepend `[CRM]` to the prompt for `completing-read-multiple`."
      (cons (concat "[CRM] " (car args)) (cdr args)))
    :config
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
    (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
    (setq read-extended-command-predicate #'command-completion-default-include-p))

  (use-package marginalia
    :straight t
    :commands (marginalia-mode)
    :bind (:map minibuffer-local-map
                ("M-A" . marginalia-cycle))
    :init (marginalia-mode))

  (use-package consult
    :straight t
    :commands (consult--customize-put consult--customize-set consult-completion-in-region)
    :bind (("C-/" . consult-line)
           ("C-c f f" . consult-find)
           ("C-c f F" . consult-locate)
           ("C-c f z" . (lambda () (interactive)(cd "~/")(consult-find)))
           ("C-c f r" . consult-recent-file)
           ("M-g T" . consult-theme)
           ("M-g M" . consult-minor-mode-menu)
           ("C-c m" . consult-mode-command)
           ("C-c k" . consult-kmacro)
           ;; C-x bindings (ctl-x-map)
           ("C-x :" . consult-complex-command)
           ("C-x b" . consult-buffer)
           ("C-x 4 b" . consult-buffer-other-window)
           ("C-x 5 b" . consult-buffer-other-frame)
           ("C-x r b" . consult-bookmark)
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-\"" . consult-register-store)
           ("H-M-'" . consult-register)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)
           ;; M-g bindings (goto-map)
           ("M-g E" . consult-compile-error)
           ("M-g M-g" . consult-goto-line)
           ("M-g o" . consult-outline)
           ("M-g M-o" . consult-org-heading)
           ("M-g m" . consult-mark)
           ("M-g M-m" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g M-i" . consult-imenu-multi)
           ;; M-s bindings (search-map)
           ("M-s a" . consult-org-agenda)
           ("M-s f" . consult-find)
           ("M-s M-f" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s M-g" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s M-l" . consult-line-multi)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; Isearch integration
           ("M-s e" . consult-isearch-history)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)
           ("M-s e" . consult-isearch-history)
           ("M-s l" . consult-line)
           ("M-s M-l" . consult-line-multi))
    :hook (completion-list-mode-hook . consult-preview-at-point-mode)
    :config
    (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args)))
    (consult-customize
     consult-theme :preview-key '(:debounce 0.4 any)
     consult-ripgrep consult-git-grep consult-org-agenda
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
     :preview-key "<right>")
    (setq consult-ripgrep-args
          "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --search-zip --no-heading --with-filename --line-number --hidden --glob=!.git/ --sortr=accessed"))
