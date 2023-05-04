;;; init.el --- Personal Emacs configuration file
;;
;; Author: DanieleArosio <daniele.arosio@cnr.it>
;; Version: 3.5.0
;;
;; This file contains my personal Emacs configuration.
;;

;;; Commentary:
;; Binding keys reserved to user are: "C-c <letter>" and <F5> to <F9>.

;;; Code:

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Inhibit frame resizing to speed up startup time
(setq frame-inhibit-implied-resize t)

;; Disable splash screen and file dialogs
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)

;; Set global variables
(setq-default debug-on-error t)
(setq-default debug-on-quit t)

;; Set custom variables
;; Ensure Doom is running out of this file's directory
(custom-set-variables
 '(user-emacs-directory (file-truename (file-name-directory load-file-name))))

;; Define a constant variable
(defconst emacs-start-time (current-time))

(progn ;; Package configuration
  ;; Disable file-name-handler-alist during startup
  (defvar doom--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist' later, because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (dolist (handler file-name-handler-alist)
                (add-to-list 'doom--initial-file-name-handler-alist handler))
              (setq file-name-handler-alist doom--initial-file-name-handler-alist)))

  ;; Use 'setq-default' instead of custom-set or setq to set variables
  (setq-default straight-vc-git-default-clone-depth 1)
  (setq-default straight-recipes-gnu-elpa-use-mirror t)

  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; (declare-function straight-use-package "straight")
  (require 'straight)
  ;; Load use-package
  (straight-use-package 'use-package)
  (eval-when-compile (require 'use-package))

  (declare-function use-package-autoload-keymap "use-package")

  ;; list-load-path-shadows built-in org
  (straight-use-package 'org)

  ;; Install packages using straight.el by default
  (setq-local straight-use-package-by-default t)

  ;; Enable autoload caching and check for modifications
  (setq straight-cache-autoloads t)
  (setq straight-check-for-modifications '(watch-files find-when-checking))

  ;; Configure use-package
  (require 'server)
  (defvar is-daemon nil)
  (if (daemonp)
      (progn
        (setq use-package-always-demand nil
              server-raise-frame t
              is-daemon t)
        (setenv "EDITOR" "emacsclient -c -a=''"))
    (setq use-package-always-defer t))

  (use-package use-package
    :straight use-package
    :config
    (setq use-package-compute-statistics t)
    (setq use-package-verbose t)
    (setq use-package-hook-name-suffix nil)
    (setq use-package-enable-imenu-support t))
  (use-package use-package-ensure-system-package)
  (use-package async)
  )
(progn                                  ; UI base setting
  (use-package bookmark
    :straight (:type built-in)
    :custom
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
    :config
    (setq-default blink-cursor-mode 0       ; Don't blink the cursor
                  buffer-file-coding-system 'utf-8-auto
                  cursor-in-non-selected-windows nil
                  cursor-type '(bar . 3)
                  echo-keystrokes 0.1
                  enable-recursive-minibuffers t ; Enable recursive minibuffers
                  fill-column 80
                  font-lock-maximum-decoration t
                  gc-cons-threshold (* 50 1000 1000)
                  global-mark-ring-max 1024
                  image-use-external-converter t ; 27.1 viewer don't display many png
                  indent-tabs-mode nil  ; use spaces instead of tabs for indentation
                  indicate-empty-lines t
                  inhibit-startup-screen t
                  kept-new-versions 6
                  kept-old-versions 2
                  major-mode 'text-mode
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
    (("M-/" . hippie-expand)
     ("H-;" . comment-box)
     ("H-<backspace>" . kill-whole-line)
     ("H-\\" . indent-region)
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
     ("C-c Q" . save-buffers-kill-emacs))
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
      "Enable ‘auto-fill-mode’ limiting it to comments."
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
     ;; (text-mode . auto-fill-mode)
     (yaml-mode-hook . mk-auto-fill-mode)))

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
    (setq window-sides-vertical nil)
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
           ("H-s" . window-toggle-side-windows)
           ("H-q" . delete-window)            ; emulate i3wm
           ("H-<up>" . windmove-up)
           ("H-<left>" . windmove-left)
           ("H-<down>" . windmove-down)
           ("H-<right>" . windmove-right)))

  (use-package electric
    :straight (:type built-in)
    :config
    (electric-indent-mode 0)
    ;; python is excluded by aggressive indent because of not absolute indentation
    :hook (python-mode-hook . electric-indent-mode))

  (use-package print-settings
    :straight (:type built-in)
    :custom
    (lpr-command "lpr")
    (printer-name "HP_LaserJet_CM1415fn")
    (ps-print-header nil)
    (ps-print-footer nil)
    (ps-print-color-p t)
    (ps-print-header-frame nil)
    (ps-print-banner nil)
    (ps-print-scale 1.0)
    (ps-print-duplex nil))
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
  (set-fontset-font "fontset-default" nil
                    (font-spec :size 20 :name "Symbola"))
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
    (if (not (daemonp))
        (load-theme 'spacemacs-light t)))
  (use-package poet-theme)
  (use-package solarized-theme
    :ensure t
    :init
    (if (daemonp)
        (load-theme 'solarized-selenized-dark t)))
  )
(progn ;; single packages
  (use-package all-the-icons
    :if (display-graphic-p)
    :commands (all-the-icons-material
               all-the-icons-faicon
               all-the-icons-octicon))

  (use-package unfill
    :bind ("C-c M-q" . unfill-toggle))

  (use-package aggressive-indent
    :bind
    ("C-c t i" . aggressive-indent-mode)
    :hook
    (emacs-lisp-mode-hook . aggressive-indent-mode)
    (html-mode-hook . aggressive-indent-mode))

  (use-package delsel
    :init (delete-selection-mode 1))

  (use-package fix-word
    :bind
    ("M-c" . fix-word-capitalize)
    ("M-l" . fix-word-downcase)
    ("M-u" . fix-word-upcase))

  (use-package which-key
    :commands (which-key-mode
               which-key-add-key-based-replacements)
    :bind ("H-<f1>" . which-key-show-top-level)
    :init
    (which-key-mode 1)
    (which-key-add-key-based-replacements "C-c t m" "Toggle mode")
    (which-key-add-key-based-replacements "C-c t o" "Toggle org")
    :config
    (setq which-key-idle-delay 0.05))

  (use-package visual-fill-column
    :commands (visual-fill-column-split-window-sensibly
               visual-fill-column-adjust) ;; although are functions
    :bind (("C-c v" . visual-fill-column-mode)
           ("<f12>" . no-distraction-enable)
           ("<C-f12>" . no-distraction-disable))
    :init
    (setq visual-fill-column-center-text t
          visual-fill-column-width 98
          visual-fill-column-fringes-outside-margins nil
          ;; set right curly arrow even when visual line mode is wrapping logical lines into visual ones.
          visual-line-fringe-indicators '(bottom-left-angle top-right-angle)
          ;; allow splitting windows with wide margins
          split-window-preferred-function #'visual-fill-column-split-window-sensibly)
    :config
    ;; adjust margins upon text resize
    (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
    :hook
    (visual-fill-column-mode-hook . visual-line-mode)
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
    ("C-a" . crux-move-beginning-of-line)
    ("C-o" . crux-smart-open-line)
    ("C-S-o" . crux-smart-open-line-above)
    ("C-H-o" . crux-duplicate-and-comment-current-line-or-region))

  (use-package recentf
    ;; (setq recentf-exclude `(,(expand-file-name "straight/build/" user-emacs-directory)
    ;;                         ,(expand-file-name "eln-cache/" user-emacs-directory)
    ;;                         ,(expand-file-name "var/" user-emacs-directory)))
    :init (recentf-mode t))

  (use-package ace-window
    :demand t
    :bind (("H-w" . ace-window)
           ("C-'" . ace-select-window)
           ("C-\"" . ace-swap-window))
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
          aw-scope 'global              ; 'frame
          aw-dispatch-always nil))

  (use-package transpose-frame
    :bind ("H-E" . transpose-frame))

  (use-package imenu-list               ; F9
    :bind (("<f9>" . imenu-list)
           ("<C-f9>" . imenu-list-smart-toggle)))

  (use-package pass
    :bind ("C-x P" . pass))

  (use-package tzc
    :demand t
    :defines tzc-favourite-time-zones
    :bind (("C-x T c" . tzc-convert-current-time)
           ("C-x T t" . tzc-convert-time-at-mark)
           ("C-x T w" . tzc-world-clock))
    :config
    (setq tzc-favourite-time-zones '("Europe/Rome")))

  (use-package doom-modeline
    :demand t
    :commands (doom-modeline-mode)
    :custom (doom-modeline-minor-modes t)
    :config (doom-modeline-mode 1))

  (use-package minions
    :commands (minions-mode)
    :init (minions-mode 1))

  (use-package smartparens
    :commands smartparens-global-mode
    :init
    (add-hook 'prog-mode-hook #'smartparens-mode)
    (setq sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil)
    :bind (:map smartparens-mode-map
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

  (use-package expand-region
    :bind ("C-=" . er/expand-region))

  (use-package hideshow ;; XXX: for folding
    :bind (("C-c t f" . hs-minor-mode)
           (:map
            prog-mode-map
            ("<backtab>" . hs-toggle-hiding)
            ("H-z" . hs-hide-all)
            ("H-Z" . hs-show-all)))
    :hook (prog-mode-hook . hs-minor-mode))

  (use-package calc
    :bind ("M-g M-a c" . calc))

  (use-package dired
    :straight nil
    :hook (dired-mode-hook . (lambda ()
                               (toggle-truncate-lines)
                               (turn-on-gnus-dired-mode)))
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
(progn ;; hydra, hl-todo and mk-text

  (use-package hydra
    :bind (("C-x C-b" . hydra-buffer/body))
    :commands (hydra-default-pre
               hydra-keyboard-quit
               hydra--call-interactively-remap-maybe
               hydra-show-hint
               hydra-set-transient-map)
    :config
    (defhydra hydra-buffer (:color red :hint nil)
      ("k" kill-this-buffer "kill")
      ("K" kill-buffer-and-window "kill all")
      ("x" delete-window "delete")
      ("s" save-buffer "save")
      ("b" consult-buffer "switch")
      ("e" eval-buffer "evaluate")
      ("<" previous-buffer "previous")
      (">" next-buffer "next")))

  (use-package hl-todo
    :bind (("C-c 2 2" . hydra-2DO/body)
           ("C-c 2 n" . hl-todo-next)
           ("C-c 2 p" . hl-todo-previous)
           ("C-c 2 i" . hl-todo-insert)
           ("C-c 2 o" . hl-todo-occur)
           )
    :config
    (defhydra hydra-2DO (:color pink :hint nil :foreign-keys warn)
      "A hydra for hl-todo."
      ("p" hl-todo-previous "previous")
      ("n" hl-todo-next "next")
      ("o" hl-todo-occur "occur")
      ("i" hl-todo-insert "insert")
      ("q" nil "cancel" :color blue))
    (add-to-list 'hl-todo-keyword-faces '("XXX:" . "#ff8c00"))
    (add-to-list 'hl-todo-keyword-faces '("TODO:" . "#dc143c"))
    (add-to-list 'hl-todo-keyword-faces '("FIXME:" . "#4e9393")))

  ;; https://github.com/mrkkrp/nixos-config/tree/master/imports/emacs
  ;; (straight-use-package '(mk :local-repo "~/.emacs.d/mk/" :branch "vanilla" :includes(mk-text mk-utils)))
  (use-package mk-utils
    :demand t
    :defer 0
    :straight nil
    :load-path "~/.emacs.d/mk")
  (use-package mk-text                  ; XXX: composable or objed
    ;;https://github.com/paldepind/composable.el
    ;;https://github.com/clemera/objed
    :demand t
    :defer 0
    :straight nil
    :load-path "~/.emacs.d/mk/"
    :commands (mk-transpose-line-down
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
  )
(progn                                  ; Completion: vertico.
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
     ("<M-RET>" . minibuffer-force-complete-and-exit)
     ("M-<tab>" . minibuffer-complete)
     )
    :hook
    (minibuffer-setup-hook . vertico-repeat-save)
    :init
    (vertico-mode t)
    :custom
    (vertico-scroll-margin 0)
    (vertico-count 20)
    (vertico-resize t)
    ;; Cycling for `vertico-next' and `vertico-previous'
    (vertico-cycle t))
  (use-package vertico-repeat)

  (use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles basic partial-completion)))))

  (use-package savehist
    :after (vertico)
    :init
    (savehist-mode)
    :config
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

  (use-package emacs
    :bind
    (("C-c l f" . find-library) ;; tmm =M-\`= and Mind mark, bookmark and register
     ("C-c l a" . apropos-library)
     ("C-c l l" . load-library))
    :init
    (which-key-add-key-based-replacements "C-c f" "Files")
    (which-key-add-key-based-replacements "C-c l" "Libraries")
    :preface
    ;; Improve `completing-read-multiple' prompt by adding a prefix.
    (defun crm-indicator (args)
      (cons (concat "[CRM] " (car args)) (cdr args)))
    :config
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
    ;; Make minibuffer prompt read-only and cursor-intangible.
    (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
    ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
    ;; Vertico commands are hidden in normal buffers.
    (setq read-extended-command-predicate #'command-completion-default-include-p)
    )

  (use-package marginalia
    :commands (marginalia-mode)
    :bind (:map minibuffer-local-map
                ("M-A" . marginalia-cycle))
    :init (marginalia-mode))

  ;; (use-package consult
  ;;   :custom
  ;;   (completion-in-region-function #'consult-completion-in-region)
  ;;   (completions-format 'vertical)
  ;;   :bind ((:map minibuffer-local-map
  ;;                ("C-r" . consult-history))
  ;;   :init
  ;;   (advice-add #'completing-read-multiple
  ;;               :override #'consult-completing-read-multiple))

  (use-package consult
    :commands (consult--customize-put
               consult--customize-set
               consult-completion-in-region)
    :bind (("C-/" . consult-line)
           ("C-c f f" . consult-find)
           ("C-c f F" . consult-locate)
           ("C-c f z" . (lambda () (interactive)(cd "~/")(consult-find)))
           ("C-c f r" . consult-recent-file)
           ("C-c f e" . consult-file-externally)
           ("M-g T" . consult-theme)
           ("M-g M" . consult-minor-mode-menu)
           ("C-c m" . consult-mode-command)
           ("C-c k" . consult-kmacro)
           ;; C-x bindings (ctl-x-map)
           ("C-x :" . consult-complex-command)       ;; C-x M-: repeat-complex-command
           ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
           ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-\"" . consult-register-store)          ;; M-' orig. abbrev-prefix-mark (unrelated)
           ("H-M-'" . consult-register)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ("<help> a" . consult-apropos)            ;; orig. apropos-command
           ;; M-g bindings (goto-map)
           ("M-g E" . consult-compile-error)
           ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
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
           ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
           ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
           ("M-s M-l" . consult-line-multi)           ;; needed by consult-line to detect isearch
           )
    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI. You may want to also
    ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
    :hook (completion-list-mode-hook . consult-preview-at-point-mode)
    ;; :init
    ;; ;; `consult-register-store' and the Emacs built-ins.
    ;; (setq register-preview-delay 0.5
    ;;       register-preview-function #'consult-register-format)
    ;; (advice-add #'register-preview :override #'consult-register-window)
    ;; (setq xref-show-xrefs-function #'consult-xref
    ;;          xref-show-definitions-function #'consult-xref)
    :config
    ;; Use `consult-completion-in-region' if Vertico is enabled.
    ;; Otherwise use the default `completion--in-region' function.
    (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args)))
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme :preview-key '(:debounce 0.4 any)
     consult-ripgrep consult-git-grep consult-org-agenda ;consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
     :preview-key "<right>")
    ;; (setq consult-ripgrep-args
    ;; "rg --hidden --null --line-buffered --color=never --max-columns=1000 --path-separator /  --smart-case --no-heading --line-number .")
    ;; Narrowing key. Both < and C-+ work reasonably well.
    (setq consult-narrow-key "C-+")
    )

  (use-package embark-consult
    :demand t ; only necessary if you have the hook below
    ;; if you want to have consult previews as you move around an
    ;; auto-updating embark collect buffer
    :hook
    (embark-collect-mode-hook . consult-preview-at-point-mode))

  (use-package embark
    :commands (embark--truncate-target
               embark-completing-read-prompter)
    :functions (which-key--hide-popup-ignore-command
                which-key--show-keymap)
    :bind (("C-." . embark-act)
           ("C->" . embark-act-noquit)
           ("M-." . embark-dwim)       ; orig. xref-find-definition
           ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
           :map minibuffer-local-completion-map
           ("H-e" . embark-export)
           ("H-b" . embark-become)
           :map minibuffer-local-map
           ("H-e" . embark-export)
           ("H-b" . embark-become))
    :preface
    (defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator immediately when using the
completing-read prompter."
      (which-key--hide-popup-ignore-command)
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))

    (defun embark-act-noquit ()
      "Run action but don't quit the minibuffer afterwards."
      (interactive)
      (let ((embark-quit-after-action nil))
        (embark-act)))

    :config
    (setq embark-indicators
          '(embark-which-key-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))
    (advice-add #'embark-completing-read-prompter
                :around #'embark-hide-which-key-indicator))

  (use-package affe
    :after (orderless)
    :bind (("C-x x" . affe-find)
           ("C-x X" . affe-grep))
    ;; :custom ((affe-regexp-function #'orderless-pattern-compiler)
    ;;       (affe-highlight-function #'orderless-highlight-matches))
    )

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
    )
  (use-package dabbrev
    ;; Swap M-/ and C-M-/
    :bind (("M-/" . dabbrev-completion)
           ("C-M-/" . dabbrev-expand))
    ;; Other useful Dabbrev configurations.
    :custom
    (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))
  (use-package corfu
    :hook (;;(prog-mode-hook . corfu-mode)
           (org-mode-hook . corfu-mode))
    :bind
    (:map corfu-map
          ("C-n" . corfu-next)
          ("C-p" . corfu-previous))
    :custom
    (corfu-cycle t)
    ;; (corfu-auto t)
    ;; (corfu-separator ?\s)
    (corfu-quit-no-match t)
    (corfu-commit-predicate nil)
    :config
    (setq corfu-preselect t)
    ;; Already set
    ;; (setq read-extended-command-predicate #'command-completion-default-include-p)
    )
  (use-package corfu-doc
    :bind (:map corfu-map
                ("C-h" . corfu-doc-toggle)
                ("C-n" . corfu-doc-scroll-down)
                ("C-p" . corfu-doc-scroll-up))
    ;; :config
    ;; (setq corfu-doc-delay 0.2
    ;;      corfu-doc-max-width 80
    ;;      corfu-doc-max-height 40)
    ;; :init
    ;; (corfu-doc-mode +1)
    )
  )
(use-package yasnippet ;; Yasnippet
  :bind
  ("M-g Y a" . yas-reload-all)
  ("M-g Y n" . yas-new-snippet)
  ("M-g Y v" . yas-visit-snippet-file)
  ("C-c t y" . yas-minor-mode)
  ;; ;; disable yas minor mode map ;; use hippie-expand instead [sp]
  ;; (setq yas-minor-mode-map (make-sparse-keymap))
  :init
  (which-key-add-key-based-replacements "M-g Y" "Yasnippet")
  :hook
  (prog-mode-hook . yas-minor-mode)
  (org-mode-hook . yas-minor-mode)
  (message-mode-hook . yas-minor-mode)
  (markdown-mode-hook . yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippets")
        yas-triggers-in-field t
        yas-wrap-around-region t)     ;or [a-z] register
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))
(use-package yasnippet-snippets
  :after yasnippet)
(use-package consult-yasnippet
  :after yasnippet
  :bind ("M-s y" . consult-yasnippet))

(progn ;; Spell checking and writing
  (use-package ispell
    :hook ((text-mode-hook . flyspell-mode)
           (org-mode-hook . flyspell-mode)
           (prog-mode-hook . flyspell-prog-mode)
           (LaTeX-mode-hook . flyspell-mode)
           (change-log-mode-hook . (lambda () (flyspell-mode -1)))
           (log-edit-mode-hook . (lambda () (flyspell-mode -1))))
    :config
    (setq ispell-program-name (executable-find "hunspell")
          ispell-really-hunspell t
          ispell-silently-savep t       ; Save personal dictionary silently
          ispell-dictionary "en_US-large")
    (add-to-list 'ispell-skip-region-alist
                 '("^From:" . "line--$")) ; Skip email headers and quoted text
    ;; ("^\\[\\[" . "^\\]\\]") ; Skip org-mode links
    ;; ("^#+BEGIN_SRC" . "^#+END_SRC")
    )

  (use-package flyspell
    :ensure nil
    :after ispell
    :hook ((text-mode-hook . flyspell-mode)
           (org-mode-hook . flyspell-mode)
           (prog-mode-hook . flyspell-prog-mode)
           (LaTeX-mode-hook . flyspell-mode)
           (change-log-mode-hook . (lambda () (flyspell-mode -1)))
           (log-edit-mode-hook . (lambda () (flyspell-mode -1))))
    :bind (("C-c t s" . flyspell-mode)
           ("C-c t S" . flyspell-correct-auto-mode)
           :map flyspell-mode-map
           ("H-," . flyspell-auto-correct-previous-word) ; I mostly use flyspell-correct
           ("C-;" . nil)                ; to avoid conflicts
           ("C-." . nil)
           ("C-," . nil)))

  (use-package flyspell-correct
    :after (flyspell)
    :bind (:map flyspell-mode-map
                ("C-M-i" . flyspell-correct-wrapper)))

  (use-package consult-flyspell
    :bind (("M-g s" . consult-flyspell)))

  (use-package cm-mode ;; critic markup
    ;; :hook (text-mode . cm-mode)
    :bind
    ("<f7> M" . cm-mode)
    ("<f7> m" . cm-prefix-map))

  (use-package guess-language
    ;; For multi language within same doc.
    :bind
    ("C-c t g" . guess-language-mode)
    ("C-c S e" . (lambda () (interactive)
                   (ispell-change-dictionary "en_US-large")
                   (flyspell-buffer)))
    ("C-c S i" . (lambda () (interactive)
                   (ispell-change-dictionary "it_IT")
                   ;; TODO: cape (ispell|dict)
                   (flyspell-buffer)))
    ;; :hook
    ;; (flyspell-mode . guess-language-mode)
    ;; ;; (flyspell-mode-prog . guess-language-mode)
    ;; (text-mode . guess-language-mode)
    :functions guess-language-switch-function
    :init
    (which-key-add-key-based-replacements "C-c S" "Spell")
    :config
    (setq guess-language-langcodes '((en . ("en_US-large" "English"))
                                     (it . ("it_IT" "Italian")))
          guess-language-min-paragraph-length 15
          guess-language-languages '(en it))
    ;; (defun guess-language-switch-function (lang beginning end)
    ;;   "Switch additional dictionaries. LANG is the ISO 639-1 code of the language
    ;;    (as a symbol). BEGINNING and END are the endpoints of the region in which
    ;;    LANG was detected but these are ignored."
    ;; (when (and (featurep 'festival)
    ;;       (festivalp))
    ;;  (pcase lang
    ;;    ('en (festival-voice-english-female))
    ;;    ('it (festival-voice-italian-female))))
    ;;   )
    ;; (add-hook 'guess-language-after-detection-functions
    ;; #'guess-language-switch-function)
    )

  (use-package apheleia
    :demand t
    :commands apheleia-global-mode
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
    (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier)))

  (use-package flycheck ;; Syntax checking
    :commands (flycheck-add-next-checker)
    :bind (("M-g e l" . flycheck-list-errors)
           ("M-g e e" . flycheck-mode)
           ("M-g e b" . flycheck-buffer)
           ("M-g e d" . flycheck-clear)
           ("M-g e h" . flycheck-describe-checker)
           ("M-g e n" . flycheck-next-error)
           ("M-g e p" . flycheck-previous-error)
           ("M-g e s" . flycheck-select-checker)
           ("M-g e S" . flycheck-set-checker-executable)
           ("M-g e v" . flycheck-verify-setup)
           ("M-g e y" . flycheck-copy-errors-as-kill)
           ("M-g e x" . flycheck-explain-error-at-point))
    :hook ((gitignore-mode-hook . flycheck-mode)
           (markdown-mode-hook . flycheck-mode)
           ;; (org-mode-hook . flycheck-mode)
           ;; (text-mode-hook . flycheck-mode)
           (prog-mode-hook . flycheck-mode)
           (yaml-mode-hook . flycheck-mode))
    :init
    (which-key-add-key-based-replacements "M-g e" "Errors(flycheck)")
    :config
    (setq flycheck-idle-change-delay nil) ; Disable idle checking to avoid performance problems
    (setq-default flycheck-emacs-lisp-load-path 'inherit
                  flycheck-temp-prefix ".flycheck"))

  (use-package consult-flycheck
    :bind ("M-g f" . consult-flycheck))

  (use-package flycheck-vale
    :after flycheck
    :commands (flycheck-vale-setup
               flycheck-vale-toggle-enabled)
    :init
    (flycheck-vale-toggle-enabled)
    :config
    (flycheck-vale-setup)
    (flycheck-add-next-checker 'vale 'proselint)
    (flycheck-add-next-checker 'proselint 'markdown-markdownlint-cli)
    (setq flycheck-checker-error-threshold 1000))

  (use-package langtool
    :commands (langtool-goto-previous-error
               langtool-goto-next-error
               langtool-check
               langtool-correct-buffer
               langtool-check-done
               langtool-switch-default-language)
    :bind ("<f7> l" . hydra-LT/body)
    :init
    (which-key-add-key-based-replacements "<f7>" "Writing")
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
          langtool-disabled-rules '("EN_UNPAIRED_BRACKETS"
                                    "MORFOLOGIK_RULE_EN_US")
          langtool-mother-tongue "it"
          langtool-default-language "en-US"))

  (use-package sdcv
    :bind
    ("<f7> s" . sdcv-search-pointer)
    (:map sdcv-mode-map
          ("n" . sdcv-next-dictionary)
          ("p" . sdcv-previous-dictionary)))
  (use-package wordnut
    :bind
    ("<f7> w" . wordnut-lookup-current-word)
    ("<f7> W" . wordnut-search))
  (use-package powerthesaurus
    :bind
    ("<f7> p 0" . powerthesaurus-lookup-dwim)
    ("<f7> p p" . powerthesaurus-lookup-synonyms-dwim)
    ("<f7> p a" . powerthesaurus-lookup-antonyms-dwim)
    ("<f7> p d" . powerthesaurus-lookup-definitions-dwim)
    ("<f7> p r" . powerthesaurus-lookup-related-dwim)
    ("<f7> p s" . powerthesaurus-lookup-sentences-dwim))

  (use-package academic-phrases
    :bind
    ("<f7> i" . academic-phrases-by-section)
    ("<f7> I" . academic-phrases))
  (use-package writegood-mode
    :bind
    ("<f7> g" . writegood-mode)
    ("<f7> Gl" . writegood-grade-level)
    ("<f7> Gr" . writegood-reading-ease))
  (use-package typo
    :bind ("C-c t t" . typo-mode)
    :commands typo-global-mode
    :config (typo-global-mode)          ; Complement `C-x 8`
    :hook
    (org-mode-hook . (lambda () (typo-mode -1)))
    (text-mode-hook . typo-mode))


  (use-package google-translate
    :defines google-translate-translation-directions-alist
    :commands (google-translate-at-point
               google-translate-at-point-reverse)
    :bind
    ("<f7> t" . google-translate-smooth-translate)
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
    ;; API changed
    (setq google-translate-backend-method 'curl))
  )
(use-package mu4e                       ; mu4e
  :demand is-daemon
  :straight (:type built-in)            ; in AUR/mu
  :commands (mu4e-compose-new
             mu4e-context-current)
  :hook
  ;; (mu4e-view-mode-hook . variable-pitch-mode)
  (mu4e-compose-mode-hook . (lambda()
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
                              (guess-language-mode)))
  :bind
  (("M-g M-a m" . mu4e)
   ("C-x m" . mu4e)
   :map mu4e-compose-mode-map
   ("C-c o a" . mu4e-compose-attach-captured-message)
   ("C-c o b" . message-goto-bcc)
   ("C-c o c" . message-goto-cc)
   ("C-c o m" . message-goto-body)
   ("C-c o s" . message-goto-subject)
   ("C-c o t" . message-goto-to)
   :map mu4e-view-mode-map
   ("f" . mu4e-headers-mark-for-flag)
   ("<tab>"     . org-next-link)    ; 'shr-next-link
   ("<backtab>" . org-previous-link)    ; 'shr-previous-link
   ("G"         . end-of-buffer)
   ("V"         . mu4e-view-verify-msg-popup)
   ("v"         . visual-fill-column-mode)
   ("C-c l" . org-store-link)         ; requires ol.el
   :map mu4e-headers-mode-map
   ("G"         . end-of-buffer)
   ("D" . "T d")
   ("f"         . mu4e-headers-mark-for-flag)
   )
  :config
  ;; (when (fboundp 'imagemagick-register-types) (imagemagick-register-types))
  ;; New feature as of mu=1.10.0
  ;; (setq mu4e-read-option-use-builtin nil
  ;;        mu4e-completing-read-function 'completing-read)
  (set-variable 'read-mail-command 'mu4e) ;; use mu4e as Default
  (setq mail-user-agent 'mu4e-user-agent  ;; use mu4e as Default
        mu4e-maildir (expand-file-name "~/Maildir")
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 30
        mu4e-index-cleanup nil      ;; don't do a full cleanup check
        mu4e-index-lazy-check t)    ;; don't consider up-to-date dirs
  (auth-source-pass-enable)
  (setq auth-source-debug t
        auth-source-do-cache nil
        auth-sources '(password-store))
  (setq smtpmail-queue-mail  nil
        smtpmail-queue-dir   "~/Maildir/queue/cur") ; Remember to "mu mkdir" and "touch /queue/.noindex"
  ;; (setq smtpmail-debug-info t)
  (setq mu4e-attachment-dir "~/"
        mu4e-change-filenames-when-moving t ; rename files when moving (Needed for mbsync)
        mu4e-completing-read-function 'completing-read ; use convenient completion for navigation =j o=
        mu4e-compose-keep-self-cc nil ;default
        mu4e-context-policy 'pick-first ; start with the first (default) context;
        mu4e-headers-include-related t ; =W= now  'P r' to view threads
        mu4e-headers-skip-duplicates nil ; =V= now 'P u' to skip duplicates
        mu4e-view-show-images t ; enable inline images and VIEW
        mu4e-confirm-quit nil
        mu4e-hide-index-messages t  ; hide updating messages
        mu4e-use-fancy-chars t
        mu4e-compose-forward-as-attachment nil
        mu4e-view-show-addresses t      ; show full addresses
        mu4e-headers-leave-behavior 'ask ; default while 'apply leaving headers view apply all marks
        mu4e-save-multiple-attachments-without-asking t
        fill-flowed-encode-column 998 ; https://www.ietf.org/rfc/rfc2822.txt
        shr-color-visible-luminance-min 80)
  (setq mu4e-maildir-shortcuts          ; Shortcuts
        '(("/cnr/INBOX"         . ?i)
          ("/gmail/Inbox"       . ?j)
          ("/gmail/archive"     . ?g)
          ("/cnr/Sent"          . ?s)
          ("/cnr/Templates"     . ?t)
          ("/archive"           . ?a)
          ("/archives/personal" . ?p)
          ("/cnr/refs"          . ?r)
          ("/cnr/keepup"        . ?k)
          ("/cnr/Drafts"        . ?d)))
  (setq mu4e-bookmarks                  ; Bookmarks
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
          ;; (:query ,(mapconcat 'identity
          ;;                     (mapcar
          ;;                      (lambda (maildir)
          ;;                       (concat "maildir:" (car maildir)))
          ;;                      mu4e-maildir-shortcuts) " OR ")
          ;;         :name "All inboxes" :key ?i)
          ))
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
                        ;; ( smtpmail-smtp-user      . "daniele.arosio@cnr.it" )
                        ;; ( smtpmail-smtp-server    . "smtp.cnr.it" )
                        ;; ;; ( smtpmail-smtp-service   . 587 )
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
                        ;; ( smtpmail-smtp-user      . "danielepietroarosio@gmail.com" )
                        ;; ( smtpmail-smtp-server    . "smtp.gmail.com" )
                        ;; ( user-full-name          . "daniele arosio" )
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
                        ;; ( smtpmail-smtp-user      . "daniele.arosio@postecert.it" )
                        ;; ( smtpmail-smtp-server    . "mail.postecert.it" )
                        ;; ( smtpmail-smtp-service   . 465 )
                        ;; ( smtpmail-stream-type 'plain)
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
    (setq ;; Save msg into sent folder only for pec
     mu4e-sent-messages-behavior
     (lambda ()
       (if (string= (message-sendmail-envelope-from) "daniele.arosio@postecert.it")
           'sent
         'delete)))
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
    (setq mu4e-headers-fields   '((:human-date     .  12)
                                  (:flags          .   6)
                                  (:size           .   7)
                                  (:mailing-list . 10)
                                  (:from           .  20)
                                  (:thread-subject)
                                  ))
    (setq mu4e-headers-auto-update t)   ; default
    (setq mu4e-headers-visible-lines 10)
    )

  (use-package message
    :straight (:type built-in)
    :functions (message-sendmail-envelope-from
                message-add-header
                message-remove-header)
    :config
    (setq message-kill-buffer-on-exit t ; don't keep message buffers around
          ;; message-send-mail-function 'smtpmail-send-it ; when using queue
          message-send-mail-function 'message-send-mail-with-sendmail
          message-sendmail-envelope-from 'header
          message-sendmail-f-is-evil nil
          message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n"
          message-citation-line-function 'message-insert-formatted-citation-line))

  (use-package sendmail
    :straight (:type built-in)
    :config
    (setq mail-specify-envelope-from t
          mail-envelope-from 'header
          mail-interactive t))

  (use-package mu4e-mark ; Tags and personal archive
    :straight mu4e
    :after mu4e
    :bind (:map
           mu4e-headers-mode-map
           ("M-z" . mu4e-headers-mark-for-tag)
           ("M-p" . mu4e-headers-mark-for-personal)
           :map
           mu4e-view-mode-map
           ("M-z" . mu4e-view-mark-for-tag)
           ("M-p" . mu4e-view-mark-for-personal))
    :config
    (add-to-list 'mu4e-marks
                 ;; https://gist.github.com/lgatto/7091552
                 '(tag
                   :char       "M-z"
                   :prompt     "gtag"
                   :ask-target (lambda () (read-string "What tag do you want to add?"))
                   :action      (lambda (docid msg target)
                                  (mu4e-action-retag-message msg (concat "+" target)))))
    (add-to-list 'mu4e-marks
                 '(personal
                   :char       "M-p"
                   :prompt     "personal"
                   :show-target (lambda (target) "personal")
                   :action      (lambda (docid msg target)
                                  ;; must come before proc-move since retag runs
                                  ;; 'sed' on the file
                                  (mu4e-action-retag-message msg "-\\Inbox")
                                  (mu4e--server-move docid "/archives/personal" "+S-u-N"))))
    (mu4e~headers-defun-mark-for tag)
    (mu4e~headers-defun-mark-for personal)
    ;; FIXME
    ;; (mu4e~view-defun-mark-for tag)
    ;; (mu4e~view-defun-mark-for personal)
    )

  (use-package org-msg
    ;; :demand t
    :after (mu4e)
    :bind (
           :map mu4e-main-mode-map
           ("C-c C-o" . org-msg-mode)
           :map  mu4e-headers-mode-map
           ("C-c C-o" . org-msg-mode)
           :map mu4e-view-mode-map
           ("C-c C-o" . org-msg-mode))
    :config
    ;; (org-msg-mode)
    (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil"
          org-msg-startup "hidestars indent inlineimages"
          org-msg-greeting-fmt "\nHi%s,\n\n"
          org-msg-greeting-name-limit 3
          org-msg-convert-citation t)
    (setq org-msg-default-alternatives '((new       . (utf-8))
                                         (reply-to-html . (utf-8 html))
                                         (reply-to-text . (utf-8))))
    (setq org-msg-signature "
#+begin_signature
  --  daniele \\\\
#+end_signature"))

  (use-package mu4e-org :straight mu4e
    :config
    (setq mu4e-org-link-query-in-headers-mode t ; `C-c l` store query
          org-mu4e-convert-to-html t))

  (use-package org-mime
    :bind (
           :map mu4e-compose-mode-map
           ("C-c M-O" . org-mime-edit-mail-in-org-mode)
           ("C-c M-o" . org-mime-htmlize)
           :map org-mode-map
           ("C-c M-o" . org-mime-org-subtree-htmlize)
           ("C-c M-O" . org-mime-org-buffer-htmlize))
    :hook
    (org-mime-html-hook . (lambda ()
                            (org-mime-change-element-style
                             "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                                           "#E6E1DC" "#232323")))) ; "darkred" "burlywood"
    ;; the following can be used to nicely offset block quotes in email bodies
    (org-mime-html-hook . (lambda ()
                            (org-mime-change-element-style
                             "blockquote" "border-left: 2px solid gray; padding-left: 4px;")))
    ;; (message-send-hook . org-mime-confirm-when-no-multipart)
    (mu4e-compose-mode-hook . (lambda ()(require 'org-mime))) ; work w/out server
    :config
    (setq org-mime-library 'semi
          org-mime-export-ascii 'utf-8
          org-mime-export-options '(
                                    :with-latex dvipng
                                    :section-numbers nil
                                    :with-author nil
                                    :with-toc nil)))
  )
(use-package mu4e-jump-to-list
  :after mu4e)
(progn                                  ; org
  (setenv "PYDEVD_DISABLE_FILE_VALIDATION" "1") ; https://github.com/nnicandro/emacs-jupyter/issues/439
  (use-package jupyter
    :straight (:no-native-compile t :no-byte-compile t)
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
                                                         (:kernel . "python3"))))
  (use-package org
    :demand 2
    :commands (org-capture-finalize
               org-speed-move-safe
               org-narrow-to-subtree
               org-clock-in)
    :functions (org-read-date
                org-get-tags
                org-entry-delete
                org-entry-put
                org-toggle-tag)
    :defines (org-state
              da-agenda-files
              da-refile-files)
    :init
    (defvar da-gtd "~/Sync/box/org/gtd.org")
    (setq-default da-agenda-files
                  (append '("~/Sync/box/org/gtd.org"
                            "~/Sync/box/org/gcal/dpa.org"
                            "~/Sync/box/org/gcal/figli.org"
                            "~/Sync/box/org/ideas.org"
                            "~/Sync/box/org/inbox.org"
                            "~/Sync/box/org/inbox.box.org"
                            "~/Sync/box/org/journal.org"
                            "~/Sync/box/org/projects.org")))
    ;; "~/Sync/box/org/TODOs.org" ;; target for org-projectile REVIEW:
    ;; "~/Sync/box/org/shopping.org")
    (setq-default da-refile-files
                  (append (directory-files "~/Sync/proj/" t "\\.org$")
                          (directory-files "~/Sync/notes/home/" t "\\.org$")
                          (directory-files-recursively "~/Sync/notes/arch/" "\\.org$") ; org files in all sub folders
                          (directory-files-recursively "~/Sync/notes/org-roam/" "\\.org$")))
    :preface
    (defun da-consult-org ()
      (interactive)
      (let ((org-agenda-files (append da-agenda-files da-refile-files)))
        (consult-org-agenda)))
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
     ("M-s M-a" . da-consult-org)
     ("M-S-h" . org-mark-element)
     :map
     org-mode-map
     ("H-<return>" . org-next-link)
     ("H-S-<return>" . org-previous-link)
     ("<C-S-left>" . nil)
     ("<C-S-right>" . nil)
     ("M-g ; ;" . org-capture-goto-last-stored) ; `C-x r b` for bookmarks
     ("M-g ; :" . org-refile-goto-last-stored)
     ("C-c t o i" . org-indent-mode))
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
    (consult-customize
     da-consult-org
     :preview-key "<right>")
    (use-package prog-mode
      :straight (:type built-in)
      :config
      (setq-default prettify-symbols-alist '(("#+begin_src" . "➙")
                                             ("#+end_src" . "⇚")
                                             ("#+BEGIN_SRC" . "⤐")
                                             ("#+END_SRC" . "⌗")
                                             ("#+RESULTS:" . "⚡")
                                             ("=>" . "⇨")))
      (setq prettify-symbols-unprettify-at-point 'right-edge))
    (use-package org-lint :straight org
      :bind (:map
             org-mode-map
             ("M-g e o" . org-lint))
      )
    ;; (use-package org-compat :straight org
    ;;   :config
    ;;   (org-add-link-type "mpv" (lambda (path) (browse-url-xdg-open path)))
    ;;   )
    (use-package org-attach :straight org
      :config
      (setq org-attach-use-inheritance t)
      )
    (use-package ol :straight org
      :bind
      (:map
       org-mode-map
       ("C-c l" . org-store-link)
       ("C-c L" . org-toggle-link-display)
       ("C-c C-L" . org-insert-link)
       ("C-c C-l" . org-insert-last-stored-link))
      :config
      (setq org-link-keep-stored-after-insertion t)
      (org-link-set-parameters "mpv" :follow (lambda (path) (browse-url-xdg-open path)))
      )
    (use-package org-indent :straight org
      :config
      (setq org-indent-indentation-per-level 1)
      )
    (setq-default org-startup-indented t)
    (setq org-refile-use-cache t)
    (setq org-reverse-note-order nil)   ; default ;;
    (setq-default org-image-actual-width 620) ;; nil ; so you can specify :width
    ;; org-image-actual-width (/ (display-pixel-width) 4)
    (setq                               ; org-*.el buffers with babel
     org-return-follows-link t
     org-cycle-separator-lines 2              ; default=2
     org-src-window-setup 'current-window
     org-src-preserve-indentation t           ; indentation in src blocks
     org-edit-src-content-indentation 2         ; default
     org-src-tab-acts-natively t)                ; tab in src blocks

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
    (use-package ox-latex
      :straight nil
      :defer t ;; XXX: https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-LaTeX.html
      :config
      (setq org-latex-remove-logfiles t)
      (add-to-list 'org-latex-logfiles-extensions "_minted-")
      (add-to-list 'org-latex-logfiles-extensions "bbl")
      (add-to-list 'org-latex-logfiles-extensions "pyg")
      (setq org-latex-pdf-process
            '("latexmk -pdflatex='xelatex -shell-escape -interaction nonstopmode' -pdf -bibtex -f %f -outdir=%o -verbose"
              "latexmk -c %b"))
      ;; Use the 'minted' package instead of 'listings' for code blocks
      (setq org-latex-listings 'minted)
      (add-to-list 'org-latex-packages-alist '("" "minted"))
      (add-to-list 'org-latex-packages-alist '("" "color"))
      (setq org-latex-minted-options
            '(("frame" "lines") ("autogobble") ("breaklines" "true")
              ;; ("fontsize" "\\small") ("baselinestretch" "1.2") ("no-messge") ("mathescape") ("tabsize" "4")
              ;; ("breakanywhere" "true") ("numbersep" "0.5cm") ("xleftmargin" "1cm") ("frame" "single")("linenos")
              ("style" "colorful")))
      (add-to-list 'org-latex-classes
                   '("draft" "\\documentclass[12pt]{article}
                     \\usepackage{setspace}
                     \\usepackage{tocloft}
                     \\usepackage{lineno}
                     \\usepackage{mathptmx}
                     \\tolerance=1000
                     \\setlength{\\parskip}{4pt}
                     \\setlength{\\parindent}{0pt}
                     \\linespread{1.2}
                     \\usepackage[left=1in, right=1in, top=1in, bottom=1in]{geometry}
                     \\renewcommand{\\cftsecleader}{\\cftdotfill{\\cftdotsep}}
                     \\usepackage{fancyhdr}
                     \\date{}
                     \\pagestyle{fancy}
                     \\fancyhf{}
                     \\rhead{Draft version: \\today}
                     \\cfoot{\\thepage}
                     [NO-DEFAULT-PACKAGES]
                     [PACKAGES]
                     [EXTRA]
                     "
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
      )
    (use-package ox-beamer :straight org :init (eval-after-load 'ox '(require 'ox-beamer)))
    (use-package ox-md :straight org :init (eval-after-load 'ox '(require 'ox-md)))
    (use-package ox-koma-letter :straight org :init (eval-after-load 'ox '(require 'ox-koma-letter)))
    (use-package ox-reveal
      ;; https://www.yanboyang.com/revealslides/
      ;; https://opensource.com/article/18/2/org-mode-slides
      :config
      ;; (setq org-reveal-root "file:///home/dan/.pandoc/reveal.js")
      ;; npm install --save reveal.js-menu
      ;; npm install --save reveal.js
      (setq org-reveal-root "file:///home/dan/node_modules/reveal.js")
      :init
      (eval-after-load 'ox '(require 'ox-reveal))
      )
    (use-package ox-rst :init (eval-after-load 'ox '(require 'ox-rst)))
    (use-package ox-pandoc :init (eval-after-load 'ox '(require 'ox-pandoc)))
    (use-package ox-twbs :init (eval-after-load 'ox '(require 'ox-twbs)))
    (use-package auctex
      :defer t
      :mode
      ("\\.tex\\'" . TeX-latex-mode)
      ("\\.latex\\'" . TeX-latex-mode)
      :config
      (use-package tex
        :straight auctex
        :config
        (setq TeX-auto-save t)
        (setq TeX-parse-self t)
        ;; (setq TeX-PDF-mode t))
        (setq-default TeX-master nil))
      (use-package tex-buf
        :straight auctex
        :config
        (setq TeX-save-query nil))
      )
    (use-package cdlatex)
    ;; (use-package ob-exp
    ;;  :straight nil
    ;;  :config (setq org-export-use-babel nil)) ; Same of :eval never-export in header.
    (add-to-list 'org-structure-template-alist '("sl" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("sb" . "src sh :results output :exports both"))
    (add-to-list 'org-structure-template-alist '("sB" . "src sh :session bash :results output :exports both"))
    (add-to-list 'org-structure-template-alist '("sj" . "src jupyter-python"))
    ;; org-babel-default-header-args:sh    '((:results . "output replace"))
    ;; org-babel-default-header-args:bash  '((:results . "output replace"))
    ;; org-babel-default-header-args:shell '((:results . "output replace"))
    (setq org-pretty-entities t)
    (setq org-confirm-babel-evaluate nil) ; don't prompt to confirm evaluation every time
    (setq org-hide-emphasis-markers t)              ; a better word processor
    (setq org-highlight-latex-and-related '(latex)) ; Change color of inline latex $y=mx+c$
    (setq org-src-fontify-natively t)               ; font in src blocks
    (setq org-columns-default-format
          "%48ITEM(Task) %TODO(todo) %ALLTAGS %SCHEDULED %6Effort(Effort){:} %6CLOCKSUM{:} %DEADLINE")
    (setq org-M-RET-may-split-line
          '((default . t)
            (headline . nil)
            (item . nil)
            (table . nil)))
    (setq org-fontify-done-headline t)
    (setq org-fontify-whole-heading-line t)
    (setq org-enforce-todo-dependencies t)
    (setq org-enforce-todo-checkbox-dependencies t)
    (setq org-track-ordered-property-with-tag t)
    (setq org-special-ctrl-a/e t)
    (setq org-special-ctrl-k t)
    (setq org-loop-over-headlines-in-active-region nil)
    (setq org-fontify-quote-and-verse-blocks t)
    ;; my-GTD
    (setq org-directory "~/Sync/box/org")
    ;; (1) Agenda files
    (setq org-agenda-files da-agenda-files )
    ;; (2) Archives
    (setq org-archive-location "~/Sync/notes/org-archives/%s_archive::")
    (setq org-agenda-text-search-extra-files `(agenda-archives)) ; Search also in archives
    (use-package org-archive :straight org
      :config
      (setq org-archive-file-header-format "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n"))
    ;; (3) Refile
    (use-package org-refile :straight org
      :config
      (setq org-refile-use-outline-path 'file) ; Full path preceded by filename
      (setq org-outline-path-complete-in-steps nil) ; Complete directly with consult
      (setq org-refile-allow-creating-parent-nodes 'confirm) ; Ask confirmation when creating parent tasks
      (setq org-refile-targets
            '(
              ;; ("~/Sync/box/org/shopping.org" :maxlevel . 5)
              (da-refile-files :maxlevel . 5)
              (da-agenda-files :maxlevel . 5)))
      )
    ;; (setq                               ; (4) Stuck project
    ;;  org-stuck-projects '("+proj/-DONE-HOLD-MAYB-PASS-WAIT" ("NEXT") nil ""))
    ;; (5) Todo states
    (setq org-todo-keywords
          ;; tracking state changes @: note !:date entering/leaving
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "WAIT(w@/!)" "PASS(p!/!)" "HOLD(h@/@)" "MAYB(m/@)" "|" "CANC(c@/@)")))
    (setq org-use-fast-todo-selection t)
    (setq org-treat-S-cursor-todo-selection-as-state-change nil) ; no log here
    (setq org-log-into-drawer t)
    (setq org-log-done 'time)
    (setq org-log-note-clock-out nil)
    (setq org-log-redeadline nil)
    (setq org-log-reschedule nil)
    (setq org-read-date-prefer-future 'time)
    ;; (5) Tags for contexts
    (setq org-tag-alist nil)                  ; default
    (setq org-tag-persistent-alist
          '((:startgroup . nil) ; mutually exclusive
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
            ))
    (setq org-fast-tag-selection-single-key t) ; 'expert doesn't show
    (setq org-fast-tag-selection-include-todo nil)
    (setq org-tags-column -82)
    (setq org-support-shift-select t)       ;do not change state with left right arrow
    ;; (5) Faces
    (use-package org-faces :straight org
      :config
      (setq org-todo-keyword-faces
            '(("TODO" :foreground "crimson" :weight bold :box (:line-width 2 :style released-button))
              ("NEXT" :foreground "light blue" :weight bold :box (:line-width 2 :style released-button))
              ;; ("APPT" :foreground "yellow" :weight bold)
              ("WAIT" (:foreground "orange" :weight bold))
              ("PASS" :foreground "SpringGreen" :weight bold)
              ("HOLD" :foreground "SaddleBrown" :weight bold )
              ("MAYB" :foreground "MediumAquamarine" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("CANC" (:foreground "forest green" :weight bold :strike-through t))
              ))
      (setq org-tag-faces
            '(("WORK" :foreground "green")
              ("PERSONAL" :foreground "orange")
              ("proj" :weight bold)
              ("@fbk" :weight italic))))
    ;; Captures
    (setq org-default-notes-file "~/Sync/box/org/inbox.org")
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
      (org-after-refile-insert-hook . save-buffer)
      :config
      (setq org-capture-templates
            '(
              ("t" "Todo simple entry" entry (file org-default-notes-file)
               "* TODO %?\n%[~/.emacs.d/templates/da-property-string]\n")
              ("f" "Fast capture and exit" entry (file org-default-notes-file)
               "* TODO %^{Title}\n%[~/.emacs.d/templates/da-property-string]\n" :immediate-finish t)
              ("T" "Tasks in gtd" entry (file+headline da-gtd "Tasks")
               "* %^{State|TODO|NEXT|WAIT|PASS|MAYB} %? \t%^{Tag|:WORK:|:PERSONAL:}\n%[~/.emacs.d/templates/da-property-string]\n" :empty-lines 1)
              ("e" "File email" entry (file org-default-notes-file)
               "* \"%:subject\"\n%[~/.emacs.d/templates/da-property-string-email]%i%?\n")
              ("W" "Wait for Reply" entry (file+headline da-gtd "E-mail")
               "* WAIT for reply \"%:subject\"\n%[~/.emacs.d/templates/da-property-string-email]%i%?\n")
              ("P" "new Project" entry (file "~/Sync/box/org/projects.org")
               "* %? \t%^{Tag|:WORK:proj:|:PERSONAL:proj:}\n%[~/.emacs.d/templates/da-property-string]\n%^{CATEGORY}p" :empty-lines 1 :prepend t)
              ("n" "Next urgent task" entry (file+headline da-gtd "Tasks")
               "* NEXT [#A] %? \t%^{Tag|:WORK:|:PERSONAL:}\nDEADLINE: %t\n%[~/.emacs.d/templates/da-property-string]\n")
              ("s" "Study item" entry (file+headline da-gtd "Study")
               "* TODO %?\n%[~/.emacs.d/templates/da-property-string]\n")

              ("w" "Weight" table-line (file+headline da-gtd "Weight")
               "|%t|%?|")

              ("h" "new Habit" entry (file+headline da-gtd "Habits")
               "* TODO %? \nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:REPEAT_TO_STATE: TODO\n:END:\n%a")
              ("i" "Idea" entry (file "~/Sync/box/org/ideas.org") "* %^{Idea} \n%u\n%a\n%?" :empty-lines 1)
              ;; backward will archive past event or trigger further actions
              ;; do I need :cal: it could be used in the view to archive refile
              ("c" "Calendar in Journal" entry (file+olp+datetree "~/Sync/box/org/journal.org")
               "* %? %:subject\t:cal:\n%^T\n%a\n%i\n" :jump-to-captured t :time-prompt t)
              ;; "* %? %^g\n%t\n%a\n%i" ) ; prefix C-1 alternative to time-prompt t
              ("j" "Journal" entry (file+olp+datetree "~/Sync/box/org/journal.org")
               "* %? %:subject %^G\n%T\n%a\n%i\n" :jump-to-captured t :time-prompt t)
              ;; ("m" "Meeting" entry (file+olp+datetree "~/Sync/box/org/journal.org") "* MEETING %? :MEETING:\n%T" :clock-in t :clock-resume t)
              ("k" "supermarKet" entry (file+headline "~/Sync/box/org/shopping.org" "Supermarket") "* %? \t:buy:\n" :unnarrowed t :kill-buffer t)
              ;; XXX: captures for: (1) project [entry and file template],
              ;; (2) peso [table-line]
              ("g" "Gcals")              ; gcals
              ("gG" "Gcal dpa" entry (file  "~/Sync/box/org/gcal/dpa.org")
               "* %? %:subject\n :PROPERTIES:\n :calendar-id: danielepietroarosio@gmail.com\n :END:\n:org-gcal:\n%^T--%^T\n%a\n:END:" :empty-lines 1)
              ("gF" "Gcal figli" entry (file  "~/Sync/box/org/gcal/figli.org")
               "* %? %:subject\n :PROPERTIES:\n :calendar-id: c87gevr5pc3191on8c7nh8b4nc@group.calendar.google.com\n :END:\n:org-gcal:\n%^T--%^T\n%a\n:END:" :empty-lines 1)
              ("gg" "Appointment" entry (file "~/Sync/box/org/gcal/dpa.org")
               "* %? %:subject\n:PROPERTIES:\n:calendar-id:\tdanielepietroarosio@gmail.com\n:END:\n:org-gcal:\n%^T--%^T\n:END:\n%a\n%i\n" :jump-to-captured t)
              ("gf" "Gcal figli" entry (file  "~/Sync/box/org/gcal/figli.org")
               "* %? %:subject\n:PROPERTIES:\n:calendar-id:\tc87gevr5pc3191on8c7nh8b4nc@group.calendar.google.com\n:END:\n:org-gcal:\n%^T--%^T\n:END:\n%a\n%i\n" :jump-to-captured t)

              ;; ("a" "Calendar" entry (file+olp+datetree "~/Sync/box/org/calendar.org")
              ;;  "* %? %:subject\n%^T--%^T\n%a\n%i\n" :jump-to-captured t :time-prompt t)
              ;; ("c" "Calendar" entry (file+olp+datetree "~/Sync/box/org/calendar.org")
              ;;  "* %? %:subject\n%T\n%a\n%i\n" :jump-to-captured t :time-prompt t)

              ("r" "Review")              ;reviews
              ("rd" "Review: Daily" entry (file+olp+datetree "/tmp/daily-reviews.org")
               (file "~/.emacs.d/templates/my_dailyreviewtemplate.org"))
              ("rw" "Review: Weekly Review" entry (file+olp+datetree "/tmp/weekly-reviews.org")
               (file "~/.emacs.d/templates/my_weeklyreviewtemplate.org"))
              ;; Only in mu4e
              ("R" "Reply to" entry
               (file+headline da-gtd "E-mail")
               "* TODO Reply \"%:subject\"\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n%[~/.emacs.d/templates/da-property-string-email]%i%?\n")
              ))
      ;; (add-to-list 'org-capture-templates  ;; comma ,(format) needs to be
      ;; added later
      ;;              `("X" "Wait for Reply" entry
      ;;                (file+headline da-gtd "E-mail")
      ;;                ,(format "%s\n%s\n%s" "* WAIT for reply %:subject" da-property-string "%i%?")))
      (setq org-capture-templates-contexts '(("R" ((in-mode . "mu4e-view-mode")))
                                             ("W" ((in-mode . "mu4e-view-mode")))
                                             ("R" ((in-mode . "mu4e-headers-mode")))
                                             ("W" ((in-mode . "mu4e-headers-mode")))))
      :bind
      (("C-c c" . org-capture)
       ("C-c T" . (lambda () (interactive "") (org-capture nil "T")))
       ("C-c R d" . nemacs-org-capture-review-daily)
       ("C-c R w" . my-new-weekly-review))
      )
    (setq org-use-property-inheritance nil) ; default
    (use-package org-agenda :straight org
      :bind
      (("M-s A" . (lambda () (interactive "") (org-agenda nil "s")))
       :map org-agenda-mode-map
       ("C-a" . org-agenda))
      :hook
      (org-agenda-mode-hook . (lambda () (hl-line-mode) (setq line-spacing 0.0)))
      :config
      (setq org-habit-show-habits-only-for-today nil)
      (setq org-habit-graph-column 60)
      (setq org-agenda-diary-file "~/Sync/box/org/journal.org")
      (setq org-agenda-include-diary t)
      (setq org-agenda-confirm-kill 1)
      (setq org-agenda-show-all-dates t)
      (setq org-agenda-show-outline-path nil)
      (setq org-agenda-skip-additional-timestamps-same-entry t)
      ;; (setq org-agenda-skip-deadline-prewarning-if-scheduled nil)
      ;; (setq org-agenda-skip-scheduled-delay-if-deadline t)
      (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
      ;; (setq org-agenda-skip-scheduled-if-done t)
      (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
      ;; (setq org-agenda-skip-timestamp-if-done t)
      (setq org-agenda-search-headline-for-time nil)
      (setq org-agenda-start-on-weekday nil)  ; 1=Monday
      (setq org-agenda-start-with-follow-mode nil)
      (setq org-agenda-sorting-strategy '(habit-down time-up priority-up category-keep)) ; '(todo-state-down priority-down))
      (setq org-agenda-compact-blocks t)
      (setq org-agenda-window-setup 'current-window)
      (setq org-agenda-restore-windows-after-quit t)
      (setq org-agenda-todo-list-sublevels t)
      (setq org-agenda-dim-blocked-tasks t)     ; Dim blocked tasks
      (setq org-agenda-show-future-repeats 'next) ; 'next to view this and the next.
      (setq org-agenda-search-view-always-boolean t) ; Lazy boolean search =C-c a s=
      (setq org-agenda-sticky t)
      (setq org-agenda-show-inherited-tags nil)
      ;; For tag searches ignore tasks with scheduled and deadline dates FIXME better control this in each agenda custom view
      ;; needed to avoid seeing missed tasks in my unscheduled view; next tasks in daily review view; in w but W is ok;
      (setq org-agenda-tags-todo-honor-ignore-options t)
      ;; (setq org-agenda-follow-indirect t)
      ;; FIXME: could help following projects individually
      (advice-add 'org-agenda-goto :after
                  (lambda (&rest args)
                    (org-narrow-to-subtree)))
      (setq org-element-use-cache nil)
      ;; org-compat
      (setq org-agenda-overriding-columns-format "%TODO 100%ITEM %7EFFORT %SCHEDULED %DEADLINE 100%TAGS")

      (setq org-agenda-custom-commands
            '(
              ("b" "Backwards calendar loops"
               (
                (agenda "Backward"
                        ((org-agenda-overriding-header "Backwards calendar loops")
                         ;; (org-agenda-overriding-columns-format "%20ITEM %DEADLINE")
                         ;; (org-agenda-view-columns-initially t)
                         (org-agenda-span 10)
                         (org-agenda-start-day "-10d")
                         (org-agenda-start-with-log-mode t)
                         (org-agenda-include-diary nil)
                         (org-agenda-skip-timestamp-if-done nil)
                         ))
                (agenda "Planning"
                        ((org-agenda-start-day "+1d")
                         (org-agenda-span 14)
                         (org-agenda-show-all-dates nil)
                         ))
                (tags-todo "-recurring-STYLE=\"habit\"&DEADLINE>\"<+14d>\""
                           ((org-agenda-overriding-header "Next deadlines")
                            (org-agenda-sorting-strategy '(deadline-up))
                            ))
                )
               (
                (org-agenda-show-future-repeats t)
                (org-agenda-compact-blocks nil)
                ))
              ("0" "Tasks to refile or archive"
               (
                (tags "REFILE" ((org-agenda-overriding-header "Tasks to Refile")))
                (tags "-NOTE-REFILE/DONE|CANC"
                      ((org-agenda-overriding-header "Tasks to Archive")))
                ))
              ("2" "Scattered action list"
               (
                (tags-todo "-proj")
                (agenda "" ((org-agenda-span 1)))
                (tags "+proj"
                      ((org-agenda-overriding-header "All projects scattered outside the agenda files")
                       (org-tags-exclude-from-inheritance '("proj"))))
                )
               (
                (org-agenda-sorting-strategy '(priority-down todo-state-down category-keep habit-down))
                (org-agenda-files da-refile-files)
                (org-tags-match-list-sublevels 'indented)
                (org-agenda-compact-blocks nil)
                )
               )
              ("a" "Actions list [today]"
               (
                (agenda "Journal"
                        ((org-agenda-entry-types '(:timestamp :scheduled :deadline))
                         (org-agenda-span 1)
                         (org-agenda-sorting-strategy '(deadline-up time-up scheduled-down priority-down))
                         (org-deadline-warning-days 0)
                         ))
                ))
              ("d" "Daily review"
               (
                (agenda "Today"
                        ((org-agenda-span 1)
                         (org-deadline-warning-days 0)
                         (org-agenda-entry-types '(:timestamp :scheduled :deadline))
                         (org-agenda-sorting-strategy '(deadline-up time-up scheduled-down priority-down))
                         ))
                (agenda "Next 6 days"
                        ((org-agenda-start-day "+1d")
                         (org-agenda-span 6)
                         (org-agenda-show-all-dates nil)
                         (org-agenda-time-grid
                          '((weekly) (1300)
                            "      " "················"))
                         ))
                (tags-todo "+Effort>\"0\"&Effort<=\"0:15\""
                           ((org-agenda-overriding-header "Quick Picks")
                            (org-agenda-todo-ignore-scheduled 'all)
                            (org-agenda-todo-ignore-deadlines 'far)))
                (tags-todo "-proj-recurring+PRIORITY=\"A\"/MAYB|TODO|NEXT" ;do not use -todo for refile archive
                           ((org-agenda-overriding-header "Pick list (standalone tasks)")
                            (org-agenda-files (append da-agenda-files
                                                      '("~/Sync/notes/arch/emacs.org"
                                                        "~/Sync/notes/arch/archlinux.org")))
                            (org-agenda-todo-ignore-scheduled 'all)
                            (org-agenda-todo-ignore-deadlines 'near)
                            (org-tags-match-list-sublevels 'indented)
                            ))
                (tags-todo "+study-PRIORITY=\"A\"" ;do not use -todo for refile archive
                           ((org-agenda-overriding-header "Pick list (to study)")
                            (org-agenda-files (append da-agenda-files
                                                      '("~/Sync/notes/arch/emacs.org"
                                                        "~/Sync/notes/arch/archlinux.org")))
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels 'indented)
                            ))
                (tags-todo "+proj-WAITING-PASSED-HOLDING-MAYBE/PASS|WAIT|NEXT" ;do not use -todo for refile archive
                           ((org-agenda-overriding-header "Pick list (projects)")
                            (org-agenda-files (append da-agenda-files
                                                      '("~/Sync/notes/arch/emacs.org"
                                                        "~/Sync/notes/arch/archlinux.org")))
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy '(category-keep todo-state-down priority-down habit-down))
                            ))
                )
               ((org-agenda-include-diary nil)
                ;; (org-agenda-sorting-strategy '(deadline-up scheduled-up habit-down time-up tag-down category-keep priority-up))
                (org-agenda-compact-blocks nil)))
              ("f" "Forwards loops, habits and recurring tasks"
               (
                (agenda "scheduled"
                        ((org-agenda-entry-types '(:scheduled :deadline))
                         (org-agenda-start-day "+15d")
                         (org-agenda-span 165)
                         (org-agenda-include-diary nil)
                         (org-agenda-show-all-dates nil)
                         (org-agenda-time-grid nil)
                         ))
                (tags-todo "+SCHEDULED>=\"<+180d>\"\|+DEADLINE>=\"<+180d>\""
                           ((org-agenda-overriding-header "Over 6 months")
                            ))
                )
               (
                (org-agenda-sorting-strategy '(deadline-up))
                ))
              ("l" "Standalone unscheduled tasks"
               (
                (tags "-proj-recurring-STYLE=\"habit\"+WORK/TODO|NEXT"
                      ((org-agenda-overriding-header "Work")))
                (tags "-proj-recurring-STYLE=\"habit\"+PERSONAL-WORK/TODO|NEXT"
                      ((org-agenda-overriding-header "Personal")))
                (tags "-proj-recurring-STYLE=\"habit\"-WORK-PERSONAL/TODO|NEXT"
                      ((org-agenda-overriding-header "Unassigned")))
                )
               (
                (org-agenda-sorting-strategy '(priority-down category-keep tag-down))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
                (org-tags-match-list-sublevels 'indented)
                ))
              ("w" "Follow-up list"
               (
                (tags "-proj/!WAIT|PASS"
                      ((org-agenda-overriding-header "Follow-up tasks list")))
                (tags "+proj/!WAIT|PASS"
                      ((org-agenda-overriding-header "Follow-up projects list")
                       (org-tags-exclude-from-inheritance '("proj"))))
                )
               (
                (org-tags-match-list-sublevels 'indented)))
              ("j" "Projects list"
               (
                ;; (tags "+proj/-HOLD-MAYB-PASS-WAIT-CANC-DONE"; -Proj=\"ignore\"
                (tags "+proj+DEADLINE={.+}/-DONE-CANC"
                      ((org-agenda-overriding-header "Projects due")
                       (org-tags-exclude-from-inheritance '("proj"))
                       ))
                (tags "+proj-DEADLINE={.+}/-DONE-CANC"
                      ((org-agenda-overriding-header "Projects")
                       (org-tags-exclude-from-inheritance '("proj"))
                       ))
                (tags "+proj"
                      ((org-agenda-overriding-header "Project tasks")
                       (org-agenda-sorting-strategy '(todo-state-up))
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":proj:"))
                       ))
                )
               ((org-tags-match-list-sublevels 'indented)
                (org-agenda-files (append da-agenda-files
                                          '("~/Sync/notes/arch/emacs.org"
                                            "~/Sync/notes/arch/archlinux.org")))
                (org-agenda-sorting-strategy '(category-keep priority-down))
                ;; (org-use-property-inheritance t)
                (org-agenda-tag-filter-preset '("-WAITING" "-PASSED" "-HOLDING" "-MAYBE" "-ENDED"))
                ))
              ("h" "Tasks and projects on hold"
               (
                (tags "-proj/+HOLD"
                      ((org-agenda-overriding-header "Tasks on hold")))
                (tags "+proj/+HOLD"
                      ((org-agenda-overriding-header "Projects on hold")
                       (org-tags-exclude-from-inheritance '("proj"))))
                (tags-todo "TIMESTAMP<=\"<now>\"")
                )
               (
                (org-tags-match-list-sublevels 'indented)))
              ("i" "Idea and hold, maybe, someday tasks and-or projects"
               (
                (tags "-proj-SCHEDULED={.+}-DEADLINE={.+}/+MAYB"
                      ((org-agenda-overriding-header "Tasks for someday")
                       ))
                (tags "+proj/+MAYB"
                      ((org-agenda-overriding-header "Projects for someday")
                       (org-tags-exclude-from-inheritance '("proj"))))
                (tags "+idea/-MAYB"
                      ((org-agenda-overriding-header "Ideas")
                       (org-tags-match-list-sublevels 'indented)
                       (org-agenda-sorting-strategy '(todo-state-up priority-down category-keep tag-down))))
                )
               (
                (org-agenda-sorting-strategy '(todo-state-up priority-down tag-down category-keep))
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
              ))
      )
    (use-package org-clock :straight org
      :config
      (setq org-clock-out-remove-zero-time-clocks t) ; Removes clocked tasks with 0:00 duration
      (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))   ; Enable auto clock resolution for finding open clocks
      )
    (org-clock-persistence-insinuate)       ; Resume clocking task when emacs is restarted
    (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 6:00 0:00")
                                        ("STYLE_ALL" . "habit"))))
    )
  (use-package ob-async
    :after (ob)
    :config
    (setq ob-async-no-async-languages-alist
          '("jupyter-python" "jupyter-julia" "jupyter-R"))
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
    :bind (:map org-mode-map
                ("C-c t b" . hydra-bullets/body))
    :hook (org-mode-hook . org-bullets-mode)
    )
  (use-package org-download
    :after (org)
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
  (use-package calendar                 ; calendars
    :straight (:type built-in)
    :hook
    (calendar-today-visible-hook . calendar-mark-today)
    )
  (use-package solar                    ; (2) sunrise and sunset
    :straight (:type built-in)
    :config
    (setq calendar-latitude 46.067270 ; Borino
          calendar-longitude 11.166153
          calendar-location-name "Trento"
          calendar-time-zone 60))
  (use-package holidays                 ; (3) Holidays
    :straight (:type built-in)
    :config
    (setq holiday-general-holidays
          '((holiday-fixed 1 1 "Capodanno")
            (holiday-fixed 5 1 "1 Maggio")
            (holiday-fixed 4 25 "Liberazione")
            (holiday-fixed 6 2 "Festa Repubblica")
            (holiday-fixed 7 14 "Bastille Day")))
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
            (holiday-fixed 11 1 "Ognissanti")))
    (setq holiday-bahai-holidays nil)
    (setq holiday-hebrew-holidays nil)
    (setq holiday-islamic-holidays nil)
    )
  (use-package org-gcal
    :bind
    ("C-c G p" . org-gcal-post-at-point) ; (add-hook 'org-capture-before-finalize-hook)
    ("C-c G d" . org-gcal-delete-at-point)
    ("C-c G g" . org-gcal-sync)
    ("C-c G G" . org-gcal-fetch)
    :commands (org-gcal-reload-client-id-secret)
    :init
    (which-key-add-key-based-replacements "C-c G" "Gcal")
    (setq org-gcal-client-id "1086004898054-uhp29b0kek41obv1dma52rpog8pr44gu.apps.googleusercontent.com")
    (setq org-gcal-client-secret "sP2Jupy5GKtdDAAgupQrSzc2")
    :config
    (setq org-gcal-auto-archive t)
    ;; (org-gcal-reload-client-id-secret)
    (setq org-gcal-file-alist
          '(("danielepietroarosio@gmail.com" . "~/Sync/box/org/gcal/dpa.org")
            ;; ("tq1af7efj4l9h8glgqi2g5vmsg@group.calendar.google.com" . "~/Sync/box/org/gcal/IBF.org")
            ("c87gevr5pc3191on8c7nh8b4nc@group.calendar.google.com" . "~/Sync/box/org/gcal/figli.org")))
    (setq plstore-cache-passphrase-for-symmetric-encryption t)
    ;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
    )
  (use-package calfw                    ; needed by calfw-org
    :bind
    ("C-c G W" . cfw:open-calendar-buffer))
  (use-package calfw-org
    :bind
    (("C-c G w" . cfw:open-org-calendar)
     :map org-agenda-mode-map
     ("W" . cfw:open-org-calendar)))
  )
(progn                                  ; org-roam and notes
  (use-package org-roam
    :after org
    :demand 2
    :commands (org-roam-db-autosync-mode)
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
      ;;# xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol
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
               ;; if using org-roam
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
    ;; (setq consult-notes-ripgrep-args
    ;;       ;; --smart-case --search-zip
    ;;       "rg --multiline --null --line-buffered --color=never --max-columns=1000 --path-separator / --ignore-case --no-heading --line-number --hidden --glob=!.git/ -g *.org -g *.md -g *.txt -g *.rst--sortr=accessed")
    ;; XXX: MAYBE: embark and citar support https://github.com/mclear-tools/consult-notes
    )

  (use-package consult-org-roam
    :commands consult-org-roam-mode
    :after org-roam
    :init
    (require 'consult-org-roam)
    ;; Activate the minor mode
    (consult-org-roam-mode 1)
    :custom
    ;; Use `ripgrep' for searching with `consult-org-roam-search'
    (consult-org-roam-grep-func #'consult-ripgrep)
    ;; Configure a custom narrow key for `consult-buffer'
    (consult-org-roam-buffer-narrow-key ?r)
    ;; Display org-roam buffers right after non-org-roam buffers
    ;; in consult-buffer (and not down at the bottom)
    (consult-org-roam-buffer-after-buffers t)
    :config
    ;; Eventually suppress previewing for certain functions
    (consult-customize
     consult-org-roam-forward-links
     :preview-key (kbd "M-."))
    :bind
    ;; Define some convenient keybindings as an addition
    ("C-c n R" . consult-org-roam-file-find)
    ("C-c n b" . consult-org-roam-backlinks)
    ("C-c n l" . consult-org-roam-forward-links)
    ("C-c n r" . consult-org-roam-search))
  )
(progn ;; Bibliography
  (use-package bibtex
    :bind (:map bibtex-mode-map
                ("<backtab>" . hs-toggle-hiding)
                ("H-z" . hs-hide-all)
                ("H-<tab>" . hs-minor-mode)
                ("H-Z" . hs-show-all))
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
    (LaTeX-mode-hook . citar-capf-setup)
    (org-mode-hook . citar-capf-setup)
    (markdown-mode-hook . citar-capf-setup)
    :config
    (setq citar-notes-source 'citar-file)
    (setq citar-symbols
          `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ") ;"file-pdf"
            (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ") ;"file-text"
            (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))) ;"link-external"
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

  (use-package pdf-tools
    :demand t
    :functions pdf-loader-install
    :bind (:map pdf-view-mode-map
                ("C-s" . isearch-forward)
                ("/" . pdf-occur)
                ("C-?" . pdf-isearch-occur))
    :init
    (pdf-loader-install)
    :config
    (setq pdf-view-resize-factor 1.1)   ;; more fine-grained zooming
    (use-package pdf-misc
      :straight pdf-tools
      :config (setq pdf-misc-print-program "/usr/bin/gtklp"))
    (use-package pdf-annot
      :straight pdf-tools
      :bind (:map pdf-view-mode-map
                  ("h" . pdf-annot-add-highlight-markup-annotation))
      :config (setq-local pdf-annot-activate-created-annotations nil)))

  ;; https://github.com/fuxialexander/org-pdftools
  ;; Maybe defun are unused but follow the instruction
  (use-package org-noter
    :commands  (org-noter-insert-note
                org-noter--valid-session
                org-noter--parse-root
                org-noter--get-precise-info
                org-noter--doc-approx-location
                org-noter--pretty-print-location)
    :bind (("C-c n n" . org-noter)
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
     ;; org-noter-notes-window-location 'other-frame
     )
    ;; (require 'org-noter-pdftools) ; org-pdftools suggestion FAIL with deman
    )
  (use-package org-pdftools
    :hook (org-mode-hook . org-pdftools-setup-link))
  (use-package org-noter-pdftools
    :after (org-noter)
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

  (use-package engine-mode
    :commands (engine/set-keymap-prefix
               engine/get-query
               engine/execute-search
               engine-mode)
    :init
    (engine-mode t)
    (engine/set-keymap-prefix (kbd "M-s M-/"))
    :config
    (defengine amazon
      "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
      :keybinding "z"
      )
    (defengine duckduckgo
      "https://duckduckgo.com/?q=%s"
      ;; :browser 'eww-browse-url
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

  (use-package gscholar-bibtex
    :bind ("M-s C-b" . gscholar-bibtex)
    :config
    (setq gscholar-bibtex-default-source "Google Scholar")
    (setq gscholar-bibtex-database-file "/home/dan/Sync/biblio/biblio.bib")
    )
  )
(progn                                  ; Magit
  (use-package magit
    :bind
    ("C-c g g" . magit-status)
    ("C-c g f" . magit-find-file-other-window)
    ("C-c g x" . magit-checkout)
    ("C-c g e" . magit-ediff-resolve)
    ("C-c g c" . magit-clone)
    ("C-c g i" . magit-init)
    (:map git-commit-mode-map
          ("M-n" . mk-transpose-line-down)
          ("M-p" . mk-transpose-line-up))
    :init
    (which-key-add-key-based-replacements "C-c g" "Git")
    (setq magit-repository-directories '(("/home/dan/workspace" . 4)
                                         ("/home/dati" . 2)
                                         ("~/Sync" . 9)))
    :config
    (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))
  (use-package magit-todos
    :after (magit)
    :bind ("C-c g 2" . magit-todos-list))
  (use-package magit-annex)
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
    :bind
    (("C-c g n" . diff-hl-next-hunk)    ; consider removing
     ("C-c g p" . diff-hl-previous-hunk))
    :hook
    ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
     (magit-post-refresh-hook . diff-hl-magit-post-refresh))
    ;; :init
    ;; (setq diff-hl-draw-borders nil)
    ;; (setq diff-hl-global-modes '(not org-mode))
    ;; (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
    ;; (setq diff-hl-global-modes (not '(image-mode org-mode)))
    :config
    (global-diff-hl-mode))
  )
(progn                                  ; Project
  ;; (use-package project
  ;;    :straight (:type built-in)
  ;;    :config
  ;;    (setq project--list))
  (use-package consult-project-extra
    :bind
    ("C-c p" . consult-project-extra-find))
  ;; (use-package rg)
  )
(progn                                  ; Additional modes
  (use-package markdown-mode            ;;markdownlint-cli ruby-mdl
    :bind (:map markdown-mode-map
                ("<return>" . nil)
                ("M-n" . mk-transpose-line-down)
                ("M-p" . mk-transpose-line-up))
    :init (setq markdown-url-compose-char ?…)
    :mode (("\\.md\\'" . markdown-mode)
           ("README\\.md\\'" . gfm-mode)
           ("\\.mkd\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode)))
  (use-package sphinx-mode)
  (use-package plantuml-mode
    :after (org)
    :defines org-plantuml-jar-path
    :init
    (setq plantuml-default-exec-mode 'jar
          plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
          org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"))
  (use-package graphviz-dot-mode
    :config (setq graphviz-dot-indent-width 4))
  (use-package gnuplot)
  (use-package ess)
  (use-package json-mode)
  (use-package ssh-config-mode)
  (use-package pkgbuild-mode)
  (use-package web-mode                 ;XXX: FIXME:
    ;; Unfortunately `web-mode' does not come with `auto-mode-alist'
    ;; autoloads. We have to establish them manually. This list comes
    ;; from the official website at <http://web-mode.org/> as of
    ;; 2018-07-09.
    :mode (("\\.phtml\\'" . web-mode)
           ("\\.tpl\\.php\\'" . web-mode)
           ("\\.[agj]sp\\'" . web-mode)
           ("\\.as[cp]x\\'" . web-mode)
           ("\\.erb\\'" . web-mode)
           ("\\.mustache\\'" . web-mode)
           ("\\.djhtml\\'" . web-mode)
           ("\\.html?\\'" . web-mode)
           ;; My additions.
           ("\\.ejs\\'" . web-mode)
           ("\\.[cm]?jsx?\\'" . web-mode)
           ("\\.tsx?\\'" . web-mode)
           ("\\.css\\'" . web-mode)
           ("\\.hbs\\'" . web-mode))
    ;; Use `web-mode' rather than `js-mode' for scripts.
    :interpreter (("js" . web-mode)
                  ("node" . web-mode))
    :config
    ;; Indent by two spaces by default. Compatibility with Prettier.
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    ;; Not sure why anyone would want 1 space indent for inline scripts
    ;; and CSS. Set it to 2 for consistency.
    (setq web-mode-script-padding 2)
    (setq web-mode-style-padding 2)
    ;; Autocomplete </ instantly.
    (setq web-mode-enable-auto-closing t)
    ;; Insert matching tags automatically. Why this is "mode 2", I have
    ;; not the slightest idea.
    (setq web-mode-auto-close-style 2)
    ;; Don't insert quotes automatically. It messes with JSX.
    (setq web-mode-enable-auto-quoting nil)
    ;; Disable `web-mode' automatically reindenting a bunch of
    ;; surrounding code when you paste anything. It's real annoying if
    ;; it happens to not know how to indent your code correctly.
    (setq web-mode-enable-auto-indentation nil)
    )
  (use-package vimrc-mode
    :hook (vimrc-mode-hook . (lambda ()
                               (setq-local tab-width 2)
                               (setq-local indent-line-function 'insert-tab))))
  (use-package yaml-mode ;; yay -S (yamllint) prettier with apheleia
    :hook ((yaml-mode . turn-off-flyspell))
    :mode "\\.yml\\'")
  (use-package toml-mode
    :mode "\\.toml\\'")
  (use-package csv-mode
    :mode (("\\.csv\\'" . csv-mode)))
  (use-package dna-mode
    :bind ("C-c t m d" . dna-mode))
  )
(progn                                  ; python
  (use-package devdocs
    :demand t
    :bind ("C-c D" . devdocs-lookup))
  (use-package numpydoc
    :commands (numpydoc-generate)
    :bind (:map python-mode-map
                ("C-c C-N" . numpydoc-generate))
    :config (setq numpydoc-insertion-style 'yas) ;'prompt|nil
    :after python)
  (use-package python
    :straight (:type built-in)
    :bind   (("C-c t m p" . python-mode)
             (:map python-mode-map
                   ("<backtab>" . hs-toggle-hiding) ; orig. python-indent-dedent-line
                   ("C-c C-P" . jupyter-run-repl)
                   ("H-<tab>" . hydra-for-py/body)))
    :config
    (setq-default python-fill-docstring-style 'pep-257-nn
                  python-indent 4)
    (setq python-shell-interpreter "jupyter"
          python-shell-interpreter-args "console --simple-prompt"
          python-shell-prompt-detect-failure-warning nil)
    (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))
  (use-package lsp-mode
    :commands (lsp-deferred
               lsp-enable-which-key-integration)
    :custom
    (lsp-completion-provider :none) ;; we use Corfu!
    :init
    (setq lsp-keymap-prefix "C-S-l")
    ;; (setq read-process-output-max (* 1024 1024)) ;; 1mb
    :hook
    (python-mode-hook . lsp-deferred)
    (lsp-mode-hook . lsp-enable-which-key-integration)
    ;; (lsp-mode-hook . (lambda ()
    ;;                 (let ((lsp-keymap-prefix "S-SPC"))
    ;;                   (lsp-enable-which-key-integration))))
    :defines
    (lsp-pylsp-plugins-flake8-enabled
     lsp-pylsp-plugins-autopep8-enabled
     lsp-pylsp-plugins-mccabe-enabled
     lsp-pylsp-plugins-pycodestyle-enabled
     lsp-pylsp-plugins-pydocstyle-enabled
     lsp-pylsp-plugins-pylint-enabled
     lsp-pylsp-plugins-pyflakes-enabled
     lsp-pylsp-plugins-yapf-enabled
     lsp-pylsp-plugins-flake8-config)
    :config
    (setq lsp-pylsp-plugins-flake8-enabled t) ;; (setq pylsp.plugins.flake8.enabled t)
    (setq lsp-pylsp-plugins-autopep8-enabled nil)
    (setq lsp-pylsp-plugins-mccabe-enabled nil)
    (setq lsp-pylsp-plugins-pycodestyle-enabled nil)
    (setq lsp-pylsp-plugins-pydocstyle-enabled nil)
    (setq lsp-pylsp-plugins-pylint-enabled nil)
    (setq lsp-pylsp-plugins-pyflakes-enabled nil)
    (setq lsp-pylsp-plugins-yapf-enabled nil)
    (setq lsp-pylsp-plugins-flake8-config ".flake8")
    )
  (use-package lsp-ui
    :requires
    (lsp-mode flycheck)
    :hook
    (lsp-mode-hook . lsp-ui-mode)
    :config
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-include-signature t)
    (setq lsp-ui-sideline-enable nil)
    (setq lsp-ui-flycheck-list-position 'right)
    )
  (use-package envrc
    :commands envrc-global-mode
    :after python
    :init
    (envrc-global-mode))
  (use-package python-pytest            ;install projectile
    :after (python)
    :bind (:map python-mode-map
                ("C-c T" . python-pytest-dispatch)
                ("<f8>" . python-pytest-dispatch)))

  (use-package eval-in-repl
    :after (python)
    :hook (python-mode-hook . (lambda () (require 'eval-in-repl-python) ))
    :config
    (setq eir-jump-after-eval nil)      ; default t
    :bind (:map python-mode-map
                ("<C-return>" . eir-eval-in-python))
    )
  (use-package py-isort                 ;yay -S python-isort
    :after (python)
    :hook (before-save-hook . py-isort-before-save)
    :bind (:map python-mode-map
                ("C-c s" . py-isort-buffer)
                ("C-c S" . py-isort-region)))
  )

(use-package emojify
  :bind ("C-c M-e" . emojify-insert-emoji)
  :hook (after-init-hook . global-emojify-mode)
  :custom (emojify-emoji-set "emojione-v2.2.6-22"))
(use-package slack
  :defer t ;; avoid halting daemon startup asking for passwords
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
  :bind (("M-g M-a s" . (lambda () (interactive "") (slack-start) (hydra-slack/body)))
         :map slack-mode-map
         ("M-p" . slack-room-pins-list)
         ("\C-n" . slack-buffer-goto-next-message)
         ("\C-p" . slack-buffer-goto-prev-message)
         ("H-<tab>" . hydra-slack/body)
         ("@" . slack-message-embed-mention)
         ("#" . slack-message-embed-channel))
  :init
  (which-key-add-key-based-replacements "M-g M-a s" "Slack")
  (setq slack-buffer-emojify t
        slack-prefer-current-team t)
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
  :hook (nov-mode-hook . visual-fill-column-mode)
  :config (setq nov-text-width t))
(use-package keyfreq
  :commands (keyfreq-mode
             keyfreq-autosave-mode)
  :config
  (keyfreq-mode 1)

  (keyfreq-autosave-mode 1))
(use-package pocket-reader
  :bind ("M-g M-a r" . pocket-reader))
(use-package calibredb
  :commands calibredb
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
(use-package ox-hugo
  :after ox
  :init (eval-after-load 'ox '(require 'ox-hugo)))

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
