
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(require 'package)
(package-initialize)
;(setq package-enable-at-startup nil)


;; package management
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
;; (package-initialize)
;; Assuming you wish to install ""
(ensure-package-installed 'evil-leader
			  'evil-nerd-commenter
			  'powerline
			  'paganini-theme
			  'evil-magit)
			  ;'helm


(setq vc-follow-symlinks t)
;; (setq vc-follow-symlinks nil)


;  https://nathantypanski.com/blog/2014-08-03-a-vim-like-emacs-config.html
;; (use-package magit
  ;; :ensure magit
  ;; :config
  ;; (progn
    ;; (evil-set-initial-state 'magit-mode 'normal)
    ;; (evil-set-initial-state 'magit-status-mode 'normal)
    ;; (evil-set-initial-state 'magit-diff-mode 'normal)
    ;; (evil-set-initial-state 'magit-log-mode 'normal)
    ;; (evil-define-key 'normal magit-mode-map
        ;; "j" 'magit-goto-next-section
        ;; "k" 'magit-goto-previous-section)
    ;; (evil-define-key 'normal magit-log-mode-map
        ;; "j" 'magit-goto-next-section
        ;; "k" 'magit-goto-previous-section)
    ;; (evil-define-key 'normal magit-diff-mode-map
        ;; "j" 'magit-goto-next-section
        ;; "k" 'magit-goto-previous-section)))


(require 'evil-leader)
(evil-leader/set-leader ",")
(global-evil-leader-mode t)
(evil-leader/set-key "w" 'save-buffer)
(evil-leader/set-key "q" 'kill-buffer-and-window)
(evil-leader/set-key "h" 'dired-jump)
(evil-leader/set-key "v" 'split-window-right)
(evil-leader/set-key "e" 'pp-eval-last-sexp)
(evil-leader/set-key "," 'other-window)
(evil-leader/set-key "b" 'ibuffer)
(evil-leader/set-key "x" 'helm-M-x)

(require 'evil)
(evil-mode t)
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)
;; (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; nerdcommenter
;; (evilnc-default-hotkeys)
(evil-leader/set-key
  "cc" 'evilnc-comment-or-uncomment-lines
  "cy" 'evilnc-copy-and-comment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  ;; "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "\\" 'evilnc-comment-operator ; if you prefer backslash key
)


(require 'dired-x)

(require 'powerline)
(powerline-default-theme)
;; (require 'evil-magit)
;; (require 'helm-config)
;; (helm-mode 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "/tmp/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "/tmp/backups/"))))
 '(custom-enabled-themes (quote (paganini)))
 '(custom-safe-themes
   (quote
    ("1e67765ecb4e53df20a96fb708a8601f6d7c8f02edb09d16c838e465ebe7f51b" default)))
 '(menu-bar-mode nil)
 '(package-selected-packages (quote (magit paganini-theme powerline evil-leader)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

 ;; (tool-bar-mode -1)
 ;; (visual-line-mode 1)
 ;; Enforce trailing newlines
 ;; (setq require-final-newline t)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 113 :width normal)))))
