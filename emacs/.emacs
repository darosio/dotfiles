
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
			  'evil-magit
			  'org-evil
			  'auto-complete)
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
(evil-leader/set-key "g" 'magit-status)

(require 'evil)
(evil-mode t)
;; (setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
;; (setq evil-emacs-state-modes nil)
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
;; powerline
(require 'powerline)
(powerline-default-theme)
;; evil-magit
(require 'evil-magit)
;; org-evil
(require 'org-evil)
;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
;; (setq ac-show-menu-immediately-on-auto-complete t) ; not sure I want/need this.

;; notmuch email
(require 'notmuch) ;(autoload 'notmuch "notmuch" "notmuch mail" t) 
(global-set-key (kbd "C-c m") 'notmuch)
(setq notmuch-search-oldest-first nil)
(setq notmuch-fcc-dirs '(("daniele.arosio@cnr.it" . "cnr/Sent")
			 (".*" . "gmail/sent"))) ;set sent mail directory
(setq mail-user-agent 'message-user-agent) ;Setup User-Agent header
(setq send-mail-function (quote sendmail-send-it)
      ;; message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "~/.progs/bin/msmtp-enqueue.sh"
      mail-specify-envelope-from t ;'header
      message-sendmail-f-is-evil nil
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header
      mail-interactive t)
(setq notmuch-saved-searches ;A few commonly used saved searches. 
      (quote
       ((:name "inbox" :query "tag:inbox" :key "i" :sort-order oldest-first)
	(:name "unread" :query "tag:unread" :key "u")
	(:name "flagged" :query "tag:flagged" :key "f")
	(:name "sent" :query "tag:sent" :key "t")
	(:name "drafts" :query "tag:draft" :key "d")
	(:name "all mail" :query "*" :key "a")
	(:name "cnr" :query "to:daniele.arosio@cnr.it" :key "c")
	(:name "gmail" :query "to:danielepietroarosio@gmail.com" :key "g"))))
(setq message-kill-buffer-on-exit t) ;kill buffer after sending mail)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       notmuch-show-all-multipart/alternative-parts nil
;;       mime-edit-pgp-signers '("C84EF897")
;;       mime-edit-pgp-encrypt-to-self t
;;       mml2015-encrypt-to-self t
;;       mml2015-sign-with-sender t
;;       notmuch-crypto-process-mime t
;;       user-full-name "Christian Kruse"
;;       user-mail-address "cjk@defunct.ch"
;;       mail-user-agent 'message-user-agent
;;       notmuch-always-prompt-for-sender t
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ; stores postponed messages to the specified directory
;; (setq message-directory "MailLocation/Drafts") ;
;; (setq message-directory "gmail/draft")
;; ;Settings for main screen
;; (setq notmuch-hello-hide-tags (quote ("killed")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-notmuch
(add-to-list 'load-path "/home/dan/workspace/dotfiles/emacs/contrib")
(require 'org-notmuch)
(define-key global-map "\C-cl" 'org-store-link)
(define-key notmuch-show-mode-map "\C-c\C-o" 'browse-url-at-point)
(define-key notmuch-search-mode-map "g" 'notmuch-poll-and-refresh-this-buffer)
(define-key notmuch-hello-mode-map "g" 'notmuch-poll-and-refresh-this-buffer)
(define-key notmuch-search-mode-map "d"
  (lambda () "toggle deleted tag for thread" (interactive)
    (if (member "deleted" (notmuch-search-get-tags))
	(notmuch-search-tag '("-deleted"))
      (notmuch-search-tag '("+deleted" "-inbox" "-unread")))))
(define-key notmuch-search-mode-map "!"
  (lambda ()
    "toggle unread tag for thread"
    (interactive)
    (if (member "unread" (notmuch-search-get-tags))
        (notmuch-search-tag '("-unread"))
      (notmuch-search-tag '("+unread")))))
(define-key notmuch-show-mode-map "d"
  (lambda ()
    "toggle deleted tag for message"
    (interactive)
    (if (member "deleted" (notmuch-show-get-tags))
        (notmuch-show-tag '("-deleted"))
      (notmuch-show-tag '("+deleted" "-inbox" "-unread")))))
(define-key notmuch-search-mode-map "a"
  (lambda ()
    "toggle archive"
    (interactive)
    (if (member "archive" (notmuch-search-get-tags))
        (notmuch-search-tag '("-archive"))
      (notmuch-search-tag '("+archive" "-inbox" "-unread")))))
(define-key notmuch-show-mode-map "a"
  (lambda ()
    "toggle archive"
    (interactive)
    (if (member "archive" (notmuch-show-get-tags))
        (notmuch-show-tag '("-archive"))
      (notmuch-show-tag '("+archive" "-inbox" "-unread")))))
(define-key notmuch-hello-mode-map "i"
  (lambda ()
    (interactive)
    (notmuch-hello-search "tag:inbox")))
(define-key notmuch-hello-mode-map "u"
  (lambda ()
    (interactive)
    (notmuch-hello-search "tag:unread")))
(define-key notmuch-hello-mode-map "a"
  (lambda ()
    (interactive)
    (notmuch-hello-search "tag:archive")))
;;;;;;;;;
;Reading mail settings:
(define-key notmuch-show-mode-map "S"
    (lambda ()
    "mark message as spam"
    (interactive)
(notmuch-show-tag (list "+spam" "-inbox"))))
(define-key notmuch-search-mode-map "S"
(lambda ()
    "mark message as spam"
    (interactive)
    (notmuch-search-tag (list "-inbox" "+spam"))
    (next-line) ))

;; example configuration for mu4e

;; make sure mu4e is in your load-path
(require 'mu4e)

;; Only needed if your maildir is _not_ ~/Maildir
;; Must be a real dir, not a symlink
(setq mu4e-maildir "/home/dan/.maildir")

;; these must start with a "/", and must exist
;; (i.e.. /home/user/Maildir/sent must exist)
;; you use e.g. 'mu mkdir' to make the Maildirs if they don't
;; already exist

;; below are the defaults; if they do not exist yet, mu4e offers to
;; create them. they can also functions; see their docstrings.
;; (setq mu4e-sent-folder   "/sent")
;; (setq mu4e-drafts-folder "/drafts")
;; (setq mu4e-trash-folder  "/trash")

;; smtp mail setting; these are the same that `gnus' uses.
(setq
   message-send-mail-function   'sendmail-send-it)
   ;; smtpmail-default-smtp-server "smtp.example.com"
   ;; smtpmail-smtp-server         "smtp.example.com"
   ;; smtpmail-local-domain        "example.com")
;; (require 'helm-config)
;; (helm-mode 1)
;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;; save attachment to my desktop (this can also be a function)
(setq mu4e-attachment-dir "~/")

;; attempt to show images when viewing messages
(setq mu4e-view-show-images t)

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
 '(package-selected-packages
   (quote
    (org-evil evil-magit magit paganini-theme powerline evil-leader)))
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
