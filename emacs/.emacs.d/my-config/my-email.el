;;; my-email.el --- To manage emails -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Binding keys:
;;; Code:

(use-package mu4e                       ; mu4e
  :demand is-daemon
  :straight (:type built-in)            ; in AUR/mu
  :commands (mu4e mu4e-compose-new)
  :preface
  (defun replace-duck-emails-in-buffer ()
    "Replace duck.com email addresses with their original format."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\([[:word:].+-]+\\)_at_\\([[:word:].+-]+\\)\\(\\.[a-zA-Z.]+\\)_[[:word:]]+@duck\\.com" nil t)
        (replace-match "\\1@\\2\\3" nil nil))))

  (defun my-mu4e-compose-mode-hook ()
    "My settings for message composition."
    (set-fill-column 80)
    (visual-fill-column-mode)
    (guess-language-mode)
    (let* ((ctx (mu4e-context-current))
           (name (mu4e-context-name ctx)))
      (when name
        (cond
         ((string= name "pec")
          (save-excursion (message-remove-header "Bcc:*")))
         ((string= name "cnr")
          (save-excursion
            (goto-char (point-max))
            (mml-attach-file "~/Sync/Maildir/firma-istituzionale.html")))))))

  :hook
  (dired-mode . turn-on-gnus-dired-mode)
  (mu4e-view-mode . variable-pitch-mode)
  (mu4e-compose-mode . (lambda ()
                         (my-mu4e-compose-mode-hook)
                         (replace-duck-emails-in-buffer)))
  (mu4e-update-pre . mu4e-update-index-nonlazy)

  :bind (("M-g M-a m" . mu4e)
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
         ("f"         . mu4e-headers-mark-for-flag))

  :config
  (setq mail-user-agent 'mu4e-user-agent
        read-mail-command 'mu4e;; use mu4e as Default
        ;; mu4e-root-maildir (expand-file-name "~/Maildir")
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 50
        mu4e-index-cleanup t;; do a full cleanup check
        mu4e-index-lazy-check t;; don't consider up-to-date dirs
        mu4e-attachment-dir "~/"
        mu4e-change-filenames-when-moving t; rename files when moving (Needed for mbsync)
        mu4e-read-option-use-builtin nil ; Use Vertico
        mu4e-completing-read-function 'completing-read; use convenient completion for navigation =j o=
        mu4e-compose-keep-self-cc nil
        mu4e-hide-index-messages t
        mu4e-use-fancy-chars t
        mu4e-headers-leave-behavior 'ask ; default while 'apply leaving headers view apply all marks
        mu4e-compose-forward-as-attachment nil
        mu4e-confirm-quit nil
        fill-flowed-encode-column 998; https://www.ietf.org/rfc/rfc2822.txt
        shr-color-visible-luminance-min 80
        mu4e-context-policy 'pick-first ; start with the first (default) context;
        mu4e-search-include-related t
        mu4e-search-skip-duplicates nil
        mu4e-compose-switch 'new-frame)

  (auth-source-pass-enable)
  (setq auth-source-debug t
        auth-source-do-cache nil
        auth-sources '(password-store))
  (setq smtpmail-queue-mail  nil
        smtpmail-queue-dir   "~/Maildir/queue/cur") ; Remember to "mu mkdir" and "touch /queue/.noindex"

  (setq mu4e-maildir-shortcuts
        '(
          ("/gmail/Inbox"             . ?j)
          ("/gmail/refs"              . ?r)
          ("/gmail/keepup"            . ?k)
          ("/gmail/[Gmail]/Drafts"    . ?d)
          ("/gmail/[Gmail]/Sent Mail" . ?s)
          ("/gmail/[Gmail]/Spam"      . ?x)
          ("/pec/INBOX"               . ?P)
          ("/personal"                . ?p)
          ("/archive"                 . ?a)
          ))

  (setq mu4e-compose-format-flowed t
        mu4e-compose-context-policy 'ask-if-none
        message-signature nil)

  (setq mu4e-bookmarks
        `(
          (:key ?a
                :name "Temporary archive"
                :query "maildir:/gmail/archive_tmp")
          (:key ?t
                :name "Trash for Gmail w/o related"
                :query "maildir:/gmail/[Gmail]/Trash")
          (:key ?s :hide t
                :name "Starred"
                :query "flag:flagged")
          (:key ?u :hide t
                :name "Unread messages"
                :query "flag:unread AND NOT flag:trashed")
          (:key ?o :hide t
                :name "Messages with Office docs"
                :query "mime:application/vnd.*")
          (:key ?b :hide t
                :name "Big messages"
                :query "size:5M..500M")
          (:key ?3 :hide t
                :name "Last 3 days"
                :query "date:3d..now")
          ))
  )

(use-package mu4e-context
  :after mu4e
  :straight mu4e
  :config
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "cnr"
             :vars '((user-mail-address . "daniele.arosio@cnr.it"  )
                     (user-full-name . "Daniele Arosio" )
                     (mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail" )
                     (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts" )
                     (mu4e-refile-folder . "/archive" )
                     (mu4e-trash-folder . "/gmail/[Gmail]/Trash")
                     (message-signature . "")))
           ,(make-mu4e-context
             :name "gmail"
             :vars '( (user-mail-address . "danielepietroarosio@gmail.com" )
                      (mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail" )
                      (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                      (mu4e-refile-folder . "/archive" )
                      (mu4e-trash-folder . "/gmail/[Gmail]/Trash")
                      (message-signature  . "daniele Arosio\n38123 Trento\n")))
           ,(make-mu4e-context
             :name "pec"
             :match-func (lambda (msg)
                           (when msg
                             (string-prefix-p "/pec" (mu4e-message-field msg :maildir))))
             :vars '( (user-mail-address . "daniele.arosio@postecert.it" )
                      (user-full-name . "Daniele Arosio" )
                      (mu4e-drafts-folder . "/pec/Drafts")
                      (mu4e-trash-folder . "/pec/trash")
                      (mu4e-sent-folder . "/pec/Sent Items")
                      (message-signature . "daniele Arosio\n38123 Trento\n")))
           ))
  )

(use-package mu4e-icalendar
  :after mu4e
  :straight mu4e
  :init
  (setq gnus-icalendar-org-capture-file "~/Sync/box/org/gtd.org")
  (setq gnus-icalendar-org-capture-headline '("Calendar"))
  :config
  ;; AGGIORNATO: da mu4e-icalendar-setup a gnus-icalendar-setup
  (gnus-icalendar-setup)
  (gnus-icalendar-org-setup))

(use-package mu4e-headers
  :after mu4e
  :straight mu4e
  ;; :functions (
  ;;             mu4e~headers-goto-docid)
  :config
  (setq mu4e-headers-date-format "%d/%m/%y")
  (setq mu4e-headers-fields   '((:human-date   .  12)
                                (:flags        .   6)
                                (:size         .   7)
                                (:mailing-list . 10)
                                (:from         .  20)
                                (:thread-subject)))
  (setq mu4e-headers-auto-update t)   ; default
  (setq mu4e-headers-visible-lines 10))

(use-package message
  :straight (:type built-in)
  ;; :functions (message-sendmail-envelope-from
  ;;             message-add-header
  ;;             message-remove-header)
  :config
  (setq message-kill-buffer-on-exit t ; don't keep message buffers around
        ;; message-send-mail-function 'smtpmail-send-it ; when using queue
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"
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
  :after mu4e
  :straight mu4e
  :bind (:map
         mu4e-headers-mode-map
         ("M-z" . mu4e-headers-mark-for-tag)
         ("M-u" . mu4e-headers-mark-for-untag)
         ("M-p" . mu4e-headers-mark-for-personal)
         :map
         mu4e-view-mode-map
         ("M-z" . mu4e-view-mark-for-tag)
         ("M-u" . mu4e-headers-mark-for-untag)
         ("M-p" . mu4e-view-mark-for-personal))
  :config
  (add-to-list 'mu4e-marks
               ;; Add tag                 ;; https://gist.github.com/lgatto/7091552
               '(tag :char       "M-z"
                     :prompt     "tag"
                     :ask-target (lambda () (read-string "What tag do you want to add?"))
                     :action      (lambda (docid msg target)
                                    (mu4e-action-retag-message msg (concat "+" target)))))
  (add-to-list 'mu4e-marks
               ;; Remove tag
               '(untag :char       "M-u"
                       :prompt     "untag"
                       :ask-target (lambda () (read-string "What tag do you want to remove?"))
                       :action      (lambda (docid msg target)
                                      (mu4e-action-retag-message msg (concat "-" target)))))
  (add-to-list 'mu4e-marks
               '(personal :char       "M-p"
                          :prompt     "personal"
                          :show-target (lambda (target) "personal")
                          :action      (lambda (docid msg target)
                                         ;; must come before proc-move since retag runs
                                         ;; 'sed' on the file
                                         (mu4e-action-retag-message msg "-\\Inbox")
                                         (mu4e--server-move docid "/personal" "+S-u-N"))))
  (mu4e~headers-defun-mark-for tag)
  (mu4e~headers-defun-mark-for personal))

(use-package mu4e-org
  :after mu4e
  :straight mu4e
  :config
  (setq mu4e-org-link-query-in-headers-mode t ; `C-c l` store query
        org-mu4e-convert-to-html t))

(use-package org-msg
  ;; :demand t
  :after mu4e
  :defines (org-msg-options
            org-msg-startup
            org-msg-greeting-fmt
            org-msg-greeting-name-limit
            org-msg-convert-citation
            org-msg-default-alternatives
            org-msg-signature)
  :init
  (eval-when-compile
    (require 'mu4e nil t))
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
        org-msg-convert-citation t
        org-msg-default-alternatives '((new . (utf-8))
                                       (reply-to-html . (utf-8 html))
                                       (reply-to-text . (utf-8)))
        org-msg-signature "\n#+begin_signature\n  --  daniele \\\\\n#+end_signature"))

(use-package org-mime
  :after mu4e
  :defines (org-mime-library
            org-mime-export-ascii
            org-mime-export-options)
  :functions org-mime-change-element-style
  :init
  (eval-when-compile
    (require 'mu4e nil t))
  :bind (
         :map mu4e-compose-mode-map
         ("C-c M-O" . org-mime-edit-mail-in-org-mode)
         ("C-c M-o" . org-mime-htmlize)
         :map org-mode-map
         ("C-c M-o" . org-mime-org-subtree-htmlize)
         ("C-c M-O" . org-mime-org-buffer-htmlize))
  :hook
  (org-mime-html . (lambda ()
                     (org-mime-change-element-style
                      "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                                    "#E6E1DC" "#232323")))) ; "darkred" "burlywood"
  ;; the following can be used to nicely offset block quotes in email bodies
  (org-mime-html . (lambda ()
                     (org-mime-change-element-style
                      "blockquote" "border-left: 2px solid gray; padding-left: 4px;")))
  ;; (message-send . org-mime-confirm-when-no-multipart)
  (mu4e-compose-mode . (lambda ()(require 'org-mime))) ; work w/out server
  :config
  (setq org-mime-library 'semi
        org-mime-export-ascii 'utf-8
        org-mime-export-options '(
                                  :with-latex dvipng
                                  :section-numbers nil
                                  :with-author nil
                                  :with-toc nil)))

(use-package mu4e-jump-to-list
  :after mu4e)
(provide 'my-email)
;;; my-email.el ends here
