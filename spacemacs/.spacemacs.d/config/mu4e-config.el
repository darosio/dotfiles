;;store org-mode links to messages
;(require 'org-mu4e)
;;store link to message if in header view, not to header query
;(setq org-mu4e-link-query-in-headers-mode nil)
; sembra non servire (http://pragmaticemacs.com/emacs/master-your-inbox-with-mu4e-and-org-mode/)

(setq mu4e-maildir "~/Sync/Maildir")
;(setq mu4e-sent-folder "/cnr/INBOX.Sent")
(setq mu4e-sent-folder "/cnr/Sent")
;(setq mu4e-drafts-folder "/cnr/INBOX.Drafts")
(setq mu4e-drafts-folder "/cnr/Drafts")
;(setq mu4e-trash-folder "/cnr/INBOX.Trash")
(setq mu4e-trash-folder "/cnr/Trash")
(setq mu4e-refile-folder "/archive")

;; allow for updating mail using 'U' in the main view:
;(setq mu4e-get-mail-command "offlineimap -u quiet")
(setq mu4e-get-mail-command "mbsync cnr")
;(setq mu4e-update-interval 120)
;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)

(setq mu4e-maildir-shortcuts
      '( ("/cnr/INBOX"        . ?i)
         ("/cnr/Sent"         . ?s)
         ("/cnr/Templates"    . ?t)
         ("/cnr/Drafts"       . ?a)))
;; something about ourselves
; mu4e-reply-to-address "daniele.arosio@cnr.it"
(setq user-mail-address "daniele.arosio@cnr.it"
 user-full-name  "Daniele Arosio"
 message-signature
 (concat
  "Daniele Arosio\n"
  "Institute of Biophysics\n"
  "National Research Council of Italy\n"
  "Bruno Kessler Foundation\n"
  "Via Sommarive 18\n"
  "38123 Trento, Italy\n"
  "Email: daniele.arosio@cnr.it"
  "\n")) ;(setq message-signature-file "~/.emacs.d/.signature") ;

(setq send-mail-function (quote sendmail-send-it)
      ;; message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "~/.local/bin/msmtp-enqueue.sh"
      mail-specify-envelope-from t ;'header
      message-sendmail-f-is-evil nil
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header
      mail-interactive t)

;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")

;; from http://www.macs.hw.ac.uk/~rs46/posts/2014-01-13-mu4e-email-client.html
; get mail
;(setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/.mbsyncrc work"
;(setq mu4e-html2text-command "w3m -T text/html")
      ;mu4e-headers-auto-update t
      ;mu4e-compose-signature-auto-include nil)

;; show images
(setq mu4e-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; don't save message to Sent Messages, IMAP takes care of this
; (setq mu4e-sent-messages-behavior 'delete)

;; spell check
;(add-hook 'mu4e-compose-mode-hook
        ;(defun my-do-compose-stuff ()
           ;"My settings for message composition."
           ;(set-fill-column 72)
           ;(flyspell-mode)))

;https://github.com/agpchil/mu4e-maildirs-extension
(setq mu4e-maildirs-extension-maildir-collapsed-prefix "archives")
(setq mu4e-maildirs-extension-default-collapse-level 1)
; taken from mu4e page to define bookmarks
;(add-to-list 'mu4e-bookmarks
;             '("size:5M..500M"       "Big messages"     ?b))
;http://www.djcbsoftware.nl/code/mu/mu4e/Bookmarks.html
;(add-to-list 'mu4e-bookmarks
;             (make-mu4e-bookmark
;              :name  "Big messages"
;              :query "size:5M..500M"
;              :key ?b))

(provide 'mu4e-config)
