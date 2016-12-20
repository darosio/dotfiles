(setq mu4e-maildir "~/Sync/Maildir")
(setq mu4e-sent-folder "/cnr/INBOX.Sent")
(setq mu4e-drafts-folder "/cnr/INBOX.Drafts")
(setq mu4e-trash-folder "/cnr/INBOX.Trash")
(setq mu4e-refile-folder "/cnr/INBOX.archive")
;; allow for updating mail using 'U' in the main view:
;; (setq mu4e-get-mail-command "offlineimap")
(setq mu4e-get-mail-command "offlineimap -u quiet")

(setq mu4e-get-mail-command "offlineimap -u quiet")
(setq mu4e-maildir-shortcuts
      '( ("/cnr/INBOX"        . ?i)
         ("/cnr/INBOX.Sent"   . ?s)
         ("/cnr/INBOX.Trash"  . ?t)
         ("/cnr/INBOX.Drafts" . ?a)))
;; something about ourselves
(setq
 user-mail-address "daniele.arosio@cnr.it"
 user-full-name  "Daniele Arosio"
 message-signature
 (concat
  "Daniele Arosio\n"
  "Email: daniele.arosio@cnr.it\n"
  "CNR: www.douban.com/people/renws"
  "\n"))

(setq send-mail-function (quote sendmail-send-it)
      ;; message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "~/.progs/bin/msmtp-enqueue.sh"
      mail-specify-envelope-from t ;'header
      message-sendmail-f-is-evil nil
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header
      mail-interactive t)

;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")

(provide 'mu4e-config)
