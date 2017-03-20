;;store org-mode links to messages
;(require 'org-mu4e)
;;store link to message if in header view, not to header query
;(setq org-mu4e-link-query-in-headers-mode nil)
; sembra non servire (http://pragmaticemacs.com/emacs/master-your-inbox-with-mu4e-and-org-mode/)

;; allow for updating mail using 'U' in the main view:
;(setq mu4e-get-mail-command "offlineimap -u quiet")
(setq mu4e-get-mail-command "mbsync -a"
	  mu4e-update-interval 120
	  ;; rename files when moving NEEDED FOR MBSYNC
	  mu4e-change-filenames-when-moving t)

;; something about ourselves
; mu4e-reply-to-address "daniele.arosio@cnr.it"
;(setq user-mail-address "daniele.arosio@cnr.it"
 ;user-full-name  "Daniele Arosio"
 ;message-signature
 ;(concat
  ;"Daniele Arosio\n"
  ;"Institute of Biophysics\n"
  ;"National Research Council of Italy\n"
  ;"Bruno Kessler Foundation\n"
  ;"Via Sommarive 18\n"
  ;"38123 Trento, Italy\n"
  ;"Email: daniele.arosio@cnr.it"
  ;"\n")) ;(setq message-signature-file "~/.emacs.d/.signature") ;

 (setq mu4e-contexts
    `( ,(make-mu4e-context
          :name "cnr"
          :enter-func (lambda () (mu4e-message "Entering Cnr context"))
          :leave-func (lambda () (mu4e-message "Leaving Cnr context"))
          ;; we match based on the contact-fields of the message
          :match-func (lambda (msg)
                        (when msg 
                          (mu4e-message-contact-field-matches msg 
                            :to "daniele.arosio@cnr.it")))
          :vars '( ( user-mail-address      . "daniele.arosio@cnr.it"  )
                   ( user-full-name         . "Daniele Arosio" )
				   ( mu4e-sent-folder   . "/cnr/Sent" )
	  			   ( mu4e-drafts-folder . "/cnr/Drafts" )
	  			   ( mu4e-trash-folder  . "/cnr/Trash" )
	  			   ( mu4e-refile-folder . "/archive" )
                   ( mu4e-compose-signature .
                     (concat
						"Daniele Arosio\n"
  						"National Research Council (CNR), Institute of Biophysics\n"
  						"Via Sommarive 18\n"
  						"38123 Trento, Italy\n"))))
       ,(make-mu4e-context
          :name "gmail"
          :enter-func (lambda () (mu4e-message "Switch to the gmail context"))
          ;; no leave-func
          ;; we match based on the contact-fields of the message
          :match-func (lambda (msg)
                        (when msg 
                          (mu4e-message-contact-field-matches msg 
                            :to "danielepietroarosio@gmail.com")))
          :vars '( ( user-mail-address       . "danielepietroarosio@gmail.com" )
                   ( user-full-name          . "daniele arosio" )
                   ( mu4e-compose-signature  .
                     (concat
                       "daniele arosio\n"
                       "38123 Trento\n"))))

       ;,(make-mu4e-context
          ;:name "Cycling"
          ;:enter-func (lambda () (mu4e-message "Switch to the Cycling context"))
          ;;; no leave-func
          ;;; we match based on the maildir of the message; assume all
          ;;; cycling-related messages go into the /cycling maildir
          ;:match-func (lambda (msg)
                        ;(when msg
                          ;(string= (mu4e-message-field msg :maildir) "/cycling")))
          ;:vars '( ( user-mail-address       . "aderleth@example.com" )
                   ;( user-full-name          . "AliceD" )
                   ;( mu4e-compose-signature  . nil)))
        ))

  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.

  ;; start with the first (default) context; 
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask 
  ;; (setq mu4e-compose-context-policy nil)


(setq mu4e-maildir "~/Sync/Maildir"
	  ;mu4e-sent-folder "/cnr/Sent"
	  ;mu4e-drafts-folder "/cnr/Drafts"
	  ;mu4e-trash-folder "/cnr/Trash"
	  ;mu4e-refile-folder "/archive"
	  mu4e-view-show-addresses t
	  ;; show images
	  mu4e-show-images t)


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
;(setq mu4e-html2text-command "w3m -T text/html"
      ;mu4e-headers-auto-update t)
      ;mu4e-compose-signature-auto-include nil)


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

;;; remove attachments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-remove-attachment (msg num) 
  "Remove attachment." 
  (let* ((attach (mu4e~view-get-attach msg num))
         (path (mu4e-msg-field msg :path))
         (filename (and attach (plist-get attach :name)))
         (cmd (format "touch /tmp/%s; altermime --input=%s --replace='%s' 
         			  --with='/tmp/%s'"  filename path filename filename)))
    (when (and filename
               (yes-or-no-p
                (format "Are you sure you want to remove '%s'?" filename)))
      (shell-command cmd)
      (message cmd))))

(add-to-list 'mu4e-view-attachment-actions
             '("remove-attachment" . my-remove-attachment))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; remove attachments ;;;
(add-to-list 'mu4e-marks
  '(tag
     :char       "z"
     :prompt     "gtag"
     :ask-target (lambda () (read-string "What tag do you want to add?"))
     :action      (lambda (docid msg target)
                    (mu4e-action-retag-message msg (concat "+" target)))))

(mu4e~headers-defun-mark-for tag)
(define-key mu4e-headers-mode-map (kbd "z") 'mu4e-headers-mark-for-tag)
;;; shortcuts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mu4e-maildir-shortcuts
      '( ("/cnr/INBOX"        . ?i)
         ("/cnr/Sent"         . ?s)
         ("/cnr/Templates"    . ?t)
         ("/archive"          . ?a)
         ("/cnr/Drafts"       . ?d)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shortcuts ;;;

;;; Bookmarks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mu4e-bookmarks
      `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
        ("date:today..now" "Today's messages" ?t)
        ("date:7d..now" "Last 7 days" ?w)
        ("mime:image/*" "Messages with images" ?p)
        (,(mapconcat 'identity
                     (mapcar
                      (lambda (maildir)
                        (concat "maildir:" (car maildir)))
                      mu4e-maildir-shortcuts) " OR ")
         "All inboxes" ?i)))
(add-to-list 'mu4e-bookmarks
             '("size:5M..500M"       "Big messages"     ?b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Bookmarks ;;;

(setq mu4e-attachment-dir  "~/")

;https://github.com/agpchil/mu4e-maildirs-extension
(setq mu4e-maildirs-extension-maildir-collapsed-prefix "archives"
	  mu4e-maildirs-extension-default-collapse-level 1)


;; use org structures and tables in message mode
(add-hook 'message-mode-hook 'turn-on-orgtbl
		  'message-mode-hook 'turn-on-orgstruct++)

;; TODO
;; http://pragmaticemacs.com/emacs/email-templates-in-mu4e-with-yasnippet/

(provide 'mu4e-config)
