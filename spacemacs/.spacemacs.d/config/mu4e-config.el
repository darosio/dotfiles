;; allow for updating mail using 'U' in the main view:
;(setq mu4e-get-mail-command "offlineimap -u quiet")
(require 'org-mu4e)

;; https://martinralbrecht.wordpress.com/2016/05/30/handling-email-with-emacs/
(require 'helm-mu)
(bind-key "S" #'helm-mu mu4e-main-mode-map)


(add-hook 'message-mode-hook #'flyspell-mode)
(add-hook 'message-mode-hook #'typo-mode)
(add-hook 'message-mode-hook #'adict-guess-dictionary)
(add-hook 'message-mode-hook #'footnote-mode)

(helm-add-action-to-source "Attach to Email" #'mml-attach-file helm-source-locate)

;; Attach multiple files from helm-ind-files-actions (and dired)
(eval-when-compile (require 'dired))
(defun iqbal-mu4e-file-attach-marked-files ()
  (gnus-dired-attach (dired-map-over-marks (dired-get-file-for-visit) nil)))
(defun iqbal-mu4e-attach-files-from-dired ()
  (interactive)
  (if (region-active-p)
      (iqbal-mu4e-file-attach-files-from-region)
    (iqbal-mu4e-file-attach-marked-files)))
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "a") #'iqbal-mu4e-attach-files-from-dired))
(with-eval-after-load 'helm-files
  (add-to-list 'helm-find-files-actions
               '("Attach files for mu4e" . iqbal-helm-mu4e-attach) t)
  (defun iqbal-helm-mu4e-attach (_file)
    (gnus-dired-attach (helm-marked-candidates))))
;; http://www.djcbsoftware.nl/code/mu/mu4e/Dired.html#Dired
(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))
(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)





(setq mu4e-confirm-quit nil)
(setq mu4e-get-mail-command "mbsync -a"
	  mu4e-update-interval 190
      mu4e-headers-auto-update t
	  ;; rename files when moving NEEDED FOR MBSYNC
	  mu4e-change-filenames-when-moving t)

(setq mu4e-maildir "~/Sync/Maildir")

(setq send-mail-function (quote sendmail-send-it)
      ;; message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp-enqueue.sh"
      mail-specify-envelope-from t ;'header
      message-sendmail-f-is-evil nil
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header
      mail-interactive t)

;; send delay
(add-to-list 'load-path "~/.spacemacs.d/mu4e-send-delay")
(require 'mu4e-send-delay)
(mu4e-send-delay-setup)
(add-hook 'mu4e-main-mode-hook 'mu4e-send-delay-initialize-send-queue-timer)
(add-hook 'mu4e-main-mode-hook (lambda ()
                                 (define-key mu4e-compose-mode-map
                                   (kbd "C-c C-c")
                                   'mu4e-send-delay-send-and-exit)))
(setq mu4e-send-delay-default-delay "5m"
      mu4e-send-delay-timer 190)
;; TODO
;; http://pragmaticemacs.com/emacs/migrating-from-offlineimap-to-mbsync-for-mu4e/
;; ;;set up queue for offline email
;; ;;use mu mkdir  ~/Maildir/queue to set up first
;; (setq smtpmail-queue-mail nil  ;; start in normal mode
;;       smtpmail-queue-dir   "~/Maildir/queue/cur")

;;; Reading messages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call EWW to display HTML messages
(defun jcs-view-in-eww (msg)
  (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))

;; Arrange to view messages in either the default browser or EWW
(add-to-list 'mu4e-view-actions '("Eww view" . jcs-view-in-eww) t)
;; see html as text; use 'a V' to open in browser
;(setq mu4e-html2text-command "w3m -T text/html")
;; usually not as awesome as w3m, but preserves urls of google alerts
;(setq mu4e-html2text-command "html2text| grep -v '&nbsp_place_holder;'")
(setq mu4e-html2text-command 'mu4e-shr2text) 
(add-hook 'mu4e-view-mode-hook
  (lambda()
    ;; try to emulate some of the eww key-bindings
    (local-set-key (kbd "<tab>") 'shr-next-link)
    (local-set-key (kbd "<backtab>") 'shr-previous-link)))
;; using a dark theme, and the messages are hard to read?
(setq shr-color-visible-luminance-min 65)

;; attempt to show images when viewing messages
(setq mu4e-view-show-images t
      mu4e-show-images t
      mu4e-view-image-max-width 800)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
;; not only show the messages that directly match a certain query, but also
;; include messages that belong to the same threads, just like Gmail.
;; toggle with 'W'
(setq mu4e-headers-include-related t)
;; skipping duplicates; toggle with 'C-v'
(setq mu4e-headers-skip-duplicates t)
;; remember actions to view messages 'a'
(setq mu4e-msg2pdf "/usr/bin/msg2pdf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reading messages ;;;

;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")
;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior
      (lambda ()
        (if (string= (message-sendmail-envelope-from) "danielepietroarosio@gmail.com")
            'delete
          'sent)))

;; something about ourselves
; mu4e-reply-to-address "daniele.arosio@cnr.it"


;;; contexts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mu4e-contexts
    `( ,(make-mu4e-context
          :name "cnr"
          :enter-func (lambda () (mu4e-message "Entering Cnr context"))
          :leave-func (lambda () (mu4e-message "Leaving Cnr context"))
          ;; we match based on the maildir folder http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
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
  						"National Research Council (CNR), Institute of Biophysics\n"
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
         :vars '( (user-mail-address  . "daniele.arosio@pec.it" )
                  (user-full-name     . "Daniele Arosio" )
                  (mu4e-drafts-folder . "/pec/Bozze")
                  (mu4e-trash-folder  . "/pec/Cestino")
                  (mu4e-sent-folder   . "/pec/Inviata")
                  (mu4e-compose-signature .
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
;; start with the first (default) context; 
;; default is to ask-if-none (ask when there's no context yet, and none match)
(setq mu4e-context-policy 'pick-first
      mu4e-compose-context-policy 'ask-if-none)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; contexts ;;;


;;; https://vxlabs.com/tag/mu4e/ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I want my format=flowed thank you very much
(setq mu4e-compose-format-flowed t)
;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)
;; every new email composition gets its own frame! (window)
(setq mu4e-compose-in-new-frame t)
;; give me ISO(ish) format date-time stamps in the header list
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; (better only use that for the last field.
;; These are the defaults:
(setq mu4e-headers-fields
    '( (:date          .  18)    ;; alternatively, use :human-date
       (:flags         .   5)
       (:from          .  22)
       (:mailing-list  .  12)
       (:subject       .  nil))) ;; alternatively, use :thread-subject
;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; https://vxlabs.com/tag/mu4e/ ;;;

;;; remove attachments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; remove attachments ;;;

;;; Tags like ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://gist.github.com/lgatto/7091552
(add-to-list 'mu4e-marks
  '(tag
     :char       "z"
     :prompt     "gtag"
     :ask-target (lambda () (read-string "What tag do you want to add?"))
     :action      (lambda (docid msg target)
                    (mu4e-action-retag-message msg (concat "+" target)))))
(add-to-list 'mu4e-marks
  '(archive
     :char       "A"
     :prompt     "Archive"
     :show-target (lambda (target) "archive")
     :action      (lambda (docid msg target)
                    ;; must come before proc-move since retag runs
                    ;; 'sed' on the file
                    (mu4e-action-retag-message msg "-\\Inbox")
                    (mu4e~proc-move docid nil "+S-u-N"))))

(mu4e~headers-defun-mark-for tag)
(define-key mu4e-headers-mode-map (kbd "z") 'mu4e-headers-mark-for-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tags like ;;;

;;; shortcuts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mu4e-maildir-shortcuts
      '( ("/cnr/INBOX"        . ?i)
         ("/gmail/Inbox"      . ?j)
         ("/gmail/archive"    . ?g)
         ("/cnr/Sent"         . ?s)
         ("/cnr/Templates"    . ?t)
         ("/archive"          . ?a)
         ("/cnr/refs"         . ?r)
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
(add-to-list 'mu4e-bookmarks
			 '( "maildir:\"/INBOX\" and flag:flagged" "Flagged in INBOX" ?f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Bookmarks ;;;

(setq mu4e-attachment-dir  "~/")

;https://github.com/agpchil/mu4e-maildirs-extension
(setq mu4e-maildirs-extension-maildir-collapsed-prefix "archives"
	  mu4e-maildirs-extension-default-collapse-level 1)


;; starred flagged "f"
;; http://pragmaticemacs.com/category/elfeed/
(defalias 'mu4e-headers-mark-for-flag
  (eval-after-load 'mu4e-header
    '(define-key mu4e-headers-mode-map (kbd "f") 'mu4e-headers-flagged-mark)))


;; spell check
;(add-hook 'mu4e-compose-mode-hook
        ;(defun my-do-compose-stuff ()
           ;"My settings for message composition."
           ;(set-fill-column 72)
           ;(flyspell-mode)))
;; configure orgmode support in mu4e
;; (require 'org-mu4e)
;; when mail is sent, automatically convert org body to HTML
(setq org-mu4e-convert-to-html t)

;;store org-mode links to messages
;;store link to message if in header view, not to header query
;(setq org-mu4e-link-query-in-headers-mode nil)
; sembra non servire (http://pragmaticemacs.com/emacs/master-your-inbox-with-mu4e-and-org-mode/)
;; use org structures and tables in message mode
(add-hook 'message-mode-hook 'turn-on-orgtbl
          'message-mode-hook 'turn-on-orgstruct++)
;; TODO
;; http://pragmaticemacs.com/emacs/email-templates-in-mu4e-with-yasnippet/
;; customize the reply-quote-string
(setq message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n")
;; choose to use the formatted string
(setq message-citation-line-function 'message-insert-formatted-citation-line)

(provide 'mu4e-config)
