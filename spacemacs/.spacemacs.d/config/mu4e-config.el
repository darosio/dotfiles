(provide 'mu4e-config)

(setq mu4e-confirm-quit nil)

(setq mu4e-update-interval 120)
(setq mu4e-auto-retrieve-keys t)
(setq mu4e-headers-leave-behavior 'apply)
(setq mu4e-headers-visible-lines 20)
(setq mu4e-headers-auto-update t  ;; this is the default
      mu4e-hide-index-messages t  ;; hide updating messages
      mu4e-view-show-addresses t)  ;; show full addresses
(setq mu4e-get-mail-command "mbsync -a")
;; rename files when moving (Needed for mbsync)
(setq mu4e-change-filenames-when-moving t)
;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(setq mu4e-use-fancy-chars t)
(setq mu4e-compose-forward-as-attachment nil)

(setq mu4e-compose-complete-addresses t)  ;; default
(setq mu4e-compose-signature-auto-include t)  ;; default

;; VIEW ========================================================================
;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types) (imagemagick-register-types))
(setq mu4e-html2text-command 'mu4e-shr2text) 
;; (setq mu4e-html2text-command "w3m -T text/html")
;; (setq mu4e-html2text-command "html2text| grep -v '&nbsp_place_holder;'")
;; (setq mu4e-html2text-command "lynx -dump -width 120 -stdin")
(define-key 'mu4e-view-mode-map (kbd "<tab>") 'org-next-link)  ;; 'shr-next-link)
(define-key 'mu4e-view-mode-map (kbd "<backtab>") 'org-previous-link)  ;; 'shr-previous-link
;; include messages that belong to the same threads, just like Gmail ("W")
(setq mu4e-headers-include-related t)
;; skipping duplicates; toggle with 'C-v'
(setq mu4e-headers-skip-duplicates t)
;; remember actions to view messages 'a'
(setq mu4e-msg2pdf "/usr/bin/msg2pdf")
;; ;; using a dark theme, and the messages are hard to read?
;; (setq shr-color-visible-luminance-min 65)
;; Call EWW to display HTML messages   http://irreal.org/blog/?p=6122
(defun jcs-view-in-eww (msg)
  (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))
(add-to-list 'mu4e-view-actions '("Eww view" . jcs-view-in-eww) t)

;; the headers to show in the headers list [a pair of a field and its width]
;; with `nil' meaning 'unlimited' (better only use that for the last field).
(setq mu4e-headers-fields
      '( (:date          .  18)    ;; alternatively, use :human-date
         (:flags         .   5)
         (:from          .  22)
         (:mailing-list  .  12)
         (:subject       .  nil))) ;; alternatively, use :thread-subject


; Maildirs extension
(setq ;;mu4e-maildirs-extension-maildir-collapsed-prefix "archives"
      mu4e-maildirs-extension-default-collapse-level 0)

;; store all attachments of an email into the same folder
(setq mu4e-save-multiple-attachments-without-asking t)
(setq mu4e-attachment-dir  "~/")
;; view msg with fill column (toggle "SPC o o")
(add-hook 'mu4e-view-mode-hook 'visual-fill-column-mode)

;; ;; every new email composition gets its own frame! (window)
;; (setq mu4e-compose-in-new-frame t)
;; give me ISO(ish) format date-time stamps in the header list
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
;; customize the reply-quote-string
(setq message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n")
;; choose to use the formatted string
(setq message-citation-line-function 'message-insert-formatted-citation-line)
;; use helm for navigation =j o=
(setq  mu4e-completing-read-function 'completing-read)


;;; Sending ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq send-mail-function (quote sendmail-send-it)
      ;; sendmail-program "msmtp-enqueue.sh"
      sendmail-program "/usr/bin/msmtp"
      ;; Alternativeto ;;use mu mkdir  ~/Maildir/queue to set up first
      ;; (setq smtpmail-queue-mail nil  ;; start in normal mode
      ;;       smtpmail-queue-dir   "~/Maildir/queue/cur")
      mail-specify-envelope-from t ;'header
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header
      message-sendmail-f-is-evil nil
      mail-interactive t)
;;; Send delay ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (add-to-list 'load-path "~/.spacemacs.d/mu4e-send-delay")
  (require 'mu4e-send-delay)
  (mu4e-send-delay-setup)
  (add-hook 'mu4e-main-mode-hook 'mu4e-send-delay-initialize-send-queue-timer)
  ;; Assigning the scheduled enabled send to C-c C-c
  (add-hook 'mu4e-main-mode-hook (lambda ()
                                   (define-key mu4e-compose-mode-map
                                     (kbd "C-c C-c")
                                     'mu4e-send-delay-send-and-exit)))
  (setq mu4e-send-delay-default-delay "5m"
        mu4e-send-delay-timer 120)
  )
(setq mu4e-compose-signature-auto-include nil)
(spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
  "w" 'message-insert-signature)

(add-hook 'mu4e-compose-mode-hook 'evil-insert-state)
;; (add-hook 'mu4e-view-mode-hook 'xah-use-variable-width-font)
;; (add-hook 'mu4e-headers-mode-hook 'xah-use-variable-width-font)

;;; contexts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
         )
      ;; start with the first (default) context; 
      mu4e-context-policy 'pick-first
      ;; default is to ask-if-none (ask when there's no context yet, and none match)
      mu4e-compose-context-policy 'ask-if-none
      )
;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior
      (lambda ()
        (if (string= (message-sendmail-envelope-from) "daniele.arosio@pec.it")
            'sent
          'delete)))
;;; shortcuts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mu4e-maildir-shortcuts
      '( ("/cnr/INBOX"         . ?i)
         ("/gmail/Inbox"       . ?j)
         ("/gmail/archive"     . ?g)
         ("/cnr/Sent"          . ?s)
         ("/cnr/Templates"     . ?t)
         ("/archive"           . ?a)
         ("/archives/personal" . ?p)
         ("/cnr/refs"          . ?r)
         ("/cnr/Drafts"        . ?d)))
;;; Bookmarks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
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
  (add-to-list
   'mu4e-bookmarks
   '("flag:unread NOT flag:trashed AND (flag:list OR from:trac@sagemath.org)"
     "Unread bulk messages" ?l))
  )
;;; Remove attachments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://vxlabs.com/tag/mu4e/
(progn
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
               '("remove-attachment" . my-remove-attachment)))
;;; Tags like gmail and personal archive ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (add-to-list 'mu4e-marks
               ;; https://gist.github.com/lgatto/7091552
               '(tag
                 :char       "z"
                 :prompt     "gtag"
                 :ask-target (lambda () (read-string "What tag do you want to add?"))
                 :action      (lambda (docid msg target)
                                (mu4e-action-retag-message msg (concat "+" target)))))

  (add-to-list 'mu4e-marks
               '(personal
                 :char       "p"
                 :prompt     "personal"
                 :show-target (lambda (target) "personal")
                 :action      (lambda (docid msg target)
                                ;; must come before proc-move since retag runs
                                ;; 'sed' on the file
                                (mu4e-action-retag-message msg "-\\Inbox")
                                (mu4e~proc-move docid "/archives/personal" "+S-u-N"))))

  (mu4e~headers-defun-mark-for tag)
  (mu4e~headers-defun-mark-for personal)
  (define-key mu4e-headers-mode-map (kbd "z") 'mu4e-headers-mark-for-tag)
  (define-key mu4e-headers-mode-map (kbd "p") 'mu4e-headers-mark-for-personal)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; http://www.djcbsoftware.nl/code/mu/mu4e/Compose-hooks.html
(add-hook 'mu4e-compose-mode-hook
          (lambda()
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
            ;; (toggle-frame-maximized)
            (set-fill-column 79)
            ;; (visual-fill-column-mode)
            (turn-on-orgtbl)
            (turn-on-orgstruct)
            (turn-on-orgstruct++)
            (flyspell-mode)))
;; (add-hook 'message-mode-hook #'typo-mode)
;; (add-hook 'message-mode-hook #'footnote-mode)


;; Set format=flowed
;; mu4e sets up visual-line-mode and also fill (M-q) to do the right thing
;; each paragraph is a single long line; at sending, emacs will add the
;; special line continuation characters.
(setq mu4e-compose-format-flowed t)

;;store link to message if in header view, not to header query; or viceversa
(setq org-mu4e-link-query-in-headers-mode t)

;; when composing an email, switch on the special mu4e/orgmode mode
(define-key mu4e-compose-mode-map (kbd "C-c o") 'org~mu4e-mime-switch-headers-or-body)
;; when mail is sent, automatically convert org body to HTML
(setq org-mu4e-convert-to-html t)


;; ;; http://pragmaticemacs.com/emacs/email-templates-in-mu4e-with-yasnippet/
;; (add-hook 'mu4e-compose-mode-hook 'spacemacs/load-yasnippet)



; Multiattchments ============================================================
;; (eval-when-compile (require 'dired))
;; (defun iqbal-mu4e-file-attach-marked-files ()
;;   (gnus-dired-attach (dired-map-over-marks (dired-get-file-for-visit) nil)))
;; (defun iqbal-mu4e-attach-files-from-dired ()
;;   (interactive)
;;   (if (region-active-p)
;;       (iqbal-mu4e-file-attach-files-from-region)
;;     (iqbal-mu4e-file-attach-marked-files)))
;; (with-eval-after-load 'dired
;;   (define-key dired-mode-map (kbd "a") #'iqbal-mu4e-attach-files-from-dired))
;; (with-eval-after-load 'helm-files
;;   (add-to-list 'helm-find-files-actions
;;                '("Attach files for mu4e" . iqbal-helm-mu4e-attach) t)
;;   (defun iqbal-helm-mu4e-attach (_file)
;;     (gnus-dired-attach (helm-marked-candidates))))

;; ;; Attachment from locate "SPC f L" ... "C-z"
;; (helm-add-action-to-source "Attach to Email" #'mml-attach-file helm-source-locate)
;; Attach multiple files from helm-ind-files-actions (and dired)

;; cpaulik
        ;; (require 'helm)
        ;; (add-to-list 'helm-find-files-actions
        ;;              '("Attach files for mu4e" .
        ;;                helm-mu4e-attach) t)

        ;; (defun helm-mu4e-attach (_file)
        ;;   (gnus-dired-attach (helm-marked-candidates)))




;; spacemacs stuff
(evilified-state-evilify-map mu4e-main-mode-map
  :mode mu4e-main-mode
  :bindings
  (kbd "TAB") 'mu4e-maildirs-extension-toggle-maildir-at-point
  (kbd "j") 'mu4e~headers-jump-to-maildir)

;; starred flagged "f"
(evilified-state-evilify-map mu4e-headers-mode-map
  :mode mu4e-headers-mode
  :bindings
  (kbd "f") 'mu4e-headers-mark-for-flag)
(evilified-state-evilify-map mu4e-view-mode-map
  :mode mu4e-view-mode
  :bindings
  (kbd "f") 'mu4e-headers-mark-for-flag)

;; (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
;;   "t" 'message-goto-to
;;   "m" 'message-goto-body
;;   "b" 'message-goto-bcc
;;   "c" 'message-goto-cc
;;   "s" 'message-goto-subject)

;; TODO
;; (defun malb/fill-column-72 ()
;;   (set-fill-column 72))
;; (defun malb/mu4e-compose-frame ()
;;   (toggle-frame-maximized)
;;   (sleep-for 0.25) ;; this is a HACK
;;   (set-frame-size (selected-frame) 80 60)
;;   (sleep-for 0.25) ;; this is a HACK
;;   (set-window-dedicated-p (get-buffer-window (current-buffer)) t))
;; (add-hook 'mu4e-compose-mode-hook #'malb/fill-column-72)
;; (add-hook 'mu4e-compose-mode-hook #'malb/mu4e-compose-frame)


;; ;; required by mu4e-send-delay for sending correctly formatted email
;; (prefer-coding-system 'utf-8)
;; (set-language-environment "UTF-8")
