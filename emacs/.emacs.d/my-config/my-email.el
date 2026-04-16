;;; my-email.el --- To manage emails -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Binding keys: "C-x m"
;;; Code:

(use-package mu4e                       ; mu4e
  :straight (:type built-in)            ; in AUR/mu
  :commands (mu4e mu4e-compose-new)
  :preface
  (unless (locate-library "mu4e")
    (dolist (dir (append
                  '("/usr/share/emacs/site-lisp/mu4e")
                  (file-expand-wildcards
                   "/usr/share/emacs/site-lisp/elpa-src/mu4e-*" t)
                  (file-expand-wildcards
                   "/usr/share/emacs/site-lisp/elpa/mu4e-*" t)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir))))

  (defun my-mu4e--search-execute-a (orig-fun expr &optional ignore-history)
    "Force related messages off for archive trash searches.
ORIG-FUN is `mu4e--search-execute', EXPR is the search query, and
IGNORE-HISTORY is passed through unchanged."
    (let ((mu4e-search-include-related
           (and mu4e-search-include-related
                (not (and expr
                          (string-match-p "maildir:\\(\"/trash\"\\|/trash\\)"
                                          expr))))))
      (funcall orig-fun expr ignore-history)))

  (defun my-mu4e-compose-mode-hook ()
    "My settings for message composition."
    (set-fill-column 80)
    (visual-fill-column-mode)
    (guess-language-mode)
    (when-let ((ctx (mu4e-context-current))
               (name (mu4e-context-name ctx)))
      (cond
       ((string= name "pec")
        (save-excursion (message-remove-header "Bcc:*"))))))

  :hook
  (dired-mode . turn-on-gnus-dired-mode)
  (mu4e-view-mode . variable-pitch-mode)
  (mu4e-compose-mode . my-mu4e-compose-mode-hook)

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
         ("<tab>"     . org-next-link)    ; 'shr-next-link
         ("<backtab>" . org-previous-link)    ; 'shr-previous-link
         ("G"         . end-of-buffer)
         ("V"         . mu4e-view-verify-msg-popup)
         ("v"         . visual-fill-column-mode)
         ("I"         . (lambda () (interactive)   ; toggle remote images for this message
                          (setq-local shr-inhibit-images (not shr-inhibit-images))
                          (mu4e-view-refresh)))
         ("C-c l" . org-store-link)         ; requires ol.el
         ("f" . mu4e-view-mark-for-flag)
         ("D" . mu4e-headers-mark-for-delete)
          :map mu4e-headers-mode-map
          ("G" . end-of-buffer)
          ("D" . mu4e-headers-mark-for-delete)
          ("f" . mu4e-headers-mark-for-flag))

  :config
  (setq mail-user-agent 'mu4e-user-agent
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
        shr-inhibit-images t            ; block remote images by default (kills pixel trackers)
        mu4e-context-policy 'pick-first ; start with the first (default) context;
        mu4e-search-include-related t
        mu4e-search-skip-duplicates nil
        mu4e-compose-switch 'pop-up-frames)

  (auth-source-pass-enable)
  (setq ;; auth-source-debug t
   auth-source-do-cache nil
   auth-sources '(password-store))
  (setq smtpmail-queue-mail  nil
        smtpmail-queue-dir   "~/Maildir/queue/cur") ; Remember to "mu mkdir" and "touch /queue/.noindex"

  (setq mu4e-maildir-shortcuts
        '(
          ("/cnr/INBOX"               . ?c)
          ("/cnr/Sent Items"          . ?C)
          ("/cnr/refs"                . ?r)
          ("/cnr/keepup"              . ?k)
          ("/gmail/Inbox"             . ?j)
          ("/gmail/[Gmail]/Sent Mail" . ?s)
          ("/gmail/[Gmail]/Spam"      . ?x)
          ("/pec/INBOX"               . ?P)
          ("/personal"                . ?p)
          ("/archive"                 . ?a)
          ("/trash"                   . ?T)
          ))

  (setq mu4e-compose-format-flowed t
         mu4e-compose-context-policy 'ask-if-none
         mu4e-refile-folder "/archive"
         mu4e-trash-folder
         (lambda (msg)
           (let ((maildir (when msg (mu4e-message-field msg :maildir))))
             (cond
              ((and maildir (string-prefix-p "/cnr" maildir)) "/cnr/Deleted Items")
              ((and maildir (string-prefix-p "/gmail" maildir)) "/gmail/[Gmail]/Trash")
              ((and maildir (string-prefix-p "/pec" maildir)) "/pec/trash")
              ((and maildir
                    (or (string-prefix-p "/archive" maildir)
                        (string-prefix-p "/personal" maildir)
                        (string-prefix-p "/trash" maildir)))
               "/trash")
              (t (pcase (when-let ((ctx (mu4e-context-current)))
                          (mu4e-context-name ctx))
                   ("cnr" "/cnr/Deleted Items")
                   ("gmail" "/gmail/[Gmail]/Trash")
                   ("pec" "/pec/trash")
                   (_ "/trash"))))))
         message-signature nil)

  ;; Outlook/Office365 already stores sent mail server-side for the CNR account.
  ;; Let mu4e avoid filing a second local copy into /cnr/Sent Items.
  (setq mu4e-sent-messages-behavior
        (lambda ()
          (if (string= (message-sendmail-envelope-from) "daniele.arosio@cnr.it")
              'delete
            'sent)))

  (setq mu4e-bookmarks
        `(
          (:key ?i
                :name "All inboxes"
                :query "maildir:/cnr/INBOX OR maildir:/gmail/Inbox")
          (:key ?c
                :name "CNR inbox"
                :query "maildir:/cnr/INBOX")
          (:key ?a
                :name "Archive"
                :query "maildir:/archive")
          (:key ?t
                :name "Trash"
                :query "maildir:/trash")
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
  (advice-remove 'mu4e--search-execute #'my-mu4e--search-execute-a)
  (advice-add 'mu4e--search-execute :around #'my-mu4e--search-execute-a)
  )

(use-package mu4e-context
  :after mu4e
  :straight mu4e
  :config
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "cnr"
             :match-func (lambda (msg)
                           (when msg
                             (string-prefix-p "/cnr" (mu4e-message-field msg :maildir))))
             :vars '((user-mail-address . "daniele.arosio@cnr.it"  )
                     (user-full-name . "Daniele Arosio" )
                     (message-sendmail-extra-arguments . ("--account=cnr"))
                     (mu4e-sent-folder . "/cnr/Sent Items" )
                     (mu4e-drafts-folder . "/cnr/Drafts" )
                     (message-signature . "")))
           ,(make-mu4e-context
             :name "gmail"
             :match-func (lambda (msg)
                           (when msg
                             (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
              :vars '( (user-mail-address . "danielepietroarosio@gmail.com" )
                       (message-sendmail-extra-arguments . ("--account=gmail"))
                       (mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail" )
                       (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                       (message-signature  . "daniele Arosio\n38123 Trento\n")))
           ,(make-mu4e-context
             :name "pec"
             :match-func (lambda (msg)
                           (when msg
                             (string-prefix-p "/pec" (mu4e-message-field msg :maildir))))
              :vars '( (user-mail-address . "daniele.arosio@postecert.it" )
                       (user-full-name . "Daniele Arosio" )
                       (message-sendmail-extra-arguments . ("--account=pec"))
                       (mu4e-drafts-folder . "/pec/Drafts")
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
        message-sendmail-extra-arguments nil
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
  (mu4e~headers-defun-mark-for personal)
  ;; Gmail IMAP quirk: moving to Trash with the \Deleted (T) flag causes mbsync
  ;; (Expunge Both) to expunge the message locally before uploading it to
  ;; [Gmail]/Trash — leaving only the Inbox label removed, message in All Mail.
  ;; Use +S (Seen) instead so mbsync syncs the file to remote Trash first.
  (let ((trash-mark (alist-get 'trash mu4e-marks)))
    (when trash-mark
      (setf (alist-get 'trash mu4e-marks)
            (plist-put trash-mark :action
                       (lambda (docid msg target)
                         (mu4e--server-move docid target "+S-u-N")))))))

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
  (setq org-mime-library 'mml
        org-mime-export-ascii 'utf-8
        org-mime-export-options '(
                                  :with-latex dvipng
                                  :section-numbers nil
                                  :with-author nil
                                  :with-toc nil)))

(use-package mu4e-jump-to-list
  :after mu4e)

;;; Corrected Elisp function for Yazi/emacsclient
(eval-after-load 'mu4e
  '(progn
     (defun mu4e-compose-new-with-attachments (paths)
       "Compose a new mu4e message with files specified in the comma-separated string PATHS.
        This function is intended to be called by emacsclient."
       (interactive "sAttachments (comma-separated): ")
       (let* ((attachment-list (split-string paths "," t))
              (attachment-paths (mapcar (lambda (p) (expand-file-name (string-trim p))) attachment-list)))
         ;; 1. Compose a new message (this sets up the subject, etc.)
         (mu4e-compose-new)
         ;; 2. Attach each file individually using the standard message-mode function
         (dolist (file attachment-paths)
           (if (file-exists-p file)
               ;; mml-attach-file takes the file name and attaches it to the current buffer
               (mml-attach-file file nil nil "attachment")
             (message "Attachment not found: %s" file)))
         ;; 3. Bring the frame to the front
         (raise-frame)))
     ))

(provide 'my-email)
;;; my-email.el ends here
