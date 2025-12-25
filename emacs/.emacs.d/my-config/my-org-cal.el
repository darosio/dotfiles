;;; my-org-cal.el --- Org calendars -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Binding keys: "C-c"
;;; Code:

(use-package calendar                 ; calendars
  :ensure nil
  :hook
  (calendar-today-visible . calendar-mark-today))

(use-package solar                    ; sunrise and sunset
  :ensure nil
  :config
  (setq calendar-latitude 46.067270 ; Borino
        calendar-longitude 11.166153
        calendar-location-name "Trento"
        calendar-time-zone 60))

(use-package holidays                 ; Holidays
  :ensure nil
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

(use-package plstore
  :defer nil
  :config
  (setq plstore-encrypt-to '("8B6A39EFA290FB41")))

(use-package org-gcal
  :after (org-capture password-store plstore)
  :preface
  (defun my/org-timestamp-time-range-1h-or-all-day ()
    "Prompt for start and optional end date/time.
Return a valid Org timestamp string. If no time is entered, treat as all-day."
    (let* ((start-str (org-read-date nil nil nil "Start date/time:"))
           (start-time (org-read-date nil t start-str)) ; t → return time object
           (start-has-time (string-match-p "\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)" start-str))
           (end-str (org-read-date nil nil nil "End date/time (optional):"))
           (end-time (if (and end-str (not (string= end-str "")))
                         (org-read-date nil t end-str)
                       (if start-has-time
                           (time-add start-time (seconds-to-time 3600))
                         nil))))
      ;; Ensure end time is after start
      (when (and end-time (time-less-p end-time start-time))
        (setq end-time (time-add start-time (seconds-to-time 3600))))
      ;; Format timestamp
      (if start-has-time
          (if (and end-time (not (equal start-time end-time)))
              (format "<%s %s-%s>"
                      (format-time-string "%Y-%m-%d %a" start-time)
                      (format-time-string "%H:%M" start-time)
                      (format-time-string "%H:%M" end-time))
            (format "<%s %s>"
                    (format-time-string "%Y-%m-%d %a" start-time)
                    (format-time-string "%H:%M" start-time)))
        ;; all-day event
        (format "<%s>" (format-time-string "%Y-%m-%d %a" start-time)))))

  :commands (org-gcal-reload-client-id-secret)
  :init
  (which-key-add-key-based-replacements "C-c G" "Gcal")
  (setq org-gcal-client-id (password-store-get "org-gcal/client-id")
        org-gcal-client-secret (password-store-get "org-gcal/client-secret"))
  (org-gcal-reload-client-id-secret)
  ;; Auto cache GPG key
  (setq plstore-cache-passphrase-for-symmetric-encryption t)
  :config
  (setq org-gcal-auto-archive t)
  (setq org-gcal-file-alist
        '(("danielepietroarosio@gmail.com" . "~/Sync/box/org/gcal/dpa.org")
          ("c87gevr5pc3191on8c7nh8b4nc@group.calendar.google.com" . "~/Sync/box/org/gcal/figli.org")))
  ;;  Use custom function instead of SCHEDULED: %^t\n
  (add-to-list
   'org-capture-templates
   '("gg" "Google Calendar event" entry
     (file+headline "~/Sync/box/org/gcal/dpa.org" "Inbox")
     "* %?\n  SCHEDULED: %(my/org-timestamp-time-range-1h-or-all-day)\n  :PROPERTIES:\n  :ORG-GCAL-ID:\n  :LOCATION: \n  :END:\n:org-gcal:\n%i\n%a\n:END:\n"))
  (add-to-list
   'org-capture-templates
   '("gf" "Google Calendar event" entry
     (file+headline "~/Sync/box/org/gcal/figli.org" "Inbox")
     "* %?\n  SCHEDULED: %(my/org-timestamp-time-range-1h-or-all-day)\n  :PROPERTIES:\n  :ORG-GCAL-ID:\n  :LOCATION: \n  :END:\n:org-gcal:\n%i\n%a\n:END:\n"))
  :bind
  ("C-c G p" . org-gcal-post-at-point) ; (add-hook 'org-capture-before-finalize-hook)
  ("C-c G d" . org-gcal-delete-at-point)
  ("C-c G G" . org-gcal-sync)
  ("C-c G F" . org-gcal-fetch)
  ("C-c G g" . (lambda () (interactive) (org-gcal-sync) (org-capture nil "gg")))
  ("C-c G f" . (lambda () (interactive) (org-gcal-sync) (org-capture nil "gf"))))

(provide 'my-org-cal)
;;; my-org-cal.el ends here
