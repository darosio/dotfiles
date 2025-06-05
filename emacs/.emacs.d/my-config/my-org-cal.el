;;; my-org-cal.el --- To spell words -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Binding keys: "C-c"
;;; Code:

(use-package calendar                 ; calendars
  :straight (:type built-in)
  :hook
  (calendar-today-visible . calendar-mark-today)
  )

(use-package solar                    ; (2) sunrise and sunset
  :straight (:type built-in)
  :config
  (setq calendar-latitude 46.067270 ; Borino
        calendar-longitude 11.166153
        calendar-location-name "Trento"
        calendar-time-zone 60))

(use-package holidays                 ; (3) Holidays
  :straight (:type built-in)
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

(use-package org-gcal
  :bind
  ("C-c G p" . org-gcal-post-at-point) ; (add-hook 'org-capture-before-finalize-hook)
  ("C-c G d" . org-gcal-delete-at-point)
  ("C-c G g" . org-gcal-sync)
  ("C-c G G" . org-gcal-fetch)
  :commands (org-gcal-reload-client-id-secret)
  :init
  (which-key-add-key-based-replacements "C-c G" "Gcal")
  (setq org-gcal-client-id "1086004898054-uhp29b0kek41obv1dma52rpog8pr44gu.apps.googleusercontent.com")
  (setq org-gcal-client-secret "sP2Jupy5GKtdDAAgupQrSzc2")
  :config
  (setq org-gcal-auto-archive t)
  ;; (org-gcal-reload-client-id-secret)
  (setq org-gcal-file-alist
        '(("danielepietroarosio@gmail.com" . "~/Sync/box/org/gcal/dpa.org")
          ;; ("tq1af7efj4l9h8glgqi2g5vmsg@group.calendar.google.com" . "~/Sync/box/org/gcal/IBF.org")
          ("c87gevr5pc3191on8c7nh8b4nc@group.calendar.google.com" . "~/Sync/box/org/gcal/figli.org")))
  (setq plstore-cache-passphrase-for-symmetric-encryption t)
  ;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  )

(provide 'my-org-cal)
;;; my-org-cal.el ends here
