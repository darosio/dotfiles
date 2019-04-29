;;; packages.el --- org-gcal layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <dan@hunza>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `org-gcal-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-gcal/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-gcal/pre-init-PACKAGE' and/or
;;   `org-gcal/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst org-gcal-packages
  '(org-gcal))

(defun org-gcal/init-org-gcal ()
  (use-package org-gcal
    ;; :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "og" #'org-gcal-sync
        "oG" #'org-gcal-fetch))
    :config
    (progn
      ;; gcal
      (setq org-gcal-token-file "~/.spacemacs.d/org-gcal/.org-gcal-token"
            org-gcal-logo nil)
      (setq org-gcal-client-id "100447390762-1unqkpjv30do2uq0r5uetd8pkr9ha81s.apps.googleusercontent.com"
            org-gcal-client-secret "EhuKxnNmAoM2XMPjCFOpiLDK"
            org-gcal-file-alist '(("danielepietroarosio@gmail.com" .
                                   "~/Sync/share/phone/box/org/gcal/dpa.org")
                                  ("c87gevr5pc3191on8c7nh8b4nc@group.calendar.google.com" .
                                   "~/Sync/share/phone/box/org/gcal/figli.org")
                                  ("tq1af7efj4l9h8glgqi2g5vmsg@group.calendar.google.com" .
                                   "~/Sync/share/phone/box/org/gcal/IBF.org")
                                  ;; ("cfaned8dou8gm2qciies0itso4@group.calendar.google.com" .
                                  ;;  "~/Sync/org/gcal/deadlines.org")
                                  ;; ("i_217.77.81.46#sunrise@group.v.calendar.google.com" .
                                  ;;  "~/Sync/org/gcal/sunrise.org")
                                  ;; ("it.italian#holiday@group.v.calendar.google.com" .
                                  ;;  "~/Sync/org/gcal/feste.org")
                                  ))
      ;; syncs whenever I load the agenda. Since this happens in the background,
      ;; if I just added something to my calendar, I might have to reload the agenda by hitting r in the agenda view.
                                        ;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
      ;; syncs with my Google calendar when I capture.
                                        ;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
      )))

(defun org-gcal/post-init-org-gcal ()
    (with-eval-after-load 'org
      ;; (2) Sunrise and sunset
      (setq calendar-latitude 46.067270) ; Borino
      (setq calendar-longitude 11.166153)
      (setq calendar-location-name "Trento")
      (setq calendar-time-zone 60)
      ;; (3) Holidays (setq holiday-general-holidays '(
      (setq holiday-other-holidays
            '((holiday-fixed 1 1 "Capodanno")
              (holiday-fixed 5 1 "1 Maggio")
              (holiday-fixed 4 25 "Liberazione")
              (holiday-fixed 6 2 "Festa Repubblica")
              (holiday-fixed 7 14 "Bastille Day")
              ))
      (setq holiday-bahai-holidays nil)
      (setq holiday-hebrew-holidays nil)
      ;; (setq holiday-islamic-holidays nil)
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
       ))

;; packages.el ends here
