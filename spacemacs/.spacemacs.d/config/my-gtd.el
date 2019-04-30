(provide 'my-gtd)

;; a better word processor
(setq org-hide-emphasis-markers t)

;; A bare minimum simple starting to personalizing org for gtd.
;; https://orgmode.org/worg/org-configs/org-customization-guide.html

;; Define variables
(progn
  ;; (eval-and-compile
  (load-library "find-lisp")
  (defvar da-agenda-and-refile-files (append
                                      '("~/Sync/share/phone/box/org/gtd.org"
                                        "~/Sync/share/phone/box/org/ideas.org"
                                        "~/Sync/share/phone/box/org/inbox.org"
                                        "~/Sync/share/phone/box/org/projects.org"
                                        "~/Sync/share/phone/box/org/someday.org"
                                        "~/Sync/share/phone/box/org/TODOs.org" ;; target for org-projectile
                                        "~/Sync/share/phone/box/org/spesa.org")
                                      (find-lisp-find-files "~/Sync/notes/arch/" "\.org$")
                                      (find-lisp-find-files "~/Sync/notes/home/" "\.org$")
                                      (find-lisp-find-files "~/Sync/proj/" "\.org$"))
    "Files forming the agenda and refile targets"))

;; (1) Agenda files; (2) Archives; and (3) Refile
(progn
  (setq-default org-directory "~/Sync/share/phone/box/org")
  (setq org-agenda-files (append da-agenda-and-refile-files
                            '("~/Sync/share/phone/box/org/diary.org"
                              "~/Sync/share/phone/box/org/gcal/")))
  (setq org-agenda-diary-file "~/Sync/share/phone/box/org/diary.org"
        org-agenda-include-diary t)
  ;; ARCHIVE
  (setq org-archive-location "~/Sync/share/phone/box/org/archives/%s_archive::")
  (defvar org-archive-file-header-format "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n")
  ;; REFILE
  (setq org-refile-targets '((da-agenda-and-refile-files :maxlevel . 5)))
  ;; Be sure to use the full path preceded by filename to insert at top level
  (setq org-refile-use-outline-path 'file)
  ;; Targets complete directly with helm/ido
  (setq org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes 'confirm))

;; (4) Stuck project; and (5) TODOs and tags
(progn
  (setq org-stuck-projects
        '("+proj/-DONE-HOLD" ("NEXT") nil ""))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")) )
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("NEXT" :foreground "light blue" :weight bold)
          ;; ("APPT" :foreground "yellow" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)
          ("MEETING" :foreground "forest green" :weight bold)
          ))
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD")) ))
  (setq org-use-fast-todo-selection t)
  ;; S-left S-right skipping setting timespamps  DEF
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)
  ;; for when I set a task as e.g. Canceled open the buffer in insert state
  (add-hook 'org-log-buffer-setup-hook 'evil-insert-state)
  (setq org-tag-persistent-alist '((:startgroup)  ;; mutually exclusive
                                   ("@errand" . ?e)
                                   ("@fbk" . ?f)
                                   ("@home" . ?h)
                                   ("@telephone" . ?t)
                                   (:endgroup)
                                   ("@net" . ?n)  ;; I doubt it is usefull
                                   ("PERSONAL" . ?p)
                                   ("WORK" . ?w)))
  (setq org-tag-alist (quote (("@dati" . ?d)
                              ("@mail" . ?m)
                              ("idea" . ?i)
                              ("proj" . ?j)
                              )))
  ;; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key 'expert)
  ;; Include the todo keywords
  (setq org-fast-tag-selection-include-todo nil)
  )

;; (6) Captures
;; TODO: fine tune capture templates
(progn
  (setq org-default-notes-file "~/Sync/share/phone/box/org/inbox.org")
  (defvar da-gtd "~/Sync/share/phone/box/org/gtd.org")
  (define-key global-map "\C-ct" (lambda () (interactive) (org-capture nil "t")))
  (setq org-capture-templates
        '(("r" "Reply to" entry (file+headline da-gtd "Reply")
           "* TODO %a to %:from \nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n %?" :unnarrowed)
          ("w" "Wait for Reply" entry (file+headline da-gtd "Reply")
           "* WAITING %a from %:from" :immediate-finish t)
          ("t" "todo" entry (file org-default-notes-file)
           "* TODO %? \n%U\n%a\n" :clock-in t :clock-resume t :unnarrowed :kill-buffer)
          ("a" "Appointment" entry (file  "~/Sync/share/phone/box/org/gcal/dpa.org")
           "* %? %^{LOCATION}p\n%^T\n%a\n")
          ("h" "new Habit" entry (file+headline da-gtd "Habits")
           "* TODO %? \nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:REPEAT_TO_STATE: TODO\n:END:\n%a")
          ("u" "Urgent task" entry (file+headline da-gtd "Tasks") "** NEXT %? \nDEADLINE: %t")
          ("i" "new Idea" entry (file "~/Sync/share/phone/box/org/ideas.org") "* %^{Idea} \n%u\n%?")
          ;; diary.org
          ("j" "Journal" entry (file+olp+datetree "~/Sync/share/phone/box/org/diary.org") "* %?\n%t\n" )
          ("J" "Journalp" entry (file+olp+datetree+prompt "~/Sync/share/phone/box/org/diary.org") "* %?\n%t\n" )
          ("m" "Meeting" entry (file+olp+datetree "~/Sync/share/phone/box/org/diary.org") "* MEETING %? :MEETING:\n%T" :clock-in t :clock-resume t)
          ;; spesa.org
          ("s" "Spesa" entry (file+headline "~/Sync/share/phone/box/org/spesa.org" "Supermarket") ;; TODO: try checkitem
           "* TODO %? \n")
          ("rd" "Review: Daily" entry (file+olp+datetree "/tmp/reviews.org")
           (file "~/Sync/share/phone/box/org/chklists/daily-review.template.org"))
          ))
  (setq org-capture-templates-contexts
        '(("r" ((in-mode . "mu4e-view-mode")))
          ("w" ((in-mode . "mu4e-view-mode")))))
  (add-hook 'org-capture-mode-hook 'evil-insert-state))



;; Display properties
(setq org-cycle-separator-lines 2) ;; default
(setq org-tags-column -82)
;; (setq org-agenda-tags-column -82)
;; (setq org-agenda-sticky t)
(setq org-columns-default-format
      "%48ITEM(Task) %4TODO(todo) %6CLOCKSUM{:} %ALLTAGS %SCHEDULED %6Effort(Effort){:} %DEADLINE")



;; == Agenda ==
(setq org-agenda-show-future-repeats nil)  ;; 'next to view this and the next.
;; TODO custom agenda view http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
(setq org-agenda-inhibit-startup nil)
;; Dim blocked tasks
(setq org-agenda-dim-blocked-tasks t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t)
;; Compact the block agenda view (disabled)
;; (setq org-agenda-compact-blocks nil)

;; ;; Set the times to display in the time grid
;; (setq org-agenda-time-grid
;;   '((daily today require-timed)
;;     "----------------"
;;     (800 1200 1300 1800)))

;; ;; for lazy boolean search =C-a a s=
;; (setq org-agenda-search-view-always-boolean t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))
;; For tag searches ignore tasks with scheduled and deadline dates FIXME better control this in each agenda custom view
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-deadline-warning-days 70)
;; (org-use-property-inheritance t) @2DO to be used with STYLE, but prefer to ignore scheduled
;; all properties are inherited
(setq org-use-property-inheritance t) ;; @2DO to be used with STYLE, e.g. habit not scheduled

(defun gs/org-agenda-add-location-string ()
  "Gets the value of the LOCATION property"
  (let ((loc (org-entry-get (point) "LOCATION")))
    (if (> (length loc) 0)
        (concat "{" loc "} ")
      "")))
(setq org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
                            (timeline . "  % s")
                            (todo . "  %-12:c  ")
                            (tags . "  %-12:c  ")
                            (search . "  %i %-12:c")))

;; (defun nemacs-org-agenda-startup ()
;;   (interactive)
;;   (org-agenda :keys "gtd"))

;; (defun nemacs-org-agenda-mark-as-done (&optional arg)
;;   (interactive "P")
;;   (org-agenda-todo "DONE"))

;; (defun nemacs-org-agenda-mark-as-done-capture-follow-up (&optional arg)
;;   (interactive "P")
;;   (org-agenda-todo "DONE")
;;   (org-agenda-switch-to)
;;   (org-capture 0 "t"))
;; :hook (org-agenda-mode . nemacs-org-agenda-hook)
;; :bind (("C-c a" . org-agenda)
;;        ("C-c d" . nemacs-org-agenda-startup)
;;        (:map org-agenda-mode-map
;;              ("g" . org-gcal-fetch)
;;              ("x" . nemacs-org-agenda-mark-as-done)
;;              ("X" . nemacs-org-agenda-mark-as-done-capture-follow-up)))

;; (org-agenda-skip-deadline-if-done nil)
;; (org-agenda-skip-scheduled-if-done nil)

  ;; (defun nemacs-org-capture-add-basic-properties ()
  ;;   (interactive)
  ;;   (org-id-get-create))

  (defun nemacs-org-capture-review-daily ()
    (interactive)
    (progn
      (org-capture nil "rd")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (org-gcal-fetch)
      (org-clock-in)))
(define-key global-map "\C-crd" 'nemacs-org-capture-review-daily)

  ;; (defun nemacs-org-capture-review-weekly ()
  ;;   (interactive)
  ;;   (progn
  ;;     (org-capture nil "rw")
  ;;     (org-capture-finalize t)
  ;;     (org-speed-move-safe 'outline-up-heading)
  ;;     (org-narrow-to-subtree)
  ;;     (org-gcal-fetch)
  ;;     (org-clock-in)))
  ;; :hook (org-capture-before-finalize . nemacs-org-capture-add-basic-properties)
  ;; :bind (("M-m"     . nemacs-org-capture)
  ;;        ("C-c c"   . org-capture)
  ;;        ("C-c r d" . nemacs-org-capture-review-daily)
  ;;        ("C-c r w" . nemacs-org-capture-review-weekly))
  ;; :custom
  ;; (org-capture-templates `(("t" "Add TODO Task" entry (file ,org-default-notes-file)
  ;;                           ,nemacs-org-capture-basic-template
  ;;                           :empty-lines 1 :immediate-finish t)
  ;;                          ("T" "Add Linked TODO Task" entry (file ,org-default-notes-file)
  ;;                           ,nemacs-org-capture-link-template
  ;;                           :empty-lines 1 :immediate-finish t)
  ;;                          ("c" "Add Contact" entry (file "~/Dropbox/orgfiles/contacts.org")
  ;;                           ,nemacs-org-capture-contact-template
  ;;                           :empty-lines 1)

  ;;                          ("rw" "Review: Weekly" entry (file+olp+datetree "/tmp/reviews.org")
  ;;                           (file "~/Dropbox/orgfiles/templates/weekly-review.template.org")))))

(setq org-agenda-custom-commands
      '(
        ("r" "Daily Review" ((tags-todo "Today")
                             (tags-todo "Tomorrow")
                             (agenda "" ((org-agenda-span 2)
                                         (org-deadline-warning-days 7)
                                         (org-agenda-start-on-weekday nil)))))
        ("u" "Unscheduled TODOs" ((todo "TODO"
                                        ((org-agenda-overriding-header "Unscheduled TODO")
                                         (org-agenda-todo-ignore-scheduled 'future)))))
        ("gtd" "My GTD Agenda" ((agenda "" ((org-agenda-overriding-header "Getting Things Done")
                                            (org-agenda-span 1)
                                            (org-deadline-warning-days 7)
                                            (org-agenda-start-on-weekday nil)))))

        ("H" "Habits" tags-todo "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-tags-match-list-sublevels 'indented)
          (org-agenda-sorting-strategy '(category-keep))) ;; todo-state-down effort-up
         )
        ;; ("n" . "Next Action lists")
        ("f" "Upcoming week and deadlines"
         ((agenda "weeks"
                  ((org-agenda-span 14)))
          (agenda "deadlines"
                  ((org-agenda-span 60)
                   (org-agenda-overriding-header "deadlines")
                   (org-agenda-entry-types '(:deadline)) ;; this entry excludes :scheduled
                   (org-agenda-start-day "+14d"))))
         ((org-agenda-time-grid nil)
          (org-agenda-category-filter-preset '("-Habits")) ;; exclude gtd.org/Habits by property category="Habits"
          (org-agenda-show-all-dates nil)
          (org-deadline-warning-days 730)))
        ("w" "Action list excluding PERSONAL"
         ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
                      (org-agenda-span 2)))
          (tags-todo "-CANCELLED/NEXT"
                     ((org-agenda-overriding-header "Next Tasks:")
                      (org-agenda-sorting-strategy '(habit-up category-keep priority-down))
                      (org-tags-match-list-sublevels 'indented)))
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-agenda-todo-ignore-scheduled nil)
                 (org-agenda-todo-ignore-deadlines nil)
                 (org-tags-match-list-sublevels nil)))
          (tags "-NOTE-REFILE-ARCHIVE/DONE|CANCELLED"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-todo-ignore-scheduled nil)
                 (org-agenda-todo-ignore-deadlines nil)
                 ))
          (tags-todo "-proj-HOLD-CANCELLED-REFILE-STYLE=\"habit\"/!-NEXT-WAITING-HOLD-CANCELLED"
                     ((org-agenda-overriding-header "Standalone Tasks")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(habit-up category-keep priority-down))))
          (tags-todo "-proj/!WAITING"
                     ((org-agenda-overriding-header "Standalone Waiting Tasks")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(category-keep)))))
         ((org-agenda-tag-filter-preset '("-PERSONAL"))
          (org-agenda-todo-ignore-scheduled t)
          (org-agenda-todo-ignore-deadlines t)
          ))
        ("A" "Agenda for Today"
         ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
                      (org-deadline-warning-days 0)
                      (org-agenda-span 1)))))
        ("z" "Agenda for Today"
         ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
                      (org-deadline-warning-days 0)
                      (org-agenda-span 1)))
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil) ;; I want to refile the whole tree; no need to visualize sublevels.
                 (org-agenda-todo-ignore-scheduled nil)
                 (org-agenda-todo-ignore-deadlines nil)))
          (tags "-NOTE-ARCHIVE/DONE|CANCELLED"  ;; FIXME -REFILE better leave out
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-todo-ignore-scheduled nil)
                 (org-agenda-todo-ignore-deadlines nil)))
          (agenda "" ((org-agenda-overriding-header "Tomorrow's Schedule:")
                       (org-agenda-span 2)))
          (tags-todo "-CANCELLED/NEXT"
                     ((org-agenda-overriding-header "Next Tasks:")
                      (org-agenda-sorting-strategy '(habit-up category-keep priority-down))
                      (org-tags-match-list-sublevels 'indented)))
          (tags-todo "-proj/!WAITING"
                     ((org-agenda-overriding-header "Standalone Waiting Tasks")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(category-keep)))))
         ((org-agenda-todo-ignore-scheduled t)
          (org-agenda-todo-ignore-deadlines t)))
        ("p" "Action list only PERSONAL"
         ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
                      (org-agenda-span 'day)))
          (tags-todo "+PERSONAL-CANCELLED/NEXT"
                     ((org-agenda-overriding-header "Next Tasks:")
                      (org-agenda-sorting-strategy '(habit-up category-keep priority-down))
                      (org-tags-match-list-sublevels 'indented)))
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (tags "-proj-NOTE-REFILE-ARCHIVE/DONE|CANCELLED"
                ((org-agenda-overriding-header "Tasks to Archive")))
          (tags-todo "-proj-HOLD-CANCELLED-REFILE-STYLE=\"habit\"/!-NEXT-WAITING-HOLD-CANCELLED"
                     ((org-agenda-overriding-header "Standalone Tasks")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-proj/!WAITING"
                     ((org-agenda-overriding-header "Standalone Waiting Tasks")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(habit-up category-keep priority-down)))))
         ((org-agenda-tag-filter-preset '("+PERSONAL"))
          (org-agenda-todo-ignore-scheduled t)
          (org-agenda-todo-ignore-deadlines t)
          ))
        ("j" "proJects"
         ((tags "-HOLD-CANCELLED+proj"
                ((org-agenda-overriding-header "active projects")
                 (org-use-tag-inheritance nil)  ;; only headlines tagged:proj
                 (org-tags-match-list-sublevels 'indented)
                 (org-agenda-skip-function '(org-agenda-skip-subtree-if 'nottodo '("NEXT")))
                 (org-agenda-sorting-strategy '(category-keep))))
          (stuck ""
                 ((org-agenda-overriding-header "stuck projects")
                  (org-use-tag-inheritance nil)
                  (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "+proj/!NEXT"
                     ((org-agenda-overriding-header "next project tasks")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":proj:"))
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "+proj/!WAITING"
                     ((org-agenda-overriding-header "waiting project tasks")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":proj:"))
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "+proj/!TODO"
                     ((org-agenda-overriding-header "todo project tasks")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":proj:"))
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags "+proj/-WAITING-TODO-NEXT"
                ((org-agenda-overriding-header "other project headings")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":proj:"))
                 (org-tags-match-list-sublevels 'indented)
                 (org-agenda-sorting-strategy '(category-keep))))
          )
         (
          ;; (org-agenda-tag-filter-preset '("-linux" "+PERSONAL"))
          ;; (org-agenda-start-with-log-mode t)
          ;; (org-agenda-log-mode-items 'clock)
          ;; (org-agenda-todo-ignore-deadlines 'near)
          (org-agenda-todo-ignore-scheduled t)
          (org-agenda-todo-ignore-deadlines t)

          (ps-number-of-columns 2)
          (ps-landscape-mode t)
          (ps-print-color-p 'black-white)
          )
         ("~/theagenda.pdf")
         )
        ("i" "Idea and hold, maybe, someday tasks and-or projects"
         ((tags "+idea"
                ((org-agenda-overriding-header "Ideas")
                 (org-tags-match-list-sublevels 'indented)
                 (org-agenda-sorting-strategy '(category-keep priority-down))))
          (tags-todo "HOLD|MAYBE"
                     ((org-agenda-overriding-header "HOLD tags")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(category-keep))))))
        ("c" "Context lists"
         ((tags "@office")
          (tags "@home")
          (tags "@dati")
          (tags "@internet")
          (tags "@phone")
          (tags "@email")
          (tags "@errands"))
         ((ps-number-of-columns 2)
          (ps-landscape-mode t)
          (ps-print-color-p 'black-white)
          (htmlize-output-type 'css))
         ("~/context-lists.pdf" "~/context-lists.html"))
        ("d" "Upcoming deadlines" agenda "display deadlines and exclude scheduled"
         ((org-agenda-span 'year)
          (org-agenda-time-grid nil)
          (org-agenda-show-all-dates nil)
          (org-agenda-entry-types '(:deadline)) ;; this entry excludes :scheduled
          (org-deadline-warning-days 365)))

        ("P" "Printed agenda"
         ((agenda "" ((org-agenda-ndays 7)                      ;; overview of appointments
                      (org-agenda-start-on-weekday nil)         ;; calendar begins today
                      (org-agenda-repeating-timestamp-show-all t)
                      (org-agenda-entry-types '(:timestamp :sexp))))
          (agenda "" ((org-agenda-ndays 1)                      ;; daily agenda
                      (org-deadline-warning-days 700)             ;; 7 day advanced warning for deadlines
                      (org-agenda-todo-keyword-format "[ ]")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-prefix-format "%t%s")))
          (todo "TODO"                                          ;; todos sorted by context
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
         ((org-agenda-with-colors nil)
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (htmlize-output-type 'css)
          (ps-number-of-columns 2)
           (ps-landscape-mode t))
         ("~/agenda.pdf" "~/agenda.html"))
        ;; other commands go here
        ))



;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; global Effort estimate values  ;http://doc.norang.ca/org-mode.html
;; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

;; FIXME:
;; function for "planner": sync effort to scheduled start-end interval
(defun org-schedule-effort ()
(interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* (
        (element (org-element-at-point))
        (effort (org-element-property :EFFORT element))
        (scheduled (org-element-property :scheduled element))
        (ts-year-start (org-element-property :year-start scheduled))
        (ts-month-start (org-element-property :month-start scheduled))
        (ts-day-start (org-element-property :day-start scheduled))
        (ts-hour-start (org-element-property :hour-start scheduled))
        (ts-minute-start (org-element-property :minute-start scheduled)) )
      (org-schedule nil (concat
        (format "%s" ts-year-start)
        "-"
        (if (< ts-month-start 10)
          (concat "0" (format "%s" ts-month-start))
          (format "%s" ts-month-start))
        "-"
        (if (< ts-day-start 10)
          (concat "0" (format "%s" ts-day-start))
          (format "%s" ts-day-start))
        " "
        (if (< ts-hour-start 10)
          (concat "0" (format "%s" ts-hour-start))
          (format "%s" ts-hour-start))
        ":"
        (if (< ts-minute-start 10)
          (concat "0" (format "%s" ts-minute-start))
          (format "%s" ts-minute-start))
        "+"
        effort)) )))
