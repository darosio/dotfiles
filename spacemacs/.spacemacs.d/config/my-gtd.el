(provide 'my-gtd)

;; a better word processor
(setq org-hide-emphasis-markers t)

;; Define variables
(eval-and-compile
  (defvar agenda-and-refile-files "~/Dropbox/"
    "My shared folder on the Cloud"))
;; I would like to simplify starting from scratch.
;; I'll try following
;; https://orgmode.org/worg/org-configs/org-customization-guide.html

(defun gs/org-agenda-add-location-string ()
  "Gets the value of the LOCATION property"
  (let ((loc (org-entry-get (point) "LOCATION")))
    (if (> (length loc) 0)
        (concat "{" loc "} ")
      "")))

(setq org-stuck-projects
      '("+proj/-DONE-HOLD" ("NEXT") nil ""))

;; Agenda folders and files
(progn
  (setq-default org-directory "~/Sync/share/phone/box/org")
  (load-library "find-lisp")
  (setq org-agenda-files (append '("~/Sync/share/phone/box/org/"
                                   "~/Sync/share/phone/box/org/gcal/"
                                   "~/Sync/notes/proj"
                                   "~/Sync/notes/work"
                                   "~/Sync/notes/home"
                                   "~/Sync/share/phone/box/org/TODOs.org" ;; target for org-projectile
                                   )
                                 ;; traverse the whole tree
                                 (find-lisp-find-files "~/Sync/notes/arch/" "\.org$")))
  (setq org-agenda-diary-file "~/Sync/share/phone/box/notes/diary.org"
        org-agenda-include-diary t)
  ;; ARCHIVE
  (setq org-archive-location "~/Sync/share/phone/box/org/archives/%s_archive::")
  (defvar org-archive-file-header-format "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n")
  )

;; TODOs
(progn
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
  (setq org-enforce-todo-dependencies t)
  ;; for when I set a task as e.g. Canceled open the buffer in insert state
  (add-hook 'org-log-buffer-setup-hook 'evil-insert-state)
  )
;; TAGs
(progn
  (setq org-tag-alist (quote ((:startgroup)
                              ("@errands" . ?e)
                              ("@home" . ?h)
                              ("@fbk" . ?f)
                              (:endgroup)
                              ("@net" . ?n)
                              ("@dati" . ?d)
                              ("@mail" . ?m)
                              ("@telephone" . ?t)
                              ("PERSONAL" . ?p)
                              ("WORK" . ?w)
                              ("idea" . ?i)
                              ("proj" . ?j)
                              )))
  ;; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key 'expert)
  ;; Include the todo keywords
  (setq org-fast-tag-selection-include-todo nil)
  )
;; Captures
(progn
  (setq org-default-notes-file "~/Sync/share/phone/box/notes/inbox.org")
  (define-key global-map "\C-ct"
    (lambda () (interactive) (org-capture nil "t")))
  (defvar org-capture-templates
    '(("t" "todo" entry (file org-default-notes-file)
       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
      ("b" "Brain" plain (function org-brain-goto-end) "* %i%?" :empty-lines 1)
      ("m" "Meeting" entry (file org-default-notes-file)
       "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
      ("a" "Appointment" entry (file  "~/Sync/share/phone/box/notes/gcal/dpa.org" )
       "* %?\n\n%^T\n%a\n:PROPERTIES:\n\n:END:\n\n")
      ;; diary.org
      ("d" "Diary" entry (file+olp+datetree "~/Sync/share/phone/box/notes/diary.org")
       "* %?\n%t\n" )
      ;; diary.org
      ("D" "Diary prompting date" entry (file+olp+datetree+prompt org-default-notes-file)
       "* %?\n%t\n" )
      ("e" "Empty" entry (file org-default-notes-file) "* %?\n%u")
      ;; ideas.org
      ("i" "idea" entry (file "~/Sync/share/phone/box/notes/ideas.org")
       "* %? :IDEA: \n%u")
      ;; gtd.org
      ("h" "Habit" entry (file+headline "~/Sync/share/phone/box/notes/gtd.org" "Habits")
       "* TODO %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:REPEAT_TO_STATE: TODO\n:END:\n")
      ("n" "Next Task" entry (file+headline "~/Sync/share/phone/box/notes/gtd.org" "Tasks")
       "** NEXT %? \nDEADLINE: %t")
      ("r" "respond" entry (file+headline "~/Sync/share/phone/box/notes/gtd.org" "Reply")
       "* TODO %a to %:from \nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n %?")
      ;; "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n")
      ("w" "waiting reply" entry (file+headline "~/Sync/share/phone/box/notes/gtd.org" "Reply")
       "* WAITING %a from %:from" :immediate-finish t)
      ;; spesa.org
      ("s" "Spesa" entry (file+headline "~/Sync/share/phone/box/notes/spesa.org" "Supermarket")
       "* TODO %? \n")
      ))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  )

;; REFILE
(progn
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 5))))
  ;; (org-refile-targets `(((,org-default-notes-file "~/Dropbox/orgfiles/someday.org" "~/Dropbox/orgfiles/todo.org") :maxlevel . 3)))
  ;; Be sure to use the full path preceded by filename to insert at top level
  (setq org-refile-use-outline-path 'file)
  ;; Targets complete directly with helm
  (setq org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; ;; Exclude DONE state tasks from refile targets @2MAYBE
  ;; (defun bh/verify-refile-target ()
  ;;   "Exclude todo keywords with a done state from refile targets"
  ;;   (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  ;; (setq org-refile-target-verify-function 'bh/verify-refile-target)
  )


(require 'org-checklist)
;; If you have a repeating task in your agenda, say every other day, and you
;; show the agenda for, say, the next 15 days, it is quite annoying to see
;; that task displayed for seven or eight days. You can now say nil or 'next
(setq org-agenda-show-future-repeats 'next)

;; Display properties
;; (setq org-startup-folded "content")
(setq org-cycle-separator-lines 2) ;; default
(setq org-tags-column -82)
;; (setq org-agenda-tags-column org-tags-column)
(setq org-agenda-tags-column -82)
;; (setq org-agenda-sticky t)
(setq org-columns-default-format
      "%48ITEM(Task) %4TODO(todo) %6CLOCKSUM{:} %ALLTAGS %SCHEDULED %6Effort(Effort){:} %DEADLINE")

;; global Effort estimate values  ;http://doc.norang.ca/org-mode.html
;; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Enable habit tracking (and a bunch of other modules)
;; (setq org-modules (quote (org-habit)))
;; (setq org-habit-graph-column 60)
;; (setq org-modules (quote (org-bbdb
;;                           org-bibtex
;;                           org-crypt
;;                           org-gnus
;;                           org-id
;;                           org-info
;;                           org-jsinfo
;;                           org-habit
;;                           org-inlinetask
;;                           org-irc
;;                           org-mew
;;                           org-mhe
;;                           org-protocol
;;                           org-rmail
;;                           org-vm
;;                           org-wl
;;                           org-w3m)))


;; == Agenda ==
;; TODO http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
;; Dim blocked tasks (and other settings)
(setq org-agenda-inhibit-startup nil)
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
;; Compact the block agenda view (disabled)
(setq org-agenda-compact-blocks nil)

;; ;; Set the times to display in the time grid
;; (setq org-agenda-time-grid
;;   '((daily today require-timed)
;;     "----------------"
;;     (800 1200 1300 1800)))

;; ;; for lazy boolean search =C-a a s=
;; (setq org-agenda-search-view-always-boolean t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))
;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-deadline-warning-days 70)
;; (org-use-property-inheritance t) @2DO to be used with STYLE, but prefer to ignore scheduled
;; all properties are inherited
(setq org-use-property-inheritance t) ;; @2DO to be used with STYLE, e.g. habit not scheduled

(setq org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
                            (timeline . "  % s")
                            (todo . "  %-12:c  ")
                            (tags . "  %-12:c  ")
                            (search . "  %i %-12:c")))

(setq org-agenda-custom-commands
      '(
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
          (tags "-proj-NOTE-REFILE-ARCHIVE/DONE|CANCELLED"
                ;; (tags "-proj-NOTE-ARCHIVE+TODO=\"DONE\"\|+TODO=\"CANCELLED\""
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


;; (require 'org-agenda)
;; (define-key org-mode-map (kbd "<SPC> o s") #'org-schedule-effort)
;; ;; (define-key org-agenda-mode-map (kbd "o s") #'org-schedule-effort)
;; (add-hook 'org-agenda-mode-hook (lambda ()
;;                                   (local-set-key "s" 'org-schedule-effort)))
