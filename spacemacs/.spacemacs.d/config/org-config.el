;; Some general settings
(setq-default org-directory "~/Sync/notes/")
(setq org-default-notes-file "~/Sync/share/phone/box/notes/refile.org")
;; (setq org-agenda-include-diary t)
;; (setq org-agenda-include-all-todo t) ;;only in http://sachachua.com/blog/tag/gtd/#post-4543
(setq org-agenda-files (quote ("~/Sync/share/phone/box/notes/todo.org"
                               "~/Sync/share/phone/box/notes/refile.org"
                               "~/Sync/notes/home"
                               "~/Sync/notes/gcal"
                               "~/Sync/notes/arch")))

;; a global-set-key example
(global-set-key (kbd "<f7> <f8>") 'calendar)    ; F7 F8

;; Display properties DEF ?
(setq org-cycle-separator-lines 0)
(setq org-tags-column 80)
(setq org-agenda-tags-column org-tags-column)
(setq org-agenda-sticky t)

;; Set default column view headings: Task Effort Clock_Summary DEF ?
(setq org-columns-default-format "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %16TIMESTAMP_IA")


;; == Tags ==
(setq org-tag-alist (quote ((:startgroup)
                              ("@errand" . ?e)
                              ("@office" . ?o)
                              ("@home" . ?h)
                            (:endgroup)
                            ("PERSONAL" . ?p)
                            ("WORK" . ?w)
                            ("NOTE" . ?n)
                            ("IDEA" . ?i)
                            ("FLAGGED" . ??)
                            (:startgroup)
                              ("NEXT" . ?N)
                              ("WAITING" . ?W)
                              ("HOLD" . ?H)
                              ("CANCELLED" . ?C)
                            (:endgroup) )))
;; Allow setting single tags without the menu
;; (setq org-fast-tag-selection-single-key 'expert)
;; Include the todo keywords
;; (setq org-fast-tag-selection-include-todo t)
;; For tag searches ignore tasks with scheduled and deadline dates
;; (setq org-agenda-tags-todo-honor-ignore-options t)


;; == STATES ==
;; todo keywords -   TYP_TODO | SEQ_TODO
;; (setq org-use-fast-todo-selection t) ;; DEF
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "APPT(a)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")) )
        ;; (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
;; Custom colors for the keywords
(setq org-todo-keyword-faces
            '(("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "light blue" :weight bold)
              ("APPT" :foreground "yellow" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ;; ("PHONE" :foreground "forest green" :weight bold)
              ))
;; Auto-update tags whenever the state ref""is changed
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t))
        ("HOLD" ("WAITING") ("HOLD" . t))
        (done ("WAITING") ("HOLD"))
        ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
        ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
        ("DONE" ("WAITING") ("CANCELLED") ("HOLD")) ))
;; S-left S-right skipping setting timespamps  DEF
;; (setq org-treat-S-cursor-todo-selection-as-state-change nil)


;; == Captures ==
(defvar org-capture-templates
  ;; '(("t" "todo" entry (file org-default-notes-file)
  '(("t" "todo" entry (file org-default-notes-file)
       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
    ("a" "Appointment" entry (file  "~/Sync/notes/gcal/dpa.org" )
     "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
    ("b" "Blank" entry (file org-default-notes-file)
     "* %?\n%u")
    ("d" "Diary" entry (file+datetree "~/Sync/share/phone/box/notes/diary.org")
     "* %?\n%U\n" :clock-in t :clock-resume t)
    ("i" "idea" entry (file+headline org-default-notes-file "Ideas")
     "* IDEA %? :IDEA: \n%u" :clock-in t :clock-resume t)
    ("m" "Meeting" entry (file org-default-notes-file)
     "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
    ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
     "** NEXT %? \nDEADLINE: %t")
    ("r" "respond" entry (file org-default-notes-file)
     "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
    ))
           ;("n" "note" entry (file "~/git/org/refile.org")
            ;"* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
           ;("j" "Journal" entry (file+datetree "~/git/org/diary.org")
            ;"* %?\n%U\n" :clock-in t :clock-resume t)
           ;("w" "org-protocol" entry (file "~/git/org/refile.org")
            ;"* TODO Review %c\n%U\n" :immediate-finish t)
           ;("h" "Habit" entry (file "~/git/org/refile.org")
            ;"* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))


;; == Refile ==
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 ;; (org-agenda-files :maxlevel . 9))))
                                 ("~/Sync/share/phone/box/notes/todo.org" :maxlevel . 9)
                                 ("~/Sync/notes/gcal/dpa.org" :maxlevel . 1)
                                 (org-default-notes-file :maxlevel . 9)
                                 )))
;;  Be sure to use the full path for refile setup
(setq org-refile-use-outline-path t)
;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)
;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)
;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)


;; == Archive ==
(setq org-archive-location "~/Sync/notes/archive/%s_archive::")
(defvar org-archive-file-header-format "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n")

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)

              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (today (format-time-string "%Y-%m-%d" (current-time)))
                  ;; (a-month-ago (* 60 60 24 (+ daynr 1))) ; From REF1
                  ;; (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                  ;; (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat today) subtree-end t)))))
                                             ;; (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Calendars
   ;; gcalendar
   (require 'org-gcal)
   (setq org-gcal-client-id "100447390762-1unqkpjv30do2uq0r5uetd8pkr9ha81s.apps.googleusercontent.com"
         org-gcal-client-secret "EhuKxnNmAoM2XMPjCFOpiLDK"
         org-gcal-file-alist '(("danielepietroarosio@gmail.com" .  "~/Sync/notes/gcal/dpa.org")
                               ("c87gevr5pc3191on8c7nh8b4nc@group.calendar.google.com" .
                                "~/Sync/notes/gcal/figli.org")
           ;                    ("cfaned8dou8gm2qciies0itso4@group.calendar.google.com" .
            ;                    "~/Sync/notes/gcal/deadlines.org")
             ;                  ("tq1af7efj4l9h8glgqi2g5vmsg@group.calendar.google.com" .
              ;                  "~/Sync/notes/gcal/IBF.org")
                               ;; ("i_217.77.81.46#sunrise@group.v.calendar.google.com" .
                               ;;  "~/Sync/notes/gcal/sunrise.org")
                               ;; ("it.italian#holiday@group.v.calendar.google.com" .
                               ;;  "~/Sync/notes/gcal/feste.org")
                               ))
   ;; syncs whenever I load the agenda. Since this happens in the background,
   ;; if I just added something to my calendar, I might have to reload the agenda by hitting r in the agenda view.
   ;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
   ;; syncs with my Google calendar when I capture.
   ;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))

   ;; Sunrise and sunset
   (setq calendar-latitude 46.067270) ; Borino
   (setq calendar-longitude 11.166153)
   (setq calendar-location-name "Trento")
   (setq calendar-time-zone 60)
   ;; Holidays (setq holiday-general-holidays '(
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
           (holiday-fixed 11 1 "Ognissanti")
           ))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; == Agenda ==

;; Dim blocked tasks (and other settings)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-inhibit-startup nil)
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view (disabled)
(setq org-agenda-compact-blocks nil)

;; Set the times to display in the time grid
(setq org-agenda-time-grid
  '((daily today require-timed)
    "----------------"
    (800 1200 1600 2000)))

;; Some helper functions for selection within agenda views
(defun gs/select-with-tag-function (select-fun-p)
  (save-restriction
    (widen)
    (let ((next-headline
	   (save-excursion (or (outline-next-heading)
			       (point-max)))))
      (if (funcall select-fun-p) nil next-headline))))
  
(defun gs/select-projects ()
  "Selects tasks which are project headers"
  (gs/select-with-tag-function #'bh/is-project-p))
(defun gs/select-project-tasks ()
  "Skips tags which belong to projects (and is not a project itself)"
  (gs/select-with-tag-function
   #'(lambda () (and
		 (not (bh/is-project-p))
		 (bh/is-project-subtree-p)))))
(defun gs/select-standalone-tasks ()
  "Skips tags which belong to projects. Is neither a project, nor does it blong to a project"
  (gs/select-with-tag-function
   #'(lambda () (and
		 (not (bh/is-project-p))
		 (not (bh/is-project-subtree-p))))))
(defun gs/select-projects-and-standalone-tasks ()
  "Skips tags which are not projects"
  (gs/select-with-tag-function
   #'(lambda () (or
		 (bh/is-project-p)
		 (bh/is-project-subtree-p)))))

(defun gs/org-agenda-project-warning ()
  "Is a project stuck or waiting. If the project is not stuck,
show nothing. However, if it is stuck and waiting on something,
show this warning instead."
  (if (gs/org-agenda-project-is-stuck)
    (if (gs/org-agenda-project-is-waiting) " !W" " !S") ""))

(defun gs/org-agenda-project-is-stuck ()
  "Is a project stuck"
  (if (bh/is-project-p) ; first, check that it's a project
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
	     (has-next))
	(save-excursion
	  (forward-line 1)
	  (while (and (not has-next)
		      (< (point) subtree-end)
		      (re-search-forward "^\\*+ NEXT " subtree-end t))
	    (unless (member "WAITING" (org-get-tags-at))
	      (setq has-next t))))
	(if has-next nil t)) ; signify that this project is stuck
    nil)) ; if it's not a project, return an empty string

(defun gs/org-agenda-project-is-waiting ()
  "Is a project stuck"
  (if (bh/is-project-p) ; first, check that it's a project
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
	(save-excursion
	  (re-search-forward "^\\*+ WAITING" subtree-end t)))
    nil)) ; if it's not a project, return an empty string

;; Some helper functions for agenda views
(defun gs/org-agenda-prefix-string ()
  "Format"
  (let ((path (org-format-outline-path (org-get-outline-path))) ; "breadcrumb" path
	(stuck (gs/org-agenda-project-warning))) ; warning for stuck projects
       (if (> (length path) 0)
	   (concat stuck ; add stuck warning
		   " [" path "]") ; add "breadcrumb"
	 stuck)))
       
(defun gs/org-agenda-add-location-string ()
  "Gets the value of the LOCATION property"
  (let ((loc (org-entry-get (point) "LOCATION")))
    (if (> (length loc) 0)
	(concat "{" loc "} ")
      "")))

;; Variables for ignoring tasks with deadlines
(defvar gs/hide-deadline-next-tasks t)
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-deadline-warning-days 10)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("h" "Habits" agenda "STYLE=\"habit\""
	 ((org-agenda-overriding-header "Habits")
	  (org-agenda-sorting-strategy
	   '(todo-state-down effort-up category-keep))))
	(" " "Export Schedule" ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
					    (org-agenda-ndays 1)
					    (org-agenda-start-on-weekday nil)
					    (org-agenda-start-day "+0d")
					    (org-agenda-todo-ignore-deadlines nil)))
				(tags-todo "-CANCELLED-ARCHIVE/!NEXT"
					   ((org-agenda-overriding-header "Next Tasks:")
					    ))
				(tags "REFILE-ARCHIVE-REFILE=\"nil\""
				      ((org-agenda-overriding-header "Tasks to Refile:")
				       (org-tags-match-list-sublevels nil)))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVEr/!"
					   ((org-agenda-overriding-header "Active Projects:")
					    (org-agenda-skip-function 'gs/select-projects)))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE-STYLE=\"habit\"/!-NEXT"
					   ((org-agenda-overriding-header "Standalone Tasks:")
					    (org-agenda-skip-function 'gs/select-standalone-tasks)))
				(agenda "" ((org-agenda-overriding-header "Week At A Glance:")
					    (org-agenda-ndays 5)
					    (org-agenda-start-day"+1d")
					    (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %s [%b] ")))))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE/!-NEXT"
					   ((org-agenda-overriding-header "Remaining Project Tasks:")
					    (org-agenda-skip-function 'gs/select-project-tasks)))
				(tags "INACTIVE-ARCHIVE"
				      ((org-agenda-overriding-header "Inactive Projects and Tasks")
				       (org-tags-match-list-sublevels nil)))
;; from ref 1
         ;; (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
         ;;                   ((org-agenda-overriding-header (concat "Standalone Tasks"
         ;;                                                          (if bh/hide-scheduled-and-waiting-next-tasks
         ;;                                                              ""
         ;;                                                            " (including WAITING and SCHEDULED tasks)")))
         ;;                    (org-agenda-skip-function 'bh/skip-project-tasks)
         ;;                    (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
         ;;                    (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
         ;;                    (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
         ;;                    (org-agenda-sorting-strategy
         ;;                     '(category-keep))))
         ;;        (tags-todo "-CANCELLED+WAITING|HOLD/!"
         ;;                   ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
         ;;                                                          (if bh/hide-scheduled-and-waiting-next-tasks
         ;;                                                              ""
         ;;                                                            " (including WAITING and SCHEDULED tasks)")))
         ;;                    (org-agenda-skip-function 'bh/skip-non-tasks)
         ;;                    (org-tags-match-list-sublevels nil)
         ;;                    (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
         ;;                    (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil)))

				(tags "ENDOFAGENDA"
				      ((org-agenda-overriding-header "End of Agenda")
				       (org-tags-match-list-sublevels nil))))
	 ((org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(clock))
	  (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
				      (timeline . "  % s")
				      (todo . "  %-12:c %(gs/org-agenda-prefix-string) ")
				      (tags . "  %-12:c %(gs/org-agenda-prefix-string) ")
				      (search . "  %i %-12:c")))
	  (org-agenda-todo-ignore-deadlines 'near)
	  (org-agenda-todo-ignore-scheduled t)))
	("X" "Agenda" ((agenda "") (alltodo))
	 ((org-agenda-ndays 10)
	  (org-agenda-start-on-weekday nil)
	  (org-agenda-start-day "-1d")
	  (org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(closed clock state))))

   ("H" "Office and Home Lists"
    ((agenda)
     (tags-todo "@office")
     (tags-todo "@home")
     (tags-todo "@pc")
     (tags-todo "WORK")
     (tags-todo "PERSONAL")
     (tags "IDEA")
     (tags "emacs")))
	 ))

;; == Agenda Navigation ==

;; Search for a "=" and go to the next line
(defun gs/org-agenda-next-section ()
  "Go to the next section in an org agenda buffer"
  (interactive)
  (if (search-forward "===" nil t 1)
      (forward-line 1)
    (goto-char (point-max)))
  (beginning-of-line))

;; Search for a "=" and go to the previous line
(defun gs/org-agenda-prev-section ()
  "Go to the next section in an org agenda buffer"
  (interactive)
  (forward-line -2)
  (if (search-forward "===" nil t -1)
      (forward-line 1)
    (goto-char (point-min))))

;; == Agenda Post-processing ==
;; Highlight the "!!" for stuck projects (for emphasis)
(defun gs/org-agenda-project-highlight-warning ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "!W" nil t)
      (progn
	(add-face-text-property
	 (match-beginning 0) (match-end 0)
	 '(bold :foreground "orange"))
	))
    (goto-char (point-min))
    (while (re-search-forward "!S" nil t)
      (progn
	(add-face-text-property
	 (match-beginning 0) (match-end 0)
	 '(bold :foreground "white" :background "red"))
	))
    ))
(add-hook 'org-finalize-agenda-hook 'gs/org-agenda-project-highlight-warning)

;; Remove empty agenda blocks
(defun gs/remove-agenda-regions ()
  (save-excursion
    (goto-char (point-min))
    (let ((region-large t))
      (while (and (< (point) (point-max)) region-large)
	(set-mark (point))
	(gs/org-agenda-next-section)
	(if (< (- (region-end) (region-beginning)) 5) (setq region-large nil)
	  (if (< (count-lines (region-beginning) (region-end)) 4)
	      (delete-region (region-beginning) (region-end)))
	  )))))
(add-hook 'org-finalize-agenda-hook 'gs/remove-agenda-regions)
;; == bh/helper-functions ==
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))
(defun bh/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))
(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil

t))))
(provide 'org-config)
