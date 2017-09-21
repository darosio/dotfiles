;; If you have a repeating task in your agenda, say every other day,
;; and you show the agenda for, say, the next 15 days, it is quite
;; annoying to see that task displayed for seven or eight days. You
;; can now say nil or 'next
(setq org-agenda-show-future-repeats 'next)

(setq org-highlight-latex-and-related '(latex))
;; Some general settings
(setq-default org-directory "~/Sync/notes/")
(setq org-default-notes-file "~/Sync/share/phone/box/notes/inbox.org")
(load-library "find-lisp")
(setq org-agenda-files (append '("~/Sync/share/phone/box/notes/gtd.org"
                               "~/Sync/share/phone/box/notes/inbox.org"
                               "~/Sync/share/phone/box/notes/gcal"
                               "~/Sync/notes/proj"
                               "~/Sync/notes/work"
                               "~/Sync/notes/home"
                               ) (find-lisp-find-files "~/Sync/notes/arch" "\.org$")))

(setq org-agenda-include-diary nil)
(setq org-agenda-diary-file "~/Sync/share/phone/box/notes/diary.org")
;; a global-set-key example
(global-set-key (kbd "<f7> <f8>") 'calendar)    ; F7 F8

;; Display properties DEF ?
(setq org-cycle-separator-lines 0)
(setq org-tags-column 80)
(setq org-agenda-tags-column org-tags-column)
(setq org-agenda-sticky t)

;; Set default column view headings: Task Effort Clock_Summary DEF ?
(setq org-columns-default-format "%50ITEM(Task) %7Effort(Effort){:} %7CLOCKSUM %1PRIORITY %TAGS %SCHEDULED %DEADLINE")


;; == Tags ==
(setq org-tag-alist (quote ((:startgroup)
                              ("@errands" . ?e)
                              ("@internet" . ?i)
                              ("@home" . ?h)
                              ("@office" . ?o)
                              ("@dati" . ?d)
                              ("@email" . ?m)
                              ("@phone" . ?t)
                            (:endgroup)
                            ("PERSONAL" . ?p)
                            ("WORK" . ?w)
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

(add-hook 'org-capture-mode-hook 'evil-insert-state)
;; == Captures ==
(defvar org-capture-templates
  '(("t" "todo" entry (file org-default-notes-file)
       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
    ("b" "Blank" entry (file org-default-notes-file)
     "* %?\n%u")
    ("m" "Meeting" entry (file org-default-notes-file)
     "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
    ;; diary.org
    ("a" "Appointment" entry (file  "~/Sync/share/phone/box/notes/gcal/dpa.org" )
     "* %?\n\n%^T\n%a\n:PROPERTIES:\n\n:END:\n\n")
    ;; diary.org
    ("d" "Diary" entry (file+datetree "~/Sync/share/phone/box/notes/diary.org")
     "* %?\n%U\n" :clock-in t :clock-resume t)
    ;; ideas.org
    ("i" "idea" entry (file "~/Sync/share/phone/box/notes/ideas.org")
     "* %? :IDEA: \n%u")
    ;; gtd.org
    ("h" "Habit" entry (file+headline "~/Sync/share/phone/box/notes/gtd.org" "Habits")
     "* TODO %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:REPEAT_TO_STATE: TODO\n:END:\n")
    ("n" "Next Task" entry (file+headline "~/Sync/share/phone/box/notes/gtd.org" "Tasks")
     "** NEXT %? \nDEADLINE: %t")
    ("r" "respond" entry (file+headline "~/Sync/share/phone/box/notes/gtd.org" "Reply")
     "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
    ("w" "waiting reply" entry (file+headline "~/Sync/share/phone/box/notes/gtd.org" "Reply")
     "* WAITING Reply from %:from on %:subject\n %U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
    ;; spesa.org
    ("s" "Spesa" entry (file+headline "~/Sync/share/phone/box/notes/spesa.org" "Supermarket")
     "* TODO %? \n")
    ))
           ;("w" "org-protocol" entry (file "~/git/org/refile.org")
            ;"* TODO Review %c\n%U\n" :immediate-finish t)


;; == Refile ==
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 3)
                                 ("~/Sync/share/phone/box/notes/gtd.org" :maxlevel . 3) ;; ???
                                 ("~/Sync/share/phone/box/notes/ideas.org" :maxlevel . 2)
                                 ("~/Sync/share/phone/box/notes/someday.org" :level . 1)
                                 )))
                                 ;; (org-default-notes-file :maxlevel . 9)
                                 ;; )))
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


;; ;; == Habits ==
(require 'org-habit)

;; (setq org-modules '(org-habit))
;; (setq org-habit-show-habits-only-for-today t)


;; == Clocking Functions ==
(require 'org-clock)
;; If not a project, clocking-in changes TODO to NEXT
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (not (bh/is-project-p)))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c C-.") 'org-time-stamp-inactive)))
;; Also ensure that NEXT projects are switched to TODO when clocking in
(add-hook 'org-clock-in-hook 'gs/mark-next-done-parent-tasks-todo 'append)

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

(defun gs/mark-next-done-parent-tasks-todo ()
  "Visit each parent task and change NEXT (or DONE) states to TODO."
  ;; Don't change the value if new state is "DONE"
  (let ((mystate (or (and (fboundp 'org-state)
                          (member state
                                  (list "NEXT" "TODO")))
                     (member (nth 2 (org-heading-components))
                             (list "NEXT" "TODO")))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT" "DONE"))
            (org-todo "TODO")))))))
(add-hook 'org-after-todo-state-change-hook 'gs/mark-next-done-parent-tasks-todo 'append)


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


;; == Agenda ==
;; TODO http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html

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
    (800 1200 1300 1800)))

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

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
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-deadline-warning-days 70)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(
        ("h" "Habits" agenda "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep)))
         )
        (" " "Export Schedule"
         (
          (agenda "" ((org-agenda-overriding-header "Today's Schedule:")
                      (org-agenda-span 2)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "+0d")
                      (org-agenda-todo-ignore-deadlines nil)))
          (tags-todo "-CANCELLED-ARCHIVE/NEXT"
					   ((org-agenda-overriding-header "Next Tasks:")
					    ))
          (tags "REFILE-ARCHIVE-REFILE=\"nil\""
				      ((org-agenda-overriding-header "Tasks to Refile:")
				       (org-tags-match-list-sublevels nil)))
          (tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE-STYLE=\"habit\"-CATEGORY=\"Habits\"/!-NEXT-WAITING"
					   ((org-agenda-overriding-header "Standalone Tasks:")
					    (org-agenda-skip-function 'gs/select-standalone-tasks)))
          (tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVEr/!"
                     ((org-agenda-overriding-header "Active Projects:")
                      (org-agenda-skip-function 'gs/select-projects)))
          (tags-todo "-CANCELLED-ARCHIVE/!WAITING"
                     ((org-agenda-overriding-header "Waiting-for Tasks:")
                      (org-agenda-skip-function 'gs/select-standalone-tasks)))
          (todo ""
           ((org-agenda-overriding-header "Upcoming deadlines:")
            (org-agenda-todo-ignore-deadlines 'near)
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))))
          (tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE/!-NEXT"
					   ((org-agenda-overriding-header "Remaining Project Tasks:")
					    (org-agenda-skip-function 'gs/select-project-tasks)))
          (tags "HOLD-ARCHIVE"
				      ((org-agenda-overriding-header "Inactive Projects and Tasks")
				       (org-tags-match-list-sublevels nil)))
          (todo ""
           ((org-agenda-overriding-header "Someday/maybe Projects and Tasks")
            (org-agenda-files '("~/Sync/share/phone/box/notes/someday.org"))))
          ;; from ref 1
          (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil)))
        )
         (
          ;; (org-agenda-start-with-log-mode t)
          ;; (org-agenda-log-mode-items 'clock)
          (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
	  		      (timeline . "  % s")
	  		      (todo . "  %-12:c %(gs/org-agenda-prefix-string) ")
	  		      (tags . "  %-12:c %(gs/org-agenda-prefix-string) ")
	  		      (search . "  %i %-12:c")))
          ;; (org-agenda-todo-ignore-deadlines 'near)
          (org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines t)

         (ps-number-of-columns 2)
          (ps-landscape-mode t)
          (ps-print-color-p 'black-white)
          )
         ("~/theagenda.pdf")
         )

        ("c" "Context lists"
         (
          (tags "@office")
          (tags "@home")
          (tags "@dati")
          (tags "@internet")
          (tags "@phone")
          (tags "@email")
          (tags "@errands")
          )
         (
          (ps-number-of-columns 2)
          (ps-landscape-mode t)
          (ps-print-color-p 'black-white)
          (htmlize-output-type 'css)
          )
         ("~/context-lists.pdf" "~/context-lists.html")
         )

        ;;(tags "+DEADLINE={.+}" ))
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

;(push "~/.spacemacs.d/config/" load-path)
(require 'cal-config nil t)

(provide 'org-config)
