;;; my-org.el --- Org config -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Binding keys: "C-c"
;;; Code:

(use-package org
  :straight (:type built-in)
  :ensure nil
  :commands (org-capture-finalize org-speed-move-safe org-narrow-to-subtree org-clock-in)
  :functions (org-read-date org-get-tags org-entry-delete org-entry-put org-toggle-tag)
  :defines org-state
  :preface
  (defun da-consult-org ()
    "Custom Agenda Search with Consult"
    (interactive)
    (let ((org-agenda-files (append da-agenda-files da-refile-files)))
      (consult-org-agenda)))

  (defun dpa-proj-state-change-hook ()
    "Automatically update project-related tags when Org entry states change.
  For entries tagged 'proj':
  - Sets WAITING/PASSED/HOLDING/MAYBE tags for WAIT/PASS/HOLD/MAYB states respectively
  - Removes all project tags for other states (except ARCHIVE/ENDED)
  This hook function runs after each state change to maintain tag/state consistency."
    (let ((tags '(("WAIT"  . "WAITING")
                  ("PASS"  . "PASSED")
                  ("HOLD"  . "HOLDING")
                  ("MAYB"  . "MAYBE"))))
      (if (member "proj" (org-get-tags nil t))
          (let ((tag-on
                 (cdr (assoc org-state tags))))
            ;; Turn on the one tag, turn off the others.
            (dolist (tag tags)
              (org-toggle-tag (cdr tag) (if (and tag-on (equal (cdr tag) tag-on)) 'on 'off))))
        ;; Remove project dedicated tags at the first state change.
        (dolist (tag tags)
          (org-toggle-tag (cdr tag) 'off)))))

  ;; Utility Functions
  (defun internet-up-p (&optional host)
    "Check internet connectivity. Default HOST is google.com."
    (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                       (or host "www.google.com"))))

  (defun fetch-calendar ()
    "Fetch Google Calendar entries."
    (interactive)
    (when (internet-up-p) (org-gcal-fetch)))

  :hook
  ((org-mode . visual-line-mode)
   (org-mode . flyspell-mode)
   (org-mode . variable-pitch-mode)
   (org-mode . prettify-symbols-mode)
   (org-after-todo-state-change . dpa-proj-state-change-hook))

  :bind
  (("C-c a" . org-agenda)
   ("M-s M-a" . da-consult-org)
   ("M-S-h" . org-mark-element)
   :map
   org-mode-map
   ("C-'" . nil)
   ("C-\"" .  org-cycle-agenda-files)
   ("H-<return>" . org-next-link)
   ("H-S-<return>" . org-previous-link)
   ("M-<tab>" . org-cycle-global)
   ("<C-S-left>" . nil)
   ("<C-S-right>" . nil)
   ("M-g ; ;" . org-capture-goto-last-stored) ; `C-x r b` for bookmarks
   ("C-c t o i" . org-indent-mode)
   ("C-c l" . org-store-link)
   ("C-c L" . org-toggle-link-display)
   ("C-c C-L" . org-insert-link)
   ("C-c C-l" . org-insert-last-stored-link))

  :config
  ;; Core Org Settings
  (setq org-adapt-indentation nil
        org-startup-indented t
        org-return-follows-link t
        org-refile-use-cache t
        org-pretty-entities t
        org-confirm-babel-evaluate nil ; don't prompt to confirm evaluation every time
        org-image-actual-width 0.67    ; 67% of text width
        org-hide-emphasis-markers t
        org-highlight-latex-and-related '(latex) ; Change color of inline latex $y=mx+c$
        org-src-fontify-natively t
        org-columns-default-format
        "%48ITEM(Task) %TODO(todo) %ALLTAGS %SCHEDULED %6Effort(Effort){:} %6CLOCKSUM{:} %DEADLINE"
        org-M-RET-may-split-line
        '((default . t) (headline . nil) (item . nil) (table . nil))
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-track-ordered-property-with-tag t
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-loop-over-headlines-in-active-region nil
        org-fontify-quote-and-verse-blocks t
        )
  ;; Org Attach Settings
  (setq org-attach-use-inheritance t)
  ;; Org Indent Settings
  (setq org-indent-indentation-per-level 2)
  ;; Org Link Settings
  (setq org-link-keep-stored-after-insertion t)
  (org-link-set-parameters "mpv" :follow (lambda (path) (browse-url-xdg-open path)))

  (consult-customize da-consult-org :preview-key "<right>")
  ;; babel
  (setq ;;org-src-preserve-indentation t           ; indentation in src blocks
   ;;org-edit-src-content-indentation 2         ; default
   org-src-tab-acts-natively t)                ; tab in src blocks
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t)
                                 (emacs-lisp . t)
                                 (awk . t)
                                 (python . t)
                                 (gnuplot . t)
                                 (latex . t)
                                 (R . t)
                                 (ditaa . t)
                                 (makefile . t)
                                 (dot . t)
                                 (shell . t)
                                 (perl . t)
                                 (jupyter . t) ;should be the last one
                                 ))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (add-to-list 'org-structure-template-alist '("sl" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sb" . "src sh :results output :exports both"))
  (add-to-list 'org-structure-template-alist '("sB" . "src sh :session bash :results output :exports both"))
  (add-to-list 'org-structure-template-alist '("sj" . "src jupyter-python"))
  ;; org-babel-default-header-args:sh    '((:results . "output replace"))
  ;; org-babel-default-header-args:bash  '((:results . "output replace"))
  ;; org-babel-default-header-args:shell '((:results . "output replace"))

  ;; -- GTD core file paths --
  (defvar da-org-dir (expand-file-name "~/Sync/box/org"))
  (defvar da-gtd (expand-file-name "gtd.org" da-org-dir))
  (defvar da-reviews-dir (expand-file-name "reviews" da-org-dir))

  (let* ((agenda-paths '("gtd.org"
                         "gcal/dpa.org"
                         "gcal/figli.org"
                         "ideas.org"
                         "inbox.org"
                         "inbox.box.org"
                         "journal.org"
                         "projects.org"))
         (da-agenda-files (mapcar (lambda (f) (expand-file-name f da-org-dir))
                                  agenda-paths))
         (proj-dir (expand-file-name "~/Sync/proj/")))
    (setq-default da-agenda-files da-agenda-files)
    (condition-case _
        (setq-default da-refile-files
                      (append (directory-files proj-dir t "\\.org$")
                              (directory-files "~/Sync/notes/home/" t "\\.org$")
                              (directory-files-recursively "~/Sync/notes/arch/" "\\.org$")
                              (directory-files-recursively "~/Sync/notes/org-roam/" "\\.org$")))
      (file-missing
       (message "The directory %s does not exist or is not accessible." proj-dir))))

  (setq org-directory da-org-dir
        org-default-notes-file (expand-file-name "inbox.org" da-org-dir)
        org-agenda-files da-agenda-files                              ;; (1) Agenda files
        org-archive-location "~/Sync/notes/org-archives/%s_archive::" ;; (2) Archives
        org-agenda-text-search-extra-files `(agenda-archives)         ;; Search also in archives
        org-use-property-inheritance nil                              ;; default
        )

  (use-package org-archive :straight org
    :config
    (setq org-archive-file-header-format "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n"))

  ;; (3) Refile
  (use-package org-refile :straight org
    :bind
    (:map org-mode-map ("M-g ; :" . org-refile-goto-last-stored))
    :config
    (setq org-refile-use-outline-path 'file) ; Full path preceded by filename
    (setq org-outline-path-complete-in-steps nil) ; Complete directly with consult
    (setq org-refile-allow-creating-parent-nodes 'confirm) ; Ask confirmation when creating parent tasks
    (setq org-refile-targets
          '(
            ("~/Sync/box/org/shopping.org" :maxlevel . 3)
            (da-refile-files :maxlevel . 5)
            (da-agenda-files :maxlevel . 5)))
    (advice-add 'org-refile :after 'org-save-all-org-buffers)
    )

  ;; (4) Stuck project
  ;;  org-stuck-projects '("+proj/-DONE-HOLD-MAYB-PASS-WAIT" ("NEXT") nil ""))

  ;; (5) Todo states
  (setq org-todo-keywords
        ;; tracking state changes @: note !:date entering/leaving
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAIT(w@/!)" "PASS(p!/!)" "HOLD(h@/@)" "MAYB(m/@)" "|" "CANC(c@/@)")))
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil) ; no log here
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline nil)
  (setq org-log-reschedule nil)
  (setq org-read-date-prefer-future 'time)

  ;; (6) Tags for contexts
  (setq org-tag-alist nil)                  ; default
  (setq org-tag-persistent-alist
        '((:startgroup . nil) ; mutually exclusive
          ("@errand" . ?e) ("@fbk" . ?f) ("@home" . ?h)
          (:endgroup . nil)
          ("@dati" . ?d) ("@telephone" . ?t)
          ("PERSONAL" . ?p) ("WORK" . ?w)
          ("idea" . ?i) ("proj" . ?j) ("buy" . ?b) ("study" . ?s)))
  (setq org-fast-tag-selection-single-key t) ; 'expert doesn't show
  (setq org-fast-tag-selection-include-todo nil)
  (setq org-tags-column -82)
  (setq org-support-shift-select t)       ;do not change state with left right arrow

  ;; (use-package org-clock :straight org
  ;;   :config
  ;;   (setq org-clock-out-remove-zero-time-clocks t) ; Removes clocked tasks with 0:00 duration
  ;;   (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))   ; Enable auto clock resolution for finding open clocks
  ;;   )
  ;; (org-clock-persistence-insinuate)       ; Resume clocking task when emacs is restarted
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit"))))
  )

(use-package org-capture :straight org
  :after org
  :preface
  (defun my-daily-review ()
    "Capture and review for daily tasks."
    (interactive)
    (org-capture nil "rd")
    (common-review-actions))

  (defun my-weekly-review ()
    "Capture and review for weekly tasks."
    (interactive)
    (org-capture nil "rw")
    (common-review-actions))

  (defun common-review-actions ()
    "Common actions for daily and weekly reviews."
    (org-capture-finalize t)
    (org-speed-move-safe 'outline-up-heading)
    (org-narrow-to-subtree)
    (fetch-calendar)
    ;; (org-clock-in)
    (save-buffer)  ;; Save review immediately after creation
    )

  (defun clean-shopping-list ()
    (interactive)
    (with-current-buffer (find-file-noselect "~/Sync/box/org/shopping.org")
      (org-map-entries (lambda () (org-cut-subtree)) "/+DONE" 'file)
      (org-map-entries (lambda () (org-cut-subtree)) "/+CANCELLED" 'file)
      (save-buffer)))

  :hook
  (org-after-refile-insert . save-buffer)

  :config
  (setq org-capture-templates
        `(
          ("t" "Todo simple entry"
           entry (file org-default-notes-file)
           "* TODO %?\n%[~/.emacs.d/templates/da-property-string]\n")

          ("T" "Tasks in gtd"
           entry (file+headline da-gtd "Tasks")
           "* %^{State|TODO|NEXT|WAIT|PASS|MAYB} %? \t%^{Tag|:WORK:|:PERSONAL:}\n%[~/.emacs.d/templates/da-property-string]\n" :empty-lines 1)

          ("n" "Next urgent task"
           entry (file+headline da-gtd "Tasks")
           "* NEXT [#A] %? \t%^{Tag|:WORK:|:PERSONAL:}\nDEADLINE: %t\n%[~/.emacs.d/templates/da-property-string]\n")

          ("P" "new Project"
           entry (file "~/Sync/box/org/projects.org")
           "* %? \t%^{Tag|:WORK:proj:|:PERSONAL:proj:}\n%[~/.emacs.d/templates/da-property-string]\n%^{CATEGORY}p"
           :empty-lines 1
           :prepend t
           :jump-to-captured t)  ;; Add this to jump after capture


          ("s" "Study item"
           entry (file+headline da-gtd "Study")
           "* TODO %?\n%[~/.emacs.d/templates/da-property-string]\n")

          ("w" "Weight"
           table-line (file+headline da-gtd "Weight")
           "|%t|%?|")

          ("h" "new Habit"
           entry (file+headline da-gtd "Habits")
           "* TODO %? \nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:REPEAT_TO_STATE: TODO\n:END:\n%a")

          ("i" "Idea"
           entry (file "~/Sync/box/org/ideas.org")
           "* %^{Idea} \n%u\n%a\n%i\n%?" :empty-lines 1)

          ;; do I need :cal: it could be used in the view to archive refile
          ;; prefix C-1 alternative to time-prompt t
          ("e" "Event in journal"
           entry (file+olp+datetree "~/Sync/box/org/journal.org")
           "* %? %:subject\t:cal:\n%^T\n%a\n%i\n" :jump-to-captured t :time-prompt t)

          ("j" "Journal"
           entry (file+olp+datetree "~/Sync/box/org/journal.org")
           "* %? %:subject %^G\n%T\n%a\n%i\n" :jump-to-captured t :time-prompt t)

          ("k" "supermarKet"
           entry (file+headline "~/Sync/box/org/shopping.org" "Supermarket")
           "* %? \t:SMT:\n" :unnarrowed t :kill-buffer t)

          ;; "Review" use `C-c R`
          ("rd" "Review: Daily"
           entry (file+datetree ,(expand-file-name "daily.org" da-reviews-dir))
           "%[~/.emacs.d/templates/my_dailyreviewtemplate.org]"
           :empty-lines 1
           :jump-to-captured t)
          ("rw" "Review: Weekly"
           entry (file+datetree ,(expand-file-name "weekly.org" da-reviews-dir))
           "%[~/.emacs.d/templates/my_weeklyreviewtemplate.org]"
           :tree-type week
           :empty-lines 1
           :jump-to-captured t)

          ;; Only in mu4e
          ("R" "Reply to"
           entry (file+headline da-gtd "E-mail")
           "* TODO Reply \"%:subject\"\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n%[~/.emacs.d/templates/da-property-string-email]%i%?\n")

          ("W" "Wait for Reply"
           entry (file+headline da-gtd "E-mail")
           "* WAIT for reply \"%:subject\"\n%[~/.emacs.d/templates/da-property-string-email]%i%?\n")
          ))
  (setq org-capture-templates-contexts '(("R" ((in-mode . "mu4e-view-mode")))
                                         ("W" ((in-mode . "mu4e-view-mode")))
                                         ("R" ((in-mode . "mu4e-headers-mode")))
                                         ("W" ((in-mode . "mu4e-headers-mode")))))
  :bind
  (("C-c c" . org-capture)
   ("C-c T" . (lambda () (interactive "") (org-capture nil "T")))
   ("C-c R d" . my-daily-review)
   ("C-c R w" . my-weekly-review))
  )

(use-package org-agenda :straight org
  :after org
  :bind
  (("M-s A" . (lambda () (interactive "") (org-agenda nil "s")))
   :map org-agenda-mode-map
   ("C-a" . org-agenda))
  :hook
  (org-agenda-mode . (lambda () (hl-line-mode) (setq line-spacing 0.0)))
  :config
  (setq org-habit-show-habits-only-for-today nil)
  (setq org-habit-graph-column 60)
  (setq org-agenda-diary-file "~/Sync/box/org/journal.org")
  (setq org-agenda-include-diary t)
  (setq org-agenda-confirm-kill 1)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-skip-additional-timestamps-same-entry t)
  ;; (setq org-agenda-skip-deadline-prewarning-if-scheduled nil)
  ;; (setq org-agenda-skip-scheduled-delay-if-deadline t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  ;; (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  ;; (setq org-agenda-skip-timestamp-if-done t)
  (setq org-agenda-search-headline-for-time nil)
  (setq org-agenda-start-on-weekday nil)  ; 1=Monday
  (setq org-agenda-start-with-follow-mode nil)
  (setq org-agenda-sorting-strategy '(habit-down time-up priority-up category-keep)) ; '(todo-state-down priority-down))
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-todo-list-sublevels t)
  (setq org-agenda-dim-blocked-tasks t)     ; Dim blocked tasks
  (setq org-agenda-show-future-repeats 'next) ; 'next to view this and the next.
  (setq org-agenda-search-view-always-boolean t) ; Lazy boolean search =C-c a s=
  (setq org-agenda-sticky t)
  (setq org-agenda-show-inherited-tags nil)
  ;; For tag searches ignore tasks with scheduled and deadline dates FIXME better control this in each agenda custom view
  ;; needed to avoid seeing missed tasks in my unscheduled view; next tasks in daily review view; in w but W is ok;
  (setq org-agenda-tags-todo-honor-ignore-options t)
  ;; (setq org-agenda-follow-indirect t)
  ;; FIXME: could help following projects individually
  (advice-add 'org-agenda-goto :after
              (lambda (&rest args)
                (org-narrow-to-subtree)))
  (setq org-element-use-cache t)
  ;; org-compat
  ;; (setq org-agenda-overriding-columns-format "%TODO 100%ITEM %7EFFORT %SCHEDULED %DEADLINE 100%TAGS")
  (setq org-columns-default-format
        "%50ITEM(Task) %15CATEGORY(Category) %5TODO(Status) %SCHEDULED %DEADLINE %TAGS")

  (setq org-agenda-custom-commands
        '(
          ("0" "Tasks to refile or archive"
           ((tags "REFILE" ((org-agenda-overriding-header "Tasks to Refile")))
            (tags "-NOTE-REFILE/DONE|CANC"
                  ((org-agenda-overriding-header "Tasks to Archive")))))

          ("l" "Standalone unscheduled tasks"
           (
            (tags "-proj-recurring-STYLE=\"habit\"+WORK/TODO|NEXT"
                  ((org-agenda-overriding-header "Work")))
            (tags "-proj-recurring-STYLE=\"habit\"+PERSONAL-WORK/TODO|NEXT"
                  ((org-agenda-overriding-header "Personal")))
            (tags "-proj-recurring-STYLE=\"habit\"-WORK-PERSONAL/TODO|NEXT"
                  ((org-agenda-overriding-header "Unassigned")))
            )
           (
            (org-agenda-sorting-strategy '(priority-down category-keep tag-down))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
            (org-tags-match-list-sublevels 'indented)
            ))

          ("b" "Backwards calendar loops"
           ((agenda "Backward"
                    ((org-agenda-overriding-header "Backwards calendar loops")
                     ;; (org-agenda-overriding-columns-format "%20ITEM %DEADLINE")
                     ;; (org-agenda-view-columns-initially t)
                     (org-agenda-span 10)
                     (org-agenda-start-day "-10d")
                     (org-agenda-start-with-log-mode t)
                     (org-agenda-include-diary nil)
                     (org-agenda-skip-timestamp-if-done nil)
                     )))
           ((org-agenda-show-future-repeats t)
            (org-agenda-compact-blocks nil)))

          ("f" "Forwards loops, habits and recurring tasks"
           ((agenda "Planning"
                    ((org-agenda-start-day "+1d")
                     (org-agenda-span 14)
                     (org-agenda-overriding-header "Next 14 days planning")
                     (org-agenda-show-all-dates nil)))
            (tags-todo "-recurring-STYLE=\"habit\"&DEADLINE>\"<+14d>\""
                       ((org-agenda-overriding-header "Next deadlines")
                        (org-agenda-sorting-strategy '(deadline-up))))
            (agenda "Scheduled_Deadline"
                    ((org-agenda-overriding-header "Significant deadlines")
                     (org-agenda-entry-types '(:scheduled :deadline))
                     (org-agenda-start-day "+15d")
                     (org-agenda-span 165)
                     (org-agenda-include-diary nil)
                     (org-agenda-show-all-dates nil)
                     (org-agenda-time-grid nil)))
            (tags-todo "+SCHEDULED>=\"<+180d>\"\|+DEADLINE>=\"<+180d>\""
                       ((org-agenda-overriding-header "Over 6 months tasks and deadlines"))))
           ((org-agenda-sorting-strategy '(deadline-up))))
          ("F" "Forward planning and deadlines"
           ((agenda "Next 14 days"
                    ((org-agenda-start-day "+1d")
                     (org-agenda-span 14)
                     (org-agenda-overriding-header "Next 14 days planning")
                     (org-agenda-show-all-dates nil)))
            (agenda "Long-term schedule"
                    ((org-agenda-entry-types '(:scheduled :deadline))
                     (org-agenda-start-day "+15d")
                     (org-agenda-span 165)
                     (org-agenda-overriding-header "Significant Deadlines (15-180 days)")
                     (org-agenda-include-diary nil)
                     (org-agenda-show-all-dates nil)
                     (org-agenda-time-grid nil)))
            (tags-todo "+SCHEDULED>=\"<+180d>\"|+DEADLINE>=\"<+180d>\""
                       ((org-agenda-overriding-header "Long-term Tasks (180+ days)"))))
           ((org-agenda-sorting-strategy '(deadline-up))
            (org-agenda-compact-blocks nil)))

          ("w" "Follow-up list"
           (
            (tags "-proj/!WAIT|PASS"
                  ((org-agenda-overriding-header "Follow-up tasks list")))
            (tags "+proj/!WAIT|PASS"
                  ((org-agenda-overriding-header "Follow-up projects list")
                   (org-tags-exclude-from-inheritance '("proj"))))
            )
           (
            (org-tags-match-list-sublevels 'indented)))

          ("X" "Project Review"
           (
            (tags "+proj"
                  ((org-agenda-overriding-header "Project Review")
                   (org-agenda-columns-compute-summary nil)
                   (org-agenda-view-columns-initially t)
                   (org-agenda-sorting-strategy '(category-keep priority-down))
                   (org-agenda-column-format "%20Project %15CATEGORY %10STATUS %30Next Action")
                   (org-agenda-tags-todo-honor-ignore-options t)
                   (org-agenda-follow-mode t)
                   (org-agenda-show-log nil)
                   (org-agenda-show-inherited-tags t)
                   ))
            ))
          ("J" "Projects"
           ((tags "+proj+DEADLINE={.+}/-DONE-CANC"
                  ((org-agenda-overriding-header "Due Projects")))
            (tags "+proj-DEADLINE={.+}/-DONE-CANC"
                  ((org-agenda-overriding-header "All Projects")))
            (org-columns-default-format
             "%50ITEM(Task) %CATEGORY(Category) %TODO(Status) %TODO(Next Action)")
            ))
          ("j" "Projects list"
           (
            ;; (tags "+proj/-HOLD-MAYB-PASS-WAIT-CANC-DONE"; -Proj=\"ignore\"
            (tags "+proj+DEADLINE={.+}/-DONE-CANC"
                  ((org-agenda-overriding-header "Projects due")
                   (org-tags-exclude-from-inheritance '("proj"))
                   ))
            (tags "+proj-DEADLINE={.+}/-DONE-CANC"
                  ((org-agenda-overriding-header "Projects")
                   (org-tags-exclude-from-inheritance '("proj"))
                   ))
            (tags "+proj"
                  ((org-agenda-overriding-header "Project tasks")
                   (org-agenda-sorting-strategy '(todo-state-up))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":proj:"))
                   ))
            )
           ((org-tags-match-list-sublevels 'indented)
            (org-agenda-files (append da-agenda-files
                                      '("~/Sync/notes/arch/emacs.org"
                                        "~/Sync/notes/arch/archlinux.org")))
            (org-agenda-sorting-strategy '(category-keep priority-down))
            ;; (org-use-property-inheritance t)
            (org-agenda-tag-filter-preset '("-WAITING" "-PASSED" "-HOLDING" "-MAYBE" "-ENDED"))
            ))

          ("h" "Tasks and projects on hold"
           (
            (tags "-proj/+HOLD"
                  ((org-agenda-overriding-header "Tasks on hold")))
            (tags "+proj/+HOLD"
                  ((org-agenda-overriding-header "Projects on hold")
                   (org-tags-exclude-from-inheritance '("proj"))))
            (tags-todo "TIMESTAMP<=\"<now>\"")
            )
           (
            (org-tags-match-list-sublevels 'indented)))

          ("i" "Idea and hold, maybe, someday tasks and-or projects"
           (
            (tags "-proj-SCHEDULED={.+}-DEADLINE={.+}/+MAYB"
                  ((org-agenda-overriding-header "Tasks for someday")
                   ))
            (tags "+proj/+MAYB"
                  ((org-agenda-overriding-header "Projects for someday")
                   (org-tags-exclude-from-inheritance '("proj"))))
            (tags "+idea/-MAYB"
                  ((org-agenda-overriding-header "Ideas")
                   (org-tags-match-list-sublevels 'indented)
                   (org-agenda-sorting-strategy '(todo-state-up priority-down category-keep tag-down))))
            )
           (
            (org-agenda-sorting-strategy '(todo-state-up priority-down tag-down category-keep))
            ))

          ("2" "Scattered action list"
           (
            (tags-todo "-proj")
            (agenda "" ((org-agenda-span 1)))
            (tags "+proj"
                  ((org-agenda-overriding-header "All projects scattered outside the agenda files")
                   (org-tags-exclude-from-inheritance '("proj"))))
            )
           (
            (org-agenda-sorting-strategy '(priority-down todo-state-down category-keep habit-down))
            (org-agenda-files da-refile-files)
            (org-tags-match-list-sublevels 'indented)
            (org-agenda-compact-blocks nil)
            )
           )
          ("a" "Actions list [today]"
           (
            (agenda "Journal"
                    ((org-agenda-entry-types '(:timestamp :scheduled :deadline))
                     (org-agenda-span 1)
                     (org-agenda-sorting-strategy '(deadline-up time-up scheduled-down priority-down))
                     (org-deadline-warning-days 0)
                     ))
            ))
          ("D" "Daily Review"
           ((agenda "" ((org-agenda-span 1)))
            (tags-todo "+proj-WAITING-PASSED-HOLDING-MAYBE/PASS|WAIT|NEXT"
                       ((org-agenda-overriding-header "Active Projects")))))
          ("d" "Daily review"
           (
            (agenda "Today"
                    ((org-agenda-span 1)
                     (org-deadline-warning-days 0)
                     (org-agenda-entry-types '(:timestamp :scheduled :deadline))
                     (org-agenda-sorting-strategy '(deadline-up time-up scheduled-down priority-down))
                     ))
            (agenda "Next 6 days"
                    ((org-agenda-start-day "+1d")
                     (org-agenda-span 6)
                     (org-agenda-show-all-dates nil)
                     (org-agenda-time-grid
                      '((weekly) (1300)
                        "      " "················"))
                     ))
            (tags-todo "+Effort>\"0\"&Effort<=\"0:15\""
                       ((org-agenda-overriding-header "Quick Picks")
                        (org-agenda-todo-ignore-scheduled 'all)
                        (org-agenda-todo-ignore-deadlines 'far)))
            (tags-todo "-proj-recurring+PRIORITY=\"A\"/MAYB|TODO|NEXT" ;do not use -todo for refile archive
                       ((org-agenda-overriding-header "Pick list (standalone tasks)")
                        (org-agenda-files (append da-agenda-files
                                                  '("~/Sync/notes/arch/emacs.org"
                                                    "~/Sync/notes/arch/archlinux.org")))
                        (org-agenda-todo-ignore-scheduled 'all)
                        (org-agenda-todo-ignore-deadlines 'near)
                        (org-tags-match-list-sublevels 'indented)
                        ))
            (tags-todo "+study-PRIORITY=\"A\"" ;do not use -todo for refile archive
                       ((org-agenda-overriding-header "Pick list (to study)")
                        (org-agenda-files (append da-agenda-files
                                                  '("~/Sync/notes/arch/emacs.org"
                                                    "~/Sync/notes/arch/archlinux.org")))
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-todo-ignore-deadlines t)
                        (org-tags-match-list-sublevels 'indented)
                        ))
            (tags-todo "+proj-WAITING-PASSED-HOLDING-MAYBE/PASS|WAIT|NEXT" ;do not use -todo for refile archive
                       ((org-agenda-overriding-header "Pick list (projects)")
                        (org-agenda-files (append da-agenda-files
                                                  '("~/Sync/notes/arch/emacs.org"
                                                    "~/Sync/notes/arch/archlinux.org")))
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-todo-ignore-deadlines t)
                        (org-tags-match-list-sublevels 'indented)
                        (org-agenda-sorting-strategy '(category-keep todo-state-down priority-down habit-down))
                        ))
            )
           ((org-agenda-include-diary nil)
            ;; (org-agenda-sorting-strategy '(deadline-up scheduled-up habit-down time-up tag-down category-keep priority-up))
            (org-agenda-compact-blocks nil)))

          ("c" . "Contexts")
          ("ce" "@errand" tags-todo "@errand")
          ("cf" "@fbk" tags-todo "@fbk")
          ("ch" "@home" tags-todo "@home")
          ("cd" "@dati" tags-todo "@dati")
          ("ct" "@telephone" tags-todo "@telephone")
          ("E" "Export agenda"
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
            (org-agenda-remove-tags t)
            (htmlize-output-type 'css)
            (ps-number-of-columns 2)
            (ps-landscape-mode t))
           ("~/agenda.pdf" "~/agenda.html"))
          ))
  )

(use-package org-faces :straight org
  :after org
  :config
  ;; Customizing Org-mode todo keywords and tags faces
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "crimson" :weight bold :box (:line-width 2 :style released-button))
          ("NEXT" :foreground "light blue" :weight bold :box (:line-width 2 :style released-button))
          ;; ("APPT" :foreground "yellow" :weight bold)
          ("WAIT" (:foreground "orange" :weight bold))
          ("PASS" :foreground "SpringGreen" :weight bold)
          ("HOLD" :foreground "SaddleBrown" :weight bold )
          ("MAYB" :foreground "MediumAquamarine" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("CANC" (:foreground "forest green" :weight bold :strike-through t))
          ))
  (setq org-tag-faces
        '(("WORK" :foreground "green")
          ("PERSONAL" :foreground "orange")
          ("proj" :weight bold)
          ("@fbk" :weight italic)))
  ;; Customizing other Org-mode element faces for consistent styling
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit '(fixed-pitch shadow))
  (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch
                      :background "burlywood" :foreground "darkred" :underline "darkred")
  (set-face-attribute 'org-block-end-line nil :inherit 'fixed-pitch
                      :background "burlywood" :foreground "darkred" :overline "yellow")
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-link nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch))

(use-package org-lint :straight org
  :after org
  :preface
  (defun da-org-lint-critical ()
    "Run org-lint with only critical checks before saving."
    (interactive)
    (let ((org-lint-checkers
           '(org-lint-invalid-babel-call
             org-lint-obsolete-syntax
             org-lint-clock-out-of-sync
             org-lint-duplicate-name)))
      (org-lint)))
  :config
  (add-hook 'org-mode-hook
            (lambda () (add-hook 'before-save-hook 'da-org-lint-critical nil 'local)))
  :bind (:map
         org-mode-map
         ("M-g e o" . org-lint))
  )

(use-package ob-ditaa :straight org
  :after org
  :config
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
  )

;; --- Other packages ---
(use-package org-modern :after org)
(use-package spacious-padding :after org)

(use-package org-autolist
  :after (org)
  :hook (org-mode . org-autolist-mode))

(use-package org-download
  :after (org)
  :bind (:map org-mode-map
              ("<Launch5> d c" . org-download-clipboard)
              ("<Launch5> d i" . org-download-image)
              ("<Launch5> d y" . org-download-yank)
              ("<Launch5> d e" . org-download-edit)
              ("<Launch5> d k" . org-download-delete)
              ("<Launch5> d r" . org-download-rename-at-point)
              ("<Launch5> d R" . org-download-rename-last-file)
              ("<Launch5> d s" . org-download-screenshot))
  :hook
  (dired-mode . org-download-enable)
  (org-mode . org-download-enable)
  :config
  (setq org-download-method 'directory) ;'attach
  ;; (setq org-download-screenshot-method "sleep 10 && flameshot gui -p %s")
  ;; (setq org-download-screenshot-method "maim -o -u -s %s")
  (setq org-download-screenshot-method "sleep 7; maim -s %s")
  (setq org-download-heading-lvl 2)  ;; nil Save all images in the same directory
  )

(use-package org-cliplink
  :bind ("<Launch5> i c" . org-cliplink)
  )

(provide 'my-org)
;;; my-org.el ends here
