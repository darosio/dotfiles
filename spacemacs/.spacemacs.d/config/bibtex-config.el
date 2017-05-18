
   ;; Activate org-zotxt-mode in org-mode buffers
   ;; (add-hook 'org-mode-hook (lambda () (org-zotxt-mode 1)))
   ;; Change citation format to be less cumbersome in files.
   ;; You'll need to install mkbehr-short into your style manager first.
   ;; (eval-after-load "zotxt"
   ;;   '(setq zotxt-default-bibliography-style "citekey"))
   ;(setq reftex-default-bibliography '("~/Sync/bib.bib"))

   ;; (defun my/org-ref-open-pdf-at-point ()
   ;;   "Open the pdf for bibtex key under point if it exists."
   ;;   (interactive)
   ;;   (let* ((results (org-ref-get-bibtex-key-and-file))
   ;;          (key (car results))
   ;;          (pdf-file (car (bibtex-completion-find-pdf key))))
   ;;     (if (file-exists-p pdf-file)
   ;;         (org-open-file pdf-file)
   ;;       (message "No PDF found for %s" key))))
   ;; (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)
   ;; (use-package ivy-bibtex :ensure t)
   ;;         bibtex-completion-library-path '("~/Sync/media/zotfiles/Journal Article"))


(setq   bibtex-completion-pdf-field "file"
        ;; bibtex-completion-bibliography '("~/Sync/bib2.bib"))
                bibtex-completion-library-path "/home/dan"
;; bibtex-completion-notes-path     "~/biblio.org"
        bibtex-completion-bibliography '("~/Sync/media/bibliography/biblio.bib"
                                         ;; "~/Sync/media/bibliography/misc.bib"
                                         "~/Sync/media/bibliography/nurturing.bib"))
;; (--map (concat it fpath)
;;        (-flatten bibtex-completion-library-path)) 
(setq bibtex-completion-display-formats
      '((t . "${author:26} ${title:*} ${date:24} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")))
;; (setq bibtex-completion-display-formats
;;       '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*} ${journal:40}")
;;         (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*} Chapter ${chapter:32}")
;;         (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*} ${booktitle:40}")
;;         (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*} ${booktitle:40}")
;;         (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*}")))
;; Attempts to open pdf (exported from zotero) from org-ref
;;    ;; see org-ref for use of these variables
(setq org-ref-bibliography-notes  "~/Sync/media/bibliography/biblio-notes.org")
(setq      org-ref-default-bibliography '("~/Sync/media/bibliography/biblio.bib" "~/Sync/media/bibliography/nurturing.bib"))
   ;; (setq org-ref-completion-pdf-field "file")
;; ;; (setq org-ref-open-pdf-function 'org-ref-get-zotero-filename
(setq   org-ref-pdf-directory '("~/Sync/media/bibliography/nurturing" "~/Sync/media/bibliography/biblio"))
;; (setq   org-ref-pdf-directory '("/home/dan/Sync/media/bibliography/biblio" ))
;;                                         ;      org-ref-pdf-directory "~/Sync/media/zotfiles/Journal Article/"
;;    ;      )
;; (defun my/org-ref-open-pdf-at-point ()
;;   "Open the pdf for bibtex key under point if it exists."
;;   (interactive)
;;   (let* ((results (org-ref-get-bibtex-key-and-file))
;;          (key (car results))
;;          (pdf-file (funcall org-ref-get-pdf-filename-function key))
;;          (pdf-other (bibtex-completion-find-pdf key)))
;;     (cond ((file-exists-p pdf-file)
;;            (org-open-file pdf-file))
;;           (pdf-other
;;            (org-open-file pdf-other))
;;           (message "No PDF found for %s" key))))

;; (setq helm-bibtex-pdf-open-function 'org-open-file)
;; (setq org-ref-open-pdf-function 'org-ref-get-mendeley-filename)
;; (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)
;; (defun org-ref-open-bibtex-pdf ()
;;   (interactive)
;;   (save-excursion
;;     (bibtex-beginning-of-entry)
;;     (let* ((bibtex-expand-strings t)
;;            (entry (bibtex-parse-entry t))
;;            (key (reftex-get-bib-field "=key=" entry))
;;            (pdf (org-ref-get-mendeley-filename key)))
;;       (message "%s" pdf)
;;       (if (file-exists-p pdf)
;;           (org-open-link-from-string (format "[[file:%s]]" pdf))
;;         (ding)))))
;; (setq org-ref-get-pdf-filename-function 'org-ref-get-mendeley-filename)

(provide 'bibtex-config)
