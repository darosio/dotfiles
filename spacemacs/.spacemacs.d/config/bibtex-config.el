
;;  see org-ref for use of these variables  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-ref-bibliography-notes     "~/Sync/biblio/biblio.org")
(setq org-ref-default-bibliography '("~/Sync/biblio/biblio.bib"
                                     ;; "~/Sync/media/bibliography/nurturing.bib"
                                     ))
(setq   org-ref-pdf-directory "~/Sync/biblio/pdfs")
        ;; '(;; "~/Sync/media/bibliography/misc")

;; (setq reftex-default-bibliography '("~/Sync/media/bibliography/biblio.bib"
;;                                     ;; "~/Sync/media/bibliography/nurturing.bib"
;;                                     ))


;; bibtex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq bibtex-completion-notes-path "~/Sync/biblio/biblio.org")
(setq bibtex-completion-library-path "~/Sync/biblio/pdfs/")
;; (setq   bibtex-completion-pdf-field "file")
(setq bibtex-completion-bibliography '("~/Sync/biblio/biblio.bib"
                                       ;; "~/Sync/media/bibliography/misc.bib"
                                       ;; "~/Sync/media/bibliography/nurturing.bib"
                                       ))

;; (setq bibtex-completion-display-formats
;;       '((t . "${author:26} ${title:*} ${date:24} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")))
;; (setq bibtex-completion-display-formats
;;       '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*} ${journal:40}")
;;         (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*} Chapter ${chapter:32}")
;;         (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*} ${booktitle:40}")
;;         (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*} ${booktitle:40}")
;;         (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*}")))


;; Activate org-zotxt-mode in org-mode buffers
;; (add-hook 'org-mode-hook (lambda () (org-zotxt-mode 1)))
;; Change citation format to be less cumbersome in files.
;; You'll need to install mkbehr-short into your style manager first.
;; (eval-after-load "zotxt"
;;   '(setq zotxt-default-bibliography-style "citekey"))

(provide 'bibtex-config)
