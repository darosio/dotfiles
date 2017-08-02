
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
;; Bibtex key format
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator ""
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 0
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5
      bibtex-autokey-name-case-convert-function 'capitalize)

;; Zotero
(setq   bibtex-completion-pdf-field "file")

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
        (org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

;; bibtex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq bibtex-completion-notes-path "~/Sync/biblio/biblio.org")
(setq bibtex-completion-library-path "~/Sync/biblio/pdfs/")
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
