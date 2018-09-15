(provide 'bibtex-config)

;; multiple bib projects
;; https://emacs.stackexchange.com/questions/30095/org-ref-managing-multiple-projects-each-with-own-notes-org-files-and-bibtex-pd#30113

;; org-ref ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'org-ref-scopus)
;; https://www.reddit.com/r/emacs/comments/4gudyw/help_me_with_my_orgmode_workflow_for_notetaking/d2l16uj/
;; (require 'org-ref-sci-id)

(setq org-ref-default-bibliography '("~/Sync/biblio/biblio.bib"
                                     ;; "~/Sync/media/bibliography/nurturing.bib"
                                     ))
;; trailing / affects ,hA associate pdf to entry
(setq   org-ref-pdf-directory "~/Sync/biblio/pdfs/")
(setq org-ref-bibliography-notes     "~/Sync/biblio/biblio.org")

;; (setq reftex-default-bibliography '("~/Sync/media/bibliography/biblio.bib"))

;; Bibtex key format
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator ""
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 0
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5
      bibtex-autokey-name-case-convert-function 'capitalize)

;; Zotero ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq   bibtex-completion-pdf-field "file")
;; (defun my/org-ref-open-pdf-at-point ()
;;   "Open the pdf for bibtex key under point if it exists."
;;   (interactive)
;;   (let* ((results (org-ref-get-bibtex-key-and-file))
;;          (key (car results))
;; 	       (pdf-file (car (bibtex-completion-find-pdf key))))
;;     (if (file-exists-p pdf-file)
;; 	      (org-open-file pdf-file)
;;       (message "No PDF found for %s" key))))
;; (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

;; Activate org-zotxt-mode in org-mode buffers
;; (add-hook 'org-mode-hook (lambda () (org-zotxt-mode 1)))
;; Change citation format to be less cumbersome in files.
;; You'll need to install mkbehr-short into your style manager first.
;; (eval-after-load "zotxt"
;;   '(setq zotxt-default-bibliography-style "citekey"))

;; helm-bibtex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq bibtex-completion-notes-path "~/Sync/biblio/biblio.org")
(setq bibtex-completion-library-path '("~/Sync/biblio/pdfs/"
                                       "~/Sync/biblio/books/"))
(setq bibtex-completion-bibliography '("~/Sync/biblio/biblio.bib"
                                       ;; "~/Sync/media/bibliography/misc.bib"
                                       ;; "~/Sync/media/bibliography/nurturing.bib"
                                       ))
;; search also in tags and keywords fields
(setq bibtex-completion-additional-search-fields '(tags keywords))
;; find also additional pdfs
(setq bibtex-completion-find-additional-pdfs t)
(setq bibtex-completion-pdf-extension '(".pdf" ".avi"))
;; (setq bibtex-completion-display-formats
;;       '((t . "${author:26} ${title:*} ${date:24} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")))
;; (setq bibtex-completion-display-formats
;;       '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*} ${journal:40}")
;;         (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*} Chapter ${chapter:32}")
;;         (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*} ${booktitle:40}")
;;         (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*} ${booktitle:40}")
;;         (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:6} ${author:36} ${title:*}")))


(setq bibtex-completion-notes-template-one-file
      (format
       "\n** TODO ${=key=}: ${title}\n \
 :PROPERTIES:\n \
  :Custom_ID: ${=key=}\n \
  :INTERLEAVE_PDF: ./pdfs/${=key=}.pdf\n \
 :END:\n\ncite:${=key=}\n\n"))

;; org-ref notes style only
;; https://github.com/jkitchin/org-ref/issues/225
;; (setq org-ref-notes-function
;;       (lambda (thekey)
;;         (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
;;           (bibtex-completion-edit-notes
;;            (list (car (org-ref-get-bibtex-key-and-file thekey)))))))

;; try to set one-file format for org-ref
;; ;; (setq org-ref-notes-function-one-file
;; ;; (setq org-ref-note-title-format
;; (setq org-ref-open-bibtex-notes
;;       (format
;;        "\n** TODO ${=key=}: ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :INTERLEAVE_PDF: ./pdfs/${=key=}.pdf\n  :END:\n\ncite:${=key=}\n\n"))

;; (defun my/org-ref-notes-function (candidates)
;;   (let ((key (helm-marked-candidates)))
;;     (funcall org-ref-notes-function (car key))))
;; (helm-delete-action-from-source "Edit notes" helm-source-bibtex)
;; ;; Note that 7 is a magic number of the index where you want to insert the command. You may need to change yours.
;; (helm-add-action-to-source "Edit notes" 'my/org-ref-notes-function helm-source-bibtex 7)

;; (setq org-ref-notes-function
;;       (lambda (thekey)
;;         (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
;;           (bibtex-completion-edit-notes
;;            (list (car (org-ref-get-bibtex-key-and-file thekey)))))))
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (define-key org-mode-map  (kbd "C-c 9") 'org-ref-open-notes-at-point)))
;; To be deleted

(add-hook 'bibtex-mode-hook 'outline-minor-mode)  ;; =z M=
(define-key bibtex-mode-map (kbd "<tab>") (kbd "za"))  ;; =TAB=
