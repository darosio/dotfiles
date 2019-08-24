(provide 'bibtex-config)

(require 'helm-bibtex)
(require 'org-ref)

;; org-ref ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-ref-default-bibliography '("~/Sync/biblio/biblio.bib")
      org-ref-bibliography-files '("~/Sync/biblio/MY/MY.bib" "~/Sync/biblio/biblio.bib")
      ;; trailing / affects ,hA associate pdf to entry
      org-ref-pdf-directory "~/Sync/biblio/pdfs/"
      org-ref-bibliography-notes "~/Sync/biblio/biblio.org"
      reftex-default-bibliography '("~/Sync/biblio/biblio.bib"))
;; Bibtex key format
(setq bibtex-autokey-name-case-convert-function 'capitalize
      bibtex-autokey-name-year-separator ""
      ;; bibtex-autokey-year-length 2
      ;; bibtex-autokey-year-title-separator "_"
      bibtex-autokey-titleword-separator ""
      bibtex-autokey-titlewords 3
      bibtex-autokey-titleword-case-convert 'capitalize
      bibtex-autokey-titleword-length 5)
;; Notes template (compatible with interleave)
(setq org-ref-note-title-format
       "** %k; %t\n \ :PROPERTIES:\n \  :Custom_ID: %k\n \  :INTERLEAVE_PDF: \
             ./pdfs/%k.pdf\n \ :END:\n")
;; org-ref-create-notes-hook

;; helm-bibtex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq bibtex-completion-bibliography '("~/Sync/biblio/MY/MY.org" "~/Sync/biblio/biblio.org")
      bibtex-completion-notes-path "~/Sync/biblio/biblio.org"
      bibtex-completion-library-path '("~/Sync/biblio/pdfs/"
                                       "~/Sync/biblio/MY/"
                                       "~/Sync/biblio/books/"))
;; search also in tags and keywords fields
(setq bibtex-completion-additional-search-fields '(tags keywords))
;; Zotero
(setq bibtex-completion-pdf-field "file")
;; find also additional pdfs
(setq bibtex-completion-find-additional-pdfs t)
;; works only from helm-bibtex. less common e.g. ".md" can go into file={...} 
(setq bibtex-completion-pdf-extension '(".pdf" ".avi" ".ppt" ".odp" ".odt" ".doc" ".docx"))
;; Notes template (compatible with interleave)
;; use this (=, h h F8=) for books with "file" field and for auto-tagging with "keywords" field.
(setq bibtex-completion-notes-template-one-file
      (format
       "\n** ${=key=}; ${title} \t :${keywords}:\n \ :PROPERTIES:\n \  :Custom_ID: ${=key=}\n \  :INTERLEAVE_PDF: \
             ./pdfs/${=key=}.pdf ${file}\n \ :END:\ncite:${=key=}\n"))

;; rifle for helm
(defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
  (let ((bibtex-completion-pdf-open-function
         ;; (lambda (fpath) (start-process "okular" "*helm-bibtex-okular*" "/usr/bin/okular" fpath))))
         (lambda (fpath) (start-process "rifle" "*helm-bibtex-external*" "/usr/bin/rifle" fpath))))
    (bibtex-completion-open-pdf keys fallback-action)))
(helm-bibtex-helmify-action bibtex-completion-open-pdf-external helm-bibtex-open-pdf-external)
(helm-add-action-to-source "Open PDF using rifle" 'helm-bibtex-open-pdf-external helm-source-bibtex 2)

;; folding ;;;;;;;;;;;;;;;
(add-hook 'bibtex-mode-hook 'outline-minor-mode)  ;; =z M=
(define-key bibtex-mode-map (kbd "<tab>") (kbd "za"))  ;; =TAB=

;; left over ;;;;;;;;;;;;;
;; multiple bib projects, I was using in My.bib and My.org
;; https://emacs.stackexchange.com/questions/30095/org-ref-managing-multiple-projects-each-with-own-notes-org-files-and-bibtex-pd#30113

;; ;; only org ref notes format
;; (defun my/org-ref-notes-function (candidates)
;;   (interactive)
;;   (let* ((key (helm-marked-candidates)))
;;     (funcall org-ref-notes-function (car key))))
;; (helm-delete-action-from-source "Edit notes" helm-source-bibtex)
;; ;; Note that 7 is a magic number of the index where you want to insert the command. You may need to change yours.
;; (helm-add-action-to-source "Edit notes" 'my/org-ref-notes-function helm-source-bibtex 7)
;; ;; for org-ref-pdf-to-bibtex
;; (setq bibtex-completion-edit-notes 'my/org-ref-notes-function)

