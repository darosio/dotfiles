;;; packages.el --- BibTeX Layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Joshua Ellis <josh@jpellis.me>
;; URL: https://github.com/JP-Ellis
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq my-bibtex-packages
      '(
        auctex
        org
        markdown-mode
        org-ref
        ivy-bibtex
        biblio
        biblio-core
        ))

(defun my-bibtex/post-init-auctex ()
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode
    "ic" 'org-ref-icy-insert-cite-link))

(defun my-bibtex/post-init-org ()
  ;; folding in .bib =z m= and =TAB=
  ;; (define-key bibtex-mode-map (kbd "<tab>") (kbd "za"))
  (add-hook 'bibtex-mode-hook 'outline-minor-mode)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "ic" 'org-ref-ivy-insert-cite-link))
  ;;   "ic" 'org-ref-helm-insert-cite-link))

(defun my-bibtex/init-org-ref ()
  (use-package org-ref
    :defer t
    :commands (org-ref-bibtex-next-entry
               org-ref-bibtex-previous-entry
               org-ref-open-in-browser
               org-ref-open-bibtex-notes
               org-ref-open-bibtex-pdf
               org-ref-bibtex-hydra/body
               org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
               org-ref-sort-bibtex-entry
               arxiv-add-bibtex-entry
               arxiv-get-pdf-add-bibtex-entry
               doi-utils-add-bibtex-entry-from-doi
               isbn-to-bibtex
               ivy-bibtex-edit-notes
               pubmed-insert-bibtex-from-pmid)
    :init
    (progn
      (setq org-ref-completion-library 'org-ref-ivy-cite)
      (setq org-ref-default-bibliography '("~/Sync/biblio/biblio.bib"
                                           "~/Sync/biblio/MY.bib")
            org-ref-pdf-directory "~/Sync/biblio/pdfs/"
            org-ref-bibliography-notes "~/Sync/biblio/biblio.org"
            org-ref-bibliography-files '("~/Sync/biblio/biblio.bib"
                                         "~/Sync/biblio/MY.bib")
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

      (evil-define-key 'normal bibtex-mode-map
        (kbd "C-j") 'org-ref-bibtex-next-entry
        (kbd "C-k") 'org-ref-bibtex-previous-entry
        "gj" 'org-ref-bibtex-next-entry
        "gk" 'org-ref-bibtex-previous-entry)


      (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode
        ;; Navigation
        "j" 'org-ref-bibtex-next-entry
        "k" 'org-ref-bibtex-previous-entry

        ;; Open
        "b" 'org-ref-open-in-browser
        "n" 'org-ref-open-bibtex-notes
        "p" 'org-ref-open-bibtex-pdf

        ;; Misc
        "h" 'org-ref-bibtex-hydra/body
        "i" 'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
        "s" 'org-ref-sort-bibtex-entry

        ;; Lookup utilities
        "la" 'arxiv-add-bibtex-entry
        "lA" 'arxiv-get-pdf-add-bibtex-entry
        "ld" 'doi-utils-add-bibtex-entry-from-doi
        "li" 'isbn-to-bibtex
        "lp" 'pubmed-insert-bibtex-from-pmid))
    :config
    (progn


      ;; Tell org-ref to let helm-bibtex find notes for it
      (setq org-ref-notes-function
            (lambda (thekey)
              ;; (let ((bibtex-completion-notes-path (org-ref-find-bibliography)))
              ;;   ;; (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
                (ivy-bibtex-edit-notes
                 ;; bibtex-completion-edit-notes
                 (list (car (org-ref-get-bibtex-key-and-file thekey))))));;)
      ;; (defun my/org-ref-notes-function (candidates)
      ;;   (let ((key (helm-marked-candidates)))
      ;;     (funcall org-ref-notes-function (car key))))
      ;; (helm-delete-action-from-source "Edit notes" helm-source-bibtex)
      ;; ;; Note that 7 is a magic number of the index where you want to insert the command. You may need to change yours.
      ;; (helm-add-action-to-source "Edit notes" 'my/org-ref-notes-function helm-source-bibtex 7)

      )))

(defun my-bibtex/pre-init-org-ref ()
  (add-hook 'org-mode-hook (lambda () (require 'org-ref))))

(defun my-bibtex/post-init-markdown-mode ()
  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
    "ic" 'org-ref-icy-insert-cite-link))

;; (defun my-bibtex/init-helm-bibtex ())
(defun my-bibtex/init-ivy-bibtex ()
  (use-package ivy-bibtex
    :defer t
    :init
    (progn
      (setq bibtex-completion-bibliography '(("~/Sync/biblio/biblio.org" . "~/Sync/biblio/biblio.bib")
                                             ("~/Sync/biblio/MY.org" . "~/Sync/biblio/MY.bib"))
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
      (setq bibtex-completion-notes-template-one-file
            (format
             "\n** TODO ${=key=}: ${title}\n \ :PROPERTIES:\n \  :Custom_ID: ${=key=}\n \  :INTERLEAVE_PDF: \
             ./pdfs/${=key=}.pdf\n \ :END:\n\ncite:${=key=}\n\n")))
    :config
    (progn
      ;; Okular =P=
      (defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
        (let ((bibtex-completion-pdf-open-function
               (lambda (fpath) (start-process "okular" "*helm-bibtex-okular*" "/usr/bin/okular" fpath))))
          (bibtex-completion-open-pdf keys fallback-action)))
      (ivy-bibtex-ivify-action bibtex-completion-open-pdf-external ivy-bibtex-open-pdf-external)
      (ivy-add-actions
       'ivy-bibtex
       '(("P" ivy-bibtex-open-pdf-external "Open PDF file in external viewer (if present)")))
      )))

(defun my-bibtex/init-biblio ())

(defun my-bibtex/init-biblio-core ())
;; I would export to LaTeX and then use bibtool -x to extract the refereneces to a .bib
