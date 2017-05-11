
   ;; Activate org-zotxt-mode in org-mode buffers
   (add-hook 'org-mode-hook (lambda () (org-zotxt-mode 1)))
   ;; Change citation format to be less cumbersome in files.
   ;; You'll need to install mkbehr-short into your style manager first.
   ;; (eval-after-load "zotxt"
   ;;   '(setq zotxt-default-bibliography-style "citekey"))
   (setq reftex-default-bibliography '("~/Sync/bib.bib"))

   (setq bibtex-completion-pdf-field "file")
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
   ;; (setq   bibtex-completion-notes-path     "~/Sync/media/Papers.org"
   ;;         bibtex-completion-bibliography '("~/Sync/bib2.bib")
   ;;         bibtex-completion-library-path '("~/Sync/media/zotfiles/Journal Article"))
   (setq   bibtex-completion-pdf-field "file")
   ;; see org-ref for use of these variables
   (setq org-ref-bibliography-notes "~/Sync/media/bibliography.org"
         org-ref-default-bibliography '("~/Sync/bib.bib")
         org-ref-completion-pdf-field "file"
         org-ref-pdf-directory "~/Sync/media/zotfiles/Journal Article/"
         )

(provide 'bibtex-config)
