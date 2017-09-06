(require 'elfeed)
(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq httpd-port 8181)
;; (setq elfeed-search-trailing-width 23)
;; (setq elfeed-search-title-max-width 190)
;; (defun elfeed-search-format-date (date)
;;   (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))


(defun elfeed-goodies/entry-line-draw (entry)
  "Print ENTRY to the buffer."
  (let* ((title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
  (date (elfeed-search-format-date (elfeed-entry-date entry)))
  (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
  (feed (elfeed-entry-feed entry))
  (feed-title
   (when feed
     (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
  (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
  (tags-str (concat "[" (mapconcat 'identity tags ",") "]"))
  (title-width (- (window-width) elfeed-goodies/feed-source-column-width
    elfeed-goodies/tag-column-width 4))
  (title-column (elfeed-format-column
   title (elfeed-clamp
          elfeed-search-title-min-width
          title-width
          title-width)
   :left))
  (tag-column (elfeed-format-column
        tags-str (elfeed-clamp (length tags-str)
          elfeed-goodies/tag-column-width
          elfeed-goodies/tag-column-width)
        :left))
  (feed-column (elfeed-format-column
         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
      elfeed-goodies/feed-source-column-width
      elfeed-goodies/feed-source-column-width)
         :left)))
    (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
 (progn
   (insert (propertize date 'face 'elfeed-search-date-face) " ")
   (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
   (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
   (insert (propertize title 'face title-faces 'kbd-help title)))
    (insert (propertize title 'face title-faces 'kbd-help title)))))


;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force)
  (elfeed-update)) ;; maybe this last line is not necessary
;;write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

;; http://pragmaticemacs.com/category/elfeed/
(defalias 'elfeed-toggle-star
  (elfeed-expose #'elfeed-search-toggle-all 'star))
;; (eval-after-load 'elfeed-search
;;   '(define-key elfeed-search-mode-map (kbd "f") 'elfeed-toggle-star))
(define-key elfeed-show-mode-map (kbd ".") 'visual-fill-column-mode)


;; Hydra macros and elfeed tags
;; http://cestlaz.github.io/posts/using-emacs-31-elfeed-3/#.WYGkIXWxVhE
(defun z/hasCap (s) ""
       (let ((case-fold-search nil))
         (string-match-p "[[:upper:]]" s)
         ))

(defun z/get-hydra-option-key (s)
  "returns single upper case letter (converted to lower) or first"
  (interactive)
  (let ( (loc (z/hasCap s)))
    (if loc
        (downcase (substring s loc (+ loc 1)))
      (substring s 0 1)
      )))

(defun mz/make-elfeed-cats (tags)
  "Returns a list of lists. Each one is line for the hydra configuratio in the form
     (c function hint)"
  (interactive)
  (mapcar (lambda (tag)
            (let* (
                   (tagstring (symbol-name tag))
                   (c (z/get-hydra-option-key tagstring))
                   )
              (list c (append '(elfeed-search-set-filter) (list (format "@6-months-ago +%s" tagstring) ))tagstring  )))
          tags))

(defmacro mz/make-elfeed-hydra ()
  `(defhydra mz/hydra-elfeed ()
     "filter"
     ,@(mz/make-elfeed-cats (elfeed-db-get-all-tags))
     ("*" (elfeed-search-set-filter "@6-months-ago +star") "Flagged")
     ("A" (elfeed-search-set-filter "@6-months-ago") "All")
     ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
     ("q" bjm/elfeed-save-db-and-bury "Quit Elfeed" :color blue)
     ))

(defun mz/make-and-run-elfeed-hydra ()
  ""
  (interactive)
  (mz/make-elfeed-hydra)
  (mz/hydra-elfeed/body))

(define-key elfeed-search-mode-map (kbd "<DEL>") 'elfeed-search-untag-all-unread)
(define-key elfeed-search-mode-map (kbd "J") 'mz/make-and-run-elfeed-hydra)

;; Custom faces
(defface pYthon-elfeed-entry
  '((t :background "Darkseagreen1"))
  "Marks a python Elfeed entry.")
(defface emacs-elfeed-entry
  '((t :background "Blue"))
  "Marks a emacs Elfeed entry.")
(defface elfeed-search-starred-title-face
  '((t :foreground "Orange"))
  "Marks a starred Elfeed entry.")
(push '(python python-elfeed-entry) elfeed-search-face-alist)
(push '(emacs emacs-elfeed-entry) elfeed-search-face-alist)
(push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)

;; Captures, email, doi into biblio
(defun email-elfeed-entry ()
  "Capture the elfeed entry and put it in an email."
  (interactive)
  (let* ((title (elfeed-entry-title elfeed-show-entry))
         (url (elfeed-entry-link elfeed-show-entry))
         (content (elfeed-entry-content elfeed-show-entry))
         (entry-id (elfeed-entry-id elfeed-show-entry))
         (entry-link (elfeed-entry-link elfeed-show-entry))
         (entry-id-str (concat (car entry-id)
                               "|"
                               (cdr entry-id)
                               "|"
                               url)))
    (compose-mail)
    (message-goto-subject)
    (insert title)
    (message-goto-body)
    (insert (format "You may find this interesting:
%s\n\n" url))
    (insert (elfeed-deref content))

    (message-goto-body)
    (while (re-search-forward "<br>" nil t)
      (replace-match "\n\n"))

    (message-goto-body)
    (while (re-search-forward "<.*?>" nil t)
      (replace-match ""))

    (message-goto-body)
    (fill-region (point) (point-max))

    (message-goto-to)
    (ivy-contacts nil)))

(defun doi-utils-add-entry-from-elfeed-entry ()
  "Add elfeed entry to bibtex."
  (interactive)
  (require 'org-ref)
  (let* ((title (elfeed-entry-title elfeed-show-entry))
	 (url (elfeed-entry-link elfeed-show-entry))
	 (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
	 (entry-id (elfeed-entry-id elfeed-show-entry))
	 (entry-link (elfeed-entry-link elfeed-show-entry))
	 (entry-id-str (concat (car entry-id)
			       "|"
			       (cdr entry-id)
			       "|"
			       url)))
    (if (string-match "DOI: \\(.*\\)$" content)
	(doi-add-bibtex-entry (match-string 1 content)
			      (ido-completing-read
			       "Bibfile: "
			       (append (f-entries "." (lambda (f)
							(and (not (string-match "#" f))
							     (f-ext? f "bib"))))
				       org-ref-default-bibliography)))
      (let ((dois (org-ref-url-scrape-dois url)))
	(cond
	 ;; One doi found. Assume it is what we want.
	 ((= 1 (length dois))
	  (doi-utils-add-bibtex-entry-from-doi
	   (car dois)
	   (ido-completing-read
	    "Bibfile: "
	    (append (f-entries "." (lambda (f)
				     (and (not (string-match "#" f))
					  (f-ext? f "bib"))))
		    org-ref-default-bibliography)))
	  action)
	 ;; Multiple DOIs found
	 ((> (length dois) 1)
	  (helm :sources
		`((name . "Select a DOI")
		  (candidates . ,(let ((dois '()))
				   (with-current-buffer (url-retrieve-synchronously url)
				     (loop for doi-pattern in org-ref-doi-regexps
					   do
					   (goto-char (point-min))
					   (while (re-search-forward doi-pattern nil t)
					     (pushnew
					      ;; Cut off the doi, sometimes
					      ;; false matches are long.
					      (cons (format "%40s | %s"
							    (substring
							     (match-string 1)
							     0 (min
								(length (match-string 1))
								40))
							    doi-pattern)
						    (match-string 1))
					      dois
					      :test #'equal)))
				     (reverse dois))))
		  (action . (lambda (candidates)
			      (let ((bibfile (ido-completing-read
					      "Bibfile: "
					      (append (f-entries "." (lambda (f)
								       (and (not (string-match "#" f))
									    (f-ext? f "bib"))))
						      org-ref-default-bibliography))))
				(loop for doi in (helm-marked-candidates)
				      do
				      (doi-utils-add-bibtex-entry-from-doi
				       doi
				       bibfile)
				      ;; this removes two blank lines before each entry.
				      (bibtex-beginning-of-entry)
				      (delete-char -2)))))))))))))

(define-key elfeed-show-mode-map (kbd "e") 'email-elfeed-entry)
(define-key elfeed-show-mode-map (kbd "c") (lambda () (interactive) (org-capture nil "t")))
(define-key elfeed-show-mode-map (kbd "d") 'doi-utils-add-entry-from-elfeed-entry)

(provide 'elfeed-config)
