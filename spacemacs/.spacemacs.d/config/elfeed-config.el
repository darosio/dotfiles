(require 'elfeed)
(require 'elfeed-goodies)
(elfeed-goodies/setup)

;; (setq elfeed-search-trailing-width 23)
;; (setq elfeed-search-title-max-width 190)
;; (defun elfeed-search-format-date (date)
;;   (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))

;; probably temporary: hack for elfeed-goodies date column:
(defun elfeed-goodies/search-header-draw ()
  "Returns the string to be used as the Elfeed header."
  (if (zerop (elfeed-db-last-update))
      (elfeed-search--intro-header)
    (let* ((separator-left (intern (format "powerline-%s-%s"
        elfeed-goodies/powerline-default-separator
        (car powerline-default-separator-dir))))
    (separator-right (intern (format "powerline-%s-%s"
         elfeed-goodies/powerline-default-separator
         (cdr powerline-default-separator-dir))))
    (db-time (seconds-to-time (elfeed-db-last-update)))
    (stats (-elfeed/feed-stats))
    (search-filter (cond
      (elfeed-search-filter-active
       "")
      (elfeed-search-filter
       elfeed-search-filter)
      (""))))
      (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
   (search-header/draw-wide separator-left separator-right search-filter stats db-time)
 (search-header/draw-tight separator-left separator-right search-filter stats db-time)))))

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
(eval-after-load 'elfeed-search
  '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))

(define-key elfeed-show-mode-map (kbd ".") 'visual-fill-column-mode)

(setq httpd-port 8181)

;; Load elfeed
(use-package elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
                                        ;              ("A" . bjm/elfeed-show-all)
                                        ;              ("E" . bjm/elfeed-show-emacs)
                                        ;              ("D" . bjm/elfeed-show-daily)
              ("q" . bjm/elfeed-save-db-and-bury)))

(provide 'elfeed-config)
