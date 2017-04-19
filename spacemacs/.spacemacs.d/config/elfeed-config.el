;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))
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

(setq httpd-port 8181)

(provide 'elfeed-config)
