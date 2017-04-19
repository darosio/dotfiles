(defalias 'elfeed-toggle-star
  (elfeed-expose #'elfeed-search-toggle-all 'star))

(eval-after-load 'elfeed-search
  '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))

(provide 'elfeed-config)
