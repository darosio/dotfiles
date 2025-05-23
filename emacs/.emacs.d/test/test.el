;;; test.el --- Emacs smoke test for dotfiles CI

;; load core packages
;; (require 'org)
(require 'magit)
(require 'embark)
(require 'project)

;; test org-mode
(with-temp-buffer
  (org-mode)
  (insert "* Test")
  (org-element-parse-buffer))

;; test magit
(magit-version)

(message "✅ All Emacs smoke tests passed.")
