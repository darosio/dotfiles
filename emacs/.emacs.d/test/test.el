;;; test.el --- Emacs smoke test for dotfiles CI

;; Wait for Elpaca to finish processing its queue
(elpaca-wait)

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

;; ;; Basic package health check
;; (defun test-package-health ()
;;   (dolist (pkg '((org . "9.6")
;;                  (magit . "3.3")
;;                  (use-package . "2.4")))
;;     (unless (package-installed-p (car pkg) (cdr pkg))
;;       (error "Package %s not properly installed" (car pkg))))

;;   ;; Core functionality test
;;   (defun test-core-features ()
;;     (require 'org)
;;     (unless (fboundp 'org-version)
;;       (error "Org mode not properly loaded")))

;;   (test-package-health)
;;   (test-core-features)

;;   (message "All tests passed successfully!")
