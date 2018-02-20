;;; packages.el --- my-mu4e layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Daniele Arosio <daniele.arosio@cnr.it>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-mu4e-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-mu4e/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-mu4e/pre-init-PACKAGE' and/or
;;   `my-mu4e/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-mu4e-packages
  '(helm-mu)
  )


(defun my-mu4e/pre-init-helm-mu ()
  (use-package helm-mu
    :defer t
    :commands helm-mu

    :init
    (progn
      ;; (setq mu4e-maildir "~/Sync/Maildir"
      ;;       mu4e-account-alist t
      ;;       ;; mode-line notifications about new messages
      ;;       mu4e-enable-mode-line t)
      ;; (push "~/.spacemacs.d/config/" load-path)
      (dolist (m '(mu4e-main-mode mu4e-headers-mode mu4e-view-mode mu4e-compose-mode))
        (spacemacs/set-leader-keys-for-major-mode m 
          "S" 'helm-mu
          "/" 'helm-mu
          "C" 'helm-mu-contacts))
      )
    ))

(defun my-mu4e/init-helm-mu ())

;; (defun my-mu4e/pre-init-mu4e ()
;;   ;; (spacemacs|use-package-add-hook mu4e
;;     ;; :post-config
;;     (progn
;;       (push "~/.spacemacs.d/config/" load-path)
;;         (require 'mu4e-config)
;;       ;; (setq mu4e-maildir "~/Sync/Maildir"
;;       ;;       mu4e-account-alist t
;;       ;;       ;; mode-line notifications about new messages
;;       ;;       mu4e-enable-mode-line t)
;;       ))
  ;; )

;;; packages.el ends here
