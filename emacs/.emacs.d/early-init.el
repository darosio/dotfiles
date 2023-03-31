;;; early-init.el --- Initialization code run before `init.el'

;;; Commentary:
;; This file contains initialization code that is run before `init.el` is loaded.
;; It is used to configure basic Emacs settings that need to be set before loading
;; other packages.

;;; Code:
;; Disable site-wide initialization and default library
(setq site-run-file nil
      inhibit-default-init t)

;; Increase threshold for garbage collector during initialization
(setq gc-cons-threshold most-positive-fixnum)

;; Disable package initialization at startup
(setq package-enable-at-startup nil)

;; Optionally set native-compilation ELN load path
;; (setq native-comp-eln-load-path
;;       (list (expand-file-name "eln-cache" user-emacs-directory)))

(provide 'early-init)
;;; early-init.el ends here
