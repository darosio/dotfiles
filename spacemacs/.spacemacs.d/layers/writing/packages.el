;;; packages.el --- writing layer packages file for Spacemacs.
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
;; added to `writing-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `writing/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `writing/pre-init-PACKAGE' and/or
;;   `writing/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst writing-packages
  '(langtool  ;; languagetool
    wordnut  ;; wordnet f12 C-f12
    sdcv  ;; sdcv f11
    dictcc ;; f8 but use cmdline trans, please!
    writegood-mode
    artbollocks-mode
    (diction :location local)  ;; diction
    )
  "The list of Lisp packages required by the writing layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun writing/init-langtool ()
  (use-package langtool
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "Sl" 'langtool-check
        ;; "Sl" 'spacemacs/languagetool-toggle
        "SL" 'langtool-correct-buffer)
      (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"
            ;; langtool-default-language "en-US"
            langtool-java-bin "/usr/bin/java"
            langtool-mother-tongue "it"))
    :config
    ;; The whitespace rules give a lot of false positives when linting rich
    ;; text.
    (setq-default langtool-disabled-rules '("WHITESPACE_RULE"))
    (global-set-key "\C-x4W" 'langtool-check-done)
    (global-set-key "\C-x44" 'langtool-show-brief-message-at-point)
    (define-key evil-normal-state-map (kbd "[ a")
      'langtool-goto-previous-error)
    ;; 'spacemacs/languagetool-previous-error)
    (define-key evil-normal-state-map (kbd "] a")
      'langtool-goto-next-error)
    ;; 'spacemacs/languagetool-next-error)
    ))

(defun writing/init-wordnut ()
  (use-package wordnut
    :defer t
    :init
    (global-set-key [f12] 'wordnut-lookup-current-word)
    (global-set-key [(control f12)] 'wordnut-search)
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'wordnut-mode "q" 'quit-window)
      (define-key wordnut-mode-map (kbd "C-q") 'delete-window)
      (spacemacs/set-leader-keys-for-major-mode 'wordnut-mode "RET" 'wordnut-lookup-current-word)
      (spacemacs/set-leader-keys-for-major-mode 'wordnut-mode "l" 'wordnut-history-backward)
      (spacemacs/set-leader-keys-for-major-mode 'wordnut-mode "r" 'wordnut-history-forward)
      (spacemacs/set-leader-keys-for-major-mode 'wordnut-mode "h" 'wordnut-history-lookup)
      (spacemacs/set-leader-keys-for-major-mode 'wordnut-mode "/" 'wordnut-search)
      (spacemacs/set-leader-keys-for-major-mode 'wordnut-mode "o" 'wordnut-show-overview)
      (define-key wordnut-mode-map (kbd "C-j") 'outline-next-visible-heading)
      (define-key wordnut-mode-map (kbd "C-k") 'outline-previous-visible-heading)
      (spacemacs/set-leader-keys-for-major-mode 'wordnut-mode (kbd "b") 'scroll-down-command))
    ))

(defun writing/init-sdcv ()
  (use-package sdcv
    :defer t
    :init
    (global-set-key [f11] 'sdcv-search-pointer)
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'sdcv-mode "q" 'quit-window)
      (define-key sdcv-mode-map (kbd "C-q") 'delete-window)
      (define-key sdcv-mode-map (kbd "C-j") 'sdcv-next-dictionary)
      (define-key sdcv-mode-map (kbd "C-k") 'sdcv-previous-dictionary))
    ))

(defun writing/init-dictcc ()
  (use-package synosaurus
    :defer t
    :init
    (global-set-key [f8] 'dictcc-at-point)
    :config
    (setq dictcc-destination-lang "it")
    ))

(defun writing/init-writegood-mode ()
  (use-package writegood-mode
    :defer t
    :init
    (global-set-key "\C-cg" 'writegood-mode)
    (global-set-key "\C-c\C-gg" 'writegood-grade-level)
    (global-set-key "\C-c\C-ge" 'writegood-reading-ease)
    ))

(defun writing/init-artbollocks-mode ()
  (use-package artbollocks-mode
    :defer t
    :init
    (global-set-key "\C-c\C-ga" 'artbollocks-mode)
    ))

(defun writing/init-diction ()
  (use-package diction
    :defer t
    :init
    (require 'diction)
    (global-set-key "\C-c\C-gd" 'diction-buffer)
    ))

;;; packages.el ends here
