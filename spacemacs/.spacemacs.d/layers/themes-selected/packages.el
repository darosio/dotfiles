;;; packages.el --- Themes selected Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq themes-selected-packages
  '(
                                      subatomic256-theme
                                      spacegray-theme
                                      zen-and-art-theme
                                      material-theme
                                      smyx-theme
                                      afternoon-theme
                                      ample-zen-theme
                                      color-theme-sanityinc-tomorrow
                                      color-theme-sanityinc-solarized
                                      solarized-theme
                                      tango-plus-theme
                                      tangotango-theme
                                      soft-stone-theme
                                      soft-morning-theme
                                      badwolf-theme
                                      doom-themes
                                      molokai-theme
                                      monokai-theme
                                      flatui-theme
                                      sublime-themes
                                      borland-blue-theme
                                      twilight-bright-theme
                                      spaceline-all-the-icons
                                      atom-dark-theme
                                      darcula-theme
                                      theme-changer
                                      idea-darkula-theme
                                      blackboard-theme
                                      xresources-theme
                                      intellij-theme
    ))

;; define programmatically the init functions
(dolist (pkg themes-selected-packages)
  (eval `(defun ,(intern (format "themes-selected/init-%S" pkg)) nil)))
