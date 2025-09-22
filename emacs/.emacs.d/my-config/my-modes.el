;;; my-modes.el --- To use LLM -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; all additional modes
;;
;;; Code:

(use-package markdown-mode            ;;markdownlint-cli ruby-mdl
  ;; :bind (:map markdown-mode-map
  ;;             ("<return>" . nil))
  :init (setq markdown-url-compose-char ?…)
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)
         ("\\.mkd\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package sphinx-mode)

(use-package plantuml-mode
  :after (org)
  :defines org-plantuml-jar-path
  :init
  (setq plantuml-default-exec-mode 'jar
        plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
        org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"))

(use-package graphviz-dot-mode
  :config (setq graphviz-dot-indent-width 4))

(use-package gnuplot)

(use-package ess)

(use-package json-mode
  :config
  (setq js-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.ipynb\\'" . js-mode)))

(use-package ssh-config-mode)

(use-package pkgbuild-mode)

(use-package web-mode
  ;; ;; Use `web-mode' rather than `js-mode' for scripts.
  ;; :interpreter (("js" . web-mode)
  ;;               ("node" . web-mode))
  :config
  ;; Dynamically add file extension associations
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; My additions.
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[cm]?jsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  ;; Set indentations
  ;; Indent by two spaces by default. Compatibility with Prettier.
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  ;; Not sure why anyone would want 1 space indent for inline scripts
  ;; and CSS. Set it to 2 for consistency.
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  ;; Autocomplete </ instantly.
  (setq web-mode-enable-auto-closing t)
  ;; Insert matching tags automatically. Why this is "mode 2", I have
  ;; not the slightest idea.
  (setq web-mode-auto-close-style 2)
  ;; Don't insert quotes automatically. It messes with JSX.
  (setq web-mode-enable-auto-quoting nil)
  ;; Disable `web-mode' automatically reindenting a bunch of
  ;; surrounding code when you paste anything. It's real annoying if
  ;; it happens to not know how to indent your code correctly.
  (setq web-mode-enable-auto-indentation nil)
  )

(use-package vimrc-mode
  :hook (vimrc-mode-hook . (lambda ()
                             (setq-local tab-width 2)
                             (setq-local indent-line-function 'insert-tab))))

(use-package yaml-mode ;; yay -S (yamllint) prettier with apheleia
  :hook ((yaml-mode . turn-off-flyspell))
  :mode "\\.yml\\'")

(use-package toml-mode                ;; For lsp: yay -S taplo
  :mode "\\.toml\\'")

(use-package csv-mode
  :mode (("\\.csv\\'" . csv-mode)))

(use-package dna-mode
  :bind ("C-c t m d" . dna-mode))

(provide 'my-modes)
;;; my-modes.el ends here
