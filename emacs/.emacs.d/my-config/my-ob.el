;;; my-ob.el --- To spell words -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Binding keys: "C-c"
;;; Code:

;; (setenv "PYDEVD_DISABLE_FILE_VALIDATION" "1") ; https://github.com/nnicandro/emacs-jupyter/issues/439
(use-package jupyter
  ;; :straight (:no-native-compile t :no-byte-compile t)
  :after (org)
  :init (with-eval-after-load 'org-babel (require 'jupyter))
  :defines org-babel-default-header-args:jupyter-python
  :bind (:map org-mode-map
              ("C-c s" . org-babel-jupyter-scratch-buffer)
              ("C-c <DEL>" . jupyter-org-clear-all-results))
  :config
  (setq jupyter-repl-prompt-margin-width 4)
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py01")
                                                       (:kernel . "python3"))))

;; TODO: https://lafith.net/posts/20250518-emacs-org-mode-jupyter.html

;; ;;------------------------------------------------------------------------------
;; ;; Jupyter / ob-jupyter / ob-async
;; ;;------------------------------------------------------------------------------
;; (when (executable-find "jupyter")
;;   ;; only load jupyter.el if you have Jupyter installed
;;   (use-package jupyter
;;     :after org
;;     :commands (org-babel-jupyter-scratch-buffer
;;                jupyter-org-clear-all-results
;;                jupyter-org-repl)
;;     :init
;;     (with-eval-after-load 'org-babel (require 'jupyter))
;;     :config
;;     (setq jupyter-repl-prompt-margin-width 4)
;;     ;; any other jupyter.el tweaks here
;;     )

;;   ;; now ob-jupyter: this is where we set header‐args
;;   (use-package ob-jupyter
;;     :after org
;;     :config
;;     ;; move your header args here:
;;     (setq org-babel-default-header-args:jupyter-python
;;           '((:async   . "yes")
;;             (:session . "py01")
;;             (:kernel  . "python3")))
;;     ;; if you always want Python to dispatch to Jupyter rather than builtin python:
;;     (org-babel-jupyter-override-src-block "python"))

;;   ;; and your async helper
;;   (use-package ob-async
;;     :after ob
;;     :config
;;     (setq ob-async-no-async-languages-alist
;;           '("jupyter-python" "jupyter-julia" "jupyter-R"))))

(use-package prog-mode
  :straight (:type built-in)
  :config
  (setq-default prettify-symbols-alist '(("#+begin_src" . "➙")
                                         ("#+end_src" . "∎")
                                         ("#+BEGIN_SRC" . "🔂")
                                         ("#+END_SRC" . "⌗")
                                         ("#+RESULTS:" . "💹")
                                         ("=>" . "⇨")))
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; --- Org packages ---
(use-package ox-latex
  :straight nil
  :defer t ;; XXX: https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-LaTeX.html
  :config
  (setq org-latex-remove-logfiles t)
  (add-to-list 'org-latex-logfiles-extensions "_minted-")
  (add-to-list 'org-latex-logfiles-extensions "bbl")
  (add-to-list 'org-latex-logfiles-extensions "pyg")
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='xelatex -shell-escape -interaction nonstopmode' -pdf -bibtex -f %f -outdir=%o -verbose"
          "latexmk -c %b"))
  ;; Use the 'minted' package instead of 'listings' for code blocks
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (setq org-latex-minted-options
        '(("frame" "lines") ("autogobble") ("breaklines" "true")
          ;; ("fontsize" "\\small") ("baselinestretch" "1.2") ("no-message") ("mathescape") ("tabsize" "4")
          ;; ("breakanywhere" "true") ("numbersep" "0.5cm") ("xleftmargin" "1cm") ("frame" "single")("linenos")
          ("style" "colorful")))
  (add-to-list 'org-latex-classes
               '("draft" "\\documentclass[12pt]{article}
                     \\usepackage{setspace}
                     \\usepackage{tocloft}
                     \\usepackage{lineno}
                     \\usepackage{mathptmx}
                     \\tolerance=1000
                     \\setlength{\\parskip}{4pt}
                     \\setlength{\\parindent}{0pt}
                     \\linespread{1.2}
                     \\usepackage[left=1in, right=1in, top=1in, bottom=1in]{geometry}
                     \\renewcommand{\\cftsecleader}{\\cftdotfill{\\cftdotsep}}
                     \\usepackage{fancyhdr}
                     \\date{}
                     \\pagestyle{fancy}
                     \\fancyhf{}
                     \\rhead{Draft version: \\today}
                     \\cfoot{\\thepage}
                     [NO-DEFAULT-PACKAGES]
                     [PACKAGES]
                     [EXTRA]
                     "
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

(use-package ox-beamer :straight org :init (eval-after-load 'ox '(require 'ox-beamer)))
(use-package ox-md :straight org :init (eval-after-load 'ox '(require 'ox-md)))
(use-package ox-koma-letter :straight org :init (eval-after-load 'ox '(require 'ox-koma-letter)))
(use-package ob-ditaa :straight org
  :after org
  :config
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
  )

;; --- Other packages ---
(use-package ob-async
  :after (ob)
  :config
  (setq ob-async-no-async-languages-alist
        '("jupyter-python" "jupyter-julia" "jupyter-R")))

;; https://opensource.com/article/18/2/org-mode-slides
(use-package ox-reveal
  :defines org-reveal-quiet
  :config
  (setq org-reveal-root "file:///home/dan/workspace/examples/reveal.js/")
  (setq org-reveal-theme "night")
  (setq org-reveal-quiet t);TODO: consider org-(re)-reveal
  ;; (setq org-reveal-css "path/to/your/custom/css")
  :init
  (eval-after-load 'ox '(require 'ox-reveal))
  )

(use-package ox-rst :init (eval-after-load 'ox '(require 'ox-rst)))
(use-package ox-pandoc :init (eval-after-load 'ox '(require 'ox-pandoc)))
(use-package ox-twbs :init (eval-after-load 'ox '(require 'ox-twbs)))
(use-package cdlatex)

(use-package auctex
  :defer t
  :mode
  ("\\.tex\\'" . TeX-latex-mode)
  ("\\.latex\\'" . TeX-latex-mode)
  :config
  (use-package tex
    :straight auctex
    :config
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    ;; (setq TeX-PDF-mode t))
    (setq-default TeX-master nil))
  (use-package tex-buf
    :straight auctex
    :config
    (setq TeX-save-query nil))
  )

(provide 'my-ob)
;;; my-ob.el ends here
