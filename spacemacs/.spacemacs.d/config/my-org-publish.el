(provide 'my-org-publish)

;; activate koma scripts export
(eval-after-load 'ox '(require 'ox-koma-letter))

;; Export from org to latex with bibliography
(setq org-export-latex-hyperref-format "\\ref{%s}")

(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
;; ;;   "xelatex -interaction nonstopmode %f"
;; ;; or as suggested by org-ref
;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;; (require 'ox-latex)
;; (add-to-list 'org-latex-classes
;;              '("koma-article"
;;                "\\documentclass{scrartcl}
;;                 \\usepackage{microtype}
;;                 \\usepackage{tgtermes}
;;                 \\usepackage[scale=.9]{tgheros}
;;                 \\usepackage{tgcursor}
;;                 \\usepackage{paralist}
;;                 \\newcommand{\\rc}{$^{14}C$}"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; (require 'ox-beamer)
