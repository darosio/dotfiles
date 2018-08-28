;; Then inline latex like $y=mx+c$ will appear in a different colour in an
;; org-mode file to help it stand out.
(setq org-highlight-latex-and-related '(latex))
;; don't prompt me to confirm everytime I want to evaluate a block
;; (setq org-confirm-babel-evaluate 'never)
(setq org-confirm-babel-evaluate nil)

(setq org-startup-truncated nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (plantuml . t)
   (awk . t)
   (python . t)
   (ipython . t)
   (gnuplot . t)
   ;; (haskell . t)
   (R . t)
   (ditaa . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   ;; (screen . t)
   (org . t)
   (makefile . t)
   (dot . t)
   ;; (sh . t)
   (shell . t) ;; works only with the provided 'org layer
   ))


(add-to-list 'org-structure-template-alist
             '("m" "#+begin_src emacs-lisp :tangle init.el\n\n#+end_src" "<src lang=\"emacs-lisp\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
             '("r" "#+begin_src R :results output :session *R* :exports both\n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
             '("R" "#+begin_src R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
             '("RR" "#+begin_src R :results output graphics :file  (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
             '("p" "#+begin_src python :session :results both :exports raw drawer\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
             '("P" "#+begin_src python :results output :session *python* :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
             '("pp" "#+begin_src ipython :session :results output :exports raw drawer\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
             '("b" "#+begin_src sh :results output :exports both\n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
             '("B" "#+begin_src sh :session foo :results output :exports both \n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
             '("g" "#+begin_src dot :results output graphics :file \"/tmp/graph.png\" :exports both
   digraph G {
      node [color=black,fillcolor=white,shape=rectangle,style=filled,fontname=\"Helvetica\"];
      A[label=\"A\"]
      B[label=\"B\"]
      A->B
   }\n#+end_src" "<src lang=\"dot\">\n\n</src>"))



;; (defun my/fix-inline-images ()
;;   (when org-inline-image-overlays
;;     (org-redisplay-inline-images)))
;; (add-hook 'org-babel-after-execute-hook 'my/fix-inline-images)

;; ;; display/update images in the buffer after I evaluate
;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)



(provide 'my-org)
