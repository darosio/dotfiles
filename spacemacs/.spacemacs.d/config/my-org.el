(provide 'my-org)
;; Then inline latex like $y=mx+c$ will appear in a different colour in an
;; org-mode file to help it stand out.
(setq org-highlight-latex-and-related '(latex))

;; don't prompt me to confirm everytime I want to evaluate a block
;; (setq org-confirm-babel-evaluate 'never)
(setq org-confirm-babel-evaluate nil)

;; ;; fill wrap long line, needed for org headings?
;; (setq org-startup-truncated nil)

;; split line for new heading, item or row?
(setq org-M-RET-may-split-line '((default . t)
                                 (headline . nil)
                                 (item . nil)
                                 (table . nil)))

(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
(org-babel-do-load-languages 'org-babel-load-languages '(
                                                         (plantuml . t)
                                                         (awk . t)
                                                         (python . t)
                                                         (ipython . t)
                                                         (gnuplot . t)
                                                         ;; (haskell . t)
                                                         (R . t)
                                                         (ditaa . t)
                                                         ;; (octave . t)
                                                         ;; (sqlite . t)
                                                         ;; (perl . t)
                                                         ;; (screen . t)
                                                         ;; (org . t)
                                                         (makefile . t)
                                                         (dot . t)
                                                         ;; (sh . t)
                                                         (shell . t) ;; works only with the provided 'org layer
                                                         ))

;; ob-ipython
;;in virtualenv install importmagic epc
(global-company-mode)
(add-to-list 'company-backends 'company-ob-ipython)
(spacemacs/set-leader-keys "h i" 'ob-ipython-inspect)
;; fix inferior ipython startup warning
(setq python-shell-completion-native-enable nil)
;; ;; not sure I need this
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i --simple-prompt")
;; (setq python-shell-interpreter "jupyter-console"
;;       python-shell-interpreter-args "--existing")
;; Fix an incompatibility between the ob-async and ob-ipython packages
(setq ob-async-no-async-languages-alist '("ipython"))
(setq org-src-window-setup 'current-window) ;;Try
;;Try (setq org-src-lang-modes '(("ipython" . python)))
;; (require 'color) ;; This is somehow obsolete.
;; (set-face-attribute 'org-block nil :background
;;                     (color-darken-name
;;                      (face-attribute 'default :background) 3))
;; (setq org-src-block-faces '(("emacs-lisp" (:background "#EEE2FF"))
;;                             ("ipython" (:background "#E5FFB8"))))
;; ;; ein
;; (setq ein:jupyter-default-server-command "jupyter-notebook"
;;       ein:jupyter-default-notebook-directory "~/Sync/"
;;       ein:use-auto-complete-superpack t)
;;       ;; ein:use-smartrep t)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("r" . "src R :results output :session *R* :exports both"))
(require 'org-tempo)
;; (add-to-list 'org-structure-template-alist
;;              '("m" "#+BEGIN_SRC emacs-lisp :tangle init.el\n\n#+END_SRC" "<src lang=\"emacs-lisp\">\n\n</src>"))

;; besides =C-c C-,=
;; (add-to-list 'org-structure-template-alist
;;              '("R" "#+BEGIN_SRC R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+END_SRC" "<src lang=\"R\">\n\n</src>"))
(add-to-list 'org-structure-template-alist '("R"  . "src R :results output :session *R* :exports both :results output graphics :width 600 :height 400 :file (org-babel-temp-file \"figure\" \".png\")"))
(add-to-list 'org-structure-template-alist '("rg" . "src R :results output :session *R* :exports both :results output graphics :width 600 :height 400 :file (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\")"))

(add-to-list 'org-structure-template-alist
             '("p" . "src python :session :results both :exports raw drawer \n"))

(add-to-list 'org-structure-template-alist
             '("P" . "src python :results output :session *python* :exports both \n"))

(add-to-list 'org-structure-template-alist
             '("ip" . "src ipython :session :results output :exports raw drawer \n"))

;; (add-to-list 'org-structure-template-alist
;;              '("b" "#+BEGIN_SRC sh :results output :exports both\n\n#+END_SRC" "<src lang=\"sh\">\n\n</src>"))

;; (add-to-list 'org-structure-template-alist
;;              '("B" "#+BEGIN_SRC sh :session foo :results output :exports both \n\n#+END_SRC" "<src lang=\"sh\">\n\n</src>"))

;; (add-to-list 'org-structure-template-alist
;;              '("g" "#+BEGIN_SRC dot :results output graphics :file \"/tmp/graph.png\" :exports both
;;    digraph G {
;;       node [color=black,fillcolor=white,shape=rectangle,style=filled,fontname=\"Helvetica\"];
;;       A[label=\"A\"]
;;       B[label=\"B\"]
;;       A->B
;;    }\n#+END_SRC" "<src lang=\"dot\">\n\n</src>"))

;; (defun my/fix-inline-images ()
;;   (when org-inline-image-overlays
;;     (org-redisplay-inline-images)))
;; (add-hook 'org-babel-after-execute-hook 'my/fix-inline-images)

;; ;; display/update images in the buffer after I evaluate
;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
