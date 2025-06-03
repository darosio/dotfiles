;;; my-prose.el --- To write prose -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Binding keys: "<F7>"
;;; Code:

(use-package cm-mode ;; critic markup
  ;; :hook (text-mode . cm-mode)
  :bind
  ("<f7> M" . cm-mode)
  ("<f7> m" . cm-prefix-map))

(use-package langtool
  :commands (langtool-goto-previous-error
             langtool-goto-next-error
             langtool-check
             langtool-correct-buffer
             langtool-check-done
             langtool-switch-default-language)
  :bind ("<f7> l" . my-langtool-transient)
  :config
  (transient-define-prefix my-langtool-transient ()
    "Langtool Commands"
    [["Navigation"
      ("p" "Previous error" langtool-goto-previous-error)
      ("n" "Next error" langtool-goto-next-error)]
     ["Actions"
      ("c" "Check" langtool-check)
      ("b" "Correct buffer" langtool-correct-buffer)
      ("d" "Done" langtool-check-done)]
     ["Configuration"
      ("l" "Switch language" langtool-switch-default-language)]])
  (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"
        langtool-java-bin "/usr/bin/java"
        langtool-disabled-rules '("EN_UNPAIRED_BRACKETS"
                                  "MORFOLOGIK_RULE_EN_US")
        langtool-mother-tongue "it"
        langtool-default-language "en-US"))

(use-package academic-phrases
  :bind
  ("<f7> i" . academic-phrases-by-section)
  ("<f7> I" . academic-phrases))

(use-package writegood-mode
  :bind (("<f7> g" . writegood-mode)
         ("<f7> Gl" . writegood-grade-level)
         ("<f7> Gr" . writegood-reading-ease)))

(provide 'my-prose)
;;; my-prose.el ends here
