;;; my-spell.el --- To spell words -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Binding keys: "<F7>"
;;; Code:

(use-package ispell
  :hook ((text-mode-hook . flyspell-mode)
         (org-mode-hook . flyspell-mode)
         (prog-mode-hook . flyspell-prog-mode)
         (LaTeX-mode-hook . flyspell-mode)
         (change-log-mode-hook . (lambda () (flyspell-mode -1)))
         (log-edit-mode-hook . (lambda () (flyspell-mode -1))))
  :config
  (setq ispell-program-name (executable-find "hunspell")
        ispell-really-hunspell t
        ispell-silently-savep t       ; Save personal dictionary silently
        ispell-dictionary "en_US-large")
  (add-to-list 'ispell-skip-region-alist '("^From:" . "line--$")))

(use-package flyspell
  :after ispell
  :hook ((text-mode-hook . flyspell-mode)
         (org-mode-hook . flyspell-mode)
         (prog-mode-hook . flyspell-prog-mode)
         (LaTeX-mode-hook . flyspell-mode)
         (change-log-mode-hook . (lambda () (flyspell-mode -1)))
         (log-edit-mode-hook . (lambda () (flyspell-mode -1))))
  :bind (("C-c t s" . flyspell-mode)
         ("C-c t S" . flyspell-correct-auto-mode)
         :map flyspell-mode-map
         ("C-M-s-," . flyspell-auto-correct-previous-word) ; I mostly use flyspell-correct
         ("C-;" . nil)                ; to avoid conflicts
         ("C-." . nil)
         ("C-," . nil)))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("<f7> s" . flyspell-correct-wrapper)))

(use-package consult-flyspell
  :after flyspell
  :bind (("M-g s" . consult-flyspell)))

(use-package guess-language
  ;; :hook (flyspell-mode . guess-language-mode)
  :init
  (which-key-add-key-based-replacements "C-c S" "Spell")
  :bind (("C-c t g" . guess-language-mode)
         ("C-c S e" . (lambda () (interactive)
                        (ispell-change-dictionary "en_US-large")
                        (flyspell-buffer)))
         ("C-c S i" . (lambda () (interactive)
                        (ispell-change-dictionary "it_IT")
                        (flyspell-buffer))))
  :config
  (setq guess-language-langcodes '((en . ("en_US-large" "English"))
                                   (it . ("it_IT" "Italian")))
        guess-language-min-paragraph-length 15
        guess-language-languages '(en it)))

(use-package sdcv
  :bind
  ("<f7> S" . sdcv-search-pointer)
  (:map sdcv-mode-map
        ("n" . sdcv-next-dictionary)
        ("p" . sdcv-previous-dictionary)))

(use-package wordnut
  :bind
  ("<f7> w" . wordnut-lookup-current-word)
  ("<f7> W" . wordnut-search))

(use-package powerthesaurus
  :bind
  ("<f7> p 0" . powerthesaurus-lookup-dwim)
  ("<f7> p p" . powerthesaurus-lookup-synonyms-dwim)
  ("<f7> p a" . powerthesaurus-lookup-antonyms-dwim)
  ("<f7> p d" . powerthesaurus-lookup-definitions-dwim)
  ("<f7> p r" . powerthesaurus-lookup-related-dwim)
  ("<f7> p s" . powerthesaurus-lookup-sentences-dwim))

(defun my/translate-region (&optional prompt-lang)
  "Translate selected region using `trans -j -b`.
If PROMPT-LANG (or called with `C-u`), prompt for language pair.
Default is English to Italian (it:en)."
  (interactive "P")
  (if (use-region-p)
      (let ((lang-pair (if prompt-lang
                           (read-string "Translate (e.g., it:en): ")
                         "it:en")))
        (shell-command-on-region (region-beginning) (region-end)
                                 (format "trans -j -b %s" lang-pair)
                                 "*Translate Output*" t))
    (message "No region selected.")))

(global-set-key (kbd "<f7> t") #'my/translate-region)

(provide 'my-spell)
;;; my-spell.el ends here
