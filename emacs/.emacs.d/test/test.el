;;; test.el --- Emacs smoke tests for dotfiles CI  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Run with: emacs --batch -l ~/.emacs.d/init.el -l ~/.emacs.d/test/test.el
;; Each section requires packages and checks that key functions/variables exist.
;; Failures signal an error immediately so CI catches regressions.
;;
;;; Code:

(defvar test--pass-count 0 "Number of passed checks.")
(defvar test--fail-count 0 "Number of failed checks.")

(defun test--check (description form)
  "Assert FORM is non-nil, printing DESCRIPTION."
  (if form
      (progn
        (setq test--pass-count (1+ test--pass-count))
        (message "  ✅ %s" description))
    (setq test--fail-count (1+ test--fail-count))
    (message "  ❌ %s" description)))

(defmacro test-section (name &rest body)
  "Run BODY under section NAME, catching errors."
  (declare (indent 1))
  `(progn
     (message "\n── %s ──" ,name)
     (condition-case err
         (progn ,@body)
       (error
        (setq test--fail-count (1+ test--fail-count))
        (message "  ❌ Section error: %S" err)))))

;; ═══════════════════════════════════════════════════════════════════════
;; Core completion UI: Vertico + Consult + Embark + Orderless + Marginalia
;; ═══════════════════════════════════════════════════════════════════════

(test-section "Completion UI"
  (require 'vertico)
  (test--check "vertico loaded" (featurep 'vertico))
  (test--check "vertico-mode available" (fboundp 'vertico-mode))

  (require 'consult)
  (test--check "consult loaded" (featurep 'consult))
  (test--check "consult-buffer" (fboundp 'consult-buffer))
  (test--check "consult-ripgrep" (fboundp 'consult-ripgrep))
  (test--check "consult-line" (fboundp 'consult-line))

  (require 'embark)
  (test--check "embark loaded" (featurep 'embark))
  (test--check "embark-act" (fboundp 'embark-act))

  (require 'orderless)
  (test--check "orderless loaded" (featurep 'orderless))

  (require 'marginalia)
  (test--check "marginalia loaded" (featurep 'marginalia)))

;; ═══════════════════════════════════════════════════════════════════════
;; Completion at point: Corfu + Cape
;; ═══════════════════════════════════════════════════════════════════════

(test-section "Corfu / Cape"
  (require 'corfu)
  (test--check "corfu loaded" (featurep 'corfu))
  (test--check "global-corfu-mode" (fboundp 'global-corfu-mode))

  (require 'cape)
  (test--check "cape loaded" (featurep 'cape))
  (test--check "cape-file" (fboundp 'cape-file)))

;; ═══════════════════════════════════════════════════════════════════════
;; UI chrome: which-key, doom-modeline, nerd-icons
;; ═══════════════════════════════════════════════════════════════════════

(test-section "UI chrome"
  (require 'which-key)
  (test--check "which-key loaded" (featurep 'which-key))
  (test--check "which-key-mode" (fboundp 'which-key-mode))

  (require 'doom-modeline)
  (test--check "doom-modeline loaded" (featurep 'doom-modeline))

  (require 'nerd-icons)
  (test--check "nerd-icons loaded" (featurep 'nerd-icons)))

;; ═══════════════════════════════════════════════════════════════════════
;; Org-mode ecosystem
;; ═══════════════════════════════════════════════════════════════════════

(test-section "Org-mode"
  (require 'org)
  (test--check "org loaded" (featurep 'org))
  (test--check "org-version" (fboundp 'org-version))

  ;; Parse a small buffer
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n** Sub\nBody text\n")
    (let ((tree (org-element-parse-buffer)))
      (test--check "org-element-parse-buffer works" tree)))

  ;; Org-roam (feature only — needs sqlite at runtime)
  (require 'org-roam nil t)
  (test--check "org-roam feature" (featurep 'org-roam))
  (test--check "org-roam-node-find" (fboundp 'org-roam-node-find))

  ;; org-modern
  (require 'org-modern nil t)
  (test--check "org-modern loaded" (featurep 'org-modern)))

;; ═══════════════════════════════════════════════════════════════════════
;; Citation: Citar
;; ═══════════════════════════════════════════════════════════════════════

(test-section "Citation (citar)"
  (require 'citar nil t)
  (test--check "citar loaded" (featurep 'citar))
  (test--check "citar-open" (fboundp 'citar-open))

  (require 'oc nil t)
  (test--check "org-cite loaded" (featurep 'oc)))

;; ═══════════════════════════════════════════════════════════════════════
;; Git: Magit + diff-hl + git-timemachine
;; ═══════════════════════════════════════════════════════════════════════

(test-section "Git"
  (require 'magit)
  (test--check "magit loaded" (featurep 'magit))
  (test--check "magit-status" (fboundp 'magit-status))
  (test--check "magit-version" (stringp (magit-version)))

  (require 'diff-hl nil t)
  (test--check "diff-hl loaded" (featurep 'diff-hl))

  (require 'git-timemachine nil t)
  (test--check "git-timemachine loaded" (featurep 'git-timemachine)))

;; ═══════════════════════════════════════════════════════════════════════
;; Dev tools: Eglot + Flymake + envrc + python-pytest
;; ═══════════════════════════════════════════════════════════════════════

(test-section "Dev tools"
  (require 'eglot)
  (test--check "eglot loaded" (featurep 'eglot))
  (test--check "eglot-ensure" (fboundp 'eglot-ensure))

  (require 'flymake)
  (test--check "flymake loaded" (featurep 'flymake))
  (test--check "flymake-mode" (fboundp 'flymake-mode))

  (require 'envrc nil t)
  (test--check "envrc loaded" (featurep 'envrc))
  (test--check "envrc-global-mode" (fboundp 'envrc-global-mode))

  (require 'python-pytest nil t)
  (test--check "python-pytest loaded" (featurep 'python-pytest)))

;; ═══════════════════════════════════════════════════════════════════════
;; Python
;; ═══════════════════════════════════════════════════════════════════════

(test-section "Python"
  (require 'python)
  (test--check "python-mode loaded" (featurep 'python))
  (test--check "python-mode function" (fboundp 'python-mode))

  (require 'numpydoc nil t)
  (test--check "numpydoc loaded" (featurep 'numpydoc)))

;; ═══════════════════════════════════════════════════════════════════════
;; Editing helpers
;; ═══════════════════════════════════════════════════════════════════════

(test-section "Editing helpers"
  (require 'smartparens)
  (test--check "smartparens loaded" (featurep 'smartparens))
  (test--check "smartparens-mode" (fboundp 'smartparens-mode))

  (require 'expand-region nil t)
  (test--check "expand-region loaded" (featurep 'expand-region))
  (test--check "er/expand-region" (fboundp 'er/expand-region))

  (require 'aggressive-indent nil t)
  (test--check "aggressive-indent loaded" (featurep 'aggressive-indent))

  (require 'unfill nil t)
  (test--check "unfill loaded" (featurep 'unfill))

  (require 'move-text nil t)
  (test--check "move-text loaded" (featurep 'move-text))

  (require 'crux nil t)
  (test--check "crux loaded" (featurep 'crux))

  (require 'visual-regexp nil t)
  (test--check "visual-regexp loaded" (featurep 'visual-regexp)))

;; ═══════════════════════════════════════════════════════════════════════
;; Snippets: YASnippet
;; ═══════════════════════════════════════════════════════════════════════

(test-section "Snippets"
  (require 'yasnippet nil t)
  (test--check "yasnippet loaded" (featurep 'yasnippet))
  (test--check "yas-global-mode" (fboundp 'yas-global-mode))

  (require 'yasnippet-snippets nil t)
  (test--check "yasnippet-snippets loaded" (featurep 'yasnippet-snippets)))

;; ═══════════════════════════════════════════════════════════════════════
;; AI: gptel + ellama
;; ═══════════════════════════════════════════════════════════════════════

(test-section "AI"
  (require 'gptel nil t)
  (test--check "gptel loaded" (featurep 'gptel))
  (test--check "gptel-send" (fboundp 'gptel-send))

  (require 'ellama nil t)
  (test--check "ellama loaded" (featurep 'ellama)))

;; ═══════════════════════════════════════════════════════════════════════
;; LaTeX: AUCTeX + cdlatex
;; ═══════════════════════════════════════════════════════════════════════

(test-section "LaTeX"
  (require 'tex nil t)
  (test--check "auctex (tex) loaded" (featurep 'tex))

  (require 'cdlatex nil t)
  (test--check "cdlatex loaded" (featurep 'cdlatex)))

;; ═══════════════════════════════════════════════════════════════════════
;; Prose & Spell
;; ═══════════════════════════════════════════════════════════════════════

(test-section "Prose & Spell"
  (require 'visual-fill-column nil t)
  (test--check "visual-fill-column loaded" (featurep 'visual-fill-column))

  (require 'writegood-mode nil t)
  (test--check "writegood-mode loaded" (featurep 'writegood-mode))

  (require 'powerthesaurus nil t)
  (test--check "powerthesaurus loaded" (featurep 'powerthesaurus)))

;; ═══════════════════════════════════════════════════════════════════════
;; File modes
;; ═══════════════════════════════════════════════════════════════════════

(test-section "File modes"
  (require 'markdown-mode nil t)
  (test--check "markdown-mode loaded" (featurep 'markdown-mode))

  (require 'yaml-mode nil t)
  (test--check "yaml-mode loaded" (featurep 'yaml-mode))

  (require 'json-mode nil t)
  (test--check "json-mode loaded" (featurep 'json-mode))

  (require 'web-mode nil t)
  (test--check "web-mode loaded" (featurep 'web-mode))

  (require 'csv-mode nil t)
  (test--check "csv-mode loaded" (featurep 'csv-mode)))

;; ═══════════════════════════════════════════════════════════════════════
;; Misc utilities
;; ═══════════════════════════════════════════════════════════════════════

(test-section "Misc"
  (require 'hl-todo nil t)
  (test--check "hl-todo loaded" (featurep 'hl-todo))

  (require 'password-store nil t)
  (test--check "password-store loaded" (featurep 'password-store))

  (require 'recentf nil t)
  (test--check "recentf loaded" (featurep 'recentf))

  (require 'wgrep nil t)
  (test--check "wgrep loaded" (featurep 'wgrep))

  (require 'devdocs nil t)
  (test--check "devdocs loaded" (featurep 'devdocs))

  (require 'browse-at-remote nil t)
  (test--check "browse-at-remote loaded" (featurep 'browse-at-remote))

  (require 'apheleia nil t)
  (test--check "apheleia loaded" (featurep 'apheleia)))

;; ═══════════════════════════════════════════════════════════════════════
;; ESS (R / Julia)
;; ═══════════════════════════════════════════════════════════════════════

(test-section "ESS"
  (require 'ess nil t)
  (test--check "ess loaded" (featurep 'ess)))

;; ═══════════════════════════════════════════════════════════════════════
;; Summary
;; ═══════════════════════════════════════════════════════════════════════

(message "\n══════════════════════════════════════")
(message "  Results: %d passed, %d failed" test--pass-count test--fail-count)
(message "══════════════════════════════════════")

(when (> test--fail-count 0)
  (error "%d smoke test(s) FAILED" test--fail-count))

(message "✅ All %d smoke tests passed." test--pass-count)

;;; test.el ends here
