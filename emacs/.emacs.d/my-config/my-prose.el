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


;; --- Grant writing tools ---
;;
;; Interactive functions for grant proposal literature research via gptel + MCP.
;; Each function connects the SearxNG and fetcher MCP tools, then fires an async
;; gptel-request using the 'proposal' system directive.  Results are collected
;; in a dedicated '*Grant: TOPIC*' org buffer so they don't pollute the current
;; buffer.
;;
;; Entry point: `<f7> R'  (my/grant-transient)
;; Direct commands: gptel-grant-sota / gap / innovation / feasibility / all
;; Citation helper: my/doi-to-bibtex  (<f7> R d)

(defun gptel--grant-search (topic template title)
  "Run a grant-oriented literature search and write a proposal section.

TOPIC is the research subject string (e.g. \"chloride biosensors\").
TEMPLATE is a printf format string for the Boolean search query, with %s
replaced by TOPIC.  TITLE is the section heading inserted into the output
buffer and used as the LLM instruction.

Connects the SearxNG and fetcher MCP servers synchronously so that
`searxng_web_search' and `fetch_url' are available, then sends an async
`gptel-request' using the `proposal' system directive (defined in
`gptel-directives').  The model is pinned to qwen3.5:35b-a3b (Ollama) —
or ministral-3:latest on whisker — regardless of the active preset, ensuring
tool support even when brainstorm/reasoning presets are active.
The response is appended as an org heading in the buffer \\='*Grant: TOPIC*\\='.

See also: `gptel-grant-sota', `gptel-grant-gap', `gptel-grant-innovation',
`gptel-grant-feasibility', `gptel-grant-all'."
  (require 'gptel)
  (gptel-mcp-connect '("searxng" "fetcher") 'sync)
  (let* ((gptel-backend (gptel-get-backend "Ollama"))
         (gptel-model   (if (string= (system-name) "whisker")
                            'ministral-3:latest
                          'qwen3.5:35b-a3b))
         (query  (format template topic))
         (prompt (format
                  "Use searxng_web_search to find recent peer-reviewed literature for:\n\n%s\n\n\
Write a \"%s\" section for a scientific grant proposal (ERC/PRIN style).\n\
Include 3-5 bullet points with inline citations (AuthorYear, Journal).\n\
Flag speculative claims.  Prefer reviews and primary literature from the last 5 years."
                  query title))
         (buf    (get-buffer-create (format "*Grant: %s*" topic))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode) (org-mode))
      (goto-char (point-max))
      (unless (= (point) (point-min)) (insert "\n"))
      (insert (format "* %s\n\n" title)))
    (gptel-request prompt
      :system (alist-get 'proposal gptel-directives)
      :buffer buf
      :callback (lambda (response info)
                  (when (stringp response)
                    (with-current-buffer (plist-get info :buffer)
                      (goto-char (point-max))
                      (insert response "\n")
                      (pop-to-buffer (current-buffer))))))
    (pop-to-buffer buf)))

(defun gptel-grant-sota (topic)
  "Search and write a State of the Art section for a grant proposal on TOPIC.

Queries for recent review articles and overviews, then synthesises a
grant-ready section with inline citations via gptel + SearxNG MCP.
Output is appended to \\='*Grant: TOPIC*\\=' in `org-mode'."
  (interactive "sGrant topic: ")
  (gptel--grant-search
   topic
   "\"recent advances\" AND \"%s\" AND (review OR overview)"
   "State of the Art"))

(defun gptel-grant-gap (topic)
  "Search and write a Current Limitations and Gaps section for TOPIC.

Queries for limitation/challenge/unmet-need literature and synthesises
a gap-analysis section highlighting what is missing in the field."
  (interactive "sGrant topic: ")
  (gptel--grant-search
   topic
   "\"%s\" AND (limitation OR challenge OR unmet) review"
   "Current Limitations and Gaps"))

(defun gptel-grant-innovation (topic)
  "Search and write an Innovation and Novelty section for TOPIC.

Queries for novel approaches and first demonstrations to position
the proposed work as genuinely innovative."
  (interactive "sGrant topic: ")
  (gptel--grant-search
   topic
   "\"%s\" AND (\"novel approach\" OR \"new method\" OR \"first demonstration\")"
   "Innovation and Novelty"))

(defun gptel-grant-feasibility (topic)
  "Search and write a Feasibility and Preliminary Evidence section for TOPIC.

Queries for proof-of-concept and validation studies to demonstrate
that the proposed approach is technically feasible."
  (interactive "sGrant topic: ")
  (gptel--grant-search
   topic
   "\"%s\" AND (\"proof-of-concept\" OR validation OR demonstrated)"
   "Feasibility and Preliminary Evidence"))

(defun gptel-grant-funding (topic)
  "Search and write a Funding Relevance section for TOPIC.

Queries for translational, clinical, and scalable work to demonstrate
alignment with EU (ERC/Horizon) and NIH funding priorities."
  (interactive "sGrant topic: ")
  (gptel--grant-search
   topic
   "\"%s\" AND (translational OR clinical OR scalable OR \"in vivo\")"
   "Funding Relevance"))

(defun gptel-grant-all (topic)
  "Run all five grant sections for TOPIC into a single org buffer.

Fires sota, gap, innovation, feasibility, and funding concurrently.
Responses are appended to \\='*Grant: TOPIC*\\=' as they complete."
  (interactive "sGrant topic: ")
  (gptel--grant-search topic "\"recent advances\" AND \"%s\" AND (review OR overview)"
                       "State of the Art")
  (gptel--grant-search topic "\"%s\" AND (limitation OR challenge OR unmet) review"
                       "Current Limitations and Gaps")
  (gptel--grant-search topic "\"%s\" AND (\"novel approach\" OR \"new method\" OR \"first demonstration\")"
                       "Innovation and Novelty")
  (gptel--grant-search topic "\"%s\" AND (\"proof-of-concept\" OR validation OR demonstrated)"
                       "Feasibility and Preliminary Evidence")
  (gptel--grant-search topic "\"%s\" AND (translational OR clinical OR scalable OR \"in vivo\")"
                       "Funding Relevance"))

;; --- org-cite helpers ---

(defun gptel-org-insert-citations ()
  "Normalise LLM-generated inline citations to org-cite format.

Converts patterns like (Smith2023) or (Smith2023, Jones2024) that look
like author-year citations into \\='[cite:@Smith2023]\\=' org-cite links.
Only matches tokens of the form CapitalWord+4digits to avoid clobbering
footnotes, numeric references, or prose brackets."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Match (AuthorYear) or (AuthorYear, AuthorYear, ...)
    (while (re-search-forward
            "(\\([A-Z][a-zA-Z]+[0-9]\\{4\\}\\(?:,[ ]*[A-Z][a-zA-Z]+[0-9]\\{4\\}\\)*\\))"
            nil t)
      (let* ((keys (match-string 1))
             (cite (mapconcat (lambda (k) (concat "@" (string-trim k)))
                              (split-string keys ",")
                              ";")))
        (replace-match (format "[cite:%s]" cite))))))

;; --- DOI -> BibTeX ---

(defun my/doi-to-bibtex (doi)
  "Fetch BibTeX for DOI from doi.org and copy to kill-ring.

DOI may be a bare identifier (10.1016/...) or a full doi.org URL.
The BibTeX entry is copied to the kill-ring for pasting into Zotero
via Add Item by Identifier (Ctrl+Shift+N), which is the correct way
to add entries — main.bib is auto-generated by Zotero and must not
be edited directly."
  (interactive "sDOI or URL: ")
  (let* ((doi    (string-trim doi))
         (doi    (replace-regexp-in-string
                  "https?://\\(?:dx\\.\\)?doi\\.org/" "" doi))
         (url    (format "https://doi.org/%s" doi))
         (bibtex (string-trim
                  (shell-command-to-string
                   (format "curl -sL -H 'Accept: application/x-bibtex' %s"
                           (shell-quote-argument url))))))
    (if (not (string-match-p "^@" bibtex))
        (message "Could not fetch BibTeX for: %s" doi)
      (kill-new bibtex)
      (message "BibTeX copied — add DOI to Zotero (Ctrl+Shift+N) to update main.bib"))))

(transient-define-prefix my/grant-transient ()
  "Grant writing and literature tools.

Each command fires an async gptel request (SearxNG + fetcher MCP) and
collects results in a dedicated *Grant: TOPIC* org buffer."
  [["Literature Search (gptel + MCP)"
    ("s" "State of the Art"        gptel-grant-sota)
    ("g" "Gaps & Limitations"      gptel-grant-gap)
    ("i" "Innovation & Novelty"    gptel-grant-innovation)
    ("f" "Feasibility & Precedent" gptel-grant-feasibility)
    ("r" "Funding Relevance"       gptel-grant-funding)
    ("a" "All five sections"       gptel-grant-all)]
   ["Citations"
    ("d" "DOI -> BibTeX"           my/doi-to-bibtex)
    ("c" "Normalise -> org-cite"   gptel-org-insert-citations)]])

(global-set-key (kbd "<f7> R") #'my/grant-transient)

(provide 'my-prose)
;;; my-prose.el ends here
