;;; gptel-mcp-scientific.el --- Scientific search integration for gptel + MCP -*- lexical-binding: t; -*-

;; Author: Dan
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (gptel "0.9"))
;; Keywords: ai, search, research, mcp
;; URL: https://github.com/your-repo/gptel-mcp-scientific

;;; Commentary:
;;
;; This package provides scientific literature search integration for gptel
;; using MCP servers (mcp-searxng and perplexica-mcp).
;;
;; Features:
;; - Grant writing helper functions (SoTA, gap analysis, methods)
;; - Automatic org-cite citation normalization
;; - Pre-configured MCP server setup for scientific workflows
;;
;; Usage:
;;   (require 'gptel-mcp-scientific)
;;   (gptel-mcp-scientific-setup)
;;
;;; Code:

(require 'gptel)

;;; Custom variables

(defgroup gptel-mcp-scientific nil
  "Scientific search integration for gptel."
  :group 'gptel
  :prefix "gptel-mcp-scientific-")

(defcustom gptel-mcp-scientific-searxng-url "http://localhost:8080"
  "URL of the SearXNG instance."
  :type 'string
  :group 'gptel-mcp-scientific)

(defcustom gptel-mcp-scientific-perplexica-url "http://localhost:3000/api/search"
  "URL of the Perplexica backend API."
  :type 'string
  :group 'gptel-mcp-scientific)

(defcustom gptel-mcp-scientific-ollama-model "qwen3"
  "Default Ollama model for Perplexica."
  :type 'string
  :group 'gptel-mcp-scientific)

(defcustom gptel-mcp-scientific-embedding-model "nomic-embed-text"
  "Default embedding model for Perplexica."
  :type 'string
  :group 'gptel-mcp-scientific)

;;; MCP Server Configuration

(defun gptel-mcp-scientific-setup ()
  "Configure gptel MCP servers for scientific workflows."
  (interactive)
  (setq gptel-mcp-servers
        `((searxng
           :command ("npx" "-y" "mcp-searxng")
           :env (("SEARXNG_URL" . ,gptel-mcp-scientific-searxng-url))
           :description "Web search via SearXNG")
          (perplexica
           :command ("uvx" "perplexica-mcp" "stdio")
           :env (("PERPLEXICA_BACKEND_URL" . ,gptel-mcp-scientific-perplexica-url)
                 ("PERPLEXICA_CHAT_MODEL_PROVIDER" . "ollama")
                 ("PERPLEXICA_CHAT_MODEL_NAME" . ,gptel-mcp-scientific-ollama-model)
                 ("PERPLEXICA_EMBEDDING_MODEL_PROVIDER" . "ollama")
                 ("PERPLEXICA_EMBEDDING_MODEL_NAME" . ,gptel-mcp-scientific-embedding-model))
           :description "AI-powered search via Perplexica")))
  (message "gptel MCP servers configured for scientific workflows"))

(defun gptel-mcp-scientific-setup-containers ()
  "Configure gptel MCP servers for containerized (Podman/Docker) setup."
  (interactive)
  (setq gptel-mcp-servers
        '((searxng
           :command ("podman" "exec" "-i" "mcp-searxng" "node" "/app/build/index.js")
           :description "Web search via SearXNG (container)")
          (perplexica
           :command ("podman" "exec" "-i" "perplexica-mcp" "python" "-m" "perplexica_mcp" "stdio")
           :description "AI-powered search via Perplexica (container)")))
  (message "gptel MCP servers configured for container setup"))

;;; System Prompts

(defconst gptel-mcp-scientific-system-prompt
  "You are assisting with scientific grant writing and literature review.

SEARCH BEHAVIOR:
- Use the search tool with focus_mode='academicSearch' for scientific queries
- Prefer sources from: PubMed, arXiv, Google Scholar, Semantic Scholar, CrossRef
- For state-of-the-art queries, include \"recent advances\" or \"review\" in search

CITATION FORMAT:
- Use org-cite format for citations: [cite:@AuthorYear]
- Include DOI when available
- Never invent citations - only cite sources from search results

OUTPUT STRUCTURE:
- Use structured sections with clear headings
- Include bullet points for key findings
- End with a \"Key References\" section listing all cited sources"
  "System prompt for scientific literature synthesis.")

(defun gptel-mcp-scientific-set-prompt ()
  "Set the scientific research system prompt."
  (interactive)
  (setq gptel-default-system-message gptel-mcp-scientific-system-prompt)
  (message "Scientific system prompt activated"))

;;; Grant Writing Functions

(defun gptel-mcp-scientific--extract-text (response)
  "Extract text content from RESPONSE, handling various formats.
RESPONSE may be a string, a cons cell (key . value), or an alist."
  (cond
   ;; Plain string
   ((stringp response) response)
   ;; Dotted pair like (reasoning . "text") - extract the cdr
   ((and (consp response)
         (symbolp (car response))
         (stringp (cdr response)))
    (cdr response))
   ;; Alist with 'content key
   ((and (listp response) (assoc 'content response))
    (gptel-mcp-scientific--extract-text (cdr (assoc 'content response))))
   ;; Alist with 'text key
   ((and (listp response) (assoc 'text response))
    (gptel-mcp-scientific--extract-text (cdr (assoc 'text response))))
   ;; Alist with 'message key
   ((and (listp response) (assoc 'message response))
    (gptel-mcp-scientific--extract-text (cdr (assoc 'message response))))
   ;; List - try to find first string
   ((listp response)
    (or (cl-find-if #'stringp response)
        (format "%S" response)))
   ;; Fallback
   (t (format "%S" response))))

(defun gptel-mcp-scientific--send-request (prompt)
  "Send PROMPT to gptel and insert response at point."
  (gptel-request prompt
    :callback (lambda (response info)
                (if (not response)
                    (message "gptel request failed: %s" (plist-get info :status))
                  (let ((text (gptel-mcp-scientific--extract-text response)))
                    (with-current-buffer (plist-get info :buffer)
                      (save-excursion
                        (goto-char (point-max))
                        (insert "\n\n" text))))))))

(defun gptel-search-sota (topic)
  "Search state-of-the-art on TOPIC and generate grant section."
  (interactive "sGrant topic: ")
  (gptel-mcp-scientific--send-request
   (format "Use search with focus_mode='academicSearch' and query:
\"recent advances\" AND \"%s\" AND (review OR overview)

Write a \"State of the Art\" section suitable for a grant proposal.
Requirements:
- 3-5 paragraphs summarizing the field
- Identify 2-3 major research directions
- Highlight remaining challenges/gaps
- Use [cite:@AuthorYear] format for all claims
- End with transition to proposed innovation" topic)))

(defun gptel-search-gaps (topic)
  "Find research gaps on TOPIC for grant justification."
  (interactive "sResearch topic: ")
  (gptel-mcp-scientific--send-request
   (format "Use search with focus_mode='academicSearch' and query:
\"%s\" AND (limitation OR challenge OR drawback OR unmet OR \"future work\")

Analyze gaps in the current literature:
- List 3-5 specific limitations of current approaches
- For each limitation, cite the source acknowledging it
- Frame gaps as opportunities for proposed research
- Use [cite:@AuthorYear] format" topic)))

(defun gptel-search-methods (topic)
  "Search methodology and techniques for TOPIC."
  (interactive "sMethodology topic: ")
  (gptel-mcp-scientific--send-request
   (format "Use search with focus_mode='academicSearch' and query:
\"%s\" AND (method OR protocol OR technique OR procedure)

Summarize common methodological approaches:
- Group by technique type
- Include validation strategies
- Note limitations of each approach
- Cite sources with [cite:@AuthorYear]" topic)))

(defun gptel-search-feasibility (topic)
  "Search feasibility and preliminary evidence for TOPIC."
  (interactive "sFeasibility topic: ")
  (gptel-mcp-scientific--send-request
   (format "Use search with focus_mode='academicSearch' and query:
\"%s\" AND (proof-of-concept OR validation OR demonstrated OR feasibility)

Write a \"Feasibility and Preliminary Evidence\" section:
- Cite prior validations of the approach
- Highlight robustness and reproducibility
- Note scalability considerations
- Use [cite:@AuthorYear] format" topic)))

(defun gptel-search-innovation (topic)
  "Search innovation positioning for TOPIC."
  (interactive "sInnovation topic: ")
  (gptel-mcp-scientific--send-request
   (format "Use search with focus_mode='academicSearch' and query:
\"%s\" AND (\"novel approach\" OR \"new method\" OR \"first demonstration\" OR breakthrough)

Write an \"Innovation and Novelty\" section:
- Explain why current approaches fall short
- Position the innovation against existing work
- Highlight unique advantages
- Use [cite:@AuthorYear] format" topic)))

;;; Integrated Grant Project Template

(defconst gptel-grant-template
  "* Grant Proposal: %s
:PROPERTIES:
:TOPIC: %s
:CREATED: %s
:END:

** State of the Art
#+BEGIN_AI sota
Use search with focus_mode='academicSearch' and query:
\"recent advances\" AND \"%s\" AND (review OR overview)

Write a \"State of the Art\" section (3-5 paragraphs):
- Summarize the current field
- Identify 2-3 major research directions
- Highlight remaining challenges/gaps
- For each claim, include the source URL in parentheses
- End with transition to proposed innovation
#+END_AI

** Current Limitations and Gaps
#+BEGIN_AI gaps
Use search with focus_mode='academicSearch' and query:
\"%s\" AND (limitation OR challenge OR drawback OR unmet OR \"future work\")

Analyze gaps in the current literature:
- List 3-5 specific limitations of current approaches
- For each limitation, cite the source with URL
- Frame gaps as opportunities for proposed research
#+END_AI

** Innovation and Novelty
#+BEGIN_AI innovation
Use search with focus_mode='academicSearch' and query:
\"%s\" AND (\"novel approach\" OR \"new method\" OR \"first demonstration\")

Write an \"Innovation and Novelty\" section:
- Explain why current approaches fall short
- Position the proposed innovation against existing work
- Highlight unique advantages and differentiators
- Include source URLs for claims
#+END_AI

** Methodology
#+BEGIN_AI methods
Use search with focus_mode='academicSearch' and query:
\"%s\" AND (method OR protocol OR technique OR procedure)

Summarize methodological approaches:
- Group by technique type
- Include validation strategies
- Note limitations of each approach
- Include source URLs
#+END_AI

** Feasibility and Preliminary Evidence
#+BEGIN_AI feasibility
Use search with focus_mode='academicSearch' and query:
\"%s\" AND (proof-of-concept OR validation OR demonstrated OR feasibility)

Write a \"Feasibility\" section:
- Cite prior validations of the approach
- Highlight robustness and reproducibility
- Note scalability considerations
- Include source URLs
#+END_AI

** References
:PROPERTIES:
:VISIBILITY: folded
:END:

#+BEGIN_SRC bibtex
# Run M-x gptel-fetch-bibtex-from-buffer after generating content
# Or manually add BibTeX entries here
#+END_SRC

** Source URLs
:PROPERTIES:
:VISIBILITY: folded
:END:

# URLs extracted from generated content will be listed here
# Run M-x gptel-extract-urls to populate

"
  "Template for integrated grant proposal with all sections.")

(defun gptel-grant-project (topic)
  "Create a new grant proposal buffer for TOPIC with all sections.
Inserts an org-mode template with AI prompts for each section."
  (interactive "sGrant project topic: ")
  (let* ((buffer-name (format "*Grant: %s*" topic))
         (timestamp (format-time-string "%Y-%m-%d"))
         (content (format gptel-grant-template
                          topic topic timestamp
                          topic topic topic topic topic)))
    (switch-to-buffer (get-buffer-create buffer-name))
    (erase-buffer)
    (org-mode)
    (insert content)
    (goto-char (point-min))
    (message "Grant template created. Use C-c RET on each #+BEGIN_AI block to generate content.")))

(defun gptel-grant-section-at-point ()
  "Execute the AI prompt in the current #+BEGIN_AI...#+END_AI block."
  (interactive)
  (save-excursion
    (let ((start (re-search-backward "^#\\+BEGIN_AI" nil t))
          (end (re-search-forward "^#\\+END_AI" nil t)))
      (when (and start end)
        (goto-char start)
        (forward-line 1)
        (let* ((prompt-start (point))
               (prompt-end (- end (length "#+END_AI\n")))
               (prompt (buffer-substring-no-properties prompt-start prompt-end)))
          (goto-char end)
          (insert "\n\n")
          (gptel-mcp-scientific--send-request prompt))))))

(defun gptel-grant-all-sections ()
  "Generate all AI sections in the current grant proposal buffer.
Processes each #+BEGIN_AI block sequentially."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^#\\+BEGIN_AI" nil t)
        (gptel-grant-section-at-point)
        (cl-incf count)
        ;; Brief pause to avoid overwhelming the API
        (sleep-for 1))
      (message "Queued %d sections for generation" count))))

;;; URL and Citation Extraction

(defun gptel-extract-urls ()
  "Extract all URLs from current buffer and insert into Source URLs section."
  (interactive)
  (let (urls)
    (save-excursion
      (goto-char (point-min))
      ;; Match http/https URLs
      (while (re-search-forward "https?://[^ \t\n\r\">)\\]]+[^ \t\n\r\".>)\\]]" nil t)
        (push (match-string 0) urls)))
    (setq urls (delete-dups (nreverse urls)))
    (if urls
        (progn
          ;; Try to find Source URLs section
          (goto-char (point-min))
          (if (re-search-forward "^\\*+ Source URLs" nil t)
              (progn
                (forward-line 2)
                (end-of-line)
                (insert "\n")
                (dolist (url urls)
                  (insert (format "- %s\n" url)))
                (message "Extracted %d URLs" (length urls)))
            ;; No section found, show in temp buffer
            (with-output-to-temp-buffer "*URLs*"
              (princ "URLs found:\n\n")
              (dolist (url urls)
                (princ (format "- %s\n" url))))))
      (message "No URLs found"))))

(defun gptel-extract-dois ()
  "Extract DOIs from current buffer."
  (interactive)
  (let (dois)
    (save-excursion
      (goto-char (point-min))
      ;; Match DOI patterns
      (while (re-search-forward "\\b10\\.[0-9]\\{4,\\}/[^ \t\n\r\"'>)}\\]]+" nil t)
        (push (match-string 0) dois)))
    (setq dois (delete-dups (nreverse dois)))
    (if dois
        (with-output-to-temp-buffer "*DOIs*"
          (princ "DOIs found:\n\n")
          (dolist (doi dois)
            (princ (format "- https://doi.org/%s\n" doi)))
          (princ "\n\nTo fetch BibTeX, use:\n")
          (princ "curl -LH 'Accept: application/x-bibtex' https://doi.org/DOI"))
      (message "No DOIs found"))))

(defun gptel-doi-to-bibtex (doi)
  "Fetch BibTeX entry for DOI using CrossRef."
  (interactive "sDOI (e.g., 10.1234/example): ")
  (let ((url (format "https://doi.org/%s" (string-trim doi))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (let ((bibtex (buffer-substring (point) (point-max))))
        (kill-new bibtex)
        (message "BibTeX copied to kill ring")
        bibtex))))

(defun gptel-fetch-bibtex-from-buffer ()
  "Fetch BibTeX for all DOIs in buffer and insert into References section."
  (interactive)
  (let (dois bibtex-entries)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\b10\\.[0-9]\\{4,\\}/[^ \t\n\r\"'>)}\\]]+" nil t)
        (push (match-string 0) dois)))
    (setq dois (delete-dups (nreverse dois)))
    (if (not dois)
        (message "No DOIs found in buffer")
      (message "Fetching BibTeX for %d DOIs..." (length dois))
      (dolist (doi dois)
        (condition-case err
            (let* ((url-request-extra-headers '(("Accept" . "application/x-bibtex")))
                   (url (format "https://doi.org/%s" doi)))
              (with-current-buffer (url-retrieve-synchronously url t t 10)
                (goto-char (point-min))
                (when (re-search-forward "\n\n" nil t)
                  (push (buffer-substring (point) (point-max)) bibtex-entries))))
          (error (message "Failed to fetch DOI %s: %s" doi err))))
      ;; Insert into References section
      (goto-char (point-min))
      (if (re-search-forward "^#\\+BEGIN_SRC bibtex" nil t)
          (progn
            (forward-line 1)
            (dolist (entry (nreverse bibtex-entries))
              (insert entry "\n"))
            (message "Inserted %d BibTeX entries" (length bibtex-entries)))
        (with-output-to-temp-buffer "*BibTeX*"
          (princ "BibTeX entries:\n\n")
          (dolist (entry (nreverse bibtex-entries))
            (princ entry)
            (princ "\n")))))))

;;; Citation Normalization

(defun gptel-normalize-citations ()
  "Convert common citation formats to org-cite format in current buffer."
  (interactive)
  (save-excursion
    (let ((count 0))
      ;; [Author, Year] -> [cite:@AuthorYear]
      (goto-char (point-min))
      (while (re-search-forward "\\[\\([A-Za-z]+\\),? *\\([0-9]\\{4\\}\\)\\]" nil t)
        (replace-match "[cite:@\\1\\2]")
        (cl-incf count))
      ;; (Author Year) -> [cite:@AuthorYear]
      (goto-char (point-min))
      (while (re-search-forward "(\\([A-Za-z]+\\) +\\([0-9]\\{4\\}\\))" nil t)
        (replace-match "[cite:@\\1\\2]")
        (cl-incf count))
      ;; Author et al. (Year) -> [cite:@AuthorYear]
      (goto-char (point-min))
      (while (re-search-forward "\\([A-Za-z]+\\) et al\\.? *(\\([0-9]\\{4\\}\\))" nil t)
        (replace-match "[cite:@\\1\\2]")
        (cl-incf count))
      (message "Normalized %d citations to org-cite format" count))))

(defun gptel-extract-citations ()
  "Extract all citations from current buffer and list unique keys."
  (interactive)
  (let (citations)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[cite:@\\([A-Za-z0-9]+\\)\\]" nil t)
        (push (match-string 1) citations)))
    (setq citations (delete-dups (nreverse citations)))
    (if citations
        (with-output-to-temp-buffer "*Citations*"
          (princ "Citations found:\n\n")
          (dolist (cite citations)
            (princ (format "- @%s\n" cite))))
      (message "No org-cite citations found"))))

;;; Keymap

(defvar gptel-mcp-scientific-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'gptel-search-sota)
    (define-key map (kbd "g") #'gptel-search-gaps)
    (define-key map (kbd "m") #'gptel-search-methods)
    (define-key map (kbd "f") #'gptel-search-feasibility)
    (define-key map (kbd "i") #'gptel-search-innovation)
    (define-key map (kbd "p") #'gptel-grant-project)       ; New project
    (define-key map (kbd "RET") #'gptel-grant-section-at-point) ; Generate section
    (define-key map (kbd "a") #'gptel-grant-all-sections)  ; Generate all
    (define-key map (kbd "c") #'gptel-normalize-citations)
    (define-key map (kbd "C") #'gptel-extract-citations)
    (define-key map (kbd "u") #'gptel-extract-urls)        ; Extract URLs
    (define-key map (kbd "d") #'gptel-extract-dois)        ; Extract DOIs
    (define-key map (kbd "b") #'gptel-fetch-bibtex-from-buffer) ; Fetch BibTeX
    (define-key map (kbd "B") #'gptel-doi-to-bibtex)       ; Single DOI to BibTeX
    map)
  "Keymap for gptel scientific search commands.

\\{gptel-mcp-scientific-map}

Key bindings:
  s - State of the Art search
  g - Gap analysis search
  m - Methods search
  f - Feasibility search
  i - Innovation search
  p - Create new grant project template
  RET - Generate current AI section
  a - Generate all AI sections
  c - Normalize citations to org-cite
  C - Extract and list citations
  u - Extract URLs from buffer
  d - Extract DOIs from buffer
  b - Fetch BibTeX for all DOIs
  B - Fetch BibTeX for single DOI")

;; Suggested binding: C-c g s (gptel scientific)
;; (global-set-key (kbd "C-c g s") gptel-mcp-scientific-map)

(provide 'gptel-mcp-scientific)

;;; gptel-mcp-scientific.el ends here
