## Emacs/gptel Integration

For Emacs users with [gptel](https://github.com/karthink/gptel) and MCP support:

### Basic Configuration

```elisp
;; Add to your Emacs config
(setq gptel-mcp-servers
      '((perplexica
         :command ("uvx" "perplexica-mcp" "stdio")
         :env (("PERPLEXICA_BACKEND_URL" . "http://localhost:3000/api/search")
               ("PERPLEXICA_CHAT_MODEL_PROVIDER" . "ollama")
               ("PERPLEXICA_CHAT_MODEL_NAME" . "qwen3")
               ("PERPLEXICA_EMBEDDING_MODEL_PROVIDER" . "ollama")
               ("PERPLEXICA_EMBEDDING_MODEL_NAME" . "nomic-embed-text"))
         :description "AI-powered search via Perplexica")))
```

### Docker/Podman Setup

```elisp
;; For containerized Perplexica MCP
(setq gptel-mcp-servers
      '((perplexica
         :command ("podman" "exec" "-i" "perplexica-mcp"
                   "python" "-m" "perplexica_mcp" "stdio")
         :description "AI-powered search via Perplexica (container)")))
```

### Scientific Research System Prompt

```elisp
(setq gptel-default-system-message
      "You have access to Perplexica AI-powered search.
Use the search tool with focus_mode='academicSearch' for scientific queries.
Use focus_mode='webSearch' for general information.

CITATION FORMAT:
- Cite sources as [cite:@AuthorYear] for org-cite compatibility
- Include URLs for verification
- Never invent citations

AVAILABLE FOCUS MODES:
- webSearch: General web search
- academicSearch: Scientific/academic papers
- writingAssistant: Writing help
- wolframAlphaSearch: Computational queries
- youtubeSearch: Video content
- redditSearch: Community discussions")
```

### Grant Writing Helper Functions

```elisp
(defun gptel-perplexica-sota (topic)
  "Search state-of-the-art on TOPIC using Perplexica."
  (interactive "sTopic: ")
  (gptel-request
   (format "Use search with focus_mode='academicSearch' and query:
\"recent advances\" AND \"%s\" AND (review OR overview)

Write a State of the Art section suitable for a grant proposal.
Include 3-5 bullet points and cite all sources as [cite:@AuthorYear]." topic)))

(defun gptel-perplexica-gaps (topic)
  "Find research gaps on TOPIC using Perplexica."
  (interactive "sTopic: ")
  (gptel-request
   (format "Use search with focus_mode='academicSearch' and query:
\"%s\" AND (limitation OR challenge OR \"future work\")

List current limitations and research gaps.
Cite sources acknowledging each limitation." topic)))

(defun gptel-perplexica-methods (topic)
  "Search methodology for TOPIC using Perplexica."
  (interactive "sTopic: ")
  (gptel-request
   (format "Use search with focus_mode='academicSearch' and query:
\"%s\" AND (method OR protocol OR technique)

Summarize common methodological approaches.
Include validation strategies and limitations." topic)))
```

### Multi-MCP Configuration (Perplexica + SearXNG)

```elisp
;; Use both for comprehensive search capabilities
(setq gptel-mcp-servers
      '((perplexica
         :command ("uvx" "perplexica-mcp" "stdio")
         :env (("PERPLEXICA_BACKEND_URL" . "http://localhost:3000/api/search")
               ("PERPLEXICA_CHAT_MODEL_PROVIDER" . "ollama")
               ("PERPLEXICA_CHAT_MODEL_NAME" . "qwen3")
               ("PERPLEXICA_EMBEDDING_MODEL_PROVIDER" . "ollama")
               ("PERPLEXICA_EMBEDDING_MODEL_NAME" . "nomic-embed-text"))
         :description "AI-synthesized search results")
        (searxng
         :command ("npx" "-y" "mcp-searxng")
         :env (("SEARXNG_URL" . "http://localhost:8080"))
         :description "Raw web search results")))
```

### Org-cite Integration

For automatic citation formatting with org-mode:

```elisp
(defun gptel-normalize-citations ()
  "Convert LLM citations to org-cite format in current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Convert [Author, Year] -> [cite:@AuthorYear]
    (while (re-search-forward "\\[\\([A-Za-z]+\\),? *\\([0-9]\\{4\\}\\)\\]" nil t)
      (replace-match "[cite:@\\1\\2]"))
    ;; Convert (Author Year) -> [cite:@AuthorYear]
    (goto-char (point-min))
    (while (re-search-forward "(\\([A-Za-z]+\\) +\\([0-9]\\{4\\}\\))" nil t)
      (replace-match "[cite:@\\1\\2]"))))
```

### Troubleshooting

1. **Model configuration errors**: Ensure all `PERPLEXICA_*` env vars are set
2. **Connection refused**: Verify Perplexica backend is running at configured URL
3. **Timeout errors**: Increase `PERPLEXICA_READ_TIMEOUT` for complex searches
4. **Empty results**: Try different `focus_mode` or broader query terms
