## Emacs Integration (gptel + mcp.el)

For Emacs users with [gptel](https://github.com/karthink/gptel) and MCP support:

### Basic Configuration

```elisp
;; Add to your Emacs config (~/.emacs.d/init.el or ~/.config/emacs/init.el)
(setq gptel-mcp-servers
      '((searxng
         :command ("npx" "-y" "mcp-searxng")
         :env (("SEARXNG_URL" . "http://localhost:8080"))
         :description "Web search via SearXNG")))
```

### Docker/Podman Container Setup

If running mcp-searxng in a container:

```elisp
;; For Docker
(setq gptel-mcp-servers
      '((searxng
         :command ("docker" "exec" "-i" "mcp-searxng" "node" "/app/build/index.js")
         :description "Web search via SearXNG (Docker)")))

;; For Podman
(setq gptel-mcp-servers
      '((searxng
         :command ("podman" "exec" "-i" "mcp-searxng" "node" "/app/build/index.js")
         :description "Web search via SearXNG (Podman)")))
```

### System Prompt for Tool Usage

Configure gptel to encourage search tool usage:

```elisp
(setq gptel-default-system-message
      "You have access to web search via SearXNG MCP tools.
Use searxng_web_search for factual claims and current information.
Use web_url_read to extract content from URLs.
Always cite sources with [Source: URL] format.")
```

### Scientific Research Configuration

For academic/scientific workflows with SearXNG configured for scholarly engines:

```elisp
(defun gptel-search-literature (topic)
  "Search scientific literature on TOPIC using SearXNG MCP."
  (interactive "sTopic: ")
  (gptel-request
   (format "Use searxng_web_search to find recent papers on: %s
Focus on review articles from PubMed, arXiv, and Google Scholar.
Summarize key findings with citations." topic)))

(defun gptel-search-sota (topic)
  "Search state-of-the-art on TOPIC."
  (interactive "sTopic: ")
  (gptel-request
   (format "Use searxng_web_search with query: \"recent advances\" AND \"%s\" AND (review OR overview)
Summarize the current state of the art." topic)))
```

### Combining with Other MCP Servers

```elisp
(setq gptel-mcp-servers
      '((searxng
         :command ("npx" "-y" "mcp-searxng")
         :env (("SEARXNG_URL" . "http://localhost:8080"))
         :description "Web search via SearXNG")
        (filesystem
         :command ("npx" "-y" "@modelcontextprotocol/server-filesystem" "/home/user/documents")
         :description "Local file access")))
```

### Troubleshooting

1. **MCP server not responding**: Ensure `SEARXNG_URL` points to a running SearXNG instance
2. **JSON format errors**: Verify SearXNG has `format: json` enabled in settings
3. **Container networking**: Use `host.containers.internal` or `host.docker.internal` for host access from containers
