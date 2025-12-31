# PR Drafts for MCP Integration

This directory contains prototype code and patch files for two upstream PRs.

## PR 1: mcp-searxng Emacs Documentation

**Repository:** [ihor-sokoliuk/mcp-searxng](https://github.com/ihor-sokoliuk/mcp-searxng)

**Files:**

- `mcp-searxng/EMACS_INTEGRATION.md` - Full documentation to add
- `mcp-searxng/README.patch` - Git patch for README.md

**To submit:**

1. Fork https://github.com/ihor-sokoliuk/mcp-searxng
2. Create branch: `git checkout -b docs/emacs-integration`
3. Apply patch or manually add section to README.md
4. PR title: `docs: Add Emacs/gptel integration documentation`

______________________________________________________________________

## PR 2: perplexica-mcp Scientific Features

**Repository:** [thetom42/perplexica-mcp](https://github.com/thetom42/perplexica-mcp)

**Files:**

- `perplexica-mcp/server_enhanced.py` - Full enhanced server (prototype)
- `perplexica-mcp/EMACS_INTEGRATION.md` - Emacs docs to add
- `perplexica-mcp/README_and_server.patch` - Git patch

**To submit:**

1. Fork https://github.com/thetom42/perplexica-mcp
2. Create branch: `git checkout -b feat/scientific-search`
3. Apply changes from patch or manually merge server_enhanced.py
4. PR title: `feat: Add scientific search features and Emacs integration`

**New features added:**

- `search_scientific()` tool with preset query templates
- MCP resources: `prompts://scientific-synthesis`, `prompts://grant-sota`, `prompts://grant-gap-analysis`
- Optional `categories` parameter for search filtering

______________________________________________________________________

## Local Testing

### Test the Emacs package immediately:

```elisp
;; In Emacs:
(load-file "/home/dan/workspace/dotfiles/searxng_perplexica/gptel-mcp-scientific.el")
(gptel-mcp-scientific-setup)
(gptel-mcp-scientific-set-prompt)

;; Then use:
;; M-x gptel-search-sota
;; M-x gptel-search-gaps
;; M-x gptel-search-methods
```

### Test the enhanced server:

```bash
# Install dependencies
pip install httpx mcp python-dotenv uvicorn pydantic fastmcp

# Set environment
export PERPLEXICA_BACKEND_URL=http://localhost:3000/api/search
export PERPLEXICA_CHAT_MODEL_PROVIDER=ollama
export PERPLEXICA_CHAT_MODEL_NAME=qwen3
export PERPLEXICA_EMBEDDING_MODEL_PROVIDER=ollama
export PERPLEXICA_EMBEDDING_MODEL_NAME=nomic-embed-text

# Run the enhanced server
python pr-drafts/perplexica-mcp/server_enhanced.py stdio
```

______________________________________________________________________

## Quick PR Submission Commands

### PR 1 (mcp-searxng):

```bash
gh repo fork ihor-sokoliuk/mcp-searxng --clone
cd mcp-searxng
git checkout -b docs/emacs-integration
# Add content from EMACS_INTEGRATION.md to README.md after "HTTP Transport" section
git add README.md
git commit -m "docs: Add Emacs/gptel integration documentation"
git push origin docs/emacs-integration
gh pr create --title "docs: Add Emacs/gptel integration documentation" \
  --body "Add comprehensive documentation for Emacs users integrating mcp-searxng with gptel and mcp.el, including basic config, container setup, system prompts, and scientific research helpers."
```

### PR 2 (perplexica-mcp):

```bash
gh repo fork thetom42/perplexica-mcp --clone
cd perplexica-mcp
git checkout -b feat/scientific-search
# Merge changes from server_enhanced.py into src/perplexica_mcp/server.py
# Add EMACS_INTEGRATION.md content to README.md
git add .
git commit -m "feat: Add scientific search features and Emacs integration"
git push origin feat/scientific-search
gh pr create --title "feat: Add scientific search features and Emacs integration" \
  --body "Add scientific research workflow enhancements including MCP prompt resources, search_scientific() tool, and Emacs/gptel documentation."
```
