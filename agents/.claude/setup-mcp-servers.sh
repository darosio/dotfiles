#!/usr/bin/env bash
# Registers user-scope MCP servers for Claude Code (reproduces `claude mcp add ... -s user`
# on a new machine). Idempotent — skips any server already registered.
#
# Prerequisites, done once per machine:
#   - claude CLI installed and authenticated
#   - zotero-mcp-server: uv tool install zotero-mcp-server[semantic]; then `zotero-mcp setup`
#     (configures ZOTERO_LOCAL=true). Zotero desktop must be running for the "zotero" server
#     to actually connect (it talks to Zotero's local API).
#   - pdf-mcp.py stowed from the emacs package (~/.local/bin/pdf-mcp.py)
#   - bib files present at the ZOTERO_BIB_FILES paths below

set -euo pipefail

add_if_missing() {
  local name=$1
  shift
  if claude mcp get "$name" &> /dev/null; then
    echo "mcp server '$name' already registered, skipping"
  else
    claude mcp add "$name" -s user "$@"
  fi
}

add_if_missing zotero \
  -e ZOTERO_LOCAL=true \
  -e ZOTERO_EMBEDDING_MODEL=default \
  -- "$HOME/.local/bin/zotero-mcp"

add_if_missing pdf \
  -e ZOTERO_BIB_FILES="$HOME/Sync/biblio/main.bib:$HOME/Sync/biblio/former.bib:$HOME/Sync/biblio/MY.bib" \
  -- uv run --with pymupdf "$HOME/.local/bin/pdf-mcp.py"
