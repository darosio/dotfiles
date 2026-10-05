#!/usr/bin/env sh
#
# Registers MCP servers for pi (reproduces `pi mcp add ...` on a new machine).
# Idempotent — skips any server already registered in ~/.pi/agent/mcp.json.
#
# Prerequisites:
#   - pi CLI installed and authenticated (providers via pi-pass-auth / registry)
#   - pdf-mcp.py stowed from emacs package (~/.local/bin/pdf-mcp.py)
#   - zotero-mcp-server installed: uv tool install zotero-mcp-server[semantic]
#     then `zotero-mcp setup` (configures ZOTERO_LOCAL=true)
#   - searxng running at http://localhost:8080 (ai-containers podman quadlet)
#   - bib files present at ZOTERO_BIB_FILES paths

set -eu

add_if_missing() {
  name=$1
  shift
  if pi mcp list --json 2> /dev/null | jq -e ".servers[] | select(.name == \"$name\")" > /dev/null; then
    echo "pi mcp server '$name' already registered, skipping"
  else
    pi mcp add "$name" "$@"
  fi
}

# Ensure mcp.json exists and is valid JSON
mkdir -p "$HOME/.pi/agent"
[ -f "$HOME/.pi/agent/mcp.json" ] || printf '{}' > "$HOME/.pi/agent/mcp.json"

# pdf-mcp (local PDF extraction via PyMuPDF) - uses wrapper with uv
# pi's -e flag doesn't pass env to spawned process; use sh -c
add_if_missing pdf \
  -- sh -c "ZOTERO_BIB_FILES='$HOME/Sync/biblio/main.bib:$HOME/Sync/biblio/former.bib:$HOME/Sync/biblio/MY.bib' exec '$HOME/.local/bin/pdf-mcp-wrapper'"

# zotero-mcp (Zotero library via local API; Zotero desktop must be running)
# Needs 'serve' subcommand for MCP server mode; use absolute path
if command -v zotero-mcp > /dev/null 2>&1; then
  ZOTERO_MCP_PATH=$(readlink -f "$HOME/.local/bin/zotero-mcp")
  add_if_missing zotero \
    -- sh -c "ZOTERO_LOCAL=true ZOTERO_EMBEDDING_MODEL=default exec '$ZOTERO_MCP_PATH' serve"
else
  echo "zotero-mcp not found (uv tool install zotero-mcp-server[semantic]), skipping"
fi

# searxng (local metasearch via ai-containers) - needs mcp-searxng bridge
# SearXNG itself doesn't expose MCP; use npm mcp-searxng
# pi needs the symlink path (/usr/bin/npx), not the resolved path
if command -v npx > /dev/null 2>&1; then
  NPX_PATH=$(which npx)
  add_if_missing searxng \
    -- sh -c "SEARXNG_URL=http://localhost:8080 exec '$NPX_PATH' -y mcp-searxng"
else
  echo "npx not found, skipping searxng MCP"
fi

echo "pi MCP setup complete. Run 'pi mcp list' to verify."
