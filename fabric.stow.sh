#!/usr/bin/env sh
#
# fabric is Go-based; official installer puts binary in ~/.local/bin
curl -fsSL https://raw.githubusercontent.com/danielmiessler/fabric/main/scripts/installer/install.sh | bash

# Configure for local Ollama — skip interactive setup
mkdir -p "$HOME"/.config/fabric/contexts
cat > "$HOME"/.config/fabric/.env << 'EOF'
DEFAULT_VENDOR=Ollama
DEFAULT_MODEL=phi4-reasoning:plus
OLLAMA_API_URL=http://localhost:11434
OLLAMA_HTTP_TIMEOUT=20m
PATTERNS_LOADER_GIT_REPO_URL=https://github.com/danielmiessler/fabric.git
PATTERNS_LOADER_GIT_REPO_PATTERNS_FOLDER=data/patterns
PROMPT_STRATEGIES_GIT_REPO_URL=https://github.com/danielmiessler/fabric.git
PROMPT_STRATEGIES_GIT_REPO_STRATEGIES_FOLDER=data/strategies
EOF

# Stow contexts (biophysics, coding, proposal, review, brainstorm)
stow -t "$HOME" fabric

# Download patterns and strategies
fabric --setup
