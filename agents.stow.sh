#!/usr/bin/env sh
#
mkdir -p "$HOME"/.claude/commands
mkdir -p "$HOME"/.copilot
mkdir -p "$HOME"/.gemini
mkdir -p "$HOME"/.codex
mkdir -p "$HOME"/.hermes
mkdir -p "$HOME"/.local/bin
stow -t "$HOME" agents

# Setup pi MCP servers (idempotent)
if command -v pi > /dev/null 2>&1; then
  "$HOME"/workspace/dotfiles/pi.stow.sh
fi
