#!/usr/bin/env sh
#
mkdir -p "$HOME"/.claude/commands
mkdir -p "$HOME"/.copilot
mkdir -p "$HOME"/.gemini
mkdir -p "$HOME"/.codex
mkdir -p "$HOME"/.hermes
stow -t "$HOME" agents
