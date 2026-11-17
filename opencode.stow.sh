#!/usr/bin/env sh
#
mkdir -p "$HOME"/.config/opencode
stow -t "$HOME" opencode
