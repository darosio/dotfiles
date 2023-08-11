#!/usr/bin/env sh
#
mkdir -p "$HOME"/.config
stow -t "$HOME" bash
stow -t "$HOME" misc
stow -t "$HOME" scripts
