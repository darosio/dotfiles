#!/usr/bin/env sh
#
mkdir -p "$HOME"/.recoll
mkdir -p "$HOME"/.config/systemd/user
mkdir -p "$HOME"/.local/bin
stow -t "$HOME" recoll
