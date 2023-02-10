#!/usr/bin/env sh
#
mkdir -p "$HOME"/.config/environment.d
stow -t "$HOME" goldendict
