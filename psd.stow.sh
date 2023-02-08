#!/usr/bin/env sh
#
mkdir -p "$HOME"/.config/psd
stow -t "$HOME" psd
