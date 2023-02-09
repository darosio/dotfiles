#!/usr/bin/env sh
#
mkdir -p "$HOME"/.config/gh
stow -t "$HOME" gh
pass cloud/gh_token | gh auth login --with-token
