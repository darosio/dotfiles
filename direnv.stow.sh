#!/usr/bin/env sh
#
yay -S --noconfirm direnv
mkdir -p "$HOME"/.config/direnv
stow -t "$HOME" direnv
