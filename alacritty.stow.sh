#!/usr/bin/env sh
#
yay -S --noconfirm alacritty
mkdir -p "$HOME"/.config/alacritty
stow -t "$HOME" alacritty
