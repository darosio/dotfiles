#!/usr/bin/env sh
#
yay -S --noconfirm starship
yay -S --noconfirm ttf-hack-nerd

mkdir -p "$HOME"/.config
stow -t "$HOME" starship
