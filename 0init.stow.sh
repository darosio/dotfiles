#!/usr/bin/env sh
#
mkdir -p "$HOME"/.config
rm -rf "$HOME"/.bashrc
rm -rf "$HOME"/.bash_profile
rm -rf "$HOME"/.bash_logout
stow -t "$HOME" bash
stow -t "$HOME" misc
mkdir -p "$HOME"/.local/bin
stow -t "$HOME" scripts

yay -S --noconfirm python-hatch
#yay -S --noconfirm direnv


