#!/usr/bin/env sh
#
mkdir -p "$HOME"/.config
stow -t "$HOME" bash
stow -t "$HOME" misc
stow -t "$HOME" scripts

yay -S --noconfirm python-hatch
yay -S --noconfirm pyenv-virtualenv
yay -S --noconfirm direnv
