#!/usr/bin/env sh
#
yay -S --noconfirm recoll
yay -S --noconfirm pstotext
yay -S --noconfirm python-mutagen
yay -S --noconfirm python-lxml
yay -S --noconfirm python-pychm
mkdir -p "$HOME"/.recoll
mkdir -p "$HOME"/.config/systemd/user
mkdir -p "$HOME"/.local/bin
stow -t "$HOME" recoll
pacaur -Qi recoll
yay -S --noconfirm python-unrar
yay -S --noconfirm lyx
