#!/usr/bin/env sh
#
yay -S --noconfirm myrepos
mkdir -p "$HOME"/workspace/arte
mkdir -p "$HOME"/workspace/examples
sudo mkdir -p /home/dati
sudo chown dan:dan /home/dati
stow -t /home mr
