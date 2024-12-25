#!/usr/bin/env sh
#
yay -S --noconfirm bpytop
yay -S --noconfirm atop
yay -S --noconfirm htop
stow -t "$HOME" htop
