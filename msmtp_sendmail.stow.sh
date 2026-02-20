#!/usr/bin/env sh
#
sudo pacman -S --noconfirm msmtp
sudo pacman -S --noconfirm msmtp-mta
stow -t "$HOME" msmtp
