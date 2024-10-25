#!/usr/bin/env sh
#
yay -S --noconfirm profile-sync-daemon
mkdir -p "$HOME"/.config/psd
stow -t "$HOME" psd
