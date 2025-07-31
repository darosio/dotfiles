#!/usr/bin/env sh
#
yay -S --noconfirm xdg-desktop-portal
yay -S --noconfirm xdg-desktop-portal-termfilechooser-hunkyburrito-git

mkdir -p "$HOME"/.config/xdg-desktop-portal
mkdir -p "$HOME"/.config/xdg-desktop-portal-termfilechooser
stow -t "$HOME" xdg-desktop-portal

systemctl --user start xdg-desktop-portal.service
