#!/usr/bin/env sh
#
yay -S --noconfirm wayland

yay -S --noconfirm sway
yay -S --noconfirm swaylock
# yay -S --noconfirm waybar
# yay -S --noconfirm wayprompt
yay -S --noconfirm i3status
yay -S --noconfirm wlroots
yay -S --noconfirm dunst

mkdir -p "$HOME"/.config/
stow -t "$HOME" sway
