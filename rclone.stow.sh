#!/usr/bin/env sh
#
yay -S --noconfirm rclone
mkdir -p "$HOME"/.config/environment.d
stow -t "$HOME" rclone
