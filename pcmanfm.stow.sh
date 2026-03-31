#!/usr/bin/env sh
#
yay -S --noconfirm pcmanfm
yay -S --noconfirm gvfs        # trash, remote mounts, MTP
yay -S --noconfirm gvfs-mtp    # phone/device mounting
yay -S --noconfirm file-roller # archive management (zip, tar, etc.)
yay -S --noconfirm tumbler     # thumbnail generation

mkdir -p "$HOME"/.config/gtk-3.0
stow -t "$HOME" pcmanfm
