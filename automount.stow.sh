#!/usr/bin/env sh
#
yay -S --noconfirm udisks2
yay -S --noconfirm udiskie
yay -S --noconfirm ntfs-3g
mkdir -p "$HOME"/.config/systemd/user
stow -t "$HOME" automount
sudo systemctl start udisks2.service
sudo systemctl enable udisks2.service
systemctl start --user udiskie.service
systemctl enable --user udiskie.service
