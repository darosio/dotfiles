#!/usr/bin/env sh
#
yay -S --noconfirm syncthing
mkdir -p "$HOME"/Sync
stow -t "$HOME" syncthing

systemctl enable --user syncthing.service
systemctl start --user syncthing.service

