#!/usr/bin/env sh
#
yay -S --noconfirm syncthing
mkdir -p "$HOME"/Sync
#stow -t "$HOME" syncthing
cp syncthing/Sync/.stignore "$HOME"/Sync/

systemctl enable --user syncthing.service
systemctl start --user syncthing.service
