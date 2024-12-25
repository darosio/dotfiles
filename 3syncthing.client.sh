#!/usr/bin/env sh
#
yay -S --noconfirm syncthing

systemctl enable --user syncthing.service
systemctl start --user syncthing.service
