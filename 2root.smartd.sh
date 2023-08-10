#!/usr/bin/env sh
#
sudo stow -t / 2root.smartd
yay -S --noconfirm smartmontools
sudo systemctl enable smartd.service
sudo systemctl start smartd.service
