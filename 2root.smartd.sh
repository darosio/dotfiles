#!/usr/bin/env sh
#
yay -S --noconfirm smartmontools
sudo stow -t / 2root.smartd
sudo systemctl enable smartd.service
sudo systemctl start smartd.service
