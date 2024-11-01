#!/usr/bin/env sh
#
yay -S --noconfirm nftables
sudo rm /etc/nftables.conf
sudo stow -t / 2root.nftables
sudo systemctl enable nftables.service
sudo systemctl start nftables.service
