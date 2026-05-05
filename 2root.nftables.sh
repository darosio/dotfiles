#!/usr/bin/env sh
#
set -eu

yay -S --noconfirm nftables

if [ -e /etc/nftables.conf ] && [ ! -L /etc/nftables.conf ]; then
  sudo cp /etc/nftables.conf /etc/nftables.conf.pre-dotfiles
fi

sudo stow -t / 2root.nftables
sudo systemctl enable nftables.service
sudo systemctl start nftables.service
