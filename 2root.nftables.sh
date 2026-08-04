#!/usr/bin/env sh
#
set -eu

yay -S --noconfirm nftables

# Arch ships a stock /etc/nftables.conf as a real file. stow refuses to replace
# anything that is not already a symlink, so move it aside rather than just
# copying it, or the stow below aborts with a conflict.
if [ -e /etc/nftables.conf ] && [ ! -L /etc/nftables.conf ]; then
  sudo mv /etc/nftables.conf /etc/nftables.conf.pre-dotfiles
fi

sudo stow -t / 2root.nftables

# Validate before loading: a syntax error would otherwise leave the input chain
# at its default-drop policy with no accept rules.
sudo nft -c -f /etc/nftables.conf

sudo systemctl enable nftables.service
sudo systemctl restart nftables.service
