#!/usr/bin/env sh
#
yay -S --noconfirm msmtp
yay -S --noconfirm msmtp-mta
sudo stow -t / 2root.msmtp
