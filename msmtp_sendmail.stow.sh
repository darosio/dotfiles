#!/usr/bin/env sh
#
yay -S --noconfirm msmtp
yay -S --noconfirm msmtp-mta
stow -t "$HOME" msmtp
