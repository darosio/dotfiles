#!/usr/bin/env sh
#
yay -S --noconfirm opensmtps
sudo systemctl start smartd.service
sudo systemctl enable smtpd.service

yay -S --noconfirm msmtp
stow -t "$HOME" msmtp
