#!/usr/bin/env sh
#
yay -S --noconfirm opensmtpd
sudo systemctl start smtpd.service
sudo systemctl enable smtpd.service

yay -S --noconfirm msmtp
stow -t "$HOME" msmtp
