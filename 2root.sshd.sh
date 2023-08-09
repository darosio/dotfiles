#!/usr/bin/env sh
#
yay -S --noconfirm openssh
sudo mkdir -p /etc/systemd/system/sshd.socket.d/
sudo stow -t / 2root.sshd
sudo systemctl enable sshd.service
