#!/usr/bin/env sh
#
yay -S openssh
sudo mkdir -p /etc/systemd/system/sshd.socket.d/
sudo stow -t / sshd
sudo systemctl enable sshd.service
