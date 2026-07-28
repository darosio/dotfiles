#!/usr/bin/env sh
#
# Install and harden sshd. Listens on 23456 (see nftables), key-auth only.
set -eu

yay -S --noconfirm openssh

sudo mkdir -p /etc/systemd/system/sshd.service.d/
sudo stow -t / sshd

# Arch ships an AuthorizedKeysCommand pointing at userdbctl, which shadows
# ~/.ssh/authorized_keys on systems that do not use systemd-homed.
sudo sed -i 's/^AuthorizedKeysCommand/#AuthorizedKeysCommand/' \
  /etc/ssh/sshd_config.d/20-systemd-userdb.conf

# The drop-in only takes effect after a reload + restart; enable --now on an
# already-running unit is a no-op and silently leaves sshd on port 22.
sudo systemctl daemon-reload
sudo systemctl enable sshd.service
sudo systemctl restart sshd.service

# Verify sshd really moved to 23456.
ss -tlnp 2> /dev/null | grep -q ':23456' &&
  echo "sshd listening on 23456" ||
  echo "WARNING: sshd is NOT listening on 23456; check systemctl status sshd"
