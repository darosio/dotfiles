#!/usr/bin/env sh
#
sudo mkdir -p /etc/systemd/system/service.d
sudo mkdir -p /etc/systemd/system/failure-notification@.service.d
sudo stow -t / 2root.systemd

# Mask systemd-homed to prevent spurious D-Bus activation failures
sudo systemctl mask systemd-homed.service
sudo mkdir -p /etc/systemd/system/failure-notification@.service.d
sudo touch /etc/systemd/system/failure-notification@.service.d/toplevel-override.conf
