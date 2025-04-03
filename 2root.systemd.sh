#!/usr/bin/env sh
#
sudo mkdir -p /etc/systemd/system/service.d
sudo mkdir -p /etc/systemd/system/failure-notification@.service.d
sudo stow -t / 2root.systemd
sudo mkdir -p /etc/systemd/system/failure-notification@.service.d
sudo touch /etc/systemd/system/failure-notification@.service.d/toplevel-override.conf
