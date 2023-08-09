#!/usr/bin/env sh
#
sudo mkdir -p /etc/systemd/system/service.d
sudo stow -t / systemd
sudo mkdir -p /etc/systemd/system/failure-notification@.service.d
sudo touch /etc/systemd/system/failure-notification@.service.d/toplevel-override.conf
