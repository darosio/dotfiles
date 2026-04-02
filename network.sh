#!/usr/bin/env sh
#
# Wireless and Bluetooth hardware support.
# wireless-regdb provides regulatory domain data required by the kernel's
# faux_driver; bluez/bluez-utils provide Bluetooth stack and CLI tools.

yay -S --noconfirm wireless-regdb

yay -S --noconfirm bluez
yay -S --noconfirm bluez-utils
sudo systemctl enable --now bluetooth.service
