#!/usr/bin/env sh
#
# Wireless and Bluetooth hardware support.
# wireless-regdb provides regulatory domain data required by the kernel's
# faux_driver; bluez/bluez-utils provide Bluetooth stack and CLI tools.

yay -S --noconfirm wireless-regdb
sudo sed -i 's/^#WIRELESS_REGDOM="IT"$/WIRELESS_REGDOM="IT"/' /etc/conf.d/wireless-regdom

yay -S --noconfirm bluez
yay -S --noconfirm bluez-utils
sudo systemctl enable --now bluetooth.service
