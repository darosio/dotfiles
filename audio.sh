#!/usr/bin/env sh
#
yay -S --noconfirm pipewire
yay -S --noconfirm pipewire-audio
yay -S --noconfirm pipewire-pulse
yay -S --noconfirm pipewire-alsa
yay -S --noconfirm pavucontrol
systemctl start --user pipewire-pulse.service
