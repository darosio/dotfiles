#!/usr/bin/env sh
#
yay -S --noconfirm wayland

yay -S --noconfirm sway
yay -S --noconfirm swaylock
# yay -S --noconfirm wayprompt
yay -S --noconfirm i3status
yay -S --noconfirm wlroots
yay -S --noconfirm dunst
yay -S --noconfirm wmenu
yay -S --noconfirm rofi-wayland
yay -S --noconfirm wldash
yay -S --noconfirm fuzzel
yay -S --noconfirm wl-clipboard

# yay -S --noconfirm swayimg
yay -S --noconfirm grim
yay -S --noconfirm slurp

yay -S --noconfirm brightnessctl

yay -S --noconfirm qt5-wayland
yay -S --noconfirm qt6-wayland

yay -S --noconfirm wlr-randr
# wdisplays
# kanshi
mkdir -p "$HOME"/.config/
stow -t "$HOME" sway

yay -S --noconfirm keyd
sudo stow -t / 2root.keyd
sudo systemctl enable keyd.service
sudo systemctl start keyd.service

yay -S --noconfirm ttf-ibm-plex
yay -S --noconfirm ttf-hack
# yay -S --noconfirm noto-fonts-extra
# yay -S --noconfirm noto-fonts-emoji
yay -S --noconfirm ttf-nerd-fonts-symbols-mono

yay -S --noconfirm xorg-xwayland
