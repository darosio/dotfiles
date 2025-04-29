#!/usr/bin/env sh
#
yay -S --noconfirm firefox
yay -S --noconfirm speech-dispatcher
yay -S --noconfirm profile-sync-daemon
mkdir -p "$HOME"/.config/psd
stow -t "$HOME" psd

systemctl --user enable --now psd.service
