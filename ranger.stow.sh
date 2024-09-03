#!/usr/bin/env sh
#
yay -S --noconfirm ranger

yay -S --noconfirm atool
yay -S --noconfirm cpio
yay -S --noconfirm lha
yay -S --noconfirm lzop
yay -S --noconfirm p7zip
yay -S --noconfirm unace
yay -S --noconfirm unrar
yay -S --noconfirm zip
yay -S --noconfirm unzip

yay -S --noconfirm ffmpegthumbnailer
yay -S --noconfirm highlight
yay -S --noconfirm imagemagick
yay -S --noconfirm mediainfo
yay -S --noconfirm odt2txt
yay -S --noconfirm perl-image-exiftool
yay -S --noconfirm poppler
yay -S --noconfirm python-pillow
yay -S --noconfirm transmission-cli
yay -S --noconfirm ueberzug
yay -S --noconfirm w3m
yay -S --noconfirm trash-cli

yay -S --noconfirm python-xlsx2csv

mkdir -p "$HOME"/.config/ranger
stow -t "$HOME" ranger
cd "$HOME"/.config/ranger || exit
ln -s "$HOME"/Sync/.config/ranger/* .
