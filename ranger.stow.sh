#!/usr/bin/env sh
#
# Check if the script is already running in an isolated environment
if [ -z "$ISOLATED_ENV" ]; then
  # Re-run the script in an isolated environment without 'exec' so it continues
  env -i HOME="$HOME" USER="$USER" PATH="/usr/bin:$PATH" ISOLATED_ENV=1 "$0" "$@"
  exit  # Ensure we don't run the original environment after re-execution
fi
# Clear any lingering variables that might interfere
unset PYTHONPATH

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

yay -S --noconfirm imv
yay -S --noconfirm ffmpegthumbnailer
yay -S --noconfirm highlight
yay -S --noconfirm imagemagick
yay -S --noconfirm mediainfo
yay -S --noconfirm odt2txt
yay -S --noconfirm perl-image-exiftool
yay -S --noconfirm poppler
yay -S --noconfirm python-pillow
yay -S --noconfirm transmission-cli
yay -S --noconfirm ueberzugpp
# yay -S --noconfirm w3m
yay -S --noconfirm trash-cli

mkdir -p "$HOME"/.config/environment.d
mkdir -p "$HOME"/.config/ranger
mkdir -p "$HOME"/.local/bin
stow -t "$HOME" ranger
cd "$HOME"/.config/ranger || exit
ln -sf "$HOME"/Sync/.config/ranger/* .

yay -S --noconfirm perl-xls2csv
yay -S --noconfirm python-pipx
pipx install xlsx2csv
