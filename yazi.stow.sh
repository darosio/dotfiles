#!/usr/bin/env sh
#
# Check if the script is already running in an isolated environment
if [ -z "$ISOLATED_ENV" ]; then
  # Re-run the script in an isolated environment without 'exec' so it continues
  env -i HOME="$HOME" USER="$USER" PATH="/usr/bin:$PATH" ISOLATED_ENV=1 "$0" "$@"
  exit # Ensure we don't run the original environment after re-execution
fi
# Clear any lingering variables that might interfere
unset PYTHONPATH

yay -S --noconfirm yazi

yay -S --noconfirm imv
yay -S --noconfirm bat
yay -S --noconfirm eza
yay -S --noconfirm zoxide
yay -S --noconfirm ffmpegthumbnailer
yay -S --noconfirm imagemagick
yay -S --noconfirm perl-image-exiftool
yay -S --noconfirm poppler
yay -S --noconfirm jq
yay -S --noconfirm resvg
yay -S --noconfirm mediainfo
yay -S --noconfirm trash-cli

mkdir -p "$HOME"/.bashrc.d
mkdir -p "$HOME"/.config/yazi
mkdir -p "$HOME"/.local/share/applications
stow -t "$HOME" yazi

ya pkg install
