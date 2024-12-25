#!/usr/bin/env sh
set -e  # Exit immediately on error

mkdir -p "$HOME"/.config

sudo pacman -S --needed base-devel
sudo pacman -S --noconfirm git

# Store the current directory
ORIGINAL_DIR=$(pwd)
# Change to the home directory
cd "$HOME" || exit
git clone https://aur.archlinux.org/paru.git
cd paru
makepkg -si --noconfirm
# Return to the original directory
cd "$ORIGINAL_DIR" || exit

paru -S --noconfirm yay-bin

yay -S --noconfirm mercurial
stow -t "$HOME" git_hg

yay -S --noconfirm pacman-contrib  # needed for pacdiff
yay -S --noconfirm pkgstats
yay -S --noconfirm downgrade
