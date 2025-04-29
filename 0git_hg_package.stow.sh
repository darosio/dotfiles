#!/usr/bin/env sh
set -e  # Exit immediately on error

mkdir -p "$HOME"/.config
mkdir -p "$HOME"/.local/bin

#sudo pacman -S --needed base-devel
sudo pacman -S --noconfirm stow
sudo pacman -S --noconfirm git
sudo pacman -S --noconfirm git-delta
sudo pacman -S --noconfirm mercurial
stow -t "$HOME" git_hg

# Store the current directory
ORIGINAL_DIR=$(pwd)
# Change to the home directory
cd "$HOME" || exit
rm -rf paru
git clone https://aur.archlinux.org/paru.git
cd paru
makepkg -si --noconfirm
# Return to the original directory
cd "$ORIGINAL_DIR" || exit

paru -S --noconfirm yay-bin

yay -S --noconfirm expac
yay -S --noconfirm pacman-contrib  # needed for pacdiff
yay -S --noconfirm pkgstats
yay -S --noconfirm downgrade
