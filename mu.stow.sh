#!/usr/bin/env sh
#

yay -S --noconfirm mu
yay -S --noconfirm isync
stow -t "$HOME" mbsync
# yay -S --noconfirm mb2md

mkdir -p "$HOME"/Maildir/
mu init --maildir "$HOME"/Maildir/ --my-address=daniele.arosio@cnr.it \
  --my-address danielepietroarosio@gmail.com \
  --my-address daniele.arosio@postecert.it
mu index
