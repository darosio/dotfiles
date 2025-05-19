#!/usr/bin/env sh
#

yay -S --noconfirm mu
yay -S --noconfirm isync
stow -t "$HOME" mbsync
# yay -S --noconfirm mb2md

mkdir -p "$HOME"/Maildir/gmail
mkdir -p "$HOME"/Maildir/pec
mbsync -a

mu init --maildir "$HOME"/Maildir/ --my-address=daniele.arosio@cnr.it \
	--my-address danielepietroarosio@gmail.com \
	--my-address daniele.arosio@postecert.it
mu index

cd "$HOME"/Maildir || exit
ln -sf "$HOME"/Sync/Maildir/archive .
ln -sf "$HOME"/Sync/Maildir/personal .
ln -sf "$HOME"/Sync/Maildir/mailrc .
