#!/usr/bin/env sh
#
mkdir -p "$HOME"/.emacs.d/straight/versions
mkdir -p "$HOME"/.config/environment.d
stow -t "$HOME" emacs
cd "$HOME"/.emacs.d || exit
ln -s "$HOME"/Sync/.emacs/* .
