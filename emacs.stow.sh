#!/usr/bin/env sh
#
mkdir -p "$HOME"/.emacs.d/straight/versions
stow -t "$HOME" emacs
cd "$HOME"/.emacs.d || exit
ln -s "$HOME"/Sync/.emacs/* .
