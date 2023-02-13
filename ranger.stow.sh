#!/usr/bin/env sh
#
mkdir -p "$HOME"/.config/ranger
stow -t "$HOME" ranger
cd "$HOME"/.config/ranger || exit
ln -s "$HOME"/Sync/.config/ranger/* .
