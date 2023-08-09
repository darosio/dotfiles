#!/usr/bin/env sh
#
mkdir -p "$HOME"/.emacs.d/straight/versions
mkdir -p "$HOME"/.config/environment.d
stow -t "$HOME" emacs
rm -rf "$HOME"/.cache/org-persist
cd "$HOME"/.emacs.d || exit
[ -d "$HOME"/Sync/.emacs ] && ln -sf "$HOME"/Sync/.emacs/* .
mkdir -p "$HOME"/Sync/notes/org-roam/
cd "$HOME" && ln -sf "$HOME"/Sync/.home/.hunspell_* .
