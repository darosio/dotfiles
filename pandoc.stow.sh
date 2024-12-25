#!/usr/bin/env sh
#
yay -S --noconfirm pandoc-cli
yay -S --noconfirm pandoc-crossref
stow -t "$HOME" pandoc
