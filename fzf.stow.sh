#!/usr/bin/env sh
#
yay -S --noconfirm fzf
yay -S --noconfirm fd # fzf-source, and the ctrl-o reload

# Create the directories *before* stowing so stow links each file individually
# instead of linking whole directories into this repo.
mkdir -p "$HOME"/.bashrc.d
mkdir -p "$HOME"/.local/bin
stow -t "$HOME" fzf
