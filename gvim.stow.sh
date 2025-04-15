#!/usr/bin/env sh
#
rm -rf "$HOME"/.vim

yay -S --noconfirm gvim
mkdir -p "$HOME"/.vim/bundle
stow -t "$HOME" gvim

# Store the current directory
ORIGINAL_DIR=$(pwd)
cd "$HOME"/.vim || exit
[ -d "$HOME"/Sync/.vim ] && ln -sf "$HOME"/Sync/.vim/* .
# Return to the original directory
cd "$ORIGINAL_DIR" || exit

git clone https://github.com/VundleVim/Vundle.vim.git "$HOME"/.vim/bundle/Vundle.vim
# Open Vim and install plugins using vim-plug
vim +PluginInstall +qall
