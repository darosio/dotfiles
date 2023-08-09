#!/usr/bin/env sh
#
yay -S --noconfirm gvim
mkdir -p "$HOME"/.vim/bundle
stow -t "$HOME" gvim

git clone https://github.com/VundleVim/Vundle.vim.git "$HOME"/.vim/bundle/Vundle.vim
# Open Vim and install plugins using vim-plug
vim +PluginInstall +qall
