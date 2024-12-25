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

yay -S --noconfirm emacs-wayland
yay -S --noconfirm watchexec

yay -S --noconfirm plocate
sudo updatedb

yay -S --noconfirm mypy
yay -S --noconfirm ruff-lsp
yay -S --noconfirm python-lsp-server
yay -S --noconfirm python-isort
yay -S --noconfirm jupyter-console
yay -S --noconfirm jupyterlab
yay -S --noconfirm tree-sitter-python

yay -S --noconfirm ripgrep
yay -S --noconfirm the_silver_searcher
yay -S --noconfirm hunspell
yay -S --noconfirm hunspell-en_us
yay -S --noconfirm hunspell-it
# for flycheck
yay -S --noconfirm vale
yay -S --noconfirm xmlstarlet
yay -S --noconfirm shellcheck
# python-pylint
# for lsp
yay -S --noconfirm bash-language-server

yay -S --noconfirm graphviz
yay -S --noconfirm plantuml
yay -S --noconfirm gnuplot

yay -S --noconfirm mu
yay -S --noconfirm isync
stow -t "$HOME" mbsync
# yay -S --noconfirm mb2md
