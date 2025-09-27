#!/usr/bin/env sh
#
mkdir -p "$HOME"/.emacs.d/straight/versions
mkdir -p "$HOME"/.config/environment.d
mkdir -p "$HOME"/.local/share/applications
mkdir -p "$HOME"/.local/bin
stow -t "$HOME" emacs
rm -rf "$HOME"/.cache/org-persist

# Store the current directory
ORIGINAL_DIR=$(pwd)
cd "$HOME"/.emacs.d || exit
[ -d "$HOME"/Sync/.emacs ] && ln -sf "$HOME"/Sync/.emacs/* .
mkdir -p "$HOME"/Sync/notes/org-roam/

cd "$HOME" && ln -sf "$HOME"/Sync/.home/.hunspell_* .

# Return to the original directory
cd "$ORIGINAL_DIR" || exit

yay -S --noconfirm emacs-wayland
yay -S --noconfirm watchexec

yay -S --noconfirm plocate
sudo updatedb

yay -S --noconfirm mypy
yay -S --noconfirm python-lsp-server
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
# for lsp and apheleia
# yay -S --noconfirm bash-language-server
# yay -S --noconfirm yaml-language-server
yay -S --noconfirm yamlfmt
# yay -S --noconfirm mdformat
# yay -S --noconfirm marksman
yay -S --noconfirm taplo-cli
yay -S --noconfirm shfmt
yay -S --noconfirm actionlint # for gh workflow

yay -S --noconfirm graphviz
yay -S --noconfirm plantuml
yay -S --noconfirm gnuplot
