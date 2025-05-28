#!/usr/bin/env sh
#
yay -S --noconfirm zotero-bin
mkdir -p "$HOME"/.zotero/zotero/90bt0tu8.default/
stow -t "$HOME" zotero
cd "$HOME" || exit
wget https://github.com/syt2/zotero-addons/releases/latest/download/zotero-addons.xpi
