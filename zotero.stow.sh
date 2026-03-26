#!/usr/bin/env sh
# Install Zotero and apply user preferences.
#
# Managed files:
#   user.js        — stowed (symlink); sets BBT auto-exports + citekey format
#   treePrefs.json — copied (not symlinked); Zotero overwrites it at runtime,
#                    breaking symlinks. Re-run this script to restore columns.
#
# BBT citation key format: auth.capitalize + year2 + shorttitle(3)
# Produces keys like: Alejaldre21MethodsEnzymeLibrary
#
# After first run / key format change:
#   Zotero → Edit → Better BibTeX → Citation Keys → Refresh all keys
#   Then re-trigger all auto-exports (Edit → Better BibTeX → Export).

yay -S --noconfirm zotero-bin

# Discover profile dir from profiles.ini (falls back to known name).
PROFILE_DIR=$(awk -F= '/^Path=/ {print $2; exit}' \
  "$HOME/.zotero/zotero/profiles.ini" 2> /dev/null)
PROFILE_DIR="${PROFILE_DIR:-90bt0tu8.default}"
mkdir -p "$HOME/.zotero/zotero/$PROFILE_DIR"

# The stow dir uses the known profile name; symlink if different.
KNOWN="90bt0tu8.default"
if [ "$PROFILE_DIR" != "$KNOWN" ]; then
  ln -sfn "$HOME/.zotero/zotero/$PROFILE_DIR" \
    "$HOME/.zotero/zotero/$KNOWN"
fi

# Stow user.js (symlink is safe — Zotero reads but never rewrites user.js).
stow -t "$HOME" zotero

cd "$HOME" || exit
wget https://github.com/syt2/zotero-addons/releases/latest/download/zotero-addons.xpi
