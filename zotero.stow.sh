#!/usr/bin/env sh
# Install Zotero and apply user preferences.
#
# Managed files:
#   user.js        — stowed (symlink); sets BBT auto-exports + citekey format
#   prefs.js       — copied (not symlinked); Zotero rewrites it at runtime
#   treePrefs.json — copied (not symlinked); Zotero overwrites it at runtime,
#                    breaking symlinks. Re-run this script to restore columns.
#   Both copies are excluded from stow by zotero/.stow-local-ignore.
#
# BBT citation key format:
#   auth.capitalize + year + "_" + authorLast(initials=true).capitalize
# Produces keys like: Vehtari2017_Gabry, Colee2024_LeconteAM
#
# FILE LAYOUT. Identity lives in the directory, not the filename. Zotero names
# the attachment from attachmentRenameTemplate, {{ title truncate="50" }}, and
# ZotMoov then files it under the citation key:
#
#   ~/Sync/biblio/main/Vehtari2017_Gabry/Practical Bayesian model evaluat….pdf
#
# so a title can change, truncate or collide without breaking the link to the
# bibliography, and one key can hold the paper, its supplement and its data.
# ZotMoov settings that implement this (extensions.zotmoov.*): dst_dir =
# ~/Sync/biblio/main, enable_subdir_move = true, subdirectory_string = {%b}.
# The loose pdfs directly under main/ predate this and are a migration backlog.
#
# After first run / key format change:
#   Zotero → Edit → Better BibTeX → Citation Keys → Refresh all keys
#   Then re-trigger all auto-exports (Edit → Better BibTeX → Export).
#   Refreshing rewrites every key, and therefore every citekey folder name and
#   every citation already written elsewhere: change the format deliberately.

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
# prefs.js and treePrefs.json are excluded by zotero/.stow-local-ignore and
# copied below instead: Zotero rewrites both, replacing any symlink.
stow -t "$HOME" zotero
for f in prefs.js treePrefs.json; do
  cp --remove-destination "zotero/.zotero/zotero/90bt0tu8.default/$f" \
    "$HOME/.zotero/zotero/90bt0tu8.default/$f"
done

uv tool install zotero-mcp-server[semantic]

cd "$HOME" || exit
wget https://github.com/syt2/zotero-addons/releases/latest/download/zotero-addons.xpi
