#!/usr/bin/env sh
#
mkdir -p "$HOME"/.config/environment.d
stow -t "$HOME" goldendict

# sdcv, the StarDict CLI that Emacs fronts, searches only /usr/share/stardict/dic
# and ~/.stardict/dic. STARDICT_DATA_DIR (environment.d/41-goldendict.conf) covers
# what the systemd user manager starts, but not a shell that predates the session
# environment -- `sdcv word` in an already-open terminal finds nothing. Linking the
# dictionaries into the default path works for every process regardless of env.
# The dictionaries themselves live in a separate git-annex repo, so this only
# links what that repo provides.
mkdir -p "$HOME"/.stardict
ln -sfn "$HOME"/.goldendict/dict_ "$HOME"/.stardict/dic
