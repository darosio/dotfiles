#!/usr/bin/env sh
#
stow -t "$HOME" goldendict

# sdcv, the StarDict CLI that Emacs fronts, searches only /usr/share/stardict/dic
# and ~/.stardict/dic, neither of which exists here. Linking the dictionaries into
# the default path needs no environment variable, so it holds for shells, ssh,
# cron and any Emacs however it was started. The dictionaries live in a separate
# git-annex repo, so this only links what that repo provides.
mkdir -p "$HOME"/.stardict
ln -sfn "$HOME"/.goldendict/dict_ "$HOME"/.stardict/dic
