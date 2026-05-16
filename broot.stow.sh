#!/usr/bin/env sh
#
# Install broot as a lightweight tree/search companion to yazi.
# Keep side effects minimal: package install + shell integration via stow.

# Check if the script is already running in an isolated environment
if [ -z "$ISOLATED_ENV" ]; then
  env -i HOME="$HOME" USER="$USER" PATH="/usr/bin:$PATH" ISOLATED_ENV=1 "$0" "$@"
  exit
fi

# Clear any lingering variables that might interfere
unset PYTHONPATH

yay -S --noconfirm broot

mkdir -p "$HOME"/.bashrc.d
stow -t "$HOME" broot
