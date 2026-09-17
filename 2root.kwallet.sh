#!/usr/bin/env sh
#
set -eu

sudo pacman -S --needed kwallet-pam

# system-local-login is provided by pambase. Preserve the package file before
# stow replaces it with the local policy managed in this repository.
if [ -e /etc/pam.d/system-local-login ] && [ ! -L /etc/pam.d/system-local-login ] &&
  [ ! -e /etc/pam.d/system-local-login.pambase ]; then
  sudo mv /etc/pam.d/system-local-login /etc/pam.d/system-local-login.pambase
fi

sudo stow -t / 2root.kwallet
