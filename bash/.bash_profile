# shellcheck shell=bash

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
  # shellcheck source=/dev/null
  . "$HOME"/.bashrc
fi
# shellcheck source=/dev/null
[[ -f ~/.profile ]] && source "$HOME"/.profile
# shellcheck source=/dev/null
[[ -f ~/.progs/bash_profile ]] && . "$HOME"/.progs/bash_profile

# User specific environment and startup programs
PATH=$HOME/.local/bin:$PATH
export PATH

set -a
# shellcheck source=/dev/null
source <(/usr/lib/systemd/user-environment-generators/30-systemd-environment-d-generator)
set +a

systemctl --user import-environment PATH

# if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
# 	startx
# fi
if [ -z "$WAYLAND_DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  exec sway
fi
