#!/usr/bin/env sh
# Himalaya email CLI — agent-facing interface alongside mu4e
# Reads from ~/Maildir/ (shared with mbsync/mu4e), sends via msmtp.
# Install: uv tool install himalaya  OR  cargo install himalaya --locked
yay -S --noconfirm himalaya
stow -t "$HOME" himalaya
