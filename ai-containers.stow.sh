#!/usr/bin/env sh
#
set -eu

# Stow AI container configs to ~/ai-containers/
# (~/ai-containers -> repo/ai-containers/ai-containers, per stow)
# Quadlet units for the podman systemd generator are stowed too:
# ~/ai-containers/systemd/users/1000/*.container
stow -t "$HOME" ai-containers

# Point the quadlet generator at the stowed units (recursive search under
# this path finds users/1000/). After changing unit files:
#   systemctl --user daemon-reload
mkdir -p "$HOME/.config/containers"
ln -sfn "$HOME/ai-containers/systemd" "$HOME/.config/containers/systemd"

# Weekly auto-update override timer (stow skips dotfiles, so link it
# explicitly; quadlet ignores .timer files in its own search path).
mkdir -p "$HOME/.config/systemd/user"
ln -sfn "$HOME/ai-containers/systemd/user-units/podman-auto-update.timer" \
  "$HOME/.config/systemd/user/podman-auto-update.timer"

# LiteLLM cloud keys (repo-free path; empty when a pass entry is absent).
LITELLM_ENV="$HOME/.config/containers/litellm.env"
PSD="${PASSWORD_STORE_DIR:-$HOME/.password-store}"
export PASSWORD_STORE_DIR="$HOME/Sync/.pass"
{
  printf 'OPENAI_API_KEY=%s\n' "$(pass show home/openai-dpa 2> /dev/null | head -1)"
  printf 'OPENCODE_ZEN_API_KEY=%s\n' "$(pass show cloud/opencode_zen 2> /dev/null | head -1)"
  printf 'OPENCODE_GO_API_KEY=%s\n' "$(pass show cloud/opencode_go 2> /dev/null | head -1)"
} > "$LITELLM_ENV"
chmod 600 "$LITELLM_ENV"
export PASSWORD_STORE_DIR="$PSD"

# Generate khoj env from pass (password on line 1, key=value on remaining
# lines). Kept in ~/.config/containers/ so the secret never sits in the
# stowed (repo) tree — ~/ai-containers IS the repo worktree.
KHOJ_ENV="$HOME/.config/containers/khoj.env"
PASS_ENTRY=$(PASSWORD_STORE_DIR="$HOME/Sync/.pass" pass ai/khoj)
KHOJ_PASSWORD=$(echo "$PASS_ENTRY" | head -1)
KHOJ_EMAIL=$(echo "$PASS_ENTRY" | sed -n 's/^KHOJ_ADMIN_EMAIL=//p')
KHOJ_SECRET=$(echo "$PASS_ENTRY" | sed -n 's/^KHOJ_DJANGO_SECRET_KEY=//p')

cat > "$KHOJ_ENV" << EOF
KHOJ_DJANGO_SECRET_KEY=$KHOJ_SECRET
KHOJ_ADMIN_EMAIL=$KHOJ_EMAIL
KHOJ_ADMIN_PASSWORD=$KHOJ_PASSWORD
EOF
chmod 600 "$KHOJ_ENV"
