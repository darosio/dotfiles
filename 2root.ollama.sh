#!/usr/bin/env sh
#
# Install ollama, configure systemd override, migrate models to /home

set -eu

SCRIPT_DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)

# Install
yay -S --noconfirm ollama

# Create model storage dir owned by the ollama user
sudo mkdir -p /home/ollama/models
sudo chown -R ollama:ollama /home/ollama

# Migrate existing models if present
if [ -d /var/lib/ollama/models ] && [ "$(ls -A /var/lib/ollama/models 2> /dev/null)" ]; then
  echo "Migrating models from /var/lib/ollama/models → /home/ollama/models ..."
  sudo rsync -av --remove-source-files /var/lib/ollama/models/ /home/ollama/models/
fi

# Stow host-specific systemd override
# Sets OLLAMA_HOST, OLLAMA_MODELS, OLLAMA_CONTEXT_LENGTH (65536 on workstation, 8192 on laptop)
sudo mkdir -p /etc/systemd/system/ollama.service.d
if [ "$(uname -n)" = "whisker" ]; then
  cd 2root.ollama.whisker || exit 1
else
  cd "$SCRIPT_DIR/2root.ollama" || exit 1
fi
sudo cp --parents etc/systemd/system/ollama.service.d/override.conf /
cd "$SCRIPT_DIR"

sudo systemctl daemon-reload
sudo systemctl enable --now ollama
