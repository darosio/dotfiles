#!/usr/bin/env sh
#
# Stow AI container configs to ~/ai-containers/
# Start services with: aic up [service]
mkdir -p "$HOME"/ai-containers
stow -t "$HOME" ai-containers

# Generate khoj .env from pass (password on line 1, key=value on remaining lines)
KHOJ_ENV="$HOME/ai-containers/khoj/.env"
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
