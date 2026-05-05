#!/usr/bin/env sh
#
set -eu

yay -S --noconfirm msmtp
yay -S --noconfirm msmtp-mta

for file in /etc/msmtprc /etc/msmtp.aliases; do
  if [ -e "$file" ] && [ ! -L "$file" ]; then
    sudo cp "$file" "$file.pre-dotfiles"
  fi
done

sudo stow -t / 2root.msmtp

# Encrypt Gmail password with systemd-creds (TPM2-bound, machine-specific).
# The credential is tied to this machine's TPM — cannot be decrypted on another host.
sudo mkdir -p /etc/credstore
PASSWORD_STORE_DIR=/home/dan/Sync/.pass/ pass email/gmail-isync | head -1 | tr -d '\n' |
  sudo systemd-creds encrypt --name=msmtp-gmail - /etc/credstore/msmtp-gmail.cred
sudo chmod 600 /etc/credstore/msmtp-gmail.cred

# Remove old plaintext fallback
if [ -f /home/dan/.msmtp-gmail-pass ]; then
  mv /home/dan/.msmtp-gmail-pass /home/dan/.msmtp-gmail-pass.pre-dotfiles
fi
