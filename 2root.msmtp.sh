#!/usr/bin/env sh
#
yay -S --noconfirm msmtp
yay -S --noconfirm msmtp-mta
sudo rm -f /etc/msmtprc /etc/msmtp.aliases
sudo stow -t / 2root.msmtp

# Encrypt Gmail password with systemd-creds (TPM2-bound, machine-specific).
# The credential is tied to this machine's TPM — cannot be decrypted on another host.
sudo mkdir -p /etc/credstore
PASSWORD_STORE_DIR=/home/dan/Sync/.pass/ pass email/gmail-isync | head -1 | tr -d '\n' |
  sudo systemd-creds encrypt --name=msmtp-gmail - /etc/credstore/msmtp-gmail.cred
sudo chmod 600 /etc/credstore/msmtp-gmail.cred

# Remove old plaintext fallback
rm -f /home/dan/.msmtp-gmail-pass
