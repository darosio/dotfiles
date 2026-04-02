#!/usr/bin/env sh
#
yay -S --noconfirm msmtp
yay -S --noconfirm msmtp-mta
sudo rm /etc/msmtprc /etc/msmtp.aliases
sudo stow -t / 2root.msmtp

# Create plaintext password file for root msmtp (used at boot without GPG agent)
if [ ! -f /home/dan/.msmtp-gmail-pass ]; then
  PASSWORD_STORE_DIR=/home/dan/Sync/.pass/ pass email/gmail-isync | head -1 > /home/dan/.msmtp-gmail-pass
  chmod 600 /home/dan/.msmtp-gmail-pass
fi
