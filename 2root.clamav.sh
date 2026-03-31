#!/usr/bin/env sh
#

# Ensure we're not using a virtual environment's python for system packages
if [ -n "$VIRTUAL_ENV" ]; then
  echo "Warning: Virtual environment detected ($VIRTUAL_ENV). Bypassing for system installation..."
  # Attempt to clean PATH by removing the venv bin directory
  PATH="$(echo "$PATH" | sed -e "s|$VIRTUAL_ENV/bin:||g")"
  export PATH
  unset VIRTUAL_ENV
fi

# We need the system python and build module for AUR packages
yay -S --noconfirm clamav python-fangfrisch

sudo rm -f /etc/clamav/clamd.conf
sudo rm -f /etc/clamav/virus-event.bash
sudo rm -f /usr/lib/systemd/system/clamav-clamonacc.service
sudo rm -f /etc/sudoers.d/clamav
sudo rm -f /etc/fangfrisch/fangfrisch.conf

# sudo stow -t / 2root.clamav

# Ensure clamav user can read configs (symlinks point into /home which has restrictive perms)
# Copy config files to override symlinks with regular files
sudo cp 2root.clamav/etc/clamav/clamd.conf /etc/clamav/clamd.conf
sudo chown root:clamav /etc/clamav/clamd.conf
sudo chmod 640 /etc/clamav/clamd.conf

sudo cp 2root.clamav/etc/clamav/virus-event.bash /etc/clamav/virus-event.bash
sudo chown root:clamav /etc/clamav/virus-event.bash
sudo chmod 750 /etc/clamav/virus-event.bash

sudo cp 2root.clamav/usr/lib/systemd/system/clamav-clamonacc.service /usr/lib/systemd/system/clamav-clamonacc.service
sudo chown root:root /usr/lib/systemd/system/clamav-clamonacc.service
sudo chmod 644 /usr/lib/systemd/system/clamav-clamonacc.service
sudo systemctl daemon-reload

sudo cp 2root.clamav/etc/sudoers.d/clamav /etc/sudoers.d/clamav
sudo chown root:root /etc/sudoers.d/clamav

sudo mkdir -p /etc/fangfrisch
sudo cp 2root.clamav/etc/fangfrisch/fangfrisch.conf /etc/fangfrisch/fangfrisch.conf
sudo chown root:clamav /etc/fangfrisch/fangfrisch.conf
sudo chmod 640 /etc/fangfrisch/fangfrisch.conf

sudo mkdir -p /root/quarantine
sudo chmod 700 /root/quarantine

sudo touch /var/log/clamav/freshclam.log
sudo chown clamav:clamav /var/log/clamav/freshclam.log
sudo chmod 600 /var/log/clamav/freshclam.log

# Handle fangfrisch database initialization
if [ ! -d /var/lib/fangfrisch ]; then
  sudo mkdir -p /var/lib/fangfrisch
  sudo chown clamav:clamav /var/lib/fangfrisch
fi

# Try to initialize. If it fails due to existing DB, we just continue.
# If you want to force re-init, run: sudo rm /var/lib/fangfrisch/db.sqlite
sudo -u clamav /usr/bin/fangfrisch --conf /etc/fangfrisch/fangfrisch.conf initdb || true

sudo systemctl stop clamav-daemon.service
sudo systemctl stop clamav-freshclam.service
sudo systemctl stop clamav-clamonacc.service
sudo systemctl stop fangfrisch.timer

sudo freshclam
sudo systemctl enable --now clamav-daemon.service
sudo systemctl enable --now clamav-freshclam.service
sudo systemctl enable --now clamav-clamonacc.service
sudo systemctl enable --now fangfrisch.timer

# Trigger initial refresh
sudo -u clamav /usr/bin/fangfrisch --conf /etc/fangfrisch/fangfrisch.conf refresh
