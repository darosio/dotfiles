#!/usr/bin/env sh
#

yay -S --noconfirm clamav

sudo rm /etc/clamav/clamd.conf
sudo rm /etc/clamav/virus-event.bash
sudo rm /usr/lib/systemd/system/clamav-clamonacc.service
sudo rm /etc/sudoers.d/clamav

sudo stow -t / 2root.clamav

sudo rm /etc/sudoers.d/clamav
sudo cp 2root.clamav/etc/sudoers.d/clamav /etc/sudoers.d/clamav
sudo chown root:root /etc/sudoers.d/clamav

sudo freshclam
sudo systemctl enable clamav-daemon.service
sudo systemctl start clamav-daemon.service
sudo systemctl enable clamav-freshclam.service
sudo systemctl start clamav-freshclam.service
sudo systemctl enable clamav-clamonacc.service
sudo systemctl start clamav-clamonacc.service

# yay -S --noconfirm python-fangfrisch
sudo -u clamav /usr/bin/fangfrisch --conf /etc/fangfrisch/fangfrisch.conf initdb
sudo systemctl enable fangfrisch.timer
sudo systemctl start fangfrisch.timer
