#!/usr/bin/env sh
#
sudo stow -t / 2root.raid_mdadm_scrubbing
if ! yay -Q mdadm >/dev/null 2>&1; then
	yay -S --noconfirm mdadm
fi
# DEVICE partitions
sudo mdadm --examine --scan | sudo tee -a /etc/mdadm.conf
echo "MAILADDR root" | sudo tee -a /etc/mdadm.conf
sudo mdadm --monitor --scan --oneshot --test
sudo systemctl start raid_scrubbing.timer
sudo systemctl enable raid_scrubbing.timer
