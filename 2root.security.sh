#!/usr/bin/env sh
#

#sudo stow -t / 2root.security

# . Disable root login over SSH by editing /etc/ssh/sshd_config and setting:
# PermitRootLogin no

# . Configure AppArmor or SELinux

# . Harden SSH Configuration
#     Edit /etc/ssh/sshd_config to:
# AllowUsers your_username
# Disable password authentication (if you’re using SSH keys):
# PasswordAuthentication no
# Change the default SSH port (optional):
#         Port 2222

yay -S --noconfirm fail2ban
sudo systemctl enable fail2ban
sudo systemctl start fail2ban

# . Enable Firewalld (or add additional rules to nftables)
# yay -S --noconfirm firewalld
# sudo systemctl enable firewalld
# sudo systemctl start firewalld
# Alternatively, if you prefer to stick with nftables, ensure that you limit open ports to only those absolutely necessary.

# . Install and Configure a MAC (Mandatory Access Control) Framework
# In addition to AppArmor, you can enhance security with Linux Security Modules (LSMs), which are supported by the default Arch Linux kernel:
#     Kernel lockdown mode: This can prevent unsigned modules from loading and restrict access to kernel memory.
#     Add lsm=landlock,lockdown,integrity,yama,apparmor to your kernel parameters to enable these LSMs.

# . Configure Sysctl for Network Security
sudo sysctl -p /etc/sysctl.d/99-sysctl.conf

# . Install a Host Intrusion Detection System (HIDS)
# sudo pacman -S aide
# sudo aide --init
# sudo cp /var/lib/aide/aide.db.new /var/lib/aide/aide.db
# Run AIDE periodically to detect unexpected file modifications:
# sudo aide --check

# . Audit logging
yay -S --noconfirm audit
sudo systemctl enable auditd
sudo systemctl start auditd

# . Configure Secure Boot (if supported by your hardware)

# . Check for Vulnerabilities
yay -S --noconfirm lynis
# sudo lynis audit system

# . Restrict USB Access
# sudo modprobe -r usb_storage
# echo "blacklist usb_storage" | sudo tee /etc/modprobe.d/usb_storage.conf

yay -S --noconfirm rkhunter
sudo rkhunter --propupd
sudo rkhunter --update
# rkhunter --check-all --sk --rwo
