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

sudo stow -t / 2root.security

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
# cd /etc/audit/rules.d
# sudo curl -O https://raw.githubusercontent.com/linux-audit/audit-userspace/master/rules/30-stig.rules
# ❯     sudo auditctl -D
# ❯     sudo augenrules
# ❯     sudo auditctl -R /etc/audit/audit.rules
sudo augenrules
sudo systemctl enable auditd
sudo systemctl start auditd
# Use:
# ❯ sudo aureport
# ❯ sudo aureport --failed -au -i
# ❯ sudo aureport --syscall --failed
# ❯ sudo ausearch -sv no -i | tail -n 20
# ❯ sudo aureport -a

# . Configure Secure Boot (if supported by your hardware)

# . Check for Vulnerabilities
yay -S --noconfirm lynis
# Use:
# sudo lynis audit system

# . Restrict USB Access
# sudo modprobe -r usb_storage
# echo "blacklist usb_storage" | sudo tee /etc/modprobe.d/usb_storage.conf

yay -S --noconfirm rkhunter
sudo rkhunter --propupd
sudo rkhunter --update
# rkhunter --check-all --sk --rwo
# Append the custom whitelist configuration for Arch Linux
sudo tee -a /etc/rkhunter.conf << 'EOF'
# --- START OF CUSTOM CONFIGURATION ---
# Whitelist modern compatibility scripts on Arch Linux
SCRIPTWHITELIST=/usr/bin/egrep
SCRIPTWHITELIST=/usr/bin/fgrep
SCRIPTWHITELIST=/usr/bin/ldd
# Allow known benign hidden files
ALLOWHIDDENFILE=/etc/.updated
ALLOWHIDDENFILE=/usr/share/man/man5/.k5identity.5.gz
ALLOWHIDDENFILE=/usr/share/man/man5/.k5login.5.gz
# My SSH config is handled by a systemd drop-in, so skip the static file check.
DISABLE_TESTS="system_configs_ssh"
# --- END OF CUSTOM CONFIGURATION ---
EOF
