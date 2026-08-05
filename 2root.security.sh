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

# /etc/sysctl.d is read by systemd-sysctl, which finishes ~2 s before /home is
# mounted. A stow symlink into /home/dan/workspace/dotfiles is therefore
# dangling at that moment and systemd-sysctl skips it silently, so none of the
# hardening below survived a reboot -- only the explicit `sysctl -p` further
# down ever applied it. Install that one file as a real copy instead, and hide
# it from stow so the two do not fight over the same target. Re-run this script
# after editing 2root.security/etc/sysctl.d/99-sysctl.conf.
#
# The rest of the package (audit rules, fail2ban) is read by services that
# start well after /home is mounted, so symlinks are fine there.
#
# rkhunter.conf.local is likewise excluded and installed as a real copy further
# down: a security scanner should not read its own config through a symlink
# into a user-writable checkout.
sudo stow -t / \
  --ignore='99-sysctl\.conf' \
  --ignore='rkhunter\.conf\.local' \
  2root.security
sudo install -Dm644 2root.security/etc/sysctl.d/99-sysctl.conf \
  /etc/sysctl.d/99-sysctl.conf

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

# The custom whitelist goes in rkhunter.conf.local, which rkhunter reads from
# the same directory as the main config. It used to be appended to
# /etc/rkhunter.conf with `tee -a`, which was not idempotent -- every re-run of
# this script added another copy of the block -- and which also diverged a
# pacman backup file, so upgrades would start producing .pacnew conflicts.
#
# One-time cleanup on a host that ran the old version: delete the
# "--- START OF CUSTOM CONFIGURATION ---" .. "--- END ... ---" block from
# /etc/rkhunter.conf, or restore the packaged file with
# `sudo pacman -S --overwrite /etc/rkhunter.conf rkhunter`.
sudo install -Dm600 2root.security/etc/rkhunter.conf.local \
  /etc/rkhunter.conf.local

sudo rkhunter --update
# --propupd baselines file properties, so it runs last, once the config that
# decides which files are in scope is in place.
sudo rkhunter --propupd
# rkhunter --check-all --sk --rwo
