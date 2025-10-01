#!/usr/bin/env sh
#
bname=$(basename "$0")
out=/tmp/$bname-$(date +%d%H%M%S)
#borg init --encryption=none /media/rsnapshots/borg-hdd
REPOSITORY=/media/rsnapshots/borg-hdd
hostname=$(cat /proc/sys/kernel/hostname) # Get the hostname from /proc

echo "To: daniele.arosio@cnr.it" > "$out"
# shellcheck disable=SC2129
echo "From: $(hostname)" >> "$out"
echo "Subject: $bname backup on $hostname" >> "$out"

#`date +%Y-%m-%d`               \
printf "\nBackup ~/.origin70 ...\n" >> "$out"
borg create --info --stats -C zlib,9 --list \
  $REPOSITORY::origin70-20160712 ~/.origin70 >> "$out" 2>&1
printf "\nBackup ~/.office2007 ...\n" >> "$out"
borg create --info --stats -C zlib,9 --list \
  $REPOSITORY::office2007-20160712 ~/.office2007 >> "$out" 2>&1
printf "\nBackup ~/.virtualmachines ...\n" >> "$out"
borg create --info --stats -C zlib,3 --list \
  $REPOSITORY::virtualmachines000 ~/.virtualmachines >> "$out" 2>&1
printf "\nBackup ~/.virtualmachines ...\n" >> "$out"
borg create --info --stats -C zlib,3 --list \
  $REPOSITORY::virtualmachines-XP-7-10 ~/.virtualmachines >> "$out" 2>&1

printf "\nNO Pruning archives...\n" >> "$out"
#borg prune -v $REPOSITORY --prefix dan- \
#	--keep-daily=4 --keep-weekly=2 --keep-monthly=6 --keep-yearly=10
msmtp daniele.arosio@cnr.it < "$out"
