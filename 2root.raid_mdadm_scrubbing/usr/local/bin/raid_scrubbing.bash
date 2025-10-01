#!/bin/bash
#
out=/tmp/raid_scrubbing.out
hostname=$(cat /proc/sys/kernel/hostname) # Get the hostname from /proc

echo "To: daniele.arosio@cnr.it" > $out
# shellcheck disable=SC2129
echo "From: $hostname" >> $out
echo "Subject: RAID scrubbing on $hostname" >> $out
printf "\nmismatches before: " >> $out
cat /sys/block/*/md/mismatch_cnt >> $out
mdadm --misc --detail /dev/md/* >> $out

echo check >> /sys/block/*/md/sync_action
sleep 5
# shellcheck disable=SC2002 # I don't want changes here
time (while cat /proc/mdstat | grep speed > /dev/null; do
  sleep 10
done) >> $out 2>&1
# shellcheck disable=SC2129
printf "\nmismatches after: " >> $out
cat /sys/block/*/md/mismatch_cnt >> $out
mdadm --misc --detail /dev/md/* >> $out
cat $out | msmtp daniele.arosio@cnr.it
