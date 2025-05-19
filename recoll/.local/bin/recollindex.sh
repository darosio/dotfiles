#!/usr/bin/env sh
#
out=/tmp/$(basename "$0")-$(date +%d%H%M%S)
useremail=daniele.arosio@cnr.it
hostname=$(cat /proc/sys/kernel/hostname) # Get the hostname from /proc

(
	echo "To: daniele.arosio@cnr.it"
	echo "From: $hostname"
	echo "Subject: recollindex from $hostname"
) >"$out"
# shellcheck disable=SC2129
cat ~/.recoll/idxstatus.txt >>"$out"
(time /usr/bin/recollindex) >>"$out" 2>&1
cat ~/.recoll/idxstatus.txt >>"$out"

# cat $out | msmtp-enqueue.sh daniele.arosio@cnr.it
cat <"$out" | sudo msmtp $useremail
