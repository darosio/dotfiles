#!/bin/sh
#
out=/tmp/`basename "$0"`-`date +%d%H%M%S`
useremail=daniele.arosio@cnr.it

(echo "To: daniele.arosio@cnr.it"
echo "From: $HOSTNAME"
echo "Subject: recollindex from $HOSTNAME")		> $out


#echo "To: daniele.arosio@cnr.it" > /tmp/recoll.out
#echo "From: $HOSTNAME" >> /tmp/recoll.out
#echo "Subject: recollindex from $HOSTNAME" >> /tmp/recoll.out
cat ~/.recoll/idxstatus.txt					   >> $out
(time /usr/bin/recollindex)					   >> $out 2>&1
cat ~/.recoll/idxstatus.txt					   >> $out

# cat $out | msmtp-enqueue.sh daniele.arosio@cnr.it
cat < "$out" | sudo msmtp $useremail
