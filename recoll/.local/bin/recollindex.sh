#!/bin/sh

echo "To: daniele.arosio@cnr.it" > /tmp/recoll.out
echo "From: $HOSTNAME" >> /tmp/recoll.out
echo "Subject: recollindex from $HOSTNAME" >> /tmp/recoll.out
cat ~/.recoll/idxstatus.txt >> /tmp/recoll.out
time /usr/bin/recollindex >> /tmp/recoll.out 2>&1
cat ~/.recoll/idxstatus.txt >> /tmp/recoll.out
cat /tmp/recoll.out | msmtp daniele.arosio@cnr.it
