#!/bin/sh
#
out=/tmp/`basename "$0"`-`date +%d%H%M%S`

#sudo mount /media/rsnapshots

echo "To: daniele.arosio@cnr.it"							 > $out
echo "From: $HOSTNAME"										>> $out
echo "Subject: rsync: $HOSTNAME-vigolana"					>> $out

printf "du before sync: ---------------------------\n"			>> $out
sudo du -ksh /media/rsnapshots/*							>> $out
echo "-------------------------------------------"			>> $out
#sudo -u dan -H sh -c "rsync -e '/usr/bin/ssh -c aes256-ctr -i /home/dan/.ssh/dan@sterzing-2013-06-03 -p 23456' -a -H --progress --delete --numeric-ids --exclude '/lost+found/' --exclude '/remotes/' --exclude '/Sync/' /media/rsnapshots/ 10.0.0.1:/data/"  > /tmp/vigolana.daily.out 2>&1
# --checksum
printf "\n\nSync $HOSTNAME to vigolana\n"					>> $out
echo "/rnapshots to /data (exept Sync, borg, remotes)..."	>> $out
echo														>> $out
rsync -i -a -H --info=progress0,stats2,del,name,flist --delete --numeric-ids \
	/media/rsnapshots/ 10.0.0.1:/data/ \
	--exclude '/lost+found/' \
	--exclude '/remotes/' \
	--exclude '/Sync/' \
	--exclude '/borg/' \
	-e '/usr/bin/ssh -c aes256-ctr -i ~/.ssh/dan@sterzing-2013-06-03 -p 23456' >> $out 2>&1

#printf
echo														>> $out
echo "du after sync: ----------------------------"			>> $out
sudo du -ksh /media/rsnapshots/*							>> $out
echo "-------------------------------------------"			>> $out

#sudo umount /media/rsnapshots

cat $out | msmtp daniele.arosio@cnr.it
