#!/bin/sh
#
out=/tmp/`basename "$0"`-`date +%d%H%M%S`


(echo "To: daniele.arosio@cnr.it"
echo "From: $HOSTNAME"
echo "Subject: rsync: $HOSTNAME-vigolana")					 > $out


#sudo mount /media/rsnapshots
(echo
echo "Checking mountpoint /media/rsnapshots:"
~/.local/bin/mount_rsnapshots.sh)							>> $out 2>&1
# Either "1" because of mount rsnapshots problem 
# or "127" because command script was ot found.
if [ $? -eq 0 ]; then
	echo "du before sync: ---------------------------"
	sudo du -ksh /media/rsnapshots/*
	echo "-------------------------------------------"
	
	# vigolana:/data/borg to sterzing:/media/rsnapshots
	printf "\n\nSync vigolana:/data/borg to $HOSTNAME:rsnapshots\n"
	echo
	rsync -i -a -H --info=progress0,stats2,del,name,flist --numeric-ids \
		--delete 10.0.0.1:/data/borg/ /media/rsnapshots/borg/ \
		-e '/usr/bin/ssh -c aes256-ctr -i ~/.ssh/dan@sterzing-2013-06-03 -p 23456'
	# --checksum
	
	# sterzing:/media/rsnapshots to vigolana:/data
	printf "\n\nSync $HOSTNAME to vigolana\n"
	echo "/rnapshots to /data (exept Sync, borg, remotes)..."
	echo
	rsync -i -a -H --info=progress0,stats2,del,name,flist --numeric-ids \
		--delete /media/rsnapshots/ 10.0.0.1:/data/ \
		--exclude '/lost+found/' \
		--exclude '/remotes/' \
		--exclude '/Sync/' \
		--exclude '/borg/' \
		-e '/usr/bin/ssh -c aes256-ctr -i ~/.ssh/dan@sterzing-2013-06-03 -p 23456'
	
	echo
	echo "du after sync: ----------------------------"
	sudo du -ksh /media/rsnapshots/*
	echo "-------------------------------------------"
fi															>> $out 2>&1

#sudo umount /media/rsnapshots

(echo
echo "### Clean Syncronizations (syncthing) ###")			>> $out
find ~/Sync/ -name \*sync-conflict\*  | xargs ls -lF		>> $out

cat $out | grep -v "+++" | grep -v "\.\.\." | msmtp daniele.arosio@cnr.it
