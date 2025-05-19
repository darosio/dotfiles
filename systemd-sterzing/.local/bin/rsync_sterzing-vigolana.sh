#!/usr/bin/env sh
#
out=/tmp/$(basename "$0")-$(date +%d%H%M%S)
rsnapshots_path=/mnt/rsnapshots
useremail=daniele.arosio@cnr.it
thishost=$(uname -n)

{
	echo "To: daniele.arosio@cnr.it"
	echo "From: $thishost"
	echo "Subject: rsync  $thishost <--> vigolana"

	echo "Checking rsnapshots mountpoint."

	# exit "1" for rsnapshots mount problem or "127" when command script is not found
	if ~/.local/bin/mount_rsnapshots.sh; then
		echo
		echo "Before sync: ------------------------------"
		sudo du -ksh $rsnapshots_path/*
		echo "-------------------------------------------"

		# vigolana:/data/borg --> sterzing: rsnapshots/borg
		printf "\n\nSync vigolana:/data/borg --> %s: rsnapshots/borg\n" "$thishost"
		# echo
		rsync -i -a -H --info=progress0,stats2,del,name,flist --numeric-ids \
			--delete 10.0.0.1:/data/borg/ $rsnapshots_path/borg/ \
			-e '/usr/bin/ssh -c aes256-ctr -i ~/.ssh/dan@sterzing-2013-06-03 -p 23456'
		# --checksum
		# sterzing: rsnapshots --> vigolana:/data
		printf "\n\nSync %s: rsnapshots --> vigolana:/data [except: borg, remotes, Sync]\n" "$thishost"
		# echo
		rsync -i -a -H --info=progress0,stats2,del,name,flist --numeric-ids \
			--exclude-from "$HOME/.local/bin/rsync_sterzing-vigolana.exclude.txt" \
			--delete $rsnapshots_path/ 10.0.0.1:/data/ \
			-e '/usr/bin/ssh -c aes256-ctr -i ~/.ssh/dan@sterzing-2013-06-03 -p 23456'
		#     	    --exclude '/Sync/' \

		echo
		echo "After sync: -------------------------------"
		sudo du -ksh $rsnapshots_path/*
		echo "-------------------------------------------"
	fi
} >"$out" 2>&1

# umount rsnapshots ?
{
	echo
	echo "### Clean Synchronizations (syncthing) ###"
	find ~/Sync/ -name \*sync-conflict\* -exec ls -lF {} +
} >>"$out" 2>&1

cat <"$out" | sudo msmtp $useremail
