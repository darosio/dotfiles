#!/bin/sh
#
bname=`basename "$0"`
out=/tmp/$bname-`date +%d%H%M%S`

# mount rsnapshots
# check virtualbox is off

#borg init --encryption=none /media/rsnapshots/borg-hdd
REPOSITORY=/media/rsnapshots/borg-hdd
SRC=/data/Sync

echo "To: daniele.arosio@cnr.it"							 > $out
echo "From: $HOSTNAME"										>> $out
echo "Subject: $bname backup on $HOSTNAME"					>> $out

#`date +%Y-%m-%d`               \
printf "\nBackup ~/.origin70 ...\n" 						>> $out
borg create --info --stats -C zlib,9    --list        \
    $REPOSITORY::origin70-20160712 ~/.origin70				>> $out 2>&1
printf "\nBackup ~/.office2007 ...\n" 						>> $out
borg create --info --stats -C zlib,9    --list        \
    $REPOSITORY::office2007-20160712 ~/.office2007			>> $out 2>&1
printf "\nBackup ~/.virtualmachines ...\n" 						>> $out
borg create --info --stats -C zlib,3    --list        \
    $REPOSITORY::virtualmachines000 ~/.virtualmachines	    >> $out 2>&1
printf "\nBackup ~/.virtualmachines ...\n" 						>> $out
borg create --info --stats -C zlib,3    --list        \
	$REPOSITORY::virtualmachines-XP-7-10 ~/.virtualmachines	    >> $out 2>&1

printf "\nNO Pruning archives...\n"							>> $out
#borg prune -v $REPOSITORY --prefix dan- \
#	--keep-daily=4 --keep-weekly=2 --keep-monthly=6 --keep-yearly=10 
cat $out | msmtp daniele.arosio@cnr.it
