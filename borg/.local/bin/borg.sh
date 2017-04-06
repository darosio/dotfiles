#!/bin/sh
#
out=/tmp/`basename "$0"`-`date +%d%H%M%S`
REPOSITORY=/data/borg
SRC=/data/Sync

echo "To: daniele.arosio@cnr.it" 				 > $out
echo "From: $HOSTNAME" 						>> $out
echo "Subject: borg backup on $HOSTNAME"			>> $out

printf "\nBackup...\n" 						>> $out
#	--exclude $SRC/tmp
borg create --info --stats -C zlib    --list        \
    $REPOSITORY::dan-`date +%Y-%m-%d`               \
    $SRC	--exclude '*/tmp'                   \
		--exclude '*/.stversions'   	    \
		--exclude '*.pyc' 				>> $out 2>&1

printf "\nPruning archives...\n"				>> $out
# Use the `prune` subcommand to maintain 7 daily, 4 weekly and 6 monthly
# archives of THIS machine. --prefix `hostname`- is very important to
# limit prune's operation to this machine's archives and not apply to
# other machine's archives also.
#borg prune -v $REPOSITORY --prefix `hostname`- \
borg prune --info --list --stats $REPOSITORY --prefix dan- \
	--keep-daily=4 --keep-weekly=2 --keep-monthly=6    \
	--keep-yearly=10 					>> $out 2>&1

cat $out | msmtp daniele.arosio@cnr.it
