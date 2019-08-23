#!/bin/sh
#
out=/tmp/$(basename "$0")-$(date +%d%H%M%S)
REPOSITORY=/data/borg
SRC=/data/Sync
thishost=$(uname -n)

{
    echo "To: daniele.arosio@cnr.it"
    echo "From: $thishost"
    echo "Subject: borg backup on $thishost"
    echo "Backing up .."
    borg create --info --stats -C zlib --list \
         "$REPOSITORY::dan-$(date +%Y-%m-%d)" $SRC \
         --exclude $SRC/tmp --exclude '*/.stversions' --exclude '*.pyc'
        # --exclude '*/Sync/tmp' \
    echo "Pruning archives .."
    # --prefix dan- is very important to limit prune's operation to this
    # machine's archives and not apply to other machine's archives also.
    borg prune --info --list --stats $REPOSITORY --prefix dan- \
	       --keep-daily=4 --keep-weekly=2 --keep-monthly=6 --keep-yearly=10
} > "$out" 2>&1


# cat $out | grep -v "/*/" | msmtp daniele.arosio@cnr.it
cat < "$out" | msmtp daniele.arosio@cnr.it
