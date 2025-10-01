#!/usr/bin/env sh
#
out=/tmp/$(basename "$0")-$(date +%d%H%M%S)
REPOSITORY=/data/borg
SRC=/data/Sync
EXCLUDE_FILE=$HOME/.local/bin/borg.exclude.txt
thishost=$(uname -n)

{
  echo "To: daniele.arosio@cnr.it"
  echo "From: $thishost"
  echo "Subject: borg backup on $thishost"

  echo "Backing up .."
  borg create -v -C zlib --stat \
    "$REPOSITORY::dan-$(date +%Y-%m-%d)" $SRC \
    --exclude-from "$EXCLUDE_FILE"

  # --prefix dan- is very important to limit prune's operation
  echo "Pruning archives .."
  borg prune -v --list --stats --keep-daily=4 --keep-weekly=2 \
    --keep-monthly=6 --keep-yearly=10 --prefix dan- $REPOSITORY
} > "$out" 2>&1

cat < "$out" | msmtp daniele.arosio@cnr.it
