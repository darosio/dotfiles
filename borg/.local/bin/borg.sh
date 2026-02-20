#!/usr/bin/env sh
#
# --- Configuration ---
out="/tmp/$(basename "$0")-$(date +%d%H%M%S)"
# Point this to your new nested mount or subvolume path
REPOSITORY="/data/borg"
# Use the subvolume path directly
SRC_SUBVOL="/data/Sync"
# Temporary location for the snapshot
SNAP_PATH="/data/.sync_snapshot"

EXCLUDE_FILE="$HOME/.local/bin/borg.exclude.txt"
thishost=$(uname -n)

{
  echo "To: daniele.arosio@cnr.it"
  echo "From: $thishost"
  echo "Subject: borg backup on $thishost"

  # 1. Take snapshot
  echo "Creating Btrfs snapshot..."
  btrfs subvolume snapshot -r "$SRC_SUBVOL" "$SNAP_PATH"

  # 2. CD into the snapshot so the paths look "standard" to Borg
  cd "$SNAP_PATH"

  # 3. Back up the CURRENT directory (.)
  echo "Backing up from snapshot.."
  borg create -v -C zlib --stat \
    "$REPOSITORY::dan-$(date +%Y-%m-%d)" . \
    --exclude-from "$EXCLUDE_FILE"

  # 4. Cleanup
  echo "Removing snapshot..."
  cd /
  btrfs subvolume delete "$SNAP_PATH"

  echo "Pruning archives .."
  # --prefix dan- is very important to limit prune's operation
  borg prune -v --list --stats --keep-daily=4 --keep-weekly=2 \
    --keep-monthly=6 --keep-yearly=10 --prefix dan- "$REPOSITORY"

} > "$out" 2>&1

msmtp daniele.arosio@cnr.it < "$out"

