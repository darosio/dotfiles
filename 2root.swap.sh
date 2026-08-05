#!/usr/bin/env sh
#
# Modest swap, low swappiness, PSI-based OOM handling. For aai (123 GiB RAM,
# btrfs root on nvme1n1p2).
#
# 16 GiB, not 123 GiB. The "swap = RAM" rule is a hibernation requirement and
# is irrelevant on a compute workstation. swappiness=10 keeps the kernel from
# preemptively paging out hot MCMC arrays. systemd-oomd uses PSI to kill on
# sustained pressure stall before thrashing becomes pathological, which
# neutralises the main argument against having swap at all.
#
# btrfs constraints: a swapfile must be NOCOW, uncompressed, preallocated, and
# on a subvolume that is never snapshotted -- hence the dedicated /swap
# subvolume rather than a file under an existing one.
#
# NOTE ON INSTALL METHOD: this package deliberately does NOT use `stow -t /`.
# Everything here is consumed before /home is mounted -- systemd-sysctl runs
# ~2 s before home.mount, and PID 1 parses unit drop-ins earlier still -- so a
# symlink into /home/dan/workspace/dotfiles is a dangling link at that point
# and is silently skipped. The files are installed as real copies instead; the
# repo stays the source of truth and this script is idempotent, so re-run it
# after editing anything under 2root.swap/.
#
# WHAT THIS DOES NOT DO. Swap is a safety net, not the fix for a fit grid that
# overshoots. The control that actually contains overshoot is a cgroup bound on
# the job itself, which works whether or not swap exists:
#
#   systemd-run --user --scope \
#     -p MemoryHigh=80G -p MemoryMax=100G -p OOMScoreAdjust=500 \
#     bash scripts/run_noise_comparison_staged.sh
#
# MemoryHigh throttles by forcing reclaim, MemoryMax is the hard wall, and
# OOMScoreAdjust=500 makes the grid the preferred victim. Together they
# guarantee the failure lands on a fit rather than on syncthing. Running the
# job in its own scope is also what lets the oomd drop-in below pick it, rather
# than something else in the session, as the cgroup to kill.
#
# Strict overcommit (vm.overcommit_memory=2) is deliberately not set. It sounds
# attractive -- allocations fail as a catchable Python MemoryError instead of
# SIGKILL -- but NumPy/PyTensor make large virtual reservations they never
# fault in, so strict accounting breaks scientific stacks well before the limit
# is real. Leave it at 0.

set -eu

DOTFILES=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)
PKG="$DOTFILES/2root.swap"

SUBVOL=/swap
SWAPFILE=$SUBVOL/swapfile
SWAPSIZE=16g

# --- swapfile ---------------------------------------------------------------

if ! sudo btrfs subvolume show "$SUBVOL" > /dev/null 2>&1; then
  sudo btrfs subvolume create "$SUBVOL"
fi

# +C on the directory so anything created inside inherits NOCOW. It only takes
# effect on files that are still empty, which is why it precedes creation.
sudo chattr +C "$SUBVOL"

if [ ! -e "$SWAPFILE" ]; then
  # mkswapfile does the NOCOW + no-compression + preallocate + mkswap dance in
  # one step (btrfs-progs >= 6.1). The hand-rolled truncate/chattr/fallocate
  # sequence is equivalent but easier to get subtly wrong.
  sudo btrfs filesystem mkswapfile --size "$SWAPSIZE" --uuid clear "$SWAPFILE"
fi
sudo chmod 600 "$SWAPFILE"

if ! grep -q "^${SWAPFILE}[[:space:]]" /etc/fstab; then
  printf '\n# 16 GiB swapfile on a NOCOW, never-snapshotted btrfs subvolume\n%s none swap defaults 0 0\n' \
    "$SWAPFILE" | sudo tee -a /etc/fstab > /dev/null
fi

if ! swapon --show=NAME --noheadings | grep -qx "$SWAPFILE"; then
  sudo systemctl daemon-reload # pick up the new fstab entry
  sudo swapon "$SWAPFILE"
fi

# --- swappiness -------------------------------------------------------------

sudo install -Dm644 "$PKG/etc/sysctl.d/99-swap.conf" /etc/sysctl.d/99-swap.conf
sudo sysctl -p /etc/sysctl.d/99-swap.conf

# --- systemd-oomd -----------------------------------------------------------

sudo install -Dm644 "$PKG/etc/systemd/system/-.slice.d/10-oomd-root-slice.conf" \
  /etc/systemd/system/-.slice.d/10-oomd-root-slice.conf
sudo install -Dm644 "$PKG/etc/systemd/system/user@.service.d/10-oomd-user-service.conf" \
  /etc/systemd/system/user@.service.d/10-oomd-user-service.conf

sudo systemctl daemon-reload
sudo systemctl enable --now systemd-oomd.service

# --- verify -----------------------------------------------------------------

echo
swapon --show
echo
sysctl vm.swappiness
echo
# Should list -.slice under "Swap Monitored CGroups" and user@.service under
# "Memory Pressure Monitored CGroups". An empty list means oomd is running but
# watching nothing.
oomctl
