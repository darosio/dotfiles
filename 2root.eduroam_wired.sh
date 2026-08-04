#!/usr/bin/env sh
#
# Wired eduroam (802.1X, PEAP/MSCHAPv2) on whisker's ethernet port.
#
# A switch port running 802.1X stays unauthorized until wpa_supplicant
# completes the EAPOL exchange. Until then the link has carrier but DHCP is
# never answered, which shows up as a lone 169.254.0.0/16 link-local address.
#
# Arch ships wpa_supplicant-wired@.service, which runs
#   wpa_supplicant -c /etc/wpa_supplicant/wpa_supplicant-wired-%I.conf -Dwired -i%I
# so this script only has to render that config and enable the unit.
#
# The password is NEVER stored in this repo. It is read from pass (default
# entry email/fbk), falling back to $EDUROAM_PASSWORD or an echo-off prompt
# when the store is unavailable, and written to a 0600 root-owned file.
#
# This is a laptop and the cable also goes into non-eduroam networks. That is
# safe: wpa_supplicant never gates the port itself, the switch does, so on a
# jack without 802.1X it emits a few unanswered EAPOL-Start frames while DHCP
# proceeds normally. Leaving the unit enabled everywhere is therefore fine.
#
# What roaming does change is the value of EDUROAM_CA. Without server-cert
# validation, a rogue authenticator on an untrusted wired network can solicit
# the PEAP exchange and capture an MSCHAPv2 challenge/response to crack
# offline. Run --probe once on campus, then keep EDUROAM_CA=1 set.
#
# Usage:
#   ./2root.eduroam_wired.sh                 # password from pass email/fbk
#   ./2root.eduroam_wired.sh --probe         # show the RADIUS server cert
#   IFACE=eno1 ./2root.eduroam_wired.sh      # different interface
#   EDUROAM_PASS_ENTRY=email/cnr ./2root.eduroam_wired.sh   # other account
#   EDUROAM_CA=1 EDUROAM_DOMAIN=fbk.eu ./2root.eduroam_wired.sh   # validated

set -eu

IFACE=${IFACE:-enp2s0}
# FBK realm. The outer (anonymous) realm must match so eduroam routes the
# request to the right home RADIUS server.
IDENTITY=${EDUROAM_IDENTITY:-darosio@fbkeduroam.it}
ANON=${EDUROAM_ANON:-anonymous@fbkeduroam.it}
PASS_ENTRY=${EDUROAM_PASS_ENTRY:-email/fbk}
CONF=/etc/wpa_supplicant/wpa_supplicant-wired-$IFACE.conf

if ! ip link show "$IFACE" > /dev/null 2>&1; then
  echo "error: interface $IFACE not found" >&2
  exit 1
fi

# --probe: dump the RADIUS server certificate so ca_cert/domain_suffix_match can
# be pinned to something real. The server cert arrives before the client
# authenticates, so this works even if the password is wrong. Run it on a
# trusted campus port only -- that is the whole point of establishing a
# baseline to validate against later.
if [ "${1:-}" = "--probe" ]; then
  if [ ! -f "$CONF" ]; then
    echo "error: $CONF does not exist yet; run this script without --probe first" >&2
    exit 1
  fi
  echo "Probing $IFACE for 15s (stop any running supplicant first) ..."
  sudo systemctl stop "wpa_supplicant-wired@$IFACE" 2> /dev/null || true
  probe_log=$(mktemp)
  trap 'rm -f "$probe_log"' EXIT INT TERM
  # SC2024: the redirect is meant to run unprivileged; probe_log is a mktemp
  # file owned by the invoking user, and only wpa_supplicant needs root.
  # shellcheck disable=SC2024
  sudo timeout 15 wpa_supplicant -c "$CONF" -D wired -i "$IFACE" -d > "$probe_log" 2>&1 || true
  sudo systemctl start "wpa_supplicant-wired@$IFACE" 2> /dev/null || true

  grep -iE 'subject=|altsubject|PEER-CERT' "$probe_log" | sort -u || true

  # depth=0 is the server certificate itself, which is what hash://server pins.
  srv_hash=$(sed -n 's/.*depth=0 .*hash=\([0-9a-f]\{64\}\).*/\1/p' "$probe_log" | head -n 1)
  srv_dom=$(sed -n 's/.*PEER-ALT depth=0 DNS:\(.*\)/\1/p' "$probe_log" | head -n 1)
  echo
  if [ -n "$srv_hash" ]; then
    echo "Pin this server certificate with:"
    echo "  EDUROAM_CA=1 EDUROAM_DOMAIN=${srv_dom:-<domain>} \\"
    echo "  EDUROAM_CA_HASH=$srv_hash \\"
    echo "  ./2root.eduroam_wired.sh"
  else
    echo "No depth=0 certificate hash seen; is the port running 802.1X?"
  fi
  exit 0
fi

# Precedence: $EDUROAM_PASSWORD, then pass, then an echo-off prompt. The prompt
# is the fallback for when the store is not there yet, e.g. a fresh reinstall
# before Sync has landed.
if [ -z "${EDUROAM_PASSWORD:-}" ] && command -v pass > /dev/null 2>&1; then
  EDUROAM_PASSWORD=$(pass show "$PASS_ENTRY" 2> /dev/null | head -n 1) || EDUROAM_PASSWORD=''
  if [ -n "$EDUROAM_PASSWORD" ]; then
    echo "Password read from pass $PASS_ENTRY"
  else
    echo "warning: could not read pass $PASS_ENTRY, falling back to prompt" >&2
  fi
fi

if [ -z "${EDUROAM_PASSWORD:-}" ]; then
  printf 'eduroam password for %s: ' "$IDENTITY" >&2
  stty -echo 2> /dev/null || true
  read -r EDUROAM_PASSWORD
  stty echo 2> /dev/null || true
  printf '\n' >&2
fi

# wpa_supplicant quotes string values, so an embedded quote or backslash would
# silently corrupt the config. Refuse rather than write something broken.
case $EDUROAM_PASSWORD in
  *\"* | *\\*)
    echo 'error: password contains a quote or backslash; use password=hash:<NT-hash> instead' >&2
    exit 1
    ;;
  '')
    echo 'error: empty password' >&2
    exit 1
    ;;
esac

# Server-certificate validation is opt-in because it needs values that only
# --probe can report. Without it the credentials are offered to any server that
# answers, which is the real risk when this laptop is plugged into a jack that
# is not FBK's.
if [ -n "${EDUROAM_CA:-}" ]; then
  # No default domain: guessing the realm wrong fails authentication in a way
  # that looks like a password problem. --probe reports the real value.
  if [ -z "${EDUROAM_DOMAIN:-}" ]; then
    echo 'error: EDUROAM_CA=1 requires EDUROAM_DOMAIN (run --probe to find it)' >&2
    exit 1
  fi
  # FBK's RADIUS cert is self-signed under a private CA (CN=sysops@fbk.eu), so
  # the system bundle cannot validate it. wpa_supplicant's hash://server pin
  # compares the server certificate directly, which is what EDUROAM_CA_HASH
  # uses. Note this must be re-probed whenever FBK rotates that certificate.
  if [ -n "${EDUROAM_CA_HASH:-}" ]; then
    ca_ref="hash://server/sha256/$EDUROAM_CA_HASH"
  else
    ca_ref=${EDUROAM_CA_FILE:-/etc/ssl/certs/ca-certificates.crt}
  fi
  ca_lines='    ca_cert="'$ca_ref'"
    domain_suffix_match="'$EDUROAM_DOMAIN'"'
else
  ca_lines='    # EDUROAM_CA=1 to validate the RADIUS server certificate'
fi

echo "Writing $CONF ..."
umask 077
tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT INT TERM

cat > "$tmp" << EOF
# Managed by 2root.eduroam_wired.sh -- do not commit, contains a password.
ctrl_interface=/run/wpa_supplicant
# Wired driver does not scan; ap_scan must be 0.
ap_scan=0
# Some switches only answer EAPOL v1. If authentication never starts, try 1.
eapol_version=2

network={
    key_mgmt=IEEE8021X
    # No dynamic WEP keying on a wired port.
    eapol_flags=0
    eap=PEAP
    phase2="auth=MSCHAPV2"
    identity="$IDENTITY"
    anonymous_identity="$ANON"
    password="$EDUROAM_PASSWORD"
$ca_lines
}
EOF

sudo install -o root -g root -m 600 "$tmp" "$CONF"

echo "Enabling wpa_supplicant-wired@$IFACE ..."
# enable --now is not enough on a re-run: it leaves an already-running
# supplicant on the old config. Restart explicitly so the file just written is
# the one actually loaded.
sudo systemctl enable "wpa_supplicant-wired@$IFACE"
sudo systemctl restart "wpa_supplicant-wired@$IFACE"

# networkd's DHCP client gave up while the port was unauthorized; restart it
# now that EAPOL has had a chance to complete.
echo "Waiting for authentication ..."
sleep 8
sudo networkctl reconfigure "$IFACE" > /dev/null || true
sleep 5

echo
systemctl is-active --quiet "wpa_supplicant-wired@$IFACE" &&
  echo "supplicant: running" ||
  echo "supplicant: NOT running -- journalctl -u wpa_supplicant-wired@$IFACE"
ip -4 -br addr show "$IFACE"
echo
echo "A 169.254.x.x address still means the port did not authorize."
echo "Debug with: sudo wpa_cli -i $IFACE status"
