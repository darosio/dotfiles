#!/usr/bin/env sh
#
# Tailscale overlay network, for remote access only.
#
# Deliberately NOT the way the FBK machines talk to each other. On campus
# aai, sterzing and vigolana are reached directly by address via /etc/hosts,
# which is simpler and keeps traffic off a third-party coordination service.
#
# This exists so aai can be reached from home, where the FBK network is not
# routable at all. Tailscale NAT-traverses, so it needs no inbound port and no
# VPN concentrator.
#
# Install on: whisker (the client, used from home) and aai (the target).
# Not on sterzing or vigolana, which are only ever used from on campus.
#
# Requires 2root.nftables, which accepts traffic on tailscale0. Without that
# rule the default-drop input policy silently blocks every peer.

set -eu

yay -S --noconfirm tailscale

sudo systemctl enable --now tailscaled.service

# --operator lets this user run `tailscale` without sudo.
# --ssh is deliberately NOT set: this host runs its own hardened sshd on 23456
# (see sshd.sh), and Tailscale SSH would bypass that key-only policy.
sudo tailscale up --operator="$USER"

echo
tailscale status
echo
echo "From home, reach aai at aai.snow-arowana.ts.net (or its 100.x address)."
echo "On campus keep using the /etc/hosts names -- they take precedence over"
echo "MagicDNS anyway, since resolved reads /etc/hosts before querying DNS."
