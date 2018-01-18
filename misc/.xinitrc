#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Start a D-Bus session it was generating serious problem
#source /etc/X11/xinit/xinitrc.d/30-dbus
# Start GNOME Keyring
##DD eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
# You probably need to do this too:
##DD export SSH_AUTH_SOCK
##DD export GPG_AGENT_INFO
##DD export GNOME_KEYRING_CONTROL
##DD export GNOME_KEYRING_PID

# exec gnome-session
# exec openbox-kde-session

# ...or the Window Manager of your choice
#exec enlightenment_start
#exec startxfce4
#if [ -s ~/.Xmodmap ]; then
	setxkbmap -layout 'us(intl)' # needed to use mod5
	setxkbmap -layout us,it -option grp:shifts_toggle &
	xmodmap ~/.Xmodmap
#fi
#eval "$(keychain --eval --agents ssh,gpg C08BC79E3A3F1D89AD90C0008B6A39EFA290FB41)" &
exec i3
