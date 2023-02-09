#!/usr/bin/env sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
	# shellcheck source=/dev/null
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources

#must be before xmodmap
#setxkbmap -layout us\(intl\),it -option grp:shifts_toggle
setxkbmap -layout us,it -option grp:shifts_toggle
#setxkbmap -layout us,it,us -variant ,,dvorak -option grp:shifts_toggle
[ -f ~/.Xmodmap ] && xmodmap ~/.Xmodmap

# ...or the Window Manager of your choice
exec udiskie --tray &
exec i3
#eval $(dbus-launch --sh-syntax)
#exec dbus-launch --sh-syntax --exit-with-session i3  # freeze using (xback)light
#exec ck-launch-session dbus-launch --sh-syntax --exit-with-session i3
