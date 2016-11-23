#!/bin/sh

if ! ping -c1 www.google.com > /dev/null 2>&1; then
	if ! wget -O -  www.google.com > /dev/null 2>&1; then
		# the end has come
		echo "No internet connection."
		exit 1;
	fi
fi

#try also this but
eval "$(keychain --eval --agents ssh,gpg C08BC79E3A3F1D89AD90C0008B6A39EFA290FB41)"
#bash /usr/share/doc/msmtp/msmtpqueue/msmtp-runqueue.sh 2>&1 >> ~/.msmtp.log
#$HOME/.progs/bin/msmtp-runqueue.sh 2>&1 >> ~/.msmtp.log
#try also this but
/home/dan/.progs/bin/msmtp-runqueue.sh >> ~/.msmtp.log

mbsync -qa

notmuch new --quiet

afew -a -m
