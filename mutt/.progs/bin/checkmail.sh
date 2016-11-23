#!/bin/sh

if ! ping -c1 www.google.com > /dev/null 2>&1; then
	if ! wget -O -  www.google.com > /dev/null 2>&1; then
		# the end has come
		echo "No internet connection."
		exit 1;
	fi
fi

#bash /usr/share/doc/msmtp/msmtpqueue/msmtp-runqueue.sh 2>&1 >> ~/.msmtp.log
#$HOME/.progs/bin/msmtp-runqueue.sh 2>&1 >> ~/.msmtp.log
/home/dan/.progs/bin/msmtp-runqueue.sh 2>&1 >> ~/.msmtp.log

mbsync -qa

notmuch new --quiet

afew -a -m
