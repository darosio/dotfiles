#!/bin/sh

if ! ping -c1 www.google.com > /dev/null 2>&1; then
	if ! wget -O -  www.google.com > /dev/null 2>&1; then
		# the end has come
		echo "No internet connection."
		exit 1;
	fi
fi
#$HOME/.progs/bin/msmtp-runqueue.sh 2>&1 >> ~/.msmtp.log
#try also this but
#/home/dan/.progs/bin/msmtp-runqueue.sh >> ~/.msmtp.log

#mbsync -qa

#notmuch new --quiet
notmuch new
afew -mav
afew -tnv
