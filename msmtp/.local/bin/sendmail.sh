#!/bin/sh

if ! ping -c1 www.google.com > /dev/null 2>&1; then
	if ! wget -O -  www.google.com > /dev/null 2>&1; then
		# the end has come
		echo "No internet connection."
		exit 1;
	fi
fi

export PASSWORD_STORE_DIR=~/Sync/.pass
msmtp-runqueue.sh 2>&1 >> ~/.sendmail.log
