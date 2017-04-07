#!/bin/sh
#
mount="/media/rsnapshots"
#if grep -qs "$mount" /proc/mounts; then
if mountpoint -q "$mount"; then
	echo "$mount is a mountpoint"
else
	echo "$mount is not a mountpoint"
    sudo mount "$mount"
    if [ $? -eq 0 ]; then
     echo "Mount success!"
    else
     echo "Something went wrong with the mount..."
     exit 1
    fi
fi
