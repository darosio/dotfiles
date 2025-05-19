#!/bin/bash

hostname=$(cat /proc/sys/kernel/hostname) # Get the hostname from /proc

# Check if the argument (service unit name) is provided
if [ -z "$1" ]; then
	echo "Usage: $0 <service_unit_name>"
	exit 1
fi

# Service unit name (passed as an argument)
SERVICE_UNIT="$1"

# Email notification settings
RECIPIENT="darosio@duck.com"
SUBJECT="Service Failure Notification - $SERVICE_UNIT"
TIMESTAMP=$(date '+%Y-%m-%d %H:%M:%S')
LOG_OUTPUT=$(journalctl -u "$SERVICE_UNIT" -n 10 --no-pager)
MESSAGE="Service unit '$SERVICE_UNIT' has failed on $HOSTNAME at $TIMESTAMP.\n\nLast logs:\n$LOG_OUTPUT\n"

# Log file path
LOG_FILE="/var/log/failure-notification.log"

# Send email using sendmail
if {
	echo "From: systemd@$hostname"
	echo "To: $RECIPIENT"
	echo "Subject: $SUBJECT"
	echo "Content-Transfer-Encoding: 8bit"
	echo ""
	echo -e "$MESSAGE"
} | sendmail -t; then
	echo "$TIMESTAMP: Failure notification for '$SERVICE_UNIT' sent to $RECIPIENT." >>"$LOG_FILE"
else
	echo "$TIMESTAMP: Failed to send notification for '$SERVICE_UNIT'." >>"$LOG_FILE"
fi
