#!/bin/bash

hostname=$(cat /proc/sys/kernel/hostname)  # Get the hostname from /proc

# Check if the argument (service unit name) is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <service_unit_name>"
  exit 1
fi

# Service unit name (passed as an argument)
SERVICE_UNIT="$1"

# Email notification settings
RECIPIENT="darosio@duck.com"
SUBJECT="Service Failure Notification"
MESSAGE="Service unit '$SERVICE_UNIT' has failed."

# Send email using sendmail
(
echo "From: systemd@$hostname"
echo "To: $RECIPIENT"
echo "Subject: $SUBJECT"
echo "Content-Transfer-Encoding: 8bit"
echo ""
echo "$MESSAGE"
) | sendmail -t
