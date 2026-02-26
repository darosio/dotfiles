#!/bin/bash

while eval; do
  while ping -c1 omero > /dev/null 2>&1; do
    sleep 30
  done
  echo "restarting..."
  fbkvpn.sh
  echo "restarted..."
  sleep 2
done
