#!/bin/sh

(pass email/fbk | sudo openconnect --juniper sslgate.fbk.eu -u darosio --passwd-on-stdin) &
pid=$!
#while ping -c1 vigolana > /dev/null 2>&1;
#do
	sleep 5
#done

echo "Process ID: " $pid
sudo ip route del 10.0.0.0/16 dev tun0
wait $pid
