#!/bin/sh

echo "$(date) — $1 $2" >> /tmp/nm-test.log

case "$2" in
    up|down|dhcp4-change|dhcp6-change|vpn-up|vpn-down|connectivity-change)
        sleep 1
        pkill -RTMIN+7 dwmblocks
        ;;
esac
