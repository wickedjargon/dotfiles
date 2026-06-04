#!/bin/sh
# Reset the Elan i2c device (touchpad + trackpoint) without rebooting.
# Fixes a firmware stall where the trackpoint/touchpad stops responding.
# Run with: sudo sh fix-trackpoint.sh
echo 0-0015 > /sys/bus/i2c/drivers/elan_i2c/unbind
sleep 1
echo 0-0015 > /sys/bus/i2c/drivers/elan_i2c/bind
