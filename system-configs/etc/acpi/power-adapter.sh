#!/bin/sh

# Small delay to let the kernel update power state
sleep 1

# Refresh your battery block (signal 2 in your config)
pkill -RTMIN+2 dwmblocks
