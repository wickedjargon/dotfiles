#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." &> /dev/null && pwd )"
FAILED=0

TARGET_USER="$1"

if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

if [ -z "$TARGET_USER" ]; then
    echo "Could not find a regular user to test."
    exit 1
fi

HOME_DIR="/home/$TARGET_USER"
echo "Testing display server configuration..."

xinitrc_file="$HOME_DIR/.xinitrc"

if [ -f "$xinitrc_file" ]; then
    if [ ! -x "$xinitrc_file" ]; then
        echo "  [FAIL] $xinitrc_file exists but is not executable"
        FAILED=1
    fi
    
    owner=$(stat -c '%U' "$xinitrc_file")
    if [ "$owner" != "$TARGET_USER" ]; then
        echo "  [FAIL] $xinitrc_file is owned by $owner instead of $TARGET_USER"
        FAILED=1
    fi
else
    echo "  [INFO] $xinitrc_file does not exist. Skipping."
fi

if [ $FAILED -eq 1 ]; then
    exit 1
fi

echo "Display server configuration verified successfully."
exit 0
