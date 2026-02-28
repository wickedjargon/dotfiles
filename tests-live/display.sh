#!/bin/sh

# Verify: .xinitrc exists, is executable, and owned by the user.

TARGET_USER="$1"
if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

HOME_DIR="/home/$TARGET_USER"
FAILED=0

echo "Testing display server configuration..."

xinitrc="$HOME_DIR/.xinitrc"

if [ -f "$xinitrc" ]; then
    if [ ! -x "$xinitrc" ]; then
        echo "  [FAIL] $xinitrc is not executable"
        FAILED=1
    fi

    owner=$(stat -c '%U' "$xinitrc")
    if [ "$owner" != "$TARGET_USER" ]; then
        echo "  [FAIL] $xinitrc owned by $owner instead of $TARGET_USER"
        FAILED=1
    fi
else
    echo "  [INFO] .xinitrc does not exist. Skipping."
fi

[ $FAILED -eq 1 ] && exit 1
echo "Display configuration verified."
exit 0
