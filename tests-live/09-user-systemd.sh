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
echo "Testing user systemd services..."

if command -v systemctl >/dev/null 2>&1 && command -v loginctl >/dev/null 2>&1; then
    user_systemd_dir="$HOME_DIR/.config/systemd/user"
    if [ -d "$user_systemd_dir" ]; then
        # Just check if files are present and owned by user
        find "$user_systemd_dir" -type f -name "*.service" | while read -r svc; do
            svc_name=$(basename "$svc")
            owner=$(stat -c '%U' "$svc")
            if [ "$owner" != "$TARGET_USER" ]; then
                echo "  [FAIL] user service $svc_name is owned by $owner instead of $TARGET_USER"
                FAILED=1
            fi
        done
        
        # We can also check if particular services we expect are enabled by looking at symlinks in default.target.wants etc.
    else
        echo "  [INFO] No custom user systemd services directory found."
    fi
else
    echo "  [SKIP] systemctl or loginctl not found."
fi

if [ $FAILED -eq 1 ]; then
    exit 1
fi

echo "User systemd configurations verified successfully."
exit 0
