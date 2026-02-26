#!/bin/sh

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
        # Use a temp file to track failures across subshell boundaries
        fail_flag=$(mktemp)
        echo 0 > "$fail_flag"

        # Just check if files are present and owned by user
        find "$user_systemd_dir" -type f -name "*.service" | while read -r svc; do
            svc_name=$(basename "$svc")
            owner=$(stat -c '%U' "$svc")
            if [ "$owner" != "$TARGET_USER" ]; then
                echo "  [FAIL] user service $svc_name is owned by $owner instead of $TARGET_USER"
                echo 1 > "$fail_flag"
            fi
        done
        
        FAILED=$(cat "$fail_flag")
        rm -f "$fail_flag"
    else
        echo "  [INFO] No custom user systemd services directory found."
    fi
else
    echo "  [SKIP] systemctl or loginctl not found."
fi

if [ "$FAILED" -eq 1 ]; then
    exit 1
fi

echo "User systemd configurations verified successfully."
exit 0
