#!/bin/sh

# Verify: home directory recursive ownership (spot checks after chown -R)

TARGET_USER="$1"
if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

HOME_DIR="/home/$TARGET_USER"
FAILED=0

echo "Testing home directory permissions..."

# Spot check critical parent directories first
for dir in ".config" ".local" ".local/src" ".mozilla"; do
    target="$HOME_DIR/$dir"
    if [ -d "$target" ]; then
        owner=$(stat -c '%U' "$target")
        if [ "$owner" != "$TARGET_USER" ]; then
            echo "  [FAIL] Directory ~/$dir owned by $owner instead of $TARGET_USER"
            FAILED=1
        fi
    fi
done

# Try a deeper spot-check of some common config directories
for dir in "bspwm" "sxhkd" "polybar" "Antigravity"; do
    target="$HOME_DIR/.config/$dir"
    if [ -d "$target" ]; then
        owner=$(stat -c '%U' "$target")
        if [ "$owner" != "$TARGET_USER" ]; then
            echo "  [FAIL] Directory ~/.config/$dir owned by $owner instead of $TARGET_USER"
            FAILED=1
        fi
    fi
done

[ $FAILED -eq 1 ] && exit 1
echo "Home directory permissions verified."
exit 0
