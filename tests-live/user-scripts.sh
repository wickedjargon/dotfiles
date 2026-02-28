#!/bin/sh

# Verify: all scripts deployed to ~/.local/bin/ are executable and owned by the user.

TARGET_USER="$1"
if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

HOME_DIR="/home/$TARGET_USER"
LOCAL_BIN="$HOME_DIR/.local/bin"

if [ ! -d "$LOCAL_BIN" ]; then
    echo "No ~/.local/bin directory. Skipping."
    exit 0
fi

FAILED=0
echo "Testing user scripts in ~/.local/bin/..."

fail_flag=$(mktemp)
echo 0 > "$fail_flag"

find "$LOCAL_BIN" -type f | while read -r script_file; do
    script_name=$(basename "$script_file")
    
    # Ownership check
    owner=$(stat -c '%U' "$script_file")
    if [ "$owner" != "$TARGET_USER" ]; then
        echo "  [FAIL] $script_name owned by $owner instead of $TARGET_USER"
        echo 1 > "$fail_flag"
    fi
    
    # Executable permissions check
    if [ ! -x "$script_file" ]; then
        echo "  [FAIL] $script_name is not executable (+x)"
        echo 1 > "$fail_flag"
    fi
done

FAILED=$(cat "$fail_flag")
rm -f "$fail_flag"

[ "$FAILED" -eq 1 ] && exit 1
echo "User scripts verified."
exit 0
