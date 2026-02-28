#!/bin/sh

# Verify: built source repositories placed expected binaries in PATH.

TARGET_USER="$1"
if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

FAILED=0
echo "Testing built binaries..."

# Define expected binaries for known source repos
# Format: repo_name:binary1,binary2,...
EXPECTED="
clipmenu:clipmenu,clipmenud
sent:sent
xob:xob
"

# We check system path because 'make install' usually puts them in /usr/local/bin
for item in $EXPECTED; do
    [ -z "$item" ] && continue
    
    repo_name="${item%%:*}"
    binaries="${item##*:}"
    
    IFS=','
    for bin in $binaries; do
        if ! command -v "$bin" >/dev/null 2>&1; then
            echo "  [FAIL] Expected binary '$bin' from $repo_name not found in PATH"
            FAILED=1
        fi
    done
    unset IFS
done

[ $FAILED -eq 1 ] && exit 1
echo "Built binaries verified."
exit 0
