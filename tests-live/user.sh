#!/bin/sh

# Verify: user exists, has a home directory, is in sudo group.

TARGET_USER="$1"
if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

FAILED=0
echo "Testing user setup..."

if ! id "$TARGET_USER" >/dev/null 2>&1; then
    echo "  [FAIL] User $TARGET_USER does not exist"
    exit 1
fi

if [ ! -d "/home/$TARGET_USER" ]; then
    echo "  [FAIL] Home directory /home/$TARGET_USER does not exist"
    FAILED=1
fi

owner=$(stat -c '%U' "/home/$TARGET_USER")
if [ "$owner" != "$TARGET_USER" ]; then
    echo "  [FAIL] Home directory owned by $owner instead of $TARGET_USER"
    FAILED=1
fi

if ! groups "$TARGET_USER" | grep -qE "\b(sudo|wheel)\b"; then
    echo "  [FAIL] User $TARGET_USER is not in sudo or wheel group"
    FAILED=1
fi

[ $FAILED -eq 1 ] && exit 1
echo "User setup verified."
exit 0
