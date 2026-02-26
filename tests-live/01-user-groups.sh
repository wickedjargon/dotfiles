#!/bin/sh

# Test 1: Verify the created user has the correct groups and home directory

# Find the highest UID regular user (assuming it's the one created by deploy.py)
TARGET_USER="$1"

if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

if [ -z "$TARGET_USER" ]; then
    echo "Could not find a regular user (UID >= 1000) in /home to test."
    exit 1
fi

echo "Testing user: $TARGET_USER"

# 1. Check if user exists
if ! id "$TARGET_USER" >/dev/null 2>&1; then
    echo "User $TARGET_USER does not exist"
    exit 1
fi

# 2. Check if user has home directory
if [ ! -d "/home/$TARGET_USER" ]; then
    echo "Home directory /home/$TARGET_USER does not exist"
    exit 1
fi

# 3. Check if user is in sudo or wheel group
if ! groups "$TARGET_USER" | grep -qE "\b(sudo|wheel)\b"; then
    echo "User $TARGET_USER is not in 'sudo' or 'wheel' group"
    exit 1
fi

# 4. Check if user owns their home directory
owner=$(stat -c '%U' "/home/$TARGET_USER")
if [ "$owner" != "$TARGET_USER" ]; then
    echo "Home directory /home/$TARGET_USER is owned by $owner instead of $TARGET_USER"
    exit 1
fi

echo "User $TARGET_USER verified successfully."
exit 0
