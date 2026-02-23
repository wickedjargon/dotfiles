#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." &> /dev/null && pwd )"
ROOT_DIR="$SCRIPT_DIR/root"
FAILED=0

if [ ! -d "$ROOT_DIR" ]; then
    echo "No root overlay directory found at $ROOT_DIR. Skipping."
    exit 0
fi

# Find the target user
TARGET_USER=$(awk -F':' -v "limit=1000" '{ if ( $3 >= limit && $3 < 65534 && $6 ~ /^\/home\// ) print $1 }' /etc/passwd | tail -n 1)

if [ -z "$TARGET_USER" ]; then
    echo "Could not find a regular user to test."
    exit 1
fi

HOME_DIR="/home/$TARGET_USER"

echo "Testing root file overlays..."

# Find all files in the root directory
find "$ROOT_DIR" -type f | while read -r src_file; do
    # Determine the target system file by stripping $ROOT_DIR
    rel_path="${src_file#$ROOT_DIR}"
    
    # Handle the special 'new-user' home directory substitution
    if [[ "$rel_path" == "/home/new-user/"* ]]; then
        # Replace /home/new-user with actual home directory
        target_file="$HOME_DIR/${rel_path#/home/new-user/}"
        expected_owner="$TARGET_USER"
    else
        target_file="$rel_path"
        expected_owner="root" # Or potentially check parent dir owner, but usually root
    fi
    
    if [ ! -f "$target_file" ]; then
        echo "  [FAIL] Deployed root file missing: $target_file"
        FAILED=1
        continue
    fi
    
    # Verify ownership
    owner=$(stat -c '%U' "$target_file")
    if [ "$owner" != "$expected_owner" ]; then
        # For non-home overlay, exact owner checking might be strict if it was supposed to be
        # owned by someone else, but deploy.py generally chowns home correctly and leaves root ones alone
        echo "  [FAIL] File $target_file is owned by $owner instead of expected $expected_owner"
        FAILED=1
    fi
    
    # Verify file content
    if ! cmp -s "$src_file" "$target_file"; then
        echo "  [FAIL] Content of $target_file does not exactly match source $src_file"
        FAILED=1
    fi
done

if [ $FAILED -eq 1 ]; then
    exit 1
fi

echo "All root file overlays verified successfully."
exit 0
