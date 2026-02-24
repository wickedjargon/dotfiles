#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." &> /dev/null && pwd )"
PATCHES_DIR="$SCRIPT_DIR/patches"
FAILED=0

if [ ! -d "$PATCHES_DIR" ]; then
    echo "No patches directory found at $PATCHES_DIR. Skipping."
    exit 0
fi

echo "Testing system patches..."

# Find all files in the patches directory
while read -r patch_file; do
    # Determine the target system file by stripping $PATCHES_DIR
    rel_path="${patch_file#$PATCHES_DIR}"
    target_file="$rel_path" # It already starts with / e.g. /etc/systemd/...
    
    if [ ! -f "$target_file" ]; then
        echo "  [FAIL] Target system file for patch does not exist: $target_file"
        FAILED=1
        continue
    fi
    
    # Read each non-empty, non-comment line from the patch file
    while IFS= read -r line || [ -n "$line" ]; do
        # Skip empty lines
        [[ -z $(echo "$line" | xargs) ]] && continue
        
        # We need to verify that this exact line exists in the target file.
        # This handles both active configuration lines and comments that we injected.
        # Use -x to match the whole line, avoiding false positives where our patch
        # is a substring of an existing commented out line.
        if ! grep -Fxq "$line" "$target_file"; then
            echo "  [FAIL] Missing patched line in $target_file"
            echo "         Expected to find: '$line'"
            FAILED=1
        fi
    done < "$patch_file"
done < <(find "$PATCHES_DIR" -type f)

if [ $FAILED -eq 1 ]; then
    exit 1
fi

echo "All system patches verified successfully."
exit 0
