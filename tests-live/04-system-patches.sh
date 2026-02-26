#!/bin/sh

SCRIPT_DIR="$(cd "$(dirname "$0")/.." >/dev/null 2>&1 && pwd)"
PATCHES_DIR="$SCRIPT_DIR/patches"
FAILED=0

if [ ! -d "$PATCHES_DIR" ]; then
    echo "No patches directory found at $PATCHES_DIR. Skipping."
    exit 0
fi

echo "Testing system patches..."

# Use a temp file to track failures across subshell boundaries
fail_flag=$(mktemp)
echo 0 > "$fail_flag"

# Find all files in the patches directory
find "$PATCHES_DIR" -type f | while read -r patch_file; do
    # Determine the target system file by stripping $PATCHES_DIR
    rel_path="${patch_file#"$PATCHES_DIR"}"
    target_file="$rel_path" # It already starts with / e.g. /etc/systemd/...
    
    if [ ! -f "$target_file" ]; then
        echo "  [FAIL] Target system file for patch does not exist: $target_file"
        echo 1 > "$fail_flag"
        continue
    fi
    
    # Read each non-empty, non-comment line from the patch file
    while IFS= read -r line || [ -n "$line" ]; do
        # Skip empty lines
        trimmed=$(echo "$line" | xargs)
        [ -z "$trimmed" ] && continue
        
        # We need to verify that this exact line exists in the target file.
        # Use -x to match the whole line, avoiding false positives.
        if ! grep -Fxq "$line" "$target_file"; then
            echo "  [FAIL] Missing patched line in $target_file"
            echo "         Expected to find: '$line'"
            echo 1 > "$fail_flag"
        fi
    done < "$patch_file"
done

FAILED=$(cat "$fail_flag")
rm -f "$fail_flag"

if [ "$FAILED" -eq 1 ]; then
    exit 1
fi

echo "All system patches verified successfully."
exit 0
