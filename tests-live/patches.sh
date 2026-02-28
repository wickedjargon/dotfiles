#!/bin/sh

# Verify: patch files from patches/ are applied to their system targets.

SCRIPT_DIR="$(cd "$(dirname "$0")/.." >/dev/null 2>&1 && pwd)"
PATCHES_DIR="$SCRIPT_DIR/patches"

if [ ! -d "$PATCHES_DIR" ]; then
    echo "No patches directory. Skipping."
    exit 0
fi

FAILED=0
echo "Testing system patches..."

# Use a temp file to track failures across subshell boundaries
fail_flag=$(mktemp)
echo 0 > "$fail_flag"

find "$PATCHES_DIR" -type f | while read -r patch_file; do
    rel_path="${patch_file#"$PATCHES_DIR"}"
    target_file="$rel_path"

    if [ ! -f "$target_file" ]; then
        echo "  [FAIL] Target file missing: $target_file"
        echo 1 > "$fail_flag"
        continue
    fi

    while IFS= read -r line || [ -n "$line" ]; do
        trimmed=$(echo "$line" | xargs)
        [ -z "$trimmed" ] && continue
        # Skip comment lines (deploy_patches ignores these too)
        case "$trimmed" in '#'*) continue ;; esac

        if ! grep -Fxq "$line" "$target_file"; then
            echo "  [FAIL] Missing line in $target_file"
            echo "         Expected: '$line'"
            echo 1 > "$fail_flag"
        fi
    done < "$patch_file"
done

FAILED=$(cat "$fail_flag")
rm -f "$fail_flag"

[ "$FAILED" -eq 1 ] && exit 1
echo "All patches verified."
exit 0
