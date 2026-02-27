#!/bin/sh

SCRIPT_DIR="$(cd "$(dirname "$0")/.." >/dev/null 2>&1 && pwd)"
ROOT_DIR="$SCRIPT_DIR/root"

if [ ! -d "$ROOT_DIR" ]; then
    echo "No root overlay directory found at $ROOT_DIR. Skipping."
    exit 0
fi

# Find the target user
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

# Files managed by the theme script that are legitimately modified after deploy.
# We still check existence and ownership, but skip content comparison.
THEME_MANAGED_FILES="
.config/gtk-3.0/settings.ini
.config/gtk-4.0/settings.ini
.config/Kvantum/kvantum.kvconfig
.config/qt5ct/qt5ct.conf
.config/qt6ct/qt6ct.conf
.config/Antigravity/User/settings.json
"

echo "Testing root file overlays..."

# Use a temp file to track failures across subshell boundaries
fail_flag=$(mktemp)
echo 0 > "$fail_flag"

# Find all files in the root directory
find "$ROOT_DIR" -type f | while read -r src_file; do
    # Determine the target system file by stripping $ROOT_DIR
    rel_path="${src_file#"$ROOT_DIR"}"
    
    # Handle the special 'new-user' home directory substitution
    case "$rel_path" in
        /home/new-user/*)
            # Replace /home/new-user with actual home directory
            target_file="$HOME_DIR/${rel_path#/home/new-user/}"
            expected_owner="$TARGET_USER"
            ;;
        *)
            target_file="$rel_path"
            expected_owner="root"
            ;;
    esac
    
    if [ ! -f "$target_file" ]; then
        echo "  [FAIL] Deployed root file missing: $target_file"
        echo 1 > "$fail_flag"
        continue
    fi
    
    # Verify ownership
    owner=$(stat -c '%U' "$target_file")
    if [ "$owner" != "$expected_owner" ]; then
        echo "  [FAIL] File $target_file is owned by $owner instead of expected $expected_owner"
        echo 1 > "$fail_flag"
    fi
    
    # Skip byte-for-byte content check for theme-managed files
    skip_content=false
    case "$rel_path" in
        /home/new-user/*)
            home_rel="${rel_path#/home/new-user/}"
            for tf in $THEME_MANAGED_FILES; do
                [ -z "$tf" ] && continue
                if [ "$home_rel" = "$tf" ]; then
                    skip_content=true
                    break
                fi
            done
            ;;
    esac

    # Verify file content
    if [ "$skip_content" = false ] && ! cmp -s "$src_file" "$target_file"; then
        echo "  [FAIL] Content of $target_file does not exactly match source $src_file"
        echo 1 > "$fail_flag"
    fi
done

FAILED=$(cat "$fail_flag")
rm -f "$fail_flag"

if [ "$FAILED" -eq 1 ]; then
    exit 1
fi

echo "All root file overlays verified successfully."
exit 0
