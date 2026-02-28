#!/bin/sh

# Verify: all files from root/ overlay are deployed with correct ownership.

TARGET_USER="$1"
if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "$0")/.." >/dev/null 2>&1 && pwd)"
ROOT_DIR="$SCRIPT_DIR/root"

if [ ! -d "$ROOT_DIR" ]; then
    echo "No root overlay directory. Skipping."
    exit 0
fi

HOME_DIR="/home/$TARGET_USER"

# Files managed by the theme script that are legitimately modified after deploy.
# We still check existence and ownership, but skip content comparison.
THEME_MANAGED="
.config/gtk-3.0/settings.ini
.config/gtk-4.0/settings.ini
.config/Kvantum/kvantum.kvconfig
.config/qt5ct/qt5ct.conf
.config/qt6ct/qt6ct.conf
.config/Antigravity/User/settings.json
"

echo "Testing root file overlays..."

fail_flag=$(mktemp)
echo 0 > "$fail_flag"

find "$ROOT_DIR" -type f | while read -r src_file; do
    rel_path="${src_file#"$ROOT_DIR"}"

    case "$rel_path" in
        /home/new-user/*)
            target_file="$HOME_DIR/${rel_path#/home/new-user/}"
            expected_owner="$TARGET_USER"
            ;;
        *)
            target_file="$rel_path"
            expected_owner="root"
            ;;
    esac

    if [ ! -f "$target_file" ]; then
        echo "  [FAIL] Missing: $target_file"
        echo 1 > "$fail_flag"
        continue
    fi

    owner=$(stat -c '%U' "$target_file")
    if [ "$owner" != "$expected_owner" ]; then
        echo "  [FAIL] $target_file owned by $owner instead of $expected_owner"
        echo 1 > "$fail_flag"
    fi

    # Skip content check for theme-managed files
    skip_content=false
    case "$rel_path" in
        /home/new-user/*)
            home_rel="${rel_path#/home/new-user/}"
            for tf in $THEME_MANAGED; do
                [ -z "$tf" ] && continue
                if [ "$home_rel" = "$tf" ]; then
                    skip_content=true
                    break
                fi
            done
            ;;
    esac

    if [ "$skip_content" = false ] && ! cmp -s "$src_file" "$target_file"; then
        echo "  [FAIL] Content mismatch: $target_file"
        echo 1 > "$fail_flag"
    fi
done

FAILED=$(cat "$fail_flag")
rm -f "$fail_flag"

[ "$FAILED" -eq 1 ] && exit 1
echo "All root overlays verified."
exit 0
