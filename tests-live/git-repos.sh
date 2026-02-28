#!/bin/sh

# Verify: git source repos are cloned and built, dotfile repos are cloned.

TARGET_USER="$1"
if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "$0")/.." >/dev/null 2>&1 && pwd)"
HOME_DIR="/home/$TARGET_USER"
SRC_DIR="$HOME_DIR/.local/src"
FAILED=0

echo "Testing built source repositories..."
while IFS= read -r url; do
    url=$(echo "$url" | sed 's/#.*//' | xargs)
    [ -z "$url" ] && continue

    repo_name=$(basename -s .git "$url")
    repo_dir="$SRC_DIR/$repo_name"

    if [ ! -d "$repo_dir" ]; then
        echo "  [FAIL] Missing source repo: $repo_dir"
        FAILED=1
        continue
    fi

    owner=$(stat -c '%U' "$repo_dir")
    if [ "$owner" != "$TARGET_USER" ]; then
        echo "  [FAIL] $repo_dir owned by $owner instead of $TARGET_USER"
        FAILED=1
    fi

    if [ ! -d "$repo_dir/.git" ]; then
        echo "  [FAIL] $repo_dir is not a git repository"
        FAILED=1
    fi
done < "$SCRIPT_DIR/packages/debian-git-packages-src.txt"

echo "Testing cloned dotfile repositories..."
while IFS= read -r line; do
    line=$(echo "$line" | sed 's/#.*//' | xargs)
    [ -z "$line" ] && continue

    url=$(echo "$line" | awk '{print $1}')
    dest=$(echo "$line" | awk '{print $2}')
    [ -z "$url" ] || [ -z "$dest" ] && continue

    target_dir="$HOME_DIR/$dest"

    if [ ! -d "$target_dir" ]; then
        echo "  [FAIL] Missing dotfile repo: $target_dir"
        FAILED=1
        continue
    fi

    owner=$(stat -c '%U' "$target_dir")
    if [ "$owner" != "$TARGET_USER" ]; then
        echo "  [FAIL] $target_dir owned by $owner instead of $TARGET_USER"
        FAILED=1
    fi

    if [ ! -d "$target_dir/.git" ]; then
        echo "  [FAIL] $target_dir is not a git repository"
        FAILED=1
    fi
done < "$SCRIPT_DIR/packages/debian-git-dotfiles.txt"

[ $FAILED -eq 1 ] && exit 1
echo "All git repositories verified."
exit 0
