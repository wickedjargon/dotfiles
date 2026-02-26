#!/bin/sh

SCRIPT_DIR="$(cd "$(dirname "$0")/.." >/dev/null 2>&1 && pwd)"
FAILED=0

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
SRC_DIR="$HOME_DIR/.local/src"

echo "Testing built packages from git-packages-src..."
while IFS= read -r url; do
    # Skip empty lines and comments
    case "$url" in
        ''|'#'*) continue ;;
    esac
    url=$(echo "$url" | sed 's/#.*//' | xargs)
    [ -z "$url" ] && continue
    
    # Extract repo name from URL
    repo_name=$(basename -s .git "$url")
    repo_dir="$SRC_DIR/$repo_name"
    
    if [ ! -d "$repo_dir" ]; then
        echo "  [FAIL] Built source directory missing: $repo_dir"
        FAILED=1
    else
        # Verify ownership
        owner=$(stat -c '%U' "$repo_dir")
        if [ "$owner" != "$TARGET_USER" ]; then
            echo "  [FAIL] Directory $repo_dir is owned by $owner instead of $TARGET_USER"
            FAILED=1
        fi
        
        # Verify it's actually a git repo
        if [ ! -d "$repo_dir/.git" ]; then
            echo "  [FAIL] Directory $repo_dir is not a valid git repository"
            FAILED=1
        fi
    fi
done < "$SCRIPT_DIR/packages/debian-git-packages-src.txt"


echo "Testing dotfiles cloned from git-dotfiles..."
while IFS= read -r line; do
    case "$line" in
        ''|'#'*) continue ;;
    esac
    line=$(echo "$line" | sed 's/#.*//' | xargs)
    [ -z "$line" ] && continue
    
    # Format: repo-url destination-path
    url=$(echo "$line" | awk '{print $1}')
    dest=$(echo "$line" | awk '{print $2}')
    
    if [ -z "$url" ] || [ -z "$dest" ]; then
        continue
    fi
    
    # Resolve destination relative to home directory
    target_dir="$HOME_DIR/$dest"
    
    if [ ! -d "$target_dir" ]; then
        echo "  [FAIL] Cloned dotfiles directory missing: $target_dir"
        FAILED=1
    else
        # Verify ownership
        owner=$(stat -c '%U' "$target_dir")
        if [ "$owner" != "$TARGET_USER" ]; then
            echo "  [FAIL] Directory $target_dir is owned by $owner instead of $TARGET_USER"
            FAILED=1
        fi
        
        # Verify it's a git repo
        if [ ! -d "$target_dir/.git" ]; then
            echo "  [FAIL] Directory $target_dir is not a valid git repository"
            FAILED=1
        fi
    fi
done < "$SCRIPT_DIR/packages/debian-git-dotfiles.txt"

if [ $FAILED -eq 1 ]; then
    exit 1
fi

echo "All git repositories verified successfully."
exit 0
