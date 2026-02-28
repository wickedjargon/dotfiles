#!/bin/sh

# Verify: all apt packages (standard + third-party) are installed.

SCRIPT_DIR="$(cd "$(dirname "$0")/.." >/dev/null 2>&1 && pwd)"
FAILED=0

echo "Testing standard APT packages..."
while IFS= read -r line; do
    # Strip inline comments, then whitespace
    pkg=$(echo "$line" | sed 's/#.*//' | xargs)
    [ -z "$pkg" ] && continue

    if ! dpkg-query -W -f='${Status}' "$pkg" 2>/dev/null | grep -q "install ok installed"; then
        echo "  [FAIL] Missing package: $pkg"
        FAILED=1
    fi
done < "$SCRIPT_DIR/packages/debian-apt-packages.txt"

echo "Testing third-party packages..."
while IFS= read -r line; do
    # Skip empty lines and comment-only lines
    stripped=$(echo "$line" | sed 's/#.*//' | xargs)
    [ -z "$stripped" ] && continue

    pkg_name=$(echo "$stripped" | cut -d'|' -f1 | xargs)
    [ -z "$pkg_name" ] && continue

    if ! dpkg-query -W -f='${Status}' "$pkg_name" 2>/dev/null | grep -q "install ok installed"; then
        echo "  [FAIL] Missing third-party package: $pkg_name"
        FAILED=1
    fi

    # Check repo source file
    repo_file="/etc/apt/sources.list.d/${pkg_name}.list"
    if [ ! -f "$repo_file" ]; then
        echo "  [FAIL] Missing repository file: $repo_file"
        FAILED=1
    elif ! grep -q "^deb " "$repo_file"; then
        echo "  [FAIL] No valid 'deb' line in $repo_file"
        FAILED=1
    fi

    # Check keyring file
    keyring_file="/usr/share/keyrings/${pkg_name}-archive-keyring.gpg"
    if [ ! -f "$keyring_file" ]; then
        echo "  [FAIL] Missing keyring: $keyring_file"
        FAILED=1
    fi
done < "$SCRIPT_DIR/packages/debian-third-party-apt-packages.txt"

[ $FAILED -eq 1 ] && exit 1
echo "All packages verified."
exit 0
