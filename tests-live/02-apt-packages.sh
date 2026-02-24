#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." &> /dev/null && pwd )"
FAILED=0

echo "Testing standard APT packages..."
while IFS= read -r pkg; do
    # Skip empty lines and comments
    [[ -z "$pkg" || "$pkg" =~ ^[[:space:]]*# ]] && continue
    
    # Strip inline comments and whitespace
    pkg=$(echo "$pkg" | sed 's/#.*//' | xargs)
    [[ -z "$pkg" ]] && continue
    
    if ! dpkg-query -W -f='${Status}' "$pkg" 2>/dev/null | grep -q "install ok installed"; then
        echo "  [FAIL] Required package missing: $pkg"
        FAILED=1
    fi
done < "$SCRIPT_DIR/packages/apt-packages"

echo "Testing third-party repositories and packages..."
while IFS= read -r line; do
    # Skip empty lines and comments
    [[ -z "$line" || "$line" =~ ^[[:space:]]*# ]] && continue
    
    # Parse format: package_name | key_url | repo_line
    pkg_name=$(echo "$line" | cut -d'|' -f1 | xargs)
    key_url=$(echo "$line" | cut -d'|' -f2 | xargs)
    repo_line=$(echo "$line" | cut -d'|' -f3 | xargs)
    
    # Clean package name
    pkg_name=$(echo "$pkg_name" | sed 's/#.*//' | xargs)
    [[ -z "$pkg_name" ]] && continue
    
    # Check package
    if ! dpkg-query -W -f='${Status}' "$pkg_name" 2>/dev/null | grep -q "install ok installed"; then
        echo "  [FAIL] Third-party package missing: $pkg_name"
        FAILED=1
    fi
    
    # Check repository source file exists
    repo_file="/etc/apt/sources.list.d/${pkg_name}.list"
    if [ ! -f "$repo_file" ]; then
        echo "  [FAIL] Repository file missing for $pkg_name: $repo_file"
        FAILED=1
    else
        # Verify repo line exists in the file (simplistic check)
        if ! grep -q "^deb " "$repo_file"; then
            echo "  [FAIL] Missing valid 'deb' line in $repo_file"
            FAILED=1
        fi
    fi
    
    # Check keyring file exists
    keyring_file="/usr/share/keyrings/${pkg_name}-archive-keyring.gpg"
    if [ ! -f "$keyring_file" ]; then
        echo "  [FAIL] Keyring file missing for $pkg_name: $keyring_file"
        FAILED=1
    fi
    
done < "$SCRIPT_DIR/packages/third-party-apt-packages"

if [ $FAILED -eq 1 ]; then
    exit 1
fi

echo "All specified APT and third-party packages verified successfully."
exit 0
