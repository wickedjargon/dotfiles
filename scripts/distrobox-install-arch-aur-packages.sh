#!/bin/sh
# Install AUR packages using yay from the AUR package list.
# Usage: ./install-arch-aur-packages [package-list-file]

set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DEFAULT_PKG_FILE="$SCRIPT_DIR/../packages/arch-aur-packages.txt"
PKG_FILE="${1:-$DEFAULT_PKG_FILE}"

if ! command -v yay >/dev/null 2>&1; then
    echo "Error: yay is not installed. Run distrobox-install-arch-yay.sh first." >&2
    exit 1
fi

if [ ! -f "$PKG_FILE" ]; then
    echo "Error: Package list not found: $PKG_FILE" >&2
    exit 1
fi

# Read non-empty, non-comment lines
PACKAGES=$(grep -v '^\s*#' "$PKG_FILE" | grep -v '^\s*$')

if [ -z "$PACKAGES" ]; then
    echo "No AUR packages to install."
    exit 0
fi

pkg_count=$(echo "$PACKAGES" | wc -l)
echo "Installing $pkg_count AUR packages..."
echo "$PACKAGES" | while IFS= read -r pkg; do
    echo "  -> $pkg"
    yay -S --needed --noconfirm "$pkg"
done
echo "Done."
