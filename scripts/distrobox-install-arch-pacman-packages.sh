#!/bin/sh
# Install Arch Linux packages from the pacman package list.
# Usage: sudo ./install-arch-pacman-packages [package-list-file]

set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DEFAULT_PKG_FILE="$SCRIPT_DIR/../packages/arch-pacman-packages.txt"
PKG_FILE="${1:-$DEFAULT_PKG_FILE}"

if [ ! -f "$PKG_FILE" ]; then
    echo "Error: Package list not found: $PKG_FILE" >&2
    exit 1
fi

# Read non-empty, non-comment lines
PACKAGES=$(grep -v '^\s*#' "$PKG_FILE" | grep -v '^\s*$')

if [ -z "$PACKAGES" ]; then
    echo "No packages to install."
    exit 0
fi

pkg_count=$(echo "$PACKAGES" | wc -l)
echo "Installing $pkg_count pacman packages..."
echo "$PACKAGES" | xargs sudo pacman -S --needed --noconfirm
echo "Done."
