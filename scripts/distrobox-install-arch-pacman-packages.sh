#!/bin/bash
# Install Arch Linux packages from the pacman package list.
# Usage: sudo ./install-arch-pacman-packages [package-list-file]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DEFAULT_PKG_FILE="$SCRIPT_DIR/../packages/arch-pacman-packages.txt"
PKG_FILE="${1:-$DEFAULT_PKG_FILE}"

if [[ ! -f "$PKG_FILE" ]]; then
    echo "Error: Package list not found: $PKG_FILE" >&2
    exit 1
fi

# Read non-empty, non-comment lines
mapfile -t PACKAGES < <(grep -v '^\s*#' "$PKG_FILE" | grep -v '^\s*$')

if [[ ${#PACKAGES[@]} -eq 0 ]]; then
    echo "No packages to install."
    exit 0
fi

echo "Installing ${#PACKAGES[@]} pacman packages..."
sudo pacman -S --needed --noconfirm "${PACKAGES[@]}"
echo "Done."
