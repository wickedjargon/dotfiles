#!/bin/bash
# Install yay (AUR helper) from the AUR.
# Usage: ./install-yay

set -euo pipefail

if command -v yay &>/dev/null; then
    echo "yay is already installed."
    exit 0
fi

echo "Installing yay dependencies..."
sudo pacman -S --needed --noconfirm git base-devel

TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

echo "Cloning yay-bin from AUR..."
git clone https://aur.archlinux.org/yay-bin.git "$TMPDIR/yay-bin"

echo "Building and installing yay..."
cd "$TMPDIR/yay-bin"
makepkg -si --noconfirm

echo "Done. yay is now installed."
