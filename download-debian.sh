#!/usr/bin/env bash

# Script to download the latest Debian stable amd64 netinst ISO
# Usage: ./download-debian.sh [target_directory]

set -euo pipefail

# Default target directory is /tmp
TARGET_DIR="${1:-/tmp}"
mkdir -p "$TARGET_DIR"

BASE_URL="https://cdimage.debian.org/debian-cd/current/amd64/iso-cd/"

echo "Finding latest Debian stable netinst ISO URL..."

# Get the filename of the netinst ISO
ISO_FILENAME=$(curl -sL "$BASE_URL" | grep -oP 'debian-\d+\.\d+\.\d+-amd64-netinst\.iso' | head -n 1)

if [[ -z "$ISO_FILENAME" ]]; then
    echo "Error: Could not find Debian netinst ISO filename at $BASE_URL" >&2
    exit 1
fi

ISO_URL="${BASE_URL}${ISO_FILENAME}"
SHA256_URL="${BASE_URL}SHA256SUMS"

echo "Latest ISO: $ISO_FILENAME"
echo "Target Directory: $TARGET_DIR"

# Download SHA256SUMS
echo "Downloading SHA256SUMS..."
curl -sL "$SHA256_URL" -o "$TARGET_DIR/SHA256SUMS"

# Download ISO
echo "Downloading ISO (this may take a while)..."
# Use curl -C - to allow resuming interrupted downloads
curl -L -C - "$ISO_URL" -o "$TARGET_DIR/$ISO_FILENAME"

# Verify download
echo "Verifying checksum..."
(cd "$TARGET_DIR" && grep "$ISO_FILENAME" SHA256SUMS | sha256sum -c -)

echo "Success! Debian ISO is ready at $TARGET_DIR/$ISO_FILENAME"
