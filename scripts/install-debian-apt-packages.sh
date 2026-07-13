#!/bin/sh

# Install all packages from packages/debian-apt-packages.txt
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PACKAGES_FILE="$SCRIPT_DIR/../packages/debian-apt-packages.txt"

if [ ! -f "$PACKAGES_FILE" ]; then
    echo "Error: $PACKAGES_FILE not found."
    exit 1
fi

echo "Reading packages from $PACKAGES_FILE..."

# Extract package names:
# 1. Remove comments (text starting with #)
# 2. Remove empty lines
# 3. Replace newlines with spaces
PACKAGES=$(sed 's/#.*//' "$PACKAGES_FILE" | grep -v '^\s*$' | tr '\n' ' ')

if [ -z "$PACKAGES" ]; then
    echo "No packages found to install."
    exit 0
fi

echo "Found packages to install."
echo "Running apt update..."
sudo apt update

echo "Installing packages..."
# We use $PACKAGES unquoted to allow the shell to split the string into arguments
sudo apt install -y $PACKAGES

echo "Done!"
