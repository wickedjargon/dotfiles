#!/bin/bash

# Path to apt-packages file
PACKAGES_FILE="./apt-packages"

# Ensure script is run from the project root or file exists
if [ ! -f "$PACKAGES_FILE" ]; then
    echo "Error: $PACKAGES_FILE not found in current directory."
    echo "Please run this script from the root of the dotfiles project."
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
