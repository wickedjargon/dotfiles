#!/bin/sh

# Remove Xfce desktop environment and associated packages
# that are installed by Debian's "task-xfce-desktop" selection.
# This script is intended to be run after deploy.py on a fresh
# Debian install where xfce was selected during installation.

set -e

echo "Removing Xfce desktop environment and associated packages..."

# Remove all xfce4 packages
sudo apt purge -y 'xfce4*'

# Remove the login manager and screen locker
sudo apt purge -y lightdm light-locker

# Remove Xfce-bundled applications
sudo apt purge -y \
    atril \
    mousepad \
    parole \
    orca \
    quodlibet \
    synaptic \
    system-config-printer \
    network-manager-applet \
    xsane

# Remove LibreOffice
sudo apt purge -y \
    'libreoffice*'

# Remove Xfce-bundled dictionaries and thesaurus
sudo apt purge -y \
    hunspell-en-us \
    hyphen-en-us \
    mythes-en-us \
    tango-icon-theme

# Remove the task meta-package itself
sudo apt purge -y task-xfce-desktop

# Clean up orphaned dependencies
sudo apt autoremove --purge -y

echo "Done! Xfce and associated packages have been removed."
