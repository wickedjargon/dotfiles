#!/bin/sh

# Remove Xfce desktop environment and associated packages
# that are installed by Debian's "task-xfce-desktop" selection.
# This script is intended to be run after deploy.py on a fresh
# Debian install where xfce was selected during installation.

echo "Removing Xfce desktop environment and associated packages..."

# Remove all xfce4 packages (covers panel, session, settings, plugins, goodies, etc.)
sudo apt purge -y 'xfce4*'

# Remove Xfce core components not caught by the xfce4* glob
sudo apt purge -y \
    xfconf \
    xfdesktop4 \
    xfwm4 \
    thunar \
    thunar-volman \
    tumbler

# Remove the login manager and screen locker
sudo apt purge -y lightdm lightdm-gtk-greeter light-locker

# Remove Xfce-bundled applications
sudo apt purge -y \
    atril \
    mousepad \
    parole \
    ristretto \
    xfburn \
    orca \
    quodlibet \
    synaptic \
    system-config-printer \
    network-manager-applet \
    xsane

# Remove LibreOffice
sudo apt purge -y 'libreoffice*'

# Remove Xfce-bundled dictionaries, thesaurus, and theming
sudo apt purge -y \
    hunspell-en-us \
    hyphen-en-us \
    mythes-en-us \
    tango-icon-theme \
    desktop-base

# Remove the task meta-package itself
sudo apt purge -y task-xfce-desktop

# Remove leftover Xfce dependencies
sudo apt purge -y \
    gnome-keyring \
    cups-pk-helper \
    mate-polkit \
    xiccd \
    xbrlapi

# Clean up orphaned dependencies
sudo apt autoremove --purge -y

echo "Done! Xfce and associated packages have been removed."
