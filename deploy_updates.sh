#!/bin/bash
set -e

echo "Deploying Polybar config..."
mkdir -p ~/.config/polybar
cp -r ~/d/projects/bspwm-config/polybar/* ~/.config/polybar/

echo "Deploying BSPWM config..."
mkdir -p ~/.config/bspwm
cp -r ~/d/projects/bspwm-config/bspwm/* ~/.config/bspwm/

echo "Deploying SXHKD config..."
mkdir -p ~/.config/sxhkd
cp -r ~/d/projects/bspwm-config/sxhkd/* ~/.config/sxhkd/

echo "Deploying Scripts..."
mkdir -p ~/.local/bin
cp ~/d/projects/dotfiles/.local/bin/bt-connect ~/.local/bin/
cp ~/d/projects/dotfiles/.local/bin/bt-disconnect ~/.local/bin/
cp ~/d/projects/dotfiles/.local/bin/poly-sb-* ~/.local/bin/
cp ~/d/projects/dotfiles/.local/bin/poly-bt-refresh ~/.local/bin/

chmod +x ~/.local/bin/bt-connect
chmod +x ~/.local/bin/bt-disconnect
chmod +x ~/.local/bin/poly-sb-*
chmod +x ~/.local/bin/poly-bt-refresh

echo "Restarting services..."

echo "Restarting SXHKD..."
pkill -USR1 -x sxhkd || echo "sxhkd not running, skipping reload"

echo "Restarting BSPWM..."
bspc wm -r || echo "bspc returned error, is bspwm running?"

echo "Restarting Polybar..."
polybar-msg cmd restart || echo "polybar-msg failed, trying pkill..."
# Fallback if polybar-msg fails or ipc is not on
# pkill polybar; polybar mybar & (Doing this blindly is risky if we don't know the bar name/launch script)

echo "Deployment complete."
