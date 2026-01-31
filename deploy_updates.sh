#!/bin/bash
set -e

echo "Deploying Polybar config..."
mkdir -p ~/.config/polybar
cp -r ~/d/projects/dotfiles/.config/polybar/* ~/.config/polybar/

echo "Deploying BSPWM config..."
mkdir -p ~/.config/bspwm
cp -r ~/d/projects/dotfiles/.config/bspwm/* ~/.config/bspwm/

echo "Deploying SXHKD config..."
mkdir -p ~/.config/sxhkd
cp -r ~/d/projects/dotfiles/.config/sxhkd/* ~/.config/sxhkd/

echo "Deploying Alacritty config..."
mkdir -p ~/.config/alacritty
cp -r ~/d/projects/dotfiles/.config/alacritty/* ~/.config/alacritty/

echo "Deploying Scripts..."
mkdir -p ~/.local/bin
cp ~/d/projects/dotfiles/.local/bin/bt-connect ~/.local/bin/
cp ~/d/projects/dotfiles/.local/bin/bt-disconnect ~/.local/bin/
cp ~/d/projects/dotfiles/.local/bin/poly-sb-* ~/.local/bin/
cp ~/d/projects/dotfiles/.local/bin/poly-bt-refresh ~/.local/bin/
cp ~/d/projects/dotfiles/.local/bin/bspwm-smart-arrow ~/.local/bin/

chmod +x ~/.local/bin/bt-connect
chmod +x ~/.local/bin/bt-disconnect
chmod +x ~/.local/bin/poly-sb-*
chmod +x ~/.local/bin/poly-bt-refresh
chmod +x ~/.local/bin/bspwm-smart-arrow

echo "Restarting services..."

echo "Restarting SXHKD..."
pkill -USR1 -x sxhkd || echo "sxhkd not running, skipping reload"

echo "Restarting BSPWM..."
bspc wm -r || echo "bspc returned error, is bspwm running?"

echo "Restarting Polybar..."
echo "Restarting Polybar..."
# Kill all instances to ensure we don't have duplicates
killall -q polybar || true
# Wait a brief moment for cleanup
sleep 1
# Launch new instance
polybar mybar &

echo "Deployment complete."
