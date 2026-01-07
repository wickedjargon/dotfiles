#!/bin/sh

# Debian VM Test Environment Setup Script
# This script creates a minimal Debian VM to test deploy.py
# Usage: ./setup-vm-test.sh <path-to-iso-file>

set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <path-to-iso-file>"
    echo "Example: $0 ~/Downloads/debian-13.2.0-amd64-netinst.iso"
    exit 1
fi

ISO_FILE="$1"

if [ ! -f "$ISO_FILE" ]; then
    echo "Error: ISO file '$ISO_FILE' not found"
    exit 1
fi

# VM Configuration
VM_DIR="$HOME/debian-vm-test"
DISK_IMG="$VM_DIR/debian.qcow2"
DISK_SIZE="10G"
RAM="1G"

echo "==> Creating VM directory..."
mkdir -p "$VM_DIR"

if [ ! -f "$DISK_IMG" ]; then
    echo "==> Creating virtual disk ($DISK_SIZE)..."
    qemu-img create -f qcow2 "$DISK_IMG" "$DISK_SIZE"
else
    echo "==> Disk image already exists"
fi

echo ""
echo "==> VM Setup Complete!"
echo ""
echo "To install Debian, run:"
echo "  qemu-system-x86_64 -enable-kvm -m $RAM -hda $DISK_IMG -cdrom $ISO_FILE -boot d"
echo ""
echo "After installation, to boot the VM:"
echo "  qemu-system-x86_64 -enable-kvm -m $RAM -hda $DISK_IMG -net user,hostfwd=tcp::2222-:22 -net nic"
echo ""
echo "To share dotfiles with VM (after install):"
echo "  qemu-system-x86_64 -enable-kvm -m $RAM -hda $DISK_IMG \\"
echo "    -virtfs local,path=$PWD,mount_tag=dotfiles,security_model=passthrough,id=dotfiles \\"
echo "    -net user,hostfwd=tcp::2222-:22 -net nic"
echo ""
echo "Inside the VM, mount dotfiles with:"
echo "  sudo mkdir -p /mnt/dotfiles"
echo "  sudo mount -t 9p -o trans=virtio,version=9p2000.L dotfiles /mnt/dotfiles"
