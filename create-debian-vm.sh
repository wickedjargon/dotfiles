#!/usr/bin/env bash

# Script to create a Debian VM disk and launch QEMU for installation
# Usage: ./create-debian-vm.sh [iso_path]

set -euo pipefail

ISO_PATH="${1:-}"
# If no ISO path is provided, look for a debian netinst iso in /tmp
if [[ -z "$ISO_PATH" ]]; then
    ISO_PATH=$(find /tmp -name "debian-*-amd64-netinst.iso" 2>/dev/null | sort -V | tail -n 1 || true)
fi

VM_NAME="debian"
ARCH="x86_64"
RAM="1G"
DISK_SIZE="12G"

# Target directory for VMs
VM_DIR="$HOME/virtual-machines"
mkdir -p "$VM_DIR"

# Format required by dmenu-explorer: name.arch.ram.qcow2
VM_FILE="$VM_DIR/${VM_NAME}.${ARCH}.${RAM}.qcow2"

if ! command -v qemu-img &> /dev/null || ! command -v "qemu-system-${ARCH}" &> /dev/null; then
    echo "Error: QEMU utilities are not installed." >&2
    echo "Please install them using your package manager." >&2
    echo "On Arch Linux: sudo pacman -S qemu-img qemu-desktop (or qemu-system-x86)" >&2
    exit 1
fi

if [[ -f "$VM_FILE" ]]; then
    echo "Error: VM disk $VM_FILE already exists." >&2
    exit 1
fi

echo "Creating QEMU disk image: $VM_FILE ($DISK_SIZE)..."
qemu-img create -f qcow2 "$VM_FILE" "$DISK_SIZE"

if [[ -n "$ISO_PATH" ]]; then
    if [[ ! -f "$ISO_PATH" ]]; then
        echo "Error: ISO file not found at $ISO_PATH" >&2
        exit 1
    fi
    echo "Starting QEMU for installation using ISO: $ISO_PATH"
    echo "Booting with 1GB RAM. Use Ctrl+Alt+G to release mouse."
    
    qemu-system-x86_64 \
        -m 1G \
        -drive "file=$VM_FILE,format=qcow2" \
        -cdrom "$ISO_PATH" \
        -boot d \
        -enable-kvm \
        -net user,hostfwd=tcp::2222-:22 \
        -net nic \
        -display gtk,zoom-to-fit=on
else
    echo "VM disk created: $VM_FILE"
    echo "To install Debian, run this script with the path to the ISO as an argument:"
    echo "  ./create-debian-vm.sh /path/to/debian.iso"
    echo ""
    echo "Once installed, you can launch it via dmenu-explorer."
fi
