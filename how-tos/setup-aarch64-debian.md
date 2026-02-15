# Setup AArch64 (ARM64) Debian VM with QEMU

This guide walks through setting up a Debian ARM64 (AArch64) virtual machine using QEMU from the command line on an x86-64 host.

## Prerequisites

1.  **Install QEMU and EFI Firmware**:
    ```bash
    sudo apt update
    sudo apt install qemu-system-arm qemu-efi-aarch64 qemu-utils
    ```

## Preparation

1.  **Create a directory for the VM**:
    ```bash
    mkdir -p ~/virtual-machines
    cd ~/virtual-machines
    ```

2.  **Download Debian ARM64 Installer ISO**:
    Download the **arm64** ISO (e.g., `debian-12.x.x-arm64-netinst.iso`) from the [Debian website](https://www.debian.org/distrib/).

## Disk Creation

Create a virtual hard disk image.

```bash
# Create a 20GB disk image
qemu-img create -f qcow2 debian.aarch64.qcow2 20G
```

## Installation

The installation on ARM64 requires specifying the machine type, CPU, and firmware (BIOS).

```bash
qemu-system-aarch64 \
  -M virt \
  -cpu cortex-a57 \
  -m 2G \
  -smp 2 \
  -bios /usr/share/qemu-efi-aarch64/QEMU_EFI.fd \
  -drive file=debian.aarch64.qcow2,format=qcow2,if=none,id=drive0 \
  -device virtio-blk-device,drive=drive0 \
  -cdrom path/to/debian-arm64-installer.iso \
  -device virtio-gpu-pci \
  -device usb-ehci \
  -device usb-kbd \
  -device usb-mouse \
  -device virtio-net-device,netdev=net0 \
  -netdev user,id=net0 \
  -display default,show-cursor=on
```

*   `-M virt`: Uses the generic "virt" machine type.
*   `-cpu cortex-a57`: Emulates a generic ARMv8 CPU.
*   `-bios ...`: Points to the UEFI firmware provided by `qemu-efi-aarch64`.
*   `-drive file=...`: Attaches the hard disk we created.
*   `-device virtio-blk-device`: Uses VirtIO for the disk.
*   `-device virtio-gpu-pci`: Uses VirtIO for graphics.
*   `-device virtio-net-device`: Uses VirtIO for networking.

**Note**: Installation will be slower than x86-64 because it is being emulated (unless you are on an ARM host).

## Running the VM

After installation, remove the `-cdrom` argument.

```bash
qemu-system-aarch64 \
  -M virt \
  -cpu cortex-a57 \
  -m 2G \
  -smp 2 \
  -bios /usr/share/qemu-efi-aarch64/QEMU_EFI.fd \
  -drive file=debian.aarch64.qcow2,format=qcow2,if=none,id=drive0 \
  -device virtio-blk-device,drive=drive0 \
  -device virtio-gpu-pci \
  -device usb-ehci \
  -device usb-kbd \
  -device usb-mouse \
  -device virtio-net-device,netdev=net0 \
  -netdev user,id=net0 \
  -display default,show-cursor=on
```

### SSH Forwarding (Optional)

Add the host forwarding to the `-netdev` parameter:

```bash
  -netdev user,id=net0,hostfwd=tcp::2223-:22
```

Connect via:
```bash
ssh -p 2223 user@localhost
```
