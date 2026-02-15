# Setup x86-64 Debian VM with QEMU/KVM

This guide walks through setting up a Debian x86-64 virtual machine using QEMU and KVM from the command line.

## Prerequisites

1.  **Check for KVM support**:
    ```bash
    grep -E --color 'vmx|svm' /proc/cpuinfo
    ```
    If you see output (highlighted in color), your CPU supports hardware virtualization.

2.  **Install QEMU and KVM**:
    ```bash
    sudo apt update
    sudo apt install qemu-system-x86 qemu-utils
    ```

3.  **Add your user to the KVM group**:
    ```bash
    sudo usermod -aG kvm $USER
    # Log out and log back in for this to take effect
    ```

## Preparation

1.  **Create a directory for the VM** (if you haven't already):
    ```bash
    mkdir -p ~/virtual-machines
    cd ~/virtual-machines
    ```

2.  **Download Debian Installer ISO**:
    Download the implementation specific ISO (e.g., `debian-12.x.x-amd64-netinst.iso`) from the [Debian website](https://www.debian.org/distrib/).

## Disk Creation

Create a virtual hard disk image. `qcow2` is the recommended format as it grows dynamically.

```bash
# Create a 20GB disk image
qemu-img create -f qcow2 debian.x86_64.qcow2 20G
```

## Installation

Run the installer using the ISO.

```bash
qemu-system-x86_64 \
  -enable-kvm \
  -m 2G \
  -smp 2 \
  -drive file=debian.x86_64.qcow2,format=qcow2 \
  -cdrom path/to/debian-installer.iso \
  -boot d
```

*   `-enable-kvm`: Enables KVM hardware acceleration (crucial for performance).
*   `-m 2G`: Allocates 2GB of RAM.
*   `-smp 2`: Allocates 2 CPU cores.
*   `-drive ...`: Attaches the hard disk we created.
*   `-cdrom ...`: Mounts the ISO file. Replace `path/to/debian-installer.iso` with the actual path.
*   `-boot d`: Boots from the CD-ROM (ISO) first.

Follow the on-screen instructions to install Debian.

## Running the VM

After installation is complete, you can run the VM without the `-cdrom` and `-boot` flags.

```bash
qemu-system-x86_64 \
  -enable-kvm \
  -m 2G \
  -smp 2 \
  -drive file=debian.x86_64.qcow2,format=qcow2 \
  -vga virtio \
  -display default,show-cursor=on \
  -usb \
  -device usb-tablet
```

*   `-vga virtio`: Uses the VirtIO graphics driver for better performance.
*   `-device usb-tablet`: Ensures the mouse cursor is synchronized between host and guest.

### SSH Forwarding (Optional)

To SSH into the VM, you can forward a port (e.g., 2222 on host to 22 on guest).

```bash
qemu-system-x86_64 \
  ... (other flags) ... \
  -net nic -net user,hostfwd=tcp::2222-:22
```

Then connect via:
```bash
ssh -p 2222 user@localhost
```
