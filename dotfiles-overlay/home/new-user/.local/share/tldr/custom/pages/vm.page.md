# vm

> The full QEMU VM lifecycle: download, install, run, ssh, delete.
> Images live in ~/virtual-machines as name.arch.RAM.qcow2 — the same convention rofi-explorer (super+e) launches.

- List VMs and their running state:

`vm`

- Download the latest distro ISO (checksum-verified) and boot the installer:

`vm install {{debian}}`

- Install a small VM with custom parameters:

`vm install {{alpine}} --name {{test}} --ram {{512M}} --disk {{4G}}`

- Print where to download a Windows ISO (Microsoft gates auto-download; then `vm create` it):

`vm install {{win11}}`

- Download a macOS recovery image from Apple's CDN (via a cached OSX-KVM checkout) and boot the installer:

`vm install {{osx}}`

- Create a VM from a local ISO:

`vm create {{~/dl/custom.iso}} --name {{custom}}`

- Create a Windows VM (detected from the filename; UEFI + TPM 2.0 + Secure Boot for 11, SATA + e1000e, 8G/200G/4cpu defaults):

`vm create {{~/dl/Win11_24H2_English_x64.iso}}`

- Create a macOS VM from a recovery image (OpenCore + AppleSMC, AHCI disks; OpenCore.qcow2 must sit next to the image, e.g. an OSX-KVM checkout — delete the .basesystem.img sidecar after installing):

`vm create {{~/OSX-KVM/BaseSystem.img}}`

- Force a guest OS profile when detection guesses wrong:

`vm create {{image.iso}} --os {{win10|win11|osx|linux}} --cpus {{8}}`

- Boot a VM (the only one, or by name):

`vm start`

- Boot without a display window:

`vm start {{debian}} --headless`

- Boot a qcow2 image by path, from anywhere (also what xdg-open does):

`vm start {{~/dl/debian.x86_64.2GB.qcow2}}`

- SSH into the running VM via its forwarded port:

`vm ssh`

- Shut down cleanly (ACPI powerdown, SIGTERM fallback):

`vm stop`

- Resize a stopped VM (disk only grows — expand the partition in-guest after; RAM renames the image + sidecars):

`vm resize {{win11}} --disk {{+40G}} --ram {{8G}} --cpus {{4}}`

- Delete a VM's disk image (asks first):

`vm delete {{debian}}`
