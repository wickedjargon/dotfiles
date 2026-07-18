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

- Create a VM from a local ISO:

`vm create {{~/dl/custom.iso}} --name {{custom}}`

- Create a Windows VM (detected from the filename; UEFI + TPM 2.0 + Secure Boot for 11, SATA + e1000e, 4G/64G/4cpu defaults):

`vm create {{~/dl/Win11_24H2_English_x64.iso}}`

- Force a guest OS profile when detection guesses wrong:

`vm create {{image.iso}} --os {{win10|win11|linux}} --cpus {{8}}`

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

- Delete a VM's disk image (asks first):

`vm delete {{debian}}`
