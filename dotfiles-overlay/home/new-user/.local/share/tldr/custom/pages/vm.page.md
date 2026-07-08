# vm

> Run and manage QEMU VMs created by create-qemu-vm.
> Images live in ~/virtual-machines as name.arch.RAM.qcow2; the filename carries the launch parameters.

- List VMs and their running state:

`vm`

- Boot a VM (the only one, or by name):

`vm start`

- Boot without a display window:

`vm start {{debian}} --headless`

- SSH into the running VM via its forwarded port:

`vm ssh`

- SSH as a specific user:

`vm ssh {{debian}} {{root}}`

- Shut down cleanly (ACPI powerdown, SIGTERM fallback):

`vm stop`
