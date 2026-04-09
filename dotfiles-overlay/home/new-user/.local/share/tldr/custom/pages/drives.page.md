# drives

> Manage external USB drives from the command line.
> Uses udisks2 for non-root mounting (mounts to /run/media/$USER/).
> Device can be a name (sdb1), path (/dev/sdb1), or filesystem label.
> If omitted and only one candidate exists, it is auto-selected.

- Show status of all external drives:

`drives`

- Mount a drive (auto-select if only one unmounted):

`drives mount`

- Mount a specific drive:

`drives mount {{sdb1}}`

- Unmount a drive:

`drives unmount`

- Safely eject a drive (unmount + power off):

`drives eject`

- Open a mounted drive in the file manager:

`drives open`

- List external drives with partition details:

`drives list`
