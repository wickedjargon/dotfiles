# disk

> Internal storage usage and cleanup — the counterpart to `drives` for external media.

- Show usage of all real filesystems with colored bars:

`disk`

- Largest entries under the home directory:

`disk big`

- Ten largest entries under a specific path:

`disk big {{~/d/video}} {{10}}`

- Reclaim caches (apt, journal, thumbnails, trash, pip) — asks first:

`disk clean`

- Machine-parseable usage:

`disk list`
