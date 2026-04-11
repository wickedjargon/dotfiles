# upload

> Upload files or text to the internet.
> Default backend is u.fftp.io (private), with 0x0.st as fallback.
> Use `--backend` to force a specific provider.

- Upload a file (u.fftp.io → 0x0.st fallback):

`upload file {{path/to/file}}`

- Upload a file directly to 0x0.st:

`upload file {{path/to/file}} --backend 0x0`

- Upload a file directly to u.fftp.io:

`upload file {{path/to/file}} --backend u`

- Upload highlighted text (X selection):

`upload selection`

- Upload selection to a specific backend:

`upload selection --backend {{0x0|u}}`

- Keep original filename on server (u.fftp.io only):

`upload file {{path/to/file}} --preserve-filename`

- Upload without stripping EXIF metadata:

`upload file {{path/to/image.jpg}} --keep-exif`
