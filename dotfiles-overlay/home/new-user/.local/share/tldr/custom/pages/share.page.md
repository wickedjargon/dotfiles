# share

> Serve a file or directory over HTTP so another device can download it.
> Binds to the Tailscale IP when available, else the LAN IP. Prints the URL and a QR code (qrencode).

- Share one file (siblings are not exposed):

`share {{report.pdf}}`

- Share a directory with a browsable listing:

`share {{~/d/images/vacation}}`

- Use a specific port:

`share {{big.iso}} {{9000}}`
