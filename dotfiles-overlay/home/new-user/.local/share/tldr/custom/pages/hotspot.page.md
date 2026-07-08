# hotspot

> Share this machine's internet connection over WiFi. Wraps nmcli's hotspot mode.
> Starting a hotspot disconnects any WiFi connection on the same radio.

- Show hotspot status, credentials, and connected clients:

`hotspot`

- Start the hotspot (reuses the saved profile, or generates SSID + password):

`hotspot on`

- Start with a custom SSID (random password is generated and printed):

`hotspot on {{MyNet}}`

- Start with a custom SSID and password:

`hotspot on {{MyNet}} {{s3cretpass}}`

- Show a QR code phones can scan to join:

`hotspot qr`

- Print the SSID and password:

`hotspot password`

- List connected client MAC addresses:

`hotspot clients`

- Stop the hotspot:

`hotspot off`

- Delete the saved hotspot profile:

`hotspot forget`
