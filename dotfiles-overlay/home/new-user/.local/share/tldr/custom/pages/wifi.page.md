# wifi

> Manage WiFi connections from the command line. Wraps nmcli with a simple interface.
> If SSID is omitted where applicable, auto-selects or shows an interactive picker.

- Show current connection status (SSID, signal, IP, speed):

`wifi`

- List available networks with signal strength:

`wifi list`

- Connect to a network (interactive picker if no SSID given):

`wifi connect`

- Connect to a specific network:

`wifi connect {{My Network}}`

- Disconnect from the current network:

`wifi disconnect`

- List saved/known connection profiles:

`wifi saved`

- Delete a saved connection:

`wifi forget {{Old Network}}`

- Show password of the current network:

`wifi password`

- Show password of a specific saved network:

`wifi password {{Network Name}}`

- Rescan for available networks:

`wifi rescan`

- Toggle wifi radio on/off:

`wifi toggle`
