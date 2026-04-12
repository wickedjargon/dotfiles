# mac

> View and spoof MAC addresses from the command line.
> Defaults to the wireless interface if only one exists.

- Show all interfaces and their MAC addresses:

`mac`

- Print the MAC address of the wifi interface:

`mac show`

- Print the MAC address of a specific interface:

`mac show {{eth0}}`

- Set a random MAC on the wifi interface:

`mac random`

- Set a random MAC on a specific interface:

`mac random {{wlan0}}`

- Set a specific MAC address:

`mac set {{aa:bb:cc:dd:ee:ff}}`

- Restore the original hardware MAC:

`mac reset`
