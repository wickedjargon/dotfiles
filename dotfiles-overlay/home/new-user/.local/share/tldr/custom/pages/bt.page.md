# bt

> Manage Bluetooth devices from the command line. Wraps bluetoothctl with audio sink routing.
> Device can be a name or MAC address. Partial name matching is supported.
> If device is omitted and only one candidate exists, it is auto-selected.
> Automatically routes audio and refreshes Polybar on connect/disconnect.

- Show status of all paired devices (with battery levels):

`bt`

- Connect to a paired device (auto-select if only one):

`bt connect`

- Connect to a specific device by name:

`bt connect {{Nothing}}`

- Disconnect a device:

`bt disconnect`

- Reconnect a device (fixes audio glitches):

`bt reconnect {{Nothing}}`

- List available audio profiles for a device:

`bt profiles`

- Switch to high-quality audio profile:

`bt profile a2dp-sink`

- Switch to microphone + audio mode:

`bt profile headset-head-unit`

- Show battery levels for connected devices:

`bt battery`

- Toggle bluetooth power on/off:

`bt toggle`

- Scan for nearby discoverable devices:

`bt scan`

- Pair with a new device by MAC address:

`bt pair {{3C:B0:ED:A7:6A:88}}`

- Remove (unpair) a device:

`bt remove {{Miniroll}}`
