# vpn

> Manage Mullvad WireGuard VPN connections from the command line.
> Reads identity from ~/.config/mullvad/identity.conf.

- Show current VPN connection status:

`vpn`

- Connect to a random relay:

`vpn connect`

- Connect to a relay by country, city, or hostname:

`vpn connect {{sweden}}`

- Disconnect from VPN:

`vpn disconnect`

- Show available locations:

`vpn location`

- Switch to a different location (auto-disconnects):

`vpn location {{germany}}`

- List all available relays:

`vpn list`

- Force refresh the relay cache:

`vpn refresh`
