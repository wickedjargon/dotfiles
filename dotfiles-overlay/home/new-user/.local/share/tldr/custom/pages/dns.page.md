# dns

> Show and switch DNS resolvers via NetworkManager.
> Changes persist in the active connection's profile. Warns about DNS leaks when the Mullvad VPN is up.

- Show current resolvers and where they come from:

`dns`

- Switch to a preset (cloudflare, quad9, google):

`dns set {{cloudflare}}`

- Use a custom resolver:

`dns set {{192.168.1.5}}`

- Go back to the router's (DHCP) DNS:

`dns set auto`

- Time lookups to confirm resolution works:

`dns test`

- Flush the system DNS cache (systemd-resolved only):

`dns flush`
