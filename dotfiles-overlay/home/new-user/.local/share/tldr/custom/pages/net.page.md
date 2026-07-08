# net

> Diagnose internet connectivity. Runs a ladder of checks and reports the first broken rung.
> Checks: default route, gateway ping, internet ping, DNS, captive portal, and VPN routing.

- Run the connectivity ladder (why is the internet broken?):

`net`

- Show local addresses and the public IP:

`net ip`

- Continuous ping to 1.1.1.1 (Ctrl+C to stop):

`net ping`

- Continuous ping to a specific host:

`net ping {{192.168.1.1}}`

- Run a speed test (needs speedtest-cli):

`net speed`
