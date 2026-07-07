# Automatic Timezone

The system timezone follows the machine's physical location (like phones and
macbooks do). No setup is needed — `deploy.py` installs
`/etc/NetworkManager/dispatcher.d/50-auto-timezone`, which NetworkManager runs
as root on network events.

## How it works

On every interface up/down and connectivity change, the script:

1. Skips unless NetworkManager reports **full** internet connectivity
   (so captive portals don't trigger it; passing the portal does).
2. Skips if the default route egresses through a tunnel device (`mullvad`,
   `tailscale*` exit node, `tun*`, `wg*`, `ppp*`) — the timezone never
   follows the VPN country. Tailscale without an exit node does not count
   as a tunnel. Disconnecting the VPN fires a `down` event, so the timezone
   corrects itself right after.
3. Asks `ipapi.co`, then `ip-api.com`, for the timezone of the public IP.
4. Validates the answer against `/usr/share/zoneinfo` and applies it with
   `timedatectl set-timezone` only if it differs from the current one.

Any failure (API down, no answer, garbage response) means "do nothing and
retry on the next network event". The wall clock itself is kept correct in
UTC by systemd-timesyncd throughout — only the timezone label is managed
here.

Timezone changes are logged to the journal:

```bash
journalctl -t auto-timezone
```

## Caveats

- Long-running apps may keep displaying the old timezone until they re-read
  `/etc/localtime` (inherent to any timezone change on Linux; restart them
  if it matters).
- If a VPN is up when you join a new network, the timezone stays as-is until
  the VPN disconnects.
