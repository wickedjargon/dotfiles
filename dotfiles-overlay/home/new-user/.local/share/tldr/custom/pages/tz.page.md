# tz

> Show and set the system timezone (via timedatectl).
> Zone names are fuzzy-matched case-insensitively and by substring.

- Show current timezone, local/UTC time, and NTP state:

`tz`

- Set the timezone (fuzzy, e.g. 'manila' matches Asia/Manila):

`tz set {{zone}}`

- List available timezones, optionally filtered:

`tz list {{filter}}`

- Compare the system timezone with IP geolocation (spots VPN/eSIM mismatches):

`tz ip`
