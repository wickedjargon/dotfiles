# power

> Battery status and sleep control. Reads sysfs directly; caffeine mode wraps systemd-inhibit.
> Suspend and hibernate release caffeine first so it cannot block them.

- Show battery level, time remaining, health, AC, and caffeine status:

`power`

- Toggle caffeine (inhibit sleep and screen blanking):

`power caffeine`

- Keep the machine awake explicitly:

`power caffeine on`

- Restore normal sleep behavior:

`power caffeine off`

- Suspend to RAM:

`power suspend`

- Hibernate to disk:

`power hibernate`

- Machine-parseable battery fields (name, capacity, status, minutes, health):

`power list`
