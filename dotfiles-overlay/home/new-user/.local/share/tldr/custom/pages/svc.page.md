# svc

> Manage systemd services from the command line.
> Wraps systemctl with colored output and auto-detection of user/system scope.

- Show overview of all user services:

`svc`

- Show detailed status of a service:

`svc status {{pipewire}}`

- Start a service:

`svc start {{syncthing}}`

- Stop a service:

`svc stop {{syncthing}}`

- Restart a service:

`svc restart {{pipewire}}`

- Enable a service to start on login:

`svc enable {{syncthing}}`

- Show last N log lines for a service:

`svc logs {{pipewire}} {{50}}`

- Follow logs in real time:

`svc follow {{pipewire}}`

- Show failed services:

`svc failed`

- Show active timers:

`svc timers`

- Print a service's unit file:

`svc cat {{pipewire}}`

- Edit a service's unit file:

`svc edit {{myservice}}`
