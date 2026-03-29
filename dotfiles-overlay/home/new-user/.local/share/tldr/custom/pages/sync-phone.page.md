# sync-phone

> Sync files between laptop and phone over Tailscale using rsync.
> Phone must be running Termux with sshd on port 8022.

- Push all directories to phone:

`sync-phone push`

- Pull all directories from phone:

`sync-phone pull`

- Push a specific file by name:

`sync-phone push {{filename}}`

- Pull the latest file from phone:

`sync-phone pull --latest`

- Pull the 3 most recent files:

`sync-phone pull --latest {{3}}`

- Pull the 2nd most recent file:

`sync-phone pull --nth {{2}}`

- Preview a full push without transferring:

`sync-phone push --dry-run`

- Pull without deleting extraneous local files:

`sync-phone pull --no-delete`
