# psync

> Sync files between laptop and phone over Tailscale using rsync.
> Phone must be running Termux with sshd on port 8022.
> Neither direction deletes by default — both are additive.
> Dotfiles (.*) are globally excluded.
> Logs: /tmp/psync/ (one per run, timestamped).
> ~/d/audio → /sdcard/Music
> ~/d/images → /sdcard/DCIM
> ~/d/notes → /sdcard/Notes
> ~/d/other → /sdcard/Other
> ~/d/images/screenshots → /sdcard/Pictures/Screenshots
> ~/d/video → /sdcard/Movies

- Full two-way sync (each device gets missing files from the other):

`psync push && psync pull`

- Push all directories to phone:

`psync push`

- Pull all directories from phone:

`psync pull`

- Sync only a specific directory (e.g. notes, audio, video, images, screenshots, other):

`psync {{push|pull}} --dir {{notes}}`

- Push a specific file by name:

`psync push {{filename}}`

- Pull a specific file by name (path copied to clipboard):

`psync pull {{filename}}`

- Pull the latest file from phone (path copied to clipboard):

`psync pull --latest`

- Pull the N most recent files (check log for paths):

`psync pull --latest {{3}}`

- Pull the Nth most recent file:

`psync pull --nth {{2}}`

- Preview a full push without transferring:

`psync push --dry-run`

- Delete destination files not present at source:

`psync {{push|pull}} --delete`

