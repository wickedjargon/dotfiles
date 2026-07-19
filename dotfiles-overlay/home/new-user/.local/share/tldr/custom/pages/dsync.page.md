# dsync

> True dotfile sync: deploy the repo overlay to $HOME, including deletions.
> Only removes files it deployed itself (manifest in ~/.local/state/dsync).
> Overlay: ~/d/projects/dotfiles/dotfiles-overlay/home/new-user (override: DSYNC_REPO).

- Show status (managed file count, pending copies/removals, last sync):

`dsync`

- List every pending change (+ new, ~ changed, - stale):

`dsync diff`

- Show how one live file differs from the repo version:

`dsync diff {{.bashrc}}`

- Deploy the overlay (copy new/changed, remove stale, run apply-theme):

`dsync sync`

- Remove files deleted from the overlay before dsync existed (asks first):

`dsync prune`

- Machine-parseable state of every managed file:

`dsync list`
