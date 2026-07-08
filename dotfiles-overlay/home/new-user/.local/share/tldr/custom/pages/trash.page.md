# trash

> Safe delete via the XDG trash (~/.local/share/Trash). No trash-cli needed.
> Restore knows the original path and refuses to overwrite anything.

- Move files to the trash instead of deleting:

`trash {{old-notes.txt}}`

- Show what's in the trash (size, age, original path):

`trash`

- Restore an entry to where it came from:

`trash restore {{old-notes.txt}}`

- Delete entries older than 30 days:

`trash empty {{30}}`

- Delete everything (asks first):

`trash empty`
