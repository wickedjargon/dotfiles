# Phone Sync (Laptop ↔ Pixel 8)

Sync files between laptop and phone over any network using `psync` — an rsync wrapper over Tailscale.

## Prerequisites

Both devices need:
- **Tailscale** — active and connected (VPN toggle ON)
- **Phone (Termux)** — `sshd` running (`sshd` command in Termux)

## Directory Mapping

| `--dir` name  | Laptop                   | Phone                          |
|---------------|--------------------------|--------------------------------|
| `audio`       | `~/d/audio`              | `/sdcard/Music`                |
| `images`      | `~/d/images`             | `/sdcard/DCIM`                 |
| `notes`       | `~/d/notes`              | `/sdcard/Notes`                |
| `other`       | `~/d/other`              | `/sdcard/Other`                |
| `screenshots` | `~/d/images/screenshots` | `/sdcard/Pictures/Screenshots` |
| `video`       | `~/d/video`              | `/sdcard/Movies`               |

> **Note:** `~/d/images` excludes the `screenshots/` subdirectory to avoid
> overlap — screenshots are synced separately via their own mapping.

## Quick Reference

```bash
# ── Full two-way sync ──────────────────────────────────────────────
psync push && psync pull        # Each device gets missing files

# ── Full sync (one direction) ─────────────────────────────────────
psync push                      # Push all dirs: Laptop → Phone
psync pull                      # Pull all dirs: Phone → Laptop

# ── Single directory ──────────────────────────────────────────────
psync pull --dir notes          # Pull only ~/d/notes ↔ /sdcard/Notes
psync push --dir audio          # Push only ~/d/audio ↔ /sdcard/Music

# ── Single file by name ──────────────────────────────────────────
psync push todo.md              # Push a file by name (searched across all dirs)
psync pull IMG_20260328.jpg     # Pull a file by name

# ── Latest files ──────────────────────────────────────────────────
psync pull --latest             # Pull the single most recent file
psync pull --latest 3           # Pull the 3 most recent files
psync pull --nth 2              # Pull the 2nd most recent file
psync push --latest             # Push the latest local file to phone

# ── Safety / preview ─────────────────────────────────────────────
psync push --dry-run            # Preview what would be pushed
psync pull --dry-run            # Preview what would be pulled
psync push --delete             # Delete phone files not on laptop
psync pull --delete             # Delete local files not on phone
```

## Flags

| Flag              | Description                                                      |
|-------------------|------------------------------------------------------------------|
| `--dir NAME`      | Sync only the named directory (case-insensitive)                 |
| `--latest [N]`    | Sync the N most recent files (default: 1)                        |
| `--nth N`         | Sync the Nth most recent file (1 = latest)                       |
| `--dry-run`       | Show what would be transferred without actually syncing           |
| `--delete`        | Delete destination files not present at source                    |

### Delete behavior

**Neither direction deletes by default** — both push and pull are additive.
This means a full two-way sync is simply:
```bash
psync push && psync pull
```
Each device gets the files it's missing. No files are removed.

Use `--delete` explicitly with either direction to remove extraneous files.

### Excluded files

All dotfiles and dotdirs (`.*`) are globally excluded from syncing.
This prevents transferring `.git/`, `.thumbnails/`, `.DS_Store`, `.Trash-*`, etc.

## Clipboard Integration

When pulling a **single file** (via filename, `--latest`, or `--nth`), the script:
1. Prints the **full local path** of the synced file
2. **Copies the path to clipboard** if in a graphical environment (`DISPLAY` or `WAYLAND_DISPLAY` is set)

When pulling **multiple files** (e.g. `--latest 3`), paths are **not** copied to clipboard.
Check the log file instead.

Supports `xclip`, `xsel`, and `wl-copy`.

## Logging

Each run creates a log file in `/tmp/psync/`:
```
/tmp/psync/2026-03-30T16-49-07.log
```

- Filenames are ISO 8601 timestamps (alphabetical = chronological)
- Contains the full rsync output for every directory/file synced
- Skipped directories are noted
- Log path is printed at the end of each run

## Common Workflows

### Wrote a note on laptop, need it on phone
```bash
psync push --dir notes          # just notes
psync push todo.md              # just that one file
psync push                      # or push everything
```

### Took a photo on phone, need it on laptop
```bash
psync pull --latest             # grab the latest file (path copied to clipboard)
psync pull --latest 5           # grab the last 5 files (check log for paths)
psync pull --dir images         # pull all images
psync pull                      # or pull everything
```

### Grab a specific older photo
```bash
psync pull --nth 2              # 2nd most recent file
psync pull --nth 3              # 3rd most recent file
```

### Sync only screenshots
```bash
psync pull --dir screenshots
psync push --dir screenshots
```

### Two-way sync (e.g. edited notes on both devices)
```bash
psync pull --dir notes          # get phone changes first
# resolve any conflicts manually
psync push --dir notes          # push merged result back
```

### Preview before syncing
```bash
psync push --dry-run            # see what would change on phone
psync pull --dry-run --dir images  # preview image pull only
```

## Missing Directories

If a directory doesn't exist on the source side, it is **skipped with a warning**
rather than failing:
- **Pull:** if the phone directory doesn't exist (e.g. no `Notes` folder), it prints
  `! Remote directory ... does not exist, skipping` and continues.
- **Push:** if the local directory doesn't exist, it prints
  `! Local directory ... does not exist, skipping` and continues.

## Troubleshooting

**"Cannot reach pixel-8"**
1. Open **Tailscale app** on phone → make sure VPN is ON
2. Open **Termux** on phone → run `sshd`
3. Try again

**Phone not showing in `tailscale status`**
```bash
distrobox-host-exec tailscale status
```
If phone is missing, open Tailscale on the phone and re-enable.

**Permission denied (SSH)**
```bash
ssh-copy-id -p 8022 pixel-8
```
Enter your Termux password to re-install the SSH key.

**"Tailscale is not running"**
Disconnect any active VPN first (`dmenu-vpn → Disconnect`), then retry.

## Setup (One-Time)

### Laptop
```bash
# Install Tailscale
curl -fsSL https://tailscale.com/install.sh | sh
sudo tailscale up
```

### Phone (Termux)
```bash
pkg install openssh rsync termux-services
termux-setup-storage
passwd                # set SSH password
sshd                  # start SSH server
sv-enable sshd        # auto-start on Termux launch (after restart)
```

### SSH Key Auth (from laptop)
```bash
ssh-copy-id -p 8022 pixel-8
```
