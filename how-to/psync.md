# psync (Laptop ↔ Pixel 8)

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
psync sync                      # Push then pull in one command
psync push && psync pull        # Same thing, manually

# ── Full sync (one direction) ─────────────────────────────────────
psync push                      # Push all dirs: Laptop → Phone
psync pull                      # Pull all dirs: Phone → Laptop

# ── Single directory ──────────────────────────────────────────────
psync pull --dir notes          # Pull only ~/d/notes ↔ /sdcard/Notes
psync push --dir audio          # Push only ~/d/audio ↔ /sdcard/Music

# ── Multiple directories ─────────────────────────────────────────
psync push --dir notes audio    # Push notes and audio only
psync pull --dir notes,audio    # Same (comma-separated also works)

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
psync push --update             # Skip destination files that are newer

# ── Info commands ─────────────────────────────────────────────────
psync status                    # Show reachability and last sync times
psync log                       # Open latest log in pager

# ── Quiet mode ────────────────────────────────────────────────────
psync push --quiet              # Sync silently (errors still shown)
psync sync --quiet --dir notes  # Quiet two-way sync of notes only
```

## Flags

| Flag              | Description                                                      |
|-------------------|------------------------------------------------------------------|
| `--dir NAME...`   | Sync only the named directories (supports multiple)              |
| `--latest [N]`    | Sync the N most recent files (default: 1)                        |
| `--nth N`         | Sync the Nth most recent file (1 = latest)                       |
| `--dry-run`       | Show what would be transferred without actually syncing          |
| `--delete`        | Delete destination files not present at source                   |
| `--quiet`         | Suppress output except errors (log file still written)           |
| `--update`        | Skip files that are newer on the destination side                |

### Delete behavior

**Neither direction deletes by default** — both push and pull are additive.
This means a full two-way sync is simply:
```bash
psync sync
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

## Desktop Notifications

When running in a graphical environment, `psync` sends a desktop notification
via `notify-send` on completion with a summary of what was transferred.

## Transfer Summary

After each sync, psync prints a summary line showing the number of files
transferred and total size (e.g. `3 files transferred (1.2 MB)`).
This is parsed from rsync's `--stats` output.

## Logging

Each run creates a log file in `/tmp/psync/`:
```
/tmp/psync/2026-03-30T16-49-07.log
```

- Filenames are ISO 8601 timestamps (alphabetical = chronological)
- Contains the full rsync output for every directory/file synced
- Skipped directories are noted
- Log path is printed at the end of each run

Use `psync log` to quickly open the latest log in your pager.

## Status

`psync status` shows:
- Phone reachability (quick TCP check, no SSH required)
- Last push timestamp (parsed from logs)
- Last pull timestamp (parsed from logs)

```
✓ Phone: reachable (pixel-8:8022)
  Last push: 2026-03-31 10:15:07 (26 min ago)
  Last pull: 2026-03-31 09:42:33 (59 min ago)
```

## Configuration

psync loads `~/.config/psync.conf` at startup if it exists. All values are
optional — anything not specified uses the built-in defaults.

```ini
[connection]
host = pixel-8
port = 8022
base_path = /data/data/com.termux/files/home/storage/shared

[directories]
# Override local or remote paths per directory.
# Remote paths are relative to base_path unless they start with /.
notes_local = ~/d/notes
notes_remote = Notes
```

## Connection Handling

- **Fast pre-check**: Uses a TCP socket connect (not SSH) for initial
  reachability, saving ~1-2 seconds per invocation.
- **Automatic retry**: If the initial connection fails, psync waits 2 seconds
  and retries once before failing. This handles Tailscale tunnel warm-up.

## Signal Handling

Ctrl-C during a sync prints a clean "Sync interrupted" message and closes the
log file properly. Exit code is 130 (standard for SIGINT).

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
psync sync --dir notes          # push then pull in one command
# Or manually:
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

### Android Settings (Crucial for Reliability)
To prevent Android from killing the SSH server when the screen is off or in the background, you **must** configure the following on your phone:
1. **Battery Optimization**: Go to Android Settings → Apps → Termux → Battery and set it to **Unrestricted**.
2. **Wake Lock**: Pull down your Android notification shade, expand the Termux notification, and tap **Acquire wakelock** (or run `termux-wake-lock` in the terminal).

### SSH Key Auth (from laptop)
```bash
ssh-copy-id -p 8022 pixel-8
```
