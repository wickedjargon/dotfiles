# Phone Sync (Laptop ↔ Pixel 8)

Sync files between laptop and phone over any network using `rsync` + Tailscale.

## Prerequisites

Both devices need:
- **Tailscale** — active and connected (VPN toggle ON)
- **Phone (Termux)** — `sshd` running (`sshd` command in Termux)

## Quick Reference

```bash
# Push laptop files to phone
sync-phone push

# Pull phone files to laptop
sync-phone pull
```

## Directory Mapping

| Laptop            | Phone                          |
|-------------------|--------------------------------|
| `~/d/audio`       | `/sdcard/Music`                |
| `~/d/images`      | `/sdcard/DCIM`                 |
| `~/d/notes`       | `/sdcard/Notes`                |
| `~/d/other`       | `/sdcard/Other`                |
| `~/d/screenshots` | `/sdcard/Pictures/Screenshots` |
| `~/d/video`       | `/sdcard/Movies`               |

## Common Workflows

### Wrote a note on laptop, need it on phone
```bash
sync-phone push
```

### Took a photo on phone, need it on laptop
```bash
sync-phone pull
```
Photos from the phone's camera (`/sdcard/DCIM`) sync directly into `~/d/images`.

### Two-way sync (e.g. edited notes on both devices)
```bash
sync-phone pull   # get phone changes first
# resolve any conflicts manually
sync-phone push   # push merged result back
```

> **⚠️ Warning:** `sync-phone` uses `--delete`, meaning the destination
> mirrors the source exactly. If you `push`, files only on the phone
> (in synced dirs) will be deleted. If you `pull`, files only on the
> laptop will be deleted. Always pull before push for two-way sync.

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
