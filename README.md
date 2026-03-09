# My Dot Files

Here are my dotfiles intended for use on a Debian-based system. This repo is mainly for my own use, but feel free to test run them on your own system.

## Repository Structure

| Path | Description |
|---|---|
| `deploy.py` | CLI entry point — deploys dotfiles and installs packages |
| `deploy_lib.py` | Business logic library used by `deploy.py` |
| `dotfiles-overlay/` | Filesystem mirror copied to matching system paths |
| `dotfiles-patches/` | Patches applied to system files post-deploy |
| `firefox/` | Firefox extensions and `user.js` config |
| `packages/` | Package lists |
| `scripts/` | Install and setup helper scripts |
| `tests-live/` | Shell-based integration tests |
| `tests-unit/` | Python unit tests |


## Deployment

Deploy my dotfiles and install packages to a new Debian system:

```bash
su -
apt update && apt upgrade -y
apt install -y git python3
git clone https://github.com/wickedjargon/dotfiles.git
cd dotfiles
python3 deploy.py --username myuser --password mypass --yes
```

## Post-Deployment

After successfully running the deployment script, complete these manual steps:

### 1. Bluetooth Setup

Pair and trust your Bluetooth devices via `bluetoothctl`:

```bash
bluetoothctl
power on
agent on
default-agent
scan on
pair XX:XX:XX:XX:XX:XX
trust XX:XX:XX:XX:XX:XX
connect XX:XX:XX:XX:XX:XX
```

### 2. Restore Private Files

Configure `rclone` to authenticate with Google Drive, then pull down your private `~/d/` directory:

```bash
distrobox-host-exec rclone config
rclone copy gdrive:Backup/d/ ~/d/ --progress
```
