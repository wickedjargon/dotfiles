# My Dot Files

Here are my dotfiles intended for use on a Debian-based system. Feel free to use them if you like. Use `super + /` to get the keybindings.

## Repository Structure

| Path | Description |
|---|---|
| `deploy.py` | CLI entry point — deploys dotfiles and installs packages |
| `deploy_lib.py` | Business logic library used by `deploy.py` |
| `dotfiles-overlay/` | Filesystem mirror copied to matching system paths |
| `dotfiles-overlay/…/.local/bin/` | `pass`-inspired CLI utilities — see [CLI_DESIGN.md](CLI_DESIGN.md) |
| `dotfiles-patches/` | Patches applied to system files post-deploy |
| `firefox/` | Firefox extensions and `user.js` config |
| `packages/` | Package lists |
| `packages/archbox.ini` | Declarative definition of the `archbox` Arch Distrobox container |
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

## archbox (Arch Distrobox)

Heavier and bleeding-edge GUI apps (Steam, Android SDK, VS Code, Godot,
Telegram, Chrome, …) run inside an Arch Linux [Distrobox](https://distrobox.it/)
container named `archbox`, on top of the stable Debian host. Recreate it from
scratch with:

```bash
# On the HOST — build the container from packages/archbox.ini:
./scripts/create-archbox.sh            # add --replace to rebuild from scratch

# Then INSIDE the container, as your normal user:
distrobox enter archbox
python3 ~/d/projects/dotfiles/scripts/install_arch_packages.py   # pacman + AUR (bootstraps yay)
~/d/projects/dotfiles/scripts/setup-steam.sh                     # Steam + lib32 multilib libs
```

Your home directory is shared with the host, so the dotfiles overlay is already
present inside the container — only packages and the container itself need
recreating.

Package lists live in `packages/arch-pacman-packages.txt` and
`packages/arch-aur-packages.txt`. Keep them honest with:

```bash
# Inside archbox — report drift between the lists and what's installed:
~/d/projects/dotfiles/scripts/arch-sync-packages.sh
~/d/projects/dotfiles/scripts/arch-sync-packages.sh --write   # rewrite lists from live state
```

`tests-live/arch-packages.sh` verifies every listed package is installed (it is
a no-op on the Debian host, where `pacman` is absent).

## Post-Deployment

After successfully running the deployment script, complete these manual steps:

### 1. Firmware Update

Update system firmware Keep laptop on AC power.

```bash
fwupdmgr refresh --force
fwupdmgr get-updates
fwupdmgr update
```

### 2. Bluetooth Setup

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

### 3. Restore Private Files

Configure `rclone` to authenticate with Google Drive, then pull down your private files:

```bash
distrobox-host-exec rclone config
rclone sync gdrive:Backup/new-user/d/      ~/d/      --progress
rclone sync gdrive:Backup/new-user/.ssh/   ~/.ssh/   --progress
rclone sync gdrive:Backup/new-user/.gnupg/ ~/.gnupg/ --progress
chmod 700 ~/.ssh ~/.gnupg
chmod 600 ~/.ssh/id_ed25519
```

### 4. Password Manager

Import the restored GPG key and clone the password store:

```bash
gpg --import  # key is already restored from gdrive backup above
pass git clone git@github.com:wickedjargon/pass-store.git ~/.password-store
```

### 5. Android Password Sync

Install the following from F-Droid:

- [OpenKeychain](https://f-droid.org/packages/org.sufficientlysecure.keychain/)
- [Password Store](https://f-droid.org/packages/dev.msfjarvis.aps/)

Export and transfer the GPG key to the phone:

```bash
gpg --armor --export-secret-keys > /tmp/private-key.asc
adb push /tmp/private-key.asc /sdcard/Download/private-key.asc
```

### 6. Mullvad VPN Setup

To configure the `vpn` tool to manage your Mullvad WireGuard connections, including the required `sudoers` setup, follow the guide here:

[How to Set Up Mullvad VPN](how-to/mullvad-vpn.md)
