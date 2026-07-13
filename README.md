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

## Post-Deployment

After successfully running the deployment script, complete these manual steps:

### 1. Verify Deployment

```bash
~/d/projects/dotfiles/tests-live/run_tests.sh myuser
```

### 2. Firmware Update

Update system firmware. Keep laptop on AC power.

```bash
fwupdmgr refresh --force
fwupdmgr get-updates
fwupdmgr update
```

### 3. Restore Private Files

Configure `rclone` to authenticate with Google Drive, then pull down your private files:

```bash
rclone config
rclone sync gdrive:Backup/new-user/d/      ~/d/      --progress
rclone sync gdrive:Backup/new-user/.ssh/   ~/.ssh/   --progress
rclone sync gdrive:Backup/new-user/.gnupg/ ~/.gnupg/ --progress
chmod 700 ~/.ssh ~/.gnupg
chmod 600 ~/.ssh/id_ed25519
```

### 4. Verify Keyring Unlock

Log out and back in, then confirm the login keyring unlocks without a prompt:

```bash
echo -n test | secret-tool store --label=test check me
secret-tool lookup check me
secret-tool clear check me
```

If prompted for a keyring password, delete stale keyrings and log in again:

```bash
rm -rf ~/.local/share/keyrings/*
```

### 5. Git & GitHub

```bash
git config --global user.name "Farzin Firouzi"
git config --global user.email "farzineff@gmail.com"
gh auth login
```

### 6. Password Manager

Clone the password store:

```bash
pass git clone git@github.com:wickedjargon/pass-store.git ~/.password-store
```

### 7. Android Password Sync

Install the following from F-Droid:

- [OpenKeychain](https://f-droid.org/packages/org.sufficientlysecure.keychain/)
- [Password Store](https://f-droid.org/packages/dev.msfjarvis.aps/)

Export and transfer the GPG key to the phone:

```bash
gpg --armor --export-secret-keys > /tmp/private-key.asc
adb push /tmp/private-key.asc /sdcard/Download/private-key.asc
```

On the phone, import the key in OpenKeychain, then clone
`git@github.com:wickedjargon/pass-store.git` inside Password Store.

Delete the exported private key from both devices afterwards:

```bash
rm /tmp/private-key.asc
adb shell rm /sdcard/Download/private-key.asc
```

### 8. Mullvad VPN Setup

To configure the `vpn` tool to manage your Mullvad WireGuard connections, follow the guide here:

[How to Set Up Mullvad VPN](how-to/mullvad-vpn.md)

### 9. Tailscale & psync

Join the tailnet on the laptop:

```bash
sudo tailscale up
```

Then complete the one-time `psync` setup (see [how-to/psync.md](how-to/psync.md)):

1. **Phone:** install the Tailscale app and join the tailnet.
2. **Phone (Termux):**

   ```bash
   pkg install openssh rsync termux-services
   termux-setup-storage
   passwd                # set SSH password
   sshd                  # start SSH server
   sv-enable sshd        # auto-start on Termux launch (after restart)
   ```

3. **Laptop:** install the SSH key and verify:

   ```bash
   ssh-copy-id -p 8022 pixel-8
   psync status
   ```

### 10. archbox (Arch Distrobox)

```bash
# On the HOST:
~/d/projects/dotfiles/scripts/create-archbox.sh   # add --replace to rebuild from scratch

# Inside the container:
distrobox enter archbox
python3 ~/d/projects/dotfiles/scripts/install_arch_packages.py
~/d/projects/dotfiles/scripts/setup-steam.sh
```

### 11. Bluetooth Setup

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

### 12. Vimium Key Mappings

I set `Custom key mappings` in the Vimium extension to the below so that i only have a small subset of the keybindings the extension uses.


```
unmapAll
map h scrollLeft
map j scrollDown
map k scrollUp
map l scrollRight
map d scrollPageDown
map u scrollPageUp
map gg scrollToTop
map G scrollToBottom
map 0 zoomReset
map = zoomIn
map - zoomOut
map H goBack
map L goForward
```

### 13. Install Offline Speech Models

```bash
kokoro --install   # ~380 MB
stt --install      # ~200 MB
```
