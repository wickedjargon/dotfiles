# Godot on Debian via Distrobox

This "Golden Path" configuration avoids the OCI runtime, permission, and pathing errors common on minimal Debian/WM setups.

## 1. Host Preparation
Install the container stack and pre-create the export directory to ensure `distrobox-export` has a valid target.

```bash
sudo apt update && sudo apt install -y podman distrobox
mkdir -p ~/.local/bin
```

## 2. Optimized Podman Configuration
This configuration is specifically tuned for Debian Stable to prevent the "Device or Resource Busy" and "Input/output" errors we encountered.

```bash
mkdir -p ~/.config/containers
cat <<EOF > ~/.config/containers/containers.conf
[containers]
log_driver = "k8s-file"

[engine]
cgroup_manager = "cgroupfs"
events_logger = "file"
EOF
```

## 3. Container Creation & Setup
We use Arch Linux for the latest Godot engine. By installing `fontconfig` immediately, we prevent the "Unable to load fontconfig" crash.

```bash
distrobox create --name arch-dev --image archlinux:latest --yes
distrobox enter arch-dev -- sudo pacman -Syu --noconfirm godot fontconfig
```

## 4. Binary Export & PATH Integration
This step makes Godot accessible globally. We force the export to your local bin and ensure your shell knows where to look.

```bash
# Export the binary from the container to the host
distrobox enter arch-dev -- distrobox-export --bin /usr/bin/godot --export-path ~/.local/bin

# Permanent PATH integration for Bash
if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
    printf '\nexport PATH="$HOME/.local/bin:$PATH"\n' >> ~/.bashrc
    source ~/.bashrc
fi
```

---

## Maintenance Cheat Sheet

### To Launch Godot:
Simply type `godot` in your terminal or `dmenu`/`rofi` launcher.

### To Update Godot:
Since Arch is a rolling release, just run this periodically:
```bash
distrobox enter arch-dev -- sudo pacman -Syu
```

### The "Nuclear Option" (If storage locks up):
If you ever see "Device or resource busy" during a cleanup, run this sequence:
1. `podman unshare fusermount -u ~/.local/share/containers/storage/overlay`
2. `pkill -9 -f containers; pkill -9 conmon`
3. `podman unshare rm -rf ~/.local/share/containers`
*(Reboot if it still persists, then repeat step 3).*
