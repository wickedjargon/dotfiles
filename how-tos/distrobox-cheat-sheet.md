# Distrobox Cheat Sheet

also see: https://wiki.archlinux.org/title/Distrobox

## 1. Creating Containers

**Basic creation (uses default image, usually Fedora)**
```bash
distrobox create -n [box-name]
```

**Create with a specific image (Arch, Ubuntu, Debian, Alpine, etc.)**
```bash
distrobox create -n [box-name] -i [image:tag]
# Example: distrobox create -n arch-dev -i archlinux:latest
```

**Create with a specific home directory (keeps dotfiles separate)**
```bash
distrobox create -n [box-name] -i [image] --home /path/to/custom/home
```

**Clone an existing distrobox**
```bash
distrobox create --name [new-name] --clone [existing-name]
```

**Pull the image but don't create the container yet**
```bash
distrobox create -n [box-name] -i [image] --dry-run
```

## 2. Entering & Executing

**Enter a container**
```bash
distrobox enter [box-name]
```

**Enter directly as root**
```bash
distrobox enter [box-name] --root
```

**Run a single command without entering shell**
```bash
distrobox enter [box-name] -- [command]
# Example: distrobox enter arch-dev -- sudo pacman -S neovim
```

## 3. Integration (Run INSIDE the container)

**Export a GUI application to the host's menu**
```bash
distrobox-export --app [package_name]
# Example: distrobox-export --app visual-studio-code
```

**Export a binary/CLI tool to the host**
```bash
distrobox-export --bin /path/to/binary --export-path ~/.local/bin
# Example: distrobox-export --bin /usr/bin/git --export-path ~/.local/bin
```

**Export a systemd service**
```bash
distrobox-export --service [service_name]
```

## 4. Host Interaction (Run INSIDE the container)

**Execute a command on the Host OS from inside the box**
```bash
distrobox-host-exec [command]
# Example: distrobox-host-exec flatpak run com.spotify.Client
```

## 5. Management

**List all containers (shows status and image)**
```bash
distrobox list
```

**Stop a running container**
```bash
distrobox stop [box-name]
```

**Remove a container**
```bash
distrobox rm [box-name]
```

**Force remove a container (even if running)**
```bash
distrobox rm -f [box-name]
```

## 6. Maintenance

**Upgrade all packages inside a container**
```bash
distrobox upgrade [box-name]
```

**Upgrade all packages in ALL containers**
```bash
distrobox upgrade --all
```

## 7. Useful Flags

**Common Flags**
```bash
--root           # Run the container/command as root
--verbose        # Show detailed logs
--init           # Use an init system (systemd/openrc) inside the box
```

## 8. Typical Session
*A typical workflow showing checking status, entering an Arch container, running an update, and exiting.*

```console
user@host:~$ distrobox list
ID           | NAME      | STATUS             | IMAGE
1a2b3c4d5e6f | arch-dev  | Up 2 hours ago     | quay.io/toolbx-images/archlinux-toolbox:latest
7g8h9i0j1k2l | ubuntu-22 | Exited 1 day ago   | ubuntu:22.04

user@host:~$ distrobox enter arch-dev
Starting container arch-dev
[OK] Container arch-dev started successfully.
user@arch-dev:~$

user@arch-dev:~$ cat /etc/os-release | grep PRETTY_NAME
PRETTY_NAME="Arch Linux"

user@arch-dev:~$ sudo pacman -Syu
:: Synchronizing package databases...
 core is up to date
 extra is up to date
:: Starting full system upgrade...
 there is nothing to do

user@arch-dev:~$ exit
logout
user@host:~$
```
