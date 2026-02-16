# How to Export GUI Apps from Distrobox to Host

This guide explains how to install a GUI application inside a Distrobox container (like Arch Linux) and export it so it appears in your host system's application launcher (e.g., Rofi, dmenu, GNOME/KDE menus).

## Prerequisites

- A running Distrobox container (e.g., `arch-dev`)
- `distrobox` installed on the host

## Step-by-Step Guide (Example: Discord)

### 1. Enter the Container
First, enter your Distrobox container.

```bash
distrobox enter arch-dev
```

### 2. Install the Application
Install the application using the container's package manager.

**For Arch Linux:**
```bash
sudo pacman -S discord
```

**For Ubuntu/Debian:**
```bash
sudo apt install discord
```

### 3. Export the Application
While **still inside the container**, use `distrobox-export` to expose the application to the host.

```bash
distrobox-export --app discord
```

**Output should look like:**
> Application discord successfully exported.
> OK!
> discord will appear in your applications list in a few seconds.

### 4. Verify on Host
Exit the container or open a new terminal on the host to verify.

The export process creates a `.desktop` file in your local applications directory.

```bash
ls -l ~/.local/share/applications/*discord*
```

You should see a file named something like `arch-dev-discord.desktop`.

### 5. Launch
Open your application launcher (Rofi, etc.) and search for the app. It will usually be suffixed with the container name, e.g., "**Discord (on arch-dev)**".

## Troubleshooting

### Icon Missing?
If the icon doesn't appear, ensure the icon files were exported. `distrobox-export` tries to copy them, but sometimes manual intervention is needed.

Check if icons exist in `~/.local/share/icons` or `~/.local/share/pixmaps`.

### App Not Launching?
Check the `.desktop` file `Exec` line to see how it's trying to launch.

```bash
cat ~/.local/share/applications/arch-dev-discord.desktop | grep Exec
```

It should look like:
`Exec=/usr/bin/distrobox-enter -n arch-dev -- /usr/bin/discord`

Try running that command manually in your host terminal to see error messages.
