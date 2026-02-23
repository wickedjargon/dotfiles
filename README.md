# My Dot Files

Here are my dotfiles intended for use on a Debian-based system. 

- `deploy.py` - deploys dotfiles and installs packages (TUI mode)
- `deploy_cli.py` - deploys dotfiles and installs packages (CLI mode, for automation/LLMs)
- `setup-ssh-repos.py` - converts wickedjargon repos to use SSH
- `firefox-ui-config.sh` - to adjust firefox ui settings

### Step 1: Deployment

Deploy my dotfiles and install packages to a new Debian system:

```bash
su -
apt update && apt upgrade -y
apt install -y git python3
git clone https://github.com/wickedjargon/dotfiles.git
cd dotfiles
python3 deploy.py
```

#### CLI Mode (for automation / LLM testing)

```bash
# Full deployment, non-interactive
sudo python3 deploy_cli.py --username myuser --password mypass --yes

# Deploy to existing user
sudo python3 deploy_cli.py --username existinguser --yes

# Preview what would happen (no root required)
python3 deploy_cli.py --username myuser --dry-run

# JSON output for LLM consumption
python3 deploy_cli.py --username myuser --dry-run --json
```

### Step 2: Copy SSH Keys and files from old computer (optional)

From your old computer, copy SSH keys to the new system:

```bash
scp -r ~/.ssh/ new_user@new_hostname:~/
```

### Step 4: Setup SSH Repos (optional)

If you copied your SSH keys in Step 2, login to new user and convert your wickedjargon repos to use SSH:

```bash
python3 setup-ssh-repos.py
```

### Step 5: Setup Google API for dmenu-gcal

copy `credentials.json` to `~/.config/dmenu-gcal/`

### Step 6: Setup firefox extensions

- launch firefox to trigger extension installation
- In Dark Reader: click ⚙ Settings → Automation → enable **"System color scheme"** (so `theme --toggle` auto-disables Dark Reader in light mode)

### Step 7: set up bluetooth devices

```
sudo systemctl enable --now bluetooth
bluetoothctl
[bluetooth]# scan on
[bluetooth]# pair XX:XX:XX:XX:XX:XX
[bluetooth]# trust XX:XX:XX:XX:XX:XX
[bluetooth]# connect XX:XX:XX:XX:XX:XX
[bluetooth]# exit
systemctl --user restart pulseaudio
```

## Post-Install
- run `post-deployment-tests/run_tests.sh` to verify everything is working
- browse to https://septatrix.github.io/prefers-color-scheme-test/ to verify dark mode is working. This should be set in bspwmrc by `gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'`
- In Dark Reader: click ⚙ Settings → Automation → enable **"System color scheme"** (so `theme --toggle` auto-disables Dark Reader in light mode)
- Turn off Firefox hardware acceleration for PCs on older Sandy Bridge CPUs
- make firefox compact (right click on address bar, customize, select `density`)
- pair bluetooth devices

## TODOS
- [x] light/dark theme universal toggle. toggles (gtk/qt/bspwm/polybar/xob/emacs/antigravity/notification daemon /dmenu and gsettings/ xdg-desktop-portal-gtk in bspwm)
- [x] deploy.py should handle `.patch` files in a cleaner way. perhaps create a patches directory and have deploy.py apply them. script still uses old logic with non existing dir system-configs.
- [ ] update deploy script to automatically add `contrib` and `non-free` components to `sources.list`
- [ ] make a rofi version of dmenu-explorer where the second item is selected first (so not `..` which is parent directory). run as a pilot project to see if rofi is a viable replacement for dmenu.
- [ ] create a download system for torrents / youtube downloader via rss / etc. prevents donwloading via hotspot network 
names. determine if there is a better way to identify hotspot network connections. might require bit torrent daemon.
- [ ] consider switching from pulseuadio to pipewire / wireplumbler
  - currently not experiencing any major issues with pulseaudio, although audio streaming is not possible using pulseaudio.
- [ ] offline games, developed by myself using godot
- [ ] dosbox games
- [ ] auto connect irc. irc notify-send notifications for tagged messages
- [ ] create script that finds rss feed given youtube channel name
- [x] create toggle dark / light theme. changes it for all packages. (bspwm, polybar, xob, firefox, emacs, antigravity)
- [x] consider setting `FastConnectable = false` in `/etc/bluetooth/main.conf` after testing on current system
- [x] reverting my shell from fish back to bash.
- [x] poly-weather should not display anything when it has no internet connection
- [ ] poly-weather should display rain right now, but it is not
- [ ] volume control should only show multiples of 5.
- [ ] common colors among bspwm, dmenu, bspwm, XOB, etc should be inherited from a dot file or system variable like `$DESKTOP_THEME_BG_COLOR`, etc.
- [ ] script to insert calendar item using natural language processing (ex: "Doctor appointment next firday - 5pm" is parsed and calendar is updated). basic natural language can be done locally but use google's free api for more complex requests.
- [ ] script to set alarm (might not need this if google integration works)
- [ ] notifications regarding calendar items
- [ ] set up research environment
- [x] display volume change using graphical scale.

## Future Features Ideas: Daemon Integration

Goal: Decouple backend logic from frontend interfaces to improve system speed, persistence, and scriptability.

### - [ ] Universal Downloader: `aria2`
Replace browser download managers and torrent clients with a single headless RPC daemon.
* **Backend:** `aria2c --enable-rpc`
* **Frontends:** Browser extension (Aria2 integration), Emacs package, `aria2p` (CLI).
* **Benefit:** Centralized queue, lower resource usage, controllable via scripts/Emacs.

### - [ ] File Synchronization: `Syncthing`
Peer-to-peer, continuous file synchronization daemon.
* **Usage:** Sync `~/org`, `~/documents`, and dotfiles across devices without third-party clouds.
* **Benefit:** Private, instant local network sync, versioning support.

### - [ ] Session Persistence: `tmux` (Headless)
Treat terminal sessions as a service that survives window manager crashes or reboots.
* **Setup:** Auto-start tmux server on boot; terminal emulator acts merely as a viewer.
* **Benefit:** Never lose compile jobs or SSH sessions; seamless context switching.

### - [ ] Async Mail: `isync` (mbsync) + `mu`
Decouple email fetching from the email client (Emacs/mu4e).
* **Backend:** Systemd timer triggers `mbsync` to fetch and `mu index` to index.
* **Benefit:** Instant load times in mail client; no blocking UI while checking mail.

