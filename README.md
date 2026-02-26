# My Dot Files

Here are my dotfiles intended for use on a Debian-based system. These notes are mainly for my own reference. If you wish to test run my dotfiles, skip over step 3 completely as it only applies to me.

- `deploy.py` - deploys dotfiles and installs packages (TUI mode)
- `deploy_cli.py` - deploys dotfiles and installs packages (CLI mode)
- `scripts/setup-ssh-repos.py` - converts wickedjargon repos to use SSH
- `scripts/distrobox-*` - scripts for distrobox system deployment

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

### Step 2: Setting up SSH Keys/Repos (skip to step 3 if you are not me)

#### a: Copy SSH Keys and files from old computer

From your old computer, copy SSH keys to the new system:

```bash
scp -r ~/.ssh/ new_user@new_hostname:~/
```


#### b: Setup SSH Repos

If you copied your SSH keys in Step 2, login to new user and convert your wickedjargon repos to use SSH:

```bash
python3 setup-ssh-repos.py
```

#### c: Setup Google API for dmenu-gcal

copy `credentials.json` to `~/.config/dmenu-gcal/`

#### d: Setup firefox extensions

- launch firefox to trigger extension installation
- In Dark Reader: click ⚙ Settings → Automation → enable **"System color scheme"** (so `theme --toggle` auto-disables Dark Reader in light mode)

#### e: set up bluetooth devices

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

## Step 3: Post-Install
- Run `tests-live/run_tests.sh` to verify everything is working
- Browse to https://septatrix.github.io/prefers-color-scheme-test/ to verify dark mode is working. 
- In Dark Reader: click ⚙ Settings → Automation → enable **"System color scheme"** (so `theme --toggle` auto-disables Dark Reader in light mode)
- Turn off Firefox hardware acceleration for PCs on older Sandy Bridge CPUs
- Make Firefox compact (right click on address bar, customize, select `density`)
- Pair bluetooth devices

## CLI Mode

```bash
# Full deployment, non-interactive
sudo python3 deploy_cli.py --username myuser --password mypass --yes

# Deploy to existing user
sudo python3 deploy_cli.py --username existinguser --yes

# Dry Run -- Preview what would happen
python3 deploy_cli.py --username myuser --dry-run

# JSON output
python3 deploy_cli.py --username myuser --dry-run --json
```

