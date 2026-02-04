# My Dot Files

Here are my dotfiles intended for use on a Debian-based system. 

- `deploy.py` - deploys dotfiles and installs packages
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

### Step 5: Setup firefox extensions

- launch firefox to trigger extension installation

### step 6: set up bluetooth devices

```
sudo systemctl enable --now bluetooth
bluetoothctl
[bluetooth]# scan on
[bluetooth]# pair XX:XX:XX:XX:XX:XX
[bluetooth]# trust XX:XX:XX:XX:XX:XX
[bluetooth]# connect XX:XX:XX:XX:XX:XX
[bluetooth]# exit
systemctl --user restart wireplumber pipewire pipewire-pulse
```

## Post-Install

- Turn off Firefox hardware acceleration for PCs on older Sandy Bridge CPUs
- make firefox compact (right click on address bar, customize, select `density`)
- pair bluetooth devices
