# My Dot Files

Here are my dotfiles intended for use on a Debian-based system. 

- `deploy.py` - deploys dotfiles and installs packages
- `setup-ssh-repos.py` - converts wickedjargon repos to use SSH


### Step 1: Copy SSH Keys (optional)
```bash
scp -r ~/.ssh/ new_user@new_hostname:~/
```

### Step 2: Copy ~/d/ Directory (optional)

```bash
scp -r ~/d/ new_user@new_hostname:~/
```

### Step 3: Deployment

Deploy my dotfiles and install packages to a new Debian system:

```bash
su -
apt update && apt upgrade -y
apt install -y git python3
git clone https://github.com/wickedjargon/dotfiles.git
cd dotfiles
python3 deploy.py
```

### Step 4: Setup SSH Repos (optional)

If you copied your SSH keys in Step 1, login to new user and convert your wickedjargon repos to use SSH:

```bash
python3 setup-ssh-repos.py
```

## Post-Install

- set up qt theme
- setup bluetooth
- Turn off Firefox hardware acceleration for PCs on older Sandy Bridge CPUs
- make firefox compact 
- make firefox use sidebar for tabs
- These firefox extensions are included in `firefox-extensions.sh`. Go to about:policies in firefox to trigger their installation:
  - Dark Reader - dark mode for websites
  - uBlock Origin - ad blocker
  - Vimium - keyboard navigation
  - SponsorBlock - skip YouTube sponsors
  - I Still Don't Care About Cookies - remove cookie banners
  - Read Aloud - text-to-speech
  - Defund Wikipedia - remove donation banners

# Features to be added to script

- setup bluetooth
