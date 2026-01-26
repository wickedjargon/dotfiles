# My Dot Files

Here are my dotfiles intended for use on a Debian-based system. 

- `deploy.py` - deploys dotfiles and installs packages
- `setup-ssh-repos.py` - converts wickedjargon repos to use SSH



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

### Step 2: Copy SSH Keys (optional)

From your old computer, copy SSH keys to the new system:

```bash
scp -r ~/.ssh/ new_user@new_hostname:~/
```

### Step 3: Copy ~/d/ Directory (optional)

From your old computer, copy your projects directory:

```bash
scp -r ~/d/ new_user@new_hostname:~/
```

### Step 4: Setup SSH Repos (optional)

If you copied your SSH keys in Step 2, login to new user and convert your wickedjargon repos to use SSH:

```bash
python3 setup-ssh-repos.py
```

### Step 5: Setup firefox extensions

- launch firefox to trigger extension installation

## Post-Install

- Turn off Firefox hardware acceleration for PCs on older Sandy Bridge CPUs
- make firefox compact 
- make firefox use sidebar for tabs
- set up qt theme
- setup bluetooth
- pair bluetooth devices

## TODOs

- [ ] automated bluetooth setup
- [x] allow extensions in private browser
- [ ] pairing bluetooth instructions
- [ ] add unit tests for `deploy.py` and `setup-ssh-repos.py`
- [x] replace hardcoded paths with `$HOME` in scripts (e.g., `dmenu-virtual-machines`)
- [ ] add `--dry-run` flag to `deploy.py`
- [ ] add error recovery/resume capability to `deploy.py`
