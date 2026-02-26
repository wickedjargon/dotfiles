# My Dot Files

Here are my dotfiles intended for use on a Debian-based system. This repo is mainly for my own use, but feel free to test run them on your own system.

## Deployment

Deploy my dotfiles and install packages to a new Debian system:

```bash
su -
apt update && apt upgrade -y
apt install -y git python3
git clone https://github.com/wickedjargon/dotfiles.git
cd dotfiles
python3 deploy.py
```

