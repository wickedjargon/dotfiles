# My Dot Files

Deploy my dotfiles and install packages to a new debian system using:

```bash
su -
apt update && apt upgrade -y
apt install -y git python3
git clone https://github.com/wickedjargon/dotfiles.git
cd dotfiles
python3 deploy.py
```

