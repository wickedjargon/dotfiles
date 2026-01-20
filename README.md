# My Dot Files

Deploy my dotfiles and install packages to a new Debian system:

```bash
su -
apt update && apt upgrade -y
apt install -y git python3
git clone https://github.com/wickedjargon/dotfiles.git
cd dotfiles
python3 deploy.py
```

## Post-Install

- Turn off Firefox hardware acceleration for PCs on older Sandy Bridge CPUs
- make firefox compact 
- make firefox use sidebar for tabs
- set up qt theme
- bluetooth

# Features to be added to script

- these firefox extensions are included in `firefox-extensions.sh`. Go to about:policies in firefox to trigger their installation:
  - Dark Reader - dark mode for websites
  - uBlock Origin - ad blocker
  - Vimium - keyboard navigation
  - SponsorBlock - skip YouTube sponsors
  - I Still Don't Care About Cookies - remove cookie banners
  - Read Aloud - text-to-speech
  - Defund Wikipedia - remove donation banners
- setup bluetooth
