# Firefox Extension Auto-Installation

This directory contains Firefox policies that automatically install your essential browser extensions.

## How It Works

Firefox supports enterprise policies that work for personal use. The `policies.json` file tells Firefox to automatically install specified extensions on first launch after deployment.

## Included Extensions

1. **Dark Reader** - Dark mode for all websites
2. **uBlock Origin** - Ad blocker and content filter
3. **Vimium** - Keyboard navigation for Firefox
4. **SponsorBlock** - Skip sponsored segments in YouTube videos
5. **I Still Don't Care About Cookies** - Remove cookie consent banners
6. **Read Aloud** - Text-to-speech for web pages  
7. **Defund Wikipedia** - Remove Wikipedia donation banners

## Installation

This directory is automatically deployed to `~/.mozilla/firefox/distribution/` when you run `deploy.py`.

## Usage

1. Deploy your dotfiles with `sudo ./deploy.py`
2. Launch Firefox
3. Extensions will automatically install on first launch
4. Extensions auto-update through Firefox

## Manual Installation

If you prefer to install extensions manually, simply don't deploy this directory and install them from:
- [Firefox Add-ons](https://addons.mozilla.org/)

## Modifying Extensions

To add or remove extensions:

1. Edit `policies.json`
2. Find the extension ID from its Mozilla Add-ons URL
3. Add a new entry following the existing format
4. Redeploy dotfiles

## Notes

- Extensions only install from Mozilla Add-ons (AMO)
- Installation happens once per new Firefox profile
- Removing an extension from policies.json won't uninstall it (do that manually)
- All extensions are set to `force_installed` mode
