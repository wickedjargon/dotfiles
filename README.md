# My Dot Files

Here are my dotfiles intended for use on a Debian-based system. This repo is mainly for my own use, but feel free to test run them on your own system.

## Repository Structure

| Path | Description |
|---|---|
| `deploy.py` | TUI that deploys dotfiles and installs packages |
| `deploy_cli.py` | CLI that deploys dotfiles and installs packages |
| `dotfiles-overlay/` | Filesystem mirror copied to matching system paths |
| `dotfiles-patches/` | Patches applied to system files post-deploy |
| `firefox/` | Firefox extensions and `user.js` config |
| `packages/` | Package lists |
| `scripts/` | Install and setup helper scripts |
| `tests-live/` | Shell-based integration tests |
| `tests-unit/` | Python unit tests |


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
