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

## working on adding:

### bookmarks script
- create a script that parses my bookmarks file and provides a rofi or dmenu front-end
- open book items using api + emacs or online resource for ISBN
- open union urls using tor browser file:///
- open youtube urls with mpv (account for both youtube domain names)
- open files online in their default program (example: pdf in zathura)
- open text files with `emacsclient -c`
- a file is identified with `file:///...`, `/home/...`, or `~/` 
- other filetypes are opened with default program
- convert some urls to use lightweight mirrored sites:
    - `reddit.com` to `old.reddit.com` or `libreddit.kavin.rocks`
    - `x.com` to `xcancel.com` or `nitter.net`
    - `twitch.tv` to `twitchls.com`
    - `youtube.com` to `piped.video`

below is an example of the bookmarks file to be parsed

the format of the bookmarks file will be this (one example of each item type - url, book, file, ):

```
# title bookmark
optional notes
https://url-goes.here/

# book title
optional notes
- ISBN: 1234567891234

# file title
optional notes
/home/ff/d/books/the_selfish_gene.pdf
```

a real example would be

```
# Odysee
youtube alternative
https://odysee.com

# PeerTube
youtube alternative
https://joinpeertube.org

# The Selfish Gene
ISBN: 9780198575191
```


## TODOS
- [ ] script to set alarm
- [ ] script to insert calendar item using natural language processing (ex: "Doctor appointment next firday - 5pm" is parsed and calendar is updated)
- [ ] make wifi-rescan watch for connections 5 seconds after rescan to see if results in connection notify-send if it happens
- [ ] set up research environment
- [x] script for youtube search using emacs+yeetube
- [ ] dmenu-screen-shot-plus utility fails sometimes. add multiple attempts. same with upload-selection
- [ ] remove most mpv keybindings as they are unused.
- [ ] display volume change using graphical scale.
