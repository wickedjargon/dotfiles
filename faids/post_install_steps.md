
# git

## let git know who I am:

git config --global user.name "John Doe"
git config --global user.email johndoe@example.com

## add these submodules:

git submodule add https://github.com/tpope/vim-commentary.git .config/nvim/plugged/vim-commentary
git submodule add https://github.com/terryma/vim-expand-region.git .config/nvim/plugged/vim-expand-region
git submodule add https://github.com/tpope/vim-surround.git .config/nvim/plugged/vim-surround
git submodule add https://github.com/emacs-evil/evil.git .config/emacs/site-lisp/evil

## git add, commit, push
/faids/git_add.sh
git commit -m "--"
git push

# network and browsing

## copy hosts file to /etc/hosts
sudo su
cat hosts >> /etc/hosts

## setup firefox

### setup firefox hardening
- https://github.com/arkenfox/user.js/
- be sure to create a new profile (instructions below)
- instructions: https://github.com/pyllyukko/user.js/ 

### install these plugins:
- vimium
- ublock origin
- dark reader
- darker jupyter

### some settings
- set `g` as prefix for google search
- remove all other search suggestions (amazon, wikipedia)
- ask where to download files
- Applications > set default PDF reader to read pdfs
- remove bookmarks
- disable `picture-in-picture`
- set as default package for html files using `perl-file-mimeinfo`
- remove youtube, facebook, reddit from suggestions:
    - set `browser.urlbar.suggest.topsites` to `false` in `about:config`

# the rest

## use perl-file-mimeinfo to update default package for pdfs

## python
    - install pip using bootstrapping script (do not install using pacman)
    - install needed dependencies

## emacs
    - setup zterm
    - install elpy dependencies
    - install ein dependencies
