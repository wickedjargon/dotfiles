export PATH="$HOME/.local/bin:$HOME/.local/scripts:$PATH"

# Session identity — needed for xdg-desktop-portal / Qt theme detection
export XDG_CURRENT_DESKTOP=GNOME
export XDG_SESSION_TYPE=x11

# Start GNOME Keyring Daemon
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

xset-settings

# qt theme
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_QPA_PLATFORMTHEME_QT6=qt6ct

# Load Xresources
[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources

# # set background
# feh --bg-scale ~/d/fakeimages/rms01.png

mpd & # Music Player Daemon
dunst & # notificatoins
lxpolkit & # authentication agent
battery-warnings & # low battery warning
export CM_LAUNCHER=rofi  # Use rofi for clipmenu
clipmenud &  # Start clipmenu daemon
unclutter -idle 1 &  # makes cursor disappear

# Start xob
# Pipe lives in XDG_RUNTIME_DIR, not /tmp: GTK file choosers sniff files in
# browsed directories and block forever opening a FIFO with no writer.
XOB_PIPE="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/xobpipe"
if [ ! -p "$XOB_PIPE" ]; then
    mkfifo "$XOB_PIPE"
fi
tail -f "$XOB_PIPE" | xob &

exec bspwm
