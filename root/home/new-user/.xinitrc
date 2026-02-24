export PATH="$HOME/.local/bin:$PATH"

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
clipmenud &  # Start clipmenu daemon
unclutter -idle 1 &  # makes cursor disappear

# Start xob
if [ ! -p /tmp/xobpipe ]; then
    mkfifo /tmp/xobpipe
fi
tail -f /tmp/xobpipe | xob &

# Apply saved theme on startup
if [ -f "$HOME/.config/theme-mode" ]; then
    theme_mode=$(cat "$HOME/.config/theme-mode" | tr -d '[:space:]')
    if [ "$theme_mode" = "light" ]; then
        theme --light &
    else
        theme --dark &
    fi
fi

exec dbus-run-session bspwm
