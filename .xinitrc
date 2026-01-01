remap

# turn off the auto monitor off
xset -dpms
xset s off
xset s off -dpms
xset -b

# # set background
# feh --bg-scale ~/d/fakeimages/rms01.png

dunst & # notificatoins
battery-warnings & # low battery warning
clipmenud &  # Start clipmenu daemon
unclutter -idle 1 &  # makes cursor disappear
emacs --daemon & # emacs server
exec dwm
