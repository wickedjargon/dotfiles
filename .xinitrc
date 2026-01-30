remap

# turn off the auto monitor off
xset -dpms
xset s off
xset s off -dpms
xset -b
xsetroot -cursor_name left_ptr

# # set background
# feh --bg-scale ~/d/fakeimages/rms01.png

dunst & # notificatoins
battery-warnings & # low battery warning
clipmenud &  # Start clipmenu daemon
unclutter -idle 1 &  # makes cursor disappear
# uncomment when ready to install emacs packages
# emacs --daemon & # emacs server
exec dwm
