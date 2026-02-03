xset-settings


# qt theme
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_QPA_PLATFORMTHEME_QT6=qt6ct

# # set background
# feh --bg-scale ~/d/fakeimages/rms01.png

dunst & # notificatoins
battery-warnings & # low battery warning
clipmenud &  # Start clipmenu daemon
unclutter -idle 1 &  # makes cursor disappear
# uncomment when ready to install emacs packages
# emacs --daemon & # emacs server
exec bspwm
