# theme

> Universal theme toggle for the desktop environment.
> Switches GTK, Qt, Kvantum, polybar, bspwm, dunst, dmenu, rofi, alacritty, zathura, calibre, sxiv, and xob between dark and light themes.

- Switch to dark theme:

`theme --dark`

- Switch to light theme:

`theme --light`

- Toggle between dark and light:

`theme --toggle`

- Switch theme without restarting services (used during bspwm startup):

`theme --dark --no-restart`

- Verify the saved theme is fully applied across all configs (detects silent drift):

`theme --verify`

- Check current theme state:

`cat ~/.config/theme-mode`
