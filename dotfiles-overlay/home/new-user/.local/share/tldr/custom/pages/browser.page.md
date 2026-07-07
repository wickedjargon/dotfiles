# browser

> Set the default web browser (Firefox or Google Chrome).
> Updates mimeapps.list (xdg default) and puts the default browser first in the rofi-web (super+w) menu.

- Set Firefox as the default browser:

`browser --firefox`

- Set Google Chrome as the default browser:

`browser --chrome`

- Toggle between Firefox and Google Chrome:

`browser --toggle`

- Check the current default browser:

`grep 'x-scheme-handler/http=' ~/.config/mimeapps.list`
