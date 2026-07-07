# brightness

> Show or set the display backlight brightness.
> Values are clamped to [5, 100] so the panel never goes fully black.
> Runs on the host when invoked inside a distrobox container.

- Show the current brightness percentage:

`brightness`

- Set the brightness to 50%:

`brightness 50`

- Increase the brightness by 10 percentage points:

`brightness +10`

- Decrease the brightness by 10 percentage points:

`brightness -10`
