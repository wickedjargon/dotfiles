# displays

> Manage external displays from the command line. Wraps xrandr with a simple interface.

- Show current display status:

`displays`

- Mirror internal display to external (auto-detected):

`displays mirror`

- Mirror to a specific output:

`displays mirror {{HDMI-2}}`

- Extend desktop to the right (default):

`displays extend`

- Extend to the left:

`displays extend --left`

- Use only a specific display:

`displays only {{HDMI-2}}`

- Turn off all externals, use laptop only:

`displays reset`

- List all outputs (connected and disconnected):

`displays list`
