# audio

> Control volume and audio devices from the command line. Wraps wpctl and pactl.
> DEVICE and NAME match by partial name, case-insensitive.
> Switching a device also moves streams that are already playing.

- Show outputs, inputs, and what's playing:

`audio`

- Raise or lower the volume by 5%:

`audio up`

- Lower the volume by a specific amount:

`audio down {{10}}`

- Set the volume to an exact level (0-150):

`audio set {{60}}`

- Mute or unmute the output (both idempotent, never toggles):

`audio {{mute|unmute}}`

- Mute the microphone:

`audio mic mute`

- List available outputs:

`audio out`

- Switch sound to a different output:

`audio out {{hdmi}}`

- Switch the default microphone:

`audio in {{webcam}}`

- List apps playing audio:

`audio apps`

- Set one app's volume without touching the rest:

`audio app {{chrome}} {{50}}`

- Mute or unmute a single app:

`audio app {{spotify}} {{mute|unmute}}`
