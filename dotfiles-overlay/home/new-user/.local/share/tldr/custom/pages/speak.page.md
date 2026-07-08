# speak

> Speak text aloud with edge-tts (Microsoft's online TTS).
> With no arguments, speaks the highlighted text (X selection).
> Running it again while speaking stops playback (toggle).
> PDF-style hard line wrapping is undone by default (see unwrap setting).

- Speak the current selection:

`speak`

- Speak the given words:

`speak {{some words to say}}`

- Speak text read from stdin:

`echo {{hello}} | speak -`

- Speak with a different voice or speed for this run only:

`speak --voice {{en-GB-RyanNeural}} --rate {{+25%}} {{hello}}`

- Persist a setting for every run, including the super+s keybinding:

`speak --set {{voice=en-GB-RyanNeural}}`

- Show the effective settings and where each comes from:

`speak --show`

- List available voices, optionally filtered:

`speak --voices {{en-GB}}`

- Keep line breaks as-is for this run (unwrap is on by default):

`speak --no-unwrap`
