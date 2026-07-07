# speak

> Speak text aloud with edge-tts (Microsoft's online TTS).
> With no arguments, speaks the highlighted text (X selection).
> Running it again while speaking stops playback (toggle).

- Speak the current selection:

`speak`

- Speak the given words:

`speak {{some words to say}}`

- Speak text read from stdin:

`echo {{hello}} | speak -`

- Undo PDF-style hard line wrapping before speaking:

`speak --unwrap`

- Speak with a different voice:

`SPEAK_VOICE={{en-GB-RyanNeural}} speak`
