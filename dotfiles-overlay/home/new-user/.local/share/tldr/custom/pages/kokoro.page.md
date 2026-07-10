# kokoro

> Offline neural TTS with the Kokoro-82M model (via sherpa-onnx).
> Natural-sounding local speech; `speak` uses it automatically when offline.
> Runtime and model live in `~/.local/share/kokoro` (shared with distrobox).

- Download the runtime (~28 MB) and model (~350 MB); works on host or in archbox:

`kokoro --install`

- Speak the given words:

`kokoro {{some words to say}}`

- Speak text read from stdin:

`echo {{hello}} | kokoro`

- Speak with a different voice or speed:

`kokoro --voice {{bf_emma}} --speed {{1.25}} {{hello}}`

- List the English voices (af/am American, bf/bm British):

`kokoro --voices`

- Check whether the runtime, model, and mpv are usable:

`kokoro --ready`

- Set a default voice or speed via the environment:

`KOKORO_VOICE={{am_michael}} KOKORO_SPEED={{1.1}} kokoro {{hello}}`
