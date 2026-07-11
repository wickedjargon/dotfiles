# stt

> Offline speech-to-text with Whisper (via sherpa-onnx).
> Mic sources copy the text to the clipboard; files print to stdout.

- Record from the microphone until Enter, transcribe, copy:

`stt`

- Start dictating from a hotkey or rofi (run again to stop; the text is copied and shown in a notification):

`stt toggle`

- Transcribe an audio file (stdout only, clipboard untouched):

`stt file {{memo.ogg}}`

- Transcribe a voice memo into a text file:

`stt file {{memo.ogg}} > {{memo.txt}}`

- Download the Whisper base.en model (~200 MB; reuses kokoro's runtime if installed):

`stt --install`

- Check whether stt is installed and a recorder is available:

`stt --ready`
