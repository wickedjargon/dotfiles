# dl

> Download video, audio, or files into the ~/d/ library.
> Media goes through yt-dlp; direct file URLs are fetched with curl and sorted like psync (documents → ~/d/notes, images → ~/d/images, archives → ~/d/other).
> Files land where psync syncs, so `psync push --latest` sends them to the phone.

- Download a video to ~/d/video:

`dl {{https://youtube.com/watch?v=xyz}}`

- Extract audio only, to ~/d/audio:

`dl audio {{https://youtube.com/watch?v=xyz}}`

- Download a document to ~/d/notes (quote URLs with parentheses or spaces):

`dl "{{https://example.com/report(1).pdf}}"`

- Several at once:

`dl audio {{URL1}} {{URL2}}`
