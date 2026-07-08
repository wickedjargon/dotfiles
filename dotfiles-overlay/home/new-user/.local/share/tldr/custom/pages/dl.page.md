# dl

> Download video or audio into the ~/d/ library via yt-dlp.
> Files land where psync syncs, so `psync push --latest` sends them to the phone.

- Download a video to ~/d/video:

`dl {{https://youtube.com/watch?v=xyz}}`

- Extract audio only, to ~/d/audio:

`dl audio {{https://youtube.com/watch?v=xyz}}`

- Several at once:

`dl audio {{URL1}} {{URL2}}`
