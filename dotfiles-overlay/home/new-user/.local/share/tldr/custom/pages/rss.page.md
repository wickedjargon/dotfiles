# rss

> Read and manage RSS/Atom feeds from the terminal.
> Stores subscriptions as OPML, caches entries locally.
> No external dependencies — pure Python stdlib.

- Show all feeds with unread counts:

`rss`

- Subscribe to a feed:

`rss add {{https://example.com/feed.xml}}`

- Subscribe with a custom name:

`rss add {{https://example.com/feed.xml}} {{My Blog}}`

- Refresh all feeds:

`rss fetch`

- Refresh a specific feed:

`rss fetch {{feed-name}}`

- Browse and read unread entries:

`rss read`

- Read entries from a specific feed:

`rss read {{feed-name}}`

- Unsubscribe from a feed:

`rss del {{feed-name}}`

- Mark all entries as read (keeps most recent unread):

`rss catchup`

- Mark a specific feed as read:

`rss catchup {{feed-name}}`

- Reset all read state (everything becomes unread):

`rss reset`

- Export subscriptions as OPML:

`rss export > {{backup.opml}}`

- Import feeds from an OPML file:

`rss import {{feeds.opml}}`
