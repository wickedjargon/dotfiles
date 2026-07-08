# pdf

> Everyday PDF operations via poppler and ghostscript. Originals are never modified.

- Show pages, size, and metadata:

`pdf info {{paper.pdf}}`

- Extract plain text (pipes nicely into speak):

`pdf text {{paper.pdf}} | speak -`

- Merge PDFs (the last argument is the new output file):

`pdf merge {{a.pdf}} {{b.pdf}} {{combined.pdf}}`

- Extract a page range:

`pdf split {{book.pdf}} {{5-20}}`

- Compress for email (presets: screen, ebook, printer):

`pdf compress {{scan.pdf}}`

- Compress as small as possible:

`pdf compress {{scan.pdf}} screen`
