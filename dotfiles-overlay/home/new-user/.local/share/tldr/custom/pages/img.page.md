# img

> Everyday image operations via imagemagick. Originals are never modified.
> Results go to a new file next to the source (photo.jpg → photo-800.jpg).

- Show dimensions, size, and whether EXIF/GPS data is present:

`img info {{photo.jpg}}`

- Dump all metadata tags:

`img exif {{photo.jpg}}`

- Resize to half size:

`img resize 50% {{photo.jpg}}`

- Resize to a maximum width:

`img resize 1920 {{photo.jpg}}`

- Rotate 90° clockwise (also 180, 270, -90):

`img rotate 90 {{photo.jpg}}`

- Bake the EXIF orientation into the pixels (upright everywhere):

`img rotate auto {{photo.jpg}}`

- Convert a phone HEIC photo to JPEG:

`img convert jpg {{IMG_0042.heic}}`

- Remove all metadata (lossless with exiftool installed):

`img strip {{photo.jpg}}`
