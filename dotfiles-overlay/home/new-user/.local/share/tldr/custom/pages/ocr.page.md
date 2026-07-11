# ocr

> Extract text from the screen or images with tesseract.
> Screen and clipboard sources also copy the text to the clipboard.

- Select a screen region and OCR it (text lands in the clipboard):

`ocr`

- Same, recognizing another installed language:

`ocr {{deu}}`

- OCR the entire screen:

`ocr screen`

- OCR the image currently in the clipboard:

`ocr clip`

- OCR an image file (stdout only, clipboard untouched):

`ocr file {{scan.png}}`

- OCR a file into a text file:

`ocr file {{scan.png}} > {{scan.txt}}`

- List installed tesseract languages:

`ocr langs`
