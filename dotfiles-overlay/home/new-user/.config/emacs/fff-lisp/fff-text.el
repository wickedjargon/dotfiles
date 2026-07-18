;;; fff-text.el --- Text editing, filtering and clipboard commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal commands, split out of the old fff-functions.el.

;;; Code:

;;;; Text Editing

(defun fff-remove-blank-lines ()
  (interactive)
  (flush-lines "^\s*$" (point-min) (point-max)))

(defun fff-remove-extra-blank-lines ()
  "Remove extra blank lines, keeping a single blank line between blocks of code."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n\\{3,\\}" nil t)
      (replace-match "\n\n"))

    ;; Remove any leading blank lines at the start of the buffer
    (goto-char (point-min))
    (when (looking-at-p "\n+")
      (replace-match ""))

    ;; Remove any trailing blank lines at the end of the buffer
    (goto-char (point-max))
    (skip-chars-backward "\n")
    (delete-region (point) (point-max))))

(defun fff-remove-newlines ()
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "" nil t))))

(defun fff-dos-to-unix ()
  "Translate contents of buffer from dos format to unix format Replace \r\n with \n."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while
        (search-forward "\r\n" () t)
      (replace-match "\n"))
    ()))

(defun fff-insert-tab()
  (interactive)
  (insert "	"))

(defun fff-insert-4-spaces()
  (interactive)
  (insert "    "))

(defun fff-clear-line ()
  "Deletes the current line"
  (interactive)
  (progn
    (delete-region
     (line-beginning-position)
     (line-end-position))
    (evil-delete-backward-char-and-join)))

(defun fff-title-case-region-or-line (@begin @end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           $p1
           $p2
           ($skipChars ""))
       (progn
         (skip-chars-backward $skipChars (line-beginning-position))
         (setq $p1 (point))
         (skip-chars-forward $skipChars (line-end-position))
         (setq $p2 (point)))
       (list $p1 $p2))))
  (let* (
         ($strPairs []))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while
                 (search-forward (aref $x 0) nil t)
               (replace-match (aref $x 1) "FIXEDCASE" "LITERAL")))
           $strPairs))))))

(defun fff-mark-gt-point-exchange ()
  (if (>  (mark) (point))
      (exchange-point-and-mark)))

(defun fff-wrap-with-function-call (function-name)
  (interactive "sFunction name:")
  (if (and transient-mark-mode mark-active)
      (progn
        (setq text-end ")")
        (setq function-name (concat function-name "("))
        (fff-mark-gt-point-exchange)
        (goto-char (region-end))
        (insert text-end)
        (goto-char (region-beginning))
        (insert function-name))
    (progn
      (setq text-end ")")
      (setq function-name (concat function-name "("))
      (setq bds (bounds-of-thing-at-point 'symbol))
      (goto-char (cdr bds))
      (insert text-end)
      (goto-char (car bds))
      (insert function-name))))

(defun fff-regions-content ()
  "Return the selected region as a string."
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))))

(defun fff-replace-spaces-with-dashes (beg end)
  "Replace spaces with dashes in the region from BEG to END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (while (search-forward " " nil t)
        (replace-match "-")))))

(defun fff-replace-spaces-with-underscores (beg end)
  "Replace spaces with dashes in the region from BEG to END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (while (search-forward " " nil t)
        (replace-match "_")))))

(defun fff-upper-camel-case-region ()
  "Capitalize the first letter of each word in the region and remove the spaces between words."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\b\\w" end t)
        (replace-match (upcase (match-string 0)) t))
      (goto-char start)
      (while (re-search-forward " +" end t)
        (replace-match "")))))

(defun fff-insert-lambda-symbol ()
  "Insert λ."
  (interactive)
  (insert "λ"))

(defun fff-delete-to-beginning-of-line ()
  "Delete from point to the beginning of the line."
  (interactive)
  (delete-region (point-at-bol) (point)))

(defun fff-indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun fff-delete-trailing-whitespace-in-dir (dir)
  "Delete trailing whitespace in all files in the specified directory."
  (interactive "DDirectory: ")
  (let ((files (directory-files-recursively dir ".*\\.\\(el\\|c\\|h\\)$"))) ;; Adjusted to exclude .txt
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (delete-trailing-whitespace)
        (write-region (point-min) (point-max) file)))))

(defun fff-indent-region ()
  "Call `indent-region` interactively."
  (interactive)
  (call-interactively 'indent-region))

(defun fff-insert-current-date ()
  "Insert the current date in the format YYYY-MM-DD."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun fff-insert-date ()
  "Insert the current date in YYYY-MM-DD format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun fff-insert-time ()
  "Insert the current time in 12-hour format with AM/PM."
  (interactive)
  (insert (format-time-string "%I:%M:%S %p")))

(defun fff-remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun fff-delete-till-beginning-of-line ()
  "Delete from the current point to the beginning of the line."
  (interactive)
  (let ((start (point)))
    (beginning-of-line)
    (delete-region (point) start)))

(defun fff-capitalize-sentences (beg end)
  "Capitalize the first letter of each sentence in the selected region.
   Works by moving sentence-by-sentence and upcasing the first word character found."
  (interactive "r")
  (save-excursion
    (save-restriction
      ;; Narrow to the region so we don't accidentally modify outside it
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        ;; Skip non-word chars (like spaces, tabs, quotes, parens)
        (skip-syntax-forward "^w")
        ;; If we aren't at the end of the buffer, upcase the character
        (unless (eobp)
          (upcase-region (point) (1+ (point))))
        ;; Jump to the end of the current sentence to prepare for the next loop
        (forward-sentence)))))


(defun fff-down-list-back ()
  (interactive)
  (down-list -1)
  )

;;;; Comments

(defun fff-comment-delete (arg)
  "Delete the first comment on this line, if any.  Don't touch
the kill ring.  With prefix ARG, delete comments on that many
lines starting with this one."
  (interactive "P")
  (comment-normalize-vars)
  (dotimes (_i (prefix-numeric-value arg))
    (save-excursion
      (beginning-of-line)
      (let ((cs (comment-search-forward (line-end-position) t)))
        (when cs
          (goto-char cs)
          (skip-syntax-backward " ")
          (setq cs (point))
          (comment-forward)
          (delete-region cs (if (bolp) (1- (point)) (point)))
          (indent-according-to-mode))))
    (if arg (forward-line 1))))

(defun fff-comment-delete-dwim (beg end arg)
  "Delete comments without touching the kill ring.  With active
region, delete comments in region.  With prefix, delete comments
in whole buffer.  With neither, delete comments on current line."
  (interactive "r\nP")
  (let ((lines (cond (arg
                      (count-lines (point-min) (point-max)))
                     ((region-active-p)
                      (count-lines beg end)))))
    (save-excursion
      (when lines
        (goto-char (if arg (point-min) beg)))
      (fff-comment-delete (or lines 1)))))

(defun fff-comment-delete-all ()
  (interactive)
  (fff-comment-delete-dwim (point-min) (point-max) +1))

(defun fff-comment ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))


;;;; Search and Filter

(defun fff-filter-lines-with-regex (regex)
  "Filter lines in the current buffer to show only those matching REGEX,
but only if the buffer is read-only."
  (interactive "sEnter regex to filter lines: ")
  (if buffer-read-only
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (delete-non-matching-lines regex))
    (message "This command only works in read-only buffers.")))

(defun fff-filter-out-lines-with-regex-writable (regex)
  "Remove lines in the current buffer that match REGEX,
but only if the buffer is read-only."
  (interactive "sEnter regex to filter out lines: ")
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (beginning-of-line)
      (let ((beg (point)))
        (forward-line 1)
        (delete-region beg (point))))))

(defun fff-filter-out-lines-with-regex (regex)
  "Remove lines in the current buffer that match REGEX,
but only if the buffer is read-only."
  (interactive "sEnter regex to filter out lines: ")
  (if buffer-read-only
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (re-search-forward regex nil t)
          (beginning-of-line)
          (let ((beg (point)))
            (forward-line 1)
            (delete-region beg (point)))))
    (message "This command only works in read-only buffers.")))

(defun fff-filter-lines-with-regex-writable (regex)
  "Filter lines in the current buffer to show only those matching REGEX,
but only if the buffer is read-only."
  (interactive "sEnter regex to filter lines: ")
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (delete-non-matching-lines regex))
  (message "This command only works in read-only buffers."))

(defun fff-filter-lines-with-regex-undo (regex)
  "just revert the buffer"
  (interactive)
  (revert-buffer))

(defun fff-hide-lines-in-region (start end)
  "Hide lines in the region from START to END."
  (interactive "r")
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'intangible t)
    (overlay-put overlay 'isearch-open-invisible
                 'delete-overlay)))

(defun fff-filter-out-lines-without-keybinding ()
  "Remove lines in the current buffer that do not contain a keybinding in brackets."
  (interactive)
  (if buffer-read-only
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (not (eobp))
          (if (not (re-search-forward " \\([^(]*\\)([^)]+)" (line-end-position) t))
              (delete-region (line-beginning-position) (line-end-position)))
          (forward-line 1)))
    (message "This command only works in read-only buffers.")))


;;;; Clipboard

(defun fff-send-to-clipboard (x)
  (with-temp-buffer
    (insert x)
    (clipboard-kill-region (point-min) (point-max))))


(provide 'fff-text)
;;; fff-text.el ends here
