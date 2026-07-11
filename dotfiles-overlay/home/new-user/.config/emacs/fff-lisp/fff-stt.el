;;; fff-stt.el --- Dictate into the current buffer with stt -*- lexical-binding: t; -*-

;; `fff-stt-toggle' starts recording from the microphone (a ●REC
;; indicator appears in the mode line); calling it again stops,
;; transcribes offline with the `stt' script, and inserts the text at
;; point. Emacs stays fully usable while recording — stt runs as an
;; async process and stops when we send it a newline.
;; `fff-stt-cancel' aborts a recording without transcribing.

(defvar fff-stt--process nil
  "The running stt process, or nil.")

(defvar fff-stt--marker nil
  "Where to insert the transcription (point at the time of stopping).")

(defvar fff-stt--output ""
  "Accumulated stdout of the stt process (the transcribed text).")

(defvar fff-stt--cancelled nil
  "Non-nil when the current recording was cancelled on purpose.")

(defvar fff-stt--recording nil
  "Non-nil while recording; drives the mode-line indicator.")

(unless global-mode-string (setq global-mode-string '("")))
(add-to-list 'global-mode-string '(fff-stt--recording " ●REC") t)

(defun fff-stt--last-error ()
  "Last meaningful stderr line of the stt process, without ANSI colors."
  (let ((buf (get-buffer " *fff-stt-stderr*")))
    (or (when (buffer-live-p buf)
          (with-current-buffer buf
            (let ((line (car (last (split-string
                                    (replace-regexp-in-string
                                     "\033\\[[0-9;]*m" "" (buffer-string))
                                    "\n" t)))))
              ;; drop the script's ✗/!/▸ status glyph
              (and line (string-trim-left line "[✗!▸✓] *")))))
        "unknown error")))

(defun fff-stt--sentinel (proc _event)
  (unless (process-live-p proc)
    (setq fff-stt--process nil
          fff-stt--recording nil)
    (force-mode-line-update t)
    (let ((text (string-trim fff-stt--output))
          (marker fff-stt--marker))
      (cond
       (fff-stt--cancelled
        (message "stt: recording cancelled"))
       ((or (/= (process-exit-status proc) 0) (string-empty-p text))
        (message "stt: %s" (fff-stt--last-error)))
       ((and marker (buffer-live-p (marker-buffer marker)))
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            ;; before-markers, so point (a marker while not in the
            ;; selected window) ends up after the text, like typing.
            (insert-before-markers text)))
        (message "stt: inserted %d characters" (length text)))
       (t
        (kill-new text)
        (message "stt: buffer is gone — text saved to the kill ring"))))))

;;;###autoload
(defun fff-stt-toggle ()
  "Start dictating; call again to stop and insert the text at point."
  (interactive)
  (if (process-live-p fff-stt--process)
      (progn
        (setq fff-stt--marker (point-marker))
        (process-send-string fff-stt--process "\n")
        (message "stt: transcribing..."))
    (unless (executable-find "stt")
      (user-error "stt not found in PATH"))
    (let ((stderr (get-buffer-create " *fff-stt-stderr*")))
      (with-current-buffer stderr (erase-buffer))
      (setq fff-stt--output ""
            fff-stt--cancelled nil
            fff-stt--recording t
            fff-stt--process
            (make-process
             :name "fff-stt"
             :command '("stt")
             :connection-type 'pipe
             :stderr stderr
             :filter (lambda (_proc chunk)
                       (setq fff-stt--output
                             (concat fff-stt--output chunk)))
             :sentinel #'fff-stt--sentinel)))
    (force-mode-line-update t)
    (message "stt: recording... `fff-stt-toggle' again to stop")))

(defun fff-stt-cancel ()
  "Abort the current recording without transcribing."
  (interactive)
  (if (not (process-live-p fff-stt--process))
      (message "stt: not recording")
    (setq fff-stt--cancelled t)
    (interrupt-process fff-stt--process)))

(provide 'fff-stt)
;;; fff-stt.el ends here
