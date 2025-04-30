;; Define play function
(defun play-url-with-mpv (url)
  "Play the given URL with mpv."
  (interactive "sEnter URL: ")
  (start-process "mpv-process" nil "mpv" url))

;; Define function to play current entry
(defun yt-dlp-play-current-entry ()
  "Play the URL from the current entry in the `tabulated-list-mode' buffer using mpv."
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      (let ((url (elt entry 1)))  ;; Assuming the URL is in the second column
        (play-url-with-mpv url)))))

;; Create a minor mode for keybindings
(defvar yt-dlp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Keymap is defined, but no global keybinding is set
    map)
  "Keymap for `yt-dlp-mode'.")

(define-minor-mode yt-dlp-mode
  "Minor mode for interacting with yt-dlp search results."
  :lighter " YTDLP"
  :keymap yt-dlp-mode-map)

;; Main function to search YouTube
(defun yt-dlp-search (query num-results)
  "Search YouTube for QUERY, displaying NUM-RESULTS titles and URLs in a table using `tabulated-list-mode`."
  (interactive "sEnter search query: \nnEnter number of results: ")
  (let ((buffer (get-buffer-create "*yt-dlp Search Results*"))
        (command (format "yt-dlp \"ytsearch%d:%s\" --flat-playlist --print '%%(title)s|||%%(url)s'" num-results query)))
    (with-current-buffer buffer
      (read-only-mode 0)
      (erase-buffer)
      (tabulated-list-mode)
      (setq tabulated-list-format [("Title" 100 t)
                                   ("URL" 30 t)])
      (setq tabulated-list-entries nil)
      (yt-dlp-mode 1))  ;; Enable the custom minor mode
    (make-process
     :name "yt-dlp-process"
     :buffer buffer
     :command (list "sh" "-c" command)
     :noquery t
     :sentinel (lambda (process event)
                 (when (eq (process-status process) 'exit)
                   (let ((buf (process-buffer process)))
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (goto-char (point-min))
                         (while (re-search-forward "\\(.*\\)|||\\(.*\\)" nil t)
                           (let ((title (match-string 1))
                                 (url (match-string 2)))
                             (push (list nil (vector title url)) tabulated-list-entries)))
                         (tabulated-list-init-header)
                         (tabulated-list-print t)
                         (goto-char (point-min))
                         (read-only-mode 1)
                         (display-buffer buf)))))))))


(defun play-audio-url-with-mpv (url)
  "Play only the audio from the given URL with mpv, using no video."
  (interactive "sEnter URL: ")
  (let ((socket (format "/tmp/mpv-socket-%d" (emacs-pid))))
    (start-process "mpv-process" nil "mpv" "--no-video" "--input-ipc-server" socket url)))

(defun mpv-pause-resume ()
  "Toggle pause/resume on the mpv process."
  (interactive)
  (let ((socket (format "/tmp/mpv-socket-%d" (emacs-pid))))
    (when (file-exists-p socket)
      (with-temp-buffer
        (call-process "echo" nil (current-buffer) nil "cycle pause" "| socat - " socket)))))

(provide 'yt-dlp-mode)
