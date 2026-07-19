;;; fff-shell.el --- Shell, compile, diff and external-program commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal commands, split out of the old fff-functions.el.

;;; Code:

;;;; Shell and External Programs

(defun fff-open-in-firefox ()
  "open in firefox"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt") ; Windows
    (shell-command (concat "firefox file://" buffer-file-name)))
   ((string-equal system-type "gnu/linux")
    (shell-command (concat "firefox file://" buffer-file-name)))
   ((string-equal system-type "darwin") ; Mac
    (shell-command (concat "open -a Firefox.app file://" buffer-file-name)))))

(defun fff-open-in-chromium ()
  "open in firefox"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt") ; Windows
    (shell-command (concat "chromium --new-window file://" buffer-file-name)))
   ((string-equal system-type "gnu/linux")
    (shell-command (concat "chromium --new-window file://" buffer-file-name)))
   ((string-equal system-type "darwin") ; Mac
    (shell-command (concat "open -a Firefox.app file://" buffer-file-name)))))

(defun fff-clear-shell ()
  (interactive)
  (require 'comint)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun fff-run-browser-sync ()
  "Run 'browser-sync --watch' as an asynchronous process."
  (interactive)
  (start-process-shell-command
   "browser-sync"        ; name of the process
   "*browser-sync-output*" ; buffer to capture output
   "browser-sync --watch"))

(defun fff-reexecute-last-shell-command ()
  "Re-execute the last shell command executed with `shell-command`."
  (interactive)
  (let ((last-command (nth 0 shell-command-history)))
    (if last-command
        (shell-command last-command)
      (message "No previous shell command found."))))

(defun fff-open-in-browser()
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))

(defun fff-play-url-at-point-with-mpv ()
  "Play the URL at point with mpv if it's a valid URL."
  (interactive)
  (require 'thingatpt)
  (let ((url (thing-at-point 'url t)))
    (if url
        (progn
          (message "Playing URL: %s" url)
          (shell-command (format "mpv '%s' &" url)))
      (message "No valid URL at point."))))

(defun fff-open-file-in-mpv ()
  "Open the file at point in dired using mpv."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (start-process "mpv-process" nil "mpv" file)))

(defun fff-dired-open-with-command ()
  "Prompt for a command and run it with the current file in Dired."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (command (read-string "Enter command: "))
         (full-command (concat command " " (shell-quote-argument file))))
    (start-process-shell-command "dired-open-with-command" nil full-command)))

(defun fff-dired-open-in-brave ()
  "Open the file at point in a new Brave window."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (start-process "brave-open" nil "brave-browser" "--new-window" (expand-file-name file))))

(defvar fff-vm-scp-port 2222
  "Host port `vm start' forwards to the guest's port 22.")

(defvar fff-vm-scp-user "farzi"
  "User account inside the VM.")

(defvar fff-vm-scp-dir "OneDrive/Desktop"
  "Destination directory relative to the guest home directory.
Windows redirects the desktop into OneDrive when it's enabled;
without OneDrive it's plain \"Desktop\".")

(defun fff-dired-send-to-vm-desktop ()
  "Copy the marked files or directories (or the one at point) to the
VM's Desktop over scp. Directories copy recursively. Needs OpenSSH
Server and key-based auth in the guest (scp runs without a tty, so it
can't prompt for a password)."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (proc (apply #'start-process "vm-scp" "*vm-scp*"
                      "scp" "-r" "-P" (number-to-string fff-vm-scp-port)
                      "-o" "StrictHostKeyChecking=accept-new"
                      "-o" "BatchMode=yes"
                      "-o" "ConnectTimeout=5"
                      (append files
                              (list (format "%s@localhost:%s/"
                                            fff-vm-scp-user
                                            fff-vm-scp-dir))))))
    (set-process-sentinel
     proc
     (lambda (_ event)
       (if (string-prefix-p "finished" event)
           (message "Sent %d file(s) to the VM desktop" (length files))
         (message "scp failed — see *vm-scp* (is sshd up in the guest?)"))))))

(defun fff-yt-play (url)
  "Play URL with `yeetube-mpv-play'."
  (interactive "sURL: ")
  (yeetube-mpv-play url))

(defun fff-publish-region-online ()
  "Publish the current region to 0x0.st and return the URL.
The URL is copied to the kill ring and displayed in the minibuffer."
  (interactive)
  ;; Load url now: the url-request-* lets below must be dynamic.
  (require 'url)
  (if (not (use-region-p))
      (message "No region selected!")
    (let* ((url "https://0x0.st") ;; URL is now defined locally here
           (text (buffer-substring-no-properties (region-beginning) (region-end)))
           (boundary (format "----EmacsFormBoundary%s" (format-time-string "%s")))
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . ,(format "multipart/form-data; boundary=%s" boundary))))
           (url-request-data
            (encode-coding-string
             (concat
              "--" boundary "\r\n"
              "Content-Disposition: form-data; name=\"file\"; filename=\"snippet.txt\"\r\n"
              "Content-Type: text/plain\r\n"
              "\r\n"
              text "\r\n"
              "--" boundary "--\r\n")
             'utf-8)))
      (url-retrieve
       url
       (lambda (status)
         (if (plist-get status :error)
             (message "Error uploading to 0x0.st: %s" (plist-get status :error))
           (goto-char (point-min))
           (re-search-forward "^$")
           (let ((result-url (string-trim (buffer-substring (point) (point-max)))))
             (kill-new result-url)
             (message "Uploaded to 0x0.st: %s (URL copied to clipboard)" result-url))))
       nil t t))))


;;;; Eshell

(defun fff-open-new-eshell ()
  "Open a new eshell buffer."
  (interactive)
  (eshell (generate-new-buffer-name "*eshell*")))

(defun fff-switch-or-create-eshell ()
  (interactive)
  (let ((eshell-buffers (delq nil (mapcar (lambda (b)
                                            (when (string-match "\\*eshell\\*<\\([0-9]+\\)>" (buffer-name b))
                                              (cons (string-to-number (match-string 1 (buffer-name b))) b)))
                                          (buffer-list))))
        (newest-buffer nil))
    (if eshell-buffers
        (progn
          (setq newest-buffer (cdr (cl-reduce (lambda (a b) (if (> (car a) (car b)) a b)) eshell-buffers)))
          (switch-to-buffer newest-buffer))
      (eshell))))

(defun fff-eshell-clear-1 ()
  "Clear eshell buffer and send the clear 1 command."
  (interactive)
  (eshell-send-input)
  (insert "clear 1")
  (eshell-send-input))


;;;; Programming and Build

(defun fff-build-pdf ()
  (interactive)
  (setq dir-path (file-name-directory (buffer-file-name)))
  (setq file-no-ext (substring (buffer-name) 0 -2))
  ;; groff -ms -T pdf -tle  resume.ms > resume.pdf & zathura resume.pdf
  (shell-command (format "cd %s && groff -ms -T pdf -tle  %s > %s.pdf & zathura %s.pdf"
                         dir-path (buffer-name) file-no-ext  file-no-ext)))

(defun fff-c-copy-terminal-command ()
  (interactive)
  (setq dir-path (file-name-directory (buffer-file-name)))
  (setq file-no-ext (substring (buffer-name) 0 -2))
  (fff-send-to-clipboard (format "cd %s && cc -w -o %s %s -lm && ./%s" dir-path file-no-ext (buffer-name) file-no-ext))
  )

(defun fff-flymake-list ()
  (interactive)
  (flymake-show-buffer-diagnostics))

(defun fff-create-go-project ()
  "Create a new Go project."
  (interactive)
  ;; request project name
  (let ((project-name (read-string "Enter project name: ")))
    (make-directory project-name)

    ;; run "go mod init"
    (shell-command (format "cd %s && go mod init fff/%s" project-name project-name))

    ;; Create a main.go file
    (find-file (format "%s/main.go" project-name))
    (insert (format "package main\n\nimport (\n\t\"fmt\"\n)\n\nfunc main() {\n\tfmt.Println(\"Hello, %s\")\n}\n" project-name))
    (save-buffer)

    (message "Go project '%s' created successfully." project-name)))

(defun fff-v-build-ctags ()
  "Run ctags command from the project root directory and load TAGS file for V programming language."
  (interactive)
  (let ((project-root (locate-dominating-file default-directory "v.mod")))
    (if project-root
        (let ((default-directory project-root))
          (shell-command
           "ctags --langmap=v:.v --regex-v='/[ \t]*fn[ \t]+(.*)[ \t]+(.*)/\\2/f,function/' --regex-v='/[ \t]*struct[ \t]+([a-zA-Z0-9_]+)/\\1/s,struct/' --regex-v='/[ \t]*interface[ \t]+([a-zA-Z0-9_]+)/\\1/i,interface/' --regex-v='/[ \t]*type[ \t]+([a-zA-Z0-9_]+)/\\1/t,type/' --regex-v='/[ \t]*enum[ \t]+([a-zA-Z0-9_]+)/\\1/e,enum/' --regex-v='/[ \t]*module[ \t]+([a-zA-Z0-9_]+)/\\1/m,module/' -e -R . ~/.local/bin/v/vlib/")
          (message "ctags command executed.")
          (call-interactively 'v-load-tags))
      (message "Not in a project root directory. No v.mod file found."))))

(defun run-v ()
  "Run an interactive REPL session for the V programming language."
  (interactive)
  (let ((buffer (get-buffer-create "*V-REPL*")))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
        (comint-mode)
        (make-comint-in-buffer "V-REPL" buffer "v" nil "repl")))
    (pop-to-buffer buffer)))

(defun fff-write-dir-locals-here-with-compile-command ()
  "Prompt for a compile command and write it to .dir-locals.el in the current directory."
  (interactive)
  (let* ((current-dir (file-name-directory (or (buffer-file-name) default-directory)))
         (compile-cmd (read-shell-command "Set compile-command: "))
         (dir-locals-file (expand-file-name ".dir-locals.el" current-dir))
         (locals-form `((nil . ((compile-command . ,compile-cmd))))))
    (with-temp-file dir-locals-file
      (insert ";;; Directory Local Variables\n")
      (insert ";;; For more information see (info \"(emacs) Directory Variables\")\n\n")
      (insert (format "%S\n" locals-form)))
    (message "Wrote compile-command to %s" dir-locals-file)))

(defun fff-elisp-eval-and-print-last-sexp ()
  "Evaluate and print expression before point like `eval-print-last-sexp'.
Prepend a comment to the return value.  Also copy the return value to
the `kill-ring' and set the mark to where point was before inserting the
return value."
  (declare (interactive-only t))
  (interactive)
  (if-let* ((string (thing-at-point 'sexp :no-properties))
            (_ (not (string-prefix-p ";" string)))
            (expression (read string)))
      (let ((return-value (eval expression)))
        (kill-new (format "%S" return-value))
        (message "Copied: `%S'" return-value)
        (push-mark (point))
        (insert (format "\n%S\n" return-value))
        (string-insert-rectangle (+ (mark) 1) (- (point) 1) ";; => "))
    (user-error "No expression at point")))


;;;; Diff

(defun fff-diff-files ()
  "Compare two files using the `diff' command."
  (interactive)
  (let ((file1 (read-file-name "Diff new file: "))
        (file2 (read-file-name "Diff old file: ")))
    (unless (and (file-readable-p file2) (file-readable-p file1))
      (error "One or both of the selected files are not readable"))
    (diff file2 file1)))

(defun fff-diff-marked-files ()
  "Compare two marked files in a Dired buffer using the `diff' command."
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (if (not (= 2 (length marked-files)))
        (error "Please mark exactly two files for comparison")
      (let ((file1 (car marked-files))
            (file2 (cadr marked-files)))
        (unless (and (file-readable-p file1) (file-readable-p file2))
          (error "One or both of the marked files are not readable"))
        (diff file1 file2)))))

(defun fff-diff-kill-ring ()
  "Diff the last two items in the kill ring.
Passes the most recent kill as the first argument (A) and the 
previous kill as the second argument (B)."
  (interactive)
  (if (< (length kill-ring) 2)
      (message "Kill ring does not have enough items to diff.")
    (let* ((new (current-kill 0 t))
           (old (current-kill 1 t))
           (file-new (make-temp-file "kill-new-"))
           (file-old (make-temp-file "kill-old-")))
      (with-temp-file file-new (insert new))
      (with-temp-file file-old (insert old))
      ;; Runs diff NEW OLD
      (diff file-new file-old "-u")
      (message "Diffing kill ring items..."))))


(provide 'fff-shell)
;;; fff-shell.el ends here
