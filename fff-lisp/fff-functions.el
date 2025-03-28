(defun fff-buffer-list-switch ()
  "Switch to buffer list and activate the window"
  (interactive)
  (list-buffers)
  (select-window (get-buffer-window "*Buffer List*" 0)))

(defun fff-open-file-in-tmp ()
  "Prompt for a filename and open it in the /tmp/ directory."
  (interactive)
  (let ((default-directory "/tmp/"))
    (call-interactively 'find-file)))

(defun fff-open-file-in-projects ()
  "Prompt for a filename and open it in the /projects"
  (interactive)
  (let ((default-directory "~/d/projects/"))
    (call-interactively 'find-file)))

(defun fff-access-config-dir ()
  "Prompt for a filename and open it in the /projects"
  (interactive)
  (let ((default-directory (expand-file-name "./" user-emacs-directory)))
    (call-interactively 'find-file)))

(defun fff-open-file-in-snippets ()
  "Prompt for a filename and open it in the snippets directory."
  (interactive)
  (let ((default-directory (expand-file-name "snippets/" user-emacs-directory)))
    (call-interactively 'find-file)))

(defun fff-open-file-in-root-dir ()
  "Prompt for a filename and open it in the root directory."
  (interactive)
  (let ((default-directory "/"))
    (call-interactively 'find-file)))

(defun fff-access-home-dir ()
  "Prompt for a filename and open it in the snippets directory."
  (interactive)
  (let ((default-directory "~/"))
    (call-interactively 'find-file)))

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
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

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

(defun fff-build-pdf ()
  (interactive)
  (setq dir-path (file-name-directory (buffer-file-name)))
  (setq file-no-ext (substring (buffer-name) 0 -2))
  ;; groff -ms -T pdf -tle  resume.ms > resume.pdf & zathura resume.pdf
  (shell-command (format "cd %s && groff -ms -T pdf -tle  %s > %s.pdf & zathura %s.pdf"
                         dir-path (buffer-name) file-no-ext  file-no-ext)))

(defun fff-access-config ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun fff-access-pass()
  (interactive)
  (find-file "~/d/p"))

(defun fff-access-sched ()
  (interactive)
  (find-file "~/d/notes/personal-notes.md"))

(defun fff-access-functions-file ()
  (interactive)
  (find-file (expand-file-name "fff-functions.el" user-emacs-directory)))

(defun fff-access-bookmarks ()
  (interactive)
  (find-file "~/d/notes/bm.md"))

(defun fff-access-books ()
  (interactive)
  (find-file "~/d/books/"))

(defun fff-access-books ()
  (interactive)
  (find-file "~/d/books/"))

(defun fff-access-hosts ()
  (interactive)
  (find-file "/sudo:root@localhost:/etc/hosts"))

(defun fff-access-phonebook ()
  (interactive)
  (find-file "~/d/notes/contacts.csv"))

(defun fff-kill-this-buffer ()
  "Kill the current buffer without asking."
  (interactive)
  (kill-buffer (current-buffer)))

(defun fff-save-and-bury-buffer ()
  "save and kill buffer"
  (interactive)
  (save-buffer)
  (bury-buffer))

(defun fff-revert-and-bury-buffer ()
  "save and kill buffer"
  (interactive)
  (revert-buffer)
  (bury-buffer))

(defun fff-copy-file-path ()
  "Put the current file path on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun fff-force-kill-this-buffer ()
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer)))

(defun fff-evil-regex-search ()
  (interactive)
  (progn
    (evil-normal-state)
    (evil-ex "%s/")))

(defun fff-insert-tab()
  (interactive)
  (insert "	"))

(defun fff-insert-4-spaces()
  (interactive)
  (insert "    "))

(defun fff-send-to-clipboard (x)
  (with-temp-buffer
    (insert x)
    (clipboard-kill-region (point-min) (point-max))))

(defun fff-c-copy-terminal-command ()
  (interactive)
  (setq dir-path (file-name-directory (buffer-file-name)))
  (setq file-no-ext (substring (buffer-name) 0 -2))
  (fff-send-to-clipboard (format "cd %s && cc -w -o %s %s -lm && ./%s" dir-path file-no-ext (buffer-name) file-no-ext))
  )

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

(defun fff-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun fff-clear-line ()
  "Deletes the current line"
  (interactive)
  (progn
    (delete-region
     (line-beginning-position)
     (line-end-position))
    (evil-delete-backward-char-and-join)))

(defun fff-delete-till-beginning-of-line ()
  "Delete from the current point to the beginning of the line."
  (interactive)
  (let ((start (point)))
    (beginning-of-line)
    (delete-region (point) start)))

(defun fff-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun fff-set-scale-to-zero ()
  (interactive)
  (text-scale-set 0))

;; (defun fff-run-blue ()
;;   (interactive)
;;   (shell-command (format "blue --line-length 300 %s" (buffer-file-name))))

(defun fff-flymake-list ()
  (interactive)
  (flymake-show-buffer-diagnostics))

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

(defun fff-toggle-centered-window-mode ()
  (interactive)
  (if (not centered-window-mode)
      (progn
        (centered-window-mode +1)
        (global-display-line-numbers-mode -1)
        (message "centered window mode on"))
    (progn
      (centered-window-mode -1)
      (global-display-line-numbers-mode +1)
      (message "centered window mode off"))))

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

(defun fff-print-debug-line-python ()
  (interactive)
  (save-excursion
    (setq expression (thing-at-point 'symbol))
    (if (and transient-mark-mode mark-active)
        (setq expression (fff-regions-content)))
    (evil-open-below 1)
    (insert expression)
    (setq text-beg (concat "print(\"" expression " -->\", "))
    (setq text-end ")  # ff-debug")
    (evil-first-non-blank)
    (insert text-beg)
    (end-of-line)
    (insert text-end)
    (evil-normal-state)))

(defun fff-regions-content ()
  "Return the selected region as a string."
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))))

(defun fff-print-debug-line-rust ()
  (interactive)
  (save-excursion
    (setq expression (thing-at-point 'symbol))
    (if (and transient-mark-mode mark-active)
        (setq expression (fff-regions-content)))
    (evil-open-below 1)
    (insert (concat "dbg!(" expression ");  // ff-debug"))
    (evil-normal-state)))

(defun fff-delete-debug-lines-python ()
  (interactive)
  (replace-regexp-in-region ".*  # ff-debug$" "" (point-min) (point-max)))

(defun fff-delete-debug-lines-rust ()
  (interactive)
  (replace-regexp-in-region ".*  // ff-debug$" "" (point-min) (point-max)))

(defun fff-python-format ()
  (interactive)
  (fff-remove-blank-lines)
  (elpy-yapf-fix-code)
  (fff-run-blue))

(defun fff-evil-paste-and-indent-after ()
  (interactive)
  (evil-with-undo
    (progn
      (evil-paste-after 1)
      (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\])))))

(defun fff-evil-paste-and-indent-before ()
  (interactive)
  (evil-with-undo
    (progn
      (evil-paste-before 1)
      (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\])))))

(defun fff-display-python ()
  (interactive)
  (run-python (python-shell-calculate-command) nil nil)
  (display-buffer-pop-up-window  (get-buffer-create "*Python*") nil))

(defun fff-go-to-git-root-dir ()
  (interactive)
  (dired (locate-dominating-file default-directory ".git")))

(defun fff-go-to-git-root-dir-interactive ()
 (interactive)
 (let ((default-directory (locate-dominating-file default-directory ".git")))
   (if default-directory
       (call-interactively 'find-file)
     (message "Not in a git repository"))))

(defun fff-kill-entire-buffer-list ()
  (interactive)
  (dolist (cur (buffer-list))
    (kill-buffer cur)))

(defun fff-switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

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

(defun fff-copy-buffer-name ()
  "Copy the name of the current buffer to the kill ring."
  (interactive)
  (kill-new (buffer-name))
  (message "Buffer name '%s' copied to the kill ring." (buffer-name)))

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

(defun fff-paste-history ()
  (interactive)
  (counsel-yank-pop))

(defun fff-save-then-quickrun ()
  (interactive)
  (progn
    (save-buffer)
    (quickrun)))

(defun fff-display-lsp-root ()
  (interactive)
  (message "LSP root: %s"
           (lsp-find-session-folder (lsp-session)
                                    (buffer-file-name))))

(defun fff-insert-lambda-symbol ()
  "Insert λ."
  (interactive)
  (insert "λ"))

(evil-define-command fff-evil-open-below (count)
  :suppress-operator t
  (interactive "p")
  (if (eq major-mode #'haskell-mode)
      (progn
        (unless (eq evil-want-fine-undo t)
          (evil-start-undo-step))
        (push (point) buffer-undo-list)
        (end-of-line)
        (haskell-indentation-newline-and-indent)
        (evil-insert-state 1)
        )
    (evil-open-below 1)))

(evil-define-command fff-evil-open-above (count)
  (interactive "p")
  (if (eq major-mode #'haskell-mode)
      (progn
        (unless (eq evil-want-fine-undo t)
          (evil-start-undo-step))
        (beginning-of-line)
        (haskell-indentation-newline-and-indent)
        (next-line -1)
        (evil-insert-state 1))
    (evil-open-above 1)))

(defun fff-toggle-theme ()
  "Toggle between the 'plain' and 'dark' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'plain)
      (progn
        (disable-theme 'plain)
        (load-theme 'dark t))
    (progn
      (disable-theme 'dark)
      (load-theme 'plain t))))

(defun fff-down-list-back ()
  (interactive)
  (down-list -1)
  )

;; contrib to crux?
(defun fff-diff-files ()
  "Compare two files using the `diff' command."
  (interactive)
  (let ((file1 (read-file-name "Diff new file: "))
        (file2 (read-file-name "Diff old file: ")))
    (unless (and (file-readable-p file2) (file-readable-p file1))
      (error "One or both of the selected files are not readable"))
    (diff file2 file1)))

;; contrib to crux?
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

(defun fff-delete-to-beginning-of-line ()
  "Delete from point to the beginning of the line."
  (interactive)
  (delete-region (point-at-bol) (point)))

(defun fff-delete-till-beginning ()
	(interactive)
	(delete-region (point) (line-beginning-position)))

(defun fff-open-file-in-notes ()
  "Prompt for a filename and open it in the /projects"
  (interactive)
  (let ((default-directory "~/d/notes/"))
    (call-interactively 'find-file)))

;; contrib to emacs core?
(defun fff-Info-search-previous ()
  "Search for previous regexp from a previous `Info-search' command."
  (interactive nil Info-mode)
  (let ((case-fold-search Info-search-case-fold))
    (if Info-search-history
        (Info-search-backward (car Info-search-history))
      (call-interactively 'Info-search))))

(defun fff-toggle-audio-mute ()
  "Toggle audio mute state using PulseAudio."
  (interactive)
  (shell-command "pactl set-sink-mute @DEFAULT_SINK@ toggle"))

(defun fff-lower-volume ()
  "Lower audio volume using PulseAudio."
  (interactive)
  (shell-command "pactl set-sink-volume @DEFAULT_SINK@ -5%"))

(defun fff-raise-volume ()
  "Raise audio volume using PulseAudio."
  (interactive)
  (shell-command "pactl set-sink-volume @DEFAULT_SINK@ +5%"))

(defun fff-tab-bar-new-tab ()
  (interactive)
  (tab-new)
  (switch-to-buffer (generate-new-buffer "*scratch*")))

(defun fff-find-file-ssh ()
  "find file ssh"
  (interactive)
  (let ((default-directory "/ssh:"))
    (call-interactively 'find-file)))

(defun fff-flymake-python-init ()
	(let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
						temp-file
						(file-name-directory buffer-file-name))))
      (list "mypy" (list local-file))))

(defun fff-delete-window-and-bury-buffer ()
  "Delete the current window and bury the buffer."
  (interactive)
  (bury-buffer)
  (delete-window))

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

(defun fff-indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun user-is-in-evil-normal-state ()
 "Check if the user is in Evil's normal state."
 (eq evil-state 'normal))

(defun move-cursor-to-next-line ()
 "Move the cursor to the beginning of the next line, creating a new line if necessary."
 (goto-char (line-beginning-position 2))
 (unless (= (point) (point-max))
 (newline)))

(defun fff-cider-pprint-eval-last-sexp-to-comment-in-evil-normal-state ()
  (interactive)
 "If the user is in Evil's normal state, move the cursor to the beginning of the next line, run `cider-pprint-eval-last-sexp-to-comment`, and then return the cursor to its original position."
 (if (user-is-in-evil-normal-state)
    (save-excursion
      (save-restriction
        (move-cursor-to-next-line)
        (call-interactively 'cider-pprint-eval-last-sexp-to-comment)))
  (call-interactively 'cider-pprint-eval-last-sexp-to-comment)))

(defun fff-open-new-vterm ()
  "Open a new vterm buffer."
  (interactive)
  (vterm (generate-new-buffer-name "*vterm*")))

(defun fff-switch-or-create-vterm ()
  (interactive)
  (let ((vterm-buffers (delq nil (mapcar (lambda (b)
										   (when (string-match "\\*vterm\\*<\\([0-9]+\\)>" (buffer-name b))
											 (cons (string-to-number (match-string 1 (buffer-name b))) b)))
										 (buffer-list))))
		(newest-buffer nil))
	(if vterm-buffers
		(progn
		  (setq newest-buffer (cdr (cl-reduce (lambda (a b) (if (> (car a) (car b)) a b)) vterm-buffers)))
		  (switch-to-buffer newest-buffer))
	  (vterm))))

(defun fff-increase-font-size ()
  "Increase the font size."
  (interactive)
  (let ((current-height (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ current-height 10))
    (message "Font size increased")))

(defun fff-decrease-font-size ()
  "Decrease the font size."
  (interactive)
  (let ((current-height (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- current-height 10))
    (message "Font size decreased")))

(defun fff-vterm-directory-sync ()
 "Synchronize current working directory."
 (interactive)
 (when vterm--process
   (let* ((pid (process-id vterm--process))
          (dir (file-truename (format "/proc/%d/cwd/" pid))))
     (setq default-directory dir))))

(defun fff-toggle-flymake-highlight ()
 "Toggle the highlighting of errors and warnings in Flymake."
 (interactive)
 (if (eq (face-attribute 'flymake-error :underline nil) nil)
     (progn
       (set-face-attribute 'flymake-error nil :underline t)
       (set-face-attribute 'flymake-note nil :underline t)
       (set-face-attribute 'flymake-warning nil :underline t))
   (progn
     (set-face-attribute 'flymake-error nil :underline nil)
     (set-face-attribute 'flymake-note nil :underline nil)
     (set-face-attribute 'flymake-warning nil :underline nil))))

(defun fff-quickrun-command ()
  "Execute a quickrun command."
  (interactive)
  (let ((command (completing-read "Run quickrun command: " obarray 'commandp t "^quickrun-")))
    (if (string= "" command)
        (message "No command selected.")
      (execute-extended-command (intern command)))))

(defun fff-find-file ()
  "Like find-file, but calls fff-vterm-directory-sync if the current buffer is a vterm buffer, unless using TRAMP."
  (interactive)
  (if (and (eq major-mode 'vterm-mode)
           (not (tramp-tramp-file-p default-directory)))
      (progn
       (fff-vterm-directory-sync)
       (call-interactively 'find-file))
    (call-interactively 'find-file)))

(defun fff-buffer-switch-pop ()
 (interactive)
 (let ((last-buf (last-buffer)))
   (when last-buf
     (switch-to-buffer last-buf))))

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

(defun fff-run-browser-sync ()
  "Run 'browser-sync --watch' as an asynchronous process."
  (interactive)
  (start-process-shell-command
   "browser-sync"        ; name of the process
   "*browser-sync-output*" ; buffer to capture output
   "browser-sync --watch"))

(defun fff-display-project-root ()
  "Display the current Projectile project root."
  (interactive)
  (message "Projectile project root: %s" (projectile-project-root)))

(defun fff-find-file-in-project-root ()
  "Find a file within the current Projectile project root."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (if project-root
        (let ((default-directory project-root))
          (call-interactively 'find-file))
      (message "Not in a Projectile project"))))

(defun fff-find-packages ()
  "Find all package names used with `use-package` in `init.el` and display them in a new buffer."
  (interactive)
  (let ((file (expand-file-name "./init.el" user-emacs-directory))
        (package-regexp "\\(use-package\\s-+\\w+\\)")
        (result '())
        (buffer-name "*Packages List*"))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward package-regexp nil t)
        (let ((package-name (thing-at-point 'symbol)))
          (when package-name
            (add-to-list 'result package-name))))
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert "Packages found:\n")
        (dolist (pkg result)
          (insert (format "%s\n" pkg)))
        (goto-char (point-min)))
      (pop-to-buffer (get-buffer-create buffer-name)))))

(defun fff-connect-to-pi-tramp ()
  "Connect to the SSH server with predefined settings."
  (interactive)
  (let ((default-directory "/ssh:pi@127.0.0.1#5022:"))
    (find-file default-directory)))

(defun fff-browser-sync-current-directory ()
  "Run browser-sync to serve the current directory."
  (interactive)
  (let ((default-directory (file-name-directory (or buffer-file-name default-directory))))
    (start-process-shell-command
     "browser-sync"
     "*browser-sync*"
     "browser-sync start --server --files \"**/*\"")
    (message "browser-sync started.")))

(defun fff-browser-sync-stop ()
  "Stop the running browser-sync process."
  (interactive)
  (let ((process (get-process "browser-sync")))
    (when process
      (delete-process process)
      (message "browser-sync stopped."))))

(defun fff-menu-programming-functions ()
  "Select and run a programming-related function."
  (interactive)
  (let* ((functions-list '(lsp
                           lsp-rename
                           lsp-describe-thing-at-point
                           lsp-format-buffer
                           lsp-find-references
                           imenu
                           compile
                           flymake-show-diagnostics-buffer
                           ))
         (selected-function (completing-read "Select a function: " functions-list nil t)))
    (when selected-function
      (call-interactively (intern selected-function)))))

(defun fff-menu-entertainment ()
  "Select and run a programming-related function."
  (interactive)
  (let* ((functions-list '(emms
                           ytdl-download
                           elfeed
                           ))
         (selected-function (completing-read "Select a function: " functions-list nil t)))
    (when selected-function
      (call-interactively (intern selected-function)))))

(defun fff-menu-system-management-functions--improved ()
  "Select and run a system management-related function."
  (interactive)
  (let* ((functions-list '(("Lock screen" . (lambda () (shell-command "slock")))
                           ("Kill dwm" . (lambda () (shell-command "kill -TERM $(pidof dwm)")))
                           ("Reboot" . (lambda () (shell-command "kill -TERM $(pidof dwm) && systemctl reboot")))
                           ("Restart Emacs" . restart-emacs)
                           ("Shutdown" . (lambda () (shell-command "kill -TERM $(pidof dwm) && systemctl poweroff")))
                           ("Turn off screen" . (lambda () (shell-command "xset dpms force off")))))
         (selected-function (completing-read "Select a system management function: "
                                             (mapcar 'car functions-list)
                                             nil t)))
    (when selected-function
      (call-interactively (cdr (assoc selected-function functions-list))))))

(defun fff-menu-magit-functions ()
  "Select and run a Magit-related function."
  (interactive)
  (let* ((functions-list '(magit
                           magit-section-hide-children))
         (selected-function (completing-read "Select a Magit function: " functions-list nil t)))
    (when selected-function
      (call-interactively (intern selected-function)))))

(defun fff-menu-search-functions ()
  "Select and run a search-related function."
  (interactive)
  (let* ((functions-list '(isearch-forward
                           query-replace
                           isearch-forward-regexp
                           occur
                           iedit-mode
                           find-tag
                           rgrep
                           deadgrep
                           swiper
                           ))
         (selected-function (completing-read "Select a search function: " functions-list nil t)))
    (when selected-function
      (call-interactively (intern selected-function)))))

(defun fff-menu-main ()
  "Select and run one of the fff-menu functions."
  (interactive)
  (let* ((menu-list '(("Programming" . fff-menu-programming-functions)
                      ("System Management" . fff-menu-system-management-functions)
                      ("Magit" . fff-menu-magit-functions)
                      ("Search" . fff-menu-search-functions)))
         (selected-menu (completing-read "Select a menu: " (mapcar 'car menu-list) nil t))
         (menu-function (cdr (assoc selected-menu menu-list))))
    (when menu-function
      (call-interactively menu-function))))

(defun fff-reexecute-last-shell-command ()
  "Re-execute the last shell command executed with `shell-command`."
  (interactive)
  (let ((last-command (nth 0 shell-command-history)))
    (if last-command
        (shell-command last-command)
      (message "No previous shell command found."))))

(defun run-v ()
  "Run an interactive REPL session for the V programming language."
  (interactive)
  (let ((buffer (get-buffer-create "*V-REPL*")))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
        (comint-mode)
        (make-comint-in-buffer "V-REPL" buffer "v" nil "repl")))
    (pop-to-buffer buffer)))

(defun fff-open-in-browser()
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))

(defun fff-open-current-dir-in-vscode ()
  "Open the current directory in Visual Studio Code."
  (interactive)
  (let ((current-dir default-directory))
    (start-process "vscode" nil "code" current-dir)))

(defun fff-filter-lines-with-regex (regex)
  "Filter lines in the current buffer to show only those matching REGEX,
but only if the buffer is read-only."
  (interactive "sEnter regex to filter lines: ")
  (if buffer-read-only
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (delete-non-matching-lines regex))
    (message "This command only works in read-only buffers.")))

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
  ("interactive")
  (revert-buffer))

(defun fff-hide-lines-in-region (start end)
  "Hide lines in the region from START to END."
  (interactive "r")
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'intangible t)
    (overlay-put overlay 'isearch-open-invisible
                 'delete-overlay)))

(defun fff-show-current-line-number ()
  "Display the current line number in the echo area."
  (interactive)
  (message "Current line number: %d" (line-number-at-pos)))

(defun fff-delete-trailing-whitespace-in-dir (dir)
  "Delete trailing whitespace in all files in the specified directory."
  (interactive "DDirectory: ")
  (let ((files (directory-files-recursively dir ".*\\.\\(el\\|c\\|h\\)$"))) ;; Adjusted to exclude .txt
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (delete-trailing-whitespace)
        (write-region (point-min) (point-max) file)))))

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

(defun fff-indent-region ()
  "Call `indent-region` interactively."
  (interactive)
  (call-interactively 'indent-region))

(defun fff-open-file-in-mpv ()
  "Open the file at point in dired using mpv."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (start-process "mpv-process" nil "mpv" file)))

(defun fff-save-close-reopen-file ()
  "Save the current file, close it, and then reopen it."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (save-buffer)
      (kill-buffer)
      (find-file filename))))

(defun fff-open-straight-package-readme (package-name)
  "Open the README file of a straight package in read-only mode.
Prompt for PACKAGE-NAME with completion."
  (interactive
   (let* ((repos-dir (expand-file-name "straight/repos/" straight-base-dir))
          (package-names (if (file-directory-p repos-dir)
                             (cl-remove-if-not
                              (lambda (dir)
                                (seq-some
                                 (lambda (file)
                                   (string-match-p "^README\\(?:\\..*\\)?$" file))
                                 (directory-files (expand-file-name dir repos-dir) nil "^[^.]+")))
                              (directory-files repos-dir nil "^[^.]+"))
                           (error "Repositories directory not found: %s" repos-dir))))
     (list (completing-read "Enter package name: " package-names nil t))))
  (let* ((repo-path (expand-file-name package-name (expand-file-name "straight/repos/" straight-base-dir)))
         (readme-path (seq-some
                       (lambda (file)
                         (when (string-match-p "^README\\(?:\\..*\\)?$" file)
                           (expand-file-name file repo-path)))
                       (directory-files repo-path))))
    (if readme-path
        (let ((buffer (find-file-read-only readme-path)))
          (message "Opened %s in read-only mode." readme-path)
          buffer)
      (message "No README file found for package: %s" package-name))))

(defun fff-set-tmr-timer-for-time (time-string)
  "Set a TMR timer for the specified TIME-STRING.
TIME-STRING should be in the format \"hh:mm am/pm\"."
  (interactive "sEnter time (e.g., 4:30 pm): ")
  (let* ((current-time (current-time))
         ;; Split the time from the period (am/pm).
         (components (split-string time-string " "))
         (time-part (car components))
         (meridiem (downcase (cadr components)))
         (parsed-time (parse-time-string time-part))
         (hour (nth 2 parsed-time))

         ;; Convert 12-hour format to 24-hour format if needed.
         (hour (if (and (equal meridiem "pm") (< hour 12))
                   (+ 12 hour)
                 (if (and (equal meridiem "am") (= hour 12))
                     0
                   hour)))
         (now (decode-time current-time))
         (target-time (encode-time (nth 0 parsed-time)  ; seconds
                                   (nth 1 parsed-time)  ; minutes
                                   hour                 ; adjusted hour
                                   (nth 3 now)          ; current day
                                   (nth 4 now)          ; current month
                                   (nth 5 now)          ; current year
                                   (nth 8 now))))       ; current timezone
    ;; Adjust if the target time is already passed for today.
    (when (time-less-p target-time current-time)
      (setq target-time (time-add target-time (days-to-time 1))))
    (let ((seconds-until-target (float-time (time-subtract target-time current-time))))
      (if (> seconds-until-target 0)
          (tmr (number-to-string (/ seconds-until-target 60)))
        (error "The specified time is invalid")))))

(defun fff-maldev-academy-open-modules ()
  "Open a dired buffer with MalDev Academy modules sorted numerically, excluding directories."
  (require 'seq)
  (interactive)
  (let* ((modules-dir "~/d/books/MalDev-Academy-2024/MalDev-Academy/MalDev Modules/")
         (files (directory-files-and-attributes modules-dir nil "^[0-9]+\\..*"))
         ;; Filter out directories
         (file-names (mapcar #'car (seq-filter (lambda (file-attr)
                                                 (not (eq t (nth 1 file-attr))))
                                               files))))
    ;; Sort the files based on the numerical prefix
    (setq file-names (sort file-names
                           (lambda (a b)
                             (< (string-to-number (car (split-string a "\\.")))
                                (string-to-number (car (split-string b "\\.")))))))
    ;; Open dired with sorted files
    (dired (cons modules-dir file-names))))

(defun fff-projectile-ibuffer-for-current-project ()
  "Open an IBuffer window showing all buffers in the current project."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (if project-root
        (projectile-ibuffer-by-project project-root)
      (message "Not in a projectile project."))))

(defun fff-swap-left-and-right-windows ()
  "Swap the buffers between the left and right windows."
  (interactive)
  (let ((window1 (selected-window))
        (window2 (next-window)))
    (unless (eq window2 window1)
      (let ((buffer1 (window-buffer window1))
            (start1 (window-start window1))
            (point1 (window-point window1))
            (buffer2 (window-buffer window2))
            (start2 (window-start window2))
            (point2 (window-point window2)))
        ;; Swap the buffers
        (set-window-buffer window1 buffer2)
        (set-window-buffer window2 buffer1)
        ;; Restore the window start and point positions
        (set-window-start window1 start2)
        (set-window-point window1 point2)
        (set-window-start window2 start1)
        (set-window-point window2 point1)))))

(defun fff-find-file-in-parent-projects-directory ()
  "Open `find-file` in a parent directory whose name includes 'projects'."
  (interactive)
  (let ((current-dir (expand-file-name default-directory))
        (parent-found nil))
    (while (and (not parent-found)
                (not (string= current-dir "/")))
      (setq current-dir (file-name-directory (directory-file-name current-dir)))
      (when (string-match-p "projects" (file-name-nondirectory (directory-file-name current-dir)))
        (setq parent-found t)))
    (if parent-found
        (let ((default-directory current-dir))
          (call-interactively 'find-file))
      (message "No parent directory containing 'projects' found."))))

(defun fff-insert-current-date ()
  "Insert the current date in the format YYYY-MM-DD."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun fff-display-tooltip-at-point ()
  "Display information about the symbol at point using posframe."
  (interactive)
  (let ((help-xref-following t)
        (thing (thing-at-point 'symbol t)))
    (when thing
      (let ((tooltip-text (or (get-text-property (point) 'help-echo)
                              (help-at-pt-kbd-string)
                              (fff-help-sym-short-doc thing)
                              (fff-help-function-arglist thing))))
        (when tooltip-text
          (fff-display-centered-tooltip tooltip-text))))))

(defun fff-display-centered-tooltip (text)
  "Display TEXT horizontally centered in the window using posframe."
  (unless (facep 'fff-tooltip-face)
    (defface fff-tooltip-face
      '((t :inherit default :height 1.5))  ; Adjust the :height value to change the font size
      "Face for tooltip text in posframe."))
  (posframe-show
    "*fff-centered-tooltip*"
    :string (propertize text 'face 'fff-tooltip-face)
    :position (point)
    :poshandler (lambda (info)
                  (cons (/ (- (plist-get info :parent-frame-width)
                              (plist-get info :posframe-width))
                          2)
                        0))
    :timeout 10))

(defun fff-help-sym-short-doc (sym)
  "Return short documentation for symbol SYM."
  (when (and sym (symbolp sym))
    (or (and (boundp sym) (documentation-property sym 'variable-documentation))
        (and (fboundp sym) (documentation sym 'function-documentation)))))

(defun fff-help-function-arglist (sym)
  "Return function argument list for symbol SYM."
  (when (and (symbolp sym) (fboundp sym))
    (when-let ((args (help-function-arglist sym t)))
      (format "%s is a function: %s" sym (prin1-to-string args)))))

(defun fff-open-straight-repo (repo-name)
  "Open the directory of REPO-NAME in the straight/repos directory."
  (interactive
   (list
    (completing-read "Repository name: "
                     (directory-files
                      (expand-file-name "straight/repos/" straight-base-dir)
                      nil "^[^.]"))))
  (let ((repo-path (expand-file-name repo-name
                                     (expand-file-name "straight/repos/" straight-base-dir))))
    (if (file-directory-p repo-path)
        (dired repo-path)
      (message "Repository not found: %s" repo-name))))

(defun fff-dired-open-with-command ()
  "Prompt for a command and run it with the current file in Dired."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (command (read-string "Enter command: "))
         (full-command (concat command " " (shell-quote-argument file))))
    (start-process-shell-command "dired-open-with-command" nil full-command)))

(defun fff-consult-projectile-open-project ()
  "Select an open Projectile project using Consult."
  (interactive)
  (require 'consult)
  (require 'projectile)
  (let ((source
         `(:name     "Open Projectile Projects"
           :narrow   ?p
           :category project
           :items    ,(lambda ()
                        (delete-dups
                         (mapcar #'projectile-project-root
                                 (delq nil
                                       (mapcar (lambda (buf)
                                                 (when-let ((proj (projectile-project-p (buffer-file-name buf))))
                                                   proj))
                                               (buffer-list))))))
           :action   ,(lambda (project)
                        (projectile-switch-project-by-name project)))))
    (consult--multi (list source)
                    :prompt "Switch to open project: "
                    :sort nil
                    :history 'consult--project-history)))

(defun fff-vertico-display-candidates-in-buffer ()
  "Display the current Vertico candidates in a dedicated buffer and switch focus to it."
  (interactive)
  (when (and (bound-and-true-p vertico--candidates) (active-minibuffer-window))
    (let ((buffer (get-buffer-create "*Vertico Candidates*"))
          ;; Vertico sometimes keeps the candidates in a different structure;
          ;; hence we should ensure we're working with a list.
          (candidates (or vertico--candidates (completion-all-sorted-completions))))
      (with-current-buffer buffer
        (erase-buffer)
        (dolist (cand candidates)
          (insert (format "%s\n" cand))))
      ;; Display and switch to the buffer, then deactivate the minibuffer.
      (select-window (display-buffer buffer)))))

(defun fff-ibuffer-filter-menu-functions ()
  "Select and run an ibuffer filter-related function."
  (interactive)
  (let* ((functions-list '(ibuffer-filter-disable
                           ibuffer-exchange-filters
                           ibuffer-filter-by-mode
                           ibuffer-filter-chosen-by-completion
                           ibuffer-negate-filter
                           ibuffer-and-filter
                           ibuffer-filter-by-starred-name
                           ibuffer-filter-by-file-extension
                           ibuffer-filter-by-size-lt
                           ibuffer-filter-by-size-gt
                           ibuffer-decompose-filter-group
                           ibuffer-filter-by-process
                           ibuffer-filter-by-directory
                           ibuffer-filter-by-derived-mode
                           ibuffer-pop-filter-group
                           ibuffer-switch-to-saved-filter-groups
                           ibuffer-save-filter-groups
                           ibuffer-delete-saved-filter-groups
                           ibuffer-clear-filter-groups
                           ibuffer-add-saved-filters
                           ibuffer-filter-by-basename
                           ibuffer-filter-by-content
                           ibuffer-decompose-filter
                           ibuffer-filter-by-predicate
                           ibuffer-filter-by-filename
                           ibuffer-filters-to-filter-group
                           ibuffer-filter-by-modified
                           ibuffer-filter-by-used-mode
                           ibuffer-filter-by-name
                           ibuffer-or-filter
                           ibuffer-pop-filter
                           ibuffer-switch-to-saved-filters
                           ibuffer-save-filters
                           ibuffer-filter-by-visiting-file
                           ibuffer-delete-saved-filters
                           ibuffer-or-filter
                           ibuffer-pop-filter-group
                           ibuffer-pop-filter))
         (selected-function (completing-read "Select an ibuffer filter function: " functions-list nil t)))
    (when selected-function
      (call-interactively (intern selected-function)))))

(defun fff-project-switch-to-buffer ()
  "Switch to a project buffer only if in a project."
  (interactive)
  (if (project-current)
      (project-switch-to-buffer (project--read-project-buffer))
    (message "Not in a project")))

(defun fff-project-ibuffer ()
  "Open an IBuffer window showing all buffers in the current project."
  (interactive)
  (if-let ((project (project-current)))
      (projectile-ibuffer-by-project (project-root project))
    (message "Not in a project")))

;; switch to buffer functions

(defun fff-switch-to-scratch-buffer ()
  (interactive)
  (let ((current-buffer-name (buffer-name))
        max-number
        max-buffer)
      (progn
        (dolist (buf (buffer-list))
          (when (string-match "\\*scratch\\*<\\([0-9]+\\)>" (buffer-name buf))
            (let ((num (string-to-number (match-string 1 (buffer-name buf)))))
              (unless max-number
                (setq max-number num
                      max-buffer buf))
              (when (> num max-number)
                (setq max-number num
                      max-buffer buf)))))
        (if max-buffer
            (switch-to-buffer max-buffer)
          (progn
            (switch-to-buffer (get-buffer-create "*scratch*"))
            (fundamental-mode))))))

(defun fff-switch-to-new-scratch-buffer ()
 (interactive)
 (let ((new-buffer-name (generate-new-buffer-name "*scratch*")))
   (switch-to-buffer new-buffer-name)))

(defun fff-switch-or-create-gptel ()
  "Switch to the ChatGPT buffer or switch to the last buffer if already in ChatGPT buffer."
  (interactive)
  (let ((current-buffer-name (buffer-name))
        max-number
        max-buffer)
      (progn
        (dolist (buf (buffer-list))
          (when (string-match "\\*ChatGPT\\*<\\([0-9]+\\)>" (buffer-name buf))
            (let ((num (string-to-number (match-string 1 (buffer-name buf)))))
              (unless max-number
                (setq max-number num
                      max-buffer buf))
              (when (> num max-number)
                (setq max-number num
                      max-buffer buf)))))
        (if max-buffer
            (switch-to-buffer max-buffer)
          (progn
            (gptel "*ChatGPT*")
            (switch-to-buffer "*ChatGPT*"))))))

(defun fff-switch-to-new-gptel-buffer ()
  "Create and switch to a new ChatGPT buffer."
  (interactive)
  (let ((new-buffer-name (generate-new-buffer-name "*ChatGPT*")))
    (gptel new-buffer-name)
    (switch-to-buffer new-buffer-name)))

(defun fff-toggle-light-dark-theme ()
  "Toggle between light and dark themes.
   The themes should be named themename-light and themename-dark."
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    (if (and current-theme
             (string-match "\\(.*\\)-\\(light\\|dark\\)" (symbol-name current-theme)))
        (let* ((theme-base (match-string 1 (symbol-name current-theme)))
               (theme-target (concat theme-base "-"
                                     (if (string= (match-string 2 (symbol-name current-theme)) "light")
                                         "dark"
                                       "light"))))
          (load-theme (intern theme-target) t)
          (disable-theme current-theme))
      (message "No active theme or theme name does not match the expected pattern."))))

(defun fff-delete-current-directory ()
  "Delete the current directory, prompting for confirmation."
  (interactive)
  (let ((current-dir default-directory))
    (when (yes-or-no-p (format "Are you sure you want to delete the directory: %s? " current-dir))
      (delete-directory current-dir t)
      (message "Directory %s deleted." current-dir))))

(defun fff-insert-date ()
  "Insert the current date in YYYY-MM-DD format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun fff-insert-time ()
  "Insert the current time in 12-hour format with AM/PM."
  (interactive)
  (insert (format-time-string "%I:%M:%S %p")))

;; From https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(defun  fff-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
    ((region-active-p)
      (keyboard-quit))
    ((derived-mode-p  'completion-list-mode)
      (delete-completion-window))
    ((>  ( minibuffer-depth)  0)
      (abort-recursive-edit))
    (t
      (keyboard-quit))))

(defun fff-easy-hugo-menu-functions ()
  "Select and run an Easy Hugo function."
  (interactive)
  (let* ((functions-list '(easy-hugo-newpost
                           easy-hugo-article
                           easy-hugo-preview
                           easy-hugo-publish
                           easy-hugo-open
                           easy-hugo-delete
                           easy-hugo-open-config
                           easy-hugo-no-help
                           easy-hugo-view
                           easy-hugo-refresh
                           easy-hugo-sort-time
                           easy-hugo-sort-char
                           easy-hugo-github-deploy
                           easy-hugo-amazon-s3-deploy
                           easy-hugo-google-cloud-storage-deploy))
         (selected-function (completing-read "Select an Easy Hugo function: " functions-list nil t)))
    (when selected-function
      (call-interactively (intern selected-function)))))
