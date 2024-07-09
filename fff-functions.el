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
    (shell-command (concat "chromium file://" buffer-file-name)))
   ((string-equal system-type "gnu/linux")
    (shell-command (concat "chromium file://" buffer-file-name)))
   ((string-equal system-type "darwin") ; Mac
    (shell-command (concat "open -a Firefox.app file://" buffer-file-name)))))

(defun fff-clear-shell ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun fff-remove-blank-lines ()
  (interactive)
  (flush-lines "^\s*$" (point-min) (point-max)))

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

(defun fff-switch-to-scratch-buffer ()
 (interactive)
 (let ((current-buffer-name (buffer-name))
       max-number
       max-buffer)
   (if (or (string-match "\\*scratch\\*<\\([0-9]+\\)>" current-buffer-name)
           (string= current-buffer-name "*scratch*"))
       (evil-switch-to-windows-last-buffer)
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
         (switch-to-buffer "*scratch*"))))))

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

(defun fff-toggle-visual-line-mode ()
  (interactive)
  (if (not visual-line-mode)
      (progn
        (visual-line-mode +1)
        (message "visual line mode on"))
    (progn
      (visual-line-mode -1)
      (message "visual line mode off")
      )))

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
        (evil-insert-state 1)
        )
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

(defun fff-split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun fff-split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

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
  "Comment region if active, otherwise uncomment if already commented, otherwise comment the current line."
  (interactive)
  (if (use-region-p)
      (if (fff-region-commented-p)
          (uncomment-region (region-beginning) (region-end))
        (comment-region (region-beginning) (region-end)))
	(comment-line 1)))

(defun fff-region-commented-p ()
 "Return t if the region is already commented, nil otherwise."
 (save-excursion
   (let ((start (region-beginning))
         (end (region-end))
         (all-commented t))
     (goto-char start)
     (while (< (point) end)
       (unless (looking-at "^[ \t]*$") ; Skip blank lines
         (setq all-commented (and all-commented
                                 (looking-at (concat "\\s-*" (regexp-quote comment-start))))))
       (forward-line 1))
     all-commented)))

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

(defun fff-switch-to-new-scratch-buffer ()
 (interactive)
 (let ((new-buffer-name (generate-new-buffer-name "*scratch*")))
   (switch-to-buffer new-buffer-name)))

(defun fff-open-new-vterm ()
  "Open a new vterm buffer."
  (interactive)
  (vterm (generate-new-buffer-name "*vterm*")))


(defun fff-switch-or-create-vterm ()
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (evil-switch-to-windows-last-buffer)
	(let ((vterm-buffers (delq nil (mapcar (lambda (b)
											 (when (string-match "\\*vterm\\*<\\([0-9]+\\)>" (buffer-name b))
											   (cons (string-to-number (match-string 1 (buffer-name b))) b)))
										   (buffer-list))))
		  (newest-buffer nil))
	  (if vterm-buffers
		  (progn
			(setq newest-buffer (cdr (cl-reduce (lambda (a b) (if (> (car a) (car b)) a b)) vterm-buffers)))
			(switch-to-buffer newest-buffer))
		(vterm)))))


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
 "Like find-file, but calls fff-vterm-directory-sync if the current buffer is a vterm buffer."
 (interactive)
 (when (eq major-mode 'vterm-mode)
   (fff-vterm-directory-sync))
 (call-interactively 'find-file))

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
