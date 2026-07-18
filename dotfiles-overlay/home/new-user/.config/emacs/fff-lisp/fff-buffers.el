;;; fff-buffers.el --- Buffer and window management commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal commands, split out of the old fff-functions.el.

;;; Code:

;;;; Buffer Management

(defun fff-buffer-list-switch ()
  "Switch to buffer list and activate the window"
  (interactive)
  (list-buffers)
  (select-window (get-buffer-window "*Buffer List*" 0)))

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
                      (dired-get-filename nil t)
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

(defun fff-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun fff-kill-entire-buffer-list ()
  (interactive)
  (dolist (cur (buffer-list))
    (kill-buffer cur)))

(defun fff-switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun fff-copy-buffer-name ()
  "Copy the name of the current buffer to the kill ring."
  (interactive)
  (kill-new (buffer-name))
  (message "Buffer name '%s' copied to the kill ring." (buffer-name)))

(defun fff-tab-bar-new-tab ()
  (interactive)
  (tab-new)
  (switch-to-buffer (generate-new-buffer "*scratch*")))

(defun fff-delete-window-and-bury-buffer ()
  "Delete the current window and bury the buffer."
  (interactive)
  (bury-buffer)
  (delete-window))

(defun fff-buffer-switch-pop ()
  (interactive)
  (let ((last-buf (last-buffer)))
    (when last-buf
      (switch-to-buffer last-buf))))

(defun fff-save-close-reopen-file ()
  "Save the current file, close it, and then reopen it."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (save-buffer)
      (kill-buffer)
      (find-file filename))))

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


;;;; Window Management

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

(defun fff-dired-open-other-window-no-focus ()
  "Open file in the 'next' window specifically, without switching focus.
   If only one window exists, split it first."
  (interactive)
  (let ((filename (dired-get-file-for-visit)))
    ;; 1. If we are the only window, we MUST split to have a place to put the file.
    (when (one-window-p)
      ;; Force a split (you can change this to split-window-vertically if you prefer)
      (split-window-horizontally))
    
    ;; 2. Load the file into memory (buffer) without displaying it yet
    (let ((new-buffer (find-file-noselect filename)))
      
      ;; 3. Force the 'next' window to display this buffer.
      ;; This bypasses Emacs's decision-making on whether to split again.
      (set-window-buffer (next-window) new-buffer))))

(defun fff-other-window-prefix-left ()
  "Display the buffer of the next command in a new window to the left."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (cons (display-buffer-in-direction buffer
                                        (append alist '((direction . left))))
           'window))))

(defun fff-other-window-prefix-right ()
  "Display the buffer of the next command in a new window to the right."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (cons (display-buffer-in-direction buffer
                                        (append alist '((direction . right))))
           'window))))

(defun fff-move-current-window-left ()
  "Swap the current window with the window to the left and follow focus."
  (interactive)
  (require 'windmove)
  (let ((target (windmove-find-other-window 'left)))
    (unless target
      (user-error "No window to the left"))
    (window-swap-states nil target)
    (select-window target)))

(defun fff-move-current-window-right ()
  "Swap the current window with the window to the right and follow focus."
  (interactive)
  (require 'windmove)
  (let ((target (windmove-find-other-window 'right)))
    (unless target
      (user-error "No window to the right"))
    (window-swap-states nil target)
    (select-window target)))


(provide 'fff-buffers)
;;; fff-buffers.el ends here
