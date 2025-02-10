(defvar fff-key-set-mode-map (make-sparse-keymap)
  "Keymap for fff-key-set-mode.")

(define-minor-mode fff-key-set-mode
  "A minor mode to set a key binding to switch to the current buffer."
  :lighter " fff"
  :keymap fff-key-set-mode-map
  :global t
  (if fff-key-set-mode
      (message "fff-key-set-mode enabled")
    (message "fff-key-set-mode disabled")))

(defun fff-assign-key-to-buffer ()
 "Assign a keybinding in `fff-key-set-mode-map' to switch to the current buffer."
 (interactive)
 (let ((key (read-string "Enter the key to switch to current buffer: "))
     (buffer (current-buffer)))
 (define-key fff-key-set-mode-map key
  `(lambda ()
     (interactive)
     (if (eq (current-buffer) ,buffer)
         (evil-switch-to-windows-last-buffer)
       (switch-to-buffer ,buffer))))))

(defun fff-assign-key-to-dir ()
 "Assign a keybinding in `fff-key-set-mode-map' to open a file in the current directory."
 (interactive)
 (let ((key (read-string "Enter the key to open a file in current directory: "))
     (directory default-directory))
 (define-key fff-key-set-mode-map key
  `(lambda ()
     (interactive)
     (let ((default-directory ,directory))
       (call-interactively 'find-file))))))

(defun fff-assign-key-to-buffer-leader ()
 "Assign a keybinding in `evil-leader--default-map' to switch to the current buffer."
 (interactive)
 (let ((key (read-string "Enter the leader key to switch to current buffer: "))
     (buffer (current-buffer)))
 (evil-leader/set-key key
  `(lambda ()
     (interactive)
     (if (eq (current-buffer) ,buffer)
         (evil-switch-to-windows-last-buffer)
       (switch-to-buffer ,buffer))))))

(defun fff-assign-key-to-dir-leader ()
 "Assign a keybinding in `evil-leader--default-map' to open a file in the current directory."
 (interactive)
 (let ((key (read-string "Enter the leader key to open a file in current directory: "))
     (directory default-directory))
 (evil-leader/set-key key
  `(lambda ()
     (interactive)
     (let ((default-directory ,directory))
       (call-interactively 'find-file))))))

(defun fff-assign-key-to-position ()
  "Assign a keybinding in `fff-key-set-mode-map' to jump to the current location in the current buffer."
  (interactive)
  (let* ((key (read-string "Enter the key to jump to the current location in this buffer: "))
         (position (point-marker)))
    (define-key fff-key-set-mode-map (kbd key)
      `(lambda ()
         (interactive)
         (goto-char ,position)))))

