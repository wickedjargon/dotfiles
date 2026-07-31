;;; fff-emacs-themes.el --- theme packages and system theme sync -*- lexical-binding: t; -*-

;;; Commentary:

;; Extracted from init.el.  Loaded via `fff-require' in init.el, so an
;; error in this file is reported but does not stop the rest of init.

;;; Code:

;;; Themes

(use-package modus-themes :straight t :defer t)

(use-package doom-themes :straight t :defer t)

(use-package ef-themes
  :defer nil
  :straight t
  :config
  (add-to-list 'custom-theme-load-path (expand-file-name "fff-lisp/tty-dark-theme" user-emacs-directory))
  (cond
   ;; On Linux
   ((eq system-type 'gnu/linux)

    (defun fff--read-sys-theme ()
      "Return 'light or 'dark based on ~/.config/theme-mode."
      (if (string-match-p "light"
                          (or (ignore-errors
                                (with-temp-buffer
                                  (insert-file-contents "~/.config/theme-mode")
                                  (buffer-string)))
                              "dark"))
          'light 'dark))

    (defun fff--apply-theme-to-frame (frame)
      "Load the correct theme for FRAME.
GUI frames get the ef-tritanopia variant that matches ~/.config/theme-mode.
TTY frames (standalone non-daemon only) get tty-dark."
      (with-selected-frame frame
        (if (display-graphic-p frame)
            (if (eq (fff--read-sys-theme) 'light)
                (load-theme 'ef-tritanopia-light t)
              (load-theme 'ef-tritanopia-dark t))
          (unless (daemonp)
            (load-theme 'tty-dark t)))))

    ;; Daemon: apply theme to new GUI frames only.
    ;; Non-daemon: apply immediately to the starting frame.
    (if (daemonp)
        (add-hook 'after-make-frame-functions
                  (defun fff--daemon-apply-theme (frame)
                    (when (display-graphic-p frame)
                      (fff--apply-theme-to-frame frame))))
      (fff--apply-theme-to-frame (selected-frame)))

    ;; File watcher: react to `theme --toggle` writing ~/.config/theme-mode.
    ;; Switches between ef-tritanopia-dark and ef-tritanopia-light on GUI frames.
    ;; TTY frames (standalone) are unaffected by this watcher.
    (require 'filenotify)
    (when (file-exists-p (expand-file-name "~/.config/theme-mode"))
      (file-notify-add-watch
       (expand-file-name "~/.config/theme-mode")
       '(change attribute-change)
       (lambda (_event)
         (let ((sys-theme (fff--read-sys-theme)))
           ;; Update default-frame-alist so new emacsclient frames
           ;; are born with the correct bg color (no flash)
           (fff-update-default-frame-colors)
           (if (eq sys-theme 'light)
               (progn (disable-theme 'ef-tritanopia-dark)
                      (load-theme 'ef-tritanopia-light t))
             (progn (disable-theme 'ef-tritanopia-light)
                    (load-theme 'ef-tritanopia-dark t))))))))
   ;; On Windows
   ((eq system-type 'windows-nt)
    ;; Delay theme loading until after frame is initialized
    (add-hook 'emacs-startup-hook
              (lambda ()
                (load-theme 'fogus t))))))

(use-package sublime-themes :straight t :defer t)

(use-package zenburn-theme :straight t :defer t)

(use-package standard-themes :straight t :defer t)

(provide 'fff-emacs-themes)
