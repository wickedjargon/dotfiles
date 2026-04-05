;; garbage collection tweak
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(defvar fff-emacs--file-name-handler-alist file-name-handler-alist)
(defvar fff-emacs--vc-handled-backends vc-handled-backends)
(setq file-name-handler-alist nil
      vc-handled-backends nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist fff-emacs--file-name-handler-alist
                  vc-handled-backends fff-emacs--vc-handled-backends)))

(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)
(set-language-environment "UTF-8")
(setq default-input-method nil)
(setq native-comp-async-report-warnings-errors 'silent)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defun fff-update-default-frame-colors ()
  "Read ~/.config/theme-mode and update `default-frame-alist' colors.
Call this at startup and whenever the theme-mode file changes so that
new emacsclient frames are born with the correct background color
\(no white flash when in dark mode, no black flash when in light mode)."
  (let* ((theme-mode (if (string-match-p
                          "light"
                          (or (ignore-errors
                                (with-temp-buffer
                                  (insert-file-contents "~/.config/theme-mode")
                                  (buffer-string)))
                              "dark"))
                         'light 'dark))
         (bg-color (if (eq theme-mode 'light) "#ffffff" "#000000"))
         (fg-color (if (eq theme-mode 'light) "#000000" "#ffffff")))
    ;; Remove any existing color/mode entries so we don't accumulate stale ones
    (setq default-frame-alist
          (cl-remove-if (lambda (pair)
                          (memq (car pair)
                                '(background-color foreground-color background-mode)))
                        default-frame-alist))
    (setq default-frame-alist
          (append
           `((background-color . ,bg-color)
             (foreground-color . ,fg-color)
             (background-mode . ,theme-mode))
           default-frame-alist))))

;; Set the initial frame colors before any frame is created
(fff-update-default-frame-colors)
,