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

(let* ((theme-mode (if (string-match-p "light" (or (ignore-errors (with-temp-buffer (insert-file-contents "~/.config/theme-mode") (buffer-string))) "dark"))
                       'light 'dark))
       (bg-color (if (eq theme-mode 'light) "#ffffff" "#000000"))
       (fg-color (if (eq theme-mode 'light) "#000000" "#ffffff")))
  (setq default-frame-alist
        (append
         `((background-color . ,bg-color)
           (foreground-color . ,fg-color)
           (background-mode . ,theme-mode))
         default-frame-alist)))

(defun fff-reapply-theme-to-new-frame (frame)
  "Reapply the current theme to FRAME so it overrides default-frame-alist colors."
  (with-selected-frame frame
    (when-let ((theme (car custom-enabled-themes)))
      (enable-theme theme))))

(add-hook 'after-make-frame-functions #'fff-reapply-theme-to-new-frame)
