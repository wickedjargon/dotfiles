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

;; (defun fff-emacs-re-enable-frame-theme (_frame)
;;   (when-let ((theme (car custom-enabled-themes)))
;;     (enable-theme theme)))

;; (setq mode-line-format nil)
;; (set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
;; (set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)
;; (add-hook 'after-make-frame-functions #'fff-emacs-re-enable-frame-theme)
