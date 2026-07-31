;;; fff-emacs-settings.el --- core settings, hooks and global keybindings -*- lexical-binding: t; -*-

;;; Commentary:

;; This file replaces the old monolithic `use-package emacs' block from
;; init.el.  Each `fff-configure' block is error-isolated: if one block
;; fails, the failure is reported and the remaining blocks still run.
;;
;; This module must load before any module that loads evil (the first
;; block sets the evil-want-* options).

;;; Code:

(fff-configure
  ;; Must be set before evil loads. Note: evil is loaded as a dependency
  ;; of evil-leader, so setting these in `use-package evil' :init is too
  ;; late — they would silently have no effect.
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-fine-undo t)

  ;; Must be set before evil-leader loads (global-evil-leader-mode reads it)
  (setq evil-leader/in-all-states t))

(fff-configure
  ;; for youtube change it to this:
  ;; (set-face-attribute 'default nil :height 150)

  ;; setting font height
  (set-face-attribute 'default nil :height 95))

(fff-configure
  ;; hooks
  (add-hook 'modus-themes-after-load-theme-hook #'pdf-view-themed-minor-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
  ;; (add-hook 'dired-mode-hook #'auto-revert-mode)          ;; revert dired buffers, but not buffer list buffers
  (add-hook 'prog-mode-hook #'hs-minor-mode)              ;; let me toggle shrink and expansion of code blocks
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (add-hook 'prog-mode-hook #'visual-line-mode)

  ;; make elpa and straight files read-only
  (add-hook 'find-file-hook
            (lambda ()
              (when (and buffer-file-name
                         (or (string-prefix-p (expand-file-name "elpa" user-emacs-directory) buffer-file-name)
                             (string-prefix-p (expand-file-name "straight" straight-base-dir) buffer-file-name)
                             (string-prefix-p (expand-file-name "~/.cargo") buffer-file-name)
                             (string-prefix-p (expand-file-name "~/.rustup") buffer-file-name)))
                (read-only-mode 1))))

  ;; confirm exit
  (add-hook 'kill-emacs-query-functions
            (lambda ()
              (yes-or-no-p "Are you sure you want to exit Emacs? "))))

(fff-configure
  ;; I don't use most emacs keybindings. unsetting many bindings here:
  ;; key bindings

  (global-unset-key (kbd "C-h ?")) ;; this allows me to use embark for entering prefix + ? to find possible completions
  (global-unset-key (kbd "M-ESC ESC"))

  ;; adding a few back that I actually might use:
  (global-set-key (kbd "C-x o") #'other-window)
  (global-set-key (kbd "C-x u") #'undo)
  (global-set-key (kbd "C-y") #'yank)

  (global-set-key (kbd "M-ESC M-ESC") #'keyboard-escape-quit)           ;; I have to remap to this instead
  (global-set-key (kbd "M-u") #'universal-argument)                     ;; C-u is bound to evil-scroll-up
  (global-set-key (kbd "C-x k") #'bury-buffer)                          ;; kill buffers doesn't save memory
  (global-set-key (kbd "C-c C-p") #'consult-yank-from-kill-ring)
  (global-set-key (kbd "C-g")  #'fff-keyboard-quit-dwim)
  (global-set-key [remap list-buffers] #'ibuffer)                       ;; ibuffer is superior
  (global-set-key [remap beginning-of-line] #'beginning-of-visual-line) ;; use visual line for beginning and end of line
  (global-set-key [remap end-of-line] #'end-of-visual-line)             ;; same here.
  (global-set-key (kbd "C-j") #'fff-elisp-eval-and-print-last-sexp))

(fff-configure
  ;; backup and auto save
  (setq version-control t)
  (setq vc-make-backup-files t)
  (setq delete-old-versions t)
  (setq kept-new-versions 10)
  (setq kept-old-versions 10)
  (setq auto-save-no-message nil)
  (setq auto-save-file-name-transforms
        `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))
  (setq backup-directory-alist
        `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
  (setq create-lockfiles nil))                            ;; no .#file lockfile litter

(fff-configure
  (setq custom-safe-themes t)                             ;; make all themes safe
  (setq inhibit-startup-message t)                        ;; no splash screen
  (setq use-short-answers t)                              ;; just type `y`, not `yes`
  (blink-cursor-mode -1)                                  ;; don't blink my cursor
  (set-default 'truncate-lines t)                         ;; don't wrap my text
  (setq custom-file (locate-user-emacs-file "custom.el")) ;; separate custom.el file
  (when (file-exists-p custom-file) (load custom-file))   ;; when it exists, load it
  (setq initial-scratch-message "")                       ;; no message on scratch buffer
  (setq auth-source-save-behavior nil)                    ;; don't prompt to save auth info in home dir
  (setq-default tab-width 4)                              ;; I prefer a tab length of 4, not 8
  (setq-default indent-tabs-mode nil)                     ;; Use spaces instead of tabs

  (setq disabled-command-function nil)                    ;; enable all disabled commands
  (setq ring-bell-function 'ignore)                       ;; don't ring my bell
  (setq sentence-end-double-space nil)                    ;; sentence ends with one space, not two
  (setq yank-excluded-properties t)                       ;; don't copy text with syntax highlighting
  (setq server-client-instructions "")

  ;; display battery information if battery exists on system
  (let ((has-battery-p
         (lambda ()
           "Check if the system has a battery by inspecting /sys/class/power_supply/."
           (let ((directory "/sys/class/power_supply/"))
             (when (file-directory-p directory)
               (cl-some (lambda (entry)
                          (string-prefix-p "BAT" entry))
                        (directory-files directory)))))))
    ;; Conditionally enable display-battery-mode using the lambda
    (when (funcall has-battery-p)
      (display-battery-mode +1)))                           ;; conditionally check if file exists before displaying battery mode

  (setq frame-resize-pixelwise t)                         ;; cover the whole screen when maximized
  (setq use-dialog-box nil)
  (setq fill-column 100)
  (setq suggest-key-bindings nil)                         ;; don't display key bindings suggestions when I run M-x commands
  (setq safe-local-variable-values
        '((checkdoc-package-keywords-flag)
          (checkdoc-minor-mode . t)))                      ;; don't prompt me about unsafe local variables
  (setq vc-follow-symlinks t)                              ;; stop prompting me about whether I want to follow symlinks


  ;; (setq warning-minimum-level :emergency)                  ;; Set the minimum level of warnings to display.
  (setq initial-major-mode 'fundamental-mode)              ;; I prefer this as the mode for scratch buffers
  (setq require-final-newline nil)                         ;; don't add a new line to the bottom of the file

  ;; prevent active process when closing a shell like eshell:
  (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

  ;; show startup time on launch
  (defun display-startup-echo-area-message ()
    (message "Emacs launched in %.2f seconds" (string-to-number (emacs-init-time))))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; all the builtin themes suck except for modus themes. remove all of them except modus themes.
  (advice-add 'custom-available-themes :filter-return
              (lambda (themes)
                (seq-remove (lambda (theme)
                              (member theme '(adwaita deeper-blue dichromacy leuven-dark
                                                      leuven light-blue manoj-dark misterioso
                                                      tango-dark tango tsdh-dark tsdh-light
                                                      wheatgrass whiteboard wombat)))
                            themes))))

(fff-configure
  ;; Disable package.el commands — this config uses straight.el.
  ;; Advice (unlike redefining the functions) survives package.el loading.
  (defun fff-package-el-disabled (&rest _)
    "Refuse to run package.el commands; this config uses straight.el."
    (interactive)
    (user-error "package.el is disabled — use straight.el (see fff-open-straight-repo, emacs-sync-packages.sh)"))

  (dolist (cmd '(package-install package-list-packages list-packages package-refresh-contents))
    (advice-add cmd :override #'fff-package-el-disabled)))

(fff-configure
  ;; Focus new windows: when one of these buffers is displayed, move the
  ;; cursor to its window.  This replaces the old
  ;; `fff-focus-new-window-or-buffer' advice — it keys on what buffer
  ;; appears rather than on which command ran.
  (add-to-list 'display-buffer-alist
               '((or (derived-mode . occur-mode)
                     (derived-mode . grep-mode)
                     (derived-mode . compilation-mode)
                     (derived-mode . diff-mode)
                     (derived-mode . devdocs-mode)
                     (derived-mode . Buffer-menu-mode)
                     "\\`\\*VC-history\\*\\'"
                     "\\`\\*Flymake diagnostics")
                 nil
                 (body-function . select-window)))

  ;; `split-window-below'/`split-window-right' never go through
  ;; `display-buffer', so remap their keys to focusing wrappers
  ;; (fff-buffers.el) instead.  Programmatic callers still get the
  ;; stock non-focusing commands.
  (global-set-key [remap split-window-below] #'fff-split-window-below-and-focus)
  (global-set-key [remap split-window-right] #'fff-split-window-right-and-focus))

(fff-configure
  ;; tty bindings
  (unless (display-graphic-p)
    (with-eval-after-load 'evil
      (define-key evil-insert-state-map (kbd "ESC ESC <escape>") #'evil-normal-state)))

  (unless (display-graphic-p)
    (with-eval-after-load 'evil
      (define-key evil-insert-state-map (kbd "M-'") #'hippie-expand)

      (define-key evil-visual-state-map (kbd "M-/") #'fff-comment)
      (define-key evil-insert-state-map (kbd "M-/") #'fff-comment)
      (define-key evil-normal-state-map (kbd "M-/") #'fff-comment)

      (define-key evil-insert-state-map (kbd "M-DEL") #'fff-delete-till-beginning-of-line)
      (define-key evil-normal-state-map (kbd "M-DEL") #'fff-delete-till-beginning-of-line))

    (global-set-key (kbd "M-;") #'iedit-mode)

    (with-eval-after-load 'vertico
      (define-key vertico-map (kbd "M-DEL") #'vertico-directory-up))

    (with-eval-after-load 'dired
      (define-key dired-mode-map (kbd "M-\\") #'fff-dired-open-other-window-no-focus))

    (with-eval-after-load 'consult
      (setq consult-preview-key "M-\\"))

    (with-eval-after-load 'lisp-mode
      (define-key lisp-mode-map (kbd "M-\\") #'sly-eval-print-last-expression))))

(provide 'fff-emacs-settings)
