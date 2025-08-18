;; works with emacs version 30.1

;; explore these packages:
;; auctex, org
;; writing studio (https://leanpub.com/emacswritingstudio)
;; dired-kill-when-opening-new-dired-buffer

;; a few emacs kick starters:
;; emacs-kick       https://github.com/LionyxML/emacs-kick
;; Lionyxemacs-kick https://github.com/LionyxML/emacs-kick
;; kickstart.emacs  https://github.com/MiniApollo/kickstart.emacs
;; venom-emacs      https://gitlab.com/dvrbs/venom-emacs

;; Common commands for Prot's Emacs
;; prot-simple.el
;; https://protesilaos.com/emacs/dotemacs#h:5f78e837-0d27-4390-bd9a-6d0bca57fa50

;; TODO: start using email in emacs
;; emacs mail clients:
;; - Rmail
;; - MH-E
;; - Gnus
;; - Mu4e
;; - Wanderlust
;; - Notmuch

;; TODO: implement my own method for customizing packages
;; either via github forking or mantaining
;; a local branch and an upstream branch

;; TODO: transition from lsp-mode to eglot.
;; eglot is built in and is more future-proof

;; Maximize screen on new frame:
(add-hook 'after-make-frame-functions
          (lambda (&optional frame)
            (when frame
              (set-frame-parameter frame 'fullscreen 'maximized))))

;; Maximize the initial frame
(set-frame-parameter nil 'fullscreen 'maximized)

;; switching to straight.el as feel it'll be a better way to manage forked packages

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package emacs :ensure nil
  :config

  ;; x220 config
  (when (string= (system-name) "x220")
	(progn
	  (global-set-key (kbd "<XF86AudioLowerVolume>") 'fff-lower-volume)
	  (global-set-key (kbd "<XF86AudioRaiseVolume>") 'fff-raise-volume)
	  (global-set-key (kbd "<XF86AudioMute>") 'fff-toggle-audio-mute)))

  ;; for youtube change it to this:
  ;; (set-face-attribute 'default nil :height 150)

  ;; setting font height

  (if (string= (system-name) "x220")
	  (set-face-attribute 'default nil :height 95)
    (set-face-attribute 'default nil :height 135))

  ;; hooks
  (add-hook 'modus-themes-after-load-theme-hook #'pdf-view-themed-minor-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'prog-mode-hook (lambda ()
                              (setq show-trailing-whitespace t)))
  ;; (add-hook 'dired-mode-hook #'auto-revert-mode)          ;; revert dired buffers, but not buffer list buffers
  (add-hook 'prog-mode-hook #'hs-minor-mode)              ;; let me toggle shrink and expansion of code blocks
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (add-hook 'prog-mode-hook 'visual-line-mode)

  ;; make elpa and straight files read-only
  (add-hook 'find-file-hook (lambda ()
                              (when (and buffer-file-name
                                         (or (string-prefix-p (expand-file-name "elpa" user-emacs-directory) buffer-file-name)
                                             (string-prefix-p (expand-file-name "straight" straight-base-dir) buffer-file-name)))
                                (read-only-mode 1))))

  ;; confirm exit
  (add-hook 'kill-emacs-query-functions
            (lambda ()
              (yes-or-no-p "Are you sure you want to exit Emacs? ")))

  ;; I don't use most emacs keybindings. unsetting many bindings here:
  ;; key bindings

  (global-unset-key (kbd "<f1>"))
  (global-unset-key (kbd "<f2>"))
  (global-unset-key (kbd "<f3>"))
  (global-unset-key (kbd "<f4>"))
  (global-unset-key (kbd "<f4>"))
  (global-unset-key (kbd "<f10>"))
  (global-unset-key (kbd "<f11>"))
  (global-unset-key (kbd "<f16>"))
  (global-unset-key (kbd "C-x C-c"))
  (global-unset-key (kbd "C-h h"))
  (global-unset-key (kbd "C-h C-a"))
  (global-unset-key (kbd "C-h ?"))
  (global-unset-key (kbd "C-h C-h"))
  (global-unset-key (kbd "C-x"))
  (global-unset-key (kbd "M-s o"))
  (global-unset-key (kbd "C-_"))
  (global-unset-key (kbd "M-g g"))
  (global-unset-key (kbd "C-M-@"))
  (global-unset-key (kbd "C-M-d"))
  (global-unset-key (kbd "M-@"))
  (global-unset-key (kbd "C-M-x"))
  (global-unset-key (kbd "M-g n"))
  (global-unset-key (kbd "C-M-o"))
  (global-unset-key (kbd "C-<right>"))
  (global-unset-key (kbd "C-M-h"))
  (global-unset-key (kbd "C-c @ C-t"))
  (global-unset-key (kbd "M-`"))
  (global-unset-key (kbd "M-X"))
  (global-unset-key (kbd "M-s h w"))
  (global-unset-key (kbd "M-<begin>"))
  (global-unset-key (kbd "M-s h l"))
  (global-unset-key (kbd "M-s ."))
  (global-unset-key (kbd "M-r"))
  (global-unset-key (kbd "M-)"))
  (global-unset-key (kbd "M-<end>"))
  (global-unset-key (kbd "M-s h ."))
  (global-unset-key (kbd "C-M-j"))
  (global-unset-key (kbd "M-\\"))
  (global-unset-key (kbd "M-|"))
  (global-unset-key (kbd "C-M-\\"))
  (global-unset-key (kbd "M-ESC ESC"))
  (global-unset-key (kbd "C-M-q"))
  (global-unset-key (kbd "C-M-x"))
  (global-unset-key (kbd "C-M-i"))
  (global-unset-key (kbd "M-a"))
  (global-unset-key (kbd "C-c C-f"))
  (global-unset-key (kbd "C-c C-e"))
  (global-unset-key (kbd "C-c C-b"))
  (global-unset-key (kbd "C-x M-g"))
  (global-unset-key (kbd "M-:"))
  (global-unset-key (kbd "C-x g"))
  (global-unset-key (kbd "C-x M-g"))
  (global-unset-key (kbd "M-q"))
  (global-unset-key (kbd "M-g i"))
  (global-unset-key (kbd "C-M-x"))
  (global-unset-key (kbd "C-M-t"))
  (global-unset-key (kbd "M-,"))
  (global-unset-key (kbd "C-M-,"))
  (global-unset-key (kbd "M-c"))
  (global-unset-key (kbd "M-i"))
  (global-unset-key (kbd "M-g M-g"))
  (global-unset-key (kbd "C-M-<down>"))
  (global-unset-key (kbd "M-g c"))
  (global-unset-key (kbd "M-d"))
  (global-unset-key (kbd "C-M-_"))
  (global-unset-key (kbd "C-M-k"))
  (global-unset-key (kbd "M-g M-n"))
  (global-unset-key (kbd "C-M-SPC"))
  (global-unset-key (kbd "C-x C-a C-w"))
  (global-unset-key (kbd "C-M-%"))
  (global-unset-key (kbd "M-s w"))
  (global-unset-key (kbd "C-l"))
  (global-unset-key (kbd "M-<"))
  (global-unset-key (kbd "M->"))
  (global-unset-key (kbd "C-c M-g"))
  (global-unset-key (kbd "M-m"))
  (global-unset-key (kbd "C-M-c"))
  (global-unset-key (kbd "C-M-v"))
  (global-unset-key (kbd "M-v"))
  (global-unset-key (kbd "M-s h u"))
  (global-unset-key (kbd "M-^"))
  (global-unset-key (kbd "M-="))
  (global-unset-key (kbd "M-("))
  (global-unset-key (kbd "M-{"))
  (global-unset-key (kbd "M-'"))
  (global-unset-key (kbd "C-M-/"))
  (global-unset-key (kbd "C-M-a"))
  (global-unset-key (kbd "M-}"))
  (global-unset-key (kbd "C-M-l"))
  (global-unset-key (kbd "C-M-."))
  (global-unset-key (kbd "M-e"))
  (global-unset-key (kbd "M-<f10>"))
  (global-unset-key (kbd "ESC <end>"))
  (global-unset-key (kbd "M-j"))
  (global-unset-key (kbd "C-M-r"))
  (global-unset-key (kbd "C-M-s"))
  (global-unset-key (kbd "M-s _"))
  (global-unset-key (kbd "<ESC> <f10>"))
  (global-unset-key (kbd "<f20>"))
  (global-unset-key (kbd "C-M-S-l"))
  (global-unset-key (kbd "C-?"))
  (global-unset-key (kbd "M-<right>"))
  (global-unset-key (kbd "M-z"))
  (global-unset-key (kbd "M-~"))
  (global-unset-key (kbd "C-<next>"))
  (global-unset-key (kbd "C-<previous>"))
  (global-unset-key (kbd "C-<delete>"))
  (global-unset-key (kbd "M-<home>"))
  (global-unset-key (kbd "M-s M-."))

  (setq hs-minor-mode-map (make-sparse-keymap))
  (setq yas-minor-mode-map (make-sparse-keymap))
  (setq erc-track-minor-mode-map (make-sparse-keymap))
  (setq emacs-lisp-mode-map (make-sparse-keymap))
  (setq lisp-interaction-mode-map (make-sparse-keymap))
  (setq aggressive-indent-mode-map (make-sparse-keymap))

  (with-eval-after-load 'hideshow
    (setq hs-minor-mode-map (make-sparse-keymap)))

  (with-eval-after-load 'yasnippet
    (setq yas-minor-mode-map (make-sparse-keymap)))

  (with-eval-after-load 'erc-track
    (setq erc-track-minor-mode-map (make-sparse-keymap)))

  (with-eval-after-load 'emacs-lisp-mode
    (setq emacs-lisp-mode-map (make-sparse-keymap)))

  (with-eval-after-load 'lisp-interaction-mode
    (setq lisp-interaction-mode-map (make-sparse-keymap)))

  ;; adding a few back that I actually might use:
  (global-set-key (kbd "C-x o") 'other-window)
  (global-set-key (kbd "C-x u") 'undo)
  (global-set-key (kbd "C-y") 'yank)
  (global-set-key (kbd "C-x C-f") 'fff-find-file)

  (global-set-key (kbd "M-ESC M-ESC") 'keyboard-escape-quit)           ;; I have to remap to this instead
  (global-set-key (kbd "M-u") 'universal-argument)                     ;; C-u is bound to evil-scroll-up
  (global-set-key (kbd "C-x k") 'bury-buffer)                          ;; kill buffers doesn't save memory
  (global-set-key (kbd "C-c c") 'fff-clear-shell)
  (global-set-key (kbd "C-g")  #'fff-keyboard-quit-dwim)
  (global-set-key [remap find-file] 'fff-find-file)                    ;; updates the current directory when in vterm
  (global-set-key [remap list-buffers] 'ibuffer)                       ;; ibuffer is superior
  (global-set-key [remap beginning-of-line] 'beginning-of-visual-line) ;; use visual line for beginning and end of line
  (global-set-key [remap end-of-line] 'end-of-visual-line)             ;; same here.

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

  ;; evil undo
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  (setq custom-safe-themes t)                             ;; make all themes safe
  (setq inhibit-startup-message t)                        ;; no splash screen
  (setq use-short-answers t)                              ;; just type `y`, not `yes`
  (blink-cursor-mode -1)                                  ;; don't blink my cursor
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode +1)                            ;; auto revert files and buffers
  (global-goto-address-mode +1)                           ;; make links/urls clickable
  (setq safe-local-variable-values '((checkdoc-minor-mode . t))) ;; make local variables safe
  (delete-selection-mode +1)                              ;; delete selction when hitting backspace on region
  (set-default 'truncate-lines t)                         ;; don't wrap my text
  (setq custom-file (locate-user-emacs-file "custom.el")) ;; separate custom.el file
  (when (file-exists-p custom-file) (load custom-file))   ;; when it exists, load it
  (setq initial-scratch-message "")                       ;; no message on scratch buffer
  (setq auth-source-save-behavior nil)                    ;; don't prompt to save auth info in home dir
  (setq-default tab-width 4)                              ;; I prefer a tab length of 4, not 8
  (setq-default indent-tabs-mode nil)                     ;; Use spaces instead of tabs
  (setq indent-tabs-mode nil)                             ;; Use spaces instead of tabs
  (electric-pair-mode +1)                                 ;; automatically insert matching paren as well as auto indent on new line
  (show-paren-mode -1)                                    ;; Disable show-paren-mode globally
  (add-hook 'prog-mode-hook #'show-paren-local-mode)      ;; Enable show-paren-mode only in prog-mode
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
  (setq help-window-select t)  ; Switch to help buffers automatically
  (setq use-dialog-box nil)
  (setq fill-column 100)
  (setq suggest-key-bindings nil)                         ;; don't display key bindings suggestions when I run M-x commands
  (setq safe-local-variable-values
        '((checkdoc-package-keywords-flag)
          (checkdoc-minor-mode . t)))                      ;; don't prompt me about unsafe local variables
  (setq vc-follow-symlinks t)                              ;; stop prompting me about whether I want to follow symlinks
  (pixel-scroll-precision-mode t)                          ;; better for scrolling
  (setq pixel-scroll-precision-use-momentum nil)           ;; but no momentum please
  (setq warning-minimum-level :emergency)                  ;; Set the minimum level of warnings to display.
  (setq initial-major-mode 'fundamental-mode)              ;; I prefer this as the mode for scratch buffers
  (setq require-final-newline nil)                         ;; don't add a new line to the bottom of the file

  ;; prevent active process when closing a shell like vterm or eshell:
  (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

  ;; show startup time on launch
  (defun display-startup-echo-area-message ()
    (message "(emacs-init-time) -> %s" (emacs-init-time)))

  ;; recent files
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25)
  (recentf-mode +1)

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
                            themes)))

  ;; load my own config
  (with-eval-after-load 'evil
    (load (expand-file-name "fff-lisp/hide-comnt.el" user-emacs-directory))
    (load (expand-file-name "fff-lisp/fff-functions.el" user-emacs-directory)))
  (unless (display-graphic-p)
    (with-eval-after-load 'evil
      (define-key evil-insert-state-map (kbd "ESC ESC <escape>") 'evil-normal-state)))

  ;; Override `package-install` to do nothing
  (defun package-install (&rest args)
    "This has been overridden to do nothing because we use straight.el."
    (interactive)
    (message "`package-install` is disabled. Use straight.el instead."))

  ;; Optionally, prevent `package-list-packages` from running
  (defun package-list-packages (&rest args)
    "This has been overridden to do nothing because we use straight.el."
    (interactive)
    (message "`package-list-packages` is disabled. Use straight.el instead."))

  ;; Prevent package menu from displaying
  (eval-after-load 'package
    '(defalias 'list-packages 'straight-list-packages)))

(use-package modus-themes :ensure t :straight t
  :config
  (if (daemonp)
      ;; If running as a client (daemon mode), load this theme
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (load-theme 'ef-tritanopia-dark t))))
    ;; Otherwise, running Emacs normally, load modus-vivendi-tinted
    (load-theme 'modus-vivendi-tinted t)))

(use-package  doom-themes :straight t :ensure t :defer t)

(use-package ef-themes :straight t :ensure t :defer t)

(use-package sublime-themes :straight t :ensure t :defer t)

(use-package zenburn-theme :straight t :ensure t :defer t)

(use-package standard-themes :straight t :ensure t)

(use-package doom-modeline :ensure t :defer t :straight t
  :config
  (setq doom-modeline-hud t)
  (setq doom-modeline-highlight-modified-buffer-name nil)
  (setq doom-modeline-position-line-format '(""))
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-percent-position '(""))
  (setq doom-modeline-modal nil)
  (setq doom-modeline-env-enable-rust nil)
  (setq display-time-default-load-average nil)
  (setq display-time-day-and-date t)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (display-time)
  :init
  (doom-modeline-mode +1))

(use-package hippie-expand :ensure nil :defer t
  :init
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list  try-expand-line  try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))

(use-package Info :ensure nil :defer t
  :init
  (add-hook 'Info-mode-hook (lambda ()
                              (define-key Info-mode-map  (kbd "M-n") 'Info-search-next)
                              (define-key Info-mode-map (kbd "M-p") 'fff-Info-search-previous))))

(use-package yasnippet :straight t :ensure t
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (add-to-list #'yas-snippet-dirs (expand-file-name "snippets/" user-emacs-directory))
  (yas-reload-all))

(use-package flimenu :ensure t :straight t
  :config
  (flimenu-global-mode))

(use-package evil-collection :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-leader :defer t :straight t
  :commands (evil-leader-mode)
  :ensure t
  :init
  (global-evil-leader-mode)
  :config
  (progn

    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "<escape> <escape> <escape>" 'keyboard-escape-quit)

    ;; single key
    (evil-leader/set-key "SPC" 'execute-extended-command)
    (evil-leader/set-key "TAB" 'tab-bar-switch-to-tab)
    (evil-leader/set-key "d" 'delete-blank-lines)
    (evil-leader/set-key "k" 'fff-hydra-expand-region/er/expand-region)
    (evil-leader/set-key "o" 'other-window)
    (evil-leader/set-key "q" 'fff-delete-window-and-bury-buffer)
    (evil-leader/set-key "w" 'save-buffer)

    ;; text scaling
    (evil-leader/set-key "0" 'fff-set-scale-to-zero)
    (evil-leader/set-key "=" 'fff-hydra-zoom/text-scale-increase)
    (evil-leader/set-key "-" 'fff-hydra-zoom/text-scale-decrease)

    ;; embark
    (evil-leader/set-key "RET" 'embark-dwim)
    (evil-leader/set-key "c e" 'embark-act)

    ;; shell, compile, eval
    (evil-leader/set-key "x x" 'shell-command)
    (evil-leader/set-key "x X" 'async-shell-command)
    (evil-leader/set-key "c c" 'compile)
    (evil-leader/set-key "v v" 'eval-expression)

    ;; paragraph navigation
    (evil-leader/set-key "[" 'fff-hydra-paragraph-movement/evil-backward-paragraph)
    (evil-leader/set-key "]" 'fff-hydra-paragraph-movement/evil-forward-paragraph)

    ;; window size adjustment
    (evil-leader/set-key "H" 'fff-hydra-windsize/windsize-left)
    (evil-leader/set-key "L" 'fff-hydra-windsize/windsize-right)
    (evil-leader/set-key "J" 'fff-hydra-windsize/windsize-down)
    (evil-leader/set-key "K" 'fff-hydra-windsize/windsize-up)

    ;; search and replace
    (evil-leader/set-key "a a" 'avy-goto-char)
    (evil-leader/set-key "r r" 'fff-evil-regex-search)

    ;; narrow
    (evil-leader/set-key "n n" 'narrow-to-region)
    (evil-leader/set-key "n N" 'widen)

    ;; magit
    (evil-leader/set-key "m m" 'magit)

    ;; f: shortcut to file or dired buffer
    (evil-leader/set-key "f b" 'fff-access-bookmarks)
    (evil-leader/set-key "f B" 'fff-access-books)
    (evil-leader/set-key "f h" 'fff-access-hosts)
    (evil-leader/set-key "f n" 'fff-access-notes)

    ;; full screen
    (evil-leader/set-key "f s" 'toggle-frame-fullscreen)

    ;; switch to scratch
    (evil-leader/set-key "i i" 'fff-switch-to-scratch-buffer)
    (evil-leader/set-key "i I" 'fff-switch-to-new-scratch-buffer)

    ;; imenu
    (evil-leader/set-key "i m" 'consult-imenu)
    (evil-leader/set-key "i M" 'lsp-ui-imenu)

    ;; terminal
    (evil-leader/set-key "t t" 'fff-switch-or-create-vterm)
    (evil-leader/set-key "t T" 'fff-open-new-vterm)
    (evil-leader/set-key "t p" 'terminal-here)

    ;; chatgpt
    (evil-leader/set-key "g g" 'fff-switch-or-create-gptel)
    (evil-leader/set-key "g G" 'fff-switch-to-new-gptel-buffer)

    ;; x: C-x prefixes
    (evil-leader/set-key "x b" 'consult-buffer)
    (evil-leader/set-key "x B" 'fff-project-switch-to-buffer)
    (evil-leader/set-key "x 0" 'delete-window)
    (evil-leader/set-key "x 1" 'delete-other-windows)
    (evil-leader/set-key "x 2" 'split-window-below)
    (evil-leader/set-key "x 3" 'split-window-right)
    (evil-leader/set-key "x 4 4" 'other-window-prefix)
    (evil-leader/set-key "x 4 1" 'same-window-prefix)
    (evil-leader/set-key "x o" 'other-window)
    (evil-leader/set-key "x k" 'bury-buffer)
    (evil-leader/set-key "x K" 'kill-buffer)
    (evil-leader/set-key "x D" 'make-directory)
    (evil-leader/set-key "x f" 'fff-find-file)
    (evil-leader/set-key "x F" 'fff-find-file-in-project-root)
    (evil-leader/set-key "x r" 'crux-recentf-find-file)
    (evil-leader/set-key "x w" 'write-file)
    (evil-leader/set-key "x SPC b" 'ibuffer)
    (evil-leader/set-key "x SPC B" 'fff-project-ibuffer)
    (evil-leader/set-key "X C" 'save-buffers-kill-terminal)

    ;; shortcut
    (evil-leader/set-key "4 4" 'other-window-prefix)
    (evil-leader/set-key "4 1" 'same-window-prefix)

    ;; access dirs
    ;; (evil-leader/set-key "x c" 'fff-access-config-dir)
    (evil-leader/set-key "x m" 'fff-access-home-dir)
    (evil-leader/set-key "x n" 'consult-notes)
    (evil-leader/set-key "x p" 'fff-open-file-in-projects)
    (evil-leader/set-key "x s" 'fff-find-file-ssh)
    (evil-leader/set-key "x t" 'fff-open-file-in-tmp)
    (evil-leader/set-key "x /" 'fff-open-file-in-root-dir)

    ;; project root
    (evil-leader/set-key "h k" 'fff-find-file-in-project-root)
    (evil-leader/set-key "p r" 'fff-find-file-in-project-root)
    (evil-leader/set-key "p p" 'project-find-file)
    (evil-leader/set-key "p P" 'projectile-find-file-or-dir)

    ;; back to previous buffer commands
    (evil-leader/set-key "j j" 'evil-switch-to-windows-last-buffer)

    ;; tooltip hover
    (evil-leader/set-key "h h" 'fff-display-tooltip-at-point)))

(use-package evil :defer nil :ensure t :straight t
  :init
  (setq evil-insert-state-message nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-fine-undo t)
  (setq evil-search-wrap nil)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-leader/in-all-states t)  ;; Make the leader key available in all states.

  ;; hitting C-n and C-p doesn't work for the company-mode pop-up
  ;; after using C-h. The code below resolves this issue
  (with-eval-after-load 'evil
    (with-eval-after-load 'company
      (define-key evil-insert-state-map (kbd "C-n") nil)
      (define-key evil-insert-state-map (kbd "C-p") nil)
      (evil-define-key nil company-active-map (kbd "C-n") #'company-select-next)
      (evil-define-key nil company-active-map (kbd "C-p") #'company-select-previous)))
  :config
  (progn

    (setq evil-undo-system 'undo-fu)
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-fine-undo t)
    (setq evil-search-wrap nil)
    (setq evil-kill-on-visual-paste nil)
    (evil-mode +2)

    (define-key evil-visual-state-map (kbd "C-a") 'beginning-of-line)
    (define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)
    (define-key evil-visual-state-map (kbd "<backpace>") 'delete-char)
    (define-key evil-visual-state-map (kbd "C-/") 'fff-comment)
    (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)

    (define-key evil-insert-state-map (kbd "C-<backspace>") 'fff-delete-till-beginning-of-line)
    (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
    (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
    (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
    (define-key evil-insert-state-map (kbd "M-w") 'easy-kill)
    (define-key evil-insert-state-map (kbd "C-y") 'yank)
    (define-key evil-insert-state-map (kbd "M-y") 'yank-pop)
    (define-key evil-insert-state-map (kbd "C-'") 'hippie-expand)
    (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
    (define-key evil-insert-state-map (kbd "C-/") 'fff-comment)
    (define-key evil-insert-state-map (kbd "C-k") 'kill-line)

    (define-key evil-normal-state-map (kbd "C-<backspace>") 'fff-delete-till-beginning-of-line)
    (define-key evil-normal-state-map (kbd "C-a") 'beginning-of-line)
    (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-S-o") 'evil-jump-backward)
    (define-key evil-normal-state-map (kbd "C-o") 'pop-to-mark-command)
    (define-key evil-normal-state-map (kbd "M-o") 'evil-jump-forward)
    (define-key evil-normal-state-map (kbd "gp") 'fff-evil-paste-and-indent-after)
    (define-key evil-normal-state-map (kbd "gP") 'fff-evil-paste-and-indent-before)
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "C-/") 'fff-comment)
    (define-key evil-normal-state-map (kbd "C-c a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-c x") 'evil-numbers/dec-at-pt)
    (define-key evil-normal-state-map (kbd "C-c g a") 'evil-numbers/inc-at-pt-incremental)
    (define-key evil-normal-state-map (kbd "C-c g x") 'evil-numbers/dec-at-pt-incremental)
    (define-key evil-normal-state-map (kbd "q") 'quit-window)
    (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)
    (define-key evil-normal-state-map (kbd "ZZ") 'fff-save-and-bury-buffer)
    (define-key evil-normal-state-map (kbd "ZQ") 'fff-revert-and-bury-buffer)
    (define-key evil-normal-state-map (kbd "C-/") 'fff-comment)
    (define-key evil-normal-state-map (kbd "<kp-left>") 'winner-undo)
    (define-key evil-normal-state-map (kbd "<kp-right>") 'winner-redo)
    (evil-global-set-key 'normal (kbd "SPC e") 'eval-last-sexp)

    ;; a new definition for Y that is consistant with K
    (evil-define-operator my-evil-yank-to-eol (beg end type register)
      "Yank from point to the end of the line into the kill-ring."
      :move-point nil
      :type inclusive
      (interactive "<x><y>")
      (evil-yank (point) (line-end-position) type register))
    (define-key evil-normal-state-map (kbd "Y") 'my-evil-yank-to-eol)


    ;; move by visual line
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "1") 'evil-beginning-of-visual-line)
    (define-key evil-normal-state-map (kbd "$") 'evil-end-of-visual-line)
    (define-key evil-visual-state-map (kbd "1") 'evil-beginning-of-visual-line)
    (define-key evil-visual-state-map (kbd "$") 'evil-end-of-visual-line)

    ;; instead of `vi(' or `di[' use  `vib' or `dib' instead
    (define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
    (define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)))

(use-package yt-dlp-mode :ensure nil
  :init
  (add-to-list 'load-path (expand-file-name "fff-lisp" user-emacs-directory))
  (load (expand-file-name "fff-lisp/yt-dlp-mode.el" user-emacs-directory))
  (with-eval-after-load 'yt-dlp-mode
    (define-key yt-dlp-mode-map (kbd "C-c RET") 'yt-dlp-play-current-entry)
    (define-key goto-address-highlight-keymap (kbd "C-c RET") 'yt-dlp-play-current-entry)
    (define-key yt-dlp-mode-map (kbd "<mouse-2>") 'yt-dlp-play-current-entry)
    (define-key goto-address-highlight-keymap (kbd "<mouse-2>") 'yt-dlp-play-current-entry)))

(use-package ocen-mode :ensure nil
  :load-path (lambda () (expand-file-name "fff-lisp/ocen-mode" user-emacs-directory))
  :mode "\\.oc\\'"
  :init
  (add-hook 'your-major-mode-hook #'tree-sitter-mode)
  (add-hook 'your-major-mode-hook #'tree-sitter-hl-mode)
  (with-eval-after-load 'lsp-mode              ;; can't use typical use-package hook as
    (add-hook 'ocen-mode-hook #'lsp-deferred)) ;; ocen-mode is not registered with lsp-mode
  :config
  (require 'lsp-mode)
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '("\\.oc\\'" . "ocen"))
    (add-to-list 'lsp-language-id-configuration '(oc-mode . "ocen")))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda () '("node" "/home/ff/.local/src/ocen-vscode/out/server/src/server.js" "--stdio")))
    :major-modes '(ocen-mode)
    :server-id 'ocen-language-server)))

(use-package undo-fu :straight t :defer t :ensure t)

(use-package evil-surround :ensure t :straight t
  :config
  (global-evil-surround-mode +1))

(use-package evil-numbers :straight t :defer t :ensure t)

(use-package expand-region :straight t :defer t :ensure t)

(use-package lisp-mode :ensure nil
  :init
  (set-default 'auto-mode-alist
               (append '(("\\.lisp$" . lisp-mode)
                         ("\\.lsp$" . lisp-mode)
                         ("\\.cl$" . lisp-mode))
                       auto-mode-alist)))

(use-package sly :straight t :defer t :ensure t
  :init
  (set-default 'auto-mode-alist
               (append '(("\\.lisp$" . lisp-mode)
                         ("\\.lsp$" . lisp-mode)
                         ("\\.cl$" . lisp-mode))
                       auto-mode-alist))
  (add-hook 'sly-mrepl-mode-hook (lambda ()
                                   (define-key sly-mrepl-mode-map (kbd "C-p") 'comint-previous-input)
                                   (define-key sly-mrepl-mode-map (kbd "C-n") 'comint-next-input)))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  :config
  (define-key lisp-mode-map (kbd "C-j") 'sly-eval-print-last-expression)
  (define-key lisp-mode-map (kbd "C-<return>") 'sly-eval-print-last-expression)
  (evil-set-initial-state 'sly-mrepl-mode 'normal))

(use-package terminal-here :straight t :defer t :ensure t
  :init
  (setq terminal-here-linux-terminal-command 'st))

(use-package so-long :defer t :ensure t :straight t
  :init
  (global-so-long-mode +1))

(use-package hydra :straight t :defer t :ensure t :commands defhydra
  :config

  (defhydra fff-hydra-windsize (:color red :pre (setq hydra-is-helpful nil) :after-exit (setq hydra-is-helpful t))
    ("H" windsize-left nil)
    ("L" windsize-right nil)
    ("J" windsize-down nil)
    ("K" windsize-up nil))

  (defhydra fff-hydra-zoom (:color red :pre (setq hydra-is-helpful nil) :after-exit (setq hydra-is-helpful t))
    ( "=" text-scale-increase)
    ( "-" text-scale-decrease)
    ( "0"  (text-scale-set 0)))

  (defhydra fff-hydra-expand-region (:color red :pre (setq hydra-is-helpful nil) :after-exit (setq hydra-is-helpful t))
    ("k" er/expand-region)
    ("j" er/contract-region))

  (defhydra fff-hydra-paragraph-movement (:color red :pre (setq hydra-is-helpful nil) :after-exit (setq hydra-is-helpful t))
    ("[" evil-backward-paragraph)
    ("]" evil-forward-paragraph))

  (defhydra fff-tabs (:color red :pre (setq hydra-is-helpful nil) :after-exit (setq hydra-is-helpful t))
    ("l" tab-next)
    ("h" tab-previous))

  (defhydra fff-buffer-switch (:color red :pre (setq hydra-is-helpful nil) :after-exit (setq hydra-is-helpful t))
    ( "h" previous-buffer)
    ( "l" next-buffer)))

(use-package company :straight t :defer t :ensure t
  :init
  (setq company-format-margin-function nil)
  ;; (setq company-idle-delay 0.2)
  (setq company-idle-delay 0)
  (setq company-tooltip-limit 4)
  (global-company-mode)
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (file-remote-p default-directory)
                ;; Remove company-clang for remote files
                (setq-local company-backends
                            (remove 'company-clang company-backends))))))

(use-package company-statistics :straight t :ensure t
  :after company
  :hook (after-init . company-statistics-mode))

(use-package windsize :straight t :defer t :ensure t)

(use-package crux :straight t :defer t :ensure t)

(use-package emmet-mode :straight t :defer t :ensure t
  :init (add-hook 'sgml-mode-hook 'emmet-mode))

(use-package mw-thesaurus :straight t :defer t :ensure t)

(use-package sicp :straight t :defer t :ensure t)

(use-package gh-md :straight t :ensure t :defer t)

(use-package go-mode :straight t :ensure t :defer t)

(use-package vertico :straight t :defer t :ensure t
  :init
  (setq enable-recursive-minibuffers t)
  :config
  (vertico-mode +1)
  (define-key vertico-map (kbd "C-c d") 'vertico-exit-input)
  (define-key vertico-map (kbd "C-<backspace>") 'vertico-directory-up)
  (define-key minibuffer-local-map (kbd "C-c C-o") 'embark-collect))

(use-package vertico-prescient :straight t :ensure t
  :config
  (setq prescient-filter-method  '(literal regexp initialism))
  (vertico-prescient-mode +1))

(use-package  savehist :straight t
  :init
  (savehist-mode))

(use-package projectile :straight t :defer t :ensure t
  :config
  (dolist (file '(".venv/" "venv/" "manage.py" ".git/" "go.mod"
                  "package.json" "Cargo.toml" "build.sh" "v.mod"
                  "make.bat" "Makefile" "Dockerfile" ".editorconfig"
                  ".gitignore" ".svn" ".hg" ".bzr" "Pipfile" "tox.ini"
                  "requirements.txt" "pom.xml" "build.gradle"
                  "Cargo.lock" "yarn.lock" "webpack.config.js"
                  "Gemfile" ".ruby-version" "composer.json" ".env"
                  "README.md" "README.txt" "README.org" ".eslint.js"
                  "tsconfig.json" ".babelrc" ".prettierrc"
                  "CMakeLists.txt" ".project" "hugo.toml"))
    (add-to-list 'projectile-project-root-files file)
    (add-to-list 'projectile-project-root-files-bottom-up file))
  (defun projectile--find-file-or-dir (invalidate-cache)
    "Jump to a project's file or directory using completion.
With INVALIDATE-CACHE, invalidates the cache first."
    (projectile-maybe-invalidate-cache invalidate-cache)
    (let* ((project (projectile-acquire-root))
           (all-entries (projectile-project-files project))
           (dir-entries (projectile-project-dirs project))
           (candidates (append all-entries dir-entries))
           (selection (projectile-completing-read "Find file or directory: " candidates)))
      (if (member selection dir-entries)
          (dired (expand-file-name selection project))
        (find-file (expand-file-name selection project)))
      (run-hooks 'projectile-find-file-hook)))

  (defun projectile-find-file-or-dir (&optional invalidate-cache)
    "Jump to a project's file or directory using completion.
With a prefix arg INVALIDATE-CACHE, invalidates the cache first."
    (interactive "P")
    (projectile--find-file-or-dir invalidate-cache))
  :init
  (setq projectile-ignored-projects '("/home/ff"))
  (defun fff-ignore-home-directory (dir)
    "Ignore the home directory as a project root."
    (let ((home (expand-file-name "~/")))
      (string= (expand-file-name dir) home)))
  (setq projectile-ignored-project-function #'fff-ignore-home-directory)
  (projectile-mode +1)
  (with-eval-after-load 'projectile
    (define-key projectile-command-map (kbd "C-c p") nil)
    (define-key projectile-command-map (kbd "C-c P") nil)))

(use-package consult-projectile :straight t :ensure t)

(use-package marginalia :straight t :defer t :ensure t
  :init
  (marginalia-mode))

(use-package emojify :straight t :ensure t :defer t)

(use-package dired :ensure nil
  :hook (dired-mode . auto-revert-mode)  ;; revert dired buffers, but not buffer list buffers
  :custom
  (setq dired-listing-switches "-ahl --group-directories-first")  ;; group my directories and display size
  (dired-kill-when-opening-new-dired-buffer nil)               ;; Close the previous buffer when opening a new `dired' instance.
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode))))

(use-package switch-window :straight t :ensure t :defer t)

(use-package rainbow-mode :straight t :ensure t :defer t)

(use-package vimrc-mode :straight t :ensure t :defer t)

(use-package emmet-mode :straight t :ensure t :defer t
  :init
  (require 'emmet-mode)
  (add-hook 'html-mode-hook (lambda () (emmet-mode 1))))

(use-package smex :straight t :ensure t)

(use-package git-gutter :straight t :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02)
  (add-hook 'find-file-hook
            (lambda ()
              (when (and (fboundp 'tramp-tramp-file-p)
                         (tramp-tramp-file-p (or buffer-file-name "")))
                (git-gutter-mode -1)))))

(use-package git-gutter-fringe :straight t :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package hl-todo :straight t :ensure t :defer t
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)))

(use-package saveplace :straight t :init (save-place-mode))

(use-package winner :straight t :ensure t :defer t
  :init (winner-mode +1))

(use-package haskell-mode :straight t :ensure t :defer t)

(use-package volatile-highlights :straight t :ensure t :defer t
  :init
  (volatile-highlights-mode t)
  :config
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil))

(use-package typescript-mode :straight t :ensure t :defer t)

(use-package lsp-mode :straight t :ensure t
  ;; preferred LSPs:
  ;; - javascript/typescript: jsts-ls
  ;; - python:  pylsp, python-pyflakes
  ;; - html:
  ;; - css:
  ;; hooks:
  :hook (rust-mode . lsp-deferred)
  :hook (rust-ts-mode . lsp-deferred)
  :hook (svelte-mode . lsp-deferred)
  :hook (c-mode . lsp-deferred)
  :hook (c-ts-mode . lsp-deferred)
  :hook (cc-mode . lsp-deferred)
  :hook (c++-mode . lsp-deferred)
  :hook (csharp-mode . lsp-deferred)
  :hook (typescript-mode . lsp-deferred)
  :hook (javascript-mode . lsp-deferred)
  :hook (python-mode . lsp-deferred)
  :hook (d-mode . lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :hook (java-mode . lsp)
  :hook (rust-ts-mode . lsp-deferred)
  :hook (c-ts-mode . lsp-deferred)
  :hook (c++-ts-mode . lsp-deferred)
  :hook (typescript-ts-mode . lsp-deferred)
  :hook (js-ts-mode . lsp-deferred)
  :hook (python-ts-mode . lsp-deferred)
  :hook (java-ts-mode . lsp)

  ;; to do, find a way to conditionally install
  ;; an lsp using:
  ;; (lsp-install-server nil 'jsts-ls)
  :init
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-diagnostics-provider :flymake)
  (setq lsp-auto-guess-root t)
  (setq lsp-keymap-prefix "C-c l")
  ;; I use the copilot.el package, so I don't need this
  (setq lsp-copilot-enabled nil)
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-enable-file-watchers nil) ; Disable file watchers for better performance
  (setq lsp-enable-symbol-highlighting nil) ; disable symbol highlighting
  (setq lsp-headerline-breadcrumb-enable nil) ; Disable breadcrumbs in the headerline
  (setq lsp-completion-show-kind nil)
  (setq lsp-completion-show-detail nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-lens-enable nil)
  ;; (setq lsp-inlay-hints-mode t) ; the type hints next to arguments in func signature lines and variable definitions.
  ;; (setq lsp-inlay-hint-enable t)
  (setq lsp-rust-analyzer-display-parameter-hints t))

(use-package lsp-ui :straight t :ensure t :defer t
  :init
  (lsp-ui-mode +1))

(use-package lsp-python-ms :straight t :ensure t :defer t)

(use-package lsp-haskell :straight t :ensure t :defer t)

(use-package lsp-metals
  :straight t
  :ensure t
  :custom
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"
                            "-J-Dmetals.icons=unicode"))
  (lsp-metals-enable-semantic-highlighting t))

(use-package lsp-tailwindcss :straight t :ensure t :defer t
  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.erb$" . "html")) ;; Associate ERB files with HTML.
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(use-package lsp-java :straight t :ensure t
  :config (add-hook 'java-mode-hook 'lsp))

(use-package macrostep :straight t :ensure t :defer t)

(use-package nov :straight t :ensure t :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; TODO: use embark-target-finders to add a new type for youtube urls.
(use-package embark
  :straight t
  :ensure t
  :defer t
  :bind*
  (("C-c e" . embark-act)
   ("C-h b" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-prompter #'embark-completing-read-prompter)
  (setq embark-indicators '(embark--vertico-indicator))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Add crux-open-with to the existing file map
  (define-key embark-file-map (kbd "o") #'crux-open-with)
  (define-key embark-file-map (kbd "y") #'yt-dlp-play-current-entry)

  ;; Set crux-open-with as the default action for files
  (setf (alist-get 'file embark-default-action-overrides) #'crux-open-with))

(use-package diminish :straight t :ensure t :defer t)

;; audio / music payer
(use-package emms :straight t :ensure t :defer t
  :diminish emms-mode-line
  :config
  (setq emms-mode-line-format "")
  (setq emms-mode-line-icon-enabled-p nil)
  (setq emms-playing-time-display-format "")
  :init
  (emms-all)
  (emms-default-players))

(use-package avy :straight t :ensure t :defer t)

(use-package pdf-tools :straight t :ensure t  :defer t
  :mode ("\\pdf\\'" . pdf-view-mode)
  :init
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (define-key pdf-view-mode-map (kbd "<tab>") 'pdf-outline)
                                  (pdf-view-themed-minor-mode)))
  :config
  (pdf-tools-install :no-query))

(use-package vterm :straight t :ensure t :defer t
  :config
  (define-key vterm-mode-map (kbd "C-c c") 'vterm-clear))

(use-package org :ensure nil :defer t
  :hook (org-mode . visual-line-mode)
  :init
  (setq org-babel-default-header-args:python
        '((:results . "output")))
  (setq org-babel-lisp-eval-fn "sly-eval")
  (setq org-confirm-babel-evaluate nil)
  (setq org-startup-with-inline-images t)
  (setq org-babel-lisp-eval-command "sbcl --script")
  (setq org-edit-src-content-indentation 0)
  (setq org-startup-folded t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python .t)
     (haskell . t)
     (lisp . t)
     (racket . t)
     (ruby . t)
     (C . t)
     (js . t))))

(use-package ob-racket
  :straight (ob-racket
	         :type git :host github :repo "hasu/emacs-ob-racket"
	         :files ("*.el" "*.rkt"))
  :after org
  :config
  (add-hook 'ob-racket-pre-runtime-library-load-hook
	        #'ob-racket-raco-make-runtime-library))

(use-package magit :straight t :ensure t :defer t
  :init
  (setq magit-section-initial-visibility-alist
        '(([hunk file staged status] . hide)
          ([file unstaged status] . show)
          ([hunk file unstaged status] . hide))))

(use-package git-timemachine :straight t :ensure t :defer t)

(use-package clojure-mode :straight t :ensure t :defer t)

(use-package cider :straight t :ensure t :defer t
  :config
  (define-key cider-repl-mode-map (kbd "C-c c") #'cider-repl-clear-buffer))

(use-package consult :straight t :ensure t :defer t
  :init
  (global-set-key [remap imenu] 'consult-imenu))

(use-package embark-consult :straight t :ensure t :defer t)

(use-package pyvenv :straight t :ensure t :defer t)

(use-package keycast :straight t :ensure t :defer t)

(use-package org-download :straight t :ensure t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package evil-org :straight t :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-visualstar :straight t :ensure t :defer nil :straight t
  :config
  (global-evil-visualstar-mode))

(use-package evil-matchit :straight t :ensure t :defer nil :straight t
  :config
  (global-evil-matchit-mode +1))

(use-package zig-mode :straight t :ensure t :defer t)

(use-package all-the-icons :straight t :ensure t
  :if (display-graphic-p))

(use-package evil-iedit-state :straight t :ensure t :defer t
  :init
  (global-set-key (kbd "C-;") 'iedit-mode))

(use-package scala-mode :straight t :ensure t :defer t
  :interpreter
  ("scala" . scala-mode))

(use-package tree-sitter-langs :straight t :ensure t :after tree-sitter)

(use-package treesit-auto :straight t :ensure t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode t)
  :init
  (setq treesit-auto-langs '(awk bash bibtex blueprint c c-sharp clojure cmake commonlisp cpp css dart dockerfile elixir glsl go gomod heex html janet java javascript json julia kotlin latex lua magik make markdown nix nu org perl proto python r ruby rust scala sql surface toml tsx typescript typst verilog vhdl vue wast wat wgsl yaml ocen)))

(use-package devdocs :ensure t :straight t
  :init
  (add-hook 'devdocs-mode-hook (lambda () (visual-line-mode +1))))

(use-package projectile-ripgrep :straight t :ensure t)

(use-package dockerfile-mode :straight t :ensure t)

(use-package json-mode :straight t :ensure t)

(use-package deadgrep :straight t :ensure t)

(use-package exec-path-from-shell :straight t :ensure t
  :if (memq system-type '(darwin gnu/linux))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package wgrep :straight t :ensure t :defer t)

(use-package gptel :straight t :ensure t
  :init
  (setq gptel-api-key (string-trim (with-temp-buffer (insert-file-contents (expand-file-name ".secrets/chat_gpt_api_key" user-emacs-directory)) (buffer-string))))
  (setq markdown-fontify-code-blocks-natively t)
  :config
  (setq gptel-model 'gpt-4o))

(use-package sml-mode :straight t :ensure t)

;; common lisp hyperspec
(use-package clhs :straight t :ensure t :defer t)

(use-package d-mode :straight t :ensure t
  :mode "\\.d\\'"
  :config
  (setq d-mode-indent-style 'k&r))

(use-package svelte-mode :straight t :ensure t :mode "\\.svelte\\'")

;; sets indentation variables
(use-package dtrt-indent :straight t :ensure t :defer nil
  :config
  (dtrt-indent-global-mode +1)
  ;; run `dtrt-indent-try-set-offset` whenever running a function that changes the indentation
  (dolist (fn '(lsp-format-buffer
                lsp-format-region
                eglot-format-buffer
                elgot-format-region
                indent-region
                tabify
                untabify))
    (advice-add fn :after (lambda (&rest _args)
                            (when (called-interactively-p 'any)
                              (dtrt-indent-try-set-offset))))))

(use-package newsticker
  :ensure nil
  :init
  (setq newsticker-url-list-defaults nil)
  (let ((rss-secret-file (expand-file-name ".secrets/rss-feeds.el" user-emacs-directory)))
    (when (file-exists-p rss-secret-file)
      (load-file rss-secret-file)))
  :config
  (evil-collection-define-key 'normal 'newsticker-treeview-mode-map "q" 'fff-newsticker-treeview-quit))

(use-package eww :ensure nil
  :config
  (setq eww-search-prefix "https://wiby.me/?q="))

(use-package asm-mode :ensure nil
  :mode ("\\.s\\'" . asm-mode)
  ("\\.asm\\'" . asm-mode)
  :config
  ;; remove indentation
  (defun asm-indent-line ()
    "Auto-indent the current line."
    (interactive)
    (indent-line-to 0))
  (defun asm-calculate-indentation () 0)
  (defun asm-colon ()
    "Insert a colon without triggering indentation."
    (interactive)
    (let ((labelp nil))
      (save-excursion
        (skip-syntax-backward "w_")
        (skip-syntax-backward " ")
        (setq labelp (bolp)))
      (call-interactively 'self-insert-command)
      (when labelp
        (delete-horizontal-space)))))

(use-package tmr :straight t :ensure t :defer t)

(use-package ibuffer  :ensure nil
  :config
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 35 35 :left :elide) ; change: 35s were originally 18s
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename))))

(use-package v-mode :straight t :ensure t :defer t)

(use-package sly-macrostep :defer t :straight t
  :config
  (add-to-list 'sly-contribs 'sly-macrostep 'append))

(use-package read-aloud :defer t :ensure t :straight t
  :config
  (cl-defun read-aloud--current-word()
    "Pronounce a word under the pointer. If under there is rubbish,
          ask user for an additional input."
    (let* ((cw (read-aloud--u-current-word))
           (word (nth 2 cw)))

      (unless (and word (string-match "[[:alnum:]]" word))
        ;; maybe we should share the hist list w/ `wordnut-completion-hist`?
        (setq word (read-string "read aloud: " word 'read-aloud-word-hist)))

      (read-aloud--overlay-make (nth 0 cw) (nth 1 cw))
      (read-aloud--string (replace-regexp-in-string "[.\n]" "," word) "word")))

  (cl-defun read-aloud-this()
    "Pronounce either the selection or a word under the pointer."
    (interactive)

    (when read-aloud--c-locked
      (read-aloud-stop)
      (cl-return-from read-aloud-selection))

    (if (use-region-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (read-aloud--string (replace-regexp-in-string "[.\n]" "," text) "selection"))
      (read-aloud--current-word))))

(use-package graphviz-dot-mode :defer t :straight t :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

;; always open urls in a new window, use chromium always.
(use-package browse-url :ensure nil
  :init
  (setq browse-url-chromium-program "chromium")
  (defun browse-url-chromium-new-window (url &optional _new-window)
    "Open URL in a new Chromium window."
    (interactive (browse-url-interactive-arg "URL: "))
    (start-process (concat "chromium " url) nil
                   browse-url-chromium-program "--new-window" url))
  (setq browse-url-browser-function 'browse-url-chromium-new-window))

;; common lisp reference / doc lookup
(use-package hyperspec :straight t :ensure t
  :config
  (defun fff-hyperspec-lookup ()
    "Open the HyperSpec entry in EWW instead of the default browser."
    (interactive)
    (let ((browse-url-browser-function 'eww-browse-url))
      (hyperspec-lookup (thing-at-point 'symbol)))))

(use-package rust-mode :straight t :ensure t)

(use-package evil-mc :straight t :ensure t
  :after evil
  :config
  (global-evil-mc-mode 1))

(use-package csharp-mode :ensure nil
  :hook (csharp-mode . (lambda ()
                         (setq imenu-create-index-function
                               (lambda ()
                                 (let ((imenu-generic-expression
                                        '(("Variables" "^\\s-*[a-zA-Z0-9._ ]* \\([a-zA-Z0-9_]*\\)\\( = \\sw*\\|\\s-*\\);$" 1)
          ("Functions" "^\\s-*[^/]* \\([a-zA-Z0-9_]+\\)(.*)\\(\\s-*.*\n\\|\\ *\\)\\s-*{" 1)
          ("Classes" "^\\s-*\\(.*\\)class +\\([a-zA-Z0-9_]+\\)" 2)
          ("Namespaces" "^namespace +\\([a-z0-9_]*\\)" 1))))
                                   (imenu--generic-function imenu-generic-expression)))))))

(use-package tab-bar :ensure nil
  :init
  (tab-bar-mode -1) ;; Off by default
  :custom
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-new-tab-choice 'empty-buffer)
  :bind
  (("C-c w" . my-tab-bar-close-tab)
   ("C-c n" . fff-tab-bar-new-tab)
   ("C-c r" . tab-bar-rename-tab)
   ("C-c h" . tab-bar-switch-to-prev-tab)
   ("C-c l" . tab-bar-switch-to-next-tab))
  :config
  (defun my-tab-bar-close-tab ()
    "Close the current tab. Disable tab-bar-mode if there's only one left."
    (interactive)
    (tab-bar-close-tab)
    (when (= (length (tab-bar-tabs)) 1)
      (tab-bar-mode -1))))

(use-package emacs-lisp-mode
  :ensure nil  ;; emacs-lisp-mode is built-in, so no need to install it
  :init
  (defun my-emacs-lisp-mode-setup ()
    "Custom setup for `emacs-lisp-mode`."
    (setq imenu-generic-expression
          '(("Functions" "^\\s-*(\\(defun\\|defsubst\\|defalias\\)\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 2)
            ("Macros" "^\\s-*(\\(defmacro\\)\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 2)
            ("Variables" "^\\s-*(\\(defvar\\|defconst\\)\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 2)
            ("Custom Variables" "^\\s-*(\\(defcustom\\)\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 2)
            ("Set Variables" "^\\s-*(setq\\s-+(?\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Use Package" "^\\s-*(use-package\\s-+'?\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Advice" "^\\s-*(defadvice\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Add Advice" "^\\s-*(advice-add\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Faces" "^\\s-*(defface\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Derived Modes" "^\\s-*(define-derived-mode\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Minor Modes" "^\\s-*(define-minor-mode\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Generic Modes" "^\\s-*(define-generic-mode\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Structs" "^\\s-*(cl-defstruct\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Evil Commands" "^\\s-*(evil-define-command\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Autoloads" "^\\s-*(autoload\\s-+'\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)))
    (imenu-add-menubar-index))
  :hook (emacs-lisp-mode . my-emacs-lisp-mode-setup))

(use-package pulsar :straight t :defer t :ensure t
  :hook (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.025)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'evil-ex-lazy-highlight)
  (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
  (add-to-list 'pulsar-pulse-functions 'evil-scroll-up)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'next-error)
  (add-to-list 'pulsar-pulse-functions 'previous-error)
  (add-to-list 'pulsar-pulse-functions 'evil-yank)
  (add-to-list 'pulsar-pulse-functions 'evil-yank-line)
  (add-to-list 'pulsar-pulse-functions 'evil-delete)
  (add-to-list 'pulsar-pulse-functions 'evil-delete-line)
  (add-to-list 'pulsar-pulse-functions 'evil-jump-item))

;; colorful parentheses
(use-package rainbow-delimiters :straight t :defer t :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; allow copy/paste when in terminal
(use-package xclip :straight t :ensure t :defer t
  :hook
  (after-init . xclip-mode))

(use-package flymake
  :ensure nil
  :defer t
  ;; run this in all programming modes except emacs lisp mode
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p 'emacs-lisp-mode)
                         (flymake-mode +1)))))

;; irc client
(use-package erc :ensure nil :defer t
  :custom
  (erc-join-buffer 'window) ;; Open a new window when joining channels.
  (erc-hide-list '("JOIN" "PART" "QUIT" "MODE" "NICK" "TOPIC" "AWAY" "INVITE" "KICK"))
  (erc-autojoin-channels-alist
   '((".*\\.libera\\.chat"
      "#programming"
      "#emacs"
      "#python"
      "#javascript"
      "#rust"
      "#c"
      "#haskell"
      "#go-nuts"
      "#linux"
      "#archlinux"
      "#debian"
      "#latex")))
  (erc-hide-timestamps t))

(use-package consult-erc
  :ensure t
  :straight (consult-erc :type git :host codeberg :repo "mekeor/consult-erc"))

(use-package markdown-mode
  :ensure nil
  :hook (markdown-mode . visual-line-mode)
  :config
  (add-to-list 'markdown-code-lang-modes '("html" . web-mode)))

(use-package edit-indirect :straight t :ensure t)

(use-package haxe-mode :ensure t :straight t :defer t)

;; sticky header function/struct signature
(use-package topsy
  :straight (topsy :type git :host github :repo "alphapapa/topsy.el")
  :hook
  ((prog-mode . topsy-mode)
   (magit-section-mode . topsy-mode)))

;; jump to definition without ctags in many supported languages
(use-package dumb-jump :straight t :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package wgrep :straight t :ensure t :defer t)

;; search incremental count in minibuffer
(use-package evil-anzu :straight t :ensure t :config (global-anzu-mode))

;; x object for editing html/xml tab attributes
(use-package exato :straight t :ensure t :defer t)

;; visual select inside generic brackets using `b', `vib'
(use-package evil-textobj-anyblock :ensure t :straight t :defer t)

;; inline evaluation
(use-package eros :straight t :ensure t :config (eros-mode +1))

;; generate markdown toc
(use-package markdown-toc :straight t :ensure t :defer t)

;; export a code file to html
(use-package htmlize :ensure t
  :straight (:type git :host github :repo "hniksic/emacs-htmlize")
  :defer t)

(use-package swiper :straight t :ensure t :defer t)

(use-package insert-shebang :straight t :ensure t :defer t
  :hook (find-file-hook . insert-shebang))

(use-package helpful :straight t :ensure t :defer t
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-callable))

(use-package elisp-demos :straight t :ensure t :defer t
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package realgud :straight t :defer t :ensure t)

(use-package web-mode
  :ensure t
  :straight t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.hugo\\'" . web-mode))
  :hook (web-mode . (lambda () (electric-pair-local-mode -1)))
  :config
  (setq web-mode-engines-alist
        '(("django" . "\\.html\\'")
          ("go" . "\\.hugo\\'")))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t))

(use-package emmet-mode :ensure t
  :straight t
  :hook (web-mode . emmet-mode) ; Enable emmet-mode in web-mode
  :config
  (setq emmet-expand-jsx-className? t) ; Optional: if you deal with JSX
  ;; You can also customize the key binding if needed
  (define-key emmet-mode-keymap (kbd "C-j") 'emmet-expand-line))

;; (use-package consult-gh :straight t :ensure t :after consult)

(use-package consult-notes
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  (setq consult-notes-file-dir-sources '(("Notes"  ?k  "~/d/notes/"))))

(use-package gitignore-mode
  :ensure t
  :straight (:host github :repo "magit/git-modes")
  :mode "\\.gitignore\\'"
  :defer t)

(use-package aggressive-indent
  :straight t
  :ensure t
  :hook
  ((emacs-lisp-mode lisp-mode lisp-interaction-mode) . aggressive-indent-mode))

(use-package pet
  :ensure t
  :straight t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package racket-mode
  :ensure t
  :straight t
  :mode "\\.rkt\\'")

(use-package yeetube
  :straight t
  :ensure t
  :config
  (setf yeetube-mpv-disable-video t)
  ;; Set RET in normal state when in yeetube-mode
  (evil-define-key 'normal yeetube-mode-map (kbd "RET") #'yeetube-play))
