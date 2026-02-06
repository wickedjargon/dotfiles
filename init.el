;; works with emacs version 30.1

;; explore these packages:
;; auctex, org
;; writing studio (https://leanpub.com/emacswritingstudio)
;; dired-kill-when-opening-new-dired-buffer

;; a few emacs kick starters:
;; emacs-kick       https://github.com/LionyxML/emacs-kick
;; kickstart.emacs  https://github.com/MiniApollo/kickstart.emacs
;; venom-emacs      https://gitlab.com/dvrbs/venom-emacs
;; prot's basic     https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/

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

;; TODO: packages to consider
;; - apheleia
;; - editorconfig
;; - smartparens
;; - evil-snipe

;;; Initialization

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

  ;; Must be set before evil and evil-collection load
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)

  ;; for youtube change it to this:
  ;; (set-face-attribute 'default nil :height 150)

  ;; setting font height
  (set-face-attribute 'default nil :height 95)

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
              (yes-or-no-p "Are you sure you want to exit Emacs? ")))

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
  (global-set-key (kbd "C-c c") #'fff-clear-shell)
  (global-set-key (kbd "C-c C-p") #'consult-yank-from-kill-ring)
  (global-set-key (kbd "C-g")  #'fff-keyboard-quit-dwim)
  (global-set-key [remap list-buffers] #'ibuffer)                       ;; ibuffer is superior
  (global-set-key [remap beginning-of-line] #'beginning-of-visual-line) ;; use visual line for beginning and end of line
  (global-set-key [remap end-of-line] #'end-of-visual-line)             ;; same here.
  (global-set-key (kbd "C-j") #'fff-elisp-eval-and-print-last-sexp)

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

  (setq custom-safe-themes t)                             ;; make all themes safe
  (setq inhibit-startup-message t)                        ;; no splash screen
  (setq use-short-answers t)                              ;; just type `y`, not `yes`
  (blink-cursor-mode -1)                                  ;; don't blink my cursor
  (setq safe-local-variable-values '((checkdoc-minor-mode . t))) ;; make local variables safe
  (set-default 'truncate-lines t)                         ;; don't wrap my text
  (setq custom-file (locate-user-emacs-file "custom.el")) ;; separate custom.el file
  (when (file-exists-p custom-file) (load custom-file))   ;; when it exists, load it
  (setq initial-scratch-message "")                       ;; no message on scratch buffer
  (setq auth-source-save-behavior nil)                    ;; don't prompt to save auth info in home dir
  (setq-default tab-width 4)                              ;; I prefer a tab length of 4, not 8
  (setq-default indent-tabs-mode nil)                     ;; Use spaces instead of tabs
  (setq indent-tabs-mode nil)                             ;; Use spaces instead of tabs

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
                            themes)))

  ;; load `lisp' directory
  (add-to-list 'load-path (expand-file-name "fff-lisp" user-emacs-directory))

  ;; Automatically load all .el files in that directory
  (dolist (file (directory-files (expand-file-name "fff-lisp" user-emacs-directory) t "\\.el$"))
    (load (file-name-sans-extension file)))

  (unless (display-graphic-p)
    (with-eval-after-load 'evil
      (define-key evil-insert-state-map (kbd "ESC ESC <escape>") #'evil-normal-state)))

  ;; Override `package-install` to do nothing
  (defun package-install (&rest args)
    "This has been overridden to do nothing because we use straight.el."
    (interactive)
    (message "`package-install` is disabled. Use straight.el instead."))

  ;; Prevent `package-list-packages` from running
  (defun package-list-packages (&rest args)
    "This has been overridden to do nothing because we use straight.el."
    (interactive)
    (message "`package-list-packages` is disabled. Use straight.el instead."))

  ;; Prevent package menu from displaying
  (eval-after-load 'package
    '(defalias 'list-packages 'straight-list-packages))

  ;; when new window opens, move cursor there
  (defun fff-focus-new-window-or-buffer (orig-fun &rest args)
    "Call ORIG-FUN and focus any new window or the window displaying a returned buffer, only if called interactively."
    (if (called-interactively-p 'any)
        (let ((before-windows (window-list)))
          (let ((result (apply orig-fun args)))
            ;; 1. Focus window displaying returned buffer, if any
            (when (bufferp result)
              (when-let ((w (get-buffer-window result t)))
                (select-window w)))
            ;; 2. Otherwise, focus any new window created
            (let ((new-windows (seq-filter
                                (lambda (w)
                                  (and (window-live-p w)
                                       (not (minibuffer-window-active-p w))))
                                (seq-difference (window-list) before-windows))))
              (when new-windows
                (select-window (car new-windows))))
            result))
      ;; If not interactive, just call normally
      (apply orig-fun args)))

  (dolist (cmd '(diff-buffer-with-file
                 compile
                 occur
                 grep
                 devdocs-lookup
                 list-buffers
                 split-window-below
                 split-window-right
                 switch-to-buffer-other-window
                 display-buffer
                 vc-region-history
                 flymake-show-buffer-diagnostics))
    (advice-add cmd :around #'fff-focus-new-window-or-buffer))

  ;; tty bindings
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
      (define-key lisp-mode-map (kbd "M-\\") #'sly-eval-print-last-expression)))
  )

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package autorevert
  :ensure nil
  :init
  (setq auto-revert-verbose nil)
  :hook (after-init . global-auto-revert-mode))

(use-package goto-addr
  :ensure nil
  :hook (after-init . global-goto-address-mode))

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode))

(use-package paren
  :ensure nil
  :init
  (show-paren-mode -1)
  :hook (prog-mode . show-paren-local-mode))

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25)
  :hook (after-init . recentf-mode))

(use-package pixel-scroll
  :ensure nil
  :init
  (setq pixel-scroll-precision-use-momentum nil)
  :hook (after-init . pixel-scroll-precision-mode))

;;; Buffer Navigation

(use-package dired
  :ensure nil
  :hook ((dired-mode . auto-revert-mode)
         (dired-mode . dired-omit-mode))
  :init
  (setq dired-listing-switches "-ahl --group-directories-first")
  (setq dired-omit-files "^\\.$")
  (setq dired-omit-verbose nil)
  (setq dired-omit-extensions nil)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package dired-subtree
  :straight t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package ibuffer  :ensure nil
  :config
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 35 35 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename))))

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

(use-package popper
  :straight t
  :init
  (setq popper-display-control nil)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^\\*Flymake diagnostics"
          help-mode
          compilation-mode))
  :config
  (popper-mode +1))

;;; Windows / Frames

(use-package windsize :straight t :defer t)

(use-package switch-window :straight t :defer t)

(use-package winner :ensure nil :defer t
  :init (winner-mode +1))

;;; Web Browser

(use-package browse-url
  :ensure nil
  :init
  (cond
   ;; --- Windows ---
   ((eq system-type 'windows-nt)
    ;; Adjust this path if Edge is installed elsewhere
    (setq browse-url-program
          "C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe"))

   ;; --- Linux ---
   ((eq system-type 'gnu/linux)
    (setq browse-url-program "firefox")))

  (defun browse-url-new-window (url &optional _new-window)
    "Open URL in a new Edge or Chrome window depending on OS."
    (interactive (browse-url-interactive-arg "URL: "))
    (start-process
     "browser" nil
     browse-url-program "--new-window" url))

  (setq browse-url-browser-function 'browse-url-new-window))

(use-package eww :ensure nil :defer t
  :init
  (setq eww-search-prefix "https://wiby.me/?q="))

;;; Themes

(use-package modus-themes
  :defer nil
  :straight t
  :config
  (add-to-list 'custom-theme-load-path (expand-file-name "fff-lisp/tty-dark-theme" user-emacs-directory))
  (cond
   ;; On Linux
   ((eq system-type 'gnu/linux)
    (if (daemonp)
        (add-hook 'after-make-frame-functions
                  (lambda (frame)
                    (with-selected-frame frame
                      (if (display-graphic-p frame)
                          (load-theme 'ef-tritanopia-dark t)
                        (load-theme 'tty-dark t)))))
      (if (display-graphic-p)
          (load-theme 'modus-vivendi-tinted t)
        (load-theme 'tty-dark t))))
   ;; On Windows
   ((eq system-type 'windows-nt)
    ;; Delay theme loading until after frame is initialized
    (add-hook 'emacs-startup-hook
              (lambda ()
                (load-theme 'fogus t))))))

(use-package doom-themes :straight t :defer t)

(use-package ef-themes :straight t :defer t)

(use-package sublime-themes :straight t :defer t)

(use-package zenburn-theme :straight t :defer t)

(use-package standard-themes :straight t :defer t)

;;; Evil Packages

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init)

  ;; Override PDF keybindings
  (with-eval-after-load 'pdf-view
    (evil-define-key 'normal pdf-view-mode-map
      "d" 'pdf-view-scroll-up-or-next-page
      "u" 'pdf-view-scroll-down-or-previous-page))

  ;; Override Dired Shift-Enter
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map
      (kbd "S-<return>") 'fff-dired-open-other-window-no-focus)))

(use-package evil-leader :defer nil :straight t
  :commands (evil-leader-mode)
  :config
  (global-evil-leader-mode)

  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key "<escape> <escape> <escape>" 'keyboard-escape-quit)

  ;; single key
  (evil-leader/set-key "SPC" 'execute-extended-command)
  (evil-leader/set-key "d" 'delete-blank-lines)
  (evil-leader/set-key "k" 'fff-hydra-expand-region/er/expand-region)
  (evil-leader/set-key "o" 'other-window)
  (evil-leader/set-key "q" 'fff-delete-window-and-bury-buffer)
  (evil-leader/set-key "w" 'save-buffer)

  ;; project
  (evil-leader/set-key "p p" 'project-find-file)

  ;; eval
  (evil-leader/set-key "e e" 'eval-last-sexp)

  ;; popper
  (evil-leader/set-key "TAB TAB" 'popper-toggle)
  (evil-leader/set-key "TAB t" 'popper-toggle-type)
  (evil-leader/set-key "TAB c" 'fff-popper/popper-cycle)

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

  ;; yasnippet
  (evil-leader/set-key "c s" 'yas-insert-snippet)

  ;; paragraph navigation
  (evil-leader/set-key "[" 'fff-hydra-paragraph-movement/evil-backward-paragraph)
  (evil-leader/set-key "]" 'fff-hydra-paragraph-movement/evil-forward-paragraph)

  ;; window size adjustment
  (evil-leader/set-key "H" 'fff-hydra-windsize/windsize-left)
  (evil-leader/set-key "L" 'fff-hydra-windsize/windsize-right)
  (evil-leader/set-key "J" 'fff-hydra-windsize/windsize-down)
  (evil-leader/set-key "K" 'fff-hydra-windsize/windsize-up)

  ;; search and replace
  (evil-leader/set-key "r r" 'fff-evil-regex-search)

  ;; narrow
  (evil-leader/set-key "n n" 'narrow-to-region)
  (evil-leader/set-key "n N" 'widen)

  ;; magit
  (evil-leader/set-key "m m" 'magit)

  ;; f: shortcut to file or dired buffer
  (evil-leader/set-key "f b" 'fff-access-bookmarks)
  (evil-leader/set-key "f n" 'fff-access-notes)
  (evil-leader/set-key "f B" 'fff-access-books)
  (evil-leader/set-key "f h" 'fff-access-hosts)

  ;; switch to scratch
  (evil-leader/set-key "i i" 'fff-switch-to-scratch-buffer)
  (evil-leader/set-key "i I" 'fff-switch-to-new-scratch-buffer)

  ;; imenu
  (evil-leader/set-key "i m" 'consult-imenu)

  ;; terminal / shell
  (evil-leader/set-key "t t" 'fff-switch-or-create-eshell)
  (evil-leader/set-key "t T" 'fff-open-new-eshell)
  (evil-leader/set-key "t p" 'terminal-here)

  ;; chatgpt
  (evil-leader/set-key "g g" 'fff-switch-or-create-gptel)
  (evil-leader/set-key "g G" 'fff-switch-to-new-gptel-buffer)

  ;; x: C-x prefixes
  (evil-leader/set-key "x b" 'switch-to-buffer)
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
  (evil-leader/set-key "x f" 'find-file)
  (evil-leader/set-key "x r" 'recentf)
  (evil-leader/set-key "x w" 'write-file)
  (evil-leader/set-key "x SPC b" 'ibuffer)
  (evil-leader/set-key "x SPC B" 'fff-project-ibuffer)
  (evil-leader/set-key "X C" 'save-buffers-kill-terminal)

  ;; window placement prefix
  (evil-leader/set-key "4 4" 'other-window-prefix)
  (evil-leader/set-key "4 1" 'same-window-prefix)

  ;; access dirs
  (evil-leader/set-key "x m" 'fff-access-home-dir)
  (evil-leader/set-key "x n" 'fff-open-file-in-notes)
  (evil-leader/set-key "x p" 'fff-open-file-in-projects)
  (evil-leader/set-key "x s" 'fff-find-file-ssh)
  (evil-leader/set-key "x t" 'fff-open-file-in-tmp)
  (evil-leader/set-key "x /" 'fff-open-file-in-root-dir)
  (evil-leader/set-key "x F" 'fff-find-file-in-project-root)

  ;; back to previous buffer commands
  (evil-leader/set-key "j j" 'evil-switch-to-windows-last-buffer)

  ;; tooltip hover
  (evil-leader/set-key "h h" 'fff-display-tooltip-at-point))

(use-package evil :defer nil :straight t
  :init
  ;; Must be set before evil and evil-collection load
  (setq evil-regexp-search nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-insert-state-message nil)
  (setq evil-want-fine-undo t)
  (setq evil-search-wrap nil)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-leader/in-all-states t)  ;; Make the leader key available in all states.

  :config
  (progn
    (evil-mode +1)

    ;; Custom evil state tags for modeline
    (setq evil-normal-state-tag   " <NORMAL> ")
    (setq evil-insert-state-tag   " <INSERT> ")
    (setq evil-visual-state-tag   " <VISUAL> ")
    (setq evil-motion-state-tag   " <MOTION> ")
    (setq evil-emacs-state-tag    " <EMACS> ")
    (setq evil-operator-state-tag " <OPERATOR> ")
    (setq evil-replace-state-tag  " <REPLACE> ")

    ;; Use block cursor in all states (consistent with TTY)
    (setq evil-normal-state-cursor 'box)
    (setq evil-insert-state-cursor 'box)
    (setq evil-visual-state-cursor 'box)
    (setq evil-motion-state-cursor 'box)
    (setq evil-replace-state-cursor 'box)
    (setq evil-operator-state-cursor 'box)
    (setq evil-emacs-state-cursor 'box)
    ;; Force Evil mode in Eglot event buffers
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (dolist (buf (buffer-list))
                  (when (string-match-p "^\\*EGLOT" (buffer-name buf))
                    (with-current-buffer buf
                      (evil-normalize-keymaps)
                      (evil-local-mode 1))))))

    ;; Optional: enable Evil in all *-buffers
    (setq evil-buffer-regexps
          (append evil-buffer-regexps '(("^\\*EGLOT" . normal))))

    (define-key evil-visual-state-map (kbd "C-a") #'beginning-of-line)
    (define-key evil-visual-state-map (kbd "C-e") #'move-end-of-line)
    (define-key evil-visual-state-map (kbd "<backspace>") #'delete-char)
    (define-key evil-visual-state-map (kbd "C-/") #'fff-comment)
    (define-key evil-visual-state-map (kbd "j") #'evil-next-visual-line)
    (define-key evil-visual-state-map (kbd "k") #'evil-previous-visual-line)

    (define-key evil-insert-state-map (kbd "C-<backspace>") #'fff-delete-till-beginning-of-line)
    (define-key evil-insert-state-map (kbd "C-a") #'beginning-of-line)
    (define-key evil-insert-state-map (kbd "C-e") #'move-end-of-line)
    (define-key evil-insert-state-map (kbd "C-w") #'kill-region)
    (define-key evil-insert-state-map (kbd "M-w") #'easy-kill)
    (define-key evil-insert-state-map (kbd "C-y") #'yank)
    (define-key evil-insert-state-map (kbd "M-y") #'yank-pop)
    (define-key evil-insert-state-map (kbd "C-'") #'hippie-expand)
    (define-key evil-insert-state-map (kbd "M-'") #'hippie-expand)
    (define-key evil-insert-state-map (kbd "C-d") #'delete-char)
    (define-key evil-insert-state-map (kbd "C-/") #'fff-comment)
    (define-key evil-insert-state-map (kbd "C-k") #'kill-line)

    (define-key evil-normal-state-map (kbd "C-<backspace>") #'fff-delete-till-beginning-of-line)
    (define-key evil-normal-state-map (kbd "C-a") #'beginning-of-line)
    (define-key evil-normal-state-map (kbd "C-e") #'move-end-of-line)
    (define-key evil-normal-state-map (kbd "C-u") #'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-S-o") #'evil-jump-backward)
    (define-key evil-normal-state-map (kbd "C-o") #'pop-to-mark-command)
    (define-key evil-normal-state-map (kbd "M-o") #'evil-jump-forward)
    (define-key evil-normal-state-map (kbd "gp") #'fff-evil-paste-and-indent-after)
    (define-key evil-normal-state-map (kbd "gP") #'fff-evil-paste-and-indent-before)
    (define-key evil-normal-state-map (kbd "j") #'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") #'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "C-/") #'fff-comment)
    (define-key evil-normal-state-map (kbd "C-c a") #'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-c x") #'evil-numbers/dec-at-pt)
    (define-key evil-normal-state-map (kbd "C-c g a") #'evil-numbers/inc-at-pt-incremental)
    (define-key evil-normal-state-map (kbd "C-c g x") #'evil-numbers/dec-at-pt-incremental)
    (define-key evil-normal-state-map (kbd "q") #'quit-window)
    (define-key evil-normal-state-map (kbd "Q") #'evil-record-macro)
    (define-key evil-normal-state-map (kbd "ZZ") #'fff-save-and-bury-buffer)
    (define-key evil-normal-state-map (kbd "ZQ") #'fff-revert-and-bury-buffer)
    (define-key evil-normal-state-map (kbd "<kp-left>") #'winner-undo)
    (define-key evil-normal-state-map (kbd "<kp-right>") #'winner-redo)
    ;; (evil-global-set-key 'normal (kbd "SPC e e") 'eval-last-sexp)

    ;; a new definition for Y that is consistent with K
    (evil-define-operator fff-evil-yank-to-eol (beg end type register)
      "Yank from point to the end of the line into the kill-ring."
      :move-point nil
      :type inclusive
      (interactive "<x><y>")
      (evil-yank (point) (line-end-position) type register))
    (define-key evil-normal-state-map (kbd "Y") #'fff-evil-yank-to-eol)

    ;; move by visual line
    (define-key evil-normal-state-map (kbd "j") #'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") #'evil-previous-visual-line)
    (define-key evil-visual-state-map (kbd "j") #'evil-next-visual-line)
    (define-key evil-visual-state-map (kbd "k") #'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "0") #'evil-beginning-of-visual-line)
    (define-key evil-normal-state-map (kbd "$") #'evil-end-of-visual-line)
    (define-key evil-visual-state-map (kbd "0") #'evil-beginning-of-visual-line)
    (define-key evil-visual-state-map (kbd "$") #'evil-end-of-visual-line)

    ;; instead of `vi(' or `di[' use  `vib' or `dib' instead
    (define-key evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
    (define-key evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block)))

(use-package undo-fu :straight t :defer t)

(use-package evil-surround :straight t
  :config
  (global-evil-surround-mode +1))

(use-package evil-numbers :straight t :defer t)

(use-package evil-org :straight t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; use * / # to go to the next word under cursor
(use-package evil-visualstar :straight t :defer nil
  :config
  (global-evil-visualstar-mode))

;; jump between html / xml tags like <div> and its match </div> using % key
(use-package evil-matchit :straight t :defer nil
  :config
  (global-evil-matchit-mode +1))

;; edit all matches for region in document
(use-package evil-iedit-state :straight t :defer t
  :init
  (global-set-key (kbd "C-;") #'iedit-mode))

(use-package evil-mc :straight t
  :after evil
  :config
  (global-evil-mc-mode 1))

;; search incremental count in minibuffer
(use-package evil-anzu :straight t :config (global-anzu-mode))

;; x object for editing html/xml tab attributes
(use-package exato :straight t :defer t)

;; visual select inside generic brackets using `b', `vib'
(use-package evil-textobj-anyblock :straight t :defer t)

(use-package evil-snipe
  :straight t
  :diminish evil-snipe-local-mode
  :config
  ;; Enable evil-snipe globally
  (evil-snipe-mode +1)
  ;; Override evil's default f/F/t/T behavior
  ;; This allows you to snipe with 2 characters using f/t
  (evil-snipe-override-mode +1))

;;; Other Key Binding Packages

(use-package hydra :straight t :defer t :commands defhydra
  :config

  ;; Only repeating hydra for cycling
  (defhydra fff-popper (:pre (setq hydra-is-helpful nil) :after-exit (setq hydra-is-helpful t))
    "Popper"
    ("TAB" popper-toggle :exit t)
    ("c" popper-cycle :exit nil)
    ("t" popper-toggle-type :exit t))

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
    ("]" evil-forward-paragraph)))

;;; Language Support Modes And Related Packages

;;; Web Dev Related Packages

(use-package web-mode
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

(use-package typescript-mode :straight t :defer t)

(use-package svelte-mode :straight t :mode "\\.svelte\\'")

;; live server behavior of vs code
(use-package simple-httpd :straight t :defer t)

(use-package impatient-mode :straight t :defer t
  :init
  (defun fff-imp-open-preview ()
    "Open current buffer in impatient-mode live preview."
    (interactive)
    (browse-url
     (format "http://localhost:8080/imp/live/%s"
             (buffer-name)))))

(use-package emmet-mode
  :straight t
  :defer t
  :hook ((web-mode . emmet-mode)
         (sgml-mode . emmet-mode)
         (html-mode . emmet-mode))
  :config
  (setq emmet-expand-jsx-className? t)
  (define-key emmet-mode-keymap (kbd "C-j") #'emmet-expand-line))

;; export a code file to html
(use-package htmlize :straight t :defer t)

;;; Org Packages

(use-package ob-racket
  :straight (ob-racket
	         :type git :host github :repo "hasu/emacs-ob-racket"
	         :files ("*.el" "*.rkt"))
  :after org
  :config
  (add-hook 'ob-racket-pre-runtime-library-load-hook
	        #'ob-racket-raco-make-runtime-library))

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
   '((python . t)
     (haskell . t)
     (lisp . t)
     (racket . t)
     (ruby . t)
     (C . t)
     (js . t))))

(use-package org-download :straight t :defer t
  :hook (dired-mode . org-download-enable))

;;; Common Lisp

(use-package lisp-mode :ensure nil
  :mode (("\\.lisp\\'" . lisp-mode)
         ("\\.lsp\\'" . lisp-mode)
         ("\\.cl\\'" . lisp-mode)))

;; common lisp hyperspec
(use-package clhs :straight t :defer t)

;; common lisp documentation
(use-package hyperspec :straight t :defer t
  :commands (hyperspec-lookup)
  :init
  (setq common-lisp-hyperspec-root
        (concat "file://" (expand-file-name "~/.local/share/HyperSpec/")))
  :config
  (defun fff-hyperspec-lookup ()
    "Open the HyperSpec entry in EWW instead of the default browser."
    (interactive)
    (let ((browse-url-browser-function 'eww-browse-url))
      (hyperspec-lookup (thing-at-point 'symbol)))))

(use-package sly :straight t :defer t
  :init
  (setq inferior-lisp-program
        (if (eq system-type 'windows-nt)
            "\"c:/Program Files/Steel Bank Common Lisp/sbcl.exe\""
          "/usr/bin/sbcl"))


  :config
  (define-key lisp-mode-map (kbd "C-j") #'sly-eval-print-last-expression)
  (define-key lisp-mode-map (kbd "C-<return>") #'sly-eval-print-last-expression)
  (evil-set-initial-state 'sly-mrepl-mode 'normal))

;; actually used in elisp
(use-package macrostep :straight t :defer t)

(use-package sly-macrostep :defer t :straight t
  :config
  (add-to-list 'sly-contribs 'sly-macrostep 'append))

;;; Elisp Packages

(use-package elisp-mode
  :ensure nil
  ;; Define your binding specifically for the interaction mode (scratch buffer)
  :bind (:map lisp-interaction-mode-map
              ("C-j" . fff-elisp-eval-and-print-last-sexp))
  :init
  (defun fff-emacs-lisp-mode-setup ()
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
            ("Autoloads" "^\\s-*(autoload\\s-+'\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Category Title" "^;;; \\(.*\\)$" 1)))
    (imenu-add-menubar-index))
  :hook (emacs-lisp-mode . fff-emacs-lisp-mode-setup))

;; inline evaluation
(use-package eros :defer nil :straight t :config (eros-mode +1))

;;; Other Language Support Packages

(use-package gitignore-mode
  :straight (:host github :repo "magit/git-modes")
  :mode "\\.gitignore\\'"
  :defer t)

(use-package asm-mode
  :ensure nil
  :mode ("\\.s\\'" "\\.asm\\'")
  :hook (asm-mode . fff-no-indent-asm)
  :config
  (defun fff-no-indent-asm ()
    ;; 1. Your existing settings (keep these to stop TAB/RET indentation)
    (setq-local indent-line-function #'ignore)
    (setq-local indent-region-function #'ignore)
    (setq-local electric-indent-inhibit t)
    (electric-indent-local-mode -1)

    ;; 2. THE FIX: Stop ':' from triggering asm-colon logic
    (local-set-key (kbd ":") #'self-insert-command)
    (local-set-key (kbd ";") #'self-insert-command)))

(use-package rust-mode :straight t :defer t)

(use-package csharp-mode :ensure nil :defer t
  :hook (csharp-mode . (lambda ()
                         (setq imenu-create-index-function
                               (lambda ()
                                 (let ((imenu-generic-expression
                                        '(("Variables" "^\\s-*[a-zA-Z0-9._ ]* \\([a-zA-Z0-9_]*\\)\\( = \\sw*\\|\\s-*\\);$" 1)
                                          ("Functions" "^\\s-*[^/]* \\([a-zA-Z0-9_]+\\)(.*)\\(\\s-*.*\n\\|\\ *\\)\\s-*{" 1)
                                          ("Classes" "^\\s-*\\(.*\\)class +\\([a-zA-Z0-9_]+\\)" 2)
                                          ("Namespaces" "^namespace +\\([a-z0-9_]*\\)" 1))))
                                   (imenu--generic-function imenu-generic-expression)))))))

(use-package pyvenv :straight t :defer t)

(use-package realgud :straight t :defer t)

;;; Markdown

(use-package markdown-mode
  :straight (:host github :repo "jrblevin/markdown-mode")
  :hook (markdown-mode . visual-line-mode)
  :config
  (add-to-list 'markdown-code-lang-modes '("html" . web-mode)))

;; generate markdown toc
(use-package markdown-toc :straight t :defer t)

(use-package gh-md :straight t :defer t)

;;; PDF / EPUB

(use-package pdf-tools :straight t :defer t
  :mode ("\\pdf\\'" . pdf-view-mode)
  :init
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (define-key pdf-view-mode-map (kbd "<tab>") 'pdf-outline)
                                  (pdf-view-themed-minor-mode)))
  :config
  (pdf-tools-install :no-query))

(use-package nov :straight t :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;;; Incremental Completion

(use-package vertico :straight t :defer nil
  :init
  (setq enable-recursive-minibuffers t)
  (vertico-mode +1)
  :config
  (define-key vertico-map (kbd "C-c d") 'vertico-exit-input)
  (define-key vertico-map (kbd "C-<backspace>") 'vertico-directory-up)
  (define-key minibuffer-local-map (kbd "C-c C-o") 'embark-collect))

(use-package vertico-prescient :straight t
  :config
  (setq prescient-filter-method  '(literal regexp initialism))
  (vertico-prescient-mode +1))

(use-package savehist :ensure nil :config (savehist-mode))

(use-package marginalia
  :straight t
  :init
  (defun fff-marginalia-annotate-command-keybinding (cand)
    "Return only the keybinding for command CAND, styled with a custom face."
    (when-let ((cmd (intern-soft cand)))
      (when (commandp cmd)
        (let ((keys (where-is-internal cmd nil t)))
          (when keys
            (propertize
             (format " (%s)" (key-description keys))
             'face 'font-lock-comment-face))))))

  ;; Use our custom annotator for commands, keep others default
  (setq marginalia-annotators
        '((command fff-marginalia-annotate-command-keybinding)
          (variable marginalia-annotate-variable)
          (t nil)))
  :config
  (marginalia-mode +1))

(use-package consult :straight t :defer nil
  :init
  (setq consult-preview-key nil)
  (setq consult-preview-key "C-<return>"))

(use-package corfu
  :straight t
  :hook (prog-mode . corfu-mode)
  :init
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 3
        corfu-quit-no-match t
        corfu-preview-current nil
        corfu-count 5)
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "C-c C-o" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

(use-package cape
  :straight t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

;; TODO: use embark-target-finders to add a new type for youtube urls.
(use-package embark
  :straight t
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

  ;; Existing actions
  (define-key embark-file-map (kbd "o") #'crux-open-with)
  (define-key embark-file-map (kbd "y") #'yt-dlp-play-current-entry)

  ;; Open in Firefox (URL or file)
  (defun fff-open-in-firefox (target)
    (interactive "sOpen in Firefox: ")
    (start-process "firefox" nil "firefox" "--new-window" target))

  (define-key embark-url-map (kbd "b") #'fff-open-in-firefox)
  (define-key embark-file-map (kbd "b") #'fff-open-in-firefox)

  ;; NEW: manual open-with program (final, fixed version)
  (defun fff-open-with-program (file)
    "Prompt for any program and open FILE with it.
Supports arguments and GUI programs. Expands path to avoid doubling."
    (interactive "fFile: ")
    (let* ((file (expand-file-name file)) ;; <-- fix path here
           (input (read-shell-command "Open with program: "))
           (parts (split-string-and-unquote input))
           (prog  (car parts))
           (args  (cdr parts))
           (exe   (executable-find prog)))
      (unless exe
        (user-error "Program not found: %s" prog))
      ;; Add file at the end
      (apply #'start-process prog nil exe (append args (list file)))))


  ;; Add it under "p" in Embark file actions
  (define-key embark-file-map (kbd "p") #'fff-open-with-program)

  ;; DEFAULT DWIM ACTIONS
  (setf (alist-get 'file embark-default-action-overrides)
        #'crux-open-with)

  (setf (alist-get 'url embark-default-action-overrides)
        #'fff-open-in-firefox))

(use-package embark-consult :straight t :defer t)

;;; Snippets

(use-package hippie-expand :ensure nil :defer t
  :init
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-complete-file-name-partially
          try-complete-file-name try-expand-all-abbrevs try-expand-list
          try-expand-line try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially try-complete-lisp-symbol)))

(use-package yasnippet :straight t
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets/" user-emacs-directory))
  (yas-reload-all))

;;; Git

(use-package magit :straight t :defer t
  :init
  (setq magit-section-initial-visibility-alist
        '(([hunk file staged status] . hide)
          ([file unstaged status] . show)
          ([hunk file unstaged status] . hide))))

(use-package git-timemachine :straight t :defer t)

(use-package git-gutter :straight t
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:update-interval 0.02)
  (add-hook 'find-file-hook
            (lambda ()
              (when (and (fboundp 'tramp-tramp-file-p)
                         (tramp-tramp-file-p (or buffer-file-name "")))
                (git-gutter-mode -1)))))

(use-package git-gutter-fringe :straight t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package consult-gh :straight t :defer t)

;;; Tools

(use-package tmr :straight t :defer t)

(use-package read-aloud :defer t :straight t)

(use-package gif-screencast
  :init
  (setq gif-screencast-output-directory (expand-file-name "gif-screencast/" user-emacs-directory))
  :straight (gif-screencast
             :type git
             :host gitlab
             :repo "ambrevar/emacs-gif-screencast"))

;;; Entertainment

;; irc client
(use-package erc :ensure nil :defer t
  :custom
  (erc-join-buffer 'window)
  (erc-hide-list '("JOIN" "PART" "QUIT" "MODE" "NICK" "TOPIC" "AWAY" "INVITE" "KICK"))
  (doom-modeline-irc nil)
  (erc-autojoin-channels-alist
   '((".*\\.libera\\.chat"
      "#programming"
      "#emacs"
      "#devuan"
      "#python"
      "#javascript"
      "#rust"
      "#c"
      "#haskell"
      "#go-nuts"
      "#linux"
      "#debian"
      "#latex")))
  (erc-hide-timestamps t)
  (erc-server-auto-reconnect t)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  :config
  (evil-leader/set-key-for-mode 'erc-mode "x B" 'consult-erc))

(use-package consult-erc
  :ensure nil
  :load-path "local-packages/consult-erc"
  :after (consult marginalia))

;; rss feed reader
(use-package newsticker
  :ensure nil
  :after evil-collection
  :init
  (setq newsticker-url-list-defaults nil)
  (let ((rss-secret-file (expand-file-name ".secrets/rss-feeds.el" user-emacs-directory)))
    (when (file-exists-p rss-secret-file)
      (load-file rss-secret-file)))
  :config
  (evil-collection-define-key
    'normal
    'newsticker-treeview-mode-map
    "q" #'fff-newsticker-treeview-quit))

;; audio / music payer
(use-package emms :straight t :defer t
  :commands (emms)
  :diminish emms-mode-line
  :init
  (defun emms-volume-set (level)
    "Set absolute volume directly using pactl (bypasses emms-volume-get)."
    (interactive "nSet volume to (0-100): ")
    (when (and (>= level 0) (<= level 100))
      ;; 'call-process' runs the command synchronously
      (call-process "pactl" nil nil nil "set-sink-volume" "@DEFAULT_SINK@" (format "%s%%" level))
      (message "Volume set to %d%%" level)))
  (setq emms-volume-change-amount 5) ;; lower / raise volume in increments of 5 instead of 2.
  ;; because it takes too long to lower / raise volume with 2
  (setq emms-mode-line-format "")
  (setq emms-mode-line-icon-enabled-p nil)
  (setq emms-playing-time-display-format "")
  :config
  (emms-all)
  (emms-default-players))

(use-package yeetube
  :defer t
  :straight t
  :config
  (setf yeetube-mpv-disable-video t)
  ;; Set RET in normal state when in yeetube-mode
  (evil-define-key 'normal yeetube-mode-map (kbd "RET") #'yeetube-play))

(use-package hacker-typer :straight t :defer t)

;;; Project / Search

(use-package project
  :ensure nil
  :defer t
  :config
  ;; Add custom project root markers
  ;; project.el looks for .git by default, but we can add more markers
  (setq project-vc-extra-root-markers
        '(".venv" "venv" "manage.py" "go.mod"
          "package.json" "Cargo.toml" "build.sh" "v.mod"
          "make.bat" "Makefile" "Dockerfile" ".editorconfig"
          ".gitignore" ".svn" ".hg" ".bzr" "Pipfile" "tox.ini"
          "requirements.txt" "pom.xml" "build.gradle"
          "Cargo.lock" "yarn.lock" "webpack.config.js"
          "Gemfile" ".ruby-version" "composer.json" ".env"
          "README.md" "README.txt" "README.org" ".eslint.js"
          "tsconfig.json" ".babelrc" ".prettierrc"
          "CMakeLists.txt" ".project" "hugo.toml")))

(use-package deadgrep :straight t :defer t)

(use-package wgrep :straight t :defer t)

;;; Docs / Lookup

(use-package help :ensure nil
  :custom
  (help-window-select t)   ; Focus the help window immediately
  :hook
  (help-fns-describe-function-functions . shortdoc-help-fns-examples-function))

(use-package Info :ensure nil :defer t
  :init
  (add-hook 'Info-mode-hook (lambda ()
                              (define-key Info-mode-map  (kbd "M-n") #'Info-search-next)
                              (define-key Info-mode-map (kbd "M-p") #'fff-Info-search-previous))))

(use-package devdocs :straight t :defer t
  :init
  (add-hook 'devdocs-mode-hook (lambda () (visual-line-mode +1))))

(use-package mw-thesaurus :straight t :defer t)

;;; Indentation

(use-package aggressive-indent
  :straight t
  :hook
  ((emacs-lisp-mode lisp-mode lisp-interaction-mode) . aggressive-indent-mode))

;; sets indentation variables
(use-package dtrt-indent :straight t :defer nil
  :config
  (dtrt-indent-global-mode +1)
  ;; run `dtrt-indent-try-set-offset` whenever running a function that changes the indentation
  (dolist (fn '(eglot-format-buffer
                elgot-format-region
                indent-region
                tabify
                untabify))
    (advice-add fn :after (lambda (&rest _args)
                            (when (called-interactively-p 'any)
                              (dtrt-indent-try-set-offset))))))

;;; Terminal / Shell

(use-package terminal-here
  :straight t
  :defer t
  :init
  (setq terminal-here-linux-terminal-command 'st)
  (setq terminal-here-windows-terminal-command 'powershell)
  :config
  (with-eval-after-load 'terminal-here
    (add-to-list 'terminal-here-terminal-command-table
                 (cons 'powershell
                       '("cmd.exe" "/C" "start" "powershell.exe" "-NoExit" "-Command" "cd $PWD")))))

(use-package exec-path-from-shell :straight t
  :if (memq system-type '(darwin gnu/linux))
  :config
  (exec-path-from-shell-initialize))

;; allow copy/paste when in terminal
(use-package xclip :straight t :defer t
  :hook
  (after-init . xclip-mode))

(use-package eshell
  :ensure nil
  :hook (eshell-mode . fff-eshell-clear-1-binding)
  :config
  (defun fff-eshell-clear-1-binding ()
    "Bind C-c c to fff-eshell-clear-1 in eshell."
    (local-set-key (kbd "C-c c") #'fff-eshell-clear-1)))

;;; UI Packages

(use-package doom-modeline :straight t
  :init
  (setq doom-modeline-hud t)
  (setq doom-modeline-highlight-modified-buffer-name nil)
  (setq doom-modeline-position-line-format '(""))
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-percent-position '(""))
  (setq doom-modeline-modal t)              ;; Show evil state in modeline (NORMAL, INSERT, etc.)
  (setq doom-modeline-modal-icon nil)       ;; Use text labels instead of icons (better for terminal)
  (setq doom-modeline-env-enable-rust nil)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-time-analogue-clock nil)
  (setq doom-modeline-icon nil)
  :config
  (doom-modeline-mode +1))

(use-package time
  :ensure nil
  :init
  (setq display-time-default-load-average nil)
  (setq display-time-day-and-date t)
  :config
  (display-time))

(use-package pulsar :straight t :defer t
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
  (add-to-list 'pulsar-pulse-functions 'evil-jump-item)
  (add-to-list 'pulsar-pulse-functions 'fff-delete-till-beginning-of-line)
  (add-to-list 'pulsar-pulse-functions 'fff-evil-yank-to-eol))

(use-package volatile-highlights :straight t :defer t
  :init
  (volatile-highlights-mode t)
  :config
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil))

;; sticky header function/struct signature
(use-package topsy
  :straight (topsy :type git :host github :repo "alphapapa/topsy.el")
  :hook
  ((prog-mode . topsy-mode)
   (magit-section-mode . topsy-mode)))

(use-package keycast :straight t :defer t)

(use-package posframe
  :straight (posframe :type git :host github :repo "tumashu/posframe"))

(use-package rainbow-mode :straight t :defer t)

;; colorful parentheses
(use-package rainbow-delimiters :straight t :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package so-long :ensure nil
  :config
  (global-so-long-mode +1))

(use-package hl-todo :straight t :defer t
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)))

(use-package diminish :straight t :defer t)

;;; General Text/Code Editing / IDE / Navigation / Jumping

(use-package flymake
  :ensure nil
  :defer t
  ;; run this in all programming modes except emacs lisp mode
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p 'emacs-lisp-mode)
                         (flymake-mode +1)))))

(use-package eglot
  :ensure nil
  :hook ((rust-mode
          rust-ts-mode
          svelte-mode
          c-ts-mode c++-ts-mode
          c-mode
          csharp-mode
          typescript-mode typescript-ts-mode
          js-ts-mode javascript-mode
          python-mode python-ts-mode
          d-mode
          go-mode
          java-mode java-ts-mode) . eglot-ensure)
  :init
  (setq eglot-ignored-server-capabilities
        '(:inlayHintProvider
          :documentHighlightProvider))
  :config
  (add-to-list 'eglot-server-programs
               '(csharp-mode . ("csharp-ls"))))

(use-package flimenu :straight t :defer nil
  :config
  (flimenu-global-mode))

(use-package saveplace :ensure nil :init (save-place-mode))

(use-package expand-region :straight t :defer t)

;; jump to definition without ctags in many supported languages
(use-package dumb-jump :straight t
  :init
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; Disable aggressive mode to prevent overly broad searches
  (setq dumb-jump-aggressive nil)
  :config
  (setq dumb-jump-selector 'vertico)

  ;; Custom function to limit search scope when not in a project
  (defun fff-dumb-jump-get-project-root (orig-fun &rest args)
    "Wrapper around dumb-jump project detection.
If not in a project, return the directory containing the current file
to limit the search scope to just that directory."
    (or (when-let ((proj (project-current nil)))
          (project-root proj))
        ;; If no project found, use the file's directory
        (file-name-directory (or buffer-file-name default-directory))))

  ;; Apply advice to dumb-jump's project detection
  (advice-add 'dumb-jump-get-project-root :around #'fff-dumb-jump-get-project-root))

(use-package edit-indirect :straight t :defer t)

(use-package treesit-auto :straight t
  :after emacs
  :config
  (global-treesit-auto-mode t))

;; see if we can remove this and use our own functions
(use-package crux :straight t :defer t
  :config
  (defun crux-open-with (arg)
    "Open visited file in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
    (interactive "P")
    (let* ((current-file-name
            (if (derived-mode-p 'dired-mode)
                (dired-get-file-for-visit)
              buffer-file-name))
           (open (pcase system-type
                   (`darwin "open")
                   ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")
                   (`windows-nt "start")))
           (program (if (or arg (not open))
                        (read-shell-command "Open current file with: ")
                      open)))
      (if (string= program "start")
          (shell-command (concat "start \"\" \"" current-file-name "\""))
        (call-process program nil 0 nil current-file-name)))))

(use-package editorconfig
  :ensure nil
  :config
  (editorconfig-mode 1))
