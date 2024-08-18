;; compatible with emacs version 28 and above

;; TODO: replace string paths with expressions (relative config location)
;; TODO: switch from evil-leader to general.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; starting our engines... ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(setq package-list '(use-package markdown-mode gcmh))

(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(gcmh-mode +1) ;; reduce garbage collection interference.

;; maximize screen on new frame:
(add-hook 'after-make-frame-functions
          (lambda (&optional frame)
            (with-selected-frame frame
              (toggle-frame-maximized))))

;; but on first launch
(toggle-frame-maximized)

;;;;;;;;;;;;;;;;;;;;;;;
;; use package setup ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :ensure nil
  :config

  ;; hooks
  (add-hook 'modus-themes-after-load-theme-hook #'pdf-view-themed-minor-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'prog-mode-hook (lambda ()
                              (setq indent-tabs-mode t)
                              (setq show-trailing-whitespace t)))
  (add-hook 'dired-mode-hook #'auto-revert-mode)          ;; revert dired buffers, but not buffer list buffers
  (add-hook 'prog-mode-hook #'hs-minor-mode)              ;; let me toggle shrink and expansion of code blocks
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (add-hook 'comint-mode-hook (lambda ()
                                (define-key comint-mode-map (kbd "C-p") 'comint-previous-input)
                                (define-key comint-mode-map (kbd "C-n") 'comint-next-input)))

  ;; make elpa files read-only
  (add-hook 'find-file-hook (lambda ()
                              (when (and buffer-file-name
                                         (string-prefix-p (expand-file-name "elpa" user-emacs-directory) buffer-file-name))
                                (read-only-mode 1))))

  (add-hook 'kill-emacs-query-functions
          (lambda ()
            (yes-or-no-p "Are you sure you want to exit Emacs? ")))

  ;; switch to new window
  (defun fff-advice-for-window-focus (orig-fun &rest args)
    "Advice function to focus on the new window after running the specified function."
    (let ((current-window (selected-window)))
      (apply orig-fun args)
      (select-window (next-window current-window))))
  (advice-add 'diff-buffer-with-file :around #'fff-advice-for-window-focus)
  (advice-add 'vc-region-history :around #'fff-advice-for-window-focus)
  (advice-add 'list-buffers :around #'fff-advice-for-window-focus)
  (advice-add 'rustic-cargo-run :around #'fff-advice-for-window-focus)
  (advice-add 'flymake-show-buffer-diagnostics :around #'fff-advice-for-window-focus)
  (advice-add 'devdocs-lookup :around #'fff-advice-for-window-focus)

  ;; key bindings
  (global-set-key (kbd "M-u") 'universal-argument)
  (global-set-key (kbd "C-x k") 'bury-buffer)
  (global-unset-key (kbd "C-x C-c"))
  (global-unset-key (kbd "C-h h"))
  (define-key ctl-x-map (kbd "C-f") 'fff-find-file)
  (global-set-key (kbd "C-x C-f")  'fff-find-file)
  (global-set-key (kbd "C-c c")  'fff-clear-shell)
  (global-set-key (kbd "C-x 3") 'fff-split-and-follow-vertically)
  (global-set-key (kbd "C-x 2") 'fff-split-and-follow-horizontally)

  ;; tab-bar mode
  (setq tab-bar-new-tab-to 'rightmost)
  (setq tab-bar-new-tab-choice 'empty-buffer)
  (tab-bar-mode +1)
  (global-set-key (kbd "C-c w") 'tab-bar-close-tab)
  (global-set-key (kbd "C-c n") 'fff-tab-bar-new-tab)
  (global-set-key (kbd "C-c r") 'tab-bar-rename-tab)

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
  ;; (setq-default indent-tabs-mode t)
  ;; (defvaralias 'c-basic-offset 'tab-width)
  (setq dired-listing-switches "-ahl --group-directories-first")  ;; group my directories and display size
  (setq disabled-command-function nil)                    ;; enable all disabled commands
  (setq ring-bell-function 'ignore)                       ;; don't ring my bell
  (setq sentence-end-double-space nil)                    ;; sentence ends with one space, not two
  (global-eldoc-mode -1)
  (display-battery-mode +1)

  (setq frame-resize-pixelwise t)                         ;; cover the whole screen when maximized
  (setq help-window-select t)  ; Switch to help buffers automatically
  (setq use-dialog-box nil)

  ;; prevent active process when closing a shell like vterm or eshell:
  (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

  ;; show startup time on launch
  (defun display-startup-echo-area-message ()
    (message "(emacs-init-time) -> %s" (emacs-init-time)))

  ;; open .pl files in prolog-mode
  (autoload 'prolog-mode "prolog" "" t)
  (add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25)
  (recentf-mode +1)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt)))

;; (use-package modus-themes :ensure t :defer nil
;;   :config
;;   (setq modus-themes-to-toggle '(modus-vivendi modus-operandi))
;;   (if (string= (system-name) "x1c")
;;       (set-face-attribute 'default nil :height 135)
;;     (set-face-attribute 'default nil :height 95))
;;   (load-theme (car modus-themes-to-toggle) t))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package ef-themes :ensure t :defer t)

(use-package flymake :ensure nil
  :config
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" fff-flymake-python-init))
  (add-hook 'python-mode-hook 'flymake-mode))

(use-package asm-mode :ensure nil :defer t)

(use-package hippie-expand :ensure nil :defer t
  :init
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list  try-expand-line  try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))

(use-package Info :ensure nil :defer t
  :init
  (add-hook 'Info-mode-hook (lambda ()
                              (define-key Info-mode-map  (kbd "M-n") 'Info-search-next)
                              (define-key Info-mode-map (kbd "M-p") 'fff-Info-search-previous))))

(use-package doom-modeline :ensure t :defer t
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
  (display-time)
  :init
  (doom-modeline-mode +1))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (add-to-list #'yas-snippet-dirs (expand-file-name "snippets/" user-emacs-directory))
  (yas-reload-all))

(use-package annalist :ensure t)

(use-package flimenu :ensure t
  :config
    (flimenu-global-mode))

(use-package evil-collection
  :ensure nil
  :load-path (lambda () (expand-file-name "evil-collection" user-emacs-directory))
  :after (annalist evil)
  :config
  (require 'annalist)
  (evil-collection-init))

(use-package evil-leader :defer t
  :commands (evil-leader-mode)
  :ensure t
  :init
  (global-evil-leader-mode)
  :config
  (progn

    (fset 'fff-C-x-C-e
          (kmacro-lambda-form [?\C-x ?\C-e] 0 "%d"))

    (evil-leader/set-leader "<SPC>")

    ;; single key
    (evil-leader/set-key "SPC" 'execute-extended-command)
    (evil-leader/set-key "<escape> <escape> <escape>" 'keyboard-escape-quit)
    (evil-leader/set-key "RET" 'crux-open-with)
    (evil-leader/set-key ";" 'eval-expression)
    (evil-leader/set-key "d" 'delete-blank-lines)
    (evil-leader/set-key "e" 'fff-C-x-C-e)
    (evil-leader/set-key "k" 'fff-hydra-expand-region/er/expand-region)
    (evil-leader/set-key "o" 'other-window)
    (evil-leader/set-key "q" 'fff-delete-window-and-bury-buffer)
    (evil-leader/set-key "w" 'save-buffer)

    ;; text scaling
    (evil-leader/set-key "0" 'fff-set-scale-to-zero)
    (evil-leader/set-key "=" 'fff-hydra-zoom/text-scale-increase)
    (evil-leader/set-key "-" 'fff-hydra-zoom/text-scale-decrease)

    ; paragraph navigation
    (evil-leader/set-key "[" 'fff-hydra-paragraph-movement/evil-backward-paragraph)
    (evil-leader/set-key "]" 'fff-hydra-paragraph-movement/evil-forward-paragraph)

    ;; search and replace
    (evil-leader/set-key "a" 'avy-goto-char)
    (evil-leader/set-key "r" 'fff-evil-regex-search)

    ;; narrow
    (evil-leader/set-key "n n" 'narrow-to-region)
    (evil-leader/set-key "n N" 'widen)

    ;; magit
    (evil-leader/set-key "m m" 'magit)

    ;; window size adjustment
    (evil-leader/set-key "H" 'fff-hydra-windsize/windsize-left)
    (evil-leader/set-key "L" 'fff-hydra-windsize/windsize-right)
    (evil-leader/set-key "J" 'fff-hydra-windsize/windsize-down)
    (evil-leader/set-key "K" 'fff-hydra-windsize/windsize-up)

    ;; f: shortcut to file or dired buffer
    (evil-leader/set-key "f b" 'fff-access-bookmarks)
    (evil-leader/set-key "f B" 'fff-access-books)

    ;; full screen
    (evil-leader/set-key "f s" 'toggle-frame-fullscreen)

    ;; switch to scratch
    (evil-leader/set-key "i i" 'fff-switch-to-scratch-buffer)
    (evil-leader/set-key "i I" 'fff-switch-to-new-scratch-buffer)

    ;; imenu
    (evil-leader/set-key "i m" 'imenu)

    ;; terminal
    (evil-leader/set-key "t t" 'fff-switch-or-create-vterm)
    (evil-leader/set-key "t T" 'fff-open-new-vterm)
    (evil-leader/set-key "t p" 'terminal-here)

    ;; chatgpt
    (evil-leader/set-key "g g" 'fff-switch-or-create-gptel)

    ;; shell/terminal
    (evil-leader/set-key "s s" 'fff-switch-or-create-vterm)
    (evil-leader/set-key "s S" 'fff-open-new-vterm)
    (evil-leader/set-key "s t" 'terminal-here)

    ;; tabs
    (evil-leader/set-key (kbd "t n") 'fff-tab-bar-new-tab)
    (evil-leader/set-key (kbd "t w") 'tab-bar-close-tab)
    (evil-leader/set-key (kbd "t r") 'tab-bar-rename-tab)
    (evil-leader/set-key (kbd "h h") 'fff-tabs/tab-previous)
    (evil-leader/set-key (kbd "l l") 'fff-tabs/tab-next)
    (evil-leader/set-key (kbd "t k") 'tab-bar-select-tab-by-name)

    ;; x: C-x prefixes
    (evil-leader/set-key "x b" 'switch-to-buffer)
    (evil-leader/set-key "x 0" 'delete-window)
    (evil-leader/set-key "x 1" 'delete-other-windows)
    (evil-leader/set-key "x 2" 'fff-split-and-follow-horizontally)
    (evil-leader/set-key "x 3" 'fff-split-and-follow-vertically)
    (evil-leader/set-key "x 4 4" 'other-window-prefix)
    (evil-leader/set-key "x 4 1" 'same-window-prefix)
    (evil-leader/set-key "x o" 'other-window)
    (evil-leader/set-key "x k" 'bury-buffer)
    (evil-leader/set-key "x K" 'kill-buffer)
    (evil-leader/set-key "x d" 'make-directory)
    (evil-leader/set-key "x f" 'fff-find-file)
    (evil-leader/set-key "x r" 'crux-recentf-find-file)
    (evil-leader/set-key "x w" 'write-file)
    (evil-leader/set-key "x SPC b" 'list-buffers)
    (evil-leader/set-key "X C" 'save-buffers-kill-terminal)

    ;; access dirs
    (evil-leader/set-key "x c" 'fff-access-config-dir)
    (evil-leader/set-key "x m" 'fff-access-home-dir)
    (evil-leader/set-key "x n" 'fff-open-file-in-notes)
    (evil-leader/set-key "x p" 'fff-open-file-in-projects)
    (evil-leader/set-key "p p" 'fff-find-file-in-project-root)
    (evil-leader/set-key "x s" 'fff-find-file-ssh)
    (evil-leader/set-key "x t" 'fff-open-file-in-tmp)
    (evil-leader/set-key "x y" 'fff-open-file-in-snippets)
    (evil-leader/set-key "x /" 'fff-open-file-in-root-dir)

    ;; project root
    (evil-leader/set-key "h k" 'fff-find-file-in-project-root)
    (evil-leader/set-key "p r" 'fff-find-file-in-project-root)

    ;; winner undo/redo and previous buffer
    (evil-leader/set-key "u u" 'fff-winner/winner-undo)
    (evil-leader/set-key "j j" 'evil-switch-to-windows-last-buffer)

    ;; previous/next buffer
    (evil-leader/set-key "x h" 'fff-buffer-switch/previous-buffer)
    (evil-leader/set-key "x l" 'fff-buffer-switch/next-buffer)

    ;; run/debug bindings for projects
    (evil-leader/set-key "c r" 'rustic-cargo-run)
    (evil-leader/set-key "c c" 'quickrun)

    ;; fff-bind
    (evil-leader/set-key "b k p" 'fff-assign-key-to-position-leader)
    (evil-leader/set-key "b k b" 'fff-assign-key-to-buffer-leader)
    (evil-leader/set-key "b k d" 'fff-assign-key-to-dir-leader)))

(use-package evil :defer t :ensure t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-fine-undo t)
  (setq evil-search-wrap nil)
  (setq evil-kill-on-visual-paste nil)

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
    (evil-mode +1)

    (define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)
    (define-key evil-visual-state-map (kbd "C-a") 'evil-first-non-blank)
    (define-key evil-visual-state-map (kbd "<backpace>") 'delete-char)
    (define-key evil-visual-state-map (kbd "C-/") 'fff-comment)
    (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)

    (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
    (define-key evil-insert-state-map (kbd "C-a") 'evil-first-non-blank)
    (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
    (define-key evil-insert-state-map (kbd "M-w") 'easy-kill)
    (define-key evil-insert-state-map (kbd "C-y") 'yank)
    (define-key evil-insert-state-map (kbd "M-y") 'yank-pop)
    (define-key evil-insert-state-map (kbd "C-'") 'hippie-expand)
    (define-key evil-insert-state-map (kbd "M-a") 'yas-insert-snippet)
    (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
    (define-key evil-insert-state-map (kbd "C-/") 'fff-comment)

    (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
    (define-key evil-normal-state-map (kbd "C-a") 'evil-first-non-blank)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-o") 'evil-jump-backward)
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
    (define-key evil-normal-state-map (kbd "o") 'fff-evil-open-below)
    (define-key evil-normal-state-map (kbd "O") 'fff-evil-open-above)
    (define-key evil-normal-state-map (kbd "C-/") 'fff-comment)))

(use-package hide-comnt :defer nil :ensure nil
  :after evil
  :init
  (load (expand-file-name "hide-comnt.el" user-emacs-directory))
  (load (expand-file-name "fff-functions.el" user-emacs-directory)))

(use-package undo-fu :defer t :ensure t)

(use-package evil-surround :ensure t
  :config
  (global-evil-surround-mode +1))

(use-package evil-numbers :defer t :ensure t)

(use-package expand-region :defer t :ensure t)

(use-package lisp-mode :ensure nil
  :init
  (set-default 'auto-mode-alist
               (append '(("\\.lisp$" . lisp-mode)
                         ("\\.lsp$" . lisp-mode)
                         ("\\.cl$" . lisp-mode))
                       auto-mode-alist)))

(use-package sly :defer t :ensure t
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

(use-package terminal-here :defer t :ensure t
  :init
  (setq terminal-here-linux-terminal-command 'st))

(use-package so-long :defer t :ensure t
  :init
  (global-so-long-mode +1))

(use-package lorem-ipsum :defer t :ensure t)

(use-package hydra :defer t :ensure t :commands defhydra
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
    ( "l" next-buffer))

  (defhydra fff-winner (:color red :pre (setq hydra-is-helpful nil) :after-exit (setq hydra-is-helpful t))
    ("u" winner-undo)
    ("U" winner-redo))

  )

(use-package company :defer t :ensure t
  :init
  (setq company-format-margin-function nil)
  (setq company-idle-delay 0.2)
  (setq company-tooltip-limit 2)
  (global-company-mode))

(use-package restart-emacs :defer t :ensure t)

(use-package windsize :defer t :ensure t)

(use-package crux :defer t :ensure t)

(use-package emmet-mode :defer t :ensure t
  :init (add-hook 'sgml-mode-hook 'emmet-mode))

(use-package markdown-mode :defer t :ensure nil
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  (add-hook 'markdown-mode-hook #'visual-line-mode))

(use-package mw-thesaurus :defer t :ensure t)

(use-package sicp :defer t :ensure t)

(use-package gh-md :ensure t :defer t)

(use-package go-mode :ensure t :defer t)

(use-package vertico :defer t :ensure t
  :init
  (setq enable-recursive-minibuffers t)
  :config
  (vertico-mode +1)
  (define-key vertico-map (kbd "C-c d") 'vertico-exit-input))

(use-package vertico-prescient :ensure t
  :config
  (setq prescient-filter-method  '(literal regexp initialism))
  (vertico-prescient-mode +1))

(use-package savehist
  :init
  (savehist-mode))

(use-package projectile :defer t :ensure t
  :config
  (dolist (file '(".venv/" "venv/" "manage.py" ".git/" "go.mod" "package.json" "Cargo.toml" "build.sh"))
    (add-to-list 'projectile-project-root-files file))
  :bind*
  (("C-c k" . projectile-find-file))
  :init
  ;; (setq projectile-project-root-files '("manage.py" ".git/" "go.mod"))
  (setq projectile-ignored-projects '("~/"))
  (projectile-mode +1)
  (with-eval-after-load 'projectile
    (define-key projectile-command-map (kbd "C-c p") nil)
    (define-key projectile-command-map (kbd "C-c P") nil)))

(use-package marginalia :defer t :ensure t
  :init
  (marginalia-mode))

(use-package emojify :ensure t :defer t)

(use-package dired :defer t :ensure nil
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode))))

(use-package switch-window :ensure t :defer t)

(use-package rainbow-mode :ensure t :defer t)

(use-package vimrc-mode :ensure t :defer t)

(use-package org-bullets :ensure t :defer t
  :init
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package emmet-mode :ensure t :defer t
  :init
  (require 'emmet-mode)
  (add-hook 'html-mode-hook (lambda () (emmet-mode 1))))

(use-package auto-package-update :ensure t :defer t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 30)
  (auto-package-update-maybe))

(use-package smex :ensure t)

(use-package git-gutter :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package hl-todo :ensure t :defer t
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)))

(use-package saveplace :init (save-place-mode))

(use-package racket-mode :ensure t :defer t
  :mode "\\.rkt\\'"
  :config
  (require 'racket-xp)
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  (add-hook 'racket-mode-hook #'prettify-symbols-mode)
  (defun setup-racket-eldoc ()
    (eldoc-mode +1)
    (setq eldoc-documentation-function #'racket-xp-eldoc-function))
  (add-hook 'racket-mode-hook #'setup-racket-eldoc)
  (define-key racket-mode-map (kbd "C-j") 'racket-run)
  (define-key racket-mode-map (kbd "C-<return>") 'racket-run)
  (define-key racket-mode-map (kbd "C-c C-c") 'racket-run))

(use-package quickrun :ensure t :defer t)

(use-package winner :ensure t :defer t
  :init (winner-mode +1))

(use-package haskell-mode :ensure t :defer t)

(use-package helpful :ensure t :defer t
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-callable))

(use-package volatile-highlights :ensure t :defer t
  :init
  (volatile-highlights-mode t)
  :config
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil))

(use-package typescript-mode :ensure t :defer t)

(use-package rust-mode :ensure t :defer t)

(use-package rustic :ensure t :defer t)

(use-package lsp-mode :ensure t :defer t
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-keymap-prefix "C-c l")
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-enable-file-watchers nil) ; Disable file watchers for better performance
  (setq lsp-enable-symbol-highlighting nil) ; disable symbol highlighting
  (setq lsp-headerline-breadcrumb-enable nil) ; Disable breadcrumbs in the headerline
  (setq lsp-completion-show-kind nil)
  (setq lsp-completion-show-detail nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-lens-enable nil)
  (lsp-inlay-hints-mode) ; the type hints next to arguments in func signature lines and variable definitions.
  (setq lsp-inlay-hint-enable t)
  (setq lsp-rust-analyzer-display-parameter-hints t))

(use-package lsp-python-ms :ensure t :defer t)

(use-package lsp-haskell :ensure t :defer t
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

(use-package lsp-metals
  :ensure t
  :custom
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"
                            "-J-Dmetals.icons=unicode"))
  (lsp-metals-enable-semantic-highlighting t))

(use-package dired-hide-dotfiles :ensure t :defer t)

(use-package macrostep :ensure t :defer t)

(use-package nov :ensure t :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package embark :ensure t :defer t
  :bind*
  (("C-c e" . embark-act)
   ("C-h b" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package diminish :ensure t :defer t)

(use-package emms :ensure t :defer t
  :diminish emms-mode-line
  :config
  (setq emms-mode-line-format "")
  (setq emms-mode-line-icon-enabled-p nil)
  (setq emms-playing-time-display-format "")
  :init
  (emms-all)
  (emms-default-players))

(use-package circe :ensure t :defer t)

(use-package avy :ensure t :defer t)

(use-package pdf-tools :ensure t  :defer t
  :mode ("\\pdf\\'" . pdf-view-mode)
  :init
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (define-key pdf-view-mode-map (kbd "<tab>") 'pdf-outline)
                                  (pdf-view-themed-minor-mode)))
  :config
  (pdf-tools-install :no-query))

(use-package vterm :ensure t :defer t
  :config
  (define-key vterm-mode-map (kbd "C-c c") 'vterm-clear))

(use-package org  :ensure nil :defer t
  :init
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
     (ruby . t)
     (C . t)
     (js . t))))

(use-package magit :ensure t :defer t
  :init
  (setq magit-section-initial-visibility-alist
        '(([hunk file staged status] . hide)
          ([file unstaged status] . show)
          ([hunk file unstaged status] . hide))))

(use-package git-timemachine :ensure t :defer t)

(use-package clojure-mode :ensure t :defer t)

(use-package cider :ensure t :defer t
  :config
  (define-key cider-repl-mode-map (kbd "C-c c") #'cider-repl-clear-buffer))

(use-package consult :ensure t :defer t)

(use-package embark-consult :ensure t :defer t)

(use-package pyvenv :ensure t :defer t)

(use-package keycast :ensure t :defer t)

(use-package org-download :ensure t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package v-mode
  :ensure t
  :mode ("\\.v\\'" . v-mode)
  :init
  (add-hook 'v-mode-hook
          (lambda () (add-to-list 'imenu-generic-expression ;;
                                 '("comment header" "^////\\(.*\\)$" 1))
            (imenu-add-to-menubar "Index"))))

(use-package evil-visualstar :ensure t :defer nil
  :config
  (global-evil-visualstar-mode))

(use-package evil-matchit :ensure t :defer nil
  :config
  (global-evil-matchit-mode +1))

(use-package zig-mode :ensure t :defer t)

(use-package all-the-icons :ensure t
  :if (display-graphic-p))

(use-package fff-key-set-mode :ensure nil :defer nil
  :config
  (load-file (expand-file-name "fff-key-set-mode.el" user-emacs-directory))
  (fff-key-set-mode 1))

(use-package dtrt-indent :ensure t :defer nil
  :config
  (setq dtrt-indent-min-offset 1)
  (setq dtrt-indent-min-quality 55)
  (setq dtrt-indent-min-relevant-lines 1)
  (setq dtrt-indent-min-soft-tab-superiority 105)
  (setq dtrt-indent-min-matching-indentations 1)
  (dtrt-indent-global-mode +1))

(use-package evil-iedit-state :ensure t :defer t
  :init
  (global-set-key (kbd "C-;") 'iedit-mode))

(use-package scala-mode :ensure t :defer t
  :interpreter
    ("scala" . scala-mode))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package devdocs :ensure t)

(use-package projectile-ripgrep :ensure t)

(use-package dockerfile-mode :ensure t)

(use-package json-mode :ensure t)

(use-package deadgrep :ensure t)

(use-package sly-quicklisp :ensure t)

(use-package aggressive-indent :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package wgrep :ensure t :defer t)

(use-package gptel
  :ensure t
  :init
  (setq gptel-api-key (string-trim (with-temp-buffer (insert-file-contents "~/.chat_gpt_api_key") (buffer-string))))
  :config
  ;; Set up the OpenAI backend
  (gptel-make-gpt4all "GPT4All"           ;Name of your choosing
 :protocol "http"
 :host "localhost:4891"                 ;Where it's running
 :models '("mistral-7b-openorca.Q4_0.gguf")) ; Available models
)
