;; compatible with emacs version 28 and above
;; DONE: set .emacs.d/elpa to read only
;; TODO: replace string paths with expressions

;; Hydras
;; TODO: hydra for tab-bar-mode
;; TODO: hydra for find-file
;; TODO: completion (company, hippie, etc)

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
;; use-package setup ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :ensure nil
  :config
  ;; remove underlines for errors and warnings
  (custom-set-faces
   '(flymake-error ((t (:inherit nil))))
   '(flymake-note ((t (:inherit nil))))
   '(flymake-warning ((t (:inherit nil)))))

  ;; hooks
  (add-hook 'modus-themes-after-load-theme-hook #'pdf-view-themed-minor-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'dired-mode-hook #'auto-revert-mode)          ;; revert dired buffers, but not buffer list buffers
  (add-hook 'prog-mode-hook #'hs-minor-mode)              ;; let me toggle shrink and expansion of code blocks 
  (add-hook 'emacs-lisp-mode-hook
			(lambda () (setq-local prettify-symbols-alist '(("lambda" . ?\λ) ("interactive" . ?\𝑖)))))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))
(add-hook 'comint-mode-hook (lambda ()
                              (define-key comint-mode-map (kbd "C-p") 'comint-previous-input)
                              (define-key comint-mode-map (kbd "C-n") 'comint-next-input)))
(add-hook 'find-file-hook (lambda ()
							(when (and buffer-file-name
									   (string-prefix-p (expand-file-name "elpa" user-emacs-directory) buffer-file-name))
							  (read-only-mode 1))))

(defun fff-advice-for-window-focus (orig-fun &rest args)
  "Advice function to focus on the new window after running the specified function."
  (let ((current-window (selected-window)))
    (apply orig-fun args)
    (select-window (next-window current-window))))

(advice-add 'diff-buffer-with-file :around #'fff-advice-for-window-focus)
(advice-add 'vc-region-history :around #'fff-advice-for-window-focus)
(advice-add 'list-buffers :around #'fff-advice-for-window-focus)

(require-theme 'modus-themes)
(load-theme 'modus-vivendi)

:init
;; key bindings
(global-set-key (kbd "M-u") 'universal-argument)
(global-set-key (kbd "C-x k") 'fff-kill-this-buffer)
(global-unset-key (kbd "C-x C-c"))                      ;; I accidently hit this sometimes
(global-set-key (kbd "C-c c")  'fff-clear-shell)
(define-key global-map (kbd "C-c t") #'modus-themes-toggle)
;; (global-set-key (kbd "C-x C-b") 'fff-buffer-list-switch)
(global-set-key (kbd "C-x 3") 'fff-split-and-follow-vertically)
(global-set-key (kbd "C-x 2") 'fff-split-and-follow-horizontally)

(setq tab-bar-new-tab-to 'rightmost)
(setq tab-bar-new-tab-choice 'empty-buffer)
(tab-bar-mode +1)
(global-set-key (kbd "C-c w") 'tab-bar-close-tab)
(global-set-key (kbd "C-c n") 'fff-tab-bar-new-tab)
(global-set-key (kbd "C-c r") 'tab-bar-rename-tab)

(if (string= (system-name) "x1c")
    (set-face-attribute 'default nil :height 109)
  (set-face-attribute 'default nil :height 95))


;; general settings
(setq evil-undo-system 'undo-fu)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(tool-bar-mode -1)                                      ;; no tool bar
(scroll-bar-mode -1)                                    ;; no scroll bar
(setq inhibit-startup-message t)                        ;; no splash screen
(setq use-short-answers t)                              ;; just type `y`, not `yes`
(setq mode-require-final-newline nil)                   ;; don't add a newline at the bottom of the file
(setq delete-old-versions t)
(setq auto-save-file-name-transforms
	  `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))
(setq backup-directory-alist
	  `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
(blink-cursor-mode -1)                                  ;; don't blink my cursor
(global-auto-revert-mode +1)                            ;; auto revert files and buffers
(global-goto-address-mode +1)                           ;; make links/urls clickable
(delete-selection-mode +1)                              ;; delete selction when hitting backspace on region
(set-default 'truncate-lines t)                         ;; don't wrap my text
(setq custom-file (locate-user-emacs-file "custom.el")) ;; separate custom.el file
(when (file-exists-p custom-file) (load custom-file))   ;; when it exists, load it
(setq initial-scratch-message "")                       ;; no message on scratch buffer
(setq auth-source-save-behavior nil)                    ;; don't prompt to save auth info in home dir
(setq-default tab-width 4)                              ;; I prefer a tab length of 4, not 8
(setq dired-listing-switches                            ;; I prefer to have dired
      "-aBhlh  --group-directories-first")              ;; group my directories and display size
(setq disabled-command-function nil)                    ;; enable all disabled commands
(setq ring-bell-function 'ignore)                       ;; don't ring my bell
(setq sentence-end-double-space nil)                    ;; sentence ends with one space, not two
;; (electric-pair-mode +1)
;; (setq electric-pair-delete-adjacent-pairs nil)
(global-eldoc-mode -1)
(display-battery-mode +1)
(setq display-time-day-and-date t)
(display-time)
(setq frame-resize-pixelwise t)                         ;; cover the whole screen when maximized
(global-prettify-symbols-mode +1)
(setq help-window-select t)  ; Switch to help buffers automatically

;; haskell path
(setq exec-path (append '("/home/ff/.ghcup/bin") exec-path))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "/home/ff/.ghcup/bin")))

;; go path
(setq exec-path (append '("/usr/local/go/bin") exec-path))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "/usr/local/go/bin")))
(setq exec-path (append '("/home/ff/go/bin") exec-path))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "/home/ff/go/bin")))
(setenv "GOPATH" "/home/ff/go")

;; rust path
(setq exec-path (append '("/home/ff/.cargo/env") exec-path))
(setq exec-path (append '("/home/ff/.cargo/bin") exec-path))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "/home/ff/.cargo/bin")))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "/home/ff/.cargo/env")))

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
:init
;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(use-package asm-mode :ensure nil :defer t
  :init
  (setq asm-comment-char ?\#))

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
  (setq doom-modeline-hud t) ;; indicates the position in the modeline
  (setq doom-modeline-highlight-modified-buffer-name nil) 
  (setq doom-modeline-position-line-format '(""))
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-percent-position '(""))
  (setq doom-modeline-modal nil)
  :init
  (doom-modeline-mode +1))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (add-to-list #'yas-snippet-dirs (expand-file-name "snippets/" user-emacs-directory))
  (yas-reload-all))

(use-package annalist :ensure t)

(use-package evil-collection :ensure nil
  :load-path "~/.emacs.d/evil-collection"
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
    (evil-leader/set-key "SPC" 'execute-extended-command)
    (evil-leader/set-key "<tab>" nil)
    (evil-leader/set-key "<escape> <escape> <escape>" 'keyboard-escape-quit)
    (evil-leader/set-key ";" 'eval-expression)
    (evil-leader/set-key "=" 'fff-hydra-zoom/text-scale-increase)
    (evil-leader/set-key "-" 'fff-hydra-zoom/text-scale-decrease)
    (evil-leader/set-key "[" 'fff-hydra-paragraph-movement/evil-backward-paragraph)
    (evil-leader/set-key "]" 'fff-hydra-paragraph-movement/evil-forward-paragraph)
    (evil-leader/set-key "0" 'fff-set-scale-to-zero)
    (evil-leader/set-key "a" 'avy-goto-char)
    (evil-leader/set-key "b" 'switch-to-buffer)
    (evil-leader/set-key "c" 'quickrun)
    (evil-leader/set-key "d" 'delete-blank-lines)
    (evil-leader/set-key "e" 'fff-C-x-C-e)

    ;; f: shortcut to file or dired buffer prefix
    (evil-leader/set-key "f b" 'fff-access-bookmarks)
    (evil-leader/set-key "f B" 'fff-access-books)
    (evil-leader/set-key "f f" 'fff-access-sched)
    (evil-leader/set-key "f i" 'fff-switch-to-scratch-buffer)
    (evil-leader/set-key "f t" 'shell)

	(evil-leader/set-key "h w" 'tab-bar-close-tab)
	(evil-leader/set-key "h n" 'fff-tab-bar-new-tab)
	(evil-leader/set-key  "h r" 'tab-bar-rename-tab)
	
	;; window size adjustment
    (evil-leader/set-key "H" 'fff-hydra-windsize/windsize-left)
    (evil-leader/set-key "L" 'fff-hydra-windsize/windsize-right)
    (evil-leader/set-key "J" 'fff-hydra-windsize/windsize-down)
    (evil-leader/set-key "K" 'fff-hydra-windsize/windsize-up)
    (evil-leader/set-key "I" 'fff-switch-to-scratch-buffer-text-mode)

    (evil-leader/set-key "j" 'evil-switch-to-windows-last-buffer)
    (evil-leader/set-key "k" 'fff-hydra-expand-region/er/expand-region)

    (evil-leader/set-key "p" 'fff-programming/body)
    (evil-leader/set-key "<DEL>" 'fff-shutdown/body)
    (evil-leader/set-key "C-<backspace>" 'fff-delete-till-beginning)
	
	;; put under hydra
    (evil-leader/set-key "n" 'narrow-to-region)
    (evil-leader/set-key "N" 'widen)
	
    (evil-leader/set-key "o" 'other-window)
    (evil-leader/set-key "P" 'crux-open-with)
    (evil-leader/set-key "Q" 'kill-buffer-and-window)
    (evil-leader/set-key "q" 'delete-window)

    (evil-leader/set-key "r" 'fff-evil-regex-search)
	
    (evil-leader/set-key "s" 'fff-tabs/tab-next)

	;; learn more about cursor undo position and put this under a hydra
	(evil-leader/set-key "t" 'vterm)
	(evil-leader/set-key "T" 'terminal-here)
    (evil-leader/set-key "u" 'universal-argument)

    (evil-leader/set-key "w" 'save-buffer)
	
    ;; x: C-x prefixes
    (evil-leader/set-key "x b" 'switch-to-buffer)
    (evil-leader/set-key "x 0" 'delete-window)
    (evil-leader/set-key "x 1" 'delete-other-windows)
    (evil-leader/set-key "x 2" 'fff-split-and-follow-horizontally)
    (evil-leader/set-key "x 3" 'fff-split-and-follow-vertically)
    (evil-leader/set-key "x o" 'other-window)
    (evil-leader/set-key "x k" 'fff-kill-this-buffer)
    (evil-leader/set-key "x t" 'fff-open-file-in-tmp)
    (evil-leader/set-key "x d" 'make-directory)
    (evil-leader/set-key "x f" 'find-file)
    (evil-leader/set-key "x h" 'fff-access-home-dir)
    (evil-leader/set-key "x c" 'fff-access-config-dir)
    (evil-leader/set-key "x c" 'fff-access-config-dir)
    (evil-leader/set-key "x p" 'fff-open-file-in-projects)
    (evil-leader/set-key "x r" 'crux-recentf-find-file)
    (evil-leader/set-key "x n" 'fff-open-file-in-notes)
    (evil-leader/set-key "x w" 'write-file)
    (evil-leader/set-key "x y" 'fff-open-file-in-snippets)
    (evil-leader/set-key "x SPC b" 'list-buffers)
    (evil-leader/set-key "X C" 'save-buffers-kill-terminal)
    (evil-leader/set-key "x x" 'fff-buffer-switch/body)))

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
	
    (define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)
    (define-key evil-visual-state-map (kbd "C-a") 'evil-first-non-blank)
    (setq evil-undo-system 'undo-fu)
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-fine-undo t)
    (setq evil-search-wrap nil)
    (setq evil-kill-on-visual-paste nil)
    (evil-mode +1)
    (define-key evil-visual-state-map (kbd "<backpace>") 'delete-char)
    (define-key evil-visual-state-map (kbd "C-/") 'comment-line)
    (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
    
    (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
    (define-key evil-insert-state-map (kbd "C-a") 'evil-first-non-blank)
    (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
    (define-key evil-insert-state-map (kbd "M-w") 'easy-kill)
    (define-key evil-insert-state-map (kbd "C-y") 'yank)
    (define-key evil-insert-state-map (kbd "M-y") 'yank-pop)
    (define-key evil-insert-state-map (kbd "C-;") 'hippie-expand)
    (define-key evil-insert-state-map (kbd "C-'") 'company-complete)
    (define-key evil-insert-state-map (kbd "C-l") 'yas-expand)
    (define-key evil-insert-state-map (kbd "M-a") 'yas-insert-snippet)
    (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
    (define-key evil-insert-state-map (kbd "C-/") 'comment-line)
	
    (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
    (define-key evil-normal-state-map (kbd "C-a") 'evil-first-non-blank)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-o") 'evil-jump-backward)
    (define-key evil-normal-state-map (kbd "M-o") 'evil-jump-forward)
    (define-key evil-normal-state-map (kbd "gp") 'fff-evil-paste-and-indent-after)
    (define-key evil-normal-state-map (kbd "gP") 'fff-evil-paste-and-indent-before)
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "C-/") 'comment-line)
    (define-key evil-normal-state-map (kbd "C-c a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-c x") 'evil-numbers/dec-at-pt)
    (define-key evil-normal-state-map (kbd "C-c g a") 'evil-numbers/inc-at-pt-incremental)
    (define-key evil-normal-state-map (kbd "C-c g x") 'evil-numbers/dec-at-pt-incremental)
    (define-key evil-normal-state-map (kbd "q") 'quit-window)
    (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)
    (define-key evil-normal-state-map (kbd "ZZ") 'fff-save-and-kill-buffer)
    (define-key evil-normal-state-map (kbd "o") 'fff-evil-open-below)
    (define-key evil-normal-state-map (kbd "O") 'fff-evil-open-above)
    (define-key evil-normal-state-map (kbd "C-/") 'comment-line)

    (load (expand-file-name "fff-functions.el" user-emacs-directory))))

(use-package undo-fu :defer t :ensure t)

(use-package evil-surround :ensure t
  :config
  (global-evil-surround-mode +1))

(use-package evil-numbers :defer t :ensure t)

(use-package expand-region :defer t :ensure t)

(use-package sly :defer t :ensure t
  :init
  (add-hook 'sly-mrepl-mode-hook (lambda ()
                                   (define-key sly-mrepl-mode-map (kbd "C-p") 'comint-previous-input)
                                   (define-key sly-mrepl-mode-map (kbd "C-n") 'comint-next-input)))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (define-key lisp-mode-map (kbd "C-j") 'sly-eval-print-last-expression)
  (define-key lisp-mode-map (kbd "C-<return>") 'sly-eval-print-last-expression))

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
	("s" tab-next)
	("S" tab-previous))
  
  (defhydra fff-buffer-switch (:color red :pre (setq hydra-is-helpful nil) :after-exit (setq hydra-is-helpful t))
	("h" evil-prev-buffer)
	("l" evil-next-buffer)))

(use-package pretty-hydra :ensure t :defer nil
  :config

  (pretty-hydra-define fff-programming (:foreign-keys warn :quit-key "q" :color blue)
	("Programming"
     (("l" lsp "lsp")
      ("r" lsp-rename "lsp-rename")
      ("d" lsp-describe-thing-at-point "lsp-describe-thing-at-point")
	  ("D" sly-documentation "sly-documentation")
      ("f" lsp-format-buffer "lsp-format-buffer")
      ("i" imenu "imenu")
      ("f" flymake-show-diagnostics-buffer "flymake-show-diagnostics-buffer")
      ("r" fff-display-lsp-root "fff-display-lsp-root")
	  )))
  
  (pretty-hydra-define fff-shutdown (:foreign-keys warn :quit-key "q" :color blue)
	("System Management"
	 (("l" (shell-command "slock") "Lock screen" :exit t)
	  ("K" (shell-command "kill -TERM $(pidof dwm)") "Kill dwm" :exit t)
	  ("R" (shell-command "kill -TERM $(pidof dwm) && systemctl reboot") "Reboot" :exit t)
	  ("r" (restart-emacs) "Restart Emacs" :exit t)
	  ("S" (shell-command "kill -TERM $(pidof dwm) && systemctl poweroff") "Shutdown" :exit t)
	  ("o" (shell-command "xset dpms force off") "Turn off screen")))))

(use-package company :defer t :ensure t
  :init
  (setq company-format-margin-function nil)
  (setq company-idle-delay 0)
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
  :bind*
  (("C-c k" . projectile-find-file))
  :init
  (setq projectile-project-root-files '("manage.py" ".git/" "go.mod"))
  (setq projectile-ignored-projects '("~/"))
  (projectile-mode +1)
  (with-eval-after-load 'projectile
	(define-key projectile-command-map (kbd "C-c p") nil)
	(define-key projectile-command-map (kbd "C-c P") nil)))

(use-package marginalia :defer t :ensure t
  :init
  (marginalia-mode))

(use-package emojify :defer t :ensure t
  :init
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode)))

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
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

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
  ;; ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-enable-file-watchers nil) ; Disable file watchers for better performance
  (setq lsp-enable-symbol-highlighting nil) ; disable symbol highlighting
  (setq lsp-headerline-breadcrumb-enable nil) ; Disable breadcrumbs in the headerline
  (setq lsp-completion-show-kind nil)
  (setq lsp-completion-show-detail nil)
  (setq lsp-lens-enable nil)
  (setq lsp-signature-auto-activate nil))

(use-package lsp-python-ms :ensure t :defer t)

(use-package lsp-haskell :ensure t :defer t
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

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
  (setq org-confirm-babel-evaluate nil)
  (setq org-startup-with-inline-images t)
  (setq org-babel-lisp-eval-command "sbcl --script")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python .t)
	 (haskell . t)
	 (lisp . t)
	 (ruby . t)
	 (js . t))))

(use-package magit :ensure t :defer t
  :init
  (setq magit-section-initial-visibility-alist
		'((modified . hide)
		  (unstaged . hide)
          (staged . hide)
          (stash . hide)
          (unpushed . hide)
		  ([hunk file unstaged status] . hide)
          (untracked . hide)))
  )

(use-package git-timemachine :ensure t :defer t)

(use-package clojure-mode :ensure t :defer t)

(use-package cider :ensure t :defer t)

(use-package consult :ensure t :defer t)

(use-package embark-consult :ensure t :defer t)

(use-package pyvenv :ensure t :defer t)