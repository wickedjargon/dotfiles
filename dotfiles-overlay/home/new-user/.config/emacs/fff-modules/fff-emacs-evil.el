;;; fff-emacs-evil.el --- evil and friends, hydras -*- lexical-binding: t; -*-

;;; Commentary:

;; Extracted from init.el.  Loaded via `fff-require' in init.el, so an
;; error in this file is reported but does not stop the rest of init.

;;; Code:

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
  (evil-leader/set-key "c p" 'fff-project-find-file)

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

  ;; left / right new window prefix and move current window
  (evil-leader/set-key "x h" 'fff-other-window-prefix-left)
  (evil-leader/set-key "x l" 'fff-other-window-prefix-right)
  (evil-leader/set-key "x H" 'fff-move-current-window-left)
  (evil-leader/set-key "x L" 'fff-move-current-window-right)

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
  (evil-leader/set-key "f f" 'fff-transient-find)
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

  ;; claude code
  (evil-leader/set-key "g g" 'agent-shell-anthropic-start-claude-code)

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
  ;; evil-want-* options live in the `use-package emacs' block at the top:
  ;; evil is already loaded (by evil-leader) when this :init runs.
  (setq evil-regexp-search nil)
  (setq evil-insert-state-message nil)
  (setq evil-search-wrap nil)
  (setq evil-kill-on-visual-paste nil)

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

(provide 'fff-emacs-evil)
