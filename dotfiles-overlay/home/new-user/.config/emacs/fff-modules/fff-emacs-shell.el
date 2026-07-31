;;; fff-emacs-shell.el --- terminals and shells -*- lexical-binding: t; -*-

;;; Commentary:

;; Extracted from init.el.  Loaded via `fff-require' in init.el, so an
;; error in this file is reported but does not stop the rest of init.

;;; Code:

;;; Terminal / Shell

(use-package terminal-here
  :straight t
  :defer t
  :init
  (setq terminal-here-linux-terminal-command 'alacritty)
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

(use-package vterm :straight t :defer t
  :hook (vterm-mode . fff-vterm-clear-binding)
  :config
  (defun fff-vterm-clear ()
    "Clear vterm buffer and scrollback."
    (interactive)
    (vterm-clear-scrollback)
    (vterm-clear))
  (defun fff-vterm-clear-binding ()
    "Bind C-c c to fff-vterm-clear in vterm."
    (local-set-key (kbd "C-c c") #'fff-vterm-clear)))

(provide 'fff-emacs-shell)
