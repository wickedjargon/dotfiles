;;; fff-emacs-tools.el --- tools and entertainment -*- lexical-binding: t; -*-

;;; Commentary:

;; Extracted from init.el.  Loaded via `fff-require' in init.el, so an
;; error in this file is reported but does not stop the rest of init.

;;; Code:

;;; Tools

(use-package tmr :straight t :defer t)

(use-package read-aloud :defer t :straight t)

(use-package gif-screencast
  :defer t
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
  (erc-hide-list '("JOIN" "PART" "QUIT" "MODE" "NICK" "TOPIC" "AWAY" "INVITE" "KICK" "324" "329"))
  (doom-modeline-irc nil)
  (erc-nick "wickedjargon")
  (erc-autojoin-channels-alist
   '((".*\\.libera\\.chat"
      "#programming"
      "#emacs"
      "#archlinux"
      "#devuan"
      "#python"
      "#javascript"
      "#rust"
      "#zig"
      "#c"
      "#haskell"
      "#linux"
      "#debian"
      "#weather"
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

(provide 'fff-emacs-tools)
