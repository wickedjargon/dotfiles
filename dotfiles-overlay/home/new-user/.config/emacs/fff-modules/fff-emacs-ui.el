;;; fff-emacs-ui.el --- modeline and visual polish -*- lexical-binding: t; -*-

;;; Commentary:

;; Extracted from init.el.  Loaded via `fff-require' in init.el, so an
;; error in this file is reported but does not stop the rest of init.

;;; Code:

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
  :defer t
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

(provide 'fff-emacs-ui)
