;;; fff-emacs-essentials.el --- small built-in modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Extracted from init.el.  Loaded via `fff-require' in init.el, so an
;; error in this file is reported but does not stop the rest of init.

;;; Code:

(use-package fff-stt
  :ensure nil
  :commands (fff-stt-toggle fff-stt-cancel))

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

(provide 'fff-emacs-essentials)
