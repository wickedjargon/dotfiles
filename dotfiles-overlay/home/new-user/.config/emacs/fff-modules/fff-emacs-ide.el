;;; fff-emacs-ide.el --- flymake, eglot, navigation, editing aids -*- lexical-binding: t; -*-

;;; Commentary:

;; Extracted from init.el.  Loaded via `fff-require' in init.el, so an
;; error in this file is reported but does not stop the rest of init.

;;; Code:

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

(use-package flimenu :straight t :defer t
  :init
  ;; load on first imenu use (e.g. `SPC i m' consult-imenu)
  (with-eval-after-load 'imenu
    (require 'flimenu)
    (flimenu-global-mode)))

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

(use-package editorconfig
  :ensure nil
  :config
  (editorconfig-mode 1))

(use-package webjump
  :ensure nil
  :custom
  (webjump-use-internal-browser t))

(provide 'fff-emacs-ide)
