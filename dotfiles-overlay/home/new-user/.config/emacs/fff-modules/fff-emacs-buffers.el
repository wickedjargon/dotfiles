;;; fff-emacs-buffers.el --- dired, buffers, tabs, windows, web browser -*- lexical-binding: t; -*-

;;; Commentary:

;; Extracted from init.el.  Loaded via `fff-require' in init.el, so an
;; error in this file is reported but does not stop the rest of init.

;;; Code:

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
  (setq dired-free-space nil)
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

(use-package casual :straight t)

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

(provide 'fff-emacs-buffers)
