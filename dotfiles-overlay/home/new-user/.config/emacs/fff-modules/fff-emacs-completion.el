;;; fff-emacs-completion.el --- minibuffer/in-buffer completion, snippets -*- lexical-binding: t; -*-

;;; Commentary:

;; Extracted from init.el.  Loaded via `fff-require' in init.el, so an
;; error in this file is reported but does not stop the rest of init.

;;; Code:

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

(use-package savehist :ensure nil
  :init
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-additional-variables '(kill-ring))       ;; persist the kill-ring across sessions
  :config
  (savehist-mode))

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

(use-package hippie-exp :ensure nil :defer t
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

(provide 'fff-emacs-completion)
