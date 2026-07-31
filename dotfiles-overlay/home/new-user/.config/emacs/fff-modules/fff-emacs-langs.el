;;; fff-emacs-langs.el --- language modes, org, lisp, markdown, pdf/epub -*- lexical-binding: t; -*-

;;; Commentary:

;; Extracted from init.el.  Loaded via `fff-require' in init.el, so an
;; error in this file is reported but does not stop the rest of init.

;;; Code:

;;; Language Support Modes And Related Packages

;;; Web Dev Related Packages

(use-package web-mode
  :straight t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.hugo\\'" . web-mode))
  :hook (web-mode . (lambda () (electric-pair-local-mode -1)))
  :config
  (setq web-mode-engines-alist
        '(("django" . "\\.html\\'")
          ("go" . "\\.hugo\\'")))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t))

(use-package typescript-mode :straight t :defer t)

(use-package svelte-mode :straight t :mode "\\.svelte\\'")

;; live-reload preview server for HTML
(use-package simple-httpd :straight t :defer t)

(use-package impatient-mode :straight t :defer t
  :init
  (defun fff-imp-open-preview ()
    "Open current buffer in impatient-mode live preview."
    (interactive)
    (browse-url
     (format "http://localhost:8080/imp/live/%s"
             (buffer-name)))))

(use-package emmet-mode
  :straight t
  :defer t
  :hook ((web-mode . emmet-mode)
         (sgml-mode . emmet-mode)
         (html-mode . emmet-mode))
  :config
  (setq emmet-expand-jsx-className? t)
  (define-key emmet-mode-keymap (kbd "C-j") #'emmet-expand-line))

;; export a code file to html
(use-package htmlize :straight t :defer t)

;;; Org Packages

(use-package ob-racket
  :straight (ob-racket
	         :type git :host github :repo "hasu/emacs-ob-racket"
	         :files ("*.el" "*.rkt"))
  :after org
  :config
  (add-hook 'ob-racket-pre-runtime-library-load-hook
	        #'ob-racket-raco-make-runtime-library))

(use-package org :ensure nil :defer t
  :hook (org-mode . visual-line-mode)
  :init
  (setq org-babel-default-header-args:python
        '((:results . "output")))
  (setq org-babel-lisp-eval-fn "sly-eval")
  (setq org-confirm-babel-evaluate nil)
  (setq org-startup-with-inline-images t)
  (setq org-babel-lisp-eval-command "sbcl --script")
  (setq org-edit-src-content-indentation 0)
  (setq org-startup-folded t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (haskell . t)
     (lisp . t)
     (racket . t)
     (ruby . t)
     (C . t)
     (js . t))))

(use-package org-download :straight t :defer t
  :hook (dired-mode . org-download-enable))

;;; Common Lisp

(use-package lisp-mode :ensure nil
  :mode (("\\.lisp\\'" . lisp-mode)
         ("\\.lsp\\'" . lisp-mode)
         ("\\.cl\\'" . lisp-mode)))

;; common lisp hyperspec
(use-package clhs :straight t :defer t)

;; common lisp documentation
(use-package hyperspec :straight t :defer t
  :commands (hyperspec-lookup)
  :init
  (setq common-lisp-hyperspec-root
        (concat "file://" (expand-file-name "~/.local/share/HyperSpec/")))
  :config
  (defun fff-hyperspec-lookup ()
    "Open the HyperSpec entry in EWW instead of the default browser."
    (interactive)
    (let ((browse-url-browser-function 'eww-browse-url))
      (hyperspec-lookup (thing-at-point 'symbol)))))

(use-package sly :straight t :defer t
  :init
  (setq inferior-lisp-program
        (if (eq system-type 'windows-nt)
            "\"c:/Program Files/Steel Bank Common Lisp/sbcl.exe\""
          "/usr/bin/sbcl"))


  :config
  (define-key lisp-mode-map (kbd "C-j") #'sly-eval-print-last-expression)
  (define-key lisp-mode-map (kbd "C-<return>") #'sly-eval-print-last-expression)
  (evil-set-initial-state 'sly-mrepl-mode 'normal))

;; actually used in elisp
(use-package macrostep :straight t :defer t)

(use-package sly-macrostep :defer t :straight t
  :config
  (add-to-list 'sly-contribs 'sly-macrostep 'append))

;;; Elisp Packages

(use-package elisp-mode
  :ensure nil
  ;; Define your binding specifically for the interaction mode (scratch buffer)
  :bind (:map lisp-interaction-mode-map
              ("C-j" . fff-elisp-eval-and-print-last-sexp))
  :init
  (defun fff-emacs-lisp-mode-setup ()
    "Custom setup for `emacs-lisp-mode`."
    (setq imenu-generic-expression
          '(("Functions" "^\\s-*(\\(defun\\|defsubst\\|defalias\\)\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 2)
            ("Macros" "^\\s-*(\\(defmacro\\)\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 2)
            ("Variables" "^\\s-*(\\(defvar\\|defconst\\)\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 2)
            ("Custom Variables" "^\\s-*(\\(defcustom\\)\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 2)
            ("Set Variables" "^\\s-*(setq\\s-+(?\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Use Package" "^\\s-*(use-package\\s-+'?\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Advice" "^\\s-*(defadvice\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Add Advice" "^\\s-*(advice-add\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Faces" "^\\s-*(defface\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Derived Modes" "^\\s-*(define-derived-mode\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Minor Modes" "^\\s-*(define-minor-mode\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Generic Modes" "^\\s-*(define-generic-mode\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Structs" "^\\s-*(cl-defstruct\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Evil Commands" "^\\s-*(evil-define-command\\s-+\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Autoloads" "^\\s-*(autoload\\s-+'\\([-A-Za-z0-9!$%^&*_=|~`@#<>/]+\\)" 1)
            ("Category Title" "^;;; \\(.*\\)$" 1)))
    (imenu-add-menubar-index))
  :hook (emacs-lisp-mode . fff-emacs-lisp-mode-setup))

;; inline evaluation
(use-package eros :defer nil :straight t :config (eros-mode +1))

;;; Other Language Support Packages

(use-package go-mode :straight t :defer t)

(use-package gitignore-mode
  :straight (:host github :repo "magit/git-modes")
  :mode "\\.gitignore\\'"
  :defer t)

(use-package asm-mode
  :ensure nil
  :mode ("\\.s\\'" "\\.asm\\'")
  :hook (asm-mode . fff-no-indent-asm)
  :config
  (defun fff-no-indent-asm ()
    ;; 1. Your existing settings (keep these to stop TAB/RET indentation)
    (setq-local indent-line-function #'ignore)
    (setq-local indent-region-function #'ignore)
    (setq-local electric-indent-inhibit t)
    (electric-indent-local-mode -1)

    ;; 2. THE FIX: Stop ':' from triggering asm-colon logic
    (local-set-key (kbd ":") #'self-insert-command)
    (local-set-key (kbd ";") #'self-insert-command)))

(use-package rust-mode :straight t :defer t)

(use-package csharp-mode :ensure nil :defer t
  :hook (csharp-mode . (lambda ()
                         (setq imenu-create-index-function
                               (lambda ()
                                 (let ((imenu-generic-expression
                                        '(("Variables" "^\\s-*[a-zA-Z0-9._ ]* \\([a-zA-Z0-9_]*\\)\\( = \\sw*\\|\\s-*\\);$" 1)
                                          ("Functions" "^\\s-*[^/]* \\([a-zA-Z0-9_]+\\)(.*)\\(\\s-*.*\n\\|\\ *\\)\\s-*{" 1)
                                          ("Classes" "^\\s-*\\(.*\\)class +\\([a-zA-Z0-9_]+\\)" 2)
                                          ("Namespaces" "^namespace +\\([a-z0-9_]*\\)" 1))))
                                   (imenu--generic-function imenu-generic-expression)))))))

(use-package pyvenv :straight t :defer t)

(use-package realgud :straight t :defer t)

;;; Markdown

(use-package markdown-mode
  :straight (:host github :repo "jrblevin/markdown-mode")
  :hook (markdown-mode . visual-line-mode)
  :config
  (add-to-list 'markdown-code-lang-modes '("html" . web-mode)))

;; generate markdown toc
(use-package markdown-toc :straight t :defer t)

(use-package gh-md :straight t :defer t)

;;; PDF / EPUB

(use-package pdf-tools :straight t :defer t
  :mode ("\\pdf\\'" . pdf-view-mode)
  :init
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (define-key pdf-view-mode-map (kbd "<tab>") 'pdf-outline)
                                  (pdf-view-themed-minor-mode)))
  :config
  (pdf-tools-install :no-query))

(use-package nov :straight t :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(provide 'fff-emacs-langs)
