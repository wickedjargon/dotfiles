# old use-package declarations

```lisp
(use-package gptel
  :straight t
  :ensure t
  :defer t
  :init
  (let ((key-file (expand-file-name ".secrets/chat_gpt_api_key" user-emacs-directory)))
    (when (file-exists-p key-file)
      (setq gptel-api-key
            (string-trim
             (with-temp-buffer
               (insert-file-contents key-file)
               (buffer-string))))))
  (setq markdown-fontify-code-blocks-natively t)
  :config
  (setq gptel-model 'gpt-4o))
  
  
(use-package all-the-icons :straight t :ensure t
  :if (display-graphic-p))

(use-package emojify :straight t :ensure t :defer t)

(use-package pet
  :defer t
  :ensure t
  :straight t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package go-mode :straight t :ensure t :defer t)

(use-package vimrc-mode :straight t :ensure t :defer t)

(use-package haskell-mode :straight t :ensure t :defer t)

(use-package clojure-mode :straight t :ensure t :defer t)

(use-package cider :straight t :ensure t :defer t
  :config
  (define-key cider-repl-mode-map (kbd "C-c c") #'cider-repl-clear-buffer))

(use-package zig-mode :straight t :ensure t :defer t)

(use-package scala-mode :straight t :ensure t :defer t
  :interpreter
  ("scala" . scala-mode))

(use-package dockerfile-mode :straight t :ensure t)

(use-package json-mode :straight t :ensure t :defer t)

(use-package sml-mode :straight t :ensure t)

(use-package d-mode :straight t :ensure t
  :mode "\\.d\\'"
  :config
  (setq d-mode-indent-style 'k&r))

(use-package v-mode :straight t :ensure t :defer t)

(use-package racket-mode
  :ensure t
  :straight t
  :mode "\\.rkt\\'")

(use-package haxe-mode :ensure t :straight t :defer t)

(use-package graphviz-dot-mode :defer t :straight t :ensure t
  :config
  (setq graphviz-dot-indent-width 4))
```
