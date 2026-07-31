;;; fff-emacs-project.el --- project, search, docs lookup, indentation -*- lexical-binding: t; -*-

;;; Commentary:

;; Extracted from init.el.  Loaded via `fff-require' in init.el, so an
;; error in this file is reported but does not stop the rest of init.

;;; Code:

;;; Project / Search

(use-package project
  :ensure nil
  :defer t
  :config
  ;; Add custom project root markers
  ;; project.el looks for .git by default, but we can add more markers
  (setq project-vc-extra-root-markers
        '(".venv" "venv" "manage.py" "go.mod"
          "package.json" "Cargo.toml" "build.sh" "v.mod"
          "make.bat" "Makefile" "Dockerfile" ".editorconfig"
          ".gitignore" ".svn" ".hg" ".bzr" "Pipfile" "tox.ini"
          "requirements.txt" "pom.xml" "build.gradle"
          "Cargo.lock" "yarn.lock" "webpack.config.js"
          "Gemfile" ".ruby-version" "composer.json" ".env"
          "README.md" "README.txt" "README.org" ".eslint.js"
          "tsconfig.json" ".babelrc" ".prettierrc"
          "CMakeLists.txt" ".project" "hugo.toml")))

(use-package deadgrep :straight t :defer t)

(use-package wgrep :straight t :defer t)

;;; Docs / Lookup

(use-package help :ensure nil
  :custom
  (help-window-select t)   ; Focus the help window immediately
  :hook
  (help-fns-describe-function-functions . shortdoc-help-fns-examples-function))

(use-package Info :ensure nil :defer t
  :init
  (add-hook 'Info-mode-hook (lambda ()
                              (define-key Info-mode-map  (kbd "M-n") #'Info-search-next)
                              (define-key Info-mode-map (kbd "M-p") #'fff-Info-search-previous))))

(use-package devdocs :straight t :defer t
  :init
  (add-hook 'devdocs-mode-hook (lambda () (visual-line-mode +1))))

(use-package mw-thesaurus :straight t :defer t)

;;; Indentation

(use-package aggressive-indent
  :straight t
  :hook
  ((emacs-lisp-mode lisp-mode lisp-interaction-mode) . aggressive-indent-mode))

;; sets indentation variables
(use-package dtrt-indent :straight t :defer nil
  :config
  (dtrt-indent-global-mode +1)
  ;; run `dtrt-indent-try-set-offset` whenever running a function that changes the indentation
  (dolist (fn '(eglot-format-buffer
                eglot-format-region
                indent-region
                tabify
                untabify))
    (advice-add fn :after (lambda (&rest _args)
                            (when (called-interactively-p 'any)
                              (dtrt-indent-try-set-offset))))))

(provide 'fff-emacs-project)
