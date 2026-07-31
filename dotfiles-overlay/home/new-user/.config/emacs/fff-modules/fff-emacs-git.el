;;; fff-emacs-git.el --- git tooling -*- lexical-binding: t; -*-

;;; Commentary:

;; Extracted from init.el.  Loaded via `fff-require' in init.el, so an
;; error in this file is reported but does not stop the rest of init.

;;; Code:

;;; Git

(use-package magit :straight t :defer t
  :init
  (setq magit-section-initial-visibility-alist
        '(([hunk file staged status] . hide)
          ([file unstaged status] . show)
          ([hunk file unstaged status] . hide))))

(use-package git-timemachine :straight t :defer t)

(use-package git-gutter :straight t
  :config
  (global-git-gutter-mode +1)
  ;; A non-zero interval live-diffs the current buffer on idle — including
  ;; freshly opened binary files (e.g. images from dired), where writing
  ;; the git blob to the diff temp file prompts "Select coding system".
  ;; With 0 the gutter still refreshes on save and revert.
  (setq git-gutter:update-interval 0)
  ;; If live updates are ever re-enabled, the diff temp files must be
  ;; written verbatim, not through text encoding.
  (advice-add 'git-gutter:live-update :around
              (lambda (orig &rest args)
                (let ((coding-system-for-write 'binary))
                  (apply orig args))))
  (add-hook 'find-file-hook
            (lambda ()
              (when (and (fboundp 'tramp-tramp-file-p)
                         (tramp-tramp-file-p (or buffer-file-name "")))
                (git-gutter-mode -1)))))

(use-package git-gutter-fringe :straight t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package consult-gh :straight t :defer t)

(provide 'fff-emacs-git)
