;; works with emacs version 30.1

;; explore these packages:
;; auctex, org
;; writing studio (https://leanpub.com/emacswritingstudio)
;; dired-kill-when-opening-new-dired-buffer

;; a few emacs kick starters:
;; emacs-kick       https://github.com/LionyxML/emacs-kick
;; kickstart.emacs  https://github.com/MiniApollo/kickstart.emacs
;; venom-emacs      https://gitlab.com/dvrbs/venom-emacs
;; prot's basic     https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/

;; Common commands for Prot's Emacs
;; prot-simple.el
;; https://protesilaos.com/emacs/dotemacs#h:5f78e837-0d27-4390-bd9a-6d0bca57fa50

;; TODO: start using email in emacs
;; emacs mail clients:
;; - Rmail
;; - MH-E
;; - Gnus
;; - Mu4e
;; - Wanderlust
;; - Notmuch

;; TODO: packages to consider
;; - apheleia
;; - editorconfig
;; - smartparens
;; - evil-snipe

;; TODO: improve `space c p' file display:
;; only display file names (without path) or
;; display filenames first with path less prominent in grey next to it.

;;; Initialization

;; Load straight.el (installed by scripts/emacs-sync-packages.sh)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory))))
  (unless (file-exists-p bootstrap-file)
    (error "straight.el is not installed. Run the following command first:\n\n  ~/d/projects/dotfiles/scripts/emacs-sync-packages.sh"))
  (load bootstrap-file nil 'nomessage))

;; Don't auto-clone packages — use scripts/emacs-sync-packages.sh instead.
;; Missing packages are skipped gracefully and reported after startup.
(defvar fff-missing-packages '()
  "List of packages that were not installed during init.")

(advice-add 'straight--clone-repository :override
  (lambda (recipe &optional _cause)
    (straight--with-plist recipe (package)
      (push package fff-missing-packages)
      (message "⚠ Skipping %s (not installed)" package))))

(add-hook 'emacs-startup-hook
  (lambda ()
    (when fff-missing-packages
      (message "")
      (message "⚠ %d package(s) not installed:" (length fff-missing-packages))
      (dolist (pkg (reverse fff-missing-packages))
        (message "  • %s" pkg))
      (message "")
      (message "Run the following command to install them:")
      (message "")
      (message "  ~/d/projects/dotfiles/scripts/emacs-sync-packages.sh")
      (message ""))))

;;; Error-Isolated Loading

;; Modelled on prot's `prot-emacs-configure': a failure inside one block
;; or module becomes a message instead of aborting the rest of init.
;; use-package blocks already get this per-block from use-package itself.

(defmacro fff-configure (&rest body)
  "Evaluate BODY, demoting any error to a message so init continues."
  (declare (indent 0))
  `(condition-case err
       (progn ,@body)
     ((error user-error quit)
      (message "⚠ Config block starting with `%S' failed: %S" (car ',body) (cdr err)))))

(defun fff-require (feature)
  "Load FEATURE, demoting any error to a message so init continues."
  (condition-case err
      (require feature)
    (error (message "⚠ Failed to load %s: %S" feature err))))

;;; Load Path

(add-to-list 'load-path (expand-file-name "fff-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "fff-modules" user-emacs-directory))

;;; Core Settings

;; Must load before any module that pulls in evil: it sets the
;; evil-want-* options, which are read when evil loads.
(fff-require 'fff-emacs-settings)

;;; Personal Libraries (fff-lisp)

(dolist (feature '(fff-files
                   fff-buffers
                   fff-text
                   fff-evil
                   fff-shell
                   fff-ui
                   fff-misc
                   fff-stt
                   fff-transient-find
                   hide-comnt
                   htop-style-monitor))
  (fff-require feature))

;;; Package Modules (fff-modules)

(dolist (feature '(fff-emacs-essentials
                   fff-emacs-buffers
                   fff-emacs-themes
                   fff-emacs-evil
                   fff-emacs-langs
                   fff-emacs-completion
                   fff-emacs-git
                   fff-emacs-tools
                   fff-emacs-project
                   fff-emacs-shell
                   fff-emacs-ui
                   fff-emacs-ide
                   fff-emacs-ai))
  (fff-require feature))
