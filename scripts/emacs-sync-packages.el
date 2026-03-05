;;; emacs-sync-packages.el --- Pre-clone all straight.el packages -*- lexical-binding: t -*-
;;; Run with: emacs --batch -l /path/to/emacs-sync-packages.el

;;; Commentary:
;; Parses init.el, extracts all use-package declarations with :straight,
;; and clones each package via straight-use-package with error handling.
;; Packages that fail to clone are skipped and reported at the end.
;; This ensures Emacs startup never needs to clone anything.

;;; Code:

(defvar fff-sync--init-file
  (expand-file-name "init.el" user-emacs-directory)
  "Path to the Emacs init.el to parse for packages.")

(defvar fff-sync--succeeded '()
  "List of packages that were successfully synced.")

(defvar fff-sync--failed '()
  "List of (package . error) pairs that failed to sync.")


;; ---- Bootstrap straight.el (same as init.el) ----
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ---- Parse init.el for use-package declarations ----
(defun fff-sync--find-keyword-value (form keyword)
  "Walk FORM (a flat list of keyword-value pairs) and find the value after KEYWORD.
Handles use-package syntax where pairs are :keyword value."
  (let ((rest form)
        (result nil))
    (while rest
      (let ((item (car rest)))
        (if (eq item keyword)
            (progn
              (setq result (cadr rest))
              (setq rest nil))              ; stop searching
          ;; Skip to next keyword: advance past current keyword's value
          (if (keywordp item)
              (setq rest (cddr rest))       ; skip keyword + its value
            (setq rest (cdr rest))))))      ; skip non-keyword item
    result))

(defun fff-sync--extract-packages (init-file)
  "Parse INIT-FILE and return a list of straight.el package specs.
Each element is either a symbol (package name) or a list (recipe)."
  (let ((packages '()))
    (with-temp-buffer
      (insert-file-contents init-file)
      (goto-char (point-min))
      (while (re-search-forward "(use-package " nil t)
        (let ((form-start (match-beginning 0)))
          (goto-char form-start)
          (condition-case nil
              (let ((form (read (current-buffer))))
                (when (and (listp form)
                           (eq (car form) 'use-package))
                  (let* ((name (cadr form))
                         (rest (cddr form))
                         (straight-val (fff-sync--find-keyword-value rest :straight)))
                    (cond
                     ;; :straight t → use package name
                     ((eq straight-val t)
                      (push name packages))
                     ;; :straight (recipe ...) → use the recipe
                     ((and straight-val (listp straight-val))
                      (push straight-val packages))
                     ;; :ensure nil or no :straight → skip (built-in)
                     ))))
            (error (goto-char (1+ form-start)))))))
    (nreverse packages)))

;; ---- Sync a single package ----
(defun fff-sync--sync-package (pkg)
  "Attempt to sync PKG via straight-use-package.
PKG is either a symbol or a recipe list.
straight-use-package is a no-op for already-cloned repos."
  (let ((name (if (listp pkg) (car pkg) pkg)))
    (condition-case err
        (progn
          (straight-use-package pkg)
          (push name fff-sync--succeeded)
          (message "  ✓ %s" name))
      (error
       (push (cons name (error-message-string err)) fff-sync--failed)
       (message "  ✗ %s: %s" name (error-message-string err))))))

;; ---- Main ----
(defun fff-sync--main ()
  "Main entry point for the sync script."
  (message "")
  (message "═══════════════════════════════════════════")
  (message "  Emacs Package Sync (straight.el)")
  (message "═══════════════════════════════════════════")
  (message "")

  (unless (file-exists-p fff-sync--init-file)
    (message "✗ init.el not found at %s" fff-sync--init-file)
    (kill-emacs 1))

  (message "Parsing %s..." fff-sync--init-file)
  (let ((packages (fff-sync--extract-packages fff-sync--init-file)))
    (message "Found %d packages with :straight" (length packages))
    (message "")

    ;; Sync each package
    (dolist (pkg packages)
      (fff-sync--sync-package pkg))

    ;; Summary
    (message "")
    (message "═══════════════════════════════════════════")
    (message "  Summary")
    (message "═══════════════════════════════════════════")
    (message "  Total:     %d" (length packages))
    (message "  Succeeded: %d" (length fff-sync--succeeded))
    (message "  Failed:    %d" (length fff-sync--failed))

    (when fff-sync--failed
      (message "")
      (message "Failed packages:")
      (dolist (failure (reverse fff-sync--failed))
        (message "  ✗ %s: %s" (car failure) (cdr failure))))

    (message "")
    (if fff-sync--failed
        (progn
          (message "⚠ Some packages failed. Re-run when connectivity is restored.")
          (kill-emacs 1))
      (message "✓ All packages synced successfully!")
      (kill-emacs 0))))

(fff-sync--main)

;;; emacs-sync-packages.el ends here
