;;; fff-evil.el --- Helpers for evil-mode workflows -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal commands, split out of the old fff-functions.el.

;;; Code:

;;;; Evil Integration

(defun fff-evil-regex-search ()
  (interactive)
  (progn
    (evil-normal-state)
    (evil-ex "%s/")))

(defun fff-evil-paste-and-indent-after ()
  (interactive)
  (evil-with-undo
    (progn
      (evil-paste-after 1)
      (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\])))))

(defun fff-evil-paste-and-indent-before ()
  (interactive)
  (evil-with-undo
    (progn
      (evil-paste-before 1)
      (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\])))))

(defun fff-user-is-in-evil-normal-state ()
  "Check if the user is in Evil's normal state."
  (eq evil-state 'normal))

(defun fff-move-cursor-to-next-line ()
  "Move the cursor to the beginning of the next line, creating a new line if necessary."
  (goto-char (line-beginning-position 2))
  (unless (= (point) (point-max))
    (newline)))

(defun fff-evil-regex-search-forward ()
  "Force a regex search using Evil's search engine, ignoring the global literal setting."
  (interactive)
  ;; We locally let-bind the variable to t (true) just for this command
  (let ((evil-regexp-search t))
    (call-interactively 'evil-search-forward)))

(defun fff-evil-regex-search-backwards ()
  "Force a regex search using Evil's search engine, ignoring the global literal setting."
  (interactive)
  ;; We locally let-bind the variable to t (true) just for this command
  (let ((evil-regexp-search t))
    (call-interactively 'evil-search-backward)))

(defun fff-toggle-evil-regexp-search ()
  "Toggle the value of evil-regexp-search and message the new state."
  (interactive)
  (setq evil-regexp-search (not evil-regexp-search))
  (message "evil-regexp-search is now %s" (if evil-regexp-search "enabled" "disabled")))


(provide 'fff-evil)
;;; fff-evil.el ends here
