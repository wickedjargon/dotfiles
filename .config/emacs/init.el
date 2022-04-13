;; TODO: some ein commands disable evil's keybindings. I will find a way to fix this.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; starting our engines...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

(setq package-list '(use-package markdown-mode))

;; if package-archive-contents is nill, package-refresh-contents
(unless package-archive-contents (package-refresh-contents))

;; if package-archive-contents is nil, install packages from package-list
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up font:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 '(default ((t (:family "Liberation Mono" :foundry "1ASC" :slant normal :weight normal :height 115 :width normal))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "white"))))
 '(font-lock-comment-face ((t (:background "gray15" :foreground "white"))))
 '(ein:basecell-input-area-face ((t (:extend t :background "gray12"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config outside of use-package:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)					;; no tool bar
(setq inhibit-startup-message t)			;; no splash screen
(defalias 'yes-or-no-p 'y-or-n-p)			;; just type `y`, not `yes`
(global-display-line-numbers-mode)			;; global line numbers
(menu-bar-mode +1)					;; I like the menu bar
(setq make-backup-files nil)				;; no backup files
(setq create-lockfiles nil)				;; no lock files
(blink-cursor-mode -1)					;; don't blink my cursor
(setq global-auto-revert-non-file-buffers t)		;; auto revert my files
(global-auto-revert-mode +1)				;; auto revert files and buffers
(delete-selection-mode +1)				;; delete selction when hitting backspace on region
(set-default 'truncate-lines t)				;; don't wrap my text
(add-hook 'prog-mode-hook #'hs-minor-mode)		;; let me toggle shrink and expansion of code blocks 
(setq custom-file (locate-user-emacs-file "custom.el")) ;; separate custom.el file
(when (file-exists-p custom-file) (load custom-file))   ;; when it exists, load it
(setq initial-scratch-message "")			;; no message on scratch buffer
(global-unset-key (kbd "C-x C-c"))			;; I accidently hit this sometimes
(setq auth-source-save-behavior nil)                    ;; don't prompt to save auth info in home dir

;; don't show `active processes exist` warning:
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

;; prevent active process when closing a shell like vterm or eshell:
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; set C-i and C-m to work:
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (fff-set-daemon-stuff)))))
(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-m] [C-m])

;; I prefer a full screen new buffer, not a split screen:
(setq display-buffer-base-action '((display-buffer-reuse-window display-buffer-same-window)))

;; show startup time on launch
(defun display-startup-echo-area-message ()
  (message "(emacs-init-time) -> %s" (emacs-init-time)))

;; open .pl files in prolog-mode
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; my functions:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fff-remove-blank-lines ()
  (interactive)
  (flush-lines "^\s*$" (point-min) (point-max))
  )

(defun fff-clean-mode-line ()
  (dolist (package package-list-diminish)
    (diminish package)))

(defun fff-run-haxe ()
  (interactive)
  (setq dir-path (file-name-directory (buffer-file-name)))
  (shell-command (format "haxe --cwd \"%s\" --run Main" dir-path)))

(defun fff-remove-newlines ()
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "" nil t))))

(defun fff-run-lua ()
  (interactive)
  (shell-command (format "lua %s" buffer-file-name)))

(defun fff-run-haskell ()
  (interactive)
  (shell-command (format "runhaskell %s" buffer-file-name)))

(defun fff-run-java ()
  (interactive)
  (setq dir-path (file-name-directory (buffer-file-name)))
  (setq file-no-ext (substring (buffer-name) 0 -5))
  (shell-command (format "cd %s && javac %s && java %s" dir-path (buffer-name) file-no-ext)))

(defun fff-run-c ()
  (interactive)
  (setq dir-path (file-name-directory (buffer-file-name)))
  (setq file-no-ext (substring (buffer-name) 0 -2))
  (shell-command (format "cd %s && cc -w -o %s %s -lm && ./%s" dir-path file-no-ext (buffer-name) file-no-ext))
  )

(defun fff-build-pdf ()
  (interactive)
  (setq dir-path (file-name-directory (buffer-file-name)))
  (setq file-no-ext (substring (buffer-name) 0 -2))
  ;; groff -ms -T pdf -tle  resume.ms > resume.pdf & zathura resume.pdf
  (shell-command (format "cd %s && groff -ms -T pdf -tle  %s > %s.pdf & zathura %s.pdf"
                         dir-path (buffer-name) file-no-ext  file-no-ext)))

(defun fff-run-cpp ()
  (interactive)
  (setq dir-path (file-name-directory (buffer-file-name)))
  (setq file-no-ext (substring (buffer-name) 0 -2))
  (shell-command (format "cd %s && g++ -w -o %s %s -lm && ./%s" dir-path file-no-ext (buffer-name) file-no-ext)))

(defun fff-set-daemon-stuff ()
  (interactive)
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key input-decode-map [?\C-i] [C-i])
  )

(defun fff-access-config ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun fff-access-home-dir()
  (interactive)
  (find-file "~/"))

(defun fff-access-sched ()
  (interactive)
  (find-file "/home/ff/d/personal-notes.md"))

(defun fff-access-bookmarks ()
  (interactive)
  (find-file "/home/ff/d/bm.md"))


;; don't ask which buffer to kill, just kill current buffer
(defun fff-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun fff-switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun fff-switch-to-scratch-buffer-fundamental-mode ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (fundamental-mode))

(defun fff-copy-file-path ()
  "Put the current file path on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun fff-space-makes-space ()
  (interactive)
  (setq keyboard-translate-table nil) )

(defun fff-space-makes-dash ()
  (interactive)
  (setq keyboard-translate-table (vconcat (make-vector 32 nil) [?-])) )

(defun fff-space-makes-underscore ()
  (interactive)
  (setq keyboard-translate-table (vconcat (make-vector 32 nil) [?_])) )

(defun fff-force-kill-this-buffer ()
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer)))

(defun fff-evil-regex-search ()
  (interactive)
  (progn
    (evil-normal-state)
    (evil-ex "%s/")))

(defun fff-open-in-vim ()
  (interactive)
  (async-shell-command
   (format "nvim +%d %s"
           (+ (if (bolp) 1 0) (count-lines 1 (point)))
           (shell-quote-argument buffer-file-name))))

(defun fff-insert-tab()
  (interactive)
  (insert "	"))


(defun fff-toggle-flycheck-mode ()
  (interactive)
  (if (not flycheck-mode)
      (flycheck-mode +1)
    (flycheck-mode -1)))

(defun fff-send-to-clipboard (x)
  (with-temp-buffer
    (insert x)
    (clipboard-kill-region (point-min) (point-max))))

(defun fff-c-copy-terminal-command ()
  (interactive)
  (setq dir-path (file-name-directory (buffer-file-name)))
  (setq file-no-ext (substring (buffer-name) 0 -2))
  (fff-send-to-clipboard (format "cd %s && cc -w -o %s %s -lm && ./%s" dir-path file-no-ext (buffer-name) file-no-ext))
  )

(defun fff-forward-paragraph (arg)
  "Move forward ARG paragraphs.
A paragraph here is simply defined: it's a block of buffer that's
separated by `[ \t\f]*$`."
  (interactive "p")
  (let ((direction (/ arg (abs arg))))
    (forward-line direction)
    (while (not (or (bobp)
                    (eobp)
                    (= arg 0)))
      (if (looking-at "[ \t\f]*$")
          (setq arg (- arg direction))
        (forward-line direction)))))

(defun fff-backward-paragraph (arg)
  "Move backward ARG paragraphs.
See `my/forward-paragraph' for the behavior."
  (interactive "p")
  (fff-forward-paragraph (- arg)))

(defun fff-undo-cursor ()
  (interactive)
  (set-mark-command t))

(defun fff-comment-delete (arg)
  "Delete the first comment on this line, if any.  Don't touch
the kill ring.  With prefix ARG, delete comments on that many
lines starting with this one."
  (interactive "P")
  (comment-normalize-vars)
  (dotimes (_i (prefix-numeric-value arg))
    (save-excursion
      (beginning-of-line)
      (let ((cs (comment-search-forward (line-end-position) t)))
	(when cs
	  (goto-char cs)
	  (skip-syntax-backward " ")
	  (setq cs (point))
	  (comment-forward)
	  ;; (kill-region cs (if (bolp) (1- (point)) (point))) ; original
	  (delete-region cs (if (bolp) (1- (point)) (point)))  ; replace kill-region with delete-region
	  (indent-according-to-mode))))
    (if arg (forward-line 1))))

(defun fff-comment-delete-dwim (beg end arg)
  "Delete comments without touching the kill ring.  With active
region, delete comments in region.  With prefix, delete comments
in whole buffer.  With neither, delete comments on current line."
  (interactive "r\nP")
  (let ((lines (cond (arg
                      (count-lines (point-min) (point-max)))
                     ((region-active-p)
                      (count-lines beg end)))))
    (save-excursion
      (when lines
        (goto-char (if arg (point-min) beg)))
      (fff-comment-delete (or lines 1)))))

(defun fff-comment-delete-all ()
  (interactive)
  (fff-comment-delete-dwim (point-min) (point-max) +1))

(defun fff-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


(defun fff-clear-line ()
  "Deletes the current line"
  (interactive)
  (delete-region
   (line-beginning-position)
   (line-end-position)))

(defun fff-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load site-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet-snippets 
  :ensure nil
  :init (add-to-list 'load-path (expand-file-name "~/.config/emacs/site-lisp/yasnippet-snippets/"))
  :load-path "yasnippet-snippets.el"
  :config
  ;; (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; use package setup:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

(setq evil-undo-system nil)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(use-package modus-themes
  :ensure t
  :init
  ;; before the package is loaded:
  (modus-themes-load-themes)
  :config
  ;; after the package is loaded:
  (modus-themes-load-vivendi)
  )

(use-package evil-leader
  :commands (evil-leader-mode)
  :ensure t
  :init (global-evil-leader-mode)
  :config
  (progn
    ;; let's use some advise instead of over writing like this:
    (defun comment-line (n)
      (interactive "p")
      (if (use-region-p)
	  (comment-or-uncomment-region
	   (save-excursion
             (goto-char (region-beginning))
             (line-beginning-position))
	   (save-excursion
             (goto-char (region-end))
             ;; (line-end-position)
             ))
	(when (and (eq last-command 'comment-line-backward)
		   (natnump n))
	  (setq n (- n)))
	(let ((range
               (list (line-beginning-position)
                     (goto-char (line-end-position n)))))
	  (comment-or-uncomment-region
	   (apply #'min range)
	   (apply #'max range)))
	(forward-line 1)
	(back-to-indentation)
	(unless (natnump n) (setq this-command 'comment-line-backward))))

    (fset 'fff-C-x-C-e
	  (kmacro-lambda-form [?\C-x ?\C-e] 0 "%d"))

    (fset 'fff-C-x-C-s
	  (kmacro-lambda-form [?\C-x ?\C-s] 0 "%d"))

    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "SPC" 'execute-extended-command)
    (evil-leader/set-key ";" 'eval-expression)
    (evil-leader/set-key "0" 'counsel-buffer-or-recentf)
    (evil-leader/set-key "7" 'fff-access-config)
    (evil-leader/set-key "8" 'fff-access-home-dir)
    (evil-leader/set-key "9" 'fff-access-sched)
    (evil-leader/set-key "a" 'yas-insert-snippet)
    (evil-leader/set-key "b" 'fff-access-bookmarks)
    (evil-leader/set-key "d" 'delete-blank-lines)
    (evil-leader/set-key "D" 'elpy-doc)

    (evil-leader/set-key "e" 'fff-C-x-C-e)
    (evil-leader/set-key "h" 'beginning-of-line)
    (evil-leader/set-key "i" 'fff-switch-to-scratch-buffer)
    (evil-leader/set-key "I" 'fff-switch-to-scratch-buffer-fundamental-mode)
    (evil-leader/set-key "t" 'vterm)
    (evil-leader/set-key "T" 'terminal-here)
    (evil-leader/set-key "p" 'crux-open-with)
    (evil-leader/set-key "<tab>" 'ivy-switch-buffer)
    (evil-leader/set-key "o" 'counsel-find-file)
    (evil-leader/set-key "<escape>" 'keyboard-escape-quit)
    (evil-leader/set-key "l" 'end-of-line)
    (evil-leader/set-key "j" 'ein:run)
    (evil-leader/set-key "q" 'kill-buffer-and-window)
    (evil-leader/set-key "r" 'fff-evil-regex-search)
    (evil-leader/set-key "R" 'anzu-query-replace-regexp)
    (evil-leader/set-key "s" 'save-buffer)

    (evil-leader/set-key "u" 'pop-tag-mark)
    (evil-leader/set-key "U" 'pop-global-mark)
    (evil-leader/set-key "v" 'turn-on-evil-mode)
    (evil-leader/set-key "x <tab>" 'fff-insert-tab)
    (evil-leader/set-key "x x" ctl-x-map)
    (evil-leader/set-key "x 0" 'delete-window)
    (evil-leader/set-key "x 1" 'delete-other-windows)
    (evil-leader/set-key "x 2" 'split-window-below)
    (evil-leader/set-key "x 3" 'split-window-right)
    (evil-leader/set-key "x o" 'other-window)
    (evil-leader/set-key "x f" 'counsel-find-file)
    (evil-leader/set-key "x w" 'write-file)
    (evil-leader/set-key "x h" 'mark-whole-buffer)
    (evil-leader/set-key "x SPC b" 'list-buffers)
    (evil-leader/set-key "y" 'fff-access-sched)
    )
 ) 

(use-package evil
  :after evil-leader
  :ensure t
  :init
  (setq evil-undo-system nil)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-fine-undo t)
  (setq evil-search-wrap 'nil)
  :config
  (progn
    (add-hook 'ein:ipdb-mode-hook 'evil-mode)
    (add-hook 'ein:$kernel-after-execute-hook 'evil-mode)
    (add-hook 'ein:$kernel-after-start-hook 'evil-mode)
    (add-hook 'ein:ipdb-mode-hook 'evil-mode)
    (add-hook 'ein:markdown-mode-hook 'evil-mode)
    (add-hook 'ein:notebook-after-rename-hook 'evil-mode)
    (add-hook 'ein:notebook-mode-hook 'evil-mode)
    (add-hook 'ein:notebooklist-first-open-hook 'evil-mode)
    (add-hook 'ein:notebooklist-mode-hook 'evil-mode)
    (add-hook 'ein:pager-mode-hook 'evil-mode)
    (add-hook 'ein:shared-output-mode-hook 'evil-mode)
    (add-hook 'ein:traceback-mode-hook 'evil-mode)
    (add-hook 'ein:worksheet--which-cell-hook 'evil-mode)
    (add-hook 'ein:worksheet-reinstall-undo-hooks 'evil-mode)
    (fset 'fff-down-paragraph
	  (kmacro-lambda-form [?\}] 0 "%d"))
    (evil-mode 1)
    (define-key evil-visual-state-map (kbd "gl") 'evil-end-of-line)
    (define-key evil-visual-state-map (kbd "gh") 'evil-beginning-of-line)
    (define-key evil-visual-state-map (kbd "<backspace>") 'delete-char)
    (define-key evil-visual-state-map (kbd "C-e") 'end-of-line)
    (define-key evil-visual-state-map (kbd "C-a") 'beginning-of-line)
    (define-key evil-visual-state-map (kbd "C-/") 'comment-line)

    (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
    (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
    (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
    (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
    (define-key evil-insert-state-map (kbd "M-w") 'easy-kill)
    (define-key evil-insert-state-map (kbd "C-y") 'yank)
    (define-key evil-insert-state-map (kbd "M-y") 'yank-pop)
    (define-key evil-insert-state-map (kbd "C-/") 'hippie-expand)
    (define-key evil-insert-state-map (kbd "C-'") 'company-complete)
    (define-key evil-insert-state-map (kbd "C-;") 'yas-expand)

    (define-key evil-normal-state-map (kbd "gl") 'evil-end-of-line)
    (define-key evil-normal-state-map (kbd "gh") 'evil-beginning-of-line)
    (define-key evil-normal-state-map (kbd "q") 'fff-kill-this-buffer)
    (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)
    (define-key evil-normal-state-map (kbd "C-k") 'er/expand-region)
    (define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
    (define-key evil-normal-state-map (kbd "C-a") 'beginning-of-line)
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "C-/") 'comment-line)
    (define-key evil-normal-state-map (kbd "[") 'fff-backward-paragraph)
    (define-key evil-normal-state-map (kbd "]") nil)
    (define-key evil-normal-state-map (kbd "]") 'fff-down-paragraph)
    (define-key evil-normal-state-map (kbd "\\") 'fff-switch-to-previous-buffer)
    (define-key evil-normal-state-map (kbd "<right>") 'next-buffer)
    (define-key evil-normal-state-map (kbd "<left>") 'previous-buffer)

    (define-key evil-visual-state-map (kbd "gl") 'evil-end-of-line)
    (define-key evil-visual-state-map (kbd "gh") 'evil-beginning-of-line)
    (define-key evil-visual-state-map (kbd "<backspace>") 'delete-char)
    (define-key evil-visual-state-map (kbd "C-e") 'end-of-line)
    (define-key evil-visual-state-map (kbd "C-a") 'beginning-of-line)
    (define-key evil-visual-state-map (kbd "C-/") 'comment-line)

    (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
    (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
    (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
    (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
    (define-key evil-insert-state-map (kbd "M-w") 'easy-kill)
    (define-key evil-insert-state-map (kbd "C-y") 'yank)
    (define-key evil-insert-state-map (kbd "M-y") 'yank-pop)
    (define-key evil-insert-state-map (kbd "C-/") 'hippie-expand)
    (define-key evil-insert-state-map (kbd "C-'") 'company-complete)
    (define-key evil-insert-state-map (kbd "C-;") 'yas-expand)

    (define-key evil-normal-state-map (kbd "gl") 'evil-end-of-line)
    (define-key evil-normal-state-map (kbd "gh") 'evil-beginning-of-line)
    (define-key evil-normal-state-map (kbd "-") 'text-scale-adjust)
    (define-key evil-normal-state-map (kbd "=") 'text-scale-adjust)
    (define-key evil-normal-state-map (kbd "0") 'text-scale-adjust)
    (define-key evil-normal-state-map (kbd "q") 'fff-kill-this-buffer)
    (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)
    (define-key evil-normal-state-map (kbd "C-k") 'er/expand-region)
    (define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
    (define-key evil-normal-state-map (kbd "C-a") 'beginning-of-line)
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "C-/") 'comment-line)

    ))

;; (use-package undo-fu
;;   :ensure t)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode +1))

(use-package ivy
  :ensure t
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-on-del-error-function #'ignore)
  :init
  (ivy-mode)
  )

(use-package counsel
  :ensure t
  :config
  :init
  (counsel-mode)
  )

(use-package elpy
  :ensure t
  :config
  (setq elpy-shell-starting-directory 'current-directory) 
  :init
  (elpy-enable)
  )

(use-package ein
  :after evil
  :ensure t
  :defer t
  :commands (ein:run ein:login)
  :init
  (defun fff-set-ein-key-map ()
    (define-key ein:notebook-mode-map (kbd "C-c C-c") nil)
    (define-key ein:notebook-mode-map (kbd "C-c C-c") 'ein:worksheet-execute-cell))
  (add-hook 'ein:notebooklist-mode-hook #'fff-set-ein-key-map)
  )

(use-package expand-region
  :ensure t
  )

(use-package vterm
  :ensure t)

(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "/usr/bin/clisp")
  )

(use-package terminal-here
  :ensure t
  :init
  (setq terminal-here-linux-terminal-command 'st)
  (setq ivy-on-del-error-function #'ignore)
  )

(use-package so-long
  :ensure t
  :init
  (global-so-long-mode +1))

(use-package lorem-ipsum
  :ensure t
  )

(use-package hydra
  :ensure t
  :commands defhydra
  :config
  (progn
    (defhydra hydra-left-shift (global-map "<f9>")
      ("h" evil-backward-paragraph)
      ("l" evil-forward-paragraph)
      ("D" evil-scroll-down)
      ("U" evil-scroll-up)
      ( "=" text-scale-increase)
      ( "-" text-scale-decrease)
      ( "0"  (text-scale-set 0))
      ("H" windsize-left)
      ("L" windsize-right)
      ("J" windsize-down)
      ("K" windsize-up)
      )
    (defhydra hydra-right-shift (global-map "<f10>")
      ("d" fff-clear-line :exit t )
      )
    ))

(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)
  )

(use-package restart-emacs
  :ensure t
  )

(use-package windsize
  :ensure t
  :config
  )

(use-package crux
  :ensure t
  )

(use-package emmet-mode
  :ensure t
  :init (add-hook 'sgml-mode-hook 'emmet-mode)
  )

(use-package org
  :ensure t
  :init
  (setq org-confirm-babel-evaluate nil)
  :bind
  (
   ("C-k" . nil)
   ("C-k" . er/expand-region)
   )
  :config
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)))
    (customize-set-value 'org-latex-hyperref-template "
\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},
 pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L},\n colorlinks=true}\n")
  ))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))
