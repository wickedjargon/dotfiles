;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; starting our engines...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)


(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(setq package-list '(use-package markdown-mode gcmh))

(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(gcmh-mode 1) ;; reduce garbage collection interference

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up font:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 '(default ((t (:family "Liberation Mono" :foundry "1ASC" :slant normal :weight normal :height 109 :width normal))))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general config:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)                                      ;; no tool bar
(scroll-bar-mode -1)                                    ;; no scroll bar
(setq inhibit-startup-message t)                        ;; no splash screen
(defalias 'yes-or-no-p 'y-or-n-p)                       ;; just type `y`, not `yes`
(menu-bar-mode -1)                                      ;; no menu bar
(setq auto-save-file-name-transforms                    ;;  (save auto save data
      '((".*" "~/.config/emacs/auto-save-list/" t)))    ;;  in a separate directory)
(setq backup-directory-alist                            ;; (save backup files
      '(("." . "~/.config/emacs/backups")))             ;; in a separate directory)
(blink-cursor-mode -1)                                  ;; don't blink my cursor
(setq global-auto-revert-non-file-buffers t)            ;; auto revert my files
(global-auto-revert-mode +1)                            ;; auto revert files and buffers
(delete-selection-mode +1)                              ;; delete selction when hitting backspace on region
(set-default 'truncate-lines t)                         ;; don't wrap my text
(add-hook 'prog-mode-hook #'hs-minor-mode)              ;; let me toggle shrink and expansion of code blocks 
(setq custom-file (locate-user-emacs-file "custom.el")) ;; separate custom.el file
(when (file-exists-p custom-file) (load custom-file))   ;; when it exists, load it
(setq initial-scratch-message "")                       ;; no message on scratch buffer
(global-unset-key (kbd "C-x C-c"))                      ;; I accidently hit this sometimes
(global-unset-key (kbd "C-a"))                          ;; I prefer the vim keybinding
(setq auth-source-save-behavior nil)                    ;; don't prompt to save auth info in home dir
(setq-default indent-tabs-mode nil)                     ;; I prefer spaces instead of tabs
(setq-default tab-width 4)                              ;; I prefer a tab length of 4, not 8
(setq dired-listing-switches                            ;; I prefer to have dired
      "-aBhl  --group-directories-first")               ;; group my directories

;; don't show `active processes exist` warning:
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

;; prevent active process when closing a shell like vterm or eshell:
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; I prefer a full screen new buffer, not a split screen:
(setq display-buffer-base-action '((display-buffer-reuse-window display-buffer-same-window)))

;; show startup time on launch
(defun display-startup-echo-area-message ()
  (message "(emacs-init-time) -> %s" (emacs-init-time)))

;; open .pl files in prolog-mode
(autoload 'prolog-mode "prolog" "" t)
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; my functions:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fff-clear-shell ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun fff-remove-blank-lines ()
  (interactive)
  (flush-lines "^\s*$" (point-min) (point-max))
  )

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

(defun fff-run-go ()
  (interactive)
  (save-buffer)
  (shell-command (format "go run %s" buffer-file-name)))

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

(defun fff-access-config ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun fff-access-home-dir()
  (interactive)
  (find-file "~/"))

(defun fff-access-sched ()
  (interactive)
  (find-file "~/d/personal-notes.md"))

(defun fff-access-bookmarks ()
  (interactive)
  (find-file "~/d/bm.md"))

(defun fff-access-hosts ()
  (interactive)
  (find-file "/sudo:root@localhost:/etc/hosts"))

(defun fff-access-phonebook ()
  (interactive)
  (find-file "~/d/contacts.csv"))

(defun fff-kill-this-buffer ()
  "Kill the current buffer without asking."
  (interactive)
  (kill-buffer (current-buffer)))

(defun fff-switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun fff-switch-to-scratch-buffer-text-mode ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (text-mode))

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

(defun fff-force-kill-this-buffer ()
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer)))

(defun fff-evil-regex-search ()
  (interactive)
  (progn
    (evil-normal-state)
    (evil-ex "%s/")))

(defun fff-insert-tab()
  (interactive)
  (insert "	"))

(defun fff-insert-4-spaces()
  (interactive)
  (insert "    "))

(defun fff-toggle-visual-line-mode ()
  (interactive)
  (if (not visual-line-mode)
      (progn
        (visual-line-mode +1)
        (message "visual line mode on"))
    (progn
      (visual-line-mode -1)
      (message "visual line mode off")
      )))

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
  (progn
    (delete-region
     (line-beginning-position)
     (line-end-position))
    (evil-delete-backward-char-and-join)
    )
  )

(defun fff-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun fff-set-scale-to-zero ()
  (interactive)
  (text-scale-set 0)
  )

(defun fff-run-blue ()
  (interactive)
  (shell-command (format "blue --line-length 300 %s" (buffer-file-name)))
  )

(defun fff-flymake-list ()
  (interactive)
  (flymake-show-buffer-diagnostics))

(defun fff-title-case-region-or-line (@begin @end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           $p1
           $p2
           ($skipChars ""))
       (progn
         (skip-chars-backward $skipChars (line-beginning-position))
         (setq $p1 (point))
         (skip-chars-forward $skipChars (line-end-position))
         (setq $p2 (point)))
       (list $p1 $p2))))
  (let* (
         ($strPairs [
                     
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while
                 (search-forward (aref $x 0) nil t)
               (replace-match (aref $x 1) "FIXEDCASE" "LITERAL")))
           $strPairs))))))

(defun fff-cycle-hyphen-underscore-space ( &optional @begin @end )
  (interactive "P")
  (let ($p1 $p2)
    (if (and @begin @end)
        (setq $p1 @begin $p2 @end)
      (if (use-region-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (if (nth 3 (syntax-ppss))
            (save-excursion
              (skip-chars-backward "^\"")
              (setq $p1 (point))
              (skip-chars-forward "^\"")
              (setq $p2 (point)))
          (let (($skipChars "^\""))
            (skip-chars-backward $skipChars (line-beginning-position))
            (setq $p1 (point))
            (skip-chars-forward $skipChars (line-end-position))
            (setq $p2 (point))
            (set-mark $p1)))))
    (let ( $charArray $length $regionWasActive-p $nowState $changeTo)
      (setq $charArray ["-" "_" " "])
      (setq $length (length $charArray))
      (setq $regionWasActive-p (region-active-p))
      (setq $nowState (if (eq last-command this-command) (get 'fff-cycle-hyphen-lowline-space 'state) 0 ))
      (setq $changeTo (elt $charArray $nowState))
      (save-excursion
        (save-restriction
          (narrow-to-region $p1 $p2)
          (goto-char (point-min))
          (while (re-search-forward (elt $charArray (% (+ $nowState 2) $length)) (point-max) "move")
            (replace-match $changeTo t t))))
      (when (or (string-equal $changeTo " ") $regionWasActive-p)
        (goto-char $p2)
        (set-mark $p1)
        (setq deactivate-mark nil))
      (put 'fff-cycle-hyphen-lowline-space 'state (% (+ $nowState 1) $length))))
  (set-transient-map (let (($kmap (make-sparse-keymap))) (define-key $kmap (kbd "t") 'fff-cycle-hyphen-lowline-space ) $kmap)))

(defun fff-toggle-centered-window-mode ()
  (interactive)
  (if (not centered-window-mode)
      (progn
        (centered-window-mode +1)
        (global-display-line-numbers-mode -1)
        (message "centered window mode on"))
    (progn
      (centered-window-mode -1)
      (global-display-line-numbers-mode +1)
      (message "centered window mode off")
      )))

(defun fff-mark-gt-point-exchange ()
  (if (>  (mark) (point))
      (exchange-point-and-mark)
    ))

(defun fff-wrap-with-function-call (function-name)
  (interactive "sFunction name:")
  (if (and transient-mark-mode mark-active)
      (progn
        (setq text-end ")")
        (setq function-name (concat function-name "("))
        (fff-mark-gt-point-exchange)
        (goto-char (region-end))
        (insert text-end)
        (goto-char (region-beginning))
        (insert function-name))
    (progn
      (setq text-end ")")
      (setq function-name (concat function-name "("))
      (setq bds (bounds-of-thing-at-point 'symbol))
      (goto-char (cdr bds))
      (insert text-end)
      (goto-char (car bds))
      (insert function-name))))


(defun fff-regions-content ()
  "Return the selected region as a string."
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))))

(defun fff-print-debug-line ()
  (interactive)
  (save-excursion
    (setq expression (thing-at-point 'symbol))
    (if (and transient-mark-mode mark-active)
        (setq expression (fff-regions-content)))
    (evil-open-below 1)
    (insert expression)
    (setq text-beg (concat "print(\"" expression " -->\", "))
    (setq text-end ") # ff-debug")
    (evil-first-non-blank)
    (insert text-beg)
    (end-of-line)
    (insert text-end)
    )
  )


(defun fff-delete-debug-lines ()
  (interactive)
  (replace-regexp-in-region ".* # ff-debug$" "" (point-min) (point-max))
  )

(defun fff-python-format ()
  (interactive)
  (fff-remove-blank-lines)
  (elpy-yapf-fix-code)
  (fff-run-blue)
  )

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



(defun fff-display-python ()
  (interactive)
  (run-python (python-shell-calculate-command) nil nil)
  (display-buffer-pop-up-window  (get-buffer-create "*Python*") nil) 
  )


(defun fff-run-python (&optional ARG)
  (interactive "P")
  (elpy-shell-send-region-or-buffer ARG)
  (switch-to-buffer  (get-buffer-create "*Python*") nil) 
  (end-of-buffer)
  (evil-append-line 1)
  (evil-force-normal-state)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load site-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

(use-package doom-modeline
  :ensure t
:custom ((doom-modeline-height 16))
  :init (doom-modeline-mode 1))

(use-package yasnippet-snippets 
  :ensure nil
  :init (add-to-list 'load-path (expand-file-name "~/.config/emacs/site-lisp/yasnippet-snippets/"))
  :load-path "yasnippet-snippets.el"
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

(use-package evil-collection
  :after evil
  :ensure nil
  :init (add-to-list 'load-path (expand-file-name "~/.config/emacs/site-lisp/evil-collection/"))
  :load-path "evil-collection.el"
  :config
  (evil-collection-init)
  (evil-collection-define-key 'normal 'dired-mode-map)
  )

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;; use-package setup:
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq evil-undo-system 'undo-fu)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(use-package evil-leader
  :defer t
  :commands (evil-leader-mode)
  :ensure t
  :init
  (global-evil-leader-mode)
  :config
  (progn
    ;; TODO: let's use some advise instead of over writing like this:
    ;; or find out what is causing this. does this happen with base evil + base emacs?
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


    (fset 'fff-C-c-C-c
          (kmacro-lambda-form [?\C-c ?\C-c] 0 "%d"))

    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "SPC" 'execute-extended-command)
    (evil-leader/set-key "<S-SPC>" 'execute-extended-command-for-buffer)
    (evil-leader/set-key "<tab>" 'ivy-switch-buffer)
    (evil-leader/set-key "<escape>" 'keyboard-escape-quit)
    (evil-leader/set-key "\\" 'evil-switch-to-windows-last-buffer)
    (evil-leader/set-key ";" 'eval-expression)
    (evil-leader/set-key "=" 'fff-hydra-zoom/text-scale-increase)
    (evil-leader/set-key "-" 'fff-hydra-zoom/text-scale-decrease)
    (evil-leader/set-key "0" 'fff-set-scale-to-zero)
    (evil-leader/set-key "1" 'delete-other-windows)
    (evil-leader/set-key "2" 'split-window-below)
    (evil-leader/set-key "3" 'split-window-right)
    (evil-leader/set-key "a" 'yas-insert-snippet)
    (evil-leader/set-key "b s" 'bookmark-set)
    (evil-leader/set-key "b l" 'bookmark-bmenu-list)
    (evil-leader/set-key "b w" 'bookmark-save)
    (evil-leader/set-key "c" 'fff-C-c-C-c)
    (evil-leader/set-key "d" 'delete-blank-lines)
    (evil-leader/set-key "D" 'elpy-doc)
    (evil-leader/set-key "e" 'fff-C-x-C-e)
    (evil-leader/set-key "f b" 'fff-access-bookmarks)
    (evil-leader/set-key "f c" 'fff-access-config)
    (evil-leader/set-key "f f" 'fff-access-sched)
    (evil-leader/set-key "f h" 'fff-access-hosts)
    (evil-leader/set-key "f o" 'fff-access-home-dir)
    (evil-leader/set-key "f p" 'fff-access-phonebook)
    (evil-leader/set-key "f u" 'fff-access-home-dir)
    (evil-leader/set-key "h" 'fff-hydra-movement/evil-backward-paragraph)
    (evil-leader/set-key "H" 'fff-hydra-windsize/windsize-left)
    (evil-leader/set-key "L" 'fff-hydra-windsize/windsize-right)
    (evil-leader/set-key "J" 'fff-hydra-windsize/windsize-down)
    (evil-leader/set-key "K" 'fff-hydra-windsize/windsize-up)
    (evil-leader/set-key "i" 'fff-switch-to-scratch-buffer)
    (evil-leader/set-key "I" 'fff-switch-to-scratch-buffer-text-mode)
    (evil-leader/set-key "k" 'fff-hydra-expand-region/er/expand-region)
    (evil-leader/set-key "l" 'fff-hydra-movement/evil-forward-paragraph)
    ;; (evil-leader/set-key "l" 'lsp-command-map)
    (evil-leader/set-key "m" 'counsel-mark-ring)
    ;; (evil-leader/set-key "p" 'projectile-command-map)         ;; find a new prefix
    (evil-leader/set-key "p" 'fff-evil-paste-and-indent-after)
    (evil-leader/set-key "P" 'fff-evil-paste-and-indent-before)
    (evil-leader/set-key "q" 'delete-window)
    (evil-leader/set-key "Q" 'kill-buffer-and-window)
    (evil-leader/set-key "r" 'fff-evil-regex-search)
    (evil-leader/set-key "R" 'query-replace)
    (evil-leader/set-key "s" 'save-buffer)
    (evil-leader/set-key "t" 'vterm)
    (evil-leader/set-key "T" 'terminal-here)
    (evil-leader/set-key "u" 'evil-jump-backward)
    (evil-leader/set-key "U" 'pop-global-mark)
    ;; (evil-leader/set-key "v" 'fff-toggle-visual-line-mode)
    ;; (evil-leader/set-key "x <tab>" 'fff-insert-tab)
    (evil-leader/set-key "x x" ctl-x-map)
    (evil-leader/set-key "x b" 'list-buffers)
    (evil-leader/set-key "x 0" 'delete-window)
    (evil-leader/set-key "x 1" 'delete-other-windows)
    (evil-leader/set-key "x 2" 'split-window-below)
    (evil-leader/set-key "x 3" 'split-window-right)
    (evil-leader/set-key "x o" 'other-window)
    (evil-leader/set-key "o" 'other-window)
    (evil-leader/set-key "x f" 'find-file)
    ;; (evil-leader/set-key "x r" 'counsel-buffer-or-recentf)
    (evil-leader/set-key "x w" 'write-file)
    (evil-leader/set-key "x h" 'mark-whole-buffer)
    (evil-leader/set-key "x SPC b" 'list-buffers)
    (evil-leader/set-key "x SPC c" 'save-buffers-kill-terminal)

    )
  ) 

(use-package evil
  :defer t
  :after evil-leader
  :ensure t
  :init

  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-fine-undo t)
  (setq evil-search-wrap 'nil)
  ;; hitting C-n and C-p doesn't work for the company-mode pop-up
  ;; after using C-h. The code below resolves this issue
  (with-eval-after-load 'evil
    (with-eval-after-load 'company
      (define-key evil-insert-state-map (kbd "C-n") nil)
      (define-key evil-insert-state-map (kbd "C-p") nil)
      (evil-define-key nil company-active-map (kbd "C-n") #'company-select-next)
      (evil-define-key nil company-active-map (kbd "C-p") #'company-select-previous)))
  :config
  (progn
    (evil-mode 1)
    (define-key evil-visual-state-map (kbd "<backspace>") 'delete-char)
    (define-key evil-visual-state-map (kbd "C-/") 'comment-line)
    (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)

    (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
    (define-key evil-insert-state-map (kbd "M-w") 'easy-kill)
    (define-key evil-insert-state-map (kbd "C-y") 'yank)
    (define-key evil-insert-state-map (kbd "M-y") 'yank-pop)
    (define-key evil-insert-state-map (kbd "C-;") 'hippie-expand)
    (define-key evil-insert-state-map (kbd "C-'") 'company-complete)
    (define-key evil-insert-state-map (kbd "C-/") 'yas-expand)

    (define-key evil-normal-state-map (kbd "q") 'fff-kill-this-buffer)
    (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "C-/") 'comment-line)
    ))

(use-package undo-fu
  :defer t
  :ensure t)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode +1))

(use-package elpy
  :defer t
  :ensure t
  :config
  (setq elpy-shell-starting-directory 'current-directory) 
  (define-key elpy-mode-map (kbd "C-c C-c") nil)
  (define-key elpy-mode-map (kbd "C-c C-c") 'fff-run-python)
  :init (add-hook 'python-mode-hook #'elpy-enable)
;; :init (with-eval-after-load 'python (elpy-enable))
  )

(use-package expand-region
  :defer t
  :ensure t
  )

(use-package vterm
  :defer t
  :ensure t)

(use-package slime
  :defer t
  :ensure t
  :init
  (setq inferior-lisp-program "/usr/bin/clisp")
  )

(use-package terminal-here
  :defer t
  :ensure t
  :init
  (setq terminal-here-linux-terminal-command 'st)
  )

(use-package so-long
  :defer t
  :ensure t
  :init
  (global-so-long-mode +1))

(use-package lorem-ipsum
  :defer t
  :ensure t
  )

(use-package hydra
  :defer t
  :ensure t
  :commands defhydra
  :config
  (progn

    (defhydra fff-hydra-movement ()
      ("h" evil-backward-paragraph)
      ("l" evil-forward-paragraph)
      )

    (defhydra fff-hydra-windsize ()
      ("H" windsize-left)
      ("L" windsize-right)
      ("J" windsize-down)
      ("K" windsize-up)
      )

    (defhydra fff-hydra-zoom ()
      ( "=" text-scale-increase)
      ( "-" text-scale-decrease)
      ( "0"  (text-scale-set 0))
      )

    (defhydra fff-hydra-expand-region ()
      ("k" er/expand-region)
      ("j" er/contract-region)
      )

    ))

(use-package company
  :defer t
  :ensure t
  :bind
  (("C-c z" . counsel-company))
  :init
  (global-company-mode)
  )

(use-package restart-emacs
  :defer t
  :ensure t
  )

(use-package windsize
  :defer t
  :ensure t
  :config
  )

(use-package crux
  :defer t
  :ensure t
  )

(use-package emmet-mode
  :defer t
  :ensure t
  :init (add-hook 'sgml-mode-hook 'emmet-mode)
  )

(use-package markdown-mode
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package pyvenv
  :defer t
  :ensure t
  :config
  (pyvenv-mode t)
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(use-package mw-thesaurus
  :defer t
  :ensure t
  )

(use-package sicp
  :defer t
  :ensure t
  )

(use-package gh-md
  :ensure t
  :defer t
  )

(use-package go-mode
  :ensure t
  :defer t
  :bind
  ("C-c C-c" . fff-run-go)
  )

(use-package cc-mode
  :ensure nil
  :defer t
  :config
  (define-key c-mode-map (kbd "C-c C-c") nil)
  (define-key c-mode-map (kbd "C-c C-c") 'fff-run-c)
  )

(use-package ivy
  :defer t
  :ensure t
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-on-del-error-function #'ignore)
  :init
  (ivy-mode)
  )

(use-package counsel
  :defer t
  :ensure t
  :bind
  (("C-c z" . counsel-company))
  :init
  (setq ivy-initial-inputs-alist nil)
  (when (commandp 'counsel-M-x)
    (global-set-key [remap execute-extended-command] #'counsel-M-x))
  (global-set-key (kbd "C-c c") 'counsel-company)
  )

(use-package lsp-mode
  :defer t
  :ensure t
  :init
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  :hook
  (go-mode . lsp)
  (c-mode . lsp)
  )

(use-package flymake
  :defer t
  :ensure t
  )

(use-package projectile
  :ensure t
  :defer t
  :init
  (projectile-mode +1)
  )

(use-package marginalia
  :defer t
  :ensure t
  :init
  (marginalia-mode))

(use-package ob-go
  :ensure t
  :defer t
  )

(use-package org
  ;; :defer t
  ;; :after ob-go
  :init
  (setq org-confirm-babel-evaluate nil)
  (setq org-startup-with-inline-images t)
  :bind
  ("C-c s" . fff-insert-4-spaces)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (go . t)
     ))
  )

(use-package emojify
  :defer t
  :ensure t
  :init
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  )

(use-package dired
  :ensure nil
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode)))
  )

(use-package switch-window
  :ensure t
  )

(use-package rainbow-mode
  :ensure t
  )

(use-package vimrc-mode
  :ensure t
  )

(use-package theme-changer
  :ensure t
  :init
  (setq calendar-location-name "Toronto, Ontario")
  (setq calendar-latitude 43.79)
  (setq calendar-longitude -79.36)
  :config
  (require 'theme-changer)
  (change-theme 'modus-vivendi 'modus-vivendi)
  )
