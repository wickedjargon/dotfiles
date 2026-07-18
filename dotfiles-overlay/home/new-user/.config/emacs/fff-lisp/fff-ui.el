;;; fff-ui.el --- Theme, display, menu and audio commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal commands, split out of the old fff-functions.el.

;;; Code:

;;;; Theme and Display

(defun fff-set-scale-to-zero ()
  (interactive)
  (text-scale-set 0))

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
      (message "centered window mode off"))))

(defun fff-increase-font-size ()
  "Increase the font size."
  (interactive)
  (let ((current-height (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ current-height 10))
    (message "Font size increased")))

(defun fff-decrease-font-size ()
  "Decrease the font size."
  (interactive)
  (let ((current-height (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- current-height 10))
    (message "Font size decreased")))

(defun fff-toggle-flymake-highlight ()
  "Toggle the highlighting of errors and warnings in Flymake."
  (interactive)
  (if (eq (face-attribute 'flymake-error :underline nil) nil)
      (progn
        (set-face-attribute 'flymake-error nil :underline t)
        (set-face-attribute 'flymake-note nil :underline t)
        (set-face-attribute 'flymake-warning nil :underline t))
    (progn
      (set-face-attribute 'flymake-error nil :underline nil)
      (set-face-attribute 'flymake-note nil :underline nil)
      (set-face-attribute 'flymake-warning nil :underline nil))))

(defun fff-show-current-line-number ()
  "Display the current line number in the echo area."
  (interactive)
  (message "Current line number: %d" (line-number-at-pos)))

(defun fff-display-tooltip-at-point ()
  "Display information about the symbol at point using posframe."
  (interactive)
  ;; Load help-mode now: the help-xref-following let must be dynamic.
  (require 'help-mode)
  (let ((help-xref-following t)
        (thing (thing-at-point 'symbol t)))
    (when thing
      (let ((tooltip-text (or (get-text-property (point) 'help-echo)
                              (help-at-pt-kbd-string)
                              (fff-help-sym-short-doc thing)
                              (fff-help-function-arglist thing))))
        (when tooltip-text
          (fff-display-centered-tooltip tooltip-text))))))

(defun fff-display-centered-tooltip (text)
  "Display TEXT horizontally centered in the window using posframe."
  (unless (facep 'fff-tooltip-face)
    (defface fff-tooltip-face
      '((t :inherit default :height 1.5))  ; Adjust the :height value to change the font size
      "Face for tooltip text in posframe."))
  (posframe-show
   "*fff-centered-tooltip*"
   :string (propertize text 'face 'fff-tooltip-face)
   :position (point)
   :poshandler (lambda (info)
                 (cons (/ (- (plist-get info :parent-frame-width)
                             (plist-get info :posframe-width))
                          2)
                       0))
   :timeout 10))

(defun fff-help-sym-short-doc (sym)
  "Return short documentation for symbol SYM."
  (when (and sym (symbolp sym))
    (or (and (boundp sym) (documentation-property sym 'variable-documentation))
        (and (fboundp sym) (documentation sym 'function-documentation)))))

(defun fff-help-function-arglist (sym)
  "Return function argument list for symbol SYM."
  (when (and (symbolp sym) (fboundp sym))
    (when-let ((args (help-function-arglist sym t)))
      (format "%s is a function: %s" sym (prin1-to-string args)))))

(defun fff-toggle-light-dark-theme ()
  "Toggle between light and dark themes.
   The themes should be named themename-light and themename-dark,
   or they should be named with specific patterns like modus-vivendi/modus-operandi."
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    (if (and current-theme
             (or (string-match "\\(.*\\)-\\(light\\|dark\\)" (symbol-name current-theme))
                 (member current-theme '(modus-vivendi modus-operandi))))
        (let* ((theme-target
                (cond
                 ;; Handle vexed naming schemes
                 ((eq current-theme 'modus-vivendi) 'modus-operandi)
                 ((eq current-theme 'modus-operandi) 'modus-vivendi)
                 ;; Handle regular -light/-dark pattern
                 (t (let ((theme-base (match-string 1 (symbol-name current-theme))))
                      (intern (concat theme-base "-"
                                      (if (string= (match-string 2 (symbol-name current-theme)) "light")
                                          "dark"
                                        "light"))))))))
          (load-theme theme-target t)
          (disable-theme current-theme))
      (message "No active theme or theme name does not match the expected pattern."))))

(defun fff-load-theme (theme)
  "Disable active themes and load the specified THEME."
  (interactive
   (list (intern (completing-read "Load custom theme: "
                                  (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun fff-theme-dark ()
  "Switch to ef-tritanopia-dark, disabling ef-tritanopia-light."
  (interactive)
  (disable-theme 'ef-tritanopia-light)
  (load-theme 'ef-tritanopia-dark t)
  (message "Switched to dark theme: ef-tritanopia-dark"))

(defun fff-theme-light ()
  "Switch to ef-tritanopia-light, disabling ef-tritanopia-dark."
  (interactive)
  (disable-theme 'ef-tritanopia-dark)
  (load-theme 'ef-tritanopia-light t)
  (message "Switched to light theme: ef-tritanopia-light"))


;;;; Menus

(defun fff-menu-programming-functions ()
  "Select and run a programming-related function."
  (interactive)
  (let* ((functions-list '(lsp
                           lsp-rename
                           lsp-describe-thing-at-point
                           lsp-format-buffer
                           lsp-find-references
                           imenu
                           compile
                           flymake-show-diagnostics-buffer
                           ))
         (selected-function (completing-read "Select a function: " functions-list nil t)))
    (when selected-function
      (call-interactively (intern selected-function)))))

(defun fff-menu-entertainment ()
  "Select and run a programming-related function."
  (interactive)
  (let* ((functions-list '(emms
                           ytdl-download
                           elfeed
                           ))
         (selected-function (completing-read "Select a function: " functions-list nil t)))
    (when selected-function
      (call-interactively (intern selected-function)))))

(defun fff-menu-system-management-functions--improved ()
  "Select and run a system management-related function."
  (interactive)
  (let* ((functions-list '(("Lock screen" . (lambda () (shell-command "slock")))
                           ("Kill dwm" . (lambda () (shell-command "kill -TERM $(pidof dwm)")))
                           ("Reboot" . (lambda () (shell-command "kill -TERM $(pidof dwm) && systemctl reboot")))
                           ("Restart Emacs" . restart-emacs)
                           ("Shutdown" . (lambda () (shell-command "kill -TERM $(pidof dwm) && systemctl poweroff")))
                           ("Turn off screen" . (lambda () (shell-command "xset dpms force off")))))
         (selected-function (completing-read "Select a system management function: "
                                             (mapcar 'car functions-list)
                                             nil t)))
    (when selected-function
      (call-interactively (cdr (assoc selected-function functions-list))))))

(defun fff-menu-magit-functions ()
  "Select and run a Magit-related function."
  (interactive)
  (let* ((functions-list '(magit
                           magit-section-hide-children))
         (selected-function (completing-read "Select a Magit function: " functions-list nil t)))
    (when selected-function
      (call-interactively (intern selected-function)))))

(defun fff-menu-search-functions ()
  "Select and run a search-related function."
  (interactive)
  (let* ((functions-list '(isearch-forward
                           query-replace
                           isearch-forward-regexp
                           occur
                           iedit-mode
                           find-tag
                           rgrep
                           deadgrep
                           swiper
                           consult-line
                           consult-line-multi
                           consult-ripgrep
                           consult-grep
                           consult-find
                           consult-locate
                           consult-outline
                           consult-imenu
                           ))
         ;; Keep only commands that are actually defined
         (available-functions (seq-filter #'fboundp functions-list))
         (selected-function (completing-read "Select a search function: "
                                             available-functions nil t)))
    (when selected-function
      (call-interactively (intern selected-function)))))

(defun fff-menu-main ()
  "Select and run one of the fff-menu functions."
  (interactive)
  (let* ((menu-list '(("Programming" . fff-menu-programming-functions)
                      ("System Management" . fff-menu-system-management-functions)
                      ("Magit" . fff-menu-magit-functions)
                      ("Search" . fff-menu-search-functions)))
         (selected-menu (completing-read "Select a menu: " (mapcar 'car menu-list) nil t))
         (menu-function (cdr (assoc selected-menu menu-list))))
    (when menu-function
      (call-interactively menu-function))))

(defun fff-ibuffer-filter-menu-functions ()
  "Select and run an ibuffer filter-related function."
  (interactive)
  (let* ((functions-list '(ibuffer-filter-disable
                           ibuffer-exchange-filters
                           ibuffer-filter-by-mode
                           ibuffer-filter-chosen-by-completion
                           ibuffer-negate-filter
                           ibuffer-and-filter
                           ibuffer-filter-by-starred-name
                           ibuffer-filter-by-file-extension
                           ibuffer-filter-by-size-lt
                           ibuffer-filter-by-size-gt
                           ibuffer-decompose-filter-group
                           ibuffer-filter-by-process
                           ibuffer-filter-by-directory
                           ibuffer-filter-by-derived-mode
                           ibuffer-pop-filter-group
                           ibuffer-switch-to-saved-filter-groups
                           ibuffer-save-filter-groups
                           ibuffer-delete-saved-filter-groups
                           ibuffer-clear-filter-groups
                           ibuffer-add-saved-filters
                           ibuffer-filter-by-basename
                           ibuffer-filter-by-content
                           ibuffer-decompose-filter
                           ibuffer-filter-by-predicate
                           ibuffer-filter-by-filename
                           ibuffer-filters-to-filter-group
                           ibuffer-filter-by-modified
                           ibuffer-filter-by-used-mode
                           ibuffer-filter-by-name
                           ibuffer-or-filter
                           ibuffer-pop-filter
                           ibuffer-switch-to-saved-filters
                           ibuffer-save-filters
                           ibuffer-filter-by-visiting-file
                           ibuffer-delete-saved-filters
                           ibuffer-or-filter
                           ibuffer-pop-filter-group
                           ibuffer-pop-filter))
         (selected-function (completing-read "Select an ibuffer filter function: " functions-list nil t)))
    (when selected-function
      (call-interactively (intern selected-function)))))

(defun fff-easy-hugo-menu-functions ()
  "Select and run an Easy Hugo function."
  (interactive)
  (let* ((functions-list '(easy-hugo-newpost
                           easy-hugo-article
                           easy-hugo-preview
                           easy-hugo-publish
                           easy-hugo-open
                           easy-hugo-delete
                           easy-hugo-open-config
                           easy-hugo-no-help
                           easy-hugo-view
                           easy-hugo-refresh
                           easy-hugo-sort-time
                           easy-hugo-sort-char
                           easy-hugo-github-deploy
                           easy-hugo-amazon-s3-deploy
                           easy-hugo-google-cloud-storage-deploy))
         (selected-function (completing-read "Select an Easy Hugo function: " functions-list nil t)))
    (when selected-function
      (call-interactively (intern selected-function)))))

(defun fff-menu-functions ()
  "Select and run a function with a name starting with 'fff-'."
  (interactive)
  (let (fff-functions)
    (mapatoms (lambda (sym)
                (when (and (fboundp sym)
                           (string-prefix-p "fff-" (symbol-name sym)))
                  (push (symbol-name sym) fff-functions)))
              obarray)
    (let ((selected-function (completing-read "Select a function: " fff-functions nil t)))
      (when selected-function
        (call-interactively (intern selected-function))))))


(defun fff-vertico-display-candidates-in-buffer ()
  "Display the current Vertico candidates in a dedicated buffer and switch focus to it."
  (interactive)
  (when (and (bound-and-true-p vertico--candidates) (active-minibuffer-window))
    (let ((buffer (get-buffer-create "*Vertico Candidates*"))
          ;; Vertico sometimes keeps the candidates in a different structure;
          ;; hence we should ensure we're working with a list.
          (candidates (or vertico--candidates (completion-all-sorted-completions))))
      (with-current-buffer buffer
        (erase-buffer)
        (dolist (cand candidates)
          (insert (format "%s\n" cand))))
      ;; Display and switch to the buffer, then deactivate the minibuffer.
      (select-window (display-buffer buffer)))))

;;;; Audio

(defun fff-toggle-audio-mute ()
  "Toggle audio mute state using PulseAudio."
  (interactive)
  (shell-command "pactl set-sink-mute @DEFAULT_SINK@ toggle"))

(defun fff-lower-volume ()
  "Lower audio volume using PulseAudio."
  (interactive)
  (shell-command "pactl set-sink-volume @DEFAULT_SINK@ -5%"))

(defun fff-raise-volume ()
  "Raise audio volume using PulseAudio."
  (interactive)
  (shell-command "pactl set-sink-volume @DEFAULT_SINK@ +5%"))


(provide 'fff-ui)
;;; fff-ui.el ends here
