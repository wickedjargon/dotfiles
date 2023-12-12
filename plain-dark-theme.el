;;; dark-theme.el --- Dark theme without syntax highlighting

;; Package-Requires: ((emacs "24"))
;; Package-Version: 8

(deftheme dark "Dark theme without syntax highlighting.")

(defgroup dark-theme nil
  "Dark theme colors and faces."
  :group 'faces
  :prefix "dark-")

(defcustom dark-background "black"
  "Color to use for background."
  :type 'color)

(defcustom dark-foreground "white"
  "Color to use for text."
  :type 'color)

(defcustom dark-faces '(default eshell-prompt fringe minibuffer-prompt)
  "List of faces to decolorize."
  :type '(repeat symbol))

(defun dark--spec (face)
  "Return spec for a FACE."
  `(,face ((t (:background ,dark-background :foreground ,dark-foreground)))))

(defun dark--add (faces)
  "Add FACES to the theme definition."
  (apply 'custom-theme-set-faces 'dark (mapcar 'dark--spec faces)))

(dark--add dark-faces)

(defcustom dark-prefix-alist
  '((font-lock . "font-lock-")
    (sh-script . "sh-")
    (web-mode . "web-mode-"))
  "Mapping from files to face prefixes: when file is first loaded,
decolorizes every face that starts with the prefix."
  :type '(alist :key-type symbol :value-type string))

(require 'cl-lib)

(defun dark--prefix (prefix)
  "Return all faces that start with PREFIX."
  (cl-remove-if-not (lambda (s) (string-prefix-p prefix (symbol-name s)))
		    (face-list)))

(dolist (a dark-prefix-alist)
  (eval-after-load (car a) `(dark--add (dark--prefix ,(cdr a)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(setq bold '((t nil)))
(setq font-lock-comment-delimiter-face '((t (:background "black" :foreground "gray70"))))
(setq font-lock-comment-face '((t (:background "black" :foreground "gray70"))))
(setq italic '((t nil)))
(setq mode-line '((t (:background "dim gray" :foreground "white" :box (:line-width (1 . -1) :style released-button)))))

(set-face-attribute 'mode-line nil
                    :background "gray23"
                    :foreground "white"
                    :box '(:line-width (1 . -1) :style released-button))


(provide-theme 'dark)

;;; dark-theme.el ends here
