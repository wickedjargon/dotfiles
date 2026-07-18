;;; fff-misc.el --- Commands that fit no other fff library -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal commands, split out of the old fff-functions.el.

;;; Code:

;;;; Miscellaneous


(defun fff-Info-search-previous ()
  "Search for previous regexp from a previous `Info-search' command."
  (interactive nil Info-mode)
  (let ((case-fold-search Info-search-case-fold))
    (if Info-search-history
        (Info-search-backward (car Info-search-history))
      (call-interactively 'Info-search))))

(defun fff-find-packages-with-categories ()
  "Scan init.el and list category headers (;;; ...) and packages (use-package ...)
   in the order they appear."
  (interactive)
  (let* ((file (expand-file-name "./init.el" user-emacs-directory))
         (category-regexp "^;;;\\s-+\\(.*\\)$")
         (package-regexp "^(?use-package\\s-+\\([A-Za-z0-9-]+\\)")
         (buffer-name "*Packages & Categories*")
         (items '()))

    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))

      ;; walk through the file
      (while (not (eobp))
        (cond
         ;; CATEGORY
         ((looking-at category-regexp)
          (let ((title (match-string 1)))
            (push (list :type 'category :name title) items)))

         ;; PACKAGE
         ((looking-at package-regexp)
          (let ((pkg (match-string 1)))
            (push (list :type 'package :name pkg) items))))
        (forward-line 1)))

    ;; Output
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert "Packages and categories found in init.el:\n\n")

      (dolist (item (nreverse items))
        (pcase item
          (`(:type category :name ,name)
           (insert (format "=== %s ===\n" name)))
          (`(:type package :name ,pkg)
           (insert (format "  %s\n" pkg)))))

      (goto-char (point-min)))

    (pop-to-buffer buffer-name)))



(defun fff-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- When `evil-mode' is active and not already in normal state, return
  to normal state (so this acts as a universal escape).
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p  'completion-list-mode)
    (delete-completion-window))
   ((>  ( minibuffer-depth)  0)
    (abort-recursive-edit))
   ((and (bound-and-true-p evil-local-mode)
         (not (evil-normal-state-p)))
    (evil-force-normal-state))
   (t
    (keyboard-quit))))

(defun fff-newsticker-treeview-quit ()
  "Quit newsticker treeview."
  (interactive)
  (setq newsticker--sentinel-callback nil)
  (kill-buffer "*Newsticker Tree*")
  (kill-buffer "*Newsticker List*")
  (kill-buffer "*Newsticker Item*")
  (set-window-configuration newsticker--saved-window-config)
  (when newsticker--frame
    (if (frame-live-p newsticker--frame)
        (delete-frame newsticker--frame))
    (setq newsticker--frame nil))
  (newsticker-treeview-save))

(defun fff-set-tmr-timer-for-time (time-string)
  "Set a TMR timer for the specified TIME-STRING.
TIME-STRING should be in the format \"hh:mm am/pm\"."
  (interactive "sEnter time (e.g., 4:30 pm): ")
  (let* ((current-time (current-time))
         ;; Split the time from the period (am/pm).
         (components (split-string time-string " "))
         (time-part (car components))
         (meridiem (downcase (cadr components)))
         (parsed-time (parse-time-string time-part))
         (hour (nth 2 parsed-time))

         ;; Convert 12-hour format to 24-hour format if needed.
         (hour (if (and (equal meridiem "pm") (< hour 12))
                   (+ 12 hour)
                 (if (and (equal meridiem "am") (= hour 12))
                     0
                   hour)))
         (now (decode-time current-time))
         (target-time (encode-time (nth 0 parsed-time)  ; seconds
                                   (nth 1 parsed-time)  ; minutes
                                   hour                 ; adjusted hour
                                   (nth 3 now)          ; current day
                                   (nth 4 now)          ; current month
                                   (nth 5 now)          ; current year
                                   (nth 8 now))))       ; current timezone
    ;; Adjust if the target time is already passed for today.
    (when (time-less-p target-time current-time)
      (setq target-time (time-add target-time (days-to-time 1))))
    (let ((seconds-until-target (float-time (time-subtract target-time current-time))))
      (if (> seconds-until-target 0)
          (tmr (number-to-string (/ seconds-until-target 60)))
        (error "The specified time is invalid")))))


(provide 'fff-misc)
;;; fff-misc.el ends here
