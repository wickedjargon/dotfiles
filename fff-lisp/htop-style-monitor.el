;;; htop-style-monitor.el --- Emulate htop header in Emacs -*- lexical-binding: t; -*-

(defgroup htop-style-monitor nil
  "Display CPU (per core), memory, swap, tasks, load average, uptime like htop."
  :group 'convenience)

(defcustom htop-style-monitor-update-interval 2
  "Seconds between updates of the monitor display."
  :type 'number)

(defcustom htop-style-monitor-bar-length 30
  "Length of each bar (CPU/mem/swap) in characters."
  :type 'integer)

(defvar htop-style-monitor-buffer "*HTop-Style Monitor*")
(defvar htop-style-monitor-timer nil)

;; --- fetch functions ----------------------------------------------------------

(defun htop-style-monitor-get-cpu-cores ()
  "Return number of CPU cores (logical)."
  (string-to-number
   (string-trim (shell-command-to-string "nproc"))))

(defvar htop-style-monitor-cpu-previous-stats nil
  "Stores the previous CPU stats to compute the difference between readings.")

(defun htop-style-monitor-parse-/proc/stat ()
  "Return a list of (used-pct) for each CPU core based on /proc/stat.
Each value is a floating percentage 0–100."
  (let* ((lines (split-string (shell-command-to-string "grep '^cpu[0-9]' /proc/stat") "\n" t))
         (current-stats (mapcar (lambda (line)
                                  (let* ((fields (split-string line))
                                         ;; fields: cpuN user nice system idle iowait irq softirq steal guest guest_nice
                                         (user (string-to-number (nth 1 fields)))
                                         (nice (string-to-number (nth 2 fields)))
                                         (system (string-to-number (nth 3 fields)))
                                         (idle (string-to-number (nth 4 fields)))
                                         (iowait (string-to-number (nth 5 fields)))
                                         (irq (string-to-number (nth 6 fields)))
                                         (softirq (string-to-number (nth 7 fields)))
                                         (steal (string-to-number (nth 8 fields)))
                                         (total (+ user nice system idle iowait irq softirq steal))
                                         (idle-time idle))  ;; Store idle time
                                    (list total idle-time)))
                                lines))
         (cpu-pcts '()))  ;; This will store the percentages for each core
    ;; Calculate the CPU usage percentage for each core
    (if htop-style-monitor-cpu-previous-stats
        (progn
          (cl-loop for current-stat in current-stats
                   for prev-stat in htop-style-monitor-cpu-previous-stats
                   do
                   (let* ((current-total (nth 0 current-stat))
                          (current-idle (nth 1 current-stat))
                          (prev-total (nth 0 prev-stat))
                          (prev-idle (nth 1 prev-stat))
                          (total-delta (- current-total prev-total))
                          (idle-delta (- current-idle prev-idle))
                          (pct (if (> total-delta 0)
                                   (* 100.0 (/ (- total-delta idle-delta) (float total-delta)))
                                 0.0)))
                     (setq cpu-pcts (append cpu-pcts (list pct)))))
          (setq htop-style-monitor-cpu-previous-stats current-stats))
      ;; Initialize the previous stats on the first run
      (setq htop-style-monitor-cpu-previous-stats current-stats))
    cpu-pcts))


(defun htop-style-monitor-parse-free ()
  "Return a plist with :mem-used, :mem-total, :swap-used, :swap-total in MB."
  (let* ((out (shell-command-to-string "free -m"))
         (lines (split-string out "\n" t))
         (mem-fields (split-string (nth 1 lines) " " t))
         (swap-fields (split-string (nth 2 lines) " " t))
         (mem-total (string-to-number (nth 1 mem-fields)))
         (mem-used  (string-to-number (nth 2 mem-fields)))
         (swap-total (string-to-number (nth 1 swap-fields)))
         (swap-used  (string-to-number (nth 2 swap-fields))))
    (list :mem-total mem-total :mem-used mem-used
          :swap-total swap-total :swap-used swap-used)))

(defun htop-style-monitor-get-tasks-load-uptime ()
  "Return a plist with :tasks-total, :tasks-running, :load1, :load5, :load15, :uptime."
  (let* ((tasks-line (shell-command-to-string "ps -e --no-headers | wc -l"))
         (tasks-total (string-to-number (string-trim tasks-line)))
         (running-line (shell-command-to-string "ps -e -o stat | grep '^ R' | wc -l"))
         (tasks-running (string-to-number (string-trim running-line)))
         (load-str (car (split-string (shell-command-to-string "cat /proc/loadavg") " ")))
         ;; loadavg has three values, uptime from /proc/uptime
         (load-parts (split-string (shell-command-to-string "cat /proc/loadavg") " " t))
         (uptime-secs (string-to-number (car (split-string (shell-command-to-string "cut -d ' ' -f1 /proc/uptime") " " t)))))
    ;; Correctly construct and return a plist
    (list :tasks-total tasks-total 
          :tasks-running tasks-running
          :load1  (string-to-number (nth 0 load-parts))
          :load5  (string-to-number (nth 1 load-parts))
          :load15 (string-to-number (nth 2 load-parts))
          :uptime (floor (/ uptime-secs 60.0)))))  ;; in minutes
  ;; in minutes

;; --- rendering helpers ---------------------------------------------------------

(defun htop-style-monitor-bar-string (used total)
  "Return a string of a bar of length `htop-style-monitor-bar-length` for USED of TOTAL."
  (let* ((pct (if (> total 0)
                  (* 100.0 (/ (float used) total))
                0.0))
         (filled (round (* htop-style-monitor-bar-length (/ pct 100.0))))
         (bar (concat
               "["
               (make-string filled ?█)
               (make-string (- htop-style-monitor-bar-length filled) ?░)
               "] "
               (format "%5.1f%%" pct))))
    bar))

(defun htop-style-monitor-cpu-bar-string (pct)
  "Return a bar string for CPU usage percentage PCT."
  (let* ((filled (round (* htop-style-monitor-bar-length (/ pct 100.0))))
         (bar (concat
               "["
               (make-string filled ?█)
               (make-string (- htop-style-monitor-bar-length filled) ?░)
               "] "
               (format "%5.1f%%" pct))))
    bar))

(defun htop-style-monitor-format-right-info (plist)
  "Format right-side text for tasks/load/uptime from PLIST."
  (let ((t-tot (plist-get plist :tasks-total))
        (t-run (plist-get plist :tasks-running))
        (l1 (plist-get plist :load1))
        (l5 (plist-get plist :load5))
        (l15 (plist-get plist :load15))
        (up-mins (plist-get plist :uptime)))
    (format "Tasks: %d, running: %d | load: %.2f %.2f %.2f | up: %d mins"
            t-tot t-run l1 l5 l15 up-mins)))

;; --- update & display ---------------------------------------------------------

(defun htop-style-monitor-update ()
  "Update the monitor buffer with current stats."
  (let* ((cpu-pcts (htop-style-monitor-parse-/proc/stat))
         (mem-plist (htop-style-monitor-parse-free))
         (tasks-plist (htop-style-monitor-get-tasks-load-uptime)))
    (with-current-buffer (get-buffer-create htop-style-monitor-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; render CPU per core
        (insert "CPU per-core:\n")
        (cl-loop for pct in cpu-pcts
                 for idx from 0 do
                 (insert (format "  core %d: %s\n" idx (htop-style-monitor-cpu-bar-string pct))))
        ;; render memory + swap
        (let ((mused (plist-get mem-plist :mem-used))
              (mtot  (plist-get mem-plist :mem-total))
              (sused (plist-get mem-plist :swap-used))
              (stot  (plist-get mem-plist :swap-total)))
          (insert "\nMem:  " (htop-style-monitor-bar-string mused mtot)
                  (format " (%d/%d MB)\n" mused mtot))
          (insert "Swap: " (htop-style-monitor-bar-string sused stot)
                  (format " (%d/%d MB)\n" sused stot)))
        ;; right side info
        (insert "\n" (htop-style-monitor-format-right-info tasks-plist) "\n")
        (goto-char (point-min))
        (read-only-mode 1)))))

;;;###autoload
(defun htop-style-monitor-start ()
  "Start the htop-style monitor."
  (interactive)
  (when (timerp htop-style-monitor-timer)
    (cancel-timer htop-style-monitor-timer))
  (pop-to-buffer (get-buffer-create htop-style-monitor-buffer))
  (setq htop-style-monitor-timer
        (run-at-time nil htop-style-monitor-update-interval
                     #'htop-style-monitor-update))
  (with-current-buffer htop-style-monitor-buffer
    (read-only-mode 1)
    (local-set-key (kbd "q") #'htop-style-monitor-quit))
  (htop-style-monitor-update))

;;;###autoload
(defun htop-style-monitor-quit ()
  "Stop the monitor and kill buffer."
  (interactive)
  (when (timerp htop-style-monitor-timer)
    (cancel-timer htop-style-monitor-timer))
  (when (get-buffer htop-style-monitor-buffer)
    (kill-buffer htop-style-monitor-buffer)))

(provide 'htop-style-monitor)
;;; htop-style-monitor.el ends here
