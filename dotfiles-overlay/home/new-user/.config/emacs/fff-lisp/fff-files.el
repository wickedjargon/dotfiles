;;; fff-files.el --- File, directory and project navigation commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal commands, split out of the old fff-functions.el.

;;; Code:

;;;; File Navigation

(defun fff-open-file-in-tmp ()
  "Prompt for a filename and open it in the /tmp/ directory."
  (interactive)
  (let ((default-directory "/tmp/"))
    (call-interactively 'find-file)))

(defun fff-open-file-in-projects ()
  "Select a project from ~/d/projects/ and find-file within it.
The projects directory itself (\".\") is always the first candidate."
  (interactive)
  (let* ((projects-dir (expand-file-name "~/d/projects/"))
         (parent-name ".")
         (dirs (cl-remove-if-not
                #'file-directory-p
                (mapcar (lambda (d) (expand-file-name d projects-dir))
                        (directory-files projects-dir nil "^[^.]"))))
         (sub-names (mapcar #'file-name-nondirectory dirs))
         (all-names (cons parent-name sub-names))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata (display-sort-function
                                  . ,(lambda (candidates)
                                       (if (member parent-name candidates)
                                           (let ((rest (delete parent-name
                                                               (copy-sequence candidates))))
                                             (cons parent-name
                                                   (if (fboundp 'prescient-sort)
                                                       (prescient-sort rest)
                                                     rest)))
                                         (if (fboundp 'prescient-sort)
                                             (prescient-sort candidates)
                                           candidates)))))
                    (complete-with-action action all-names string pred))))
         (selected (completing-read "Project: " table nil t))
         (project-path (if (string= selected parent-name)
                           projects-dir
                         (expand-file-name (concat selected "/") projects-dir))))
    (dired project-path)))

(defun fff-access-config-dir ()
  "Prompt for a filename and open it in the /projects"
  (interactive)
  (let ((default-directory (expand-file-name "./" user-emacs-directory)))
    (call-interactively 'find-file)))

(defun fff-open-file-in-snippets ()
  "Prompt for a filename and open it in the snippets directory."
  (interactive)
  (let ((default-directory (expand-file-name "snippets/" user-emacs-directory)))
    (call-interactively 'find-file)))

(defun fff-open-file-in-root-dir ()
  "Prompt for a filename and open it in the root directory."
  (interactive)
  (let ((default-directory "/"))
    (call-interactively 'find-file)))

(defun fff-access-home-dir ()
  "Prompt for a filename and open it in the snippets directory."
  (interactive)
  (let ((default-directory "~/"))
    (call-interactively 'find-file)))

(defun fff-access-config ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun fff-access-pass()
  (interactive)
  (find-file "~/d/p"))

(defun fff-access-sched ()
  (interactive)
  (find-file "~/d/notes/personal-notes.md"))

(defun fff-access-notes ()
  (interactive)
  (find-file "~/d/notes/exercise_routine.md"))

(defun fff-access-functions-file ()
  (interactive)
  (find-file (expand-file-name "fff-lisp" user-emacs-directory)))

(defun fff-access-bookmarks ()
  (interactive)
  (find-file "~/d/notes/bm.md"))

(defun fff-access-books ()
  (interactive)
  (find-file "~/d/books/"))

(defun fff-access-hosts ()
  (interactive)
  (find-file "/sudo:root@localhost:/etc/hosts"))

(defun fff-access-phonebook ()
  (interactive)
  (find-file "~/d/notes/contacts.csv"))

(defun fff-go-to-git-root-dir ()
  (interactive)
  (dired (locate-dominating-file default-directory ".git")))

(defun fff-go-to-git-root-dir-interactive ()
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (if default-directory
        (call-interactively 'find-file)
      (message "Not in a git repository"))))

(defun fff-open-file-in-notes ()
  "Prompt for a filename and open it in the /projects"
  (interactive)
  (let ((default-directory "~/d/notes/"))
    (call-interactively 'find-file)))

(defun fff-find-file-ssh ()
  "find file ssh"
  (interactive)
  (let ((default-directory "/ssh:"))
    (call-interactively 'find-file)))

(defun fff-connect-to-pi-tramp ()
  "Connect to the SSH server with predefined settings."
  (interactive)
  (let ((default-directory "/ssh:pi@127.0.0.1#5022:"))
    (find-file default-directory)))

(defun fff-open-straight-package-readme (package-name)
  "Open the README file of a straight package in read-only mode.
Prompt for PACKAGE-NAME with completion."
  (interactive
   (let* ((repos-dir (expand-file-name "straight/repos/" straight-base-dir))
          (package-names (if (file-directory-p repos-dir)
                             (cl-remove-if-not
                              (lambda (dir)
                                (seq-some
                                 (lambda (file)
                                   (string-match-p "^README\\(?:\\..*\\)?$" file))
                                 (directory-files (expand-file-name dir repos-dir) nil "^[^.]+")))
                              (directory-files repos-dir nil "^[^.]+"))
                           (error "Repositories directory not found: %s" repos-dir))))
     (list (completing-read "Enter package name: " package-names nil t))))
  (let* ((repo-path (expand-file-name package-name (expand-file-name "straight/repos/" straight-base-dir)))
         (readme-path (seq-some
                       (lambda (file)
                         (when (string-match-p "^README\\(?:\\..*\\)?$" file)
                           (expand-file-name file repo-path)))
                       (directory-files repo-path))))
    (if readme-path
        (let ((buffer (find-file-read-only readme-path)))
          (message "Opened %s in read-only mode." readme-path)
          buffer)
      (message "No README file found for package: %s" package-name))))

(defun fff-maldev-academy-open-modules ()
  "Open a dired buffer with MalDev Academy modules sorted numerically, excluding directories."
  (interactive)
  (require 'seq)
  (let* ((modules-dir "~/d/books/MalDev-Academy-2024/MalDev-Academy/MalDev Modules/")
         (files (directory-files-and-attributes modules-dir nil "^[0-9]+\\..*"))
         ;; Filter out directories
         (file-names (mapcar #'car (seq-filter (lambda (file-attr)
                                                 (not (eq t (nth 1 file-attr))))
                                               files))))
    ;; Sort the files based on the numerical prefix
    (setq file-names (sort file-names
                           (lambda (a b)
                             (< (string-to-number (car (split-string a "\\.")))
                                (string-to-number (car (split-string b "\\.")))))))
    ;; Open dired with sorted files
    (dired (cons modules-dir file-names))))

(defun fff-find-file-in-parent-projects-directory ()
  "Open `find-file` in a parent directory whose name includes 'projects'."
  (interactive)
  (let ((current-dir (expand-file-name default-directory))
        (parent-found nil))
    (while (and (not parent-found)
                (not (string= current-dir "/")))
      (setq current-dir (file-name-directory (directory-file-name current-dir)))
      (when (string-match-p "projects" (file-name-nondirectory (directory-file-name current-dir)))
        (setq parent-found t)))
    (if parent-found
        (let ((default-directory current-dir))
          (call-interactively 'find-file))
      (message "No parent directory containing 'projects' found."))))

(defun fff-open-straight-repo (repo-name)
  "Open the directory of REPO-NAME in the straight/repos directory."
  (interactive
   (list
    (completing-read "Repository name: "
                     (directory-files
                      (expand-file-name "straight/repos/" straight-base-dir)
                      nil "^[^.]"))))
  (let ((repo-path (expand-file-name repo-name
                                     (expand-file-name "straight/repos/" straight-base-dir))))
    (if (file-directory-p repo-path)
        (dired repo-path)
      (message "Repository not found: %s" repo-name))))


(defun fff-delete-current-directory ()
  "Delete the current directory, prompting for confirmation."
  (interactive)
  (let ((current-dir default-directory))
    (when (yes-or-no-p (format "Are you sure you want to delete the directory: %s? " current-dir))
      (delete-directory current-dir t)
      (message "Directory %s deleted." current-dir))))

;;;; Project Management

(defun fff-display-project-root ()
  "Display the current project root."
  (interactive)
  (if-let ((proj (project-current)))
      (message "Project root: %s" (project-root proj))
    (message "Not in a project")))

(defun fff-find-file-in-project-root ()
  "Find a file within the current project root."
  (interactive)
  (if-let ((proj (project-current)))
      (let ((default-directory (project-root proj)))
        (call-interactively 'find-file))
    (message "Not in a project")))

(defun fff-project-ibuffer-for-current-project ()
  "Open an IBuffer window showing all buffers in the current project."
  (interactive)
  (if-let ((proj (project-current)))
      (let ((project-root (project-root proj))
            (project-buffers (project-buffers proj)))
        (ibuffer nil "*project-ibuffer*"
                 `((predicate . (member (current-buffer) ',project-buffers)))))
    (message "Not in a project.")))

(defun fff-consult-open-projects ()
  "Select an open project using Consult."
  (interactive)
  (require 'consult)
  (let ((source
         `(:name     "Open Projects"
                     :narrow   ?p
                     :category project
                     :items    ,(lambda ()
                                  (delete-dups
                                   (delq nil
                                         (mapcar (lambda (buf)
                                                   (when-let ((proj (with-current-buffer buf
                                                                      (project-current nil))))
                                                     (project-root proj)))
                                                 (buffer-list)))))
                     :action   ,(lambda (project)
                                  (let ((default-directory project))
                                    (project-find-file))))))
    (consult--multi (list source)
                    :prompt "Switch to open project: "
                    :sort nil
                    :history 'consult--project-history)))

(defun fff-project-switch-to-buffer ()
  "Switch to a project buffer only if in a project."
  (interactive)
  (if (project-current)
      (project-switch-to-buffer (fff-project--read-project-buffer))
    (message "Not in a project")))

(defun fff-project--read-project-buffer ()
  (let* ((pr (project-current t))
         (project-root (project-root pr))
         (current-buffer (current-buffer))
         (other-buffer (other-buffer current-buffer))
         (other-name (buffer-name other-buffer))
         (buffers (project-buffers pr))
         (predicate
          (lambda (buffer)
            ;; BUFFER is (BUF-NAME . BUF-OBJ) from `Vbuffer_alist`.
            (let ((buf (cdr buffer))
                  (file (buffer-file-name (cdr buffer))))
              (and
               ;; must be one of the project's buffers
               (memq buf buffers)

               ;; must be a file-visiting buffer
               file

               ;; file must live under the project root
               (file-in-directory-p file project-root)

               ;; and not ignored by project ignore rules
               (not (project--buffer-check
                     buf project-ignore-buffer-conditions))))))

         (buffer (read-buffer
                  "Switch to buffer: "
                  (when (funcall predicate (cons other-name other-buffer))
                    other-name)
                  nil
                  predicate)))

    ;; Keep Emacs’ original fallback logic
    (if (or (get-buffer buffer)
            (file-in-directory-p default-directory project-root))
        buffer
      (let ((default-directory project-root))
        (get-buffer-create buffer)))))

(defun fff-project-ibuffer ()
  "Open an IBuffer window showing all buffers in the current project."
  (interactive)
  (if-let ((proj (project-current)))
      (let ((project-root (project-root proj))
            (project-buffers (project-buffers proj)))
        (ibuffer nil "*project-ibuffer*"
                 `((predicate . (member (current-buffer) ',project-buffers)))))
    (message "Not in a project")))


(defun fff-project-find-file ()
  "Call `project-find-file' if in a project, otherwise display a message."
  (interactive)
  (if (project-current)
      (call-interactively #'project-find-file)
    (message "Not in a project")))

(defun fff-open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (derived-mode-p 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")
                 (`windows-nt "start")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (if (string= program "start")
        (shell-command (concat "start \"\" \"" current-file-name "\""))
      (call-process program nil 0 nil current-file-name))))

(provide 'fff-files)
;;; fff-files.el ends here
