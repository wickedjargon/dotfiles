;;; fff-transient-find.el --- Magit-style transient menu for finding files in common directories -*- lexical-binding: t; -*-

(with-eval-after-load 'transient
  (transient-define-prefix fff-transient-find ()
    "Find file in a common directory."
    ["Directories"
     ("p" "Projects     ~/d/projects/"   fff-open-file-in-projects)
     ("n" "Notes        ~/d/notes/"      fff-open-file-in-notes)
     ("t" "Tmp          /tmp/"           fff-open-file-in-tmp)
     ("/" "Root         /"               fff-open-file-in-root-dir)
     ("m" "Home         ~/"              fff-access-home-dir)
     ("s" "SSH                "          fff-find-file-ssh)
     ("c" "Emacs config       "          fff-access-config-dir)
     ("S" "Snippets           "          fff-open-file-in-snippets)]
    ["Other"
     ("r" "Recent files"                 recentf)
     ("g" "Git project root"             fff-find-file-in-project-root)]))

(provide 'fff-transient-find)
;;; fff-transient-find.el ends here
