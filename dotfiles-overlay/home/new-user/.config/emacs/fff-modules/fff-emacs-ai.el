;;; fff-emacs-ai.el --- AI assistants and key-chord -*- lexical-binding: t; -*-

;;; Commentary:

;; Extracted from init.el.  Loaded via `fff-require' in init.el, so an
;; error in this file is reported but does not stop the rest of init.

;;; Code:

;; agent-shell deps. Install the ACP bridge:
;;   distrobox enter archbox -- npm install -g @agentclientprotocol/claude-agent-acp
(use-package shell-maker
  :straight (shell-maker :type git :host github :repo "xenodium/shell-maker")
  :defer t)

(use-package acp
  :straight (acp :type git :host github :repo "xenodium/acp.el")
  :defer t)

;; Start with M-x agent-shell-anthropic-start-claude-code (or `SPC g a').
(use-package agent-shell
  :straight (agent-shell :type git :host github :repo "xenodium/agent-shell")
  :defer t
  :commands (agent-shell-anthropic-start-claude-code)
  :config
  ;; Run the bridge inside archbox (node lives there, not on the host).
  (setq agent-shell-anthropic-claude-acp-command
        '("distrobox" "enter" "archbox" "--" "claude-agent-acp"))
  (setq agent-shell-anthropic-default-session-mode-id "bypassPermissions")
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)))

(use-package gptel
  :straight t
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :config
  (defun fff-groq-api-key ()
    "Read the Groq API key from ~/.secrets/groq-key."
    (let ((key-file (expand-file-name "~/.secrets/groq-key")))
      (unless (file-exists-p key-file)
        (user-error "Groq key file not found: %s" key-file))
      (string-trim
       (with-temp-buffer
         (insert-file-contents key-file)
         (buffer-string)))))
  (setq gptel-backend
        (gptel-make-openai "Groq"
          :host "api.groq.com"
          :endpoint "/openai/v1/chat/completions"
          :stream t
          :key #'fff-groq-api-key
          :models '(llama-3.3-70b-versatile
                    llama-3.1-8b-instant
                    moonshotai/kimi-k2-instruct
                    qwen/qwen3-32b
                    deepseek-r1-distill-llama-70b))
        gptel-model 'llama-3.3-70b-versatile))

(use-package key-chord
  :straight t
  :config
  (key-chord-mode 1)
  (key-chord-define-global ";;" ":"))

(provide 'fff-emacs-ai)
