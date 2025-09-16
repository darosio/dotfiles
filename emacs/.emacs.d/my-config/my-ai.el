;;; my-ai.el --- To use LLM -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Binding keys reserved to user are: "C-c C-<return>" "C-c e"
;;; Code:

;; --- Define API Keys ---
(defvar my/openai-api-key (lambda () (nth 0 (process-lines "pass" "show" "home/openai-dpa"))))
(defvar my/gemini-api-key (lambda () (nth 0 (process-lines "pass" "show" "cloud/gemini_API_key"))))
(defvar my/kagi-api-key (lambda () (nth 0 (process-lines "pass" "show" "cloud/kagi"))))
(defvar my/claude-api-key (lambda () (nth 0 (process-lines "pass" "show" "cloud/claude"))))
(defvar my/openrouter-api-key (lambda () (nth 0 (process-lines "pass" "show" "cloud/openrouter"))))
(defvar my/groq-api-key (lambda () (nth 0 (process-lines "pass" "show" "cloud/groq"))))
(defvar my/deepseek-api-key (lambda () (nth 0 (process-lines "pass" "show" "cloud/deepseek"))))

(use-package llm
  :commands make-llm-ollama)

(use-package ellama
  :commands (make-llm-openai
             make-llm-gemini
             ellama-context-header-line-global-mode
             ellama-session-header-line-global-mode)
  :functions ellama-disable-scroll ellama-enable-scroll
  :bind ("C-c e" . ellama)
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init
  (setopt ellama-auto-scroll t)
  (setopt ellama-language "English")
  :config
  (setopt ellama-sessions-directory "~/Sync/ellama_sessions/")
  (require 'llm-ollama)
  (require 'llm-openai)
  (require 'llm-gemini)
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
          (make-llm-ollama
           :chat-model "gemma3:4b-it-qat"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Customize display buffer behavior
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  ;; Show ellama context/session in header line
  (ellama-context-header-line-global-mode +1)
  (ellama-session-header-line-global-mode +1)
  ;; Handle scrolling events (optional, keep if you find it useful)
  (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
  (advice-add 'end-of-buffer :after #'ellama-enable-scroll)
  ;; --- Define Providers ---
  (setopt ellama-providers
          `(("Ollama gemma3" . ,(make-llm-ollama
                                 :chat-model "gemma3:4b-it-qat"
                                 :embedding-model "nomic-embed-text"
                                 :default-chat-non-standard-params '(("num_ctx" . 8192))))
            ("Ollama LLaVA" . ,(make-llm-ollama ; For vision
                                :chat-model "llava:latest" ; Or specific version like "llava:7b-v1.6-mistral-q8_0"
                                :embedding-model "nomic-embed-text" ; Might not be used by llava for embeddings
                                :default-chat-non-standard-params '(("num_ctx" . 4096))))
            ("OpenAI o4-mini" . ,(make-llm-openai
                                  :key my/openai-api-key
                                  :chat-model "o4-mini"
                                  :embedding-model "text-embedding-ada-002"))
            ;; ("DeepSeek Chat" . ,(make-llm-openai
            ;;                      :api-url "https://api.deepseek.com/v1" ;; CRITICAL: DeepSeek's API endpoint
            ;;                      :key my/deepseek-api-key
            ;;                      ;; :embedding-model "deepseek-embed"
            ;;                      :chat-model "deepseek-chat"))
            ;; :host "api.deepseek.com"
            ;; :endpoint "/v1/chat/completions"
            ;; :chat-model "deepseek-r1-0528:free"))
            ("Gemini 2.0 Flash" . ,(make-llm-gemini
                                    :key my/gemini-api-key
                                    :chat-model "gemini-2.0-flash"))))
  ;; Summarization provider (translation extraction)
  (setopt ellama-summarization-provider
          (make-llm-ollama
           :chat-model "gemma3:4b-it-qat"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  )

(use-package gptel-aibo)
(use-package inline-diff
  :straight (:repo "https://code.tecosaur.net/tec/inline-diff")
  :after gptel-rewrite) ;or use :defer

;; Updated version available at https://github.com/karthink/gptel/wiki
(use-package gptel-rewrite
  :straight gptel
  :bind (:map gptel-rewrite-actions-map
              ("C-c M-d" . gptel--rewrite-inline-diff))
  :config
  (defun gptel--rewrite-inline-diff (&optional ovs)
    "Start an inline-diff session on OVS."
    (interactive (list (gptel--rewrite-overlay-at)))
    (unless (require 'inline-diff nil t)
      (user-error "Inline diffs require the inline-diff package"))
    (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
                ((buffer-live-p ov-buf)))
      (with-current-buffer ov-buf
        (cl-loop for ov in (ensure-list ovs)
                 for ov-beg = (overlay-start ov)
                 for ov-end = (overlay-end ov)
                 for response = (overlay-get ov 'gptel-rewrite)
                 do (delete-overlay ov)
                 (inline-diff-words
                  ov-beg ov-end response)))))
  (when (boundp 'gptel--rewrite-dispatch-actions)
    (add-to-list
     'gptel--rewrite-dispatch-actions '(?i "inline-diff")
     'append)))

(use-package gptel
  :bind ("C-c C-<return>" . gptel-send)
  :preface
  (defun get-ollama-models ()
    "Fetch the list of installed Ollama models."
    (let* ((output (shell-command-to-string "ollama list"))
           (lines (split-string output "\n" t))
           models)
      (dolist (line (cdr lines))  ; Skip the first line
        (when (string-match "^\\([^[:space:]]+\\)" line)
          (push (match-string 1 line) models)))
      (nreverse models)))
  :config
  (require 'gptel-integrations)
  (setq gptel-default-mode 'org-mode)
  (setq gptel-api-key my/openai-api-key)
  ;; (setq gptel-backend
  ;; (setq gptel-model 'gemini-2.5-flash)
  (gptel-make-ollama "Ollama" :stream t :host "localhost:11434" :models (get-ollama-models))
  (gptel-make-gh-copilot "Copilot")
  (gptel-make-kagi "Kagi" :key my/kagi-api-key)
  (gptel-make-gemini "Gemini" :stream t :key my/gemini-api-key)
  (gptel-make-anthropic "Claude" :stream t :key my/claude-api-key)
  (gptel-make-deepseek "DeepSeek" :stream t :key my/deepseek-api-key)
  (gptel-make-openai "OpenRouter" :stream t :key my/openrouter-api-key
                     :host "openrouter.ai"
                     :endpoint "/api/v1/chat/completions"
                     :models '(openai/gpt-3.5-turbo
                               mistralai/mixtral-8x7b-instruct
                               meta-llama/codellama-34b-instruct
                               codellama/codellama-70b-instruct
                               google/palm-2-codechat-bison-32k
                               deepseek/deepseek-r1-0528:free))
  (gptel-make-openai "Groq" :stream t :key my/groq-api-key
                     :host "api.groq.com"
                     :endpoint "/openai/v1/chat/completions"
                     :models '(llama-3.3-70b-versatile
                               deepseek-r1-distill-llama-70b
                               qwen-qwq-32b
                               gemma2-9b-it))
  )

(use-package mcp
  :after gptel
  :custom (mcp-hub-servers
           `(
             ("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,(getenv "HOME"))))
             ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
             ("github" . (:command "docker"
                                   :args ("run" "-i" "--rm"
                                          "-e" "GITHUB_PERSONAL_ACCESS_TOKEN"
                                          "ghcr.io/github/github-mcp-server")
                                   :env (:GITHUB_PERSONAL_ACCESS_TOKEN
                                         ,(string-trim
                                           (shell-command-to-string "pass cloud/github_mcp.el")))))
             ("duckduckgo" . (:command "uvx" :args ("duckduckgo-mcp-server")))
             ("nixos" . (:command "uvx" :args ("mcp-nixos")))
             ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
             ("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp") :env (:DEFAULT_MINIMUM_TOKENS "6000")))
             ;; ("qdrant" . (:url "http://localhost:8000/sse"))
             ("graphlit" . (
                            :command "npx"
                            :args ("-y" "graphlit-mcp-server")
                            :env (
                                  :GRAPHLIT_ORGANIZATION_ID "b2821f53-fda4-4ac1-8c25-5312d4807139"
                                  :GRAPHLIT_ENVIRONMENT_ID "e48c582c-3523-4833-9e5f-037d87cf510b"
                                  :GRAPHLIT_JWT_SECRET "PjpJX7IRDdsMjQkc8pDxjHbF4LkyO8tBLTFTK/S1IqI=")))))
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))

;; (use-package chatgpt-shell
;;   :custom
;;   (chatgpt-shell-openai-key my/openai-api-key)
;;   (dall-e-shell-openai-key my/openai-api-key)
;;   (chatgpt-shell-google-key my/gemini-api-key)
;;   (chatgpt-shell-deepseek-key my/deepseek-api-key)
;;   (chatgpt-shell-openrouter-key my/openrouter-api-key)
;;   (chatgpt-shell-kagi-key my/kagi-api-key)
;;   (chatgpt-shell-anthropic-key my/claude-api-key)
;;   (ollama-shell-buffer-name "*ollama-shell*")
;;   (chatgpt-shell-chat-buffer-name "*chatgpt-shell*")
;;   (gemini-shell-buffer-name "*gemini-shell*")
;;   (openrouter-shell-buffer-name "*openrouter-shell*")
;;   (kagi-shell-buffer-name "*kagi-shell*")
;;   (anthropic-shell-buffer-name "*anthropic-shell*")
;;   ;; (chatgpt-shell-model-version "gpt-4.1-mini")
;;   (chatgpt-shell-model-version "gemini-2.0-flash"))

;; (use-package ob-chatgpt-shell
;;   :after org
;;   :hook (org-mode-hook . (lambda () (require 'ob-chatgpt-shell)))
;;   :custom (chatgpt-shell-openai-key my/openai-api-key))

(provide 'my-ai)
;;; my-ai.el ends here
