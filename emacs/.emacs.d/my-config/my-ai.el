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
  :commands (
             gptel-make-kagi
             gptel-make-anthropic
             gptel-make-openai
             gptel-make-ollama
             gptel-make-gemini
             )
  :config
  (setq gptel-default-mode 'org-mode)
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models (get-ollama-models))
  (setq gptel-api-key my/openai-api-key)
  (gptel-make-kagi "Kagi"
    :key my/kagi-api-key)
  (gptel-make-anthropic "Claude"
    :stream t
    :key my/claude-api-key)
  (gptel-make-gemini "Gemini"
    :key my/gemini-api-key
    :stream t)
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/v1/chat/completions"
    :stream t
    :key my/deepseek-api-key
    :models '(deepseek-chat
              deepseek-reasoner
              deepseek-coder))
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key my/openrouter-api-key
    :models '(openai/gpt-3.5-turbo
              mistralai/mixtral-8x7b-instruct
              meta-llama/codellama-34b-instruct
              codellama/codellama-70b-instruct
              google/palm-2-codechat-bison-32k
              deepseek/deepseek-r1-0528:free))
  (setq gptel-model 'llama-3.3-70b-versatile)
  (setq gptel-backend
        ;; Groq offers an OpenAI compatible API
        (gptel-make-openai "Groq"
          :host "api.groq.com"
          :endpoint "/openai/v1/chat/completions"
          :stream t
          :key my/groq-api-key
          :models '(llama-3.3-70b-versatile
                    deepseek-r1-distill-llama-70b
                    qwen-qwq-32b
                    gemma2-9b-it)))
  )

(use-package chatgpt-shell
  :custom
  (chatgpt-shell-openai-key my/openai-api-key)
  (dall-e-shell-openai-key my/openai-api-key)
  (chatgpt-shell-google-key my/gemini-api-key)
  (chatgpt-shell-deepseek-key my/deepseek-api-key)
  (chatgpt-shell-openrouter-key my/openrouter-api-key)
  (chatgpt-shell-kagi-key my/kagi-api-key)
  (chatgpt-shell-anthropic-key my/claude-api-key)
  (ollama-shell-buffer-name "*ollama-shell*")
  (chatgpt-shell-chat-buffer-name "*chatgpt-shell*")
  (gemini-shell-buffer-name "*gemini-shell*")
  (openrouter-shell-buffer-name "*openrouter-shell*")
  (kagi-shell-buffer-name "*kagi-shell*")
  (anthropic-shell-buffer-name "*anthropic-shell*")
  ;; (chatgpt-shell-model-version "gpt-4.1-mini")
  (chatgpt-shell-model-version "gemini-2.0-flash"))

(use-package ob-chatgpt-shell
  :after org
  :hook (org-mode-hook . (lambda () (require 'ob-chatgpt-shell)))
  :custom (chatgpt-shell-openai-key my/openai-api-key))

(provide 'my-ai)
;;; my-ai.el ends here
