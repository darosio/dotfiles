;;; my-ai.el --- To use LLM -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This configuration provides a comprehensive AI-powered development environment
;; within Emacs, integrating local and cloud-based LLMs with advanced tools.
;;
;; Key bindings:
;; - C-c C-<return> : Send text to AI (gptel)
;; - C-c e          : Start ellama session
;; - C-c C-i        : Apply inline diff from gptel-rewrite
;;
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
  ;; Session naming provider
  (setopt ellama-naming-provider
          (make-llm-ollama
           :chat-model "gemma3:4b-it-qat"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Display behavior
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  ;; Show context/session in header line
  (ellama-context-header-line-global-mode +1)
  ;; Hide session info from header line when no session is active
  (advice-add 'ellama-session-line :around
              (lambda (orig-fn)
                (if (or ellama--current-session
                        ellama--current-session-id
                        ellama--current-session-uid)
                    (funcall orig-fn)
                  "")))
  (ellama-session-header-line-global-mode +1)
  ;; Scrolling behavior
  (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
  (advice-add 'end-of-buffer :after #'ellama-enable-scroll)
  ;; AI Providers Configuration
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
  ("<Launch5> <Launch5>" . gptel-send)
  ("<Launch5> g" . gptel)
  ("<Launch5> m" . gptel-menu)
  ("<Launch5> M" . gptel-mcp-connect)
  ("<Launch5> r" . gptel-rewrite)
  ("<Launch5> t" . gptel-tools)
  ("<Launch5> c a" . gptel-add)
  ("<Launch5> c A" . gptel-add-file)
  ("<Launch5> c c" . gptel-context-add)
  ("<Launch5> c n" . gptel-context-next)
  ("<Launch5> c p" . gptel-context-previous)
  ("<Launch5> h" . gptel-highlight-mode)
  ("<Launch5> o" . gptel-mode)
  ("<Launch5> O" . gptel-aibo-mode)
  ("<Launch5> a" . gptel-aibo)
  ("<Launch5> s" . gptel-aibo-summon)
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

  (defun codel-edit-buffer (buffer-name old-string new-string)
    "In BUFFER-NAME, replace OLD-STRING with NEW-STRING."
    (with-current-buffer buffer-name
      (let ((case-fold-search nil))  ;; Case-sensitive search
        (save-excursion
          (goto-char (point-min))
          (let ((count 0))
            (while (search-forward old-string nil t)
              (setq count (1+ count)))
            (if (= count 0)
                (format "Error: Could not find text to replace in buffer %s" buffer-name)
              (if (> count 1)
                  (format "Error: Found %d matches for the text to replace in buffer %s" count buffer-name)
                (goto-char (point-min))
                (search-forward old-string)
                (replace-match new-string t t)
                (format "Successfully edited buffer %s" buffer-name))))))))
  :config
  (require 'gptel-integrations)
  ;; Default settings
  (setq gptel-default-mode 'org-mode
        gptel-api-key my/openai-api-key
        gptel-post-response-functions  'gptel-end-of-response
        gptel-expert-commands t
        gptel-track-media t
        gptel-log-level 'info
        gptel-model 'mistral:latest ;; or 'deepseek-reasoner
        gptel-backend
        (gptel-make-ollama "Ollama" :stream t :host "localhost:11434" :models (get-ollama-models)))
  (gptel-make-deepseek "DeepSeek" :stream t :key my/deepseek-api-key)
  (gptel-make-gh-copilot "Copilot")
  (gptel-make-kagi "Kagi" :key my/kagi-api-key)
  (gptel-make-gemini "Gemini" :stream t :key my/gemini-api-key)
  (gptel-make-anthropic "Claude" :stream t :key my/claude-api-key)
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
  (gptel-make-gh-copilot "Copilot")
  ;; Enable tool use
  (setq gptel-use-tools t)
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "read_buffer"
                :function (lambda (buffer)
                            (unless (buffer-live-p (get-buffer buffer))
                              (error "Error: buffer %s is not live." buffer))
                            (with-current-buffer buffer
                              (buffer-substring-no-properties (point-min) (point-max))))
                :description "Return the contents of an Emacs buffer"
                :args (list '(:name "buffer"
                                    :type string
                                    :description "The name of the buffer whose contents are to be retrieved"))
                :category "emacs"))
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "EditBuffer"
                :function #'codel-edit-buffer
                :description "Edits Emacs buffers"
                :args '((:name "buffer_name"
                               :type string
                               :description "Name of the buffer to modify"
                               :required t)
                        (:name "old_string"
                               :type string
                               :description "Text to replace (must match exactly)"
                               :required t)
                        (:name "new_string"
                               :type string
                               :description "Text to replace old_string with"
                               :required t))
                :category "edit"))
  (setq gptel-directives
        '((default    . "You are a helpful assistant. Be concise and precise.")
          (biophysics . "You are a biophysicist assistant. Be precise about units, statistics, and experimental methodology.")
          (proposal   . "Help write a scientific grant proposal. Use formal academic language. Flag speculative claims. Structure with Specific Aims, Significance, Innovation, and Approach.")
          (brainstorm . "You are a creative scientific collaborator. Challenge assumptions. Suggest unexpected angles. Think across disciplines. Propose unexpected connections and alternative hypotheses. Be explicit about uncertainty and speculation.")
          (review     . "You are a critical peer reviewer. Identify logical gaps, missing controls, unsupported claims, and statistical issues. Be constructive but thorough.")
          (writing    . "You are helping a biophysicist write scientific documents. Use formal academic language. Structure arguments clearly. Flag speculative claims. Prefer precise quantitative statements over vague qualitative ones.")
          (coding     . "You are an expert coding assistant. Provide high-quality code solutions, refactorings, and explanations. Prefer clarity over cleverness.")))


  ;; presets select the right model + backend, system prompt from directives above
  (gptel-make-preset 'writing
    :description "Scientific writing — proposals, manuscripts"
    :backend "Ollama" :model 'qwen3.5:27b
    :system (alist-get 'writing gptel-directives))
  (gptel-make-preset 'brainstorm
    :description "Scientific ideation — explore, challenge, connect"
    :backend "Ollama" :model 'deepseek-r1:32b
    :system (alist-get 'brainstorm gptel-directives))
  (gptel-make-preset 'coding
    :description "Coding with qwen3.5:27b + buffer tools"
    :backend "Ollama" :model 'qwen3.5:27b
    :system (alist-get 'coding gptel-directives)
    :tools '("read_buffer" "EditBuffer"))
  (gptel-make-preset 'review
    :description "Critical peer review — gaps, controls, statistics"
    :backend "Ollama" :model 'qwen3.5:27b
    :system (alist-get 'review gptel-directives))

  (gptel-make-preset 'reasoning
    :description "Deep reasoning with deepseek-r1:32b"
    :backend "Ollama"
    :model 'deepseek-r1:32b)
  (gptel-make-preset 'fast
    :description "Fast iteration with qwen3.5:35b-a3b MoE"
    :backend "Ollama"
    :model 'qwen3.5:35b-a3b)
  (gptel-make-preset 'math
    :description "Math/science with phi4-reasoning:plus"
    :backend "Ollama"
    :model 'phi4-reasoning:plus)
  (gptel-make-preset 'vision
    :description "Multimodal/vision with qwen3-vl:32b"
    :backend "Ollama"
    :model 'qwen3-vl:32b)
  (gptel-make-preset 'copilot
    :description "GitHub Copilot cloud backend"
    :backend "Copilot")
  ;; host-specific overrides: repoint all Ollama presets to laptop models
  (when (string= (system-name) "whisker")
    ;; default backend + model
    (setq gptel-model 'ministral-3:latest
          gptel-backend (gptel-make-ollama "Ollama"
                          :host "localhost:11434"
                          :stream t
                          :models '(ministral-3:latest
                                    qwen3.5:4b
                                    gemma3:latest)))
    (gptel-make-preset 'writing
      :description "Writing with gemma3 — good multilingual Italian/English"
      :backend "Ollama" :model 'gemma3:latest
      :system (alist-get 'writing gptel-directives))
    (gptel-make-preset 'brainstorm
      :description "Brainstorm with ministral-3"
      :backend "Ollama" :model 'ministral-3:latest
      :system (alist-get 'brainstorm gptel-directives))
    (gptel-make-preset 'coding
      :description "Coding with qwen3.5:4b — fastest response for completions"
      :backend "Ollama" :model 'qwen3.5:4b
      :system (alist-get 'coding gptel-directives)
      :tools '("read_buffer" "EditBuffer"))
    (gptel-make-preset 'review
      :description "Peer review with ministral-3 — best quality locally"
      :backend "Ollama" :model 'ministral-3:latest
      :system (alist-get 'review gptel-directives))

    (gptel-make-preset 'reasoning
      :description "Reasoning with ministral-3"
      :backend "Ollama" :model 'ministral-3:latest)
    (gptel-make-preset 'fast
      :description "Fast with qwen3.5:4b"
      :backend "Ollama" :model 'qwen3.5:4b)
    (gptel-make-preset 'math
      :description "Math with ministral-3"
      :backend "Ollama" :model 'ministral-3:latest)
    (gptel-make-preset 'vision
      :description "Vision with gemma3 — only local multimodal option"
      :backend "Ollama" :model 'gemma3:latest)
    )
  :hook
  (gptel-mode . visual-line-mode)  ;; The chats can have long lines.
  (gptel-post-stream-hook . gptel-auto-scroll)  ;; And can be pages long.
  )


(use-package gptel-aibo
  :after gptel
  :config
  (setq gptel-aibo-default-mode 'markdown-mode)
  (setq gptel-aibo-max-buffer-count 55))

(use-package mcp
  :after gptel
  :custom (mcp-hub-servers
           `(;; Local custom scripts
             ("searxng" . (:command "podman" :args ("attach" "--sig-proxy=false" "mcp-searxng")))
             ;; Official & Community Servers
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

(use-package khoj
  :after org
  :bind ("M-s M-k" . #'khoj)
  :custom
  (khoj-server-url "http://127.0.0.1:42110")
  (khoj-server-is-local nil)
  (khoj-auto-setup nil)
  (khoj-auto-index nil)
  (khoj-index-directories '("~/Sync/Grants/" "~/Sync/notes/" "~/Sync/arte/"))
  (khoj-index-files '("~/Sync/todo-khoj.org"))
  :config
  ;; khoj.el 2.x bug: server returns null/empty-array for onlineContext sub-fields.
  ;; :null → (-map f :null) throws "sequencep"; [] → (elt [] 0) throws "Args out of range".
  ;; Strip both before khoj--extract-online-references processes them.
  (advice-add 'khoj--extract-online-references :filter-args
              (lambda (args)
                (list (car args)
                      (mapcar (lambda (query-pair)
                                (cons (car query-pair)
                                      (seq-remove (lambda (r)
                                                    (or (eq (cdr r) :null)
                                                        (and (arrayp (cdr r))
                                                             (= (length (cdr r)) 0))))
                                                  (cdr query-pair))))
                              (cadr args))))))

(use-package inline-diff
  :straight (:repo "https://code.tecosaur.net/tec/inline-diff")
  :after gptel-rewrite) ;or use :defer

;; Updated version available at https://github.com/karthink/gptel/wiki
(use-package gptel-rewrite
  :straight gptel
  :bind (:map gptel-rewrite-actions-map
              ("C-c C-i" . gptel--rewrite-inline-diff))
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

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

(provide 'my-ai)
;;; my-ai.el ends here
