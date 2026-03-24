;;; my-ai.el --- AI/LLM configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Emacs AI stack: gptel (LLM front-end), ellama, MCP tools, Khoj RAG.
;;
;; Key bindings:
;; - C-c C-<return>  : Send to AI (gptel)
;; - C-c e           : Start ellama session
;; - <Launch5> ...   : gptel commands (see :bind below)
;;
;;; Code:

;; --- API Keys ---
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
  (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
  (advice-add 'end-of-buffer :after #'ellama-enable-scroll)
  (setopt ellama-providers
          `(("Ollama gemma3" . ,(make-llm-ollama
                                 :chat-model "gemma3:4b-it-qat"
                                 :embedding-model "nomic-embed-text"
                                 :default-chat-non-standard-params '(("num_ctx" . 8192))))
            ("Ollama LLaVA" . ,(make-llm-ollama
                                :chat-model "llava:latest"
                                :embedding-model "nomic-embed-text"
                                :default-chat-non-standard-params '(("num_ctx" . 4096))))
            ("OpenAI o4-mini" . ,(make-llm-openai
                                  :key my/openai-api-key
                                  :chat-model "o4-mini"
                                  :embedding-model "text-embedding-ada-002"))
            ("Gemini 2.0 Flash" . ,(make-llm-gemini
                                    :key my/gemini-api-key
                                    :chat-model "gemini-2.0-flash"))))
  (setopt ellama-summarization-provider
          (make-llm-ollama
           :chat-model "gemma3:4b-it-qat"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192)))))

(use-package gptel
  :bind (("C-c C-<return>" . gptel-send)
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
         ("<Launch5> l" . my/literature-scan)
         ("<Launch5> h" . gptel-highlight-mode)
         ("<Launch5> o" . gptel-mode)
         ("<Launch5> O" . gptel-aibo-mode)
         ("<Launch5> a" . gptel-aibo)
         ("<Launch5> s" . gptel-aibo-summon))
  :preface
  ;; Machine detection: used for model selection throughout this file.
  (defconst my/on-whisker (string= (system-name) "whisker"))
  (defun my/ollama-model (primary &optional fallback)
    "Return PRIMARY model, or FALLBACK (default: ministral-3:latest) on whisker."
    (if my/on-whisker (or fallback 'ministral-3:latest) primary))

  (defun get-ollama-models ()
    "Fetch the list of installed Ollama models."
    (let* ((output (shell-command-to-string "ollama list"))
           (lines (split-string output "\n" t))
           models)
      (dolist (line (cdr lines))
        (when (string-match "^\\([^[:space:]]+\\)" line)
          (push (match-string 1 line) models)))
      (nreverse models)))

  (defun codel-edit-buffer (buffer-name old-string new-string)
    "In BUFFER-NAME, replace OLD-STRING with NEW-STRING."
    (with-current-buffer buffer-name
      (let ((case-fold-search nil))
        (save-excursion
          (goto-char (point-min))
          (let ((count 0))
            (while (search-forward old-string nil t)
              (setq count (1+ count)))
            (if (= count 0)
                (format "Error: Could not find text to replace in buffer %s" buffer-name)
              (if (> count 1)
                  (format "Error: Found %d matches for text in buffer %s" count buffer-name)
                (goto-char (point-min))
                (search-forward old-string)
                (replace-match new-string t t)
                (format "Successfully edited buffer %s" buffer-name))))))))

  (defvar gptel-zotero-bib-file "~/Sync/biblio/main.bib"
    "Path to Zotero Better BibTeX auto-export file.")

  (defun my/literature-scan (topic)
    "Prepare a one-command literature synthesis prompt for TOPIC.
Opens a dedicated gptel buffer pre-filled with the search-science preset
and the literature synthesis template from grant-synthesis.org.
Review and send with \\[gptel-send]."
    (interactive "sTopic for literature scan: ")
    (let* ((tpl-file (expand-file-name "prompts/grant-synthesis.org" user-emacs-directory))
           (template
            (when (file-readable-p tpl-file)
              (with-temp-buffer
                (insert-file-contents tpl-file)
                (goto-char (point-min))
                (when (re-search-forward "^\\* Literature synthesis" nil t)
                  (forward-line 1)
                  (let ((start (point))
                        (end (or (and (re-search-forward "^\\* " nil t)
                                      (match-beginning 0))
                                 (point-max))))
                    (string-trim (buffer-substring-no-properties start end)))))))
           (body (if template
                     (format "Topic: %s\n\n%s" topic template)
                   (format "Topic: %s\n\nSearch 8-12 papers, call zotero_lookup per DOI, cite as [cite:@Key] or report DOI." topic)))
           (buf (get-buffer-create (format "*literature-scan: %s*" topic))))
      (with-current-buffer buf
        (org-mode)
        (gptel-mode 1)
        (erase-buffer)
        (insert "@search-science\n" body))
      (pop-to-buffer buf)
      (message "Review prompt and send with %s"
               (substitute-command-keys "\\[gptel-send]"))))

  :config
  (require 'gptel-integrations)

  ;; Core settings
  (setq gptel-default-mode 'org-mode
        gptel-api-key my/openai-api-key
        gptel-post-response-functions 'gptel-end-of-response
        gptel-expert-commands t
        gptel-track-media t
        gptel-log-level 'info
        gptel-model (my/ollama-model 'qwen3.5:35b-a3b 'qwen3.5:4b)
        gptel-backend
        (gptel-make-ollama "Ollama"
          :stream t :host "localhost:11434"
          :models (if my/on-whisker
                      '(ministral-3:latest qwen3.5:4b gemma3:latest)
                    (get-ollama-models)))
        gptel-display-buffer-action '((display-buffer-full-frame))
        ;; org-cite: Zotero Better BibTeX auto-export is the primary bibliography
        org-cite-global-bibliography
        (append (when (file-readable-p (expand-file-name gptel-zotero-bib-file))
                  (list (expand-file-name gptel-zotero-bib-file)))
                '("~/Sync/bib/references.bib" "~/bib/library.bib")))

  ;; Cloud backends
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
                               deepseek/deepseek-r1-0528:free))
  (gptel-make-openai "Groq" :stream t :key my/groq-api-key
                     :host "api.groq.com"
                     :endpoint "/openai/v1/chat/completions"
                     :models '(llama-3.3-70b-versatile
                               deepseek-r1-distill-llama-70b
                               qwen-qwq-32b
                               gemma2-9b-it))

  ;; Tools
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
                :args '((:name "buffer"
                               :type string
                               :description "The name of the buffer whose contents are to be retrieved"))
                :category "emacs"))
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "EditBuffer"
                :function #'codel-edit-buffer
                :description "Replace OLD-STRING with NEW-STRING in an Emacs buffer"
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

  ;; System prompt directives (select via gptel-menu or presets below)
  (setq gptel-directives
        '((default    . "You are a helpful assistant. Be concise and precise.")
          (biophysics . "You are a biophysicist assistant. Be precise about units, statistics, and experimental methodology.")
          (proposal   . "You are helping write a scientific grant proposal. Use formal academic language. Flag speculative claims. Be precise and quantitative. Follow the structure provided in the user's prompt or context — do not impose a default structure.\n\nFor citations: call zotero_lookup with the DOI — if found, cite as [cite:@Key]; if not, report: DOI: 10.xxxx/xxx (user will add it manually). Never invent citation keys.")
          (brainstorm . "You are a creative scientific collaborator. Challenge assumptions. Suggest unexpected angles. Think across disciplines. Propose unexpected connections and alternative hypotheses. Be explicit about uncertainty and speculation.")
          (review     . "You are a critical peer reviewer. Identify logical gaps, missing controls, unsupported claims, and statistical issues. Be constructive but thorough.")
          (writing    . "You are helping a biophysicist write scientific documents. Use formal academic language. Structure arguments clearly. Flag speculative claims. Prefer precise quantitative statements over vague qualitative ones.\n\nFor citations: call zotero_lookup with the DOI — if found, cite as [cite:@Key]; if not, report: DOI: 10.xxxx/xxx (user will add it manually). Never invent citation keys.")
          (coding     . "You are an expert coding assistant. Provide high-quality code solutions, refactorings, and explanations. Prefer clarity over cleverness.")))

  ;; --- Presets ---
  ;; my/ollama-model selects the appropriate local model per machine.

  (gptel-make-preset 'writing
                     :description "Scientific writing - proposals, manuscripts"
                     :backend "Ollama" :model (my/ollama-model 'qwen3.5:27b 'gemma3:latest)
                     :system (alist-get 'writing gptel-directives)
                     :pre (lambda () (gptel-mcp-connect '("pdf") 'sync))
                     :tools '(:append ("zotero_lookup")))

  (gptel-make-preset 'brainstorm
                     :description "Scientific ideation - explore, challenge, connect"
                     :backend "Ollama" :model (my/ollama-model 'deepseek-r1:32b)
                     :system (alist-get 'brainstorm gptel-directives))

  (gptel-make-preset 'coding
                     :description "Coding - refactor, review, buffer editing"
                     :backend "Ollama" :model (my/ollama-model 'qwen3.5:27b 'gemma3:latest)
                     :system (alist-get 'coding gptel-directives)
                     :tools '("read_buffer" "EditBuffer"))

  (gptel-make-preset 'review
                     :description "Critical peer review - gaps, controls, statistics"
                     :backend "Ollama" :model (my/ollama-model 'qwen3.5:27b 'gemma3:latest)
                     :system (alist-get 'review gptel-directives))

  (gptel-make-preset 'reasoning
                     :description "Deep reasoning - chain-of-thought, hard problems"
                     :backend "Ollama" :model (my/ollama-model 'deepseek-r1:32b))

  (gptel-make-preset 'fast
                     :description "Fast iteration - MoE, low latency"
                     :backend "Ollama" :model (my/ollama-model 'qwen3.5:35b-a3b 'qwen3.5:4b))

  (gptel-make-preset 'math
                     :description "Math / science reasoning"
                     :backend "Ollama" :model (my/ollama-model 'phi4-reasoning:plus))

  (gptel-make-preset 'vision
                     :description "Multimodal / vision"
                     :backend "Ollama" :model (my/ollama-model 'qwen3-vl:32b 'gemma3:latest))

  (gptel-make-preset 'copilot
                     :description "GitHub Copilot cloud backend"
                     :backend "Copilot")

  (gptel-make-preset 'search
                     :description "Web search - SearxNG + fetch via MCP"
                     :backend "Ollama" :model (my/ollama-model 'qwen3.5:35b-a3b 'qwen3.5:4b)
                     :system "Use the provided tools to search the web for up-to-date information. Always cite sources with URL and title."
                     :pre (lambda () (gptel-mcp-connect '("searxng" "fetcher") 'sync))
                     :tools '(:append ("searxng_web_search" "web_url_read" "fetch_url")))

  (gptel-make-preset 'search-science
                     :description "Scientific literature search - PubMed / arXiv / Scholar via MCP"
                     :backend "Ollama" :model (my/ollama-model 'qwen3.5:35b-a3b 'qwen3.5:4b)
                     :system "You are a scientific literature assistant. Use searxng_web_search to find peer-reviewed literature. Prefer PubMed, arXiv, Google Scholar, and Semantic Scholar.\n\nFor each paper found:\n1. Extract the DOI from the result URL or metadata\n2. Call zotero_lookup with the DOI — if found, cite as [cite:@Key]\n3. If not in Zotero, report it as: DOI: 10.xxxx/xxx (user will add it to Zotero manually)\n\nHighlight knowledge gaps and translational relevance. Never invent citations."
                     :pre (lambda () (gptel-mcp-connect '("searxng" "fetcher" "pdf") 'sync))
                     :tools '(:append ("searxng_web_search" "web_url_read" "fetch_url" "zotero_lookup")))

  (gptel-make-preset 'grant
                     :description "Grant writing - lit search + structured proposal sections"
                     :backend "Ollama" :model (my/ollama-model 'qwen3.5:27b 'gemma3:latest)
                     :system (alist-get 'proposal gptel-directives)
                     :pre (lambda () (gptel-mcp-connect '("searxng" "fetcher" "pdf") 'sync))
                     :tools '(:append ("searxng_web_search" "web_url_read" "fetch_url" "zotero_lookup")))

  (gptel-make-preset 'pdf
                     :description "Local PDF reader - extract text, cite via MCP"
                     :backend "Ollama" :model (my/ollama-model 'qwen3.5:35b-a3b 'qwen3.5:4b)
                     :system "You have access to read_pdf, extract_doi, and zotero_lookup tools.\n\nCitation workflow:\n1. Call read_pdf with the absolute path — the header shows filename, pages, and DOI if found\n2. Call zotero_lookup with the DOI (and/or filename) to find the entry in your Zotero library\n3. If found, cite as [cite:@Key]\n4. If not found, report: DOI: 10.xxxx/xxx (user will add it to Zotero manually)\n\nNever invent citations."
                     :pre (lambda () (gptel-mcp-connect '("pdf") 'sync)))

  (gptel-make-preset 'pdf-science
                     :description "PDF + literature search - read papers, search, cite"
                     :backend "Ollama" :model (my/ollama-model 'qwen3.5:35b-a3b 'qwen3.5:4b)
                     :system "You are a scientific research assistant with PDF reading and web search tools.\n\nFor PDFs:\n1. Call read_pdf to extract text (DOI appears in the header)\n2. Call zotero_lookup with the DOI and/or filename\n3. If found, cite as [cite:@Key]; if not found, report: DOI: 10.xxxx/xxx\n\nFor web search:\n1. Use searxng_web_search — prefer PubMed, arXiv, Google Scholar, Semantic Scholar\n2. Extract the DOI; call zotero_lookup — cite as [cite:@Key] if found, else report the DOI\n\nNever invent citations. Never invent citation keys."
                     :pre (lambda () (gptel-mcp-connect '("pdf" "searxng" "fetcher") 'sync))
                     :tools '(:append ("searxng_web_search" "web_url_read" "fetch_url" "zotero_lookup")))

  :hook
  ((gptel-mode . visual-line-mode)
   (gptel-post-stream-hook . gptel-auto-scroll)))

(use-package gptel-aibo
  :after gptel
  :config
  (setq gptel-aibo-default-mode 'markdown-mode)
  (setq gptel-aibo-max-buffer-count 55))

(use-package mcp
  :after gptel
  :custom (mcp-hub-servers
           `(;; Local scripts
             ("searxng" . (:command "podman" :args ("exec" "-i" "mcp-searxng" "node" "dist/index.js")))
             ("pdf" . (:command "uv"
                                :args ("run" "--with" "pymupdf"
                                       "/home/dan/.local/bin/pdf-mcp.py")
                                :env (:ZOTERO_BIB_FILE "~/Sync/biblio/main.bib")))
             ;; Official & community servers
             ("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,(getenv "HOME"))))
             ("fetcher" . (:command "npx" :args ("-y" "fetcher-mcp")))
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
             ("graphlit" . (:command "npx"
                                     :args ("-y" "graphlit-mcp-server")
                                     :env (:GRAPHLIT_ORGANIZATION_ID "b2821f53-fda4-4ac1-8c25-5312d4807139"
                                           :GRAPHLIT_ENVIRONMENT_ID "e48c582c-3523-4833-9e5f-037d87cf510b"
                                           :GRAPHLIT_JWT_SECRET "PjpJX7IRDdsMjQkc8pDxjHbF4LkyO8tBLTFTK/S1IqI=")))))
  :config (require 'mcp-hub))

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
  ;; khoj.el 2.x: server may return :null or [] for onlineContext sub-fields,
  ;; causing crashes in khoj--extract-online-references. Strip them first.
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
  :after gptel-rewrite)

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
                 (inline-diff-words ov-beg ov-end response)))))
  (when (boundp 'gptel--rewrite-dispatch-actions)
    (add-to-list
     'gptel--rewrite-dispatch-actions '(?i "inline-diff")
     'append)))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

(provide 'my-ai)
;;; my-ai.el ends here
