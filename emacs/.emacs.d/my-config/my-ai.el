;;; my-ai.el --- AI/LLM configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Emacs AI stack: gptel (LLM front-end), ellama, MCP tools, Khoj RAG.
;;
;; Key bindings:
;; - C-c C-<return>  : Send to AI (gptel)
;; - C-c e           : Start ellama session
;; - <Launch5> ...   : gptel commands (see :bind below)
;;   <Launch5> l     : Literature scan (search-science preset + template)
;;   <Launch5> v     : Vane search (AI web search via /api/chat, localhost:3000)
;;   <Launch5> n     : Save gptel buffer as org-roam AI note
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
(defconst my/on-whisker (string= (system-name) "whisker")
  "Whether this Emacs instance runs on the whisker host.")

(defconst my/ollama-host "localhost:11434"
  "Ollama host used by local Emacs AI clients.")

(defconst my/vane-base-url "http://localhost:3000"
  "Vane base URL.")

(defconst my/khoj-server-url "http://127.0.0.1:42110"
  "Khoj base URL for the local Emacs client.")

(defconst my/ollama-light-model 'ministral-3:latest
  "Lightweight Ollama fallback model.")

(defconst my/ollama-fast-model 'qwen3.6:35b-a3b
  "Fast general-purpose Ollama model.

Deliberately still the MoE: only ~3B parameters are active per token, so it
generates at 106 tok/s against qwen3.8's 58.7. It is the right pick where
throughput dominates (agentic loops, search); qwen3.8 wins on total latency
for short tool-calling turns because it emits far fewer tokens.")

(defconst my/ollama-fast-fallback 'qwen3.5:4b
  "Fallback fast Ollama model for smaller hosts.")

(defconst my/ollama-writing-model 'qwen3.8:latest
  "Primary Ollama model for writing/coding tasks.

Replaced qwen3.6:27b (2026-08-27). Same class — dense ~27B, Q4_K_M, 262144
context, tools + thinking — but measured on a tool-calling turn it runs at
58.7 tok/s against 23.0, and reasons far more tersely (94 vs 610 characters
of thinking), so ~8x better wall-clock. It also ships a real CLIP projector,
which qwen3.6:27b advertises but does not carry. Needs ollama >= 0.32.12.

`:latest' is a floating tag, unlike the pinned tags elsewhere here; it is the
only one published for this model so far.")

(defconst my/ollama-writing-fallback 'gemma4:e4b
  "Fallback Ollama model for writing/coding tasks.")

(defconst my/ollama-reasoning-model 'qwen3.8:latest
  "Primary local reasoning model; enable thinking mode when needed.
See `my/ollama-writing-model' for why this is no longer qwen3.6:27b.")

(defconst my/ollama-vision-model 'qwen3.8:latest
  "Primary local multimodal model.

Replaced qwen3-vl:32b (2026-08-27), which was 20GB for strictly worse
behaviour: on the same generated test image both read it correctly (red
circle, blue square, the text \"K=47\"), but qwen3-vl needed 41.9s against
22.4s, and 4.9 tok/s against 62.6 on a tool-calling turn. qwen3.8 carries a
real CLIP projector, so this is the same model as
`my/ollama-writing-model' on purpose — the constant is kept to name the
role, not to point somewhere different.")

(defconst my/ollama-embedding-model "qwen3-embedding:latest"
  "Primary local embedding model shared with Vane and Khoj.")

(defconst my/vane-chat-model (symbol-name my/ollama-fast-model)
  "Ollama chat model key to use for Vane queries.")

(defconst my/vane-embedding-model my/ollama-embedding-model
  "Ollama embedding model key to use for Vane queries.")

(defun my/ollama-model (primary &optional fallback)
  "Return PRIMARY model, or FALLBACK on whisker.

If FALLBACK is nil, use `my/ollama-light-model'."
  (if my/on-whisker (or fallback my/ollama-light-model) primary))

(defun my/ollama-model-name (primary &optional fallback)
  "Return `my/ollama-model' as a string for llm.el providers."
  (symbol-name (my/ollama-model primary fallback)))

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
           :chat-model (my/ollama-model-name my/ollama-fast-model my/ollama-fast-fallback)
           :embedding-model my/ollama-embedding-model
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
          `(("Ollama local" . ,(make-llm-ollama
                                :chat-model (my/ollama-model-name my/ollama-fast-model my/ollama-fast-fallback)
                                :embedding-model my/ollama-embedding-model
                                :default-chat-non-standard-params '(("num_ctx" . 8192))))
            ("Ollama vision" . ,(make-llm-ollama
                                 :chat-model (my/ollama-model-name my/ollama-vision-model my/ollama-writing-fallback)
                                 :embedding-model my/ollama-embedding-model
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
           :chat-model (my/ollama-model-name my/ollama-fast-model my/ollama-fast-fallback)
           :embedding-model my/ollama-embedding-model
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
         ("<Launch5> v" . my/vane-search)
         ("<Launch5> n" . my/gptel-capture-to-roam)
         ("<Launch5> h" . gptel-highlight-mode)
         ("<Launch5> o" . gptel-mode)
         ("<Launch5> O" . gptel-aibo-mode)
         ("<Launch5> a" . gptel-aibo)
         ("<Launch5> s" . gptel-aibo-summon))
  :preface
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

  ;; --- Vane (AI-powered web search) ---
  (defvar my/vane-focus-mode "academicSearch"
    "Vane focus mode.
Options: webSearch, academicSearch, writingAssistant, wolframAlphaSearch.")

  (defvar my/vane--provider-id-cache nil
    "Cached Vane provider ID for `my/vane-chat-model'.")

  (defun my/vane--provider-id ()
    "Return Vane Ollama provider ID for `my/vane-chat-model', cached."
    (or my/vane--provider-id-cache
        (let* ((raw (shell-command-to-string
                     (format "curl -s %s/api/config" my/vane-base-url)))
               (cfg (ignore-errors
                      (let ((json-object-type 'alist))
                        (json-read-from-string raw))))
               (providers (alist-get 'modelProviders (alist-get 'values cfg))))
          (setq my/vane--provider-id-cache
                (catch 'found
                  (seq-do (lambda (p)
                            (when (seq-some (lambda (m)
                                              (equal (alist-get 'key m)
                                                     my/vane-chat-model))
                                            (alist-get 'chatModels p))
                              (throw 'found (alist-get 'id p))))
                          (or providers []))
                  nil)))))

  (defun my/vane--parse-ndjson (raw)
    "Parse Vane NDJSON streaming RAW into (MESSAGE . SOURCES).
Only `text'-type blocks are collected; non-string block data (subSteps,
images, etc.) is silently ignored.  sources events carry citations."
    (let ((blocks (make-hash-table :test 'equal))
          sources)
      (dolist (line (split-string raw "\n"))
        (let ((d (ignore-errors
                   (let ((json-object-type 'alist))
                     (json-read-from-string line)))))
          (when d
            (pcase (alist-get 'type d)
              ("block"
               (let ((b (alist-get 'block d)))
                 ;; Only store text blocks — other block types carry arrays/alists
                 (when (equal (alist-get 'type b) "text")
                   (puthash (alist-get 'id b) (or (alist-get 'data b) "") blocks))))
              ("updateBlock"
               (seq-do (lambda (patch)
                         (when (and (equal (alist-get 'op patch) "replace")
                                    (equal (alist-get 'path patch) "/data"))
                           (let ((val (alist-get 'value patch)))
                             ;; Only update if we already have this block (text type)
                             ;; and the new value is a string
                             (when (and (gethash (alist-get 'blockId d) blocks)
                                        (stringp val))
                               (puthash (alist-get 'blockId d) val blocks)))))
                       (alist-get 'patch d)))
              ("sources"
               (setq sources (alist-get 'data d)))))))
      (let (texts)
        (maphash (lambda (_k v) (when (stringp v) (push v texts))) blocks)
        (cons (string-trim (mapconcat #'identity (nreverse texts) "\n\n"))
              sources))))

  (defun my/vane-search (query)
    "Search Vane with QUERY; display results in an org buffer.
Requires Vane running at `my/vane-base-url' (default: localhost:3000).
Results include the answer and a clickable sources list."
    (interactive "sVane search: ")
    (let ((provider-id (my/vane--provider-id)))
      (unless provider-id
        (user-error "Vane: provider for %s not found — is Vane running?"
                    my/vane-chat-model))
      (let* ((buf (get-buffer-create (format "*vane: %s*" query)))
             (payload (json-encode
                       `(("message" . (("messageId" . ,(format "%d" (abs (random))))
                                       ("chatId"    . ,(format "%d" (abs (random))))
                                       ("content"   . ,query)))
                         ("history" . [])
                         ("focusMode" . ,my/vane-focus-mode)
                         ("optimizationMode" . "balanced")
                         ("chatModel"
                          . (("providerId" . ,provider-id)
                             ("key"        . ,my/vane-chat-model)))
                         ("embeddingModel"
                          . (("providerId" . ,provider-id)
                             ("key"        . ,my/vane-embedding-model)))))))
        (with-current-buffer buf
          (org-mode)
          (erase-buffer)
          (insert (format "* Vane: %s\n\n/Searching…/\n" query)))
        (pop-to-buffer buf)
        (make-process
         :name "vane-search"
         :buffer (generate-new-buffer " *vane-raw*")
         :command (list "curl" "-s" "-X" "POST"
                        "-H" "Content-Type: application/json"
                        "-d" payload
                        (concat my/vane-base-url "/api/chat"))
         :sentinel
         (lambda (proc _event)
           (when (eq (process-status proc) 'exit)
             (let ((raw (with-current-buffer (process-buffer proc)
                          (buffer-string))))
               (kill-buffer (process-buffer proc))
               (pcase-let ((`(,message . ,sources) (my/vane--parse-ndjson raw)))
                 (with-current-buffer buf
                   (erase-buffer)
                   (org-mode)
                   (insert (format "* Vane: %s\n\n" query))
                   (insert (if (string-empty-p message)
                               "/No response — check Vane logs: aic logs vane/\n"
                             message))
                   (when (and sources (> (length sources) 0))
                     (insert "\n\n** Sources\n\n")
                     (seq-do (lambda (src)
                               (let* ((meta  (alist-get 'metadata src))
                                      (title (or (alist-get 'title meta) "Unknown"))
                                      (url   (or (alist-get 'url meta) "#")))
                                 (insert (format "- [[%s][%s]]\n" url title))))
                             sources)))))))))))

  ;; --- Org-roam capture from gptel buffers ---
  (defun my/gptel-capture-to-roam (title)
    "Save current buffer content as a new org-roam AI note titled TITLE.
Creates the file in `org-roam-directory'/ai-notes/ with `:ai:' filetag.
[cite:@Key] citations in the content automatically become backlinks in
the org-roam graph via `citar-org-roam-mode' (already enabled)."
    (interactive
     (list (read-string "Roam note title: "
                        (when (string-match "\\*\\(.+?\\)\\*" (buffer-name))
                          (match-string 1 (buffer-name))))))
    (require 'org-roam)
    (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
           (slug (downcase (replace-regexp-in-string "[^a-z0-9]+" "-" title)))
           (date (format-time-string "%Y%m%d"))
           (file (expand-file-name
                  (format "ai-notes/%s-%s.org" date slug)
                  org-roam-directory))
           (id (org-id-new)))
      (make-directory (file-name-directory file) t)
      (with-current-buffer (find-file-noselect file)
        (erase-buffer)
        (insert ":PROPERTIES:\n:ID: " id "\n:END:\n"
                "#+title: " title "\n"
                "#+filetags: :ai:\n"
                "#+date: [" (format-time-string "%Y-%m-%d") "]\n\n"
                content)
        (save-buffer)
        (when (fboundp 'org-roam-db-update-file)
          (org-roam-db-update-file)))
      (find-file file)
      (message "Saved org-roam note: %s" (file-name-nondirectory file))))

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
        gptel-model (my/ollama-model my/ollama-fast-model my/ollama-fast-fallback)
        gptel-backend
        (gptel-make-ollama "Ollama"
          :stream t :host my/ollama-host
          ;; `delete-dups' because the writing and vision roles now resolve to
          ;; the same model; a duplicated entry shows up twice in the menu.
          :models (if my/on-whisker
                      (delete-dups
                       (list my/ollama-light-model
                             my/ollama-fast-model
                             my/ollama-writing-model
                             my/ollama-writing-fallback
                             my/ollama-vision-model))
                    (get-ollama-models)))
        gptel-display-buffer-action '((display-buffer-full-frame))
        ;; org-cite: all Zotero Better BibTeX auto-exports are the primary bibliography
        org-cite-global-bibliography
        (seq-filter #'file-readable-p
                    (mapcar #'expand-file-name completion-bibliography)))

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
                               :description "Name of the buffer to modify")
                        (:name "old_string"
                               :type string
                               :description "Text to replace (must match exactly)")
                        (:name "new_string"
                               :type string
                               :description "Text to replace old_string with"))
                :category "edit"))

  ;; System prompt directives (select via gptel-menu or presets below)
  (setq gptel-directives
        '((default    . "You are a helpful assistant. Be concise and precise.")
          (biophysics . "You are a biophysicist assistant. Be precise about units, statistics, and experimental methodology.")
          (proposal   . "You are helping write a scientific grant proposal. Use formal academic language. Flag speculative claims. Be precise and quantitative. Follow the structure provided in the user's prompt or context — do not impose a default structure.")
          (brainstorm . "You are a creative scientific collaborator. Challenge assumptions. Suggest unexpected angles. Think across disciplines. Propose unexpected connections and alternative hypotheses. Be explicit about uncertainty and speculation.")
          (review     . "You are a critical peer reviewer. Identify logical gaps, missing controls, unsupported claims, and statistical issues. Be constructive but thorough.")
          (writing    . "You are helping a biophysicist write scientific documents. Use formal academic language. Structure arguments clearly. Flag speculative claims. Prefer precise quantitative statements over vague qualitative ones.")
          (coding     . "You are an expert coding assistant. Provide high-quality code solutions, refactorings, and explanations. Prefer clarity over cleverness.")))

  ;; --- Preset helpers ---
  ;; my/ollama-model selects the appropriate local model per machine.

  (defun my/gptel-mcp-reset-tools ()
    "Unregister all MCP-provided tools from `gptel-tools', without stopping servers.

Unlike calling `gptel-mcp-disconnect' with no arguments and
INTERACTIVE nil, this never falls through to killing already-running
MCP server processes: when no MCP tool categories are currently
registered, `gptel-mcp-disconnect' would otherwise call
`mcp-stop-server' on every live server in `mcp-server-connections'
non-interactively (no prompt), which could kill an MCP server started
ad hoc outside of a preset. This wrapper only calls
`gptel-mcp-disconnect' when there is something registered to remove."
    (when (cl-some (lambda (cat) (string-prefix-p "mcp-" (car cat)))
                    gptel--known-tools)
      (gptel-mcp-disconnect)))

  (defun my/gptel-mcp-reap-dead-servers ()
    "Mark MCP servers whose process has died as stopped.

`mcp--status' is only ever set to `stop' by `mcp-stop-server', so a
server whose process died keeps the status `connected' forever.  That
stale status is load-bearing in two places: `gptel-mcp-connect' decides
a server needs no restart when its status is `connected', and
`gptel-mcp--get-tools' returns a server's cached tool list under the
same condition.  A dead-but-`connected' entry therefore gets its tools
re-registered without the process ever being restarted, and every tool
call then fails silently.

Stopping the dead ones here makes `mcp--server-running-p' return nil for
them, so the next `gptel-mcp-connect' actually restarts them.  Live
servers are never touched, which is the property `my/gptel-mcp-reset-tools'
exists to protect."
    (when (boundp 'mcp-server-connections)
      (maphash (lambda (name conn)
                 (when (and conn (not (jsonrpc-running-p conn)))
                   (mcp-stop-server name)))
               mcp-server-connections)))

  (defun my/gptel-tools-if-available (names)
    "Return a `:tools' spec resolving to whichever of NAMES are available.

Unlike a plain `:tools' list, this never signals a `user-error' if one
of NAMES fails to resolve (e.g. its MCP server failed to start/connect
this time) — it drops that tool instead of aborting the whole preset
application (which would otherwise leave `gptel-tools' and the system
prompt out of sync: `:system' is applied before `:tools', so a `:tools'
error still leaves the new system prompt in place with the OLD tool set).

Dropped tools are reported via `message'. The usual cause is an MCP
server that failed to start or connect, and a silently empty tool set is
indistinguishable from a preset that deliberately asks for no tools."
    (list :function
          (lambda (_current)
            (let* ((found (seq-filter (lambda (name) (ignore-errors (gptel-get-tool name)))
                                      names))
                   (missing (seq-difference names found)))
              (when missing
                (message "gptel preset: %d tool(s) unavailable (MCP server not connected?): %s"
                         (length missing) (string-join missing ", ")))
              found))))

  (defun my/gptel-mcp-pre (&rest servers)
    "Return a preset `:pre' function that connects exactly SERVERS.

Every preset below uses this, so applying a preset first unregisters the
MCP tools of the previously active one and then connects only the servers
it needs.  Together with each preset's explicit, exclusive `:tools' list
\(never `:append', and resolved leniently via `my/gptel-tools-if-available')
this means switching presets always yields exactly that preset's tool set,
never a union with whatever was active before.  See `gptel--modify-value':
a plain (non-:append) `:tools' value fully replaces `gptel-tools', while
`:append' merges onto the current value, which is how tools used to leak
across preset switches.

Every preset must declare `:system' for the same reason: a preset that
omits a key does not reset that key, it inherits whatever the previously
active preset left there."
    (lambda ()
      (my/gptel-mcp-reset-tools)
      (my/gptel-mcp-reap-dead-servers)
      ;; One `gptel-mcp-connect' call per server, deliberately.  Given a batch,
      ;; `mcp-hub-start-all-server' only invokes the callback that registers
      ;; tools once *every* server in that batch has started, and a server that
      ;; fails asynchronously never increments its counter — so one unhealthy
      ;; server silently costs you the tools of all the healthy ones too.
      (dolist (server servers)
        (condition-case err
            (gptel-mcp-connect (list server) 'sync)
          (error (message "gptel preset: MCP server %s failed to connect: %s"
                          server (error-message-string err)))))))

  ;; Presets differ mostly in their system prompt, not in which tools they
  ;; need, so the shared tool sets live here instead of being repeated.
  (defconst my/gptel-citation-workflow
    "Citation workflow — never invent citations or citation keys:
1. Obtain the DOI: from a search result's URL or metadata, or from read_pdf's
   header, which reports filename, pages and the DOI if one was found.
2. Call zotero_lookup with that DOI (and/or the filename).
3. Found → cite as [cite:@Key].  Not found → report: DOI: 10.xxxx/xxx, and the
   user will add it to Zotero manually.

Local PDFs arrive as an absolute path, never as file contents in the
conversation — call read_pdf with that path to read one."
    "Shared `:system' tail for every preset carrying read_pdf/zotero_lookup.

Kept out of `gptel-directives' on purpose: a directive selected on its own
from `gptel-menu' sets no tools, so naming zotero_lookup there promises a
tool that is not present.  Presets own their tools, so presets own this.")

  (defconst my/gptel-web-tools '("searxng_web_search" "web_url_read" "fetch_url")
    "Web search and page fetch tools, from the searxng and fetcher MCP servers.")

  (defconst my/gptel-lit-tools
    (append my/gptel-web-tools
            '("read_pdf" "zotero_lookup" "zotero_search_items" "zotero_get_item_fulltext"))
    "Literature tools: web search plus PDF reading and Zotero citation lookup.")

  (defconst my/gptel-pdf-tools '("read_pdf" "extract_doi" "zotero_lookup")
    "Local PDF reading and citation tools, all from the pdf MCP server.")

  ;; --- Presets ---
  ;; IMPORTANT: `@preset' typed into a prompt is a *request-time* transform.
  ;; `gptel--transform-apply-preset' applies it inside the throwaway
  ;; " *gptel-prompt*" buffer, so it never touches this buffer's `gptel-tools'
  ;; and the tool menu will not list its tools.  To configure the session,
  ;; load the preset from `gptel-menu' with `@' (`gptel-preset').
  ;;
  ;; Models and backends are picked in `gptel-menu' (-m / -b).  A preset exists
  ;; only to bundle a system prompt with the tools that prompt actually needs,
  ;; so there are no model-only presets.

  (gptel-make-preset 'writing
                     :description "Scientific writing - proposals, manuscripts"
                     :backend "Ollama" :model (my/ollama-model my/ollama-writing-model my/ollama-writing-fallback)
                     ;; Same reasoning as `grant': the directive says to cite via
                     ;; zotero_lookup, but the DOI has to come from somewhere, and a
                     ;; PDF cannot be attached as context to a text-only Ollama model.
                     :system (concat (alist-get 'writing gptel-directives)
                                     "\n\n" my/gptel-citation-workflow)
                     :pre (my/gptel-mcp-pre "pdf")
                     :tools (my/gptel-tools-if-available '("read_pdf" "zotero_lookup")))

  (gptel-make-preset 'brainstorm
                     :description "Scientific ideation - explore, challenge, connect"
                     :backend "Ollama" :model (my/ollama-model my/ollama-reasoning-model my/ollama-fast-model)
                     :system (alist-get 'brainstorm gptel-directives)
                     :pre (my/gptel-mcp-pre)
                     :tools nil)

  (gptel-make-preset 'review
                     :description "Critical peer review - gaps, controls, statistics"
                     :backend "Ollama" :model (my/ollama-model my/ollama-writing-model my/ollama-writing-fallback)
                     :system (alist-get 'review gptel-directives)
                     :pre (my/gptel-mcp-pre)
                     :tools nil)

  (gptel-make-preset 'coding
                     :description "Coding - refactor, review, buffer editing, repo + GitHub + docs via MCP"
                     :backend "Ollama" :model (my/ollama-model my/ollama-writing-model my/ollama-writing-fallback)
                     :system (alist-get 'coding gptel-directives)
                     ;; github is filtered to a subset: unfiltered it contributes ~45
                     ;; tools, and a 62-schema payload is more than a local model
                     ;; handles well.  `gptel-mcp-connect' accepts the
                     ;; ("server" "tool"...) form, which `my/gptel-mcp-pre' passes
                     ;; through unchanged — extend the list to taste.
                     :pre (my/gptel-mcp-pre
                           "filesystem" "context7"
                           '("github" "search_code" "get_file_contents" "list_issues"
                             "create_pull_request" "get_me"))
                     ;; `:append' is intentional here: `:pre' just reset MCP tools to
                     ;; exactly the servers above, so this only adds the buffer-local
                     ;; tools on top of that known-clean base.
                     :tools '(:append ("read_buffer" "EditBuffer")))

  (gptel-make-preset 'search
                     :description "Web search - SearxNG + fetch via MCP"
                     :backend "Ollama" :model (my/ollama-model my/ollama-fast-model my/ollama-fast-fallback)
                     :system "Use the provided tools to search the web for up-to-date information. Always cite sources with URL and title."
                     :pre (my/gptel-mcp-pre "searxng" "fetcher")
                     :tools (my/gptel-tools-if-available my/gptel-web-tools))

  (gptel-make-preset 'search-science
                     :description "Literature - search papers, read PDFs, cite via Zotero"
                     :backend "Ollama" :model (my/ollama-model my/ollama-fast-model my/ollama-fast-fallback)
                     :system (concat "You are a scientific literature assistant with web search and"
                                     " PDF reading tools. Use searxng_web_search to find peer-reviewed"
                                     " literature — prefer PubMed, arXiv, Google Scholar and Semantic"
                                     " Scholar. Highlight knowledge gaps and translational relevance.\n\n"
                                     my/gptel-citation-workflow)
                     :pre (my/gptel-mcp-pre "searxng" "fetcher" "pdf" "zotero")
                     :tools (my/gptel-tools-if-available my/gptel-lit-tools))

  (gptel-make-preset 'grant
                     :description "Grant writing - lit search + structured proposal sections"
                     :backend "Ollama" :model (my/ollama-model my/ollama-writing-model my/ollama-writing-fallback)
                     ;; The `proposal' directive covers prose and citations but says
                     ;; nothing about local files.  Ollama models are text-only, so a
                     ;; PDF cannot be attached with `gptel-add-file' (gptel gates binary
                     ;; context on the model declaring `media' + the application/pdf
                     ;; MIME type).  Reading it with the read_pdf tool is the supported
                     ;; path, so the prompt has to actually mention that it exists.
                     :system (concat (alist-get 'proposal gptel-directives)
                                     "\n\n" my/gptel-citation-workflow)
                     :pre (my/gptel-mcp-pre "searxng" "fetcher" "pdf" "zotero")
                     :tools (my/gptel-tools-if-available my/gptel-lit-tools))

  (gptel-make-preset 'grant-landscape
                     :description "Grant landscape - funders, calls, competing awards via Exa (semantic web search)"
                     :backend "Ollama" :model (my/ollama-model my/ollama-writing-model my/ollama-writing-fallback)
                     :system "You are a research funding analyst. Use Exa's web search and fetch tools to find funding-agency calls, program priorities, and comparable or competing awarded grants (e.g. NIH RePORTER, CORDIS, ERC, national funders). This is landscape and competitive-intelligence research, not peer-reviewed literature — do not treat results as citable scientific sources or route them through Zotero. Report the source URL and publication/award date for every claim. Flag anything that looks outdated or unconfirmed."
                     :pre (my/gptel-mcp-pre "exa")
                     :tools (my/gptel-tools-if-available '("web_search_exa" "web_fetch_exa")))

  (gptel-make-preset 'pdf
                     :description "Local PDF reader - extract text, cite via MCP"
                     :backend "Ollama" :model (my/ollama-model my/ollama-fast-model my/ollama-fast-fallback)
                     :system (concat "You read local PDFs and cite them. read_pdf extracts the"
                                     " text of a PDF at an absolute path.\n\n"
                                     my/gptel-citation-workflow)
                     :pre (my/gptel-mcp-pre "pdf")
                     :tools (my/gptel-tools-if-available my/gptel-pdf-tools))

  (gptel-make-preset 'copilot
                     :description "GitHub Copilot cloud backend"
                     :backend "Copilot"
                     ;; `:system' is as load-bearing as `:tools' here.  A preset that
                     ;; omits it inherits the previously active preset's prompt, so
                     ;; switching pdf -> copilot used to tell Copilot it "has access to
                     ;; read_pdf, extract_doi and zotero_lookup" while holding no tools.
                     :system (alist-get 'default gptel-directives)
                     :pre (my/gptel-mcp-pre)
                     :tools nil)

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
             ("searxng" . (:command "podman" :args ("exec" "-i" "mcp-searxng" "node" "dist/cli.js")))
             ("pdf" . (:command "uv"
                                :args ("run" "--with" "pymupdf"
                                       "/home/dan/.local/bin/pdf-mcp.py")
                                :env (:ZOTERO_BIB_FILES ,(mapconcat #'identity completion-bibliography ":"))))
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
             ;; Zotero local MCP — talks to running Zotero desktop via local API
             ;; Installed: uv tool install zotero-mcp-server[semantic]
             ;; Setup run:  zotero-mcp setup  (configured ZOTERO_LOCAL=true)
             ;; Requires Zotero desktop to be running
             ("zotero" . (:command "/home/dan/.local/bin/zotero-mcp"
                                   :env (:ZOTERO_LOCAL "true"
                                                       :ZOTERO_EMBEDDING_MODEL "default")))
             ("duckduckgo" . (:command "uvx" :args ("duckduckgo-mcp-server")))
             ;; Exa neural/semantic web search — hosted API, no container needed.
             ;; Requires its own key (Claude Code's exa plugin auths differently):
             ;;   pass insert cloud/exa_api_key.el   (get key from https://exa.ai/)
             ("exa" . (:command "npx" :args ("-y" "exa-mcp-server")
                                :env (:EXA_API_KEY ,(string-trim (shell-command-to-string "pass cloud/exa_api_key.el")))))
             ("nixos" . (:command "uvx" :args ("mcp-nixos")))
              ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
              ("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp") :env (:DEFAULT_MINIMUM_TOKENS "6000")))
             ))
   :config (require 'mcp-hub))

(use-package khoj
  :after org
  :bind ("M-s M-k" . #'khoj)
  :custom
  (khoj-server-url my/khoj-server-url)
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

;; Summarize a citar-referenced PDF via pymupdf + llm CLI.
;; Requires: pymupdf (pip install pymupdf) and llm (pip install llm).
(defun my/citar-llm-summarize (key)
  "Summarize the PDF for citation KEY via the llm CLI in a dedicated buffer."
  (interactive (list (citar-select-ref)))
  (if-let* ((ht    (citar-get-files key))
            (files (gethash key ht))
            (file  (car files)))
      (async-shell-command
       (format
        (concat "python3 -c \""
                "import pymupdf,sys;"
                "doc=pymupdf.open(sys.argv[1]);"
                "print('\\n'.join(p.get_text() for p in doc))"
                "\" %s | llm -m %s"
                " 'Summarize in 300 words. Key findings, methods, limitations.'")
        (shell-quote-argument file)
        (symbol-name my/ollama-writing-model))
       (format "*citar-summary:%s*" key))
    (message "No PDF found for %s" key)))
(bind-key "M-s b s" #'my/citar-llm-summarize)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook (prog-mode . (lambda () (unless noninteractive (copilot-mode 1))))
  :bind (:map copilot-completion-map
              ("<Launch5> <TAB>"   . copilot-accept-completion)
              ("<Launch5> M-<TAB>" . copilot-accept-completion-by-word)
              ("C-g"   . copilot-clear-overlay))
  :custom
  (copilot-idle-delay 0.5)
  (copilot-max-char -1)
  (copilot-indent-offset-warning-disable t))

(provide 'my-ai)
;;; my-ai.el ends here
