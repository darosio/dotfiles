# Local AI Stack — Setup & Workflow Notes

*Daniele · A4500 24GB · Arch Linux · March 2026*

______________________________________________________________________

## 1. Hardware & inference baseline

- **GPU:** NVIDIA A4500, **24GB** GDDR6 — fits all 30B Q4 models fully on GPU
- **Inference:** Ollama with `OLLAMA_CONTEXT_LENGTH=65536` (set in systemd override)
- **Ollama host binding:** must be `0.0.0.0:11434` for container access:
  ```ini
  # systemctl --user edit ollama
  [Service]
  Environment="OLLAMA_HOST=0.0.0.0:11434"
  ```

______________________________________________________________________

## 2. Model roster — current & recommended

### Keep (justified roles)

| Model                    | Role                                                                                                                                             |
| ------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| `glm-4.7-flash:q4_K_M`   | 59.2% SWE-bench, 79.5% τ²-Bench. Best tool-calling stability for Claude Code if qwen3.5 has multi-turn tool errors                               |
| `qwen3.5:27b`            | Best local coder — 72.4% SWE-bench, highest instruction following (IFEval 95%). Use for precise refactoring, scientific Python, proposal writing |
| `qwen3.5:35b-a3b`        | Fast MoE (~3B active), 111 tok/s. Use for Claude Code agentic sessions and fast iteration                                                        |
| `gpt-oss:latest`         | OpenAI open-weights, fast MoE                                                                                                                    |
| `phi4-reasoning:plus`    | 11GB, strong math/science reasoning. Punches above weight                                                                                        |
| `ministral-3:14b`        | Fast lightweight general inference                                                                                                               |
| `deepseek-ocr:latest`    | OCR on scanned PDFs — unique capability                                                                                                          |
| `qwen3-embedding:latest` | Best local embedding — use for Khoj RAG                                                                                                          |
| `qwen3-vl:32b`           | Multimodal/vision tasks                                                                                                                          |
| `magistral:latest`       | magistral:latest — Mistral's reasoning model, still solid overlap with deepseek-r1                                                               |
| `deepseek-r1:32b`        | Best local reasoning. No tools support — use via `gptel`/`llm` CLI only                                                                          |
| `gemma3:27b-it-qat`      | Strong general, QAT quantization                                                                                                                 |
| `gemma3:27b`             | Strong general                                                                                                                                   |
| `devstral:latest`        | Agentic coding complement specialized for multi-step code planning, automated review, and intelligent code translation, good for agentic tasks.  |
| All `:cloud` models      | Zero disk cost, keep all                                                                                                                         |

______________________________________________________________________

## 3. Agentic coding tools

### Comparison

| Tool                 | Local Ollama                 | TUI       | Free local     | Notes                              |
| -------------------- | ---------------------------- | --------- | -------------- | ---------------------------------- |
| Claude Code + Ollama | ✓ via `ollama launch claude` | Moderate  | ✓              | Best agent quality                 |
| OpenCode             | ✓ native                     | Excellent | ✓ MIT          | Best TUI, 75+ providers            |
| Codex CLI            | Partial                      | Moderate  | ✗              | Best GitHub integration            |
| Gemini CLI           | ✗                            | Poor      | ✓ 1000 req/day | 1M context, useful for large files |
| Copilot CLI          | ✗                            | Moderate  | ✗ subscription | Native GitHub PR review            |

### Claude Code + Ollama setup

```bash
# launch with local model (must support tools)
ollama launch claude --model qwen3.5:35b-a3b

# models that support tools ✓
# qwen3.5:27b, qwen3.5:35b-a3b, glm-4.7-flash, gemma3:27b-it-qat, ministral-3:14b

# models that do NOT support tools ✗
# deepseek-r1 variants, deepseek-v3 variants
```

### Task routing

| Task                           | Tool             | Model                    |
| ------------------------------ | ---------------- | ------------------------ |
| Agentic multi-file refactor    | Claude Code      | `qwen3.5:35b-a3b` (fast) |
| Precise scientific Python      | Claude Code      | `qwen3.5:27b` (quality)  |
| Tool-call reliability fallback | Claude Code      | `glm-4.7-flash:q4_K_M`   |
| Hard reasoning (no tools)      | `gptel` in Emacs | `deepseek-r1:32b`        |
| Math/science reasoning         | `gptel` or `llm` | `phi4-reasoning:plus`    |
| Fast throwaway                 | OpenCode / CLI   | `ministral-3:14b`        |
| Large context (whole codebase) | Gemini CLI       | cloud                    |

### AI agent config files

**Strategy:** maintain one `CLAUDE.md` per scope, symlink everywhere. Add a `## Workflows` section as natural language triggers for Copilot/Codex/Gemini where slash commands aren't available.

______________________________________________________________________

## 4. Khoj — self-hosted RAG

### Setup with Podman

```bash
mkdir ~/.khoj && cd ~/.khoj
wget https://raw.githubusercontent.com/khoj-ai/khoj/master/docker-compose.yml

# add to khoj service in docker-compose.yml:
# extra_hosts:
#   - "host.docker.internal:host-gateway"

podman-compose up -d

# verify containers
podman ps -a
podman logs -f khoj_server_1
```

### Admin panel (`http://localhost:42110/admin`)

- Ollama API URL: `http://host.docker.internal:11434/v1/`
- Chat model: `qwen3.5:35b-a3b` or `qwen3.5:27b`
- Embedding model: `qwen3-embedding:latest`

### Emacs integration

```elisp
(use-package khoj
  :bind ("C-c s" . 'khoj)
  :config
  (setq khoj-server-url "http://127.0.0.1:42110"
        khoj-index-directories '("~/projects" "~/manuscripts" "~/protocols")))
```

### What Khoj is for

- Cross-document retrieval: "which of my protocols use ionophore treatment?"
- Persistent memory over your entire manuscript/protocol corpus
- Supersedes Perplexica for local document RAG
- Web search (via SearxNG) still available — supersedes Perplexica entirely

**RAG over your own documents**

`Khoj` — index once, query always:

```
~/manuscripts/   → Khoj indexes all PDFs
~/protocols/     → Khoj indexes all markdown
~/projects/      → Khoj indexes code + docs
```

Then from Emacs via khoj.el: "what ionophore concentrations have I used across my calibration protocols?" — it retrieves from your actual files.
llm-rag is the lighter alternative if you want to build ad-hoc collections without a running server — good for temporary document sets.

______________________________________________________________________

## 5. Daily workflow map

### Reading & literature

```bash
ppl models default ...
pdfllm paper.pdf "summarize in 300 words"
```

Use **Khoj** for cross-paper retrieval across your whole manuscript collection.

### Writing — proposals, grant sections, ideas

`gptel` in Emacs with named directives:

```elisp
(setq gptel-directives
  '((default  . "You are a biophysicist assistant. Be precise.")
    (proposal . "Help write a scientific grant proposal. Use formal academic language. Flag speculative claims.")
    (brainstorm . "Creative scientific collaborator. Challenge assumptions. Think across disciplines.")
    (review   . "Critical peer reviewer. Identify logical gaps, missing controls, unsupported claims.")))
```

- Brainstorming: `deepseek-r1:32b` (best chain-of-thought)
- Proposal writing: `qwen3.5:27b` (best instruction following)
- Final polish: `fabric --pattern improve_writing < draft.txt`

### Coding

| Scope                             | Tool                           |
| --------------------------------- | ------------------------------ |
| Inline completion while typing    | `copilot.el` in `prog-mode`    |
| Explain / fix / discuss in buffer | `gptel` in Emacs               |
| Multi-file refactor / review      | Claude Code in Sway scratchpad |

```elisp
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-keymap
              ("TAB"   . copilot-accept-completion)
              ("M-TAB" . copilot-accept-completion-by-word)))
```

### Shell pipelines

```bash
# explain mypy errors
mypy src/ | llm "explain these type errors grouped by root cause"

# review git diff before committing
git diff | llm -m ollama/qwen3.5:27b "review this diff for bugs and unintended changes"

# pipe nima errors
python -m nima analyze --input stack.tif 2>&1 | llm "what is causing this error?"

# extract protocol steps from a scanned PDF
pymupdf paper.pdf | llm "extract all experimental steps as a numbered list"

# after installing fabric
fabric --pattern extract_wisdom < paper.txt    # key insights
fabric --pattern summarize < protocol.md       # concise summary
fabric --pattern improve_writing < draft.txt   # proposal polish
```

You can also define custom patterns matching your scientific workflow in `~/.config/fabric/patterns/`.

### Scientific tasks

| Task                      | Best local model                          | Notes                      |
| ------------------------- | ----------------------------------------- | -------------------------- |
| Elaborate scientific idea | `deepseek-r1:32b` via gptel               | Best chain-of-thought      |
| Write proposal            | `qwen3.5:27b`                             | High instruction following |
| Design experiments        | `deepseek-r1:32b` + `phi4-reasoning:plus` | Reasoning + quantitative   |
| Protein design            | ESM3 + RFdiffusion (not Ollama)           | Specialized tools required |
| Literature Q&A            | `pymupdf` + `llm` pipe                    | Ad-hoc; Khoj for cross-doc |

### Web search

**Perplexica** — keep as browser tab for current information with citations. Not worth integrating into Emacs pipeline. Best for: recent papers, current best practices.

launch it when you need current information with citations. Keep it as a browser tab.
Best for: "what are recent papers on ESM3 protein design", "current best practice for PyMC4 hierarchical models".

**Gemini CLI** — 1000 free requests/day, 1M context. Useful for large-context tasks without API cost.

______________________________________________________________________

## 6. Tool quick-reference

| Tool         | Install                                    | Primary use               |
| ------------ | ------------------------------------------ | ------------------------- |
| `copilot.el` | Emacs package                              | Inline completion         |
| `khoj`       | `podman-compose up`                        | RAG over local docs + web |
| Perplexica   | Docker/Podman                              | Grounded web search       |
| Claude Code  | `npm install -g @anthropic-ai/claude-code` | Agentic coding            |
| OpenCode     | `npm install -g opencode-ai`               | Agentic coding (local)    |

## 7. Consolidated daily map

| Activity                 | Primary tool              | Secondary                        |
| ------------------------ | ------------------------- | -------------------------------- |
| Reading a new paper      | pymupdf + llm pipe        | Khoj for cross-paper search      |
| Writing proposals/grants | gptel in Emacs            | fabric --pattern improve_writing |
| Scientific brainstorming | gptel + deepseek-r1:32b   | cogito:671b-cloud                |
| Coding inline            | copilot.el                | —                                |
| Coding discussion/fix    | gptel in Emacs            | —                                |
| Multi-file refactor      | Claude Code + qwen3.5:27b | OpenCode                         |
| Shell pipelines          | llm                       | fabric patterns                  |
| Cross-document retrieval | Khoj                      | llm-rag                          |
| Current web info         | Perplexica                | Gemini CLI (1M context)          |
| OCR on scanned PDFs      | deepseek-ocr via Ollama   | pymupdf                          |

The one thing worth doing immediately is setting up the gptel-directives presets.

______________________________________________________________________

*Generated from conversation — March 2026*
