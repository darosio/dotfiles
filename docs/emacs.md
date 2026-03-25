# Emacs

## Package Management

Emacs packages are managed with
[straight.el](https://github.com/radian-software/straight.el) and pinned via
`emacs/.emacs.d/straight/versions/default.el`.

### Upgrading Packages

**Locally:**

```bash
make upgrade-emacs    # pull → normalize → freeze → thaw → check → smoke tests
# review changes to emacs/.emacs.d/straight/versions/default.el
git add -A && git commit -m "bump(emacs): update straight.el packages"
git push
```

**Via CI:** trigger the *Emacs CI* workflow manually (`workflow_dispatch`). It
runs `make upgrade-emacs` and creates a PR if versions changed.

**After merging a CI-created PR:**

```bash
git pull
make thaw-emacs       # checkout pinned commits + rebuild
```

### Package Management Commands

```text
straight-remove-unused-repos
straight-prune-build
straight-pull-all
straight-check-all
straight-freeze-versions
```

## Keybindings

Keybindings reserved for users: `C-c <letter>` and `F5`–`F9`.

## Configured Packages

### Core

- **use-package**, **straight**, **async**, **which-key**

### Completion & Search

- **vertico**, **consult**, **embark**, **corfu**, **cape**, **orderless**
- **yasnippet** (`M-s y`)
- **recoll** (counsel recoll)
- **consult-project-extra**

### Navigation

- **imenu** (`F9`, `C-F9`)
- **ace-window**

### Org Mode

- **org-roam**, **consult-notes** (`M-s M-n`), **org-modern**
- **org-super-agenda**, **org-ql** — agenda views with daily and weekly review
  - Shopping and weight captures
- **org-babel** — graphviz, plantuml, ditaa, Jupyter
- **org-noter** — precise insertion
- **org-capture**, **org-download**, **org-cliplink**, **org-pdftools**,
  **org-autolist**, **org-indent**
- **org-lint**
- **org-compat** — mpv links
- Export: **ox-rst**, **ox-pandoc**, **ox-twbs**, **ox-beamer**, **ox-md**,
  **ox-koma-letter**

### Email

- **mu4e** and calendar
- **org-mime** for mu4e compose
- `C-c o a` — mu4e-compose-attach-captured-message

### Notes

- **deft** — native insert mode; can create new `./proj/file`

### Git

- **magit**, **magit-todos**

### Development

- **eglot**, **flymake**, **envrc** (direnv support)
- **python-pytest**
- **apheleia** — in-place formatting (black, prettier)
- **numpydoc**

### Editing

- **smartparens** — `C-backspace`, `H-b`, `H-f`, `H-h`, `H-d`, `H-k`, `H-t`,
  `H-()`, `H-{}`
- **expand-region** (`C-=`)
- **aggressive-indent**, **electric-indent**
- **hideshow** — code folding
- **move-text**, **unfill**
- **visual-fill-column** — also for distraction-free writing

### LaTeX

- **auctex**, **cdlatex**

### AI

- **gptel** — LLM chat and inline editing; config in `my-ai.el`
- **ellama** — alternative LLM interface via `llm`
- **mcp** — Model Context Protocol tool integration (SearxNG, fetcher, GitHub,
  filesystem, sequential-thinking, context7, …)
- **khoj** — self-hosted RAG over local documents and web (`M-s M-k`)
- **copilot** — inline completions in `prog-mode` (TAB / M-TAB)

#### gptel Presets

Presets select backend, model, system prompt, and tools in one step.
Activate via `gptel-menu` (`<Launch5> m`) or type `@preset-name` in the prompt.
Host-specific model overrides apply automatically on `whisker` (laptop).

| Preset           | Model                 | Tools                               | Notes                              |
| ---------------- | --------------------- | ----------------------------------- | ---------------------------------- |
| `writing`        | qwen3.5:27b           | `zotero_lookup`                     | Formal academic prose + citations  |
| `brainstorm`     | deepseek-r1:32b       | —                                   | Challenge assumptions              |
| `coding`         | qwen3.5:27b           | `read_buffer`, `EditBuffer`         | Refactor, review, buffer editing   |
| `review`         | qwen3.5:27b           | —                                   | Peer-review style critique         |
| `reasoning`      | deepseek-r1:32b       | —                                   | Deep chain-of-thought              |
| `fast`           | qwen3.5:35b-a3b (MoE) | —                                   | Fast iteration                     |
| `math`           | phi4-reasoning:plus   | —                                   | Math / science reasoning           |
| `vision`         | qwen3-vl:32b          | —                                   | Multimodal / image input           |
| `search`         | qwen3.5:35b-a3b       | SearxNG + fetcher                   | General web search                 |
| `search-science` | qwen3.5:35b-a3b       | SearxNG + fetcher + `zotero_lookup` | PubMed/arXiv + Zotero citation     |
| `grant`          | qwen3.5:27b           | SearxNG + fetcher + `zotero_lookup` | Grant writing, structured sections |
| `pdf`            | qwen3.5:35b-a3b       | `read_pdf`, `zotero_lookup`         | Read + cite a local PDF            |
| `pdf-science`    | qwen3.5:35b-a3b       | All above                           | PDF + broader literature search    |
| `copilot`        | —                     | —                                   | GitHub Copilot cloud backend       |

#### MCP Tools

MCP servers start on Emacs init and wire into gptel automatically.
`<Launch5> M` — connect interactively.

| Server                | Tools exposed                                      |
| --------------------- | -------------------------------------------------- |
| `searxng`             | `searxng_web_search`, `web_url_read`               |
| `fetcher`             | `fetch_url`, `fetch_urls` (Playwright, handles JS) |
| `pdf`                 | `read_pdf`, `extract_doi`, `zotero_lookup`         |
| `github`              | PR / issue / repo tools                            |
| `filesystem`          | File read / write                                  |
| `sequential-thinking` | Structured reasoning chains                        |
| `context7`            | Library documentation lookup                       |
| `duckduckgo`          | DDG web search                                     |
| `nixos`               | NixOS / home-manager option lookup                 |

#### Citation Workflow

All science presets follow a consistent citation loop:

```
searxng_web_search(query) or read_pdf(path)
  → extract DOI
  → zotero_lookup(doi)
      found   → cite as [cite:@Key]          (org-cite)
      missing → report DOI: 10.xxxx/xxx      (add to Zotero manually via browser connector)
```

Zotero's BetterBibTeX plugin auto-exports to `~/Sync/biblio/main.bib`.
`org-cite-global-bibliography` is set to that file — `citar-insert-citation`
works for any paper already in Zotero.

#### One-Command Literature Scan

`<Launch5> l` → `my/literature-scan` — prompts for a topic, opens a dedicated
`*literature-scan: <topic>*` org buffer pre-filled with `@search-science` and the
literature synthesis template. Review and send with `<Launch5> <Launch5>`.

#### Vane Search (AI-powered web search)

`<Launch5> v` → `my/vane-search` — prompts for a query, POSTs to Vane at
`localhost:3000`, and opens an org buffer with the answer and clickable source
links. Requires Vane running (`aic up vane`). Change `my/vane-focus-mode` to
`academicSearch` for PubMed/Scholar-focused results.

#### Save to Org-roam

`<Launch5> n` → `my/gptel-capture-to-roam` — saves the current gptel buffer as
a new org-roam note under `~/Sync/notes/org-roam/ai-notes/`, tagged `:ai:`.
Any `[cite:@Key]` citations in the content automatically become backlinks in the
org-roam graph via `citar-org-roam-mode`.

#### Prompt Library

Reusable structured templates live in `~/.emacs.d/prompts/grant-synthesis.org`.
Add as context with `<Launch5> c A` (whole file) or `<Launch5> c a` (region).

| Section                         | Use for                                                |
| ------------------------------- | ------------------------------------------------------ |
| *Literature synthesis*          | 4–5 targeted searches, grouped by approach             |
| *Grant writing*                 | State of the art / Innovation / Feasibility / Outcomes |
| *NIH (R01/R21)*                 | Specific Aims / Significance / Innovation / Approach   |
| *ERC*                           | Excellence / Impact / Implementation                   |
| *Horizon Europe*                | Excellence / Impact / Implementation + TRL / KPIs      |
| *Methods*                       | Experimental protocol / Calibration / Statistics       |
| *Reviewer objection simulation* | Major/Minor objections + rebuttal drafting             |

#### Example Prompts

**Single PDF** (`pdf` preset):

```
Read the PDF at ~/papers/ChlorON_review_2024.pdf.
Summarize the key findings related to pH independence. Cite the paper.
```

**State-of-the-art section** (`pdf-science` or `grant` preset):

```
Search recent literature on pH-independent chloride biosensors.
Read any relevant review PDFs I provide.
Write a State of the Art section with org-cite citations.
```

**Multi-PDF extraction** (`pdf-science` preset — add files first with `<Launch5> c A`):

```
Read the PDFs I have added to context and extract for each:
- sensing mechanism  - calibration strategy  - reported pH dependence
Reference the relevant page numbers.
```

### UI & Window Management

- **doom-modeline**
- Text scale increase/decrease (`C-x C-0`)
- Maximize window (`H-m`) — `prot/window-single-toggle`

### Misc

- **password-store**, **recentf**, **hl-todo**, **visual-regexp**
- **nov** — EPUB reader
- **csv-mode** (`C-c C-a`)
- **gtklp** — CUPS printing
- **citar** — citation management
- **calfw-org** — calendar

## Vanilla Emacs Use Cases

- **PIM:** GTD (org, super-agenda), email (mu4e), notes (deft)
- **Bibliography:** importing/searching (org-ref, doi), managing .bib
  (bibtex, ivy-bibtex), notes (org-noter, ORB)
- **Writing:** LaTeX, pandoc, org-ref
- **Git:** magit

______________________________________________________________________

## Local AI Stack

*Hardware: NVIDIA A4500 24 GB · Arch Linux · Ollama*

### Hardware & Inference

- **GPU:** A4500, 24 GB GDDR6 — fits all 30B Q4 models fully on-GPU

- **Ollama context:** `OLLAMA_CONTEXT_LENGTH=65536` (systemd override)

- **Container access:** Ollama must bind `0.0.0.0:11434`:

  ```ini
  # systemctl --user edit ollama
  [Service]
  Environment="OLLAMA_HOST=0.0.0.0:11434"
  ```

### Model Roster

| Model                    | Role                                                              |
| ------------------------ | ----------------------------------------------------------------- |
| `qwen3.5:27b`            | Best local coder (72.4% SWE-bench, IFEval 95%). Proposals, Python |
| `qwen3.5:35b-a3b`        | Fast MoE ~111 tok/s. Agentic sessions, fast iteration             |
| `deepseek-r1:32b`        | Best local reasoning. No tools — use via gptel/llm CLI only       |
| `phi4-reasoning:plus`    | 11 GB, strong math/science reasoning                              |
| `glm-4.7-flash:q4_K_M`   | Best tool-calling fallback if qwen3.5 has multi-turn errors       |
| `ministral-3:14b`        | Fast lightweight general inference                                |
| `qwen3-vl:32b`           | Multimodal / vision                                               |
| `qwen3-embedding:latest` | Best local embedding — Khoj RAG                                   |
| `deepseek-ocr:latest`    | OCR on scanned PDFs                                               |
| `gemma3:27b-it-qat`      | Strong general, QAT quantization                                  |
| `devstral:latest`        | Agentic coding: multi-step planning, review, translation          |
| `magistral:latest`       | Mistral reasoning model                                           |
| All `:cloud` models      | Zero disk cost — keep all                                         |

Models that **support tools** (for Claude Code / gptel):
`qwen3.5:27b`, `qwen3.5:35b-a3b`, `glm-4.7-flash`, `gemma3:27b-it-qat`, `ministral-3:14b`

Models that do **not** support tools: `deepseek-r1` variants, `deepseek-v3` variants

This is state-of-the-art scientific writing infrastructure.

### Agentic Coding Tools

| Tool                 | Local Ollama                 | TUI       | Free local     | Notes                   |
| -------------------- | ---------------------------- | --------- | -------------- | ----------------------- |
| Claude Code + Ollama | ✓ via `ollama launch claude` | Moderate  | ✓              | Best agent quality      |
| OpenCode             | ✓ native                     | Excellent | ✓ MIT          | Best TUI, 75+ providers |
| Gemini CLI           | ✗                            | Poor      | ✓ 1000 req/day | 1M context              |
| Copilot CLI          | ✗                            | Moderate  | ✗ subscription | GitHub PR review        |

```bash
ollama launch claude --model qwen3.5:35b-a3b
```

### AI agent config files

**Strategy:** maintain one `CLAUDE.md` per scope, symlink everywhere. Add a `## Workflows` section as natural language triggers for Copilot/Codex/Gemini where slash commands aren't available.

### Daily Workflow

| Activity                   | Primary tool                                | Secondary                            |
| -------------------------- | ------------------------------------------- | ------------------------------------ |
| Read a new paper           | `pdfllm "instruction"`                      | Khoj for cross-paper search          |
| Write proposals/grants     | gptel `@grant`                              | `fabric --pattern improve_writing`   |
| Scientific brainstorm      | gptel `@brainstorm`                         | cogito:671b-cloud                    |
| Literature synthesis       | `<Launch5> l`                               | gptel `@search-science`              |
| Local web search (AI)      | `<Launch5> v` (Vane)                        | Vane browser tab                     |
| Current web info           | Perplexica                                  | Gemini CLI (1M context)              |
| Coding inline              | `copilot.el`                                | —                                    |
| Coding discussion/fix      | gptel in Emacs                              | —                                    |
| Multi-file refactor        | Claude Code + qwen3.5:27b                   | OpenCode                             |
| Shell pipelines            | `llm`                                       | `fabric` patterns                    |
| Cross-document retrieval   | Khoj (`M-s M-k`)                            | `llm-rag`                            |
| OCR on scanned PDFs        | `deepseek-ocr` via Ollama                   | `pymupdf`                            |
| -------------------------- | ------------------------------------------- | ------------------------------------ |
| Design experiments         | `deepseek-r1:32b` + `phi4-reasoning:plus`   | Reasoning + quantitative             |
| Protein design             | ESM3 + RFdiffusion (not Ollama)             | Specialized tools required           |
| -------------------------- | ------------------------------------------- | ------------------------------------ |

#### Web search

##### Vane

When you need current information with citations. Keep it as a browser tab.
Best for: "what are recent papers on ESM3 protein design", "current best practice for PyMC4 hierarchical models".

##### Gemini CLI

1000 free requests/day, 1M context. Useful for large-context tasks without API cost.

#### Coding

-(use-package copilot

- :hook (prog-mode . copilot-mode)
- :bind (:map copilot-completion-keymap
- ```
           ("TAB"   . copilot-accept-completion)
  ```
- ```
           ("M-TAB" . copilot-accept-completion-by-word)))
  ```

### Shell Pipelines

```bash
# debug / review
mypy src/ | llm "explain these type errors grouped by root cause"
python -m nima analyze --input stack.tif 2>&1 | llm "what is causing this error?"
git diff | llm -m ollama/qwen3.5:27b "review this diff for bugs and unintended changes"

# fabric patterns
fabric --pattern extract_wisdom < paper.txt
fabric --pattern summarize < protocol.md
fabric --pattern improve_writing < draft.txt
```

Custom fabric patterns: `~/.config/fabric/patterns/`

______________________________________________________________________

## PDF Shell Tools

Defined in `bash/.bashrc.d/functions.sh`.
All use **pymupdf** for text extraction, then pipe to `llm` or `fabric`.

### `pdfllm` — pipe a PDF to `llm`

```bash
pdfllm [-m model] [-t template] <file.pdf> [prompt]

pdfllm paper.pdf "summarize key findings on pH-independent sensing"
pdfllm -m ollama/qwen3.5:27b paper.pdf "extract all calibration steps as a numbered list"
pdfllm -t fabric:extract_wisdom paper.pdf   # use a fabric template
```

| Flag | Meaning                                            |
| ---- | -------------------------------------------------- |
| `-m` | Override LLM model (default: `llm models default`) |
| `-t` | Template name, e.g. `fabric:extract_wisdom`        |

### `pdffabric` — pipe a PDF through a fabric pattern

```bash
pdffabric [-p pattern] [-m model] <file.pdf>

pdffabric paper.pdf                          # default: extract_wisdom
pdffabric -p summarize paper.pdf
pdffabric -p improve_writing draft.pdf
pdffabric -m qwen3.5:27b -p extract_wisdom paper.pdf
```

| Flag | Meaning                                         |
| ---- | ----------------------------------------------- |
| `-p` | Fabric pattern (default: `extract_wisdom`)      |
| `-m` | Override fabric model (default: fabric default) |

### `pdfembed` — embed PDFs into an `llm` collection for RAG

```bash
pdfembed [-c collection] [-m embedding-model] <dir|file.pdf...>

pdfembed ~/papers/                         # embed whole folder → collection "biblio"
pdfembed -c myproject ~/papers/review.pdf  # embed single file into named collection
```

| Flag | Meaning                                      |
| ---- | -------------------------------------------- |
| `-c` | Collection name (default: `biblio`)          |
| `-m` | Embedding model (default: `qwen3-embedding`) |

### `ragask` — query an `llm` RAG collection

```bash
ragask [-m model] [-c collection] [-l chain-limit] <prompt>

ragask "what ionophore concentrations have I used in my calibration protocols?"
ragask -c myproject "summarize the sensing mechanisms in these papers"
ragask -m ollama/qwen3.5:27b -c biblio "compare pH dependence across included papers"
```

| Flag | Meaning                            |
| ---- | ---------------------------------- |
| `-m` | Override model                     |
| `-c` | RAG collection (default: `biblio`) |
| `-l` | Chain limit (default: 10)          |

> Use `pdfembed` to build the collection first, then `ragask` to query it.
> This is the lightweight RAG path — no server required. For persistent,
> cross-session, cross-corpus RAG use **Khoj** below.

______________________________________________________________________

## Khoj — Self-Hosted RAG

Khoj provides persistent document indexing with web search, chat memory, and
an Emacs client. Runs as a Podman container at `localhost:42110`.

### Start / stop

```bash
aic up khoj      # start Khoj + its Postgres backend
aic down khoj    # stop
aic status       # check all AI containers
```

Container definition: `ai-containers/ai-containers/khoj/podman-compose.yml`

Khoj is configured to reach Ollama on the host via
`http://host.docker.internal:11434/v1/`. The default chat model is
`qwen3.5:35b-a3b`; the embedding model is set in the admin panel.

### Web UI (no Emacs required)

Open `http://localhost:42110` in a browser.

**First-time setup (admin panel at `http://localhost:42110/admin`):**

1. Set **Chat model** → provider: Ollama, model: `qwen3.5:35b-a3b`
2. Set **Embedding model** → provider: Ollama, model: `qwen3-embedding:latest`
3. Go to **Files** → upload PDFs or point to local folders

**Typical web UI workflow:**

```
Ask: "Which of my protocols use ionophore treatment?"
     → Khoj retrieves from indexed files, cites source filenames

Ask: "Find recent papers on pH-independent chloride biosensors"
     → Khoj runs a web search (via SearxNG), summarises with citations

Ask: "Compare the sensing mechanisms across the papers I uploaded"
     → Khoj reads indexed PDFs, produces a structured comparison
```

### Emacs client (`M-s M-k`)

`M-s M-k` opens a chat buffer. Type a question; Khoj retrieves from your
indexed documents and/or the web and responds with citations.

**Combining Khoj with gptel:**

1. Run a Khoj search (`M-s M-k`) to gather and cite sources
2. Copy relevant passages into a gptel buffer
3. Use the `writing` preset to synthesise into a grant section with org-cite
   citations

### What Khoj indexes

| Source                    | How to add                                           |
| ------------------------- | ---------------------------------------------------- |
| PDFs (papers, protocols)  | Web UI → Files, or `khoj-index-directories`          |
| Org / Markdown notes      | `khoj-index-directories` in Emacs config             |
| Web search results        | Automatic when answering questions                   |
| Zotero library (optional) | Export collection as BibTeX → upload PDF attachments |

### Khoj vs `pdfembed`/`ragask`

| Aspect            | Khoj                         | `pdfembed` + `ragask`        |
| ----------------- | ---------------------------- | ---------------------------- |
| Persistence       | Persistent (Postgres)        | Persistent (`llm` store)     |
| Server required   | Yes (Podman)                 | No                           |
| Web search        | Yes (built-in)               | No                           |
| Emacs integration | `khoj.el`                    | Shell pipeline               |
| Multi-modal       | PDFs, Org, Markdown, web     | PDFs only                    |
| Best for          | Cross-corpus, daily research | Ad-hoc collection, scripting |

______________________________________________________________________

## Custom Ollama Models

Create fine-tuned system-prompt models for recurring tasks:

```bash
mkdir -p ~/models

cat > ~/models/daniele-research.Modelfile << 'EOF'
FROM qwen3.5:27b
PARAMETER temperature 0.7
PARAMETER top_p 0.9
PARAMETER num_ctx 65536
SYSTEM "You are a biophysicist research assistant. Be precise about units, statistics, and experimental methodology. Flag speculative claims. When citing sources use org-cite format [cite:@AuthorYEAR]. Never invent citations."
EOF
ollama create daniele-research -f ~/models/daniele-research.Modelfile

cat > ~/models/daniele-proposal.Modelfile << 'EOF'
FROM qwen3.5:27b
PARAMETER temperature 0.4
PARAMETER top_p 0.85
PARAMETER num_ctx 65536
SYSTEM "You are helping write a scientific grant proposal (ERC/PRIN/Horizon style). Use formal academic language. Structure with Specific Aims, Significance, Innovation, and Approach. Flag speculative claims. Prefer quantitative statements. Use org-cite format for citations."
EOF
ollama create daniele-proposal -f ~/models/daniele-proposal.Modelfile

cat > ~/models/daniele-brainstorm.Modelfile << 'EOF'
FROM deepseek-r1:32b
PARAMETER temperature 0.9
PARAMETER top_p 0.95
PARAMETER num_ctx 65536
SYSTEM "You are a creative scientific collaborator. Challenge assumptions. Think across disciplines. Propose unexpected connections. Be explicit about speculation vs established fact."
EOF
ollama create daniele-brainstorm -f ~/models/daniele-brainstorm.Modelfile
```

These models are available in gptel via `@custom-model` or directly in `llm`:

```bash
llm -m ollama/daniele-research "evaluate this experimental design"
pdfllm -m ollama/daniele-proposal paper.pdf "extract innovation claims"
```
