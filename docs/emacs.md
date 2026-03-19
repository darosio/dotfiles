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

#### gptel Presets (`C-c <return> p`)

Presets select backend, model, system prompt, and tools in one step.
Host-specific overrides apply automatically on `whisker` (laptop).

| Preset           | Model                 | Notes                                                 |
| ---------------- | --------------------- | ----------------------------------------------------- |
| `writing`        | qwen3.5:27b           | `writing` directive — formal academic prose           |
| `brainstorm`     | deepseek-r1:32b       | `brainstorm` directive — challenge assumptions        |
| `coding`         | qwen3.5:27b           | `coding` directive + `read_buffer`/`EditBuffer` tools |
| `review`         | qwen3.5:27b           | `review` directive — peer-review style critique       |
| `reasoning`      | deepseek-r1:32b       | deep chain-of-thought, no tools                       |
| `fast`           | qwen3.5:35b-a3b (MoE) | fast iteration                                        |
| `math`           | phi4-reasoning:plus   | math/science reasoning                                |
| `vision`         | qwen3-vl:32b          | multimodal / image input                              |
| `search`         | qwen3.5:35b-a3b       | web search via SearxNG + fetcher MCP tools            |
| `search-science` | qwen3.5:35b-a3b       | as `search` with science-focused system prompt        |
| `grant`          | qwen3.5:27b           | `proposal` directive + MCP search (see Grant tools)   |
| `copilot`        | —                     | GitHub Copilot cloud backend                          |

#### MCP Tools

MCP servers start on Emacs init and wire into gptel automatically.
`<Launch5> M` — connect interactively.

| Server                | Command                                         | Tools                                                       |
| --------------------- | ----------------------------------------------- | ----------------------------------------------------------- |
| `searxng`             | `podman exec -i mcp-searxng node dist/index.js` | `searxng_web_search`, `web_url_read`                        |
| `pdf`                 | `python3 ~/.local/bin/pdf-mcp.py`               | `read_pdf` — local PDF text extraction with page markers    |
| `fetcher`             | `npx -y fetcher-mcp`                            | `fetch_url`, `fetch_urls` (Playwright, handles JS/PDF/DOCX) |
| `github`              | Docker `ghcr.io/github/github-mcp-server`       | PR/issue/repo tools                                         |
| `filesystem`          | `npx @modelcontextprotocol/server-filesystem`   | file read/write                                             |
| `sequential-thinking` | npx                                             | structured reasoning chains                                 |
| `context7`            | npx `@upstash/context7-mcp`                     | library documentation lookup                                |
| `duckduckgo`          | `uvx duckduckgo-mcp-server`                     | DDG web search                                              |
| `nixos`               | `uvx mcp-nixos`                                 | NixOS/home-manager option lookup                            |

#### Grant Writing Tools (`<f7> R`)

Interactive functions that fire async `gptel-request` calls with the `proposal`
system directive, collecting results in a dedicated `*Grant: TOPIC*` org buffer.
MCP SearxNG + fetcher are connected automatically before each request.

| Key | Command                      | Search strategy                                                         |
| --- | ---------------------------- | ----------------------------------------------------------------------- |
| `s` | `gptel-grant-sota`           | `"recent advances" AND TOPIC AND (review OR overview)`                  |
| `g` | `gptel-grant-gap`            | `TOPIC AND (limitation OR challenge OR unmet) review`                   |
| `i` | `gptel-grant-innovation`     | `TOPIC AND ("novel approach" OR "new method" OR "first demonstration")` |
| `f` | `gptel-grant-feasibility`    | `TOPIC AND ("proof-of-concept" OR validation OR demonstrated)`          |
| `r` | `gptel-grant-funding`        | `TOPIC AND (translational OR clinical OR scalable OR "in vivo")`        |
| `a` | `gptel-grant-all`            | All five above, concurrently                                            |
| `d` | `my/doi-to-bibtex`           | Fetch BibTeX from doi.org; copy to kill-ring (add via Zotero)           |
| `c` | `gptel-org-insert-citations` | Normalise `(AuthorYEAR)` citations in buffer to `[cite:@AuthorYEAR]`    |

`my/doi-to-bibtex` accepts a bare DOI (`10.1016/...`) or a full `doi.org` URL.
The BibTeX is copied to the kill-ring; add to your library via **Zotero → Add Item by
Identifier** (`Ctrl+Shift+N`) since `main.bib` is auto-generated by Zotero's BetterBibTeX
plugin and must not be edited directly.

#### PDF MCP — Local PDF Reader

`scripts/pdf-mcp.py` is a minimal MCP server that exposes one tool, `read_pdf`, backed
by **pymupdf**. It speaks JSON-RPC 2.0 over STDIO and returns extracted text with
per-page markers (up to 30 000 chars).

Enable in gptel by connecting the `pdf` server:

```elisp
(gptel-mcp-connect '("pdf") 'sync)
```

Then prompt naturally:

```
Read the PDF at ~/papers/ChlorON_review_2024.pdf.
Summarize the key findings on pH independence and cite the paper.
```

The model calls `read_pdf`, reads the text, and cites by filename. Combine with
SearxNG for a full NotebookLM-style workflow:

```
Search recent literature on pH-independent chloride biosensors.
Read ~/papers/ChlorON_review_2024.pdf for my own preliminary data.
Write a State of the Art section with [cite:@AuthorYEAR] citations.
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
