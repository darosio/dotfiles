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
