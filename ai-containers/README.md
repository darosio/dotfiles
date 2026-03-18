# AI Containers

Local AI services stack managed with podman-compose and stow.

## Services

| Service         | URL                    | Purpose                                      |
| --------------- | ---------------------- | -------------------------------------------- |
| **Vane**        | http://localhost:3000  | AI-powered web search (Perplexica successor) |
| **SearxNG**     | http://localhost:8080  | Privacy-focused metasearch (MCP backend)     |
| **Khoj**        | http://localhost:42110 | Local RAG — index files, ask questions       |
| **MCP-SearxNG** | stdio                  | Web search tool for gptel in Emacs           |

## Manage

```bash
aic up              # start all
aic up vane         # start one service
aic down khoj       # stop one
aic restart searxng # restart
aic ps              # status
aic logs khoj       # follow logs
```

______________________________________________________________________

## Setup

### Deploy

```bash
./ai-containers.stow.sh   # stow configs + generate khoj .env from pass
aic up                    # start all services
```

______________________________________________________________________

### Vane — http://localhost:3000

First visit opens a settings screen. Configure:

1. **Chat Model Provider** → Ollama
2. **Ollama API URL** → `http://host.docker.internal:11434`
3. **Chat Model** → `qwen3.5:35b-a3b`
4. **Embedding Model Provider** → Ollama
5. **Embedding Model** → `qwen3-embedding`

Vane bundles its own SearxNG — no additional search setup needed.

______________________________________________________________________

### SearxNG — http://localhost:8080

Pre-configured with scientific engines in `~/ai-containers/searxng/settings.yml`:

- PubMed, Google Scholar, Semantic Scholar, arXiv, CrossRef, Wolfram Alpha
- JSON output enabled (required by MCP-SearxNG)

To tweak engines:

```bash
$EDITOR ~/ai-containers/searxng/settings.yml
aic restart searxng
```

______________________________________________________________________

### Khoj — http://localhost:42110

Runs in anonymous mode (no login required for the main UI).
Admin panel at `http://localhost:42110/server/admin` — credentials in `pass ai/khoj`.

#### 1. AI Model API (pre-configured)

**Admin → AI Model APIs → Ollama** already points to `http://host.docker.internal:11434/v1/`.

#### 2. Chat Model — set default

**Admin → Server Chat Settings** → set **Chat default** to `qwen3.5:35b-a3b`.

All Ollama models are auto-discovered and listed under **Chat Models**.

#### 3. Embedding Model

**Admin → Search Model Configs → default → Edit:**

| Field                                 | Value                                   |
| ------------------------------------- | --------------------------------------- |
| Bi encoder                            | `qwen3-embedding`                       |
| Embeddings inference endpoint         | `http://host.docker.internal:11434/v1/` |
| Embeddings inference endpoint type    | `openai`                                |
| Embeddings inference endpoint API key | `ollama`                                |

#### 4. Web Scraper

**Admin → Web Scrapers → Add:**

| Field    | Value                |
| -------- | -------------------- |
| Name     | `Jina`               |
| URL      | `https://r.jina.ai/` |
| Priority | `1`                  |

Jina Reader is free, no API key — prefixes URLs to return clean markdown.

#### 5. Speech-to-Text (optional)

**Admin → Speech to Text Model Options → Add:**

| Field        | Value                                |
| ------------ | ------------------------------------ |
| Model name   | `whisper-1`                          |
| Model type   | `openai`                             |
| AI Model API | Ollama (if `whisper` pulled locally) |

```bash
# Check if whisper is available
ollama pull whisper
```

Khoj degrades gracefully if no STT is configured.

#### 6. Index your files

Go to **http://localhost:42110** → Settings → Files:

- Add directories: `~/Sync/notes/`, `~/Sync/Grants/`, `~/manuscripts/`
- Khoj watches and re-indexes on changes

Or use the Emacs client (`M-s M-k`) to query directly.

______________________________________________________________________

## Client Applications

### Emacs (primary)

`khoj.el` is configured in `emacs/.emacs.d/my-config/my-ai.el`:

```
M-s M-k   → open Khoj chat
```

Set `khoj-server-url` to `http://127.0.0.1:42110`.

### Obsidian

Install the **Khoj plugin** from Obsidian community plugins.
Set server URL to `http://localhost:42110` and API key to any non-empty string (anonymous mode).

### Browser

- **Vane** — add as browser search engine: `http://localhost:3000/?q=%s`
- **SearxNG** — add as browser search engine: `http://localhost:8080/?q=%s`
- **Khoj** — use directly at http://localhost:42110

### Mobile (Khoj app)

iOS/Android Khoj app available — set server to your machine's LAN IP:
`http://192.168.x.x:42110`

______________________________________________________________________

## Credentials

Khoj admin credentials are stored in `pass ai/khoj` and generated into
`~/ai-containers/khoj/.env` by `ai-containers.stow.sh`. The `.env` file
is gitignored and never committed.

To update credentials:

```bash
PASSWORD_STORE_DIR=~/Sync/.pass pass edit ai/khoj
aic restart khoj
```
